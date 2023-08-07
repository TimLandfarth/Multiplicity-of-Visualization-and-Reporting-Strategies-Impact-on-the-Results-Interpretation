# 0. Set Wd and libraries----
## 0.1 WD----
setwd()

## 0.2 Librarys----
library(ggplot2)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(forcats)
library(ggradar)
library(janitor)
library(ggformula)
library(kableExtra)
library(cowplot)
library(scales)
library(ggthemes)

## 0.3 Set standards----
gray1 <- "#969696"
gray2 <- c("#636363","#969696")
gray3 <- c("#636363","#969696","#bdbdbd")
col_s <- c("#8C8C8C", "#88BDE6", "#FBB258", "#90CD97", "#F6AAC9", "#BFA554", "#A899C7", "#EDDD46", "#F07E6E")
col_m <- c("#4D4D4D", "#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#B2912F", "#B276B2", "#DECF35", "#F15854")
col_h <- c("#000000", "#265DAB", "#DF5C24", "#059748", "#E5126F", "#9D722A", "#7B3A96", "#C7B42D", "#CB2027")
axis_col <- "grey70"
linesize <- 1.5
pointsize <- 5
axissize <- .5
pdf_w_h <- c((7/3)*4, (7/3) * 3)


# 1. first graphic----
## 1.1 From Paper----
get_cv_means <- function(df, meas = "cindex.uno") {
  meas <- sym(meas)
  df %>%
    group_by(task.id, learner.id) %>%
    summarise(cv_mean = mean(!! enquo(meas)))
}

bench_t_test <- function(dat_set, meth1, meth2, meas = "m_cindex") {
  spreaded <- dat_set %>% filter(learner.id %in% c(meth1, meth2))  %>% tidyr::spread("task.id", meas)
  tt1 <- unlist(spreaded[1, -1])
  tt2 <- unlist(spreaded[2, -1])
  t.test(tt1, tt2, paired = TRUE, conf.level = 0.99)
}

load("data/merged-results.RData")

tt_means <- 
  get_cv_means(df_res, "timetrain") %>%
  mutate(
    approach = case_when(
      learner.id %in% c("Kaplan-Meier", "Clinical only") ~ "reference",
      learner.id %in% c("rfsrc", "ranger", "blockForest") ~ "Random forest",
      learner.id %in% c("CoxBoost", "CoxBoost favoring", "glmboost") ~ "Boosting",
      learner.id %in% c("prioritylasso", "prioritylasso favoring", 
                        "grridge", "ipflasso", "Lasso")  ~ "Penalized regression"
    ))

task_size_order <- c("BRCA", "LUAD", "LUSC", "HNSC", "LGG", "UCEC", "BLCA", "STAD", "SKCM", "KIRC",
                     "OV", "KIRP", "COAD", "LIHC", "SARC", "PAAD", "ESCA", "LAML")
tt_means$task.id <- factor(tt_means$task.id, levels = rev(task_size_order))
names(tt_means) <- c("task.id", "learner", "mean", "approach")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
lty <- c("solid", "solid", "dashed", "dotted", "solid","dashed", "dashed", "dotted", "dotdash", "solid", "dashed")
lty2 <- c("solid", "dashed", "dotted")
p_t_lines <-
  ggplot(data = filter(tt_means, !learner %in% c("Kaplan-Meier", "Clinical only")),
         aes(x = task.id, y = mean, group = learner)) +
  scale_shape_manual(values = 1:nlevels(tt_means$learner)) +
  geom_line(aes(linetype = learner, colour = learner), size = 0.75) +
  geom_point(aes(shape = learner), size = 1) + 
  labs(x = "Dataset", y = "Time in seconds") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "top",
        #axis.text.y = element_text(angle = 90),
        axis.text.x = element_text(angle = 45, vjust = 0.75),
        axis.title.x = element_blank())

p_t_lines_log <-
  ggplot(data = filter(tt_means, !learner %in% c("Kaplan-Meier", "Clinical only")), 
         aes(x = task.id, y = mean, group = learner)) +
  scale_shape_manual(values = 1:nlevels(tt_means$learner)) +
  geom_line(aes(linetype = learner, colour = learner), size = 0.75) +
  geom_point(aes(shape = learner), size = 1) + 
  labs(x = "Data set", y = "Time in seconds (log scale)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none", 
        #axis.text.y = element_text(angle = 90),
        axis.text.x = element_text(angle = 45, vjust = 0.75),
        axis.title.x = element_blank()) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 3),
                labels = trans_format("log10", math_format(10^.x)))

fig1 <- plot_grid(p_t_lines, p_t_lines_log,
                  labels = c('A', 'B'),
                  label_size = 12,
                  align = "v",
                  nrow = 2,
                  rel_heights = c(1.25, 1))


##pdf(file = "original.pdf", width = 9, height = 7)
fig1
dev.off()

rm(df_res, fig1, p_t_lines, p_t_lines_log, cbbPalette, lty, lty2, task_size_order)

## 1.2 Variations----
tt_means$task.id <- factor(tt_means$task.id, levels = c("LAML", "ESCA", "PAAD", "SARC", "LIHC", "KIRP", "COAD", "OV", "KIRC", "SKCM", "STAD", "BLCA", "UCEC", "LUSC", "LGG", "LUAD", "HNSC", "BRCA"))
tt_means$n <- NA
tt_means$n[which(tt_means$task.id == "BLCA")] <- 382
tt_means$n[which(tt_means$task.id == "BRCA")] <- 735
tt_means$n[which(tt_means$task.id == "COAD")] <- 191
tt_means$n[which(tt_means$task.id == "ESCA")] <- 106
tt_means$n[which(tt_means$task.id == "HNSC")] <- 443
tt_means$n[which(tt_means$task.id == "KIRC")] <- 249
tt_means$n[which(tt_means$task.id == "KIRP")] <- 167
tt_means$n[which(tt_means$task.id == "LAML")] <- 35
tt_means$n[which(tt_means$task.id == "LGG")] <- 419
tt_means$n[which(tt_means$task.id == "LIHC")] <- 159
tt_means$n[which(tt_means$task.id == "LUAD")] <- 426
tt_means$n[which(tt_means$task.id == "LUSC")] <- 418
tt_means$n[which(tt_means$task.id == "OV")] <- 219
tt_means$n[which(tt_means$task.id == "PAAD")] <- 124
tt_means$n[which(tt_means$task.id == "SARC")] <- 126
tt_means$n[which(tt_means$task.id == "SKCM")] <- 249
tt_means$n[which(tt_means$task.id == "STAD")] <- 295
tt_means$n[which(tt_means$task.id == "UCEC")] <- 405

df_sum <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(task.id) %>% summarise(avg = sum(mean))
df_mean <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(task.id) %>% summarise(avg = mean(mean))
df_median <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(task.id) %>% summarise(avg = median(mean))
df_min <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(task.id) %>% summarise(avg = min(mean))
df_max <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(task.id) %>% summarise(avg = max(mean))

### 1.2.1. Nominal Comparison----

#### 1.2.1.1. Bars ----
#### x = time
#### y = Data set size
### Sum
#pdf(file = "comp_bars_dataset_sum.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_sum, aes(y = avg, x = task.id)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Calculation time (sec)", title = "Calculation time by data set", subtitle = "learners summarised by the sum over time")+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### mean
#pdf(file = "comp_bars_dataset_mean.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_mean, aes(y = avg, x = task.id)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Calculation time (sec)", title = "Calculation time by data set", subtitle = "learners summarised by the maximum over time")+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Median
#pdf(file = "comp_bars_dataset_median.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_median, aes(y = avg, x = task.id)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Calculation time (sec)", title = "Calculation time by data set", subtitle = "learners summarised by the median over time")+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Minimun
#pdf(file = "comp_bars_dataset_min.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_min, aes(y = avg, x = task.id)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Calculation time (sec)", title = "Calculation time by data set", subtitle = "learners summarised by the minimum over time")+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Maximum
#pdf(file = "comp_bars_dataset_max.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_max, aes(y = avg, x = task.id)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Calculation time (sec)", title = "Calculation time by data set", subtitle = "learners summarised by the maximum over time")+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

#### Line: Reference min-max
### Sum

#pdf(file = "comp_bars_dataset_sum_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_sum, aes(y = avg, x = task.id)) +
  geom_col(fill = gray2[2])+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Calculation time (sec)", title = "Calculation time by data set", subtitle = "learners summarised by the sum over time")+
  geom_segment(aes(x = levels(df_sum$task.id)[1], xend = levels(df_sum$task.id)[length(df_sum$task.id)],
                   y = df_sum$avg[df_sum$task.id == levels(df_sum$task.id)[1]], yend = df_sum$avg[df_sum$task.id == levels(df_sum$task.id)[length(df_sum$task.id)]]),
               colour = gray2[1], size = linesize)+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Mean
#pdf(file = "comp_bars_dataset_mean_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_mean, aes(y = avg, x = task.id)) +
  geom_col(fill = gray2[2])+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Calculation time", title = "Calculation time by data set", subtitle = "learners summarised by the mean over time")+
  geom_segment(aes(x = levels(df_mean$task.id)[1],
                   xend = levels(df_mean$task.id)[length(df_mean$task.id)],
                   y = df_mean$avg[df_mean$task.id == levels(df_mean$task.id)[1]],
                   yend = df_mean$avg[df_mean$task.id == levels(df_mean$task.id)[length(df_mean$task.id)]]),
               colour = gray2[1], size = linesize)+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Median
#pdf(file = "comp_bars_dataset_median_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_median, aes(y = avg, x = task.id)) +
  geom_col(fill = gray2[2])+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Calculation time", title = "Calculation time by data set", subtitle = "leaners summarised by the median over time")+
  geom_segment(aes(x = levels(df_median$task.id)[1],
                   xend = levels(df_median$task.id)[length(df_median$task.id)],
                   y = df_median$avg[df_median$task.id == levels(df_median$task.id)[1]],
                   yend = df_median$avg[df_median$task.id == levels(df_median$task.id)[length(df_median$task.id)]]),
               colour = gray2[1], size = linesize)+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Minimun
#pdf(file = "comp_bars_dataset_min_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_min, aes(y = avg, x = task.id)) +
  geom_col(fill = gray2[2])+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Calculation time", title = "Calculation time by data set", subtitle = "learners summarised by the minimum over time")+
  geom_segment(aes(x = levels(df_min$task.id)[1],
                   xend = levels(df_min$task.id)[length(df_min$task.id)],
                   y = df_min$avg[df_min$task.id == levels(df_min$task.id)[1]],
                   yend = df_min$avg[df_min$task.id == levels(df_min$task.id)[length(df_min$task.id)]]),
               colour = gray2[1], size = linesize)+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Maximum
#pdf(file = "comp_bars_dataset_max_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[1])
ggplot(data = df_max, aes(y = avg, x = task.id)) +
  geom_col(fill = gray2[2])+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Calculation time", title = "Calculation time by data set", subtitle = "learners summarised by the maximum over time")+
  geom_segment(aes(x = levels(df_max$task.id)[1],
                   xend = levels(df_max$task.id)[length(df_max$task.id)],
                   y = df_max$avg[df_max$task.id == levels(df_max$task.id)[1]], 
                   yend = df_max$avg[df_max$task.id == levels(df_max$task.id)[length(df_max$task.id)]]), 
               colour = gray2[1], size = linesize)+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

rm(df_sum, df_mean, df_median, df_min, df_max)

## Learner
df_sum <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(learner) %>% summarise(avg = sum(mean))
df_sum$approach <- c("Penalized regression", "Boosting", "Boosting", "Penalized regression", "Penalized regression", "Boosting", "Penalized regression", "Random forest", "Random forest", "Random forest", "Penalized regression")
df_mean <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(learner) %>% summarise(avg = mean(mean))
df_mean$approach <- c("Penalized regression", "Boosting", "Boosting", "Penalized regression", "Penalized regression", "Boosting", "Penalized regression", "Random forest", "Random forest", "Random forest", "Penalized regression")
df_median <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(learner) %>% summarise(avg = median(mean))
df_median$approach <- c("Penalized regression", "Boosting", "Boosting", "Penalized regression", "Penalized regression", "Boosting", "Penalized regression", "Random forest", "Random forest", "Random forest", "Penalized regression")
df_min <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(learner) %>% summarise(avg = min(mean))
df_min$approach <- c("Penalized regression", "Boosting", "Boosting", "Penalized regression", "Penalized regression", "Boosting", "Penalized regression", "Random forest", "Random forest", "Random forest", "Penalized regression")
df_max <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(learner) %>% summarise(avg = max(mean))
df_max$approach <- c("Penalized regression", "Boosting", "Boosting", "Penalized regression", "Penalized regression", "Boosting", "Penalized regression", "Random forest", "Random forest", "Random forest", "Penalized regression")
df_min_dat <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only") & task.id == "LAML")
df_min_dat$approach <- c("Penalized regression", "Boosting", "Boosting", "Penalized regression", "Penalized regression", "Boosting", "Penalized regression", "Random forest", "Random forest", "Random forest", "Penalized regression")
df_max_dat <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only") & task.id == "BRCA")
df_max_dat$approach <- c("Penalized regression", "Boosting", "Boosting", "Penalized regression", "Penalized regression", "Boosting", "Penalized regression", "Random forest", "Random forest", "Random forest", "Penalized regression")
df_med_dat <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only") & task.id == "KIRC")
df_med_dat$approach <- c("Penalized regression", "Boosting", "Boosting", "Penalized regression", "Penalized regression", "Boosting", "Penalized regression", "Random forest", "Random forest", "Random forest", "Penalized regression")



### Sum
#pdf(file = "comp_bars_learner_nsub_sum.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_sum %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the sum over time")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Mean
#pdf(file = "comp_bars_learner_nsub_mean.pdf", width = pdf_w_h[1], height = pdf_w_h[1])
ggplot(data = df_mean %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the mean over time")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Median
#pdf(file = "comp_bars_learner_nsub_median.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_median %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the median over time")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Minimun
#pdf(file = "comp_bars_learner_nsub_min.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_min %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the minimum over time")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Maximum
#pdf(file = "comp_bars_learner_nsub_max.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_max %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the maximum over time")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Minimum Data set size
#pdf(file = "comp_bars_learner_nsub_minDat.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_min_dat %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "for the smallest dataset")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Median Data set size
#pdf(file = "comp_bars_learner_nsub_medianDat.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_med_dat %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "for the median dataset")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Maximum Data set size
#pdf(file = "comp_bars_learner_nsub_maxDat.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_max_dat %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "for the biggest dataset")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### With reference
### Sum
#pdf(file = "comp_bars_learner_nsub_sum_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_sum %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the sum over time")+
  geom_segment(aes(x = df_sum$learner[df_sum$avg == min(df_sum$avg)],
                   xend = df_sum$learner[df_sum$avg == max(df_sum$avg)],
                   y = min(df_sum$avg), 
                   yend = max(df_sum$avg)), 
               colour = gray2[1], size = linesize)+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Mean
#pdf(file = "comp_bars_learner_nsub_mean_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[1])
ggplot(data = df_mean %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the mean over time")+
  geom_segment(aes(x = df_mean$learner[df_mean$avg == min(df_mean$avg)],
                   xend = df_mean$learner[df_mean$avg == max(df_mean$avg)],
                   y = min(df_mean$avg), 
                   yend = max(df_mean$avg)), 
               colour = gray2[1], size = linesize)+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Median
#pdf(file = "comp_bars_learner_nsub_median_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_median %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the median over time")+
  geom_segment(aes(x = df_median$learner[df_median$avg == min(df_median$avg)],
                   xend = df_median$learner[df_median$avg == max(df_median$avg)],
                   y = min(df_median$avg), 
                   yend = max(df_median$avg)), 
               colour = gray2[1], size = linesize)+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Minimun
#pdf(file = "comp_bars_learner_nsub_min_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_min %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the minimum over time")+
  geom_segment(aes(x = df_min$learner[df_min$avg == min(df_min$avg)],
                   xend = df_min$learner[df_min$avg == max(df_min$avg)],
                   y = min(df_min$avg), 
                   yend = max(df_min$avg)), 
               colour = gray2[1], size = linesize)+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Maximum
#pdf(file = "comp_bars_learner_nsub_max_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_max %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the maximum over time")+
  geom_segment(aes(x = df_max$learner[df_max$avg == min(df_max$avg)],
                   xend = df_max$learner[df_max$avg == max(df_max$avg)],
                   y = min(df_max$avg), 
                   yend = max(df_max$avg)), 
               colour = gray2[1], size = linesize)+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Minimum Data set size
#pdf(file = "comp_bars_learner_nsub_minDat_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_min_dat %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "for the smallest dataset")+
  geom_segment(aes(x = df_min_dat$learner[df_min_dat$mean == min(df_min_dat$mean)],
                   xend = df_min_dat$learner[df_min_dat$mean == max(df_min_dat$mean)],
                   y = min(df_min_dat$mean), 
                   yend = max(df_min_dat$mean)), 
               colour = gray2[1], size = linesize)+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Median Data set size
#pdf(file = "comp_bars_learner_nsub_medianDat_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_med_dat %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "for the median dataset")+
  geom_segment(aes(x = df_med_dat$learner[df_med_dat$mean == min(df_med_dat$mean)],
                   xend = df_med_dat$learner[df_med_dat$mean == max(df_med_dat$mean)],
                   y = min(df_med_dat$mean), 
                   yend = max(df_med_dat$mean)), 
               colour = gray2[1], size = linesize)+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### Maximum Data set size
#pdf(file = "comp_bars_learner_nsub_maxDat_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_max_dat %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "for the biggest dataset")+
  geom_segment(aes(x = df_max_dat$learner[df_max_dat$mean == min(df_max_dat$mean)],
                   xend = df_max_dat$learner[df_max_dat$mean == max(df_max_dat$mean)],
                   y = min(df_max_dat$mean), 
                   yend = max(df_max_dat$mean)), 
               colour = gray2[1], size = linesize)+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### With subgroups
### Sum
#pdf(file = "nomcomp_bars_learner_sum.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_sum %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the sum over time")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()
dev.off()

### Mean
#pdf(file = "comp_bars_learner_mean.pdf", width = pdf_w_h[1], height = pdf_w_h[1])
ggplot(data = df_mean %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the mean over time")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()
dev.off()

### Median
#pdf(file = "comp_bars_learner_median.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_median %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the median over time")+
  facet_grid(. ~ approach, space="free_x", scales="free_x", switch="x")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()
dev.off()

### Minimun
#pdf(file = "comp_bars_learner_min.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_min %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the minimum over time")+
  facet_grid(. ~ approach, space="free_x", scales="free_x", switch="x")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()
dev.off()

### Maximum
#pdf(file = "comp_bars_learner_max.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_max %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the maximum over time")+
  facet_grid(. ~ approach, space="free_x", scales="free_x", switch="x")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()
dev.off()

### Minimum Data set size
#pdf(file = "comp_bars_learner_minDat.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_min_dat %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "for the smallest dataset")+
  facet_grid(. ~ approach, space="free_x", scales="free_x", switch="x")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()
dev.off()

### Median Data set size
#pdf(file = "comp_bars_learner_medianDat.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_med_dat %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "for the median dataset")+
  facet_grid(. ~ approach, space="free_x", scales="free_x", switch="x")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()
dev.off()

### Maximum Data set size
#pdf(file = "comp_bars_learner_maxDat.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df_max_dat %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner)) +
  geom_col(fill = gray1)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "for the biggest dataset")+
  facet_grid(. ~ approach, space="free_x", scales="free_x", switch="x")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()
dev.off()

#### with reference: according to learner
### Sum
#pdf(file = "comp_bars_learner_max_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ref <- data.frame(x = c("glmboost", "Lasso", "ranger"), xend = c("CoxBoost", "prioritylasso favoring", "blockForest"),
                   y = c(df_sum$avg[df_sum$learner == "glmboost"], df_sum$avg[df_sum$learner == "Lasso"], df_sum$avg[df_sum$learner == "ranger"]),
                   yend = c(df_sum$avg[df_sum$learner == "CoxBoost"], df_sum$avg[df_sum$learner == "prioritylasso favoring"], df_sum$avg[df_sum$learner == "blockForest"]),
                   approach = c("Boosting", "Penalized regression", "Random forest"))
ggplot(data = df_sum %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray2[2])+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the sum over time")+
  facet_grid(. ~ approach, space="free_x", scales="free_x", switch="x")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()+
  geom_segment(data = ref, aes(x = x, y = y, xend = xend, yend = yend), colour = gray2[1], size = linesize)
dev.off()

### Mean
#pdf(file = "comp_bars_learner_mean_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ref <- data.frame(x = c("glmboost", "Lasso", "ranger"), xend = c("CoxBoost", "prioritylasso favoring", "blockForest"),
                  y = c(df_mean$avg[df_mean$learner == "glmboost"], df_mean$avg[df_mean$learner == "Lasso"], df_mean$avg[df_mean$learner == "ranger"]),
                  yend = c(df_mean$avg[df_mean$learner == "CoxBoost"], df_mean$avg[df_mean$learner == "prioritylasso favoring"], df_mean$avg[df_mean$learner == "blockForest"]),
                  approach = c("Boosting", "Penalized regression", "Random forest"))
ggplot(data = df_mean %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray2[2])+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the mean over time")+
  facet_grid(. ~ approach, space="free_x", scales="free_x", switch="x")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()+
  geom_segment(data = ref, aes(x = x, y = y, xend = xend, yend = yend), colour = gray2[1], size = linesize)
dev.off()

### Median
#pdf(file = "comp_bars_learner_median_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ref <- data.frame(x = c("glmboost", "Lasso", "ranger"), xend = c("CoxBoost", "grridge", "blockForest"),
                  y = c(df_median$avg[df_median$learner == "glmboost"], df_median$avg[df_median$learner == "Lasso"], df_median$avg[df_median$learner == "ranger"]),
                  yend = c(df_median$avg[df_median$learner == "CoxBoost"], df_median$avg[df_median$learner == "grridge"], df_median$avg[df_median$learner == "blockForest"]),
                  approach = c("Boosting", "Penalized regression", "Random forest"))
ggplot(data = df_median %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray2[2])+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the median over time")+
  facet_grid(. ~ approach, space="free_x", scales="free_x", switch="x")+
  facet_grid(. ~ approach, space="free_x", scales="free_x", switch="x")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()+
  geom_segment(data = ref, aes(x = x, y = y, xend = xend, yend = yend), colour = gray2[1], size = linesize)
dev.off()

### Minimum
#pdf(file = "comp_bars_learner_min_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ref <- data.frame(x = c("glmboost", "Lasso", "ranger"), xend = c("CoxBoost favoring", "grridge", "blockForest"),
                  y = c(df_min$avg[df_min$learner == "glmboost"], df_min$avg[df_min$learner == "Lasso"], df_min$avg[df_min$learner == "ranger"]),
                  yend = c(df_min$avg[df_min$learner == "CoxBoost favoring"], df_min$avg[df_min$learner == "grridge"], df_min$avg[df_min$learner == "blockForest"]),
                  approach = c("Boosting", "Penalized regression", "Random forest"))
ggplot(data = df_min %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray2[2])+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the minimum over time")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()+
  geom_segment(data = ref, aes(x = x, y = y, xend = xend, yend = yend), colour = gray2[1], size = linesize)
dev.off()

### Maximum
#pdf(file = "comp_bars_learner_max_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ref <- data.frame(x = c("glmboost", "Lasso", "ranger"), xend = c("CoxBoost favoring", "ipflasso", "blockForest"),
                  y = c(df_max$avg[df_max$learner == "glmboost"], df_max$avg[df_max$learner == "Lasso"], df_max$avg[df_max$learner == "ranger"]),
                  yend = c(df_max$avg[df_max$learner == "CoxBoost favoring"], df_max$avg[df_max$learner == "ipflasso"], df_max$avg[df_max$learner == "blockForest"]),
                  approach = c("Boosting", "Penalized regression", "Random forest"))
ggplot(data = df_max %>% arrange(avg) %>% mutate(learner = factor(learner, levels = learner)), aes(y = avg, x = learner)) +
  geom_col(fill = gray2[2])+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "datasets summarised by the maximum over time")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()+
  geom_segment(data = ref, aes(x = x, y = y, xend = xend, yend = yend), colour = gray2[1], size = linesize)
dev.off()

### Minimum Data set size
#pdf(file = "comp_bars_learner_minDat_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ref <- data.frame(x = c("glmboost", "Lasso", "ranger"), xend = c("CoxBoost favoring", "grridge", "blockForest"),
                  y = c(df_min_dat$mean[df_min_dat$learner == "glmboost"], df_min_dat$mean[df_min_dat$learner == "Lasso"], df_min_dat$mean[df_min_dat$learner == "ranger"]),
                  yend = c(df_min_dat$mean[df_min_dat$learner == "CoxBoost favoring"], df_min_dat$mean[df_min_dat$learner == "grridge"], df_min_dat$mean[df_min_dat$learner == "blockForest"]),
                  approach = c("Boosting", "Penalized regression", "Random forest"))
ggplot(data = df_min_dat %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner)) +
  geom_col(fill = gray2[2])+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "for the smallest dataset")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()+
  geom_segment(data = ref, aes(x = x, y = y, xend = xend, yend = yend), colour = gray2[1], size = linesize)
dev.off()

### Median Data set size
#pdf(file = "comp_bars_learner_medDat_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ref <- data.frame(x = c("glmboost", "Lasso", "ranger"), xend = c("CoxBoost favoring", "prioritylasso", "blockForest"),
                  y = c(df_med_dat$mean[df_med_dat$learner == "glmboost"], df_med_dat$mean[df_med_dat$learner == "Lasso"], df_med_dat$mean[df_med_dat$learner == "ranger"]),
                  yend = c(df_med_dat$mean[df_med_dat$learner == "CoxBoost favoring"], df_med_dat$mean[df_med_dat$learner == "prioritylasso"], df_med_dat$mean[df_med_dat$learner == "blockForest"]),
                  approach = c("Boosting", "Penalized regression", "Random forest"))
ggplot(data = df_med_dat %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner)) +
  geom_col(fill = gray2[2])+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "for the median dataset")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()+
  geom_segment(data = ref, aes(x = x, y = y, xend = xend, yend = yend), colour = gray2[1], size = linesize)
dev.off()

### Maximum Data set size
#pdf(file = "comp_bars_learner_maxDat_ref.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ref <- data.frame(x = c("CoxBoost", "Lasso", "ranger"), xend = c("glmboost", "grridge", "blockForest"),
                  y = c(df_max_dat$mean[df_max_dat$learner == "CoxBoost"], df_max_dat$mean[df_max_dat$learner == "Lasso"], df_max_dat$mean[df_max_dat$learner == "ranger"]),
                  yend = c(df_max_dat$mean[df_max_dat$learner == "glmboost"], df_max_dat$mean[df_max_dat$learner == "grridge"], df_max_dat$mean[df_max_dat$learner == "blockForest"]),
                  approach = c("Boosting", "Penalized regression", "Random forest"))
ggplot(data = df_max_dat %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner)) +
  geom_col(fill = gray2[2])+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "for the median dataset")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col),
        axis.ticks.x = element_line(size = axissize, color = axis_col))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  coord_flip()+
  geom_segment(data = ref, aes(x = x, y = y, xend = xend, yend = yend), colour = gray2[1], size = linesize)
dev.off()

rm(df_max, df_mean, df_median, df_min, df_max, df_sum, ref, df_max_dat, df_med_dat, df_min_dat)

#### 1.2.1.2 clustered Barchart----
## Data set size
##pdf(file = "C:/Uni/14. Semester/Masterarbeit/zu analysieren/Grafiken selbst gemacht/data visualisation/clustered barchart/clustbar_dataset.pdf", width = 4*4, height = 3*4)
df_sum <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(approach, task.id) %>% summarise(avg = sum(mean))
df_max <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(approach, task.id) %>% summarise(avg = max(mean))
df_mean <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(approach, task.id) %>% summarise(avg = mean(mean))
df_med <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(approach, task.id) %>% summarise(avg = median(mean))
df_min <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(approach, task.id) %>% summarise(avg = min(mean))
df_dat <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only") & task.id %in% c("LAML", "BRCA", "KIRC")) 

# Sum
#pdf(file = "clustbar_sum.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(df_sum, aes(y = avg, x = task.id, fill = approach))+
  geom_bar(stat = "identity", position = "dodge", color = "white")+
  theme_tufte(base_family = "sans")+
  scale_fill_manual(values = col_s, name = "Approach")+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = axis_col, size = axissize),
        legend.position = "bottom")+
  labs(x = "Data set", y = "Calculation time", title = "Calculation time by Dataset and Approach", subtitle = "learners summarised by the sum over time to approach")+
  coord_flip()
dev.off()

# Max
#pdf(file = "clustbar_max.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(df_max, aes(y = avg, x = task.id, fill = approach))+
  geom_bar(stat = "identity", position = "dodge", color = "white")+
  theme_tufte(base_family = "sans")+
  scale_fill_manual(values = col_s, name = "Approach")+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = axis_col, size = axissize),
        legend.position = "bottom")+
  labs(x = "Data set", y = "Calculation time", title = "Calculation time by Dataset and Approach", subtitle = "learners summarised by the maximum over time to approach")+
  coord_flip()
dev.off()

# Mean
#pdf(file = "clustbar_mean.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(df_mean, aes(y = avg, x = task.id, fill = approach))+
  geom_bar(stat = "identity", position = "dodge", color = "white")+
  theme_tufte(base_family = "sans")+
  scale_fill_manual(values = col_s, name = "Approach")+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = axis_col, size = axissize),
        legend.position = "bottom")+
  labs(x = "Data set", y = "Calculation time", title = "Calculation time by Dataset and Approach", subtitle = "learners summarised by the mean over time to approach")+
  coord_flip()
dev.off()

# Med
#pdf(file = "clustbar_median.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(df_med, aes(y = avg, x = task.id, fill = approach))+
  geom_bar(stat = "identity", position = "dodge", color = "white")+
  theme_tufte(base_family = "sans")+
  scale_fill_manual(values = col_s, name = "Approach")+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = axis_col, size = axissize),
        legend.position = "bottom")+
  labs(x = "Data set", y = "Calculation time", title = "Calculation time by Dataset and Approach", subtitle = "learners summarised by the median over time to approach")+
  coord_flip()
dev.off()

# Min
#pdf(file = "clustbar_min.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(df_min, aes(y = avg, x = task.id, fill = approach))+
  geom_bar(stat = "identity", position = "dodge", color = "white")+
  theme_tufte(base_family = "sans")+
  scale_fill_manual(values = col_s, name = "Approach")+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = axis_col, size = axissize),
        legend.position = "bottom")+
  labs(x = "Data set", y = "Calculation time", title = "Calculation time by Dataset and Approach", subtitle = "learners summarised by the minimum over time to approach")+
  coord_flip()
dev.off()

### Learner without subgroups
#pdf(file = "clustbar_Learner.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(df_dat %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner, fill = task.id))+
  geom_bar(stat = "identity", position = "dodge", color = "white")+
  theme_tufte(base_family = "sans")+
  scale_fill_manual(values = col_s, name = "Approach")+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = axis_col, size = axissize),
        legend.position = "bottom")+
  labs(x = "Data set", y = "Calculation time", title = "Calculation time by Learner", subtitle = "for smallest (LAML), median (KIRC) and biggest (BRCA) dataset")+
  coord_flip()
dev.off()

# Lerner with subgroups
#pdf(file = "clustbar_Learner_sub.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(df_dat %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner, fill = task.id))+
  geom_bar(stat = "identity", position = "dodge", color = "white")+
  theme_tufte(base_family = "sans")+
  scale_fill_manual(values = col_s, name = "Approach")+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = axis_col, size = axissize),
        legend.position = "bottom")+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  labs(x = "Data set", y = "Calculation time", title = "Calculation time by Learner", subtitle = "for smallest (LAML), median (KIRC) and biggest (BRCA) dataset")+
  coord_flip()
dev.off()

#### 1.2.1.3 connected dot plot----
# Learner
## Min Datset - Max Datset (ordinal)
df <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% filter(task.id %in% c("LAML", "BRCA"))
#pdf(file = "condotplot_dat_ord1.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(x = mean, y = learner, color = task.id))+
  geom_line(aes(group = learner), color = gray2[1], size = linesize)+
  geom_point(size = pointsize)+
  theme_tufte(base_family = "sans")+
  theme(panel.grid.major.y = element_line(linetype = 3, color = gray2[2]))+
  facet_grid(rows = vars(approach), space = "free_y", scales = "free_y", switch = "y")+
  theme(strip.placement = "outside",
        legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col))+
  labs(x = "Calculation time", y = "Learner", title = "Calculation time by Learner", subtitle = "for the smallest (LAML) and largest (BRCA) dataset")+
  #scale_x_continuous(expand = c(0,0))+
  scale_color_manual(values = c("LAML" = col_s[2], "BRCA" = col_s[3]), name = "Dataset")
dev.off()

#pdf(file = "condotplot_dat_ord2.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = df %>% arrange(mean) %>% mutate(learner = factor(learner, levels = learner)), aes(x = mean, y = learner, color = task.id))+
  geom_line(aes(group = learner), color = gray2[1], size = linesize)+
  geom_point(size = pointsize)+
  theme_tufte(base_family = "sans")+
  theme(panel.grid.major.y = element_line(linetype = 3, color = gray2[2]))+
  theme(strip.placement = "outside",
        legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = axissize, color = axis_col))+
  labs(x = "Calculation time", y = "Learner", title = "Calculation time by Learner", subtitle = "for the smallest (LAML) and largest (BRCA) dataset")+
  #scale_x_continuous(expand = c(0,0))+
  scale_color_manual(values = c("LAML" = col_s[2], "BRCA" = col_s[3]), name = "Dataset")
dev.off()

#### 1.2.1.4 Heatmap----
# Learner - Data set size
#pdf(file = "heatm_lea_dat_sub.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% arrange(mean) 
       %>% mutate(learner = factor(learner, levels = learner)), aes(y = task.id, x = learner, fill = mean))+
  geom_raster()+
  scale_fill_gradient(trans = "log10", breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                      labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000), 
                      low = "#C8E7FF",
                      high = col_h[2])+
  theme_tufte(base_family = "sans")+
  facet_grid(.~approach, space="free_x", scales="free_x", switch="x")+
  labs(x = "Learner", y = "Dataset", fill = "Calculation\ntime", title = "Calculation time by learner and dataset")+
  theme(strip.placement = "outside",
        legend.position = "bottom",
        axis.ticks = element_blank())
dev.off()

#pdf(file = "heatm_lea_dat.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% arrange(mean) 
       %>% mutate(learner = factor(learner, levels = learner)), aes(y = task.id, x = learner, fill = mean))+
  geom_raster()+
  scale_fill_gradient(trans = "log10", breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                      labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000), 
                      low = "#C8E7FF",
                      high = col_h[2])+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Dataset", fill = "Calculation\ntime", title = "Calculation time by learner and dataset")+
  theme(strip.placement = "outside",
        legend.position = "bottom",
        axis.ticks = element_blank())
dev.off()

# Data set size - Learner
#pdf(file = "heatm_dat_lea_sub.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% arrange(mean) 
       %>% mutate(learner = factor(learner, levels = learner)), aes(x = task.id, y = learner, fill = mean))+
  geom_raster()+
  scale_fill_gradient(trans = "log10", breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                      labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000), 
                      low = "#C8E7FF",
                      high = col_h[2])+
  theme_tufte(base_family = "sans")+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  labs(y = "Learner", x = "Dataset", fill = "Calculation\ntime", title = "Calculation time by dataset and learner")+
  theme(strip.placement = "outside",
        legend.position = "bottom",
        axis.ticks = element_blank())
dev.off()

#pdf(file = "heatm_dat_lea.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% arrange(mean) 
       %>% mutate(learner = factor(learner, levels = learner)), aes(x = task.id, y = learner, fill = mean))+
  geom_raster()+
  scale_fill_gradient(trans = "log10", breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                      labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000), 
                      low = "#C8E7FF",
                      high = col_h[2])+
  theme_tufte(base_family = "sans")+
  labs(y = "Learner", x = "Dataset", fill = "Calculation\ntime", title = "Calculation time by dataset and learner")+
  theme(strip.placement = "outside",
        legend.position = "bottom",
        axis.ticks = element_blank())
dev.off()

rm(df, df_dat, df_max, df_mean, df_med, df_min, df_sum)

#### 1.2.1.5 Matrix chart----
# learner - data set size
df <- tt_means
levels(df$learner) <- c("Kaplan-Meier", "Lasso", "glmboost", "CoxBoost", "Clinical only", "prioritylasso", "prioritylasso\nfavoring",
                        "CoxBoost\nfavoring", "grridge", "blockForest", "rfsrc", "ranger", "ipflasso")
#pdf(file = "matrixcha_lea_dat_sub.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(df %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% arrange(mean) 
       %>% mutate(learner = factor(learner, levels = learner)), aes(y = task.id, x = learner, size = mean))+
  geom_count(color = gray1)+
  scale_size_continuous(trans = "log10", breaks = c(10, 100, 1000, 10000, 100000),
                        labels = c(10, 100, 1000, 10000, 100000))+
  theme_tufte(base_family = "sans")+
  facet_grid(.~approach, space="free_x", scales="free_x", switch="x")+
  labs(x = "Learner", y = "Dataset", size = "Calculation\ntime", title = "Calculation time by learner and dataset")+
  theme(strip.placement = "outside",
        axis.ticks = element_blank(),
        legend.position = "bottom")
dev.off()

#pdf(file = "matrixcha_lea_dat.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(df %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% arrange(mean) 
       %>% mutate(learner = factor(learner, levels = learner)), aes(y = task.id, x = learner, size = mean))+
  geom_count(color = gray1)+
  scale_size_continuous(trans = "log10", breaks = c(10, 100, 1000, 10000, 100000),
                        labels = c(10, 100, 1000, 10000, 100000))+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Dataset", size = "Calculation\ntime", title = "Calculation time by learner and dataset")+
  theme(strip.placement = "outside",
        axis.ticks = element_blank(),
        legend.position = "bottom")
dev.off()

# data set size - learner
#pdf(file = "matrixcha_dat_lea_sub.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% arrange(mean) 
       %>% mutate(learner = factor(learner, levels = learner)), aes(x = task.id, y = learner, size = mean))+
  geom_count(color = gray1)+
  scale_size_continuous(trans = "log10", breaks = c(10, 100, 1000, 10000, 100000),
                        labels = c(10, 100, 1000, 10000, 100000))+
  theme_tufte(base_family = "sans")+
  facet_grid(approach ~ ., space="free_y", scales="free_y", switch="y")+
  labs(y = "Learner", x = "Dataset", size = "Calculation\ntime", title = "Calculation time by learner and dataset")+
  theme(strip.placement = "outside",
        axis.ticks = element_blank(),
        legend.position = "bottom")
dev.off()

#pdf(file = "matrixcha_dat_lea.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% arrange(mean) 
       %>% mutate(learner = factor(learner, levels = learner)), aes(x = task.id, y = learner, size = mean))+
  geom_count(color = gray1)+
  scale_size_continuous(trans = "log10", breaks = c(10, 100, 1000, 10000, 100000),
                        labels = c(10, 100, 1000, 10000, 100000))+
  theme_tufte(base_family = "sans")+
  labs(y = "Learner", x = "Dataset", size = "Calculation\ntime", title = "Calculation time by learner and dataset")+
  theme(strip.placement = "outside",
        axis.ticks = element_blank(),
        legend.position = "bottom")
dev.off()

### 1.2.2 Ranking----
#### 1.2.2.1 Boxplots----
# data set size
#pdf(file = "ranking_box_dataset.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = filter(tt_means, !learner %in% c("Kaplan-Meier", "Clinical only")), aes(y = mean, x = task.id)) +
  geom_boxplot(fill = gray1, outlier.colour = gray1, outlier.fill = gray1, outlier.alpha = 1)+
  geom_boxplot(aes(color = task.id),fatten = NULL, fill = NA, coef = 0, outlier.alpha = 0,
               show.legend = F)+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Calculation time", title = "Calculation time by data set", subtitle = "regarding learner")+
  scale_color_manual(values = rep(gray1, 18))+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = axis_col, size = axissize))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

# data set size - approach
#pdf(file = "ranking_box_dataset_approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(data = filter(tt_means, !learner %in% c("Kaplan-Meier", "Clinical only")), aes(y = task.id, x = mean, fill = approach)) +
  geom_boxplot(outlier.colour = gray1, outlier.fill = gray1, outlier.alpha = 1, orientation = "y")+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Calculation time", title = "Calculation time by data set", subtitle = "regarding learner")+
  scale_fill_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]))+
  scale_color_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]))+
  theme(axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = axis_col, size = axissize),
        legend.position = "bottom")+
  scale_x_continuous(breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                     labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000))+
  coord_trans(x = "log10")
dev.off()

#pdf(file = "ranking_box_learner_sub.pdf", width = 3*4, height = 2*4)
ggplot(data = tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% arrange(mean) %>% 
         mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner)) +
  geom_boxplot(fill = gray1, outlier.colour = gray1, outlier.fill = gray1, outlier.alpha = 1)+
  geom_boxplot(aes(color = learner),fatten = NULL, fill = NA, coef = 0, outlier.alpha = 0,
               show.legend = F)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "regarding datasets")+
  scale_color_manual(values = rep(gray1, 18))+
  facet_grid(approach ~ ., space = "free_y", scales = "free_y", switch = "y")+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = axis_col, size = axissize),
        legend.position = "bottom")+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

#pdf(file = "ranking_box_learner.pdf", width = 3*4, height = 2*4)
ggplot(data = tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% arrange(mean) %>% 
         mutate(learner = factor(learner, levels = learner)), aes(y = mean, x = learner)) +
  geom_boxplot(fill = gray1, outlier.colour = gray1, outlier.fill = gray1, outlier.alpha = 1)+
  geom_boxplot(aes(color = learner),fatten = NULL, fill = NA, coef = 0, outlier.alpha = 0,
               show.legend = F)+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Calculation time", title = "Calculation time by learner", subtitle = "regarding datasets")+
  scale_color_manual(values = rep(gray1, 18))+
  theme(strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = axis_col, size = axissize),
        legend.position = "bottom")+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()
dev.off()

### 1.2.3 Multiple distributions----
rm(df)
df_sum <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(approach, n) %>% summarise(avg = sum(mean))
df_max <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(approach, n) %>% summarise(avg = max(mean))
df_mean <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(approach, n) %>% summarise(avg = mean(mean))
df_med <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(approach, n) %>% summarise(avg = median(mean))
df_min <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(approach, n) %>% summarise(avg = min(mean))

w_sum <- log10(c(coef(lm(n ~ avg, data = df_sum[df_sum$approach == "Boosting",]))[2], 
  coef(lm(n ~ avg, data = df_sum[df_sum$approach == "Penalized regression",]))[2],
  coef(lm(n ~ avg, data = df_sum[df_sum$approach == "Random forest",]))[2])) 
  
#### 1.2.3.1 Lines----
# Sum
#pdf(file = "multdist_sum.pdf", width = 7 * sqrt(min(w_sum) * max(w_sum)), height = 7)
ggplot(df_sum, aes(y = n, x = avg, group = approach, color = approach))+
  geom_line(size = linesize)+
  theme_tufte(base_family = "sans")+
  #facet_wrap(. ~ approach, ncol = 1)+
  scale_x_continuous(breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                     labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000))+
  coord_trans(x = "log10")+
  labs(y = "Number of observations", x = "Calculation time", title = "Calculation time for different approaches",
       subtitle = "learners summarised by the sum over time to approach")+
  theme(
    axis.line = element_line(size = axissize, color = axis_col,),
    legend.position = "bottom"
  )+
  scale_color_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]),
                     name = "Approach")
dev.off()

# Max
#pdf(file = "multdist_max.pdf", width = 7 * sqrt(min(w_sum) * max(w_sum)), height = 7)
ggplot(df_max, aes(y = n, x = avg, group = approach, color = approach))+
  geom_line(size = linesize)+
  theme_tufte(base_family = "sans")+
  #facet_wrap(. ~ approach, ncol = 1)+
  scale_x_continuous(breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                     labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000))+
  coord_trans(x = "log10")+
  labs(y = "Number of observations", x = "Calculation time", title = "Calculation time for different approaches",
       subtitle = "learners summarised by the maximum over time to approach")+
  theme(
    axis.line = element_line(size = axissize, color = axis_col,),
    legend.position = "bottom"
  )+
  scale_color_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]),
                     name = "Approach")
dev.off()

# Mean
#pdf(file = "multdist_mean.pdf", width = 7 * sqrt(min(w_sum) * max(w_sum)), height = 7)
ggplot(df_mean, aes(y = n, x = avg, group = approach, color = approach))+
  geom_line(size = linesize)+
  theme_tufte(base_family = "sans")+
  #facet_wrap(. ~ approach, ncol = 1)+
  scale_x_continuous(breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                     labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000))+
  coord_trans(x = "log10")+
  labs(y = "Number of observations", x = "Calculation time", title = "Calculation time for different approaches",
       subtitle = "learners summarised by the mean over time to approach")+
  theme(
    axis.line = element_line(size = axissize, color = axis_col,),
    legend.position = "bottom"
  )+
  scale_color_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]),
                     name = "Approach")
dev.off()

# Median
#pdf(file = "multdist_median.pdf", width = 7 * sqrt(min(w_sum) * max(w_sum)), height = 7)
ggplot(df_med, aes(y = n, x = avg, group = approach, color = approach))+
  geom_line(size = linesize)+
  theme_tufte(base_family = "sans")+
  #facet_wrap(. ~ approach, ncol = 1)+
  scale_x_continuous(breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                     labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000))+
  coord_trans(x = "log10")+
  labs(y = "Number of observations", x = "Calculation time", title = "Calculation time for different approaches",
       subtitle = "learners summarised by the median over time to approach")+
  theme(
    axis.line = element_line(size = axissize, color = axis_col,),
    legend.position = "bottom"
  )+
  scale_color_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]),
                     name = "Approach")
dev.off()

# Min
#pdf(file = "multdist_min.pdf", width = 7 * sqrt(min(w_sum) * max(w_sum)), height = 7)
ggplot(df_min, aes(y = n, x = avg, group = approach, color = approach))+
  geom_line(size = linesize)+
  theme_tufte(base_family = "sans")+
  #facet_wrap(. ~ approach, ncol = 1)+
  scale_x_continuous(breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                     labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000))+
  coord_trans(x = "log10")+
  labs(y = "Number of observations", x = "Calculation time", title = "Calculation time for different approaches",
       subtitle = "learners summarised by the minimum over time to approach")+
  theme(
    axis.line = element_line(size = axissize, color = axis_col,),
    legend.position = "bottom"
  )+
  scale_color_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]),
                     name = "Approach")
dev.off()

rm(df_max, df_mean, df_med, df_min, df_sum, w_max, w_mean, w_med, w_min)

#### 1.2.3.2 Density----
w <- abs(log10(coef(lm(n ~ mean, data = tt_means))[2]))

# Through P spline
#pdf(file = "multdist_ps.pdf", width = 7 * sqrt(w), height = 7)
ggplot(tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")), aes(x = mean, y = n, color = approach, fill = approach))+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps"), se = F, size = linesize)+
  theme_tufte(base_family = "sans")+
  labs(y = "Number of observations", x = "Calculation time", title = "Calculation time for different Approaches", 
       subtitle = "smoothing estimated by penalised splines")+
  theme(axis.line = element_line(size = axissize, color = axis_col),
        legend.position = "bottom")+
  scale_color_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]),
                     name = "Approach")+
  scale_fill_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]),
                    name = "Approach")+
  scale_x_continuous(breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                     labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000))+
  coord_trans(x = "log10")
dev.off()

# Through Loess
#pdf(file = "multdist_loess.pdf", width = 7 * sqrt(w), height = 7)
ggplot(tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")), aes(x = mean, y = n, color = approach, fill = approach))+
  geom_smooth(method = "loess", formula = y ~ x, se = F, size = linesize)+
  theme_tufte(base_family = "sans")+
  labs(y = "Number of observations", x = "Calculation time", title = "Calculation time for different Approaches", 
       subtitle = "smoothing estimated by loess")+
  theme(axis.line = element_line(size = axissize, color = axis_col),
        legend.position = "bottom")+
  scale_color_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]),
                     name = "Approach")+
  scale_fill_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]),
                    name = "Approach")+
  scale_x_continuous(breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                     labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000))+
  coord_trans(x = "log10")
dev.off()

# Through P spline and reference
#pdf(file = "multdist_ps_ref.pdf", width = 7 * sqrt(w), height = 7)
ggplot(rbind(tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")),
             tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% mutate(approach = "Overall")),
       aes(x = mean, y = n, color = approach, linewidth = approach))+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps"), se = F, size = linesize)+
  theme_tufte(base_family = "sans")+
  labs(y = "Number of observations", x = "Calculation time", title = "Calculation time for different Approaches", 
       subtitle = "smoothing estimated by penalised splines")+
  theme(axis.line = element_line(size = axissize, color = axis_col),
        legend.position = "bottom")+
  scale_color_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4],
                                "Overall" = col_h[1]),
                     name = "Approach")+
  scale_discrete_manual("linewidth", values = c("Boosting" = linesize, "Penalized regression" = linesize, "Random forest" = linesize,
                                                "Overall" = linesize+.75))+
  guides(linewidth = "none")+
  scale_x_continuous(breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                     labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000))+
  coord_trans(x = "log10")
dev.off()

# Through Loess and Reference
#pdf(file = "multdist_loess_ref.pdf", width = 7 * sqrt(w), height = 7)
ggplot(rbind(tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")),
             tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% mutate(approach = "Overall")),
       aes(x = mean, y = n, color = approach, linewidth = approach))+
  geom_smooth(method = "loess", formula = y ~ x, se = F, size = linesize)+
  theme_tufte(base_family = "sans")+
  labs(y = "Number of observations", x = "Calculation time", title = "Calculation time for different Approaches", 
       subtitle = "smoothing estimated by loess")+
  theme(axis.line = element_line(size = axissize, color = axis_col),
        legend.position = "bottom")+
  scale_color_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4],
                                "Overall" = col_h[1]),
                     name = "Approach")+
  scale_discrete_manual("linewidth", values = c("Boosting" = linesize, "Penalized regression" = linesize, "Random forest" = linesize,
                                                "Overall" = linesize+.75))+
  guides(linewidth = "none")+
  scale_x_continuous(breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                     labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000))+
  coord_trans(x = "log10")
dev.off()


### 1.2.4. Correlation----
#### 1.2.4.1 Points----
# Through P spline
#pdf(file = "corr_ps.pdf", width = 7 * sqrt(w), height = 7)
ggplot(tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")), aes(x = mean, y = n, color = approach, fill = approach))+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps"), se = F, size = linesize)+
  geom_point(size = pointsize-3, alpha = .5, pch = 21)+
  theme_tufte(base_family = "sans")+
  labs(y = "Number of observations", x = "Calculation time", title = "Calculation time for different Approaches", 
       subtitle = "smoothing estimated by penalised splines")+
  theme(axis.line = element_line(size = axissize, color = axis_col),
        legend.position = "bottom")+
  scale_color_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]),
                     name = "Approach")+
  scale_fill_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]),
                    name = "Approach")+
  scale_x_continuous(breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                     labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000))+
  coord_trans(x = "log10")
dev.off()

# Through Loess
#pdf(file = "corr_loess.pdf", width = 7 * sqrt(w), height = 7)
ggplot(tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")), aes(x = mean, y = n, color = approach, fill = approach))+
  geom_smooth(method = "loess", formula = y ~ x, se = F, size = linesize)+
  geom_point(size = pointsize-3, alpha = .5)+
  theme_tufte(base_family = "sans")+
  labs(y = "Number of observations", x = "Calculation time", title = "Calculation time for different Approaches", 
       subtitle = "smoothing estimated by loess")+
  theme(axis.line = element_line(size = axissize, color = axis_col),
        legend.position = "bottom")+
  scale_color_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]),
                     name = "Approach")+
  scale_fill_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4]),
                    name = "Approach")+
  scale_x_continuous(breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                     labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000))+
  coord_trans(x = "log10")
dev.off()

# Through P spline and Reference
#pdf(file = "corr_ps_ref.pdf", width = 7 * sqrt(w), height = 7)
ggplot(rbind(tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")),
             tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% mutate(approach = "Overall")),
       aes(x = mean, y = n, color = approach, linewidth = approach))+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps"), se = F, size = linesize)+
  geom_point(size = pointsize-3, alpha = .5)+
  theme_tufte(base_family = "sans")+
  labs(y = "Number of observations", x = "Calculation time", title = "Calculation time for different Approaches", 
       subtitle = "smoothing estimated by penalised splines")+
  theme(axis.line = element_line(size = axissize, color = axis_col),
        legend.position = "bottom")+
  scale_color_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4],
                                "Overall" = col_h[1]),
                     name = "Approach")+
  scale_discrete_manual("linewidth", values = c("Boosting" = linesize, "Penalized regression" = linesize, "Random forest" = linesize,
                                                "Overall" = linesize+.75))+
  guides(linewidth = "none")+
  scale_x_continuous(breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                     labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000))+
  coord_trans(x = "log10")
dev.off()

# Through Loess and Reference
#pdf(file = "corr_loess_ref.pdf", width = 7 * sqrt(w), height = 7)
ggplot(rbind(tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")),
             tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% mutate(approach = "Overall")),
       aes(x = mean, y = n, color = approach, linewidth = approach))+
  geom_smooth(method = "loess", formula = y ~ x, se = F, size = linesize)+
  geom_point(size = pointsize-3, alpha = .5)+
  theme_tufte(base_family = "sans")+
  labs(y = "Number of observations", x = "Calculation time", title = "Calculation time for different Approaches", 
       subtitle = "smoothing estimated by loess")+
  theme(axis.line = element_line(size = axissize, color = axis_col),
        legend.position = "bottom")+
  scale_color_manual(values = c("Boosting" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4],
                                "Overall" = col_h[1]),
                     name = "Approach")+
  scale_discrete_manual("linewidth", values = c("Boosting" = linesize, "Penalized regression" = linesize, "Random forest" = linesize,
                                                "Overall" = linesize+.75))+
  guides(linewidth = "none")+
  scale_x_continuous(breaks = c((1:10)*10, (1:10)*100, (1:10)*1000, (1:10)*10000),
                     labels = c(rep("", 9), 100, rep("", 9), 1000, rep("", 9), 10000, rep("", 9), 100000))+
  coord_trans(x = "log10")
dev.off()


#### 1.2.4.2  Bars----
df_sum <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(task.id) %>% summarise(avg = sum(mean)) %>%
  left_join(y = unique(tt_means %>% select(task.id, n)), by = "task.id")
df_mean <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(task.id) %>% summarise(avg = mean(mean)) %>%
  left_join(y = unique(tt_means %>% select(task.id, n)), by = "task.id")
df_median <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(task.id) %>% summarise(avg = median(mean)) %>%
  left_join(y = unique(tt_means %>% select(task.id, n)), by = "task.id")
df_min <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(task.id) %>% summarise(avg = min(mean)) %>%
  left_join(y = unique(tt_means %>% select(task.id, n)), by = "task.id")
df_max <- tt_means %>% filter(!learner %in% c("Kaplan-Meier", "Clinical only")) %>% group_by(task.id) %>% summarise(avg = max(mean)) %>%
  left_join(y = unique(tt_means %>% select(task.id, n)), by = "task.id")

### Sum
#pdf(file = "bars_sum.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2])
plot_grid(ggdraw() + draw_label("Number of observations and calculation time by dataset\nlearners summarised by the sum over time",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01),
          plot_grid(
            ggplot(df_sum  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = n))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.ticks.y = element_blank())+
              labs(x = "Number of observations", y = "Dataset")+
              scale_x_continuous(expand = c(0,0)),
            ggplot(df_sum  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = avg))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank())+
              labs(x = "Calculation time")+
              scale_x_continuous(expand = c(0,0))
          ), rel_heights = c(.1,1), ncol = 1)
dev.off()


#pdf(file = "bars_sum_ref.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2])
plot_grid(ggdraw() + draw_label("Number of observations and calculation time by dataset\nlearners summarised by the sum over time",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01),
          plot_grid(
            ggplot(df_sum  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = n))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.ticks.y = element_blank())+
              labs(x = "Number of observations", y = "Dataset")+
              geom_segment(y = df_sum$task.id[df_sum$n == min(df_sum$n)],
                               yend = df_sum$task.id[df_sum$n == max(df_sum$n)],
                               x = df_sum$n[df_sum$n == min(df_sum$n)], 
                               xend = df_sum$n[df_sum$n == max(df_sum$n)], 
                           colour = gray2[1], size = linesize)+
              scale_x_continuous(expand = c(0,0)),
            ggplot(df_sum  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = avg))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank())+
              labs(x = "Calculation time")+
              geom_segment(y = df_sum$task.id[df_sum$n == min(df_sum$n)],
                           yend = df_sum$task.id[df_sum$n == max(df_sum$n)],
                           x = df_sum$avg[df_sum$n == min(df_sum$n)], 
                           xend = df_sum$avg[df_sum$n == max(df_sum$n)], 
                           colour = gray2[1], size = linesize)+
              scale_x_continuous(expand = c(0,0))
          ), rel_heights = c(.1,1), ncol = 1)
dev.off()

### Mean
#pdf(file = "bars_mean.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2])
plot_grid(ggdraw() + draw_label("Number of observations and calculation time by dataset\nlearners summarised by the mean over time",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01),
          plot_grid(
            ggplot(df_mean  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = n))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.ticks.y = element_blank())+
              labs(x = "Number of observations", y = "Dataset")+
              scale_x_continuous(expand = c(0,0)),
            ggplot(df_mean  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = avg))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank())+
              labs(x = "Calculation time")+
              scale_x_continuous(expand = c(0,0))
          ), rel_heights = c(.1,1), ncol = 1)
dev.off()


#pdf(file = "bars_mean_ref.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2])
plot_grid(ggdraw() + draw_label("Number of observations and calculation time by dataset\nlearners summarised by the mean over time",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01),
          plot_grid(
            ggplot(df_mean  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = n))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.ticks.y = element_blank())+
              labs(x = "Number of observations", y = "Dataset")+
              geom_segment(y = df_mean$task.id[df_mean$n == min(df_mean$n)],
                           yend = df_mean$task.id[df_mean$n == max(df_mean$n)],
                           x = df_mean$n[df_mean$n == min(df_mean$n)], 
                           xend = df_mean$n[df_mean$n == max(df_mean$n)], 
                           colour = gray2[1], size = linesize)+
              scale_x_continuous(expand = c(0,0)),
            ggplot(df_mean  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = avg))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank())+
              labs(x = "Calculation time")+
              geom_segment(y = df_mean$task.id[df_mean$n == min(df_mean$n)],
                           yend = df_mean$task.id[df_mean$n == max(df_mean$n)],
                           x = df_mean$avg[df_mean$n == min(df_mean$n)], 
                           xend = df_mean$avg[df_mean$n == max(df_mean$n)], 
                           colour = gray2[1], size = linesize)+
              scale_x_continuous(expand = c(0,0))
          ), rel_heights = c(.1,1), ncol = 1)
dev.off()


### Median
#pdf(file = "bars_median.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2])
plot_grid(ggdraw() + draw_label("Number of observations and calculation time by dataset\nlearners summarised by the median over time",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01),
          plot_grid(
            ggplot(df_median  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = n))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.ticks.y = element_blank())+
              labs(x = "Number of observations", y = "Dataset")+
              scale_x_continuous(expand = c(0,0)),
            ggplot(df_median  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = avg))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank())+
              labs(x = "Calculation time")+
              scale_x_continuous(expand = c(0,0))
          ), rel_heights = c(.1,1), ncol = 1)
dev.off()


#pdf(file = "bars_median_ref.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2])
plot_grid(ggdraw() + draw_label("Number of observations and calculation time by dataset\nlearners summarised by the median over time",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01),
          plot_grid(
            ggplot(df_median  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = n))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.ticks.y = element_blank())+
              labs(x = "Number of observations", y = "Dataset")+
              geom_segment(y = df_median$task.id[df_median$n == min(df_median$n)],
                           yend = df_median$task.id[df_median$n == max(df_median$n)],
                           x = df_median$n[df_median$n == min(df_median$n)], 
                           xend = df_median$n[df_median$n == max(df_median$n)], 
                           colour = gray2[1], size = linesize)+
              scale_x_continuous(expand = c(0,0)),
            ggplot(df_median  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = avg))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank())+
              labs(x = "Calculation time")+
              geom_segment(y = df_median$task.id[df_median$n == min(df_median$n)],
                           yend = df_median$task.id[df_median$n == max(df_median$n)],
                           x = df_median$avg[df_median$n == min(df_median$n)], 
                           xend = df_median$avg[df_median$n == max(df_median$n)], 
                           colour = gray2[1], size = linesize)+
              scale_x_continuous(expand = c(0,0))
          ), rel_heights = c(.1,1), ncol = 1)
dev.off()

### Max
#pdf(file = "bars_max.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2])
plot_grid(ggdraw() + draw_label("Number of observations and calculation time by dataset\nlearners summarised by the maximum over time",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01),
          plot_grid(
            ggplot(df_max  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = n))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.ticks.y = element_blank())+
              labs(x = "Number of observations", y = "Dataset")+
              scale_x_continuous(expand = c(0,0)),
            ggplot(df_max  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = avg))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank())+
              labs(x = "Calculation time")+
              scale_x_continuous(expand = c(0,0))
          ), rel_heights = c(.1,1), ncol = 1)
dev.off()


#pdf(file = "bars_max_ref.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2])
plot_grid(ggdraw() + draw_label("Number of observations and calculation time by dataset\nlearners summarised by the maximum over time",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01),
          plot_grid(
            ggplot(df_max  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = n))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.ticks.y = element_blank())+
              labs(x = "Number of observations", y = "Dataset")+
              geom_segment(y = df_min$task.id[df_min$n == min(df_min$n)],
                           yend = df_min$task.id[df_min$n == max(df_min$n)],
                           x = df_min$n[df_min$n == min(df_min$n)], 
                           xend = df_min$n[df_min$n == max(df_min$n)], 
                           colour = gray2[1], size = linesize)+
              scale_x_continuous(expand = c(0,0)),
            ggplot(df_max  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = avg))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank())+
              labs(x = "Calculation time")+
              geom_segment(y = df_max$task.id[df_max$n == min(df_max$n)],
                           yend = df_max$task.id[df_max$n == max(df_max$n)],
                           x = df_max$avg[df_max$n == min(df_max$n)], 
                           xend = df_max$avg[df_max$n == max(df_max$n)], 
                           colour = gray2[1], size = linesize)+
              scale_x_continuous(expand = c(0,0))
          ), rel_heights = c(.1,1), ncol = 1)
dev.off()

### Min
#pdf(file = "bars_min.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2])
plot_grid(ggdraw() + draw_label("Number of observations and calculation time by dataset\nlearners summarised by the minimum over time",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01),
          plot_grid(
            ggplot(df_min  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = n))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.ticks.y = element_blank())+
              labs(x = "Number of observations", y = "Dataset")+
              scale_x_continuous(expand = c(0,0)),
            ggplot(df_min  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = avg))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank())+
              labs(x = "Calculation time")+
              scale_x_continuous(expand = c(0,0))
          ), rel_heights = c(.1,1), ncol = 1)
dev.off()


#pdf(file = "bars_min_ref.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2])
plot_grid(ggdraw() + draw_label("Number of observations and calculation time by dataset\nlearners summarised by the minimum over time",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01),
          plot_grid(
            ggplot(df_min  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = n))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.ticks.y = element_blank())+
              labs(x = "Number of observations", y = "Dataset")+
              geom_segment(y = df_min$task.id[df_min$n == min(df_min$n)],
                           yend = df_min$task.id[df_min$n == max(df_min$n)],
                           x = df_min$n[df_min$n == min(df_min$n)], 
                           xend = df_min$n[df_min$n == max(df_min$n)], 
                           colour = gray2[1], size = linesize)+
              scale_x_continuous(expand = c(0,0)),
            ggplot(df_min  %>% arrange(n) %>% mutate(task.id = factor(task.id, levels = unique(task.id))),
                   aes(y = task.id, x = avg))+
              geom_col(fill = gray2[2])+
              theme_tufte(base_family = "sans")+
              theme(axis.line.x = element_line(colour = axis_col, size = axissize),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank())+
              labs(x = "Calculation time")+
              geom_segment(y = df_min$task.id[df_min$n == min(df_min$n)],
                           yend = df_min$task.id[df_min$n == max(df_min$n)],
                           x = df_min$avg[df_min$n == min(df_min$n)], 
                           xend = df_min$avg[df_min$n == max(df_min$n)], 
                           colour = gray2[1], size = linesize)+
              scale_x_continuous(expand = c(0,0))
          ), rel_heights = c(.1,1), ncol = 1)
dev.off()

