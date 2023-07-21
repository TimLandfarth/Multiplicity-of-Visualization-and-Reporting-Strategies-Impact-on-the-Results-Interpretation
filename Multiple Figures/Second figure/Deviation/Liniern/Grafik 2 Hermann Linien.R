library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(cowplot)
library(ggthemes)

# 1. Daten laden----
load(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/data.RData")
load(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/stats.RData")

# 1.1 Parameter ----
scale_fill <-  c("block\nForest" = gray3[3],
                 "Cox\nproportional\nhazard" = gray3[3],
                 "CoxBoost" = gray3[3],
                 "CoxBoost\nfavoring" = gray3[3],
                 "glmboost" = gray3[3],
                 "grridge" = gray3[3],
                 "ipflasso" = gray3[3],
                 "Kaplan\nMeier" = gray3[1],
                 "Lasso" = gray3[3],
                 "priority\nLasso" = gray3[3],
                 "priority\nLasso\nfavoring" = gray3[3],
                 "ranger" = gray3[3],
                 "rfsrc" = gray3[3]
)

scale_fill_a <- c("Reference" = col_h[2], "Random forest" = col_s[3], "Penalized regression" = col_s[4], "Boosting" = col_s[5])
scale_linewidth_a <- c("Reference" = linesize + 1, "Random forest" = linesize, "Penalized regression" = linesize, "Boosting" = linesize)
df$approach <- factor(df$approach, levels = c("Reference",
                                              "Random forest",
                                              "Penalized regression",
                                              "Boosting"), ordered = T)
df$task.id <- factor(df$task.id, levels = unique(df$task.id[order(df$n)]))

# 2. Funktionen----
mymedian <- function(lst) {
  n <- length(lst)
  s <- sort(lst)
  ifelse(n%%2==1,s[(n+1)/2],s[floor((n+1)/2)])
}

fig1 <- function(dataset1, dataset2, label){
  d1 <- dataset1
  d2 <- dataset2
  
d1$dif_c <- NA

d1$dif_c[d1$approach == "Penalized regression"] <- d1$cv_mean_cindex[d1$approach == "Penalized regression"] - 
                     d1$cv_mean_cindex[d1$approach == "Reference"]

d1$dif_c[d1$approach == "Boosting"] <- d1$cv_mean_cindex[d1$approach == "Boosting"] - 
  d1$cv_mean_cindex[d1$approach == "Reference"]

d1$dif_c[d1$approach == "Random forest"] <- d1$cv_mean_cindex[d1$approach == "Random forest"] - 
  d1$cv_mean_cindex[d1$approach == "Reference"]

d1$dif_c[d1$approach == "Reference"] <- d1$cv_mean_cindex[d1$approach == "Reference"] - 
  d1$cv_mean_cindex[d1$approach == "Reference"]

d2$dif_i <- NA

d2$dif_i[d2$approach == "Penalized regression"] <- d2$cv_mean_ibrier[d2$approach == "Penalized regression"] - 
  d2$cv_mean_ibrier[d2$approach == "Reference"]

d2$dif_i[d2$approach == "Boosting"] <- d2$cv_mean_ibrier[d2$approach == "Boosting"] - 
  d2$cv_mean_ibrier[d2$approach == "Reference"]

d2$dif_i[d2$approach == "Random forest"] <- d2$cv_mean_ibrier[d2$approach == "Random forest"] - 
  d2$cv_mean_ibrier[d2$approach == "Reference"]

d2$dif_i[d2$approach == "Reference"] <- d2$cv_mean_ibrier[d2$approach == "Reference"] - 
  d2$cv_mean_ibrier[d2$approach == "Reference"]

limits_dif <- max(max(abs(d1$dif_c)), max(abs(d2$dif_i)))
limits_n <- max(max(abs(d1$cv_mean_cindex)), max(abs(d2$cv_mean_ibrier)))

p1 <- ggplot(d1, aes(x = n, y = cv_mean_cindex, color = approach, size = approach))+
  geom_line(show.legend = FALSE)+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Cindex", title = "Cindex")+
  theme(legend.position = "bottom",
        strip.placement = "outside",
        axis.title.x = element_blank(),
        axis.line = element_line(color = axis_col, size = axissize),
        plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = scale_fill_a)+
  scale_size_manual(values = scale_linewidth_a)+
  ylim(0, limits_n)

p2 <- ggplot(d1, aes(x = n, y = dif_c, color = approach, size = approach))+
  geom_line()+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Difference to reference")+
  theme(legend.position = "bottom",
        strip.placement = "outside",
        axis.title.x = element_blank(),
        axis.line = element_line(color = axis_col, size = axissize))+
  scale_color_manual(values = scale_fill_a, name = "Approach")+
  scale_size_manual(values = scale_linewidth_a, name = "Approach")+
  ylim(c(-limits_dif, limits_dif))

p3 <- ggplot(d2, aes(x = n, y = cv_mean_ibrier, color = approach, size = approach))+
  geom_line(show.legend = FALSE)+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Ibrier", title = "Ibrier")+
  theme(legend.position = "bottom",
        strip.placement = "outside",
        axis.title.x = element_blank(),
        axis.line = element_line(color = axis_col, size = axissize),
        plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = scale_fill_a)+
  scale_size_manual(values = scale_linewidth_a)+
  ylim(0, limits_n)

p4 <- ggplot(d2, aes(x = n, y = dif_i, color = approach, size = approach))+
  geom_line()+
  theme_tufte(base_family = "sans")+
  labs(x = "Dataset", y = "Difference to reference")+
  theme(legend.position = "bottom",
        strip.placement = "outside",
        axis.title.x = element_blank(),
        axis.line = element_line(color = axis_col, size = axissize))+
  scale_color_manual(values = scale_fill_a, name = "Approach")+
  scale_size_manual(values = scale_linewidth_a, name = "Approach")+
  ylim(c(-limits_dif, limits_dif))

plot_grid(ggdraw() + draw_label(label,
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p3, p2, p4, nrow = 2, align = "v"),
          rel_heights = c(.05,1), ncol = 1)
}


# 3. x: dataset, y: Mase, color: Approach, Ref: ergibt sich----
## 3.1 Cindex----
### 3.1.1 Minimum----
df_cmin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/cmin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_cmin, "Performance measures\nsummarised by minimum cindex conditional on approach and data set")
dev.off()

rm(df_cmin)

### 3.1.2 Median----
df_cmed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/cmed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_cmed, "Performance measures\nsummarised by median cindex conditional on approach and data set")
dev.off()

rm(df_cmed)

### 3.1.3 Maximum----
df_cmax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/cmax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_cmax, "Performance measures\nsummarised by maximum cindex conditional on approach and data set")
dev.off()

rm(df_cmax)

## 3.2 Ibrier----
### 3.2.1 Minimum----
df_imin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/imin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imin, df_imin, "Performance measures\nsummarised by minimum ibrier conditional on approach and data set")
dev.off()

rm(df_imin)

### 3.2.2 Median----
df_imed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/imed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imed, df_imed, "Performance measures\nsummarised by median ibrier conditional on approach and data set")
dev.off()

rm(df_imed)

### 3.2.3 Maximum----
df_imax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/imax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imax, df_imax, "Performance measures\nsummarised by maximum ibrier conditional on approach and data set")
dev.off()

rm(df_imax)


## 3.3 Zsf----
### 3.3.1 Minimum----
df_cmin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% ungroup()
df_imin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/cmin_imin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_imin, "Performance measures\nsummarised by minimum cindex and minimum ibrier conditional on approach and data set")
dev.off()

rm(df_imin, df_cmin)

### 3.3.2 Median----
df_cmed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% ungroup()
df_imed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/cmed_imed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_imed, "Performance measures\nsummarised by median cindex and median ibrier conditional on approach and data set")
dev.off()

rm(df_imed, df_cmed)

### 3.3.3 Maximum----
df_cmax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% ungroup()
df_imax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/cmax_imax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_imax, "Performance measures\nsummarised by maximum cindex and maximum ibrier conditional on approach and data set")
dev.off()

rm(df_imax, df_cmax)

### 3.3.4 Mean----
df_cmean <- df %>% group_by(approach, task.id) %>% mutate(cv_mean_cindex = mean(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()
df_imean <- df %>% group_by(approach, task.id) %>% mutate(cv_mean_ibrier = mean(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/cmean_imean.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmean, df_imean, "Performance measures\nsummarised by mean cindex and mean ibrier conditional on approach and data set")
dev.off()

rm(df_imean, df_cmean)

# 4. x: dataset, y: Mase, color: Approach, Ref: Kaplan Meier----
## 4.1 Cindex----
### 4.1.1 Minimum----
df_cmin <- df %>% filter(learner.id != "Cox\nproportional\nhazard") %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/km_cmin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_cmin, "Performance measures\nsummarised by minimum cindex for kaplan-meier and conditional on data set")
dev.off()

rm(df_cmin)

### 3.1.2 Median----
df_cmed <- df %>% filter(learner.id != "Cox\nproportional\nhazard") %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/km_cmed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_cmed, "Performance measures\nsummarised by median cindex for kaplan-meier and conditional on data set")
dev.off()

rm(df_cmed)

### 3.1.3 Maximum----
df_cmax <- df %>% filter(learner.id != "Cox\nproportional\nhazard") %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/km_cmax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_cmax, "Performance measures\nsummarised by maximum cindex for kaplan-meier and conditional on data set")
dev.off()

rm(df_cmax)

## 3.2 Ibrier----
### 3.2.1 Minimum----
df_imin <- df %>% filter(learner.id != "Cox\nproportional\nhazard") %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/km_imin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imin, df_imin, "Performance measures\nsummarised by minimum ibrier for kaplan-meier and conditional on data set")
dev.off()

rm(df_imin)

### 3.2.2 Median----
df_imed <- df %>% filter(learner.id != "Cox\nproportional\nhazard") %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/km_imed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imed, df_imed, "Performance measures\nsummarised by median ibrier for kaplan-meier and conditional on data set")
dev.off()

rm(df_imed)

### 3.2.3 Maximum----
df_imax <- df %>% filter(learner.id != "Cox\nproportional\nhazard") %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/km_imax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imax, df_imax, "Performance measures\nsummarised by maximum ibrier for kaplan-meier and conditional on data set")
dev.off()

rm(df_imax)


## 3.3 Zsf----
### 3.3.1 Minimum----
df_cmin <- df %>% filter(learner.id != "Cox\nproportional\nhazard") %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% ungroup()
df_imin <- df %>% filter(learner.id != "Cox\nproportional\nhazard") %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/km_cmin_imin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_imin, "Performance measures\nsummarised by minimum cindex and minimum ibrier for kaplan-meier and conditional on data set")
dev.off()

rm(df_imin, df_cmin)

### 3.3.2 Median----
df_cmed <- df %>% filter(learner.id != "Cox\nproportional\nhazard") %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% ungroup()
df_imed <- df %>% filter(learner.id != "Cox\nproportional\nhazard") %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/km_cmed_imed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_imed, "Performance measures\nsummarised by median cindex and median ibrier for kaplan-meier and conditional on data set")
dev.off()

rm(df_imed, df_cmed)

### 3.3.3 Maximum----
df_cmax <- df %>% filter(learner.id != "Cox\nproportional\nhazard") %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% ungroup()
df_imax <- df %>% filter(learner.id != "Cox\nproportional\nhazard") %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/km_cmax_imax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_imax, "Performance measures\nsummarised by maximum cindex and maximum ibrier for kaplan-meier and conditional on data set")
dev.off()

rm(df_imax, df_cmax)

### 3.3.4 Mean----
df_cmean <- df %>% filter(learner.id != "Cox\nproportional\nhazard") %>% group_by(approach, task.id) %>% mutate(cv_mean_cindex = mean(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()
df_imean <- df %>% filter(learner.id != "Cox\nproportional\nhazard") %>% group_by(approach, task.id) %>% mutate(cv_mean_ibrier = mean(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/km_cmean_imean.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmean, df_imean, "Performance measures\nsummarised by mean cindex and mean ibrier for kaplan-meier and conditional on data set")
dev.off()

rm(df_imean, df_cmean)


# 4. x: dataset, y: Mase, color: Approach, Ref: Cox proportional hazard----
## 4.1 Cindex----
### 4.1.1 Minimum----
df_cmin <- df %>% filter(learner.id != "Kaplan\nMeier") %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/ph_cmin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_cmin, "Performance measures\nsummarised by minimum cindex for Cox proportional hazard and conditional on data set")
dev.off()

rm(df_cmin)

### 3.1.2 Median----
df_cmed <- df %>% filter(learner.id != "Kaplan\nMeier") %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/ph_cmed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_cmed, "Performance measures\nsummarised by median cindex for Cox proportional hazard and conditional on data set")
dev.off()

rm(df_cmed)

### 3.1.3 Maximum----
df_cmax <- df %>% filter(learner.id != "Kaplan\nMeier") %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/ph_cmax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_cmax, "Performance measures\nsummarised by maximum cindex for Cox proportional hazard and conditional on data set")
dev.off()

rm(df_cmax)

## 3.2 Ibrier----
### 3.2.1 Minimum----
df_imin <- df %>% filter(learner.id != "Kaplan\nMeier") %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/ph_imin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imin, df_imin, "Performance measures\nsummarised by minimum ibrier for Cox proportional hazard and conditional on data set")
dev.off()

rm(df_imin)

### 3.2.2 Median----
df_imed <- df %>% filter(learner.id != "Kaplan\nMeier") %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/ph_imed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imed, df_imed, "Performance measures\nsummarised by median ibrier for Cox proportional hazard and conditional on data set")
dev.off()

rm(df_imed)

### 3.2.3 Maximum----
df_imax <- df %>% filter(learner.id != "Kaplan\nMeier") %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/ph_imax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imax, df_imax, "Performance measures\nsummarised by maximum ibrier for Cox proportional hazard and conditional on data set")
dev.off()

rm(df_imax)


## 3.3 Zsf----
### 3.3.1 Minimum----
df_cmin <- df %>% filter(learner.id != "Kaplan\nMeier") %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% ungroup()
df_imin <- df %>% filter(learner.id != "Kaplan\nMeier") %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/ph_cmin_imin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_imin, "Performance measures\nsummarised by minimum cindex and minimum ibrier for Cox proportional hazard and conditional on data set")
dev.off()

rm(df_imin, df_cmin)

### 3.3.2 Median----
df_cmed <- df %>% filter(learner.id != "Kaplan\nMeier") %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% ungroup()
df_imed <- df %>% filter(learner.id != "Kaplan\nMeier") %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/ph_cmed_imed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_imed, "Performance measures\nsummarised by median cindex and median ibrier for Cox proportional hazard and conditional on data set")
dev.off()

rm(df_imed, df_cmed)

### 3.3.3 Maximum----
df_cmax <- df %>% filter(learner.id != "Kaplan\nMeier") %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% ungroup()
df_imax <- df %>% filter(learner.id != "Kaplan\nMeier") %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/ph_cmax_imax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_imax, "Performance measures\nsummarised by maximum cindex and maximum ibrier for Cox proportional hazard and conditional on data set")
dev.off()

rm(df_imax, df_cmax)

### 3.3.4 Mean----
df_cmean <- df %>% filter(learner.id != "Kaplan\nMeier") %>% group_by(approach, task.id) %>% mutate(cv_mean_cindex = mean(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()
df_imean <- df %>% filter(learner.id != "Kaplan\nMeier") %>% group_by(approach, task.id) %>% mutate(cv_mean_ibrier = mean(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Deviation/Liniern/ph_cmean_imean.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmean, df_imean, "Performance measures\nsummarised by mean cindex and mean ibrier for Cox proportional hazard and conditional on data set")
dev.off()

rm(df_imean, df_cmean)


