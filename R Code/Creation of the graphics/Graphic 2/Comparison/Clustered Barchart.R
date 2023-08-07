# 0. librarys----
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(cowplot)
library(ggthemes)

# 1. loading datasets----
load(file = "data.RData")
load(file = "stats.RData")

# 1.1 Parameter ----
scale_fill1 <- c("LAML" = col_s[2], "KIRC" = col_s[3], "BRCA" = col_s[4])
scale_fill2 <- c("Reference" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4], "Boosting" = col_s[5])
df$approach <- factor(df$approach, levels = c("Reference",
                                   "Random forest",
                                   "Penalized regression",
                                   "Boosting"))
df$task.id <- factor(df$task.id, levels = unique(df$task.id[order(df$n)]))
df$cv_mean_spars[is.na(df$cv_mean_spars)] <- 0

# 2. Functions----
## 2.1 Median----
mymedian <- function(lst) {
  n <- length(lst)
  s <- sort(lst)
  ifelse(n%%2==1,s[(n+1)/2],s[floor((n+1)/2)])
}

## 2.2 graphs----
datset_fig <- function(dataset1, dataset2, dataset3, label, label.x, label.legend){
  p1 <- ggplot(data = dataset1,
               aes(y = cv_mean_cindex, x = learner.id, fill = task.id))+
    geom_col(position = "dodge", color = "white")+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset", y = "Cindex")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          legend.position = "none",
          axis.text.x = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_blank())+
    scale_y_continuous(expand = c(0,0))+
    scale_fill_manual(values = scale_fill1)+
    facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")
  
  
  p2 <- ggplot(data = dataset2,
               aes(y = cv_mean_ibrier, x = learner.id, fill = task.id))+
    geom_col(position = "dodge", color = "white")+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset", y = "Ibrier")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          strip.placement = "outside",
          legend.position = "none",
          axis.text.x = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_blank())+
    scale_y_continuous(expand = c(0,0))+
    scale_fill_manual(values = scale_fill1)+
    facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")
  
  p3 <- ggplot(data = dataset3 ,
               aes(y = cv_mean_spars, x = learner.id, fill = task.id))+
    geom_col(position = "dodge", color = "white")+
    theme_tufte(base_family = "sans")+
    labs(x = label.x, y = "No. of features")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          strip.placement = "outside",
          legend.position = "bottom")+
    scale_y_continuous(expand = c(0,0))+
    scale_fill_manual(values = scale_fill1, name = label.legend)+
    facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")
  
  plot_grid(ggdraw() + draw_label(label,
                                  size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
            rel_heights = c(.1,1), ncol = 1)
}

datset_approach_fig <- function(dataset1, dataset2, dataset3, label, label.x, label.legend){
  p1 <- ggplot(data = dataset1,
               aes(y = cv_mean_cindex, x = approach, fill = task.id))+
    geom_col(position = "dodge", color = "white")+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset", y = "Cindex")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          legend.position = "none",
          axis.text.x = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_blank())+
    scale_y_continuous(expand = c(0,0))+
    scale_fill_manual(values = scale_fill1)
  
  
  p2 <- ggplot(data = dataset2,
               aes(y = cv_mean_ibrier, x = approach, fill = task.id))+
    geom_col(position = "dodge", color = "white")+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset", y = "Ibrier")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          strip.placement = "outside",
          legend.position = "none",
          axis.text.x = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_blank())+
    scale_y_continuous(expand = c(0,0))+
    scale_fill_manual(values = scale_fill1)
  
  p3 <- ggplot(data = dataset3 ,
               aes(y = cv_mean_spars, x = approach, fill = task.id))+
    geom_col(position = "dodge", color = "white")+
    theme_tufte(base_family = "sans")+
    labs(x = label.x, y = "No. of features")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          strip.placement = "outside",
          legend.position = "bottom")+
    scale_y_continuous(expand = c(0,0))+
    scale_fill_manual(values = scale_fill1, name = label.legend)
  
  plot_grid(ggdraw() + draw_label(label,
                                  size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
            rel_heights = c(.1,1), ncol = 1)
}

datset_approach_fig2 <- function(dataset1, dataset2, dataset3, label, label.x, label.legend){
  p1 <- ggplot(data = dataset1,
               aes(y = cv_mean_cindex, x = task.id, fill = approach))+
    geom_col(position = "dodge", color = "white")+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset", y = "Cindex")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          legend.position = "none",
          axis.text.x = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_blank())+
    scale_y_continuous(expand = c(0,0))+
    scale_fill_manual(values = scale_fill2)
  
  
  p2 <- ggplot(data = dataset2,
               aes(y = cv_mean_ibrier, x = task.id, fill = approach))+
    geom_col(position = "dodge", color = "white")+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset", y = "Ibrier")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          strip.placement = "outside",
          legend.position = "none",
          axis.text.x = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_blank())+
    scale_y_continuous(expand = c(0,0))+
    scale_fill_manual(values = scale_fill2)
  
  p3 <- ggplot(data = dataset3 ,
               aes(y = cv_mean_spars, x = task.id, fill = approach))+
    geom_col(position = "dodge", color = "white")+
    theme_tufte(base_family = "sans")+
    labs(x = label.x, y = "No. of features")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          strip.placement = "outside",
          legend.position = "bottom")+
    scale_y_continuous(expand = c(0,0))+
    scale_fill_manual(values = scale_fill2, name = label.legend)
  
  plot_grid(ggdraw() + draw_label(label,
                                  size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
            rel_heights = c(.1,1), ncol = 1)
}




# 3. x: learner, y = performance, color = dataset----
## 3.1 Cindex----
### 3.1.1 Minimum----
df_cmin <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1)

#pdf(file = "learner_dataset_cmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_cmin, df_cmin, df_cmin ,"Performance measures\nlearners summarised by the smallest cindex\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
           "Learner", "Dataset")
dev.off()

### 3.1.2 Maximum----
df_cmax <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1)

#pdf(file = "learner_dataset_cmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_cmax, df_cmax, df_cmax ,"Performance measures\nlearners summarised by the biggest cindex\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
           "Learner", "Dataset")
dev.off()

### 3.1.3 Median----
df_cmed <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% filter(1:n() == 1)

#pdf(file = "learner_dataset_cmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_cmed, df_cmed, df_cmed ,"Performance measures\nlearners summarised by the median cindex\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
           "Learner", "Dataset")
dev.off()

rm(df_cmax, df_cmed, df_cmin)
## 3.2 Ibrier----
### 3.2.1 Minimum----
df_imin <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1)

#pdf(file = "learner_dataset_imin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_imin, df_imin, df_imin ,"Performance measures\nlearners summarised by the smallest ibrier\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
           "Learner", "Dataset")
dev.off()

### 3.2.2 Maximum----
df_imax <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1)

#pdf(file = "learner_dataset_imax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_imax, df_imax, df_imax ,"Performance measures\nlearners summarised by the biggest ibrier\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
           "Learner", "Dataset")
dev.off()

### 3.2.3 Median----
df_imed <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1)

#pdf(file = "learner_dataset_imed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_imed, df_imed, df_imed ,"Performance measures\nlearners summarised by the median ibrier\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
           "Learner", "Dataset")
dev.off()

rm(df_imax, df_imed, df_imin)

## 3.3 Spars----
### 3.3.1 Minimum----
df_smin <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_spars == min(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "learner_dataset_smin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_smin, df_smin, df_smin ,"Performance measures\nlearners summarised by the smallest no. of features\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
           "Learner", "Dataset")
dev.off()

### 3.3.2 Maximum----
df_smax <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_spars == max(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "learner_dataset_smax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_smax, df_smax, df_smax ,"Performance measures\nlearners summarised by the biggest no. of features\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
           "Learner", "Dataset")
dev.off()

### 3.3.3 Median----
df_smed <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_spars == mymedian(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "learner_dataset_smed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_smed, df_smed, df_smed ,"Performance measures\nlearners summarised by the median no. of features\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
           "Learner", "Dataset")
dev.off()

rm(df_smax, df_smed, df_smin)

## 3.4 Summary----
### 3.4.1 Minimum----
df_cmin <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1)

df_imin <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1)

df_smin <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_spars == min(cv_mean_spars)) %>% filter(1:n() == 1)


#pdf(file = "learner_dataset_allmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_cmin, df_imin, df_smin ,"Performance measures\nlearners summarised by the smallest cindex, smallest ibrier and smallest no. of features\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
           "Learner", "Dataset")
dev.off()

rm(df_cmin, df_imin, df_smin)

### 3.4.2 Maximum----
df_cmax <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1)

df_imax <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1)

df_smax <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_spars == max(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "learner_dataset_allmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_cmax, df_imax, df_smax ,"Performance measures\nlearners summarised by the biggest cindex, biggest ibrier and biggest no. of features\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
           "Learner", "Dataset")
dev.off()

rm(df_cmax, df_imax, df_smax)

### 3.4.3 Median----
df_cmed <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% filter(1:n() == 1)

df_imed <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1)

df_smed <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% filter(cv_mean_spars == mymedian(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "learner_dataset_allmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_cmed, df_imed, df_smed ,"Performance measures\nlearners summarised by the median cindex, median ibrier and median no. of features\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
           "Learner", "Dataset")
dev.off()

rm(df_cmed, df_imed, df_smed)

### 3.4.3 mean----
df_cmean <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% mutate(cv_mean_cindex = mean(cv_mean_cindex)) %>% filter(1:n() == 1)

df_imean <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% mutate(cv_mean_ibrier = mean(cv_mean_ibrier)) %>% filter(1:n() == 1)

df_smean <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(learner.id, task.id) %>% mutate(cv_mean_spars = mean(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "learner_dataset_allmean.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_cmean, df_imean, df_smean ,"Performance measures\nlearners summarised by the mean cindex, mean ibrier and mean no. of features\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
           "Learner", "Dataset")
dev.off()

rm(df_cmean, df_imean, df_smean)

# 4. x: approach, y = Performance, color = dataset----
## 4.1 Cindex----
### 4.1.1 Minimum----
df_cmin <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1)

#pdf(file = "approach_dataset_cmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig(df_cmin, df_cmin, df_cmin ,"Performance measures\napproach summarised by the smallest cindex\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
           "Approach", "Dataset")
dev.off()
rm(df_cmin)

### 4.1.2 Maximum----
df_cmax <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1)

#pdf(file = "approach_dataset_cmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig(df_cmax, df_cmax, df_cmax ,"Performance measures\napproach summarised by the biggest cindex\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
                    "Approach", "Dataset")
dev.off()
rm(df_cmax)


### 4.1.3 Median----
df_cmed <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% filter(1:n() == 1)

#pdf(file = "approach_dataset_cmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig(df_cmed, df_cmed, df_cmed ,"Performance measures\napproach summarised by the median cindex\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
                    "Approach", "Dataset")
dev.off()
rm(df_cmed)

## 4.2 ibrier----
### 4.2.1 Minimum----
df_imin <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1)

#pdf(file = "approach_dataset_imin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig(df_imin, df_imin, df_imin ,"Performance measures\napproach summarised by the smallest ibrier\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
                    "Approach", "Dataset")
dev.off()
rm(df_imin)

### 4.2.2 Maximum----
df_imax <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1)

#pdf(file = "approach_dataset_imax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig(df_imax, df_imax, df_imax ,"Performance measures\napproach summarised by the biggest ibrier\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
                    "Approach", "Dataset")
dev.off()
rm(df_imax)

### 4.2.3 Median----
df_imed <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1)

#pdf(file = "approach_dataset_imed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig(df_imed, df_imed, df_imed ,"Performance measures\napproach summarised by the median ibrier\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
                    "Approach", "Dataset")
dev.off()
rm(df_imed)

## 4.3 spars----
### 4.3.1 Minimum----
df_smin <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_spars == min(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "approach_dataset_smin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig(df_smin, df_smin, df_smin ,"Performance measures\napproach summarised by the smallest no. of features\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
                    "Approach", "Dataset")
dev.off()
rm(df_smin)

### 4.3.2 Maximum----
df_smax <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_spars == max(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "approach_dataset_smax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig(df_smax, df_smax, df_smax ,"Performance measures\napproach summarised by the biggest no. of features\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
                    "Approach", "Dataset")
dev.off()
rm(df_smax)

### 4.3.3 Median----
df_smed <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_spars == mymedian(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "approach_dataset_smed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig(df_smed, df_smed, df_smed ,"Performance measures\napproach summarised by the median no. of features\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
                    "Approach", "Dataset")
dev.off()
rm(df_smed)

## 4.4 Summary----
### 4.4.1 Minimum----
df_cmin <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1)

df_imin <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1)

df_smin <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_spars == min(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "approach_dataset_allemin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig(df_cmin, df_imin, df_smin ,"Performance measures\napproach summarised by the smallest cindex, smallest ibrier and smallest no. of features\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
                    "Approach", "Dataset")
dev.off()
rm(df_cmin, df_imin, df_smin)

### 4.4.2 Maximum----
df_cmax <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1)

df_imax <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1)

df_smax <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_spars == max(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "approach_dataset_allemax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig(df_cmax, df_imax, df_smax ,"Performance measures\napproach summarised by the biggest cindex, biggest ibrier and biggest no. of features\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
                    "Approach", "Dataset")
dev.off()
rm(df_cmax, df_imax, df_smax)

### 4.4.3 Median----
df_cmed <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% filter(1:n() == 1)

df_imed <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1)

df_smed <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% filter(cv_mean_spars == mymedian(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "approach_dataset_allemed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig(df_cmed, df_imed, df_smed ,"Performance measures\napproach summarised by the median cindex, median ibrier and median no. of features\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
                    "Approach", "Dataset")
dev.off()
rm(df_cmed, df_imed, df_smed)

### 4.4.4 Mean----
df_cmean <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% mutate(cv_mean_cindex = mean(cv_mean_cindex)) %>% filter(1:n() == 1)

df_imean <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% mutate(cv_mean_ibrier = mean(cv_mean_ibrier)) %>% filter(1:n() == 1)

df_smean <- df %>%
  filter(task.id %in% c("LAML", "KIRC", "BRCA")) %>% 
  group_by(approach, task.id) %>% mutate(cv_mean_spars = mean(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "approach_dataset_allemean.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig(df_cmean, df_imean, df_smean ,"Performance measures\napproach summarised by the mean cindex, mean ibrier and mean no. of features\nfor smallest (LAML), median (KIRC) and biggest (BRCA) dataset",
                    "Approach", "Dataset")
dev.off()
rm(df_cmean, df_imean, df_smean)

# 5. x: Dataset, y = Performance, color = approach----
## 5.1 Cindex----
### 5.1.1 Minimum----
df_cmin <- df %>%
  group_by(task.id, approach) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1)

#pdf(file = "dataset_approach_cmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig2(df_cmin, df_cmin, df_cmin ,"Performance measures\nfor the smallest cindex given the dataset and approach",
                    "Dataset", "Approach")
dev.off()
rm(df_cmin)

### 5.1.2 Maximum----
df_cmax <- df %>%
  group_by(task.id, approach) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1)

#pdf(file = "dataset_approach_cmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig2(df_cmax, df_cmax, df_cmax ,"Performance measures\nfor the biggest cindex given the dataset and approach",
                     "Dataset", "Approach")
dev.off()
rm(df_cmax)

### 5.1.3 Median----
df_cmed <- df %>%
  group_by(task.id, approach) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% filter(1:n() == 1)

#pdf(file = "dataset_approach_cmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig2(df_cmed, df_cmed, df_cmed ,"Performance measures\nfor the median cindex given the dataset and approach",
                     "Dataset", "Approach")
dev.off()
rm(df_cmed)

## 5.2 Ibrier----
### 5.2.1 Minimum----
df_imin <- df %>%
  group_by(task.id, approach) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1)

#pdf(file = "dataset_approach_imin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig2(df_imin, df_imin, df_imin ,"Performance measures\nfor the smallest ibrier given the dataset and approach",
                     "Dataset", "Approach")
dev.off()
rm(df_imin)

### 5.2.2 Maximum----
df_imax <- df %>%
  group_by(task.id, approach) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1)

#pdf(file = "dataset_approach_imax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig2(df_imax, df_imax, df_imax ,"Performance measures\nfor the biggest ibrier given the dataset and approach",
                     "Dataset", "Approach")
dev.off()
rm(df_imax)

### 5.2.3 Median----
df_imed <- df %>%
  group_by(task.id, approach) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1)

#pdf(file = "dataset_approach_imed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig2(df_imed, df_imed, df_imed ,"Performance measures\nfor the median ibrier given the dataset and approach",
                     "Dataset", "Approach")
dev.off()
rm(df_imed)

## 5.3 Spars----
### 5.3.1 Minimum----
df_smin <- df %>%
  group_by(task.id, approach) %>% filter(cv_mean_spars == min(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "dataset_approach_smin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig2(df_smin, df_smin, df_smin ,"Performance measures\nfor the smallest no. of features given the dataset and approach",
                     "Dataset", "Approach")
dev.off()
rm(df_smin)

### 5.3.2 Maximum----
df_smax <- df %>%
  group_by(task.id, approach) %>% filter(cv_mean_spars == max(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "dataset_approach_smax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig2(df_smax, df_smax, df_smax ,"Performance measures\nfor the biggest no. of features given the dataset and approach",
                     "Dataset", "Approach")
dev.off()
rm(df_smax)

### 5.3.3 Median----
df_smed <- df %>%
  group_by(task.id, approach) %>% filter(cv_mean_spars == mymedian(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "dataset_approach_smed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig2(df_smed, df_smed, df_smed ,"Performance measures\nfor the median no. of features given the dataset and approach",
                     "Dataset", "Approach")
dev.off()
rm(df_smed)

## 5.4 Summary----
### 5.4.1 Minimum----
df_cmin <- df %>%
  group_by(task.id, approach) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1)

df_imin <- df %>%
  group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1)

df_smin <- df %>%
  group_by(approach, task.id) %>% filter(cv_mean_spars == min(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "dataset_approach_allemin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig2(df_cmin, df_imin, df_smin, "Performance measures\nfor the smallest cindex, smallest ibrier, smallest no. of features given the dataset and approach",
                     "Dataset", "Approach")
dev.off()

rm(df_cmin, df_imin, df_smin)

### 5.4.2 Maximum----
df_cmax <- df %>%
  group_by(task.id, approach) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1)

df_imax <- df %>%
  group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1)

df_smax <- df %>%
  group_by(approach, task.id) %>% filter(cv_mean_spars == max(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "dataset_approach_allemax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig2(df_cmax, df_imax, df_smax, "Performance measures\nfor the biggest cindex, biggest ibrier, biggest no. of features given the dataset and approach",
                     "Dataset", "Approach")
dev.off()

rm(df_cmax, df_imax, df_smax)

### 5.4.3 Median----
df_cmed <- df %>%
  group_by(task.id, approach) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% filter(1:n() == 1)

df_imed <- df %>%
  group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1)

df_smed <- df %>%
  group_by(approach, task.id) %>% filter(cv_mean_spars == mymedian(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "dataset_approach_allemed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig2(df_cmed, df_imed, df_smed, "Performance measures\nfor the median cindex, median ibrier, median no. of features given the dataset and approach",
                     "Dataset", "Approach")
dev.off()

rm(df_cmed, df_imed, df_smed)

### 5.4.4 Mean----
df_cmean <- df %>%
  group_by(task.id, approach) %>% summarise(cv_mean_cindex = mean(cv_mean_cindex), .groups = "keep") %>% filter(1:n() == 1)

df_imean <- df %>%
  group_by(approach, task.id) %>% summarise(cv_mean_ibrier = mean(cv_mean_ibrier), .groups = "keep") %>% filter(1:n() == 1)

df_smean <- df %>%
  group_by(approach, task.id) %>% summarise(cv_mean_spars = mean(cv_mean_spars), .groups = "keep") %>% filter(1:n() == 1)

#pdf(file = "dataset_approach_allemean.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_approach_fig2(df_cmean, df_imean, df_smean, "Performance measures\nfor the mean cindex, mean ibrier, mean no. of features given the dataset and approach",
                     "Dataset", "Approach")
dev.off()

rm(df_cmean, df_imean, df_smean)

