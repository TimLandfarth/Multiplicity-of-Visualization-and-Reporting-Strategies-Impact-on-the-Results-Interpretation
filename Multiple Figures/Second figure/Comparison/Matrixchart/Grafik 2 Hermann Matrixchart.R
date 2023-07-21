# 0. librarys----
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
scale_fill1 <- c("LAML" = col_s[2], "KIRC" = col_s[3], "BRCA" = col_s[4])
scale_fill2 <- c("Reference" = col_s[2], "Penalized regression" = col_s[3], "Random forest" = col_s[4], "Boosting" = col_s[5])
df$approach <- factor(df$approach, levels = c("Reference",
                                              "Random forest",
                                              "Penalized regression",
                                              "Boosting"))
df$task.id <- factor(df$task.id, levels = unique(df$task.id[order(df$n)]))
df$cv_mean_spars[is.na(df$cv_mean_spars)] <- 0

# 2. Funktionen----
## 2.1 Median----
mymedian <- function(lst) {
  n <- length(lst)
  s <- sort(lst)
  ifelse(n%%2==1,s[(n+1)/2],s[floor((n+1)/2)])
}

# 2.2 Heatmap----
fig_1 <- function(datset1, datset2, datset3, label, label.x){
  
  p1 <- ggplot(datset1, aes(x = task.id, y = learner.id, size = cv_mean_cindex))+
    geom_count(color = gray1)+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset", fill = "Cindex")+
    theme(legend.position = "right",
          axis.ticks = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_blank(),
          axis.title.x = element_blank())+
    scale_size_continuous(limits = c(0,1))+
    facet_grid(approach~., space="free_y", scales="free_y", switch="y")
  
  p2 <- ggplot(datset2, aes(x = task.id, y = learner.id, size = cv_mean_ibrier))+
    geom_count(color = gray1)+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset",y = "Learner", fill = "Ibrier")+
    theme(legend.position = "right",
          axis.ticks = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_blank(),
          axis.title.x = element_blank())+
    scale_size_continuous(limits = c(0,1))+
    facet_grid(approach~., space="free_y", scales="free_y", switch="y")
  
  p3 <- ggplot(datset3, aes(x = task.id, y = learner.id, size = cv_mean_spars))+
    geom_count(color = gray1)+
    theme_tufte(base_family = "sans")+
    labs(x = label.x, fill = "No. of\nfeatures")+
    theme(legend.position = "right",
          axis.ticks = element_blank(),
          strip.placement = "outside")+
    facet_grid(approach~., space="free_y", scales="free_y", switch="y")
  
  plot_grid(ggdraw() + draw_label(label,
                                  size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
            rel_heights = c(.05,1), ncol = 1)
}

fig_2 <- function(datset1, datset2, datset3, label, label.x){
  
  p1 <- ggplot(datset1, aes(x = task.id, y = approach, size = cv_mean_cindex))+
    geom_count(color = gray1)+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset", fill = "Cindex")+
    theme(legend.position = "right",
          axis.ticks = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())+
    scale_size_continuous(limits = c(0,1))
  
  p2 <- ggplot(datset2, aes(x = task.id, y = approach, size = cv_mean_ibrier))+
    geom_count(color = gray1)+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset",y = "Approach", fill = "Ibrier")+
    theme(legend.position = "right",
          axis.ticks = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_blank(),
          axis.title.x = element_blank())+
    scale_size_continuous(limits = c(0,1))
  
  p3 <- ggplot(datset3, aes(x = task.id, y = approach, size = cv_mean_spars))+
    geom_count(color = gray1)+
    theme_tufte(base_family = "sans")+
    labs(x = label.x, fill = "No. of\nfeatures")+
    theme(legend.position = "right",
          axis.ticks = element_blank(),
          strip.placement = "outside",
          axis.title.y = element_blank())
  
  plot_grid(ggdraw() + draw_label(label,
                                  size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
            rel_heights = c(.05,1), ncol = 1)
}

fig_3 <- function(datset1, datset2, datset3, label, label.x){
  
  p1 <- ggplot(datset1, aes(y = task.id, x = learner.id, size = cv_mean_cindex))+
    geom_count(color = gray1)+
    theme_tufte(base_family = "sans")+
    labs(y = "Dataset", fill = "Cindex")+
    theme(legend.position = "right",
          axis.ticks = element_blank(),
          strip.placement = "none",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_blank())+
    scale_size_continuous(limits = c(0,1))+
    facet_grid(.~approach, space="free_x", scales="free_x", switch="x")
  
  p2 <- ggplot(datset2, aes(y = task.id, x = learner.id, size = cv_mean_ibrier))+
    geom_count(color = gray1)+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset",y = "Dataset", fill = "Ibrier")+
    theme(legend.position = "right",
          axis.ticks = element_blank(),
          strip.placement = "none",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          strip.text.x = element_blank())+
    scale_size_continuous(limits = c(0,1))+
    facet_grid(.~approach, space="free_x", scales="free_x", switch="x")
  
  p3 <- ggplot(datset3, aes(y = task.id, x = learner.id, size = cv_mean_spars))+
    geom_count(color = gray1)+
    theme_tufte(base_family = "sans")+
    labs(x = label.x, fill = "No. of\nfeatures")+
    theme(legend.position = "right",
          axis.ticks = element_blank(),
          strip.placement = "outside",
          axis.title.y = element_blank())+
    facet_grid(.~approach, space="free_x", scales="free_x", switch="x")
  
  plot_grid(ggdraw() + draw_label(label,
                                  size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
            rel_heights = c(.05,1), ncol = 1)
}

fig_4 <- function(datset1, datset2, datset3, label, label.x){
  
  p1 <- ggplot(datset1, aes(y = task.id, x = approach, size = cv_mean_cindex))+
    geom_count(color = gray1)+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset", fill = "Cindex")+
    theme(legend.position = "right",
          axis.ticks = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())+
    scale_size_continuous(limits = c(0,1))
  
  p2 <- ggplot(datset2, aes(y = task.id, x = approach, size = cv_mean_ibrier))+
    geom_count(color = gray1)+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset",y = "Dataset", fill = "Ibrier")+
    theme(legend.position = "right",
          axis.ticks = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_blank(),
          axis.title.x = element_blank())+
    scale_size_continuous(limits = c(0,1))
  
  p3 <- ggplot(datset3, aes(y = task.id, x = approach, size = cv_mean_spars))+
    geom_count(color = gray1)+
    theme_tufte(base_family = "sans")+
    labs(x = label.x, fill = "No. of\nfeatures")+
    theme(legend.position = "right",
          axis.ticks = element_blank(),
          strip.placement = "outside",
          axis.title.y = element_blank())
  
  plot_grid(ggdraw() + draw_label(label,
                                  size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
            rel_heights = c(.05,1), ncol = 1)
}

# 3. x: Dat, y: Learner----
pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/dataset_learner.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2.5)
fig_1(df, df, df, "Performance measures", "Dataset")
dev.off()

# 4. x: Dat, y: Approach----
## 4.1 Cindex----
### 4.1.1 Minimum----
df_cmin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/dataset_approach_cmin.pdf", width = pdf_w_h[1]*1.25, height = pdf_w_h[2] * 1.25)
fig_2(df_cmin, df_cmin, df_cmin, "Performance measures\nfor the smallest cindex given the dataset and approach", "Dataset")
dev.off()

rm(df_cmin)

### 4.1.2 maximum----
df_cmax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/dataset_approach_cmax.pdf", width = pdf_w_h[1]*1.25, height = pdf_w_h[2] * 1.25)
fig_2(df_cmax, df_cmax, df_cmax, "Performance measures\nfor the biggest cindex given the dataset and approach", "Dataset")
dev.off()

rm(df_cmax)

### 4.1.3 Median----
df_cmed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/dataset_approach_cmed.pdf", width = pdf_w_h[1]*1.25, height = pdf_w_h[2] * 1.25)
fig_2(df_cmed, df_cmed, df_cmed, "Performance measures\nfor the median cindex given the dataset and approach", "Dataset")
dev.off()

rm(df_cmed)

## 4.2 Ibrier----
### 4.2.1 Minimum----
df_imin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/dataset_approach_imin.pdf", width = pdf_w_h[1]*1.25, height = pdf_w_h[2] * 1.25)
fig_2(df_imin, df_imin, df_imin, "Performance measures\nfor the smallest ibrier given the dataset and approach", "Dataset")
dev.off()

rm(df_imin)

### 4.2.2 Maximum----
df_imax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/dataset_approach_imax.pdf", width = pdf_w_h[1]*1.25, height = pdf_w_h[2] * 1.25)
fig_2(df_imax, df_imax, df_imax, "Performance measures\nfor the biggest ibrier given the dataset and approach", "Dataset")
dev.off()

rm(df_imax)

### 4.2.3 Median----
df_imed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/dataset_approach_imed.pdf", width = pdf_w_h[1]*1.25, height = pdf_w_h[2] * 1.25)
fig_2(df_imed, df_imed, df_imed, "Performance measures\nfor the median ibrier given the dataset and approach", "Dataset")
dev.off()

rm(df_imed)

## 4.3 spars----
### 4.3.1 Minimum----
df_smin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_spars == min(cv_mean_spars))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/dataset_approach_smin.pdf", width = pdf_w_h[1]*1.25, height = pdf_w_h[2] * 1.25)
fig_2(df_smin, df_smin, df_smin, "Performance measures\nfor the smallest no. of features given the dataset and approach", "Dataset")
dev.off()

rm(df_smin)

### 4.3.2 Maximum----
df_smax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_spars == max(cv_mean_spars))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/dataset_approach_smax.pdf", width = pdf_w_h[1]*1.25, height = pdf_w_h[2] * 1.25)
fig_2(df_smax, df_smax, df_smax, "Performance measures\nfor the biggest no. of features given the dataset and approach", "Dataset")
dev.off()

rm(df_smax)

### 4.3.3 Median----
df_smed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_spars == mymedian(cv_mean_spars))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/dataset_approach_smed.pdf", width = pdf_w_h[1]*1.25, height = pdf_w_h[2] * 1.25)
fig_2(df_smed, df_smed, df_smed, "Performance measures\nfor the median no. of features given the dataset and approach", "Dataset")
dev.off()

rm(df_smed)


## 4.3 Zsf. ----
### 4.3.1 Minimum----
df_cmin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex))
df_imin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier))
df_smin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_spars == min(cv_mean_spars))


pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/dataset_approach_allemin.pdf", width = pdf_w_h[1]*1.25, height = pdf_w_h[2] * 1.25)
fig_2(df_cmin, df_imin, df_smin, "Performance measures\nfor the smallest cindex, smallest ibrier and smallest no. of features given the dataset and approach", "Dataset")
dev.off()

rm(df_cmin, df_imin, df_smin)

### 4.3.2 Maximum----
df_cmax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex))
df_imax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier))
df_smax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_spars == max(cv_mean_spars))


pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/dataset_approach_allemax.pdf", width = pdf_w_h[1]*1.25, height = pdf_w_h[2] * 1.25)
fig_2(df_cmax, df_imax, df_smax, "Performance measures\nfor the biggest cindex, biggest ibrier and biggest no. of features given the dataset and approach", "Dataset")
dev.off()

rm(df_cmax, df_imax, df_smax)

### 4.3.3 Median----
df_cmed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex))
df_imed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier))
df_smed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_spars == mymedian(cv_mean_spars))


pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/dataset_approach_allemed.pdf", width = pdf_w_h[1]*1.25, height = pdf_w_h[2] * 1.25)
fig_2(df_cmed, df_imed, df_smed, "Performance measures\nfor the median cindex, median ibrier and median no. of features given the dataset and approach", "Dataset")
dev.off()

rm(df_cmed, df_imed, df_smed)

### 4.3.3 Mean----
df_cmean <- df %>% group_by(approach, task.id) %>% summarise(cv_mean_cindex = mean(cv_mean_cindex))
df_imean <- df %>% group_by(approach, task.id) %>% summarise(cv_mean_ibrier = mean(cv_mean_ibrier))
df_smean <- df %>% group_by(approach, task.id) %>% summarise(cv_mean_spars = mean(cv_mean_spars))


pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/dataset_approach_allemean.pdf", width = pdf_w_h[1]*1.25, height = pdf_w_h[2] * 1.25)
fig_2(df_cmean, df_imean, df_smean, "Performance measures\nfor the mean cindex, mean ibrier and mean no. of features given the dataset and approach", "Dataset")
dev.off()

rm(df_cmean, df_imean, df_smean)

# 5. x: Learner, y = Dataset----

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/learner_dataset.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2)
fig_3(df, df, df, "Performance measure", "Learner")
dev.off()

# 6. x: Approach, y = Dataset----
## 6.1 cindex----
### 6.1.1 Minimum----
df_cmin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/approach_dataset_cmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2.5)
fig_4(df_cmin, df_cmin, df_cmin, "Performance measures\nfor the smallest cindex given dataset and approach", "Dataset")
dev.off()

rm(df_cmin)

### 6.1.2 Maximum----
df_cmax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/approach_dataset_cmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2.5)
fig_4(df_cmax, df_cmax, df_cmax, "Performance measures\nfor the biggest cindex given dataset and approach", "Dataset")
dev.off()

rm(df_cmax)

### 6.1.3 Median----
df_cmed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/approach_dataset_cmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2.5)
fig_4(df_cmed, df_cmed, df_cmed, "Performance measures\nfor the median cindex given dataset and approach", "Dataset")
dev.off()

rm(df_cmed)

## 6.2 Ibrier----
### 6.2.1 Minimum----
df_imin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/approach_dataset_imin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2.5)
fig_4(df_imin, df_imin, df_imin, "Performance measures\nfor the smallest ibrier given dataset and approach", "Dataset")
dev.off()

rm(df_imin)

### 6.2.2 Maximum----
df_imax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/approach_dataset_imax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2.5)
fig_4(df_imax, df_imax, df_imax, "Performance measures\nfor the biggest ibrier given dataset and approach", "Dataset")
dev.off()

rm(df_imax)

### 6.2.3 Median----
df_imed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/approach_dataset_imed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2.5)
fig_4(df_imed, df_imed, df_imed, "Performance measures\nfor the median ibrier given dataset and approach", "Dataset")
dev.off()

rm(df_imed)


## 6.3 spars----
### 6.3.1 Minimum----
df_smin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_spars == min(cv_mean_spars))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/approach_dataset_smin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2.5)
fig_4(df_smin, df_smin, df_smin, "Performance measures\nfor the smallest no. of features given dataset and approach", "Dataset")
dev.off()

rm(df_smin)

### 6.3.2 Maximum----
df_smax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_spars == max(cv_mean_spars))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/approach_dataset_smax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2.5)
fig_4(df_smax, df_smax, df_smax, "Performance measures\nfor the biggest no. of features given dataset and approach", "Dataset")
dev.off()

rm(df_smax)

### 6.3.3 Median----
df_smed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_spars == mymedian(cv_mean_spars))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/approach_dataset_smed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2.5)
fig_4(df_smed, df_smed, df_smed, "Performance measures\nfor the median no. of features given dataset and approach", "Dataset")
dev.off()

rm(df_smed)

## 6.4 Zsf----
### 6.4.1 Minimum----
df_cmin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex))
df_imin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier))
df_smin <- df %>% group_by(approach, task.id) %>% filter(cv_mean_spars == min(cv_mean_spars))


pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/approach_dataset_allmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2.5)
fig_4(df_cmin, df_imin, df_smin, "Performance measures\nfor the smallest cindex, smallest ibrier and smallest no. of features given dataset and approach", "Dataset")
dev.off()

rm(df_cmin, df_imin, df_smin)

### 6.4.2 Maximum----
df_cmax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex))
df_imax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier))
df_smax <- df %>% group_by(approach, task.id) %>% filter(cv_mean_spars == max(cv_mean_spars))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/approach_dataset_allmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2.5)
fig_4(df_cmax, df_imax, df_smax, "Performance measures\nfor the biggest cindex, biggest ibrier and biggest no. of features given dataset and approach", "Dataset")
dev.off()

rm(df_smax)

### 6.4.3 Median----
df_cmed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex))
df_imed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier))
df_smed <- df %>% group_by(approach, task.id) %>% filter(cv_mean_spars == mymedian(cv_mean_spars))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/approach_dataset_allmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2.5)
fig_4(df_cmed, df_imed, df_smed, "Performance measures\nfor the median cindex, median ibrier and median no. of features given dataset and approach", "Dataset")
dev.off()

rm(df_cmed, df_imed, df_smed)

### 6.4.3 Mean----
df_cmean <- df %>% group_by(approach, task.id) %>% summarise(cv_mean_cindex = mean(cv_mean_cindex))
df_imean <- df %>% group_by(approach, task.id) %>% summarise(cv_mean_ibrier = mean(cv_mean_ibrier))
df_smean <- df %>% group_by(approach, task.id) %>% summarise(cv_mean_spars = mean(cv_mean_spars))

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Comparison/Matrixchart/approach_dataset_allmean.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 2.5)
fig_4(df_cmean, df_imean, df_smean, "Performance measures\nfor the mean cindex, mean ibrier and mean no. of features given dataset and approach", "Dataset")
dev.off()

rm(df_cmean, df_imean, df_smean)
