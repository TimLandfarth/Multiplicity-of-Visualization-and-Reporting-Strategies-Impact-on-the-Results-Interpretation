# 0. librarys----
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(cowplot)
library(ggthemes)

# 1. Loading Data----
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

## 2.2 Plots----

fig_1 <- function(datset1, datset2, datset3, label, label.x, label.legend){
  
  p1 <- ggplot(datset1, aes(x = approach, y = cv_mean_cindex, color = task.id))+
    geom_line(aes(group = approach), color = gray1, linewidth = linesize)+
    geom_point(size = pointsize)+
    theme_tufte(base_family = "sans")+
    theme(panel.grid.major.x = element_line(linetype = 3, color = gray2[2]),
          legend.position = "none", 
          axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.text.x = element_blank(),
          strip.placement = "outside",
          strip.text = element_blank(),
          axis.title.x = element_blank())+
    scale_color_manual(values = c("LAML" = col_s[2], "BRCA" = col_s[3]), name = "Dataset")+
    scale_y_continuous(expand = expansion(mult = c(0,.05)))+
    ylab("Cindex")+
    expand_limits(y = 0)
  
  p2 <- ggplot(datset2, aes(x = approach, y = cv_mean_ibrier, color = task.id))+
    geom_line(aes(group = approach), color = gray1, linewidth = linesize)+
    geom_point(size = pointsize)+
    theme_tufte(base_family = "sans")+
    theme(panel.grid.major.x = element_line(linetype = 3, color = gray2[2]),
          legend.position = "none", 
          axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.text.x = element_blank(),
          strip.placement = "outside",
          strip.text = element_blank(),
          axis.title.x = element_blank())+
    scale_color_manual(values = c("LAML" = col_s[2], "BRCA" = col_s[3]), name = "Dataset")+
    expand_limits(y = 0)+
    scale_y_continuous(expand = expansion(mult = c(0,.05)))+
    ylab("Ibrier")
  
  p3 <- ggplot(datset3, aes(x = approach, y = cv_mean_spars, color = task.id))+
    geom_line(aes(group = approach), color = gray1, linewidth = linesize)+
    geom_point(size = pointsize)+
    theme_tufte(base_family = "sans")+
    theme(panel.grid.major.x = element_line(linetype = 3, color = gray2[2]),
          legend.position = "bottom", 
          axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          strip.placement = "outside")+
    scale_color_manual(values = c("LAML" = col_s[2], "BRCA" = col_s[3]), name = label.legend)+
    expand_limits(y = 0)+
    scale_y_continuous(expand = expansion(mult = c(0,1)))+
    ylab("No. of features")+
    xlab(label.x)
  
  plot_grid(ggdraw() + 
             draw_label(label, size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
           rel_heights = c(.1,1), ncol = 1)
}


# 3. x = learner, y = Performance, first/second Dot: min/max Dataset----
df1 <- df %>% filter(task.id %in% c("LAML", "BRCA"))

p1 <- ggplot(df1, aes(x = learner.id, y = cv_mean_cindex, color = task.id))+
  geom_line(aes(group = learner.id), color = gray1, linewidth = linesize)+
  geom_point(size = pointsize)+
  theme_tufte(base_family = "sans")+
  theme(panel.grid.major.x = element_line(linetype = 3, color = gray2[2]),
        legend.position = "none", 
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.text.x = element_blank(),
        strip.placement = "outside",
        strip.text = element_blank(),
        axis.title.x = element_blank())+
  facet_grid(.~approach, space = "free_x", scales = "free_x", switch = "x")+
  scale_color_manual(values = c("LAML" = col_s[2], "BRCA" = col_s[3]), name = "Dataset")+
  scale_y_continuous(expand = expansion(mult = c(0,.05)))+
  ylab("Cindex")+
  expand_limits(y = 0)

p2 <- ggplot(df1, aes(x = learner.id, y = cv_mean_ibrier, color = task.id))+
  geom_line(aes(group = learner.id), color = gray1, linewidth = linesize)+
  geom_point(size = pointsize)+
  theme_tufte(base_family = "sans")+
  theme(panel.grid.major.x = element_line(linetype = 3, color = gray2[2]),
        legend.position = "none", 
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.text.x = element_blank(),
        strip.placement = "outside",
        strip.text = element_blank(),
        axis.title.x = element_blank())+
  facet_grid(.~approach, space = "free_x", scales = "free_x", switch = "x")+
  scale_color_manual(values = c("LAML" = col_s[2], "BRCA" = col_s[3]), name = "Dataset")+
  expand_limits(y = 0)+
  scale_y_continuous(expand = expansion(mult = c(0,.05)))+
  ylab("Ibrier")

p3 <- ggplot(df1, aes(x = learner.id, y = cv_mean_spars, color = task.id))+
  geom_line(aes(group = learner.id), color = gray1, linewidth = linesize)+
  geom_point(size = pointsize)+
  theme_tufte(base_family = "sans")+
  theme(panel.grid.major.x = element_line(linetype = 3, color = gray2[2]),
        legend.position = "bottom", 
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside")+
  facet_grid(.~approach, space = "free_x", scales = "free_x", switch = "x")+
  scale_color_manual(values = c("LAML" = col_s[2], "BRCA" = col_s[3]), name = "Dataset")+
  expand_limits(y = 0)+
  scale_y_continuous(expand = expansion(mult = c(0,1)))+
  ylab("No. of features")+
  xlab("Learner")

#pdf(file = "learner.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + 
            draw_label("Performance measures\nfor the smallest (LAML) and biggest (BRCA) dataset", size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

# 4. x = Approach, y = Performance, first/second Dot: min/max Dataset----
## 4.1 cindex----
### 4.1.1 Minimum----
df_cmin <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex))

#pdf(file = "approach_cmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
fig_1(df_cmin, df_cmin, df_cmin, "Performance measures\nfor the smallest cindex given approach and dataset\nfor the smallest (LAML) and biggest (BRCA) dataset", "Approach", "Dataset")
dev.off()

rm(df_cmin)

### 4.1.2 Maximum----
df_cmax <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex))

#pdf(file = "approach_cmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
fig_1(df_cmax, df_cmax, df_cmax, "Performance measures\nfor the biggest cindex given approach and dataset\nfor the smallest (LAML) and biggest (BRCA) dataset", "Approach", "Dataset")
dev.off()

rm(df_cmax)

### 4.1.3 Median----
df_cmed <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex))

#pdf(file = "approach_cmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
fig_1(df_cmed, df_cmed, df_cmed, "Performance measures\nfor the median cindex given approach and dataset\nfor the smallest (LAML) and biggest (BRCA) dataset", "Approach", "Dataset")
dev.off()

rm(df_cmed)

## 4.2 Ibrier----
### 4.2.1 Minimum----
df_imin <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier))

#pdf(file = "approach_imin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
fig_1(df_imin, df_imin, df_imin, "Performance measures\nfor the smallest ibrier given approach and dataset\nfor the smallest (LAML) and biggest (BRCA) dataset", "Approach", "Dataset")
dev.off()

rm(df_imin)

### 4.2.2 Maximum----
df_imax <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier))

#pdf(file = "approach_imax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
fig_1(df_imax, df_imax, df_imax, "Performance measures\nfor the biggest ibrier given approach and dataset\nfor the smallest (LAML) and biggest (BRCA) dataset", "Approach", "Dataset")
dev.off()

rm(df_imax)

### 4.2.3 Median----
df_imed <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier))

#pdf(file = "approach_imed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
fig_1(df_imed, df_imed, df_imed, "Performance measures\nfor the median ibrier given approach and dataset\nfor the smallest (LAML) and biggest (BRCA) dataset", "Approach", "Dataset")
dev.off()

rm(df_imed)

## 4.3 spars----
### 4.3.1 Minimum----
df_smin <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_spars == min(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "approach_smin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
fig_1(df_smin, df_smin, df_smin, "Performance measures\nfor the smallest no. of features given approach and dataset\nfor the smallest (LAML) and biggest (BRCA) dataset", "Approach", "Dataset")
dev.off()

rm(df_smin)

### 4.3.2 Maximum----
df_smax <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_spars == max(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "approach_smax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
fig_1(df_smax, df_smax, df_smax, "Performance measures\nfor the biggest no. of features given approach and dataset\nfor the smallest (LAML) and biggest (BRCA) dataset", "Approach", "Dataset")
dev.off()

rm(df_smax)

### 4.3.3 Median----
df_smed <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_spars == mymedian(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "approach_smed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
fig_1(df_smed, df_smed, df_smed, "Performance measures\nfor the median no. of features given approach and dataset\nfor the smallest (LAML) and biggest (BRCA) dataset", "Approach", "Dataset")
dev.off()

rm(df_smed)

## 4.3 all features----
### 4.3.1 Minimum----
df_cmin <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1)
df_imin <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1)
df_smin <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_spars == min(cv_mean_spars)) %>% filter(1:n() == 1)

#pdf(file = "approach_allemin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
fig_1(df_cmin, df_imin, df_smin, "Performance measures\nfor the smallest cindex, smallest ibrier and smallest no. of features given approach and dataset\nfor the smallest (LAML) and biggest (BRCA) dataset", "Approach", "Dataset")
dev.off()

rm(df_cmin, df_imin, df_smin)

### 4.3.1 Maximum----
df_cmax <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex))
df_imax <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier))
df_smax <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_spars == max(cv_mean_spars))

#pdf(file = "approach_allemax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
fig_1(df_cmax, df_imax, df_smax, "Performance measures\nfor the biggest cindex, biggest ibrier and biggest no. of features given approach and dataset\nfor the smallest (LAML) and biggest (BRCA) dataset", "Approach", "Dataset")
dev.off()

rm(df_cmax, df_imax, df_smax)

### 4.3.1 Median----
df_cmed <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex))
df_imed <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier))
df_smed <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% filter(cv_mean_spars == mymedian(cv_mean_spars))

#pdf(file = "approach_allemed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
fig_1(df_cmed, df_imed, df_smed, "Performance measures\nfor the median cindex, median ibrier and median no. of features given approach and dataset\nfor the smallest (LAML) and biggest (BRCA) dataset", "Approach", "Dataset")
dev.off()

rm(df_cmed, df_imed, df_smed)

### 4.3.1 Mean----
df_cmean <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% summarise(cv_mean_cindex = mean(cv_mean_cindex))
df_imean <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% summarise(cv_mean_ibrier = mean(cv_mean_ibrier))
df_smean <- df %>% filter(task.id %in% c("LAML", "BRCA")) %>% group_by(approach, task.id) %>% summarise(cv_mean_spars = mean(cv_mean_spars))

#pdf(file = "approach_allemean.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
fig_1(df_cmean, df_imean, df_smean, "Performance measures\nfor the mean cindex, mean ibrier and mean no. of features given approach and dataset\nfor the smallest (LAML) and biggest (BRCA) dataset", "Approach", "Dataset")
dev.off()

rm(df_cmean, df_imean, df_smean)
