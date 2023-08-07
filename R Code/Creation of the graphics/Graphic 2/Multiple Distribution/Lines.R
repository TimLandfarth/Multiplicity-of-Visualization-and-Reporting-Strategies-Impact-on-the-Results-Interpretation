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

# 2. Functions----
mymedian <- function(lst) {
  n <- length(lst)
  s <- sort(lst)
  ifelse(n%%2==1,s[(n+1)/2],s[floor((n+1)/2)])
}


fig1 <- function(dataset1, dataset2, dataset3, label){
  d1 <- dataset1
  d2 <- dataset2
  d3 <- dataset3
  
  limits_n <- max(max(abs(d1$cv_mean_cindex)), max(abs(d2$cv_mean_ibrier)))
  
  p1 <- ggplot(d1[d1$approach != "Reference",], aes(x = n, y = cv_mean_cindex, color = approach, size = approach))+
    geom_line(show.legend = FALSE)+
    geom_line(data = d1[d1$approach == "Reference",], aes(x = n, y = cv_mean_cindex, color = approach, size = approach), show.legend = F)+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset", y = "Cindex")+
    theme(legend.position = "bottom",
          strip.placement = "outside",
          axis.title.x = element_blank(),
          axis.line = element_line(color = axis_col, size = axissize),
          plot.title = element_text(hjust = 0.5))+
    scale_color_manual(values = scale_fill_a)+
    scale_size_manual(values = scale_linewidth_a)+
    ylim(0, limits_n)
  
  p2 <- ggplot(d2[d2$approach != "Reference",], aes(x = n, y = cv_mean_ibrier, color = approach, size = approach))+
    geom_line(show.legend = FALSE)+
    geom_line(data = d2[d2$approach == "Reference",], aes(x = n, y = cv_mean_ibrier, color = approach, size = approach), show.legend = F)+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset", y = "Ibrier")+
    theme(legend.position = "bottom",
          strip.placement = "outside",
          axis.title.x = element_blank(),
          axis.line = element_line(color = axis_col, size = axissize),
          plot.title = element_text(hjust = 0.5))+
    scale_color_manual(values = scale_fill_a)+
    scale_size_manual(values = scale_linewidth_a)+
    ylim(0, limits_n)
  
  p3 <- ggplot(d3, aes(x = n, y = cv_mean_spars, color = approach, size = approach))+
    geom_line()+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset", y = "No. of features")+
    theme(legend.position = "bottom",
          strip.placement = "outside",
          axis.title.x = element_blank(),
          axis.line = element_line(color = axis_col, size = axissize),
          plot.title = element_text(hjust = 0.5))+
    scale_color_manual(values = scale_fill_a, name = "Approach")+
    scale_size_manual(values = scale_linewidth_a, name = "Approach")
  
  
  plot_grid(ggdraw() + draw_label(label,
                                  size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
            rel_heights = c(.05,1), ncol = 1)
}


# 3. x: dataset, y: Performance, color = Approach----
## 3.1 Cindex----
### 3.1.1 Minimum----
h <-
  df %>% 
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == min(m1)) %>% 
  select(learner.id)
df_cmin <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cmin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_cmin,df_cmin, "Performance measures\nsummarised by minimum cindex conditional on learner and dataset")
dev.off()

rm(h, df_cmin)

### 3.1.3 Maximum----
h <-
  df %>% 
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == max(m1)) %>% 
  select(learner.id)
df_cmax <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cmax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_cmax,df_cmax, "Performance measures\nsummarised by maximum cindex conditional on learner and dataset")
dev.off()

rm(h, df_cmax)


### 3.1.3 Median----
h <-
  df %>% 
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == mymedian(m1)) %>% 
  select(learner.id)
df_cmed <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cmed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_cmed,df_cmed, "Performance measures\nsummarised by median cindex conditional on learner and dataset")
dev.off()

rm(h, df_cmed)

## 3.1 ibrier----
### 3.1.1 Minimum----
h <-
  df %>% 
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == min(m1)) %>% 
  select(learner.id)
df_imin <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "imin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imin, df_imin,df_imin, "Performance measures\nsummarised by minimum ibrier conditional on learner and dataset")
dev.off()

rm(h, df_imin)

### 3.1.3 Maximum----
h <-
  df %>% 
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == max(m1)) %>% 
  select(learner.id)
df_imax <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "imax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imax, df_imax,df_imax, "Performance measures\nsummarised by maximum ibrier conditional on learner and dataset")
dev.off()

rm(h, df_imax)


### 3.1.3 Median----
h <-
  df %>% 
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == mymedian(m1)) %>% 
  select(learner.id)
df_imed <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "imed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imed, df_imed,df_imed, "Performance measures\nsummarised by median ibrier conditional on learner and dataset")
dev.off()

rm(h, df_imed)

## 3.1 Summary----
### 3.1.1 Minimum----
h <-
  df %>% 
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == min(m1)) %>% 
  select(learner.id)
df_cmin <- df %>% filter(learner.id %in% h$learner.id)

h <-
  df %>% 
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == min(m1)) %>% 
  select(learner.id)
df_imin <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cmin_imin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_imin, df_imin, "Performance measures\nsummarised by minimum cindex and minimum ibrier conditional on learner and dataset")
dev.off()

rm(h, df_imin, df_cmin)

### 3.1.3 Maximum----
h <-
  df %>% 
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == max(m1)) %>% 
  select(learner.id)
df_cmax <- df %>% filter(learner.id %in% h$learner.id)

h <-
  df %>% 
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == max(m1)) %>% 
  select(learner.id)
df_imax <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cmax_imax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_imax, df_imax, "Performance measures\nsummarised by maximum cindex and maximum ibrier conditional on learner and dataset")
dev.off()

rm(h, df_imax, df_cmax)


### 3.1.3 Median----
h <-
  df %>% 
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == mymedian(m1)) %>% 
  select(learner.id)
df_cmed <- df %>% filter(learner.id %in% h$learner.id)

h <-
  df %>% 
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == mymedian(m1)) %>% 
  select(learner.id)
df_imed <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cmed_imed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_imed,df_imed, "Performance measures\nsummarised by median cindex and median ibrier conditional on learner and dataset")
dev.off()

rm(h, df_imed, df_cmed)

# 4. x: dataset, y = Performance, color = Approach, Ref = KM
## 3.1 Cindex----
### 3.1.1 Minimum----
h <-
  df %>% 
  filter(learner.id != "Cox\nproportional\nhazard") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == min(m1)) %>% 
  select(learner.id)
df_cmin <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "km_cmin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_cmin,df_cmin, "Performance measures\nsummarised by minimum cindex for kaplan-meier conditional on learner and dataset")
dev.off()

rm(h, df_cmin)

### 3.1.3 Maximum----
h <-
  df %>%  
  filter(learner.id != "Cox\nproportional\nhazard") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == max(m1)) %>% 
  select(learner.id)
df_cmax <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "km_cmax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_cmax,df_cmax, "Performance measures\nsummarised by maximum cindex for kaplan-meier conditional on learner and dataset")
dev.off()

rm(h, df_cmax)


### 3.1.3 Median----
h <-
  df %>%  
  filter(learner.id != "Cox\nproportional\nhazard") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == mymedian(m1)) %>% 
  select(learner.id)
df_cmed <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "km_cmed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_cmed,df_cmed, "Performance measures\nsummarised by median cindex for kaplan-meier conditional on learner and dataset")
dev.off()

rm(h, df_cmed)

## 3.1 ibrier----
### 3.1.1 Minimum----
h <-
  df %>%  
  filter(learner.id != "Cox\nproportional\nhazard") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == min(m1)) %>% 
  select(learner.id)
df_imin <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "km_imin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imin, df_imin,df_imin, "Performance measures\nsummarised by minimum ibrier for kaplan-meier conditional on learner and dataset")
dev.off()

rm(h, df_imin)

### 3.1.3 Maximum----
h <-
  df %>%  
  filter(learner.id != "Cox\nproportional\nhazard") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == max(m1)) %>% 
  select(learner.id)
df_imax <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "km_imax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imax, df_imax,df_imax, "Performance measures\nsummarised by maximum ibrier for kaplan-meier conditional on learner and dataset")
dev.off()

rm(h, df_imax)


### 3.1.3 Median----
h <-
  df %>%  
  filter(learner.id != "Cox\nproportional\nhazard") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == mymedian(m1)) %>% 
  select(learner.id)
df_imed <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "km_imed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imed, df_imed,df_imed, "Performance measures\nsummarised by median ibrier for kaplan-meier conditional on learner and dataset")
dev.off()

rm(h, df_imed)

## 3.1 Summary----
### 3.1.1 Minimum----
h <-
  df %>%  
  filter(learner.id != "Cox\nproportional\nhazard") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == min(m1)) %>% 
  select(learner.id)
df_cmin <- df %>% filter(learner.id %in% h$learner.id)

h <-
  df %>%  
  filter(learner.id != "Cox\nproportional\nhazard") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == min(m1)) %>% 
  select(learner.id)
df_imin <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "km_cmin_imin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_imin, df_imin, "Performance measures\nsummarised by minimum cindex and minimum ibrier for kaplan-meier conditional on learner and dataset")
dev.off()

rm(h, df_imin, df_cmin)

### 3.1.3 Maximum----
h <-
  df %>%  
  filter(learner.id != "Cox\nproportional\nhazard") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == max(m1)) %>% 
  select(learner.id)
df_cmax <- df %>% filter(learner.id %in% h$learner.id)

h <-
  df %>%  
  filter(learner.id != "Cox\nproportional\nhazard") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == max(m1)) %>% 
  select(learner.id)
df_imax <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "km_cmax_imax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_imax, df_imax, "Performance measures\nsummarised by maximum cindex and maximum ibrier for kaplan-meier conditional on learner and dataset")
dev.off()

rm(h, df_imax, df_cmax)


### 3.1.3 Median----
h <-
  df %>%  
  filter(learner.id != "Cox\nproportional\nhazard") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == mymedian(m1)) %>% 
  select(learner.id)
df_cmed <- df %>% filter(learner.id %in% h$learner.id)

h <-
  df %>%  
  filter(learner.id != "Cox\nproportional\nhazard") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == mymedian(m1)) %>% 
  select(learner.id)
df_imed <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "km_cmed_imed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_imed,df_imed, "Performance measures\nsummarised by median cindex and median ibrier for kaplan-meier conditional on learner and dataset")
dev.off()

rm(h, df_imed, df_cmed)


# 4. x: dataset, y = Performance, color = Approach, Ref = CoxPH
## 3.1 Cindex----
### 3.1.1 Minimum----
h <-
  df %>% 
  filter(learner.id != "Kaplan\nMeier") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == min(m1)) %>% 
  select(learner.id)
df_cmin <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cph_cmin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_cmin,df_cmin, "Performance measures\nsummarised by minimum cindex for cox proportional hazard conditional on learner and dataset")
dev.off()

rm(h, df_cmin)

### 3.1.3 Maximum----
h <-
  df %>%  
  filter(learner.id != "Kaplan\nMeier") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == max(m1)) %>% 
  select(learner.id)
df_cmax <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cph_cmax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_cmax,df_cmax, "Performance measures\nsummarised by maximum cindex for cox proportional hazard conditional on learner and dataset")
dev.off()

rm(h, df_cmax)


### 3.1.3 Median----
h <-
  df %>%  
  filter(learner.id != "Kaplan\nMeier") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == mymedian(m1)) %>% 
  select(learner.id)
df_cmed <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cph_cmed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_cmed,df_cmed, "Performance measures\nsummarised by median cindex for cox proportional hazard conditional on learner and dataset")
dev.off()

rm(h, df_cmed)

## 3.1 ibrier----
### 3.1.1 Minimum----
h <-
  df %>%  
  filter(learner.id != "Kaplan\nMeier") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == min(m1)) %>% 
  select(learner.id)
df_imin <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cph_imin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imin, df_imin,df_imin, "Performance measures\nsummarised by minimum ibrier for cox proportional hazard conditional on learner and dataset")
dev.off()

rm(h, df_imin)

### 3.1.3 Maximum----
h <-
  df %>%  
  filter(learner.id != "Kaplan\nMeier") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == max(m1)) %>% 
  select(learner.id)
df_imax <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cph_imax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imax, df_imax,df_imax, "Performance measures\nsummarised by maximum ibrier for cox proportional hazard conditional on learner and dataset")
dev.off()

rm(h, df_imax)


### 3.1.3 Median----
h <-
  df %>%  
  filter(learner.id != "Kaplan\nMeier") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == mymedian(m1)) %>% 
  select(learner.id)
df_imed <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cph_imed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_imed, df_imed,df_imed, "Performance measures\nsummarised by median ibrier for cox proportional hazard conditional on learner and dataset")
dev.off()

rm(h, df_imed)

## 3.1 Summary----
### 3.1.1 Minimum----
h <-
  df %>%  
  filter(learner.id != "Kaplan\nMeier") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == min(m1)) %>% 
  select(learner.id)
df_cmin <- df %>% filter(learner.id %in% h$learner.id)

h <-
  df %>%  
  filter(learner.id != "Kaplan\nMeier") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == min(m1)) %>% 
  select(learner.id)
df_imin <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cph_cmin_imin.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_imin, df_imin, "Performance measures\nsummarised by minimum cindex and minimum ibrier for cox proportional hazard conditional on learner and dataset")
dev.off()

rm(h, df_imin, df_cmin)

### 3.1.3 Maximum----
h <-
  df %>%  
  filter(learner.id != "Kaplan\nMeier") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == max(m1)) %>% 
  select(learner.id)
df_cmax <- df %>% filter(learner.id %in% h$learner.id)

h <-
  df %>%  
  filter(learner.id != "Kaplan\nMeier") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == max(m1)) %>% 
  select(learner.id)
df_imax <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cph_cmax_imax.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_imax, df_imax, "Performance measures\nsummarised by maximum cindex and maximum ibrier for cox proportional hazard conditional on learner and dataset")
dev.off()

rm(h, df_imax, df_cmax)


### 3.1.3 Median----
h <-
  df %>%  
  filter(learner.id != "Kaplan\nMeier") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_cindex)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == mymedian(m1)) %>% 
  select(learner.id)
df_cmed <- df %>% filter(learner.id %in% h$learner.id)

h <-
  df %>%  
  filter(learner.id != "Kaplan\nMeier") %>%
  group_by(learner.id) %>% 
  mutate(m1 = mean(cv_mean_ibrier)) %>% 
  filter(1:n() == 1) %>% 
  group_by(approach) %>% 
  filter(m1 == mymedian(m1)) %>% 
  select(learner.id)
df_imed <- df %>% filter(learner.id %in% h$learner.id)

#pdf(file = "cph_imed.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_imed,df_imed, "Performance measures\nsummarised by median cindex and median ibrier for cox proportional hazard conditional on learner and dataset")
dev.off()

rm(h, df_imed, df_cmed)


