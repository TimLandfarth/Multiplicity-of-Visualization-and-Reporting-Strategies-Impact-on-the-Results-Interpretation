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
df$approach <- factor(df$approach, levels = c("Reference",
                                              "Random forest",
                                              "Penalized regression",
                                              "Boosting"))
df$task.id <- factor(df$task.id, levels = unique(df$task.id[order(df$n)]))

# 2. Functions----
mymedian <- function(lst) {
  n <- length(lst)
  s <- sort(lst)
  ifelse(n%%2==1,s[(n+1)/2],s[floor((n+1)/2)])
}


fig1 <- function(dataset1, dataset2, label, Ref = "Kaplan\nMeier", label.x){
dat1 <- dataset1 %>% mutate(dif_c = dataset1$cv_mean_cindex - dataset1$cv_mean_cindex[dataset1$learner.id == Ref])
dat2 <- dataset2 %>% mutate(dif_i = dataset2$cv_mean_ibrier - dataset2$cv_mean_ibrier[dataset2$learner.id == Ref])
scale <- max(max(abs(dat1$dif_c)), max(abs(dat2$dif_i)))

fill <-  c("block\nForest" = gray3[3],
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

if(Ref == "Cox\nproportional\nhazard"){
  fill["Cox\nproportional\nhazard"] <- gray3[1]
  fill["Kaplan\nMeier"] <- gray3[3]
}

p1 <- ggplot(data = dat1,
             aes(y = cv_mean_cindex, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Cindex")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        legend.position = "none",
        axis.text.x = element_blank(),
        strip.text = element_blank(),
        axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = dat1$cv_mean_cindex[dat1$learner.id == Ref], size = linesize, color = gray3[1])+
  scale_fill_manual(values = fill)


p2 <- ggplot(data = dat2,
             aes(y = cv_mean_ibrier, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "Ibrier")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none",
        axis.text.x = element_blank(),
        strip.text = element_blank(),
        axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = dat2$cv_mean_ibrier[dat2$learner.id == Ref], size = linesize, color = gray3[1])+
  scale_fill_manual(values = fill)

p3 <- ggplot(dat1, aes(x = learner.id, y = dif_c))+
  geom_col(fill = gray3[3])+
  theme_tufte(base_family = "sans")+
  facet_grid(.~approach, space = "free_x", scales = "free_x", switch = "x")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7))+
  geom_hline(yintercept = 0, color = gray3[1], size = linesize)+
  labs(x = "Learner", y = "Difference")+
  ylim(-scale, scale)

p4 <- ggplot(dat2, aes(x = learner.id, y = dif_i))+
  geom_col(fill = gray3[3])+
  theme_tufte(base_family = "sans")+
  facet_grid(.~approach, space = "free_x", scales = "free_x", switch = "x")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        axis.text.x = element_text(size = 7),
        strip.placement = "outside",
        legend.position = "none",
        axis.title.x = element_blank())+
  geom_hline(yintercept = 0, color = gray3[1], size = linesize)+
  labs(x = "Learner", y = "Difference")+
  ylim(-scale, scale)


d <- plot_grid(ggdraw() + draw_label(label,
                                size = 11,
                                fontfamily = "sans",
                                hjust = 0,
                                x = .01),
          cowplot::plot_grid(p1,
                             p2,
                             p3,
                             p4,
                             nrow = 2,
                             align = "v"),
          rel_heights = c(.1,1),
          ncol = 1)

ggdraw(add_sub(d, label.x, vpadding=grid::unit(0,"lines"),y=6, x=0.5, vjust=4.5))
}

fig2 <- function(dataset1, dataset2, label, label.x){
  dat1 <- dataset1 %>% mutate(dif_c = dataset1$cv_mean_cindex - dataset1$cv_mean_cindex[dataset1$approach == "Reference"])
  dat2 <- dataset2 %>% mutate(dif_i = dataset2$cv_mean_ibrier - dataset2$cv_mean_ibrier[dataset2$approach == "Reference"])
  scale <- max(max(abs(dat1$dif_c)), max(abs(dat2$dif_i)))
  
  fill <-  c("Reference" = gray3[1],
             "Penalized regression" = gray3[3],
             "Boosting" = gray3[3],
             "Random forest" = gray3[3]
  )
  
  p1 <- ggplot(data = dat1,
               aes(y = cv_mean_cindex, x = approach, fill = approach))+
    geom_col()+
    theme_tufte(base_family = "sans")+
    labs(x = "Learner", y = "Cindex")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          legend.position = "none",
          axis.text.x = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_blank())+
    scale_y_continuous(expand = c(0,0), limits = c(0,1))+
    geom_hline(yintercept = dat1$cv_mean_cindex[dat1$approach == "Reference"], size = linesize, color = gray3[1])+
    scale_fill_manual(values = fill)
  
  
  p2 <- ggplot(data = dat2,
               aes(y = cv_mean_ibrier, x = approach, fill = approach))+
    geom_col()+
    theme_tufte(base_family = "sans")+
    labs(x = "Learner", y = "Ibrier")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          strip.placement = "outside",
          legend.position = "none",
          axis.text.x = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_blank())+
    scale_y_continuous(expand = c(0,0), limits = c(0,1))+
    geom_hline(yintercept = dat2$cv_mean_ibrier[dat2$approach == "Reference"], size = linesize, color = gray3[1])+
    scale_fill_manual(values = fill)
  
  p3 <- ggplot(dat1, aes(x = approach, y = dif_c))+
    geom_col(fill = gray3[3])+
    theme_tufte(base_family = "sans")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          strip.placement = "outside",
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 7))+
    geom_hline(yintercept = 0, color = gray3[1], size = linesize)+
    labs(x = "Learner", y = "Difference")+
    ylim(-scale, scale)
  
  p4 <- ggplot(dat2, aes(x = approach, y = dif_i))+
    geom_col(fill = gray3[3])+
    theme_tufte(base_family = "sans")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          axis.text.x = element_text(size = 7),
          strip.placement = "outside",
          legend.position = "none",
          axis.title.x = element_blank())+
    geom_hline(yintercept = 0, color = gray3[1], size = linesize)+
    labs(x = "Learner", y = "Difference")+
    ylim(-scale, scale)
  
  
  d <- plot_grid(ggdraw() + draw_label(label,
                                       size = 11,
                                       fontfamily = "sans",
                                       hjust = 0,
                                       x = .01),
                 cowplot::plot_grid(p1,
                                    p2,
                                    p3,
                                    p4,
                                    nrow = 2,
                                    align = "v"),
                 rel_heights = c(.1,1),
                 ncol = 1)
  
  ggdraw(add_sub(d, label.x, vpadding=grid::unit(0,"lines"),y=6, x=0.5, vjust=4.5))
}

# 3. x: Learner, y: Performance, Ref: KM----
## 3.1 Dataset----
### 3.1.1 Minimum----
df_dmin <- df[df$n == min(df$n),]

fig1(df_dmin, df_dmin, "Performance measure\nfor the smallest (LAML) dataset", Ref = "Kaplan\nMeier", label.x = "Learner")
dev.off()

rm(df_dmin)

### 3.1.2 Maximum----
df_dmax <- df[df$n == max(df$n),]

#pdf(file = "learner_dmax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_dmax, df_dmax, "Performance measure\nfor the biggest (BRCA) dataset", Ref = "Kaplan\nMeier", label.x = "Learner")
dev.off()

rm(df_dmax)


### 3.1.3 Median----
df_dmed <- df[df$task.id == "KIRC",] 


#pdf(file = "learner_dmed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_dmed, df_dmed, "Performance measure\nfor the median (KIRC) dataset", Ref = "Kaplan\nMeier", label.x = "Learner")
dev.off()

rm(df_dmed)

## 3.2 Cindex----
### 3.2.1 Minimum----
df_cmin <- df %>% group_by(learner.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_cmin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_cmin, "Performance measure\nfor the smallest Cindex", Ref = "Kaplan\nMeier", label.x = "Learner")
dev.off()

rm(df_cmin)

### 3.2.2 Maximum----
df_cmax <- df %>% group_by(learner.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_cmax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_cmax, "Performance measure\nfor the biggest Cindex", Ref = "Kaplan\nMeier", label.x = "Learner")
dev.off()

rm(df_cmax)


### 3.2.3 Median----
df_cmed <- df %>% group_by(learner.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_cmed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_cmed, "Performance measure\nfor the median Cindex", Ref = "Kaplan\nMeier", label.x = "Learner")
dev.off()

rm(df_cmed)

## 3.3 Ibrier----
### 3.3.1 Minimum----
df_imin <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_imin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_imin, df_imin, "Performance measure\nfor the smallest ibrier", Ref = "Kaplan\nMeier", label.x = "Learner")
dev.off()

rm(df_imin)

### 3.3.2 Maximum----
df_imax <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_imax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_imax, df_imax, "Performance measure\nfor the biggest ibrier", Ref = "Kaplan\nMeier", label.x = "Learner")
dev.off()

rm(df_imax)


### 3.3.3 Median----
df_imed <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_imed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_imed, df_imed, "Performance measure\nfor the median ibrier", Ref = "Kaplan\nMeier", label.x = "Learner")
dev.off()

rm(df_imed)

## 3.4 Summary----
### 3.4.1 Minimum----
df_cmin <- df %>% group_by(learner.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()
df_imin <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_allemin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_imin, "Performance measure\nfor the smallest cindex and smallest ibrier", Ref = "Kaplan\nMeier", label.x = "Learner")
dev.off()

rm(df_cmin, df_imin)

### 3.4.2 Maximum----
df_cmax <- df %>% group_by(learner.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()
df_imax <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_allemax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_imax, "Performance measure\nfor the biggest cindex and biggest ibrier", Ref = "Kaplan\nMeier", label.x = "Learner")
dev.off()

rm(df_imax, df_cmax)


### 3.4.3 Median----
df_cmed <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()
df_imed <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_allemed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_imed, "Performance measure\nfor the median cindex and median ibrier", Ref = "Kaplan\nMeier", label.x = "Learner")
dev.off()

rm(df_cmed, df_imed)

### 3.4.3 Mean----
df_cmean <- df %>% group_by(learner.id) %>% mutate(cv_mean_ibrier = mean(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()
df_imean <- df %>% group_by(learner.id) %>% mutate(cv_mean_ibrier = mean(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_allemean.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_imed, "Performance measure\nfor the mean cindex and mean ibrier", Ref = "Kaplan\nMeier", label.x = "Learner")
dev.off()

rm(df_cmean, df_imean)

# 4. x: Learner, y: Performance, Ref: Cox PH----
## 4.1 Dataset----
### 4.1.1 Minimum----
df_dmin <- df[df$n == min(df$n),]

#pdf(file = "learner_ph_dmin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_dmin, df_dmin, "Performance measure\nfor the smallest (LAML) dataset", Ref = "Cox\nproportional\nhazard", label.x = "Learner")
dev.off()

rm(df_dmin)

### 4.1.2 Maximum----
df_dmax <- df[df$n == max(df$n),]

#pdf(file = "learner_ph_dmax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_dmax, df_dmax, "Performance measure\nfor the biggest (BRCA) dataset", Ref = "Cox\nproportional\nhazard", label.x = "Learner")
dev.off()

rm(df_dmax)


### 4.1.3 Median----
df_dmed <- df[df$task.id == "KIRC",] 


#pdf(file = "learner_ph_dmed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_dmed, df_dmed, "Performance measure\nfor the median (KIRC) dataset", Ref = "Cox\nproportional\nhazard", label.x = "Learner")
dev.off()

rm(df_dmed)

## 4.2 Cindex----
### 4.2.1 Minimum----
df_cmin <- df %>% group_by(learner.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_ph_cmin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_cmin, "Performance measure\nfor the smallest Cindex", Ref = "Cox\nproportional\nhazard", label.x = "Learner")
dev.off()

rm(df_cmin)

### 4.2.2 Maximum----
df_cmax <- df %>% group_by(learner.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_ph_cmax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_cmax, "Performance measure\nfor the biggest Cindex", Ref = "Cox\nproportional\nhazard", label.x = "Learner")
dev.off()

rm(df_cmax)


### 4.2.3 Median----
df_cmed <- df %>% group_by(learner.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_ph_cmed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_cmed, "Performance measure\nfor the median Cindex", Ref = "Cox\nproportional\nhazard", label.x = "Learner")
dev.off()

rm(df_cmed)

## 4.3 Ibrier----
### 4.3.1 Minimum----
df_imin <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_ph_imin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_imin, df_imin, "Performance measure\nfor the smallest ibrier", Ref = "Cox\nproportional\nhazard", label.x = "Learner")
dev.off()

rm(df_imin)

### 4.3.2 Maximum----
df_imax <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_ph_imax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_imax, df_imax, "Performance measure\nfor the biggest ibrier", Ref = "Cox\nproportional\nhazard", label.x = "Learner")
dev.off()

rm(df_imax)


### 4.3.3 Median----
df_imed <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_ph_imed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_imed, df_imed, "Performance measure\nfor the median ibrier", Ref = "Cox\nproportional\nhazard", label.x = "Learner")
dev.off()

rm(df_imed)

## 4.4 Summary----
### 4.4.1 Minimum----
df_cmin <- df %>% group_by(learner.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()
df_imin <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_ph_allemin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_cmin, df_imin, "Performance measure\nfor the smallest cindex and smallest ibrier", Ref = "Cox\nproportional\nhazard", label.x = "Learner")
dev.off()

rm(df_cmin, df_imin)

### 4.4.2 Maximum----
df_cmax <- df %>% group_by(learner.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()
df_imax <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_ph_allemax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_cmax, df_imax, "Performance measure\nfor the biggest cindex and biggest ibrier", Ref = "Cox\nproportional\nhazard", label.x = "Learner")
dev.off()

rm(df_imax, df_cmax)


### 4.4.3 Median----
df_cmed <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()
df_imed <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_ph_allemed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_cmed, df_imed, "Performance measure\nfor the median cindex and median ibrier", Ref = "Cox\nproportional\nhazard", label.x = "Learner")
dev.off()

rm(df_cmed, df_imed)

### 4.4.3 Mean----
df_cmean <- df %>% group_by(learner.id) %>% mutate(cv_mean_ibrier = mean(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()
df_imean <- df %>% group_by(learner.id) %>% mutate(cv_mean_ibrier = mean(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "learner_ph_allemean.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig1(df_cmean, df_imean, "Performance measure\nfor the mean cindex and mean ibrier", Ref = "Cox\nproportional\nhazard", label.x = "Learner")
dev.off()

rm(df_cmean, df_imean)

# 5. x: Approach, y: Performance, Ref: Reference
## 5.1 x: dataset, y:Performance----
### 5.1.1 Minimum----
#### 5.1.1.1 Cindex----
##### 5.1.1.1.1 Minimum----
df_dmincmin <- df %>% filter(task.id == "LAML") %>% group_by(approach) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% ungroup()

#pdf(file = "approach_dmin_cmin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmincmin, df_dmincmin, "Performance measure\nfor smallest dataset (LAML) and smallest cindex", "Approach")
dev.off()

rm(df_dmincmin)

##### 5.1.1.1.2 Maximum----
df_dmincmax <- df %>% filter(task.id == "LAML") %>% group_by(approach) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% ungroup()

#pdf(file = "approach_dmin_cmax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmincmax, df_dmincmax, "Performance measure\nfor smallest dataset (LAML) and biggest cindex", "Approach")
dev.off()

rm(df_dmincmax)

##### 5.1.1.1.3 Median----
df_dmincmed <- df %>% filter(task.id == "LAML") %>% group_by(approach) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% ungroup()

#pdf(file = "approach_dmin_cmed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmincmed, df_dmincmed, "Performance measure\nfor smallest dataset (LAML) and median cindex", "Approach")
dev.off()

rm(df_dmincmed)

### 5.1.1.2 Ibrier----
##### 5.1.1.2.1 Minimum----
df_dminimin <- df %>% filter(task.id == "LAML") %>% group_by(approach) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmin_imin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dminimin, df_dminimin, "Performance measure\nfor smallest dataset (LAML) and smallest ibrier", "Approach")
dev.off()

rm(df_dminimin)

##### 5.1.1.2.2 Maximum----
df_dminimax <- df %>% filter(task.id == "LAML") %>% group_by(approach) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmin_imax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dminimax, df_dminimax, "Performance measure\nfor smallest dataset (LAML) and biggest ibrier", "Approach")
dev.off()

rm(df_dminimax)

##### 5.1.1.2.3 Median----
df_dminimed <- df %>% filter(task.id == "LAML") %>% group_by(approach) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmin_imed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dminimed, df_dminimed, "Performance measure\nfor smallest dataset (LAML) and median ibrier", "Approach")
dev.off()

rm(df_dminimed)

### 5.1.1.3 Summary----
##### 5.1.1.3.1 Minimum----
df_dmincmin <- df %>% filter(task.id == "LAML") %>% group_by(approach) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% ungroup()
df_dminimin <- df %>% filter(task.id == "LAML") %>% group_by(approach) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmin_allemin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmincmin, df_dminimin, "Performance measure\nfor smallest dataset (LAML) and smallest ibrier and smallest cindex", "Approach")
dev.off()

rm(df_dmincmin, df_dminimin)

##### 5.1.1.3.2 Maximum----
df_dmincmax <- df %>% filter(task.id == "LAML") %>% group_by(approach) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% ungroup()
df_dminimax <- df %>% filter(task.id == "LAML") %>% group_by(approach) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmin_allemax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmincmax, df_dminimax, "Performance measure\nfor smallest dataset (LAML) and biggest ibrier and biggest cindex", "Approach")
dev.off()

rm(df_dmincmax, df_dminimax)

##### 5.1.1.3.3 Median----
df_dmincmed <- df %>% filter(task.id == "LAML") %>% group_by(approach) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% ungroup()
df_dminimed <- df %>% filter(task.id == "LAML") %>% group_by(approach) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmin_allemed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmincmed, df_dminimed, "Performance measure\nfor smallest dataset (LAML) and median ibrier and median cindex", "Approach")
dev.off()

rm(df_dmincmed, df_dminimed)

##### 5.1.1.3.4 Mean----
df_dmincmean <- df %>% filter(task.id == "LAML") %>% group_by(approach) %>% mutate(cv_mean_cindex = mean(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()
df_dminimean <- df %>% filter(task.id == "LAML") %>% group_by(approach) %>% mutate(cv_mean_ibrier = mean(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "approach_dmin_allemean.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmincmean, df_dminimean, "Performance measure\nfor smallest dataset (LAML) and median ibrier and median cindex", "Approach")
dev.off()

rm(df_dmincmean, df_dminimean)

### 5.1.2 Maximum----
#### 5.1.2.1 Cindex----
##### 5.1.2.1.1 Minimum----
df_dmaxcmin <- df %>% filter(task.id == "BRCA") %>% group_by(approach) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% ungroup()

#pdf(file = "approach_dmax_cmin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmaxcmin, df_dmaxcmin, "Performance measure\nfor biggest dataset (BRCA) and smallest cindex", "Approach")
dev.off()

rm(df_dmaxcmin)

##### 5.1.2.1.1 Maximum----
df_dmaxcmax <- df %>% filter(task.id == "BRCA") %>% group_by(approach) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% ungroup()

#pdf(file = "approach_dmax_cmax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmaxcmax, df_dmaxcmax, "Performance measure\nfor biggest dataset (BRCA) and biggest cindex", "Approach")
dev.off()

rm(df_dmaxcmax)

##### 5.1.2.1.2 Median----
df_dmaxcmed <- df %>% filter(task.id == "BRCA") %>% group_by(approach) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% ungroup()

#pdf(file = "approach_dmax_cmed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmaxcmed, df_dmaxcmed, "Performance measure\nfor biggest dataset (BRCA) and median cindex", "Approach")
dev.off()

rm(df_dmaxcmed)

#### 5.1.2.2 Ibrier----
##### 5.1.2.2.1 Minimum----
df_dmaximin <- df %>% filter(task.id == "BRCA") %>% group_by(approach) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmax_imin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmaximin, df_dmaximin, "Performance measure\nfor biggest dataset (BRCA) and smallest ibrier", "Approach")
dev.off()

rm(df_dmaximin)

##### 5.1.2.2.2 Maximum----
df_dmaximax <- df %>% filter(task.id == "BRCA") %>% group_by(approach) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmax_imax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmaximax, df_dmaximax, "Performance measure\nfor biggest dataset (BRCA) and biggest ibrier", "Approach")
dev.off()

rm(df_dmaximax)

##### 5.1.2.2.3 Median----
df_dmaximed <- df %>% filter(task.id == "BRCA") %>% group_by(approach) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmax_imed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmaximed, df_dmaximed, "Performance measure\nfor biggest dataset (BRCA) and median ibrier", "Approach")
dev.off()

rm(df_dmaximed)

#### 5.1.2.3 Summary----
##### 5.1.2.3.1 Minimum----
df_dmaxcmin <- df %>% filter(task.id == "BRCA") %>% group_by(approach) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% ungroup()
df_dmaximin <- df %>% filter(task.id == "BRCA") %>% group_by(approach) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmax_allemin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmaxcmin, df_dmaximin, "Performance measure\nfor biggest dataset (BRCA) and smallest ibrier and smallest cindex", "Approach")
dev.off()

rm(df_dmaxcmin, df_dmaximin)

##### 5.1.2.3.2 Maximum----
df_dmaxcmax <- df %>% filter(task.id == "BRCA") %>% group_by(approach) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% ungroup()
df_dmaximax <- df %>% filter(task.id == "BRCA") %>% group_by(approach) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmax_allemax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmaxcmax, df_dmaximax, "Performance measure\nfor biggest dataset (BRCA) and biggest ibrier and biggest cindex", "Approach")
dev.off()

rm(df_dmaxcmax, df_dmaximax)

##### 5.1.2.3.3 Median----
df_dmaxcmed <- df %>% filter(task.id == "BRCA") %>% group_by(approach) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% ungroup()
df_dmaximed <- df %>% filter(task.id == "BRCA") %>% group_by(approach) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmax_allemed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmaxcmed, df_dmaximed, "Performance measure\nfor biggest dataset (BRCA) and median ibrier and median cindex", "Approach")
dev.off()

rm(df_dmaxcmed, df_dmaximed)

##### 5.1.2.3.4 Mean----
df_dmaxcmean <- df %>% filter(task.id == "BRCA") %>% group_by(approach) %>% mutate(cv_mean_cindex = mean(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()
df_dmaximean <- df %>% filter(task.id == "BRCA") %>% group_by(approach) %>% mutate(cv_mean_ibrier = mean(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "approach_dmax_allemean.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmaxcmean, df_dmaximean, "Performance measure\nfor biggest dataset (BRCA) and median ibrier and median cindex", "Approach")
dev.off()

rm(df_dmaxcmean, df_dmaximean)

### 5.1.3 Median----
#### 5.1.3.1 Cindex----
##### 5.1.3.1.1 Minimum----
df_dmedcmin <- df %>% filter(task.id == "KIRC") %>% group_by(approach) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% ungroup()

#pdf(file = "approach_dmed_cmin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmedcmin, df_dmedcmin, "Performance measure\nfor median dataset (KIRC) and smallest cindex", "Approach")
dev.off()

rm(df_dmedcmin)

##### 5.1.3.1.2 Maximum----
df_dmedcmax <- df %>% filter(task.id == "KIRC") %>% group_by(approach) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% ungroup()

#pdf(file = "approach_dmed_cmax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmedcmax, df_dmedcmax, "Performance measure\nfor median dataset (KIRC) and biggest cindex", "Approach")
dev.off()

rm(df_dmedcmax)

##### 5.1.3.1.3 Median----
df_dmedcmed <- df %>% filter(task.id == "KIRC") %>% group_by(approach) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% ungroup()

#pdf(file = "approach_dmed_cmed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmedcmed, df_dmedcmed, "Performance measure\nfor median dataset (KIRC) and median cindex", "Approach")
dev.off()

rm(df_dmedcmed)

#### 5.1.3.2 Ibrier----
##### 5.1.3.2.1 Minimum----
df_dmedimin <- df %>% filter(task.id == "KIRC") %>% group_by(approach) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmed_imin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmedimin, df_dmedimin, "Performance measure\nfor median dataset (KIRC) and smallest ibrier", "Approach")
dev.off()

rm(df_dmedimin)

##### 5.1.3.2.2 Maximum----
df_dmedimax <- df %>% filter(task.id == "KIRC") %>% group_by(approach) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmed_imax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmedimax, df_dmedimax, "Performance measure\nfor median dataset (KIRC) and biggest ibrier", "Approach")
dev.off()

rm(df_dmedimax)

##### 5.1.3.2.3 Median----
df_dmedimed <- df %>% filter(task.id == "KIRC") %>% group_by(approach) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmed_imed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmedimed, df_dmedimed, "Performance measure\nfor median dataset (KIRC) and median ibrier", "Approach")
dev.off()

rm(df_dmedimed)

#### 5.1.3.4 Summary----
##### 5.1.3.4.1 Minimum----
df_dmedcmin <- df %>% filter(task.id == "KIRC") %>% group_by(approach) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% ungroup()
df_dmedimin <- df %>% filter(task.id == "KIRC") %>% group_by(approach) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmed_allemin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmedcmin, df_dmedimin, "Performance measure\nfor median dataset (KIRC) and smallest ibrier and smallest cindex", "Approach")
dev.off()

rm(df_dmedcmin, df_dmedimin)

##### 5.1.3.4.2 Maximum----
df_dmedcmax <- df %>% filter(task.id == "KIRC") %>% group_by(approach) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% ungroup()
df_dmedimax <- df %>% filter(task.id == "KIRC") %>% group_by(approach) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmed_allemax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmedcmax, df_dmedimax, "Performance measure\nfor median dataset (KIRC) and biggest ibrier and biggest cindex", "Approach")
dev.off()

rm(df_dmedcmax, df_dmedimax)

##### 5.1.3.4.3 Median----
df_dmedcmed <- df %>% filter(task.id == "KIRC") %>% group_by(approach) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% ungroup()
df_dmedimed <- df %>% filter(task.id == "KIRC") %>% group_by(approach) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% ungroup()

#pdf(file = "approach_dmed_allemed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmedcmed, df_dmedimed, "Performance measure\nfor median dataset (KIRC) and median ibrier and median cindex", "Approach")
dev.off()

rm(df_dmedcmed, df_dmedimed)

##### 5.1.3.4.4 Mean----
df_dmedcmean <- df %>% filter(task.id == "KIRC") %>% group_by(approach) %>% mutate(cv_mean_cindex = mean(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()
df_dmedimean <- df %>% filter(task.id == "KIRC") %>% group_by(approach) %>% mutate(cv_mean_ibrier = mean(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "approach_dmed_allemean.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_dmedcmean, df_dmedimean, "Performance measure\nfor median dataset (KIRC) and median ibrier and median cindex", "Approach")
dev.off()

rm(df_dmedcmean, df_dmedimean)

### 5.2 Cindex----
#### 5.2.1 Minimum----
df_cmin <- df %>% group_by(approach) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "approach_cmin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_cmin, df_cmin, "Performance measure\nfor smallest cindex", "Approach")
dev.off()

rm(df_cmin)

#### 5.2.2 Maximum----
df_cmax <- df %>% group_by(approach) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "approach_cmax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_cmax, df_cmax, "Performance measure\nfor biggest cindex", "Approach")
dev.off()

rm(df_cmax)

#### 5.2.2 Median----
df_cmed <- df %>% group_by(approach) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "approach_cmed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_cmed, df_cmed, "Performance measure\nfor median cindex", "Approach")
dev.off()

rm(df_cmed)

### 5.3 Ibrier----
#### 5.3.1 Minimum----
df_imin <- df %>% group_by(approach) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "approach_imin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_imin, df_imin, "Performance measure\nfor smallest ibrier", "Approach")
dev.off()

rm(df_imin)

#### 5.3.2 Maximum----
df_imax <- df %>% group_by(approach) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "approach_imax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_imax, df_imax, "Performance measure\nfor biggest ibrier", "Approach")
dev.off()

rm(df_imax)

#### 5.3.2 Median----
df_imed <- df %>% group_by(approach) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "approach_imed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_imed, df_imed, "Performance measure\nfor median ibrier", "Approach")
dev.off()

rm(df_imed)


### 5.4 Zsf----
#### 5.4.1 Minimum----
df_cmin <- df %>% group_by(approach) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()
df_imin <- df %>% group_by(approach) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "approach_allemin.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_cmin, df_imin, "Performance measure\nfor smallest ibrier and smallest cindex", "Approach")
dev.off()

rm(df_imin, df_cmin)

#### 5.4.2 Maximum----
df_cmax <- df %>% group_by(approach) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()
df_imax <- df %>% group_by(approach) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "approach_allemax.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_cmax, df_imax, "Performance measure\nfor biggest ibrier and biggest cindex", "Approach")
dev.off()

rm(df_imax, df_cmax)

#### 5.4.3 Median----
df_cmed <- df %>% group_by(approach) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()
df_imed <- df %>% group_by(approach) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "approach_allemed.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_cmed, df_imed, "Performance measure\nfor median ibrier and median cindex", "Approach")
dev.off()

rm(df_imed, df_cmed)

#### 5.4.4 Mean----
df_cmean <- df %>% group_by(approach) %>% mutate(cv_mean_cindex = mean(cv_mean_cindex)) %>% filter(1:n() == 1) %>% ungroup()
df_imean <- df %>% group_by(approach) %>% mutate(cv_mean_ibrier = mean(cv_mean_ibrier)) %>% filter(1:n() == 1) %>% ungroup()

#pdf(file = "approach_allemean.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2] * 1.5)
fig2(df_cmean, df_imean, "Performance measure\nfor mean ibrier and mean cindex", "Approach")
dev.off()

rm(df_imean, df_cmean)
