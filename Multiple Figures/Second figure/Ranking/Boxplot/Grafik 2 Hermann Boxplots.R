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
scale_fill3 <- c("Reference" = col_s[2], "Penalized regression" = gray3[3], "Random forest" = gray3[3], "Boosting" = gray3[3])
scale_fill4 <- c("Reference" = "white", "Penalized regression" = gray3[3], "Random forest" = "white", "Boosting" = gray3[3])
scale_color <- c("Reference" = "white", "Penalized regression" = "black", "Random forest" = "white", "Boosting" = "black")

df$approach <- factor(df$approach, levels = c("Reference",
                                              "Random forest",
                                              "Penalized regression",
                                              "Boosting"))
df$task.id <- factor(df$task.id, levels = unique(df$task.id[order(df$n)]))
df$cv_mean_spars[is.na(df$cv_mean_spars)] <- 0
scale_fill <-  c("block\nForest" = gray3[3],
                 "Cox\nproportional\nhazard" = col_m[4],
                 "CoxBoost" = gray3[3],
                 "CoxBoost\nfavoring" = gray3[3],
                 "glmboost" = gray3[3],
                 "grridge" = gray3[3],
                 "ipflasso" = gray3[3],
                 "Kaplan\nMeier" = col_m[3],
                 "Lasso" = gray3[3],
                 "priority\nLasso" = gray3[3],
                 "priority\nLasso\nfavoring" = gray3[3],
                 "ranger" = gray3[3],
                 "rfsrc" = gray3[3]
)

scale_fill_b <-  c("block\nForest" = "white",
                 "Cox\nproportional\nhazard" = "white",
                 "CoxBoost" = gray3[3],
                 "CoxBoost\nfavoring" = gray3[3],
                 "glmboost" = gray3[3],
                 "grridge" = "white",
                 "ipflasso" = gray3[3],
                 "Kaplan\nMeier" = "white",
                 "Lasso" = gray3[3],
                 "priority\nLasso" = gray3[3],
                 "priority\nLasso\nfavoring" = gray3[3],
                 "ranger" = "white",
                 "rfsrc" = "white"
)

scale_color_b <-  c("block\nForest" = "white",
                   "Cox\nproportional\nhazard" = "white",
                   "CoxBoost" = "black",
                   "CoxBoost\nfavoring" = "black",
                   "glmboost" = "black",
                   "grridge" = "white",
                   "ipflasso" = "black",
                   "Kaplan\nMeier" = "white",
                   "Lasso" = "black",
                   "priority\nLasso" = "black",
                   "priority\nLasso\nfavoring" = "black",
                   "ranger" = "white",
                   "rfsrc" = "white"
)

# 2. Funktionen----
## 2.1 Median----
mymedian <- function(lst) {
  n <- length(lst)
  s <- sort(lst)
  ifelse(n%%2==1,s[(n+1)/2],s[floor((n+1)/2)])
}

# 3. x: learner, y: Mase----

p1 <- ggplot(df %>% arrange(cv_mean_ibrier) %>% mutate(learner.id = factor(learner.id, levels = learner.id)),
       aes(learner.id, cv_mean_cindex, fill = learner.id))+
  geom_boxplot(size = linesize-.5, show.legend = F)+
  geom_hline(yintercept = median(df$cv_mean_cindex[df$learner.id == "Kaplan\nMeier"]), color = col_m[3], size = linesize-.5)+
  geom_hline(yintercept = median(df$cv_mean_cindex[df$learner.id == "Cox\nproportional\nhazard"]), color = col_m[4], size = linesize-.5)+
  facet_grid(.~approach, space="free_x", scales="free_x", switch="x")+
  theme_tufte(base_family = "sans")+
  labs(y = "Cindex")+
  theme(legend.position = "right",
        axis.ticks = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col))+
  scale_size_continuous(limits = c(0,1))+
  scale_fill_manual(values = scale_fill)+
  expand_limits(y = 0)

p2 <- ggplot(df %>% arrange(cv_mean_ibrier) %>% mutate(learner.id = factor(learner.id, levels = learner.id)),
             aes(learner.id, cv_mean_ibrier, fill = learner.id))+
  geom_boxplot(size = linesize-.5, show.legend = F)+
  geom_hline(yintercept = median(df$cv_mean_ibrier[df$learner.id == "Kaplan\nMeier"]), color = col_m[3], size = linesize-.5)+
  geom_hline(yintercept = median(df$cv_mean_ibrier[df$learner.id == "Cox\nproportional\nhazard"]), color = col_m[4], size = linesize-.5)+
  facet_grid(.~approach, space="free_x", scales="free_x", switch="x")+
  theme_tufte(base_family = "sans")+
  labs(y = "Ibrier")+
  theme(legend.position = "right",
        axis.ticks = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col))+
  scale_size_continuous(limits = c(0,1))+
  scale_fill_manual(values = scale_fill)+
  expand_limits(y = 0)

p3 <- ggplot(df %>% arrange(cv_mean_spars) %>% mutate(learner.id = factor(learner.id, levels = learner.id)),
             aes(learner.id, cv_mean_spars, fill = learner.id, color = learner.id))+
  geom_boxplot(size = linesize-.5, show.legend = F)+
  facet_grid(.~approach, space="free_x", scales="free_x", switch="x")+
  theme_tufte(base_family = "sans")+
  labs(y = "No. of features", x = "Learner")+
  theme(legend.position = "right",
        axis.ticks = element_blank(),
        strip.placement = "outside",
        axis.line.y = element_line(size = axissize, color = axis_col))+
  scale_size_continuous(limits = c(0,1))+
  scale_fill_manual(values = scale_fill_b)+
  scale_color_manual(values = scale_color_b)

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Ranking/Boxplot/learner.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.05,1), ncol = 1)
dev.off()

# 4. x: Approach----
p1 <- ggplot(df,
             aes(approach, cv_mean_cindex, fill = approach))+
  geom_boxplot(size = linesize-.5, show.legend = F)+
  geom_hline(yintercept = median(df$cv_mean_cindex[df$approach == "Reference"]), color = col_m[2], size = linesize-.5)+
  theme_tufte(base_family = "sans")+
  labs(y = "Cindex")+
  theme(legend.position = "right",
        axis.ticks = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col))+
  scale_size_continuous(limits = c(0,1))+
  scale_fill_manual(values = scale_fill3)+
  expand_limits(y = 0)

p2 <- ggplot(df,
             aes(approach, cv_mean_ibrier, fill = approach))+
  geom_boxplot(size = linesize-.5, show.legend = F)+
  geom_hline(yintercept = median(df$cv_mean_ibrier[df$approach == "Reference"]), color = col_m[2], size = linesize-.5)+
  theme_tufte(base_family = "sans")+
  labs(y = "Ibrier")+
  theme(legend.position = "right",
        axis.ticks = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col))+
  scale_size_continuous(limits = c(0,1))+
  scale_fill_manual(values = scale_fill3)+
  expand_limits(y = 0)

p3 <- ggplot(df ,
             aes(approach, cv_mean_spars, fill = approach, color = approach))+
  geom_boxplot(size = linesize-.5, show.legend = F)+
  theme_tufte(base_family = "sans")+
  labs(y = "No. of features", x = "Learner")+
  theme(legend.position = "right",
        axis.ticks = element_blank(),
        strip.placement = "outside",
        axis.line.y = element_line(size = axissize, color = axis_col))+
  scale_size_continuous(limits = c(0,1))+
  scale_fill_manual(values = scale_fill4)+
  scale_color_manual(values = scale_color)

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Ranking/Boxplot/approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.05,1), ncol = 1)
dev.off()

# 5. x: dataset----
p1 <- ggplot(df, aes(task.id, cv_mean_cindex))+
  geom_boxplot(size = linesize-.5, show.legend = F, fill = gray3[3])+
  theme_tufte(base_family = "sans")+
  labs(y = "Cindex")+
  theme(legend.position = "right",
        axis.ticks = element_blank(),
        strip.placement = "outside",
        strip.text = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  scale_size_continuous(limits = c(0,1))+
  scale_fill_manual(values = scale_fill)+
  expand_limits(y = 0)

p2 <- ggplot(df, aes(task.id, cv_mean_ibrier))+
  geom_boxplot(size = linesize-.5, show.legend = F, fill = gray3[3])+
  theme_tufte(base_family = "sans")+
  labs(y = "Ibrier")+
  theme(legend.position = "right",
        axis.ticks = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col))+
  scale_size_continuous(limits = c(0,1))+
  scale_fill_manual(values = scale_fill)+
  expand_limits(y = 0)

p3 <- ggplot(df, aes(task.id, cv_mean_spars))+
  geom_boxplot(size = linesize-.5, show.legend = F, fill = gray3[3])+
  theme_tufte(base_family = "sans")+
  labs(y = "No. of features", x = "Dataset")+
  theme(legend.position = "right",
        axis.ticks = element_blank(),
        strip.placement = "outside",
        axis.line.y = element_line(size = axissize, color = axis_col))+
  scale_size_continuous(limits = c(0,1))+
  scale_fill_manual(values = scale_fill_b)+
  scale_color_manual(values = scale_color_b)

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Ranking/Boxplot/dataset.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.05,1), ncol = 1)
dev.off()


