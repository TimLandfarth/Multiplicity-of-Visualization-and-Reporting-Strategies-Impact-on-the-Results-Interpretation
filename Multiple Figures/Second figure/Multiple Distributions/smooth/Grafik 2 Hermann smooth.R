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
df$approach <- factor(df$approach, levels = c("Reference",
                                              "Random forest",
                                              "Penalized regression",
                                              "Boosting"), ordered = T)
scale_fill_a <- c("Reference" = col_h[2], "Random forest" = col_s[3], "Penalized regression" = col_s[4], "Boosting" = col_s[5])
df$task.id <- factor(df$task.id, levels = unique(df$task.id[order(df$n)]))
scale_linewidth_a <- c("Reference" = linesize + 1, "Random forest" = linesize, "Penalized regression" = linesize, "Boosting" = linesize)


# 2. Funktionen----
mymedian <- function(lst) {
  n <- length(lst)
  s <- sort(lst)
  ifelse(n%%2==1,s[(n+1)/2],s[floor((n+1)/2)])
}

limits <- max(max(df$cv_mean_cindex), max(df$cv_mean_ibrier))


# 3. loess----
p1 <- ggplot(df, aes(x = n, y = cv_mean_cindex, color = approach, size = approach))+
  geom_smooth(method = "loess", formula = y ~ x, se = F)+
  geom_smooth(data = df[df$approach == "Reference",], aes(x = n, y = cv_mean_cindex, color = approach, size = approach),
              method = "loess", formula = y ~ x, se = F)+
  theme_tufte()+
  labs(x = "Observations in the dataset", y = "Cindex")+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.line = element_line(color = axis_col, size = axissize))+
  scale_color_manual(values = scale_fill_a)+
  scale_size_manual(values = scale_linewidth_a)+
  ylim(0,limits)

p2 <- ggplot(df, aes(x = n, y = cv_mean_ibrier, color = approach, size = approach))+
  geom_smooth(method = "loess", formula = y ~ x, se = F, size = linesize)+
  geom_smooth(data = df[df$approach == "Reference",], aes(x = n, y = cv_mean_ibrier, color = approach, size = approach),
              method = "loess", formula = y ~ x, se = F)+
  theme_tufte()+
  labs(x = "Observations in the dataset", y = "Cindex")+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.line = element_line(color = axis_col, size = axissize))+
  scale_color_manual(values = scale_fill_a)+
  scale_size_manual(values = scale_linewidth_a)+
  ylim(0,limits)

p3 <- ggplot(df, aes(x = n, y = cv_mean_spars, color = approach, size = approach))+
  geom_smooth(method = "loess", formula = y ~ x, se = F, size = linesize)+
  theme_tufte()+
  labs(x = "Observations in the dataset", y = "Cindex")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.line = element_line(color = axis_col, size = axissize))+
  scale_color_manual(values = scale_fill_a, drop = FALSE, name = "Approach")+
  scale_size_manual(values = scale_linewidth_a, drop = FALSE, name = "Approach")

pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Multiple Distributions/smooth/loess.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nsmoothed by loess",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.05,1), ncol = 1)
dev.off()  

# 3. loess----
p1 <- ggplot(df, aes(x = n, y = cv_mean_cindex, color = approach, size = approach))+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps"), se = F)+
  geom_smooth(data = df[df$approach == "Reference",],aes(x = n, y = cv_mean_cindex, color = approach, size = approach),
              method = "gam", formula = y ~ s(x, bs = "ps"), se = F)+
  theme_tufte()+
  labs(x = "Observations in the dataset", y = "Cindex")+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.line = element_line(color = axis_col, size = axissize))+
  scale_color_manual(values = scale_fill_a)+
  scale_size_manual(values = scale_linewidth_a)+
  ylim(0,limits)

p2 <- ggplot(df, aes(x = n, y = cv_mean_ibrier, color = approach, size = approach))+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps"), se = F)+
  geom_smooth(data = df[df$approach == "Reference",], aes( x = n, y = cv_mean_ibrier, color = approach, size = approach),
              method = "gam", formula = y ~ s(x, bs = "ps"), se = F)+
  theme_tufte()+
  labs(x = "Observations in the dataset", y = "Cindex")+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.line = element_line(color = axis_col, size = axissize))+
  scale_color_manual(values = scale_fill_a)+
  scale_size_manual(values = scale_linewidth_a)+
  ylim(0,limits)

p3 <- ggplot(df, aes(x = n, y = cv_mean_spars, color = approach, size = approach))+
  geom_smooth(method = "loess", formula = y ~ x, se = F, size = linesize)+
  theme_tufte()+
  labs(x = "Observations in the dataset", y = "Cindex")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.line = element_line(color = axis_col, size = axissize))+
  scale_color_manual(values = scale_fill_a, drop = FALSE, name = "Approach")+
  scale_size_manual(values = scale_linewidth_a, drop = FALSE, name = "Approach")


pdf(file = "C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 2/Multiple Distributions/smooth/ps.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nsmoothed by penalized spline",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.05,1), ncol = 1)
dev.off()
