## 0.1 Librarys----
library(dplyr)
library(ggplot2)
library(xtable)
library(DescTools)
library(gglorenz)
library(ggthemes)
library(forcats)

## 0.2 Loading Data----
load(file = "Aufbereitete Daten.Rda")
load(file = "stats.RData")
load(file = "Data1 sub.Rda")
load(file = "Data2 sub.Rda")


## 0.3 Functions----
lorenz_plot <- function(data){
  ggplot(data, aes(n)) +
    stat_lorenz(desc = F, linewidth = linesize, color = gray1)+
    coord_fixed() +
    geom_abline(linetype = "dashed", linewidth = linesize-.75, color = gray3[3]) +
    theme_tufte()+
    theme(legend.position = "none",
          axis.line = element_line(linewidth = axissize, color = axis_col))+
    scale_y_continuous(expand = expansion(mult = c(0,0)))+
    scale_x_continuous(expand = expansion(mult = c(0,0)))+
    labs(x = "Proportion of attribute contributors", y = "Cumulated relative attribute sum")
  
}

lorenz_plot_strat <- function(data){
  t$mas <- "Cindex"
  t1$mas <- "Ibrier"
  u <- rbind(t, t1)
  ggplot(u, aes(n, color = mas)) +
    stat_lorenz(desc = F, linewidth = linesize)+
    coord_fixed() +
    geom_abline(linetype = "dashed", linewidth = linesize-.75, color = gray3[3]) +
    theme_tufte()+
    scale_color_manual(values = c("Cindex" = gray2[1], "Ibrier" = gray2[2]))+
    theme(axis.line = element_line(linewidth = axissize, color = axis_col),
          legend.position = "bottom")+
    scale_y_continuous(expand = expansion(mult = c(0,0)))+
    scale_x_continuous(expand = expansion(mult = c(0,0)))+
    labs(x = "Proportion of attribute contributors", y = "Cumulated relative attribute sum",
         color = "Measurement")
  
}

# 1. Graphic 1----
## 1.1 count ----
### 1.1.1 Produced graphics----
y <- df_1 %>% group_by(Grafikname) %>% filter(1:n() == 1)
y$Grafikname[y$Typ == "Comparison" & y$Darstellungsart == "Balken"]
y <- rbind(y, y[y$Grafikname == "heatm_lea_dat",])
y$Grafikname[y$Grafikname == "heatm_lea_dat"][2] <- "heatm_lea_dat_sub"
y <- rbind(y, y[y$Grafikname == "ranking_box_learner",])
y$Grafikname[y$Grafikname == "ranking_box_learner"][2] <- "ranking_box_learner_sub"
nrow(y)
y <- rbind(y, data.frame(Typ = c("Comparison", "Comparison", "Comparison"), Darstellungsart = c("Balken","Balken","Balken") , x.Achse = c("Calculation time","Calculation time","Calculation time"),
                         y.Achse = c("learner", "learner", "learner"), Stratifiziert = c(NA, NA, NA), Beob_Strata = c(NA, NA, NA), Referenz = c(NA, NA, NA),
                         Steigungsrichtung = c(NA, NA, NA), Variabilitaet = c(NA, NA, NA), Form = c(NA, NA, NA), Steigung = c(NA, NA, NA), 
                         Grafikname = c("comp_bars_learner_maxDat_ref", "comp_bars_learner_sum_ref", "comp_bars_learner_sum")))
y <- y[y$Grafikname != "nomcomp_bars_learner_sum",]

### 1.1.2 Types used----
table(y$Typ)
prop.table(table(y$Typ))
table(y$Typ, y$Darstellungsart)
prop.table(table(y$Typ, y$Darstellungsart))

prop.table(table(df_1$Darstellungsart, df_1$Typ))
xtable(table(df_1$Darstellungsart, df_1$Typ), type = "latex", tabular.environment="longtable")
xtable(prop.table(table(df_1$Darstellungsart, df_1$Typ)), type = "latex", tabular.environment="longtable")

### 1.1.3 Proportion learner-dataset----
prop.table(table(y$x.Achse, y$y.Achse))

### 1.1.4  Direction of gradient----
prop.table(table(y$Steigungsrichtung))

### 1.1.5 Variability----
prop.table(table(y$Variabilitaet))

### 1.1.6 Form and Gradient----
table(y$Form)
prop.table(table(y$Form))

table(y$Steigung)
prop.table(table(y$Steigung))

table(y$Variabilitaet)
prop.table(table(y$Variabilitaet))

y$Grafikname[y$Typ == "Comparison" & y$Darstellungsart == "Balken"]


### 1.1.7 Graphics ultimately used----
q <- rbind(df1_1, df1_2, df1_3, df1_4, df1_5, df1_6, df1_7,
      df1_8, df1_9, df1_10, df1_11)
length(unique(q$Grafikname)) 
length(unique(df_1$Grafikname))
length(unique(q$Grafikname)) / length(unique(df_1$Grafikname))
rm(q)

## 1.2 Gini----
### 1.2.1 Stratum 1----
t1_1 <- df1_1 %>% count(Form, Steigung)
g_form <- df1_1 %>% count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist()
g <- DescTools::Gini(t1_1$n)
lorenz_plot(t1_1)
rm(t)

### 1.2.2 Stratum 2----
t1_2 <- df1_2 %>% count(Form, Steigung)
g_form <- rbind(g_form, 
                df1_2 %>% count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())
(g[2] <- DescTools::Gini(t1_2$n))
lorenz_plot(t1_2)
rm(t)

### 1.2.3 Stratum 3----
t1_3 <- df1_3 %>% count(Form, Steigung)
g_form <- rbind(g_form, 
                df1_3 %>% count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())
(g[3] <- DescTools::Gini(t1_3$n))
lorenz_plot(t1_3)
rm(t)

### 1.2.4 Stratum 4----
t1_4 <- df1_4 %>% count(Form, Steigung)
g_form <- rbind(g_form, 
                df1_4 %>% count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())
(g[4] <- DescTools::Gini(t1_4$n))
lorenz_plot(t1_4)
rm(t)

#t1_1$set <- "Max"
#pdf("Lorenz1_4.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(rbind(t1_1, t1_2 %>% mutate(set = "Mean"),
             t1_3 %>% mutate(set = "Median"),
             t1_4 %>% mutate(set = "Min")), aes(n, color = set)) +
    stat_lorenz(desc = F, linewidth = linesize)+
    coord_fixed() +
    geom_abline(linetype = "dashed", linewidth = linesize-.75, color = gray3[3]) +
    theme_tufte()+
    scale_color_manual(values = c("Max" = col_s[2], "Mean" = col_s[3],
                                  "Median" = col_s[4], "Min" = col_s[5]))+
    theme(axis.line = element_line(linewidth = axissize, color = axis_col),
          legend.position = "bottom")+
    scale_y_continuous(expand = expansion(mult = c(0,0)))+
    scale_x_continuous(expand = expansion(mult = c(0,0)))+
    labs(x = "Proportion of attribute contributors", y = "Cumulated relative attribute sum",
         color = "Function")
dev.off()

### 1.2.5 Stratum 5----
t1_5 <- df1_5 %>% count(Form, Steigung)
g_form <- rbind(g_form, 
                df1_5 %>% count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())
(g[5] <- DescTools::Gini(t1_5$n))
lorenz_plot(t1_5)
rm(t)

### 1.2.6 Stratum 6----
t1_6 <- df1_6 %>% count(Form, Steigung)
g_form <- rbind(g_form, 
                df1_6 %>% count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())
(g[6] <- DescTools::Gini(t1_6$n))
lorenz_plot(t1_6)
rm(t)

### 1.2.7 Stratum 7----
t1_7 <- df1_7 %>% count(Form, Steigung)
g_form <- rbind(g_form, 
                df1_7 %>% count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())
(g[7] <- DescTools::Gini(t1_7$n))
lorenz_plot(t1_7)
rm(t)

#t1_5$set <- "Max"
#pdf("Lorenz1_7.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(rbind(t1_5, t1_6 %>% mutate(set = "Median"),
             t1_7 %>% mutate(set = "Min")), aes(n, color = set)) +
  stat_lorenz(desc = F, linewidth = linesize)+
  coord_fixed() +
  geom_abline(linetype = "dashed", linewidth = linesize-.75, color = gray3[3]) +
  theme_tufte()+
  scale_color_manual(values = c("Max" = col_s[2], 
                                "Median" = col_s[4], "Min" = col_s[5]))+
  theme(axis.line = element_line(linewidth = axissize, color = axis_col),
        legend.position = "bottom")+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  scale_x_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "Proportion of attribute contributors", y = "Cumulated relative attribute sum",
       color = "Function")
dev.off()

### 1.2.8 Stratum 8----
t1_8_Bo <- df1_8 %>% filter(Beob_Strata %like% "%Boosting") %>% count(Form, Steigung)

g_form <- rbind(g_form, 
                df1_8 %>% filter(Beob_Strata %like% "%Boosting") %>%
                  count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())
(g[8] <- DescTools::Gini(t1_8_Bo$n))
lorenz_plot(t1_8_Bo)
rm(t)

t1_8_Ra <- df1_8 %>% filter(Beob_Strata %like% "%Random forest") %>% count(Form, Steigung)
g_form <- rbind(g_form, 
                df1_8 %>% filter(Beob_Strata %like% "%Random forest") %>%
                  count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())
(g[9] <- DescTools::Gini(t1_8_Ra$n))
lorenz_plot(t1_8_Ra)
rm(t)

t1_8_Pe <- df1_8 %>% filter(Beob_Strata %like% "%Penalized regression") %>% count(Form, Steigung)
g_form <- rbind(g_form, 
                df1_8 %>% filter(Beob_Strata %like% "%Penalized regression") %>%
                  count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())

(g[10] <- DescTools::Gini(t1_8_Pe$n))
lorenz_plot(t1_8_Pe)
rm(t)

### 1.2.9 Stratum 9----
t1_9_Bo <- df1_9 %>% filter(Beob_Strata %like% "%Boosting") %>% count(Form, Steigung)
g_form <- rbind(g_form, 
                df1_9 %>% filter(Beob_Strata %like% "%Boosting") %>%
                  count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())

(g[11] <- DescTools::Gini(t1_9_Bo$n))
lorenz_plot(t1_9_Bo)
rm(t)

t1_9_Ra <- df1_9 %>% filter(Beob_Strata %like% "%Random forest") %>% count(Form, Steigung)
g_form <- rbind(g_form, 
                df1_9 %>% filter(Beob_Strata %like% "%Random forest") %>%
                  count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())
(g[12] <- DescTools::Gini(t1_9_Ra$n))
lorenz_plot(t1_9_Ra)
rm(t)

t1_9_Pe <- df1_9 %>% filter(Beob_Strata %like% "%Penalized regression") %>% count(Form, Steigung)
g_form <- rbind(g_form, 
                df1_9 %>% filter(Beob_Strata %like% "%Penalized regression") %>%
                  count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())
(g[13] <- DescTools::Gini(t1_9_Pe$n))
lorenz_plot(t1_9_Pe)
rm(t)

### 1.2.10 Stratum 10----
t1_10_Bo <- df1_10 %>% filter(Beob_Strata %like% "%Boosting") %>% count(Form, Steigung)
g_form <- rbind(g_form, 
                df1_10 %>% filter(Beob_Strata %like% "%Boosting") %>%
                  count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())
(g[14] <- DescTools::Gini(t1_10_Bo$n))
lorenz_plot(t1_10_Bo)
rm(t)

t1_10_Ra <- df1_10 %>% filter(Beob_Strata %like% "%Random forest") %>% count(Form, Steigung)
g_form <- rbind(g_form, 
                df1_10 %>% filter(Beob_Strata %like% "%Random forest") %>%
                  count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())
(g[15] <- DescTools::Gini(t1_10_Ra$n))
lorenz_plot(t1_10_Ra)
rm(t)

t1_10_Pe <- df1_10 %>% filter(Beob_Strata %like% "%Penalized regression") %>% count(Form, Steigung)
g_form <- rbind(g_form, 
                df1_10 %>% filter(Beob_Strata %like% "%Penalized regression") %>%
                  count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())
(g[16] <- DescTools::Gini(t1_10_Pe$n))
lorenz_plot(t1_10_Pe)
rm(t)

#t1_8_Bo$set <- "max dat bo"

g_form <- rbind(g_form, 
                df1_10 %>% filter(Beob_Strata %like% "%Penalized regression") %>%
                  count(Form) %>% mutate(g_f = DescTools::Gini(n)) %>% filter(1:n() == 1) %>% select(g_f) %>% unlist())
(g[16] <- DescTools::Gini(t1_10_Pe$n))
lorenz_plot(t1_10_Pe)
rm(t)

df1_11$Form[df1_11$Grafikname == "ranking_box_dataset_approach" & df1_11$Beob_Strata == "Allgemein"] <- "linear"

t1_11 <- df1_11 %>% filter(Beob_Strata == "Allgemein") %>% count(Form, Steigung)
g[17] <- DescTools::Gini(t1_11$n)


#pdf("Lorenz1_10.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(rbind(t1_8_Bo, t1_8_Pe %>% mutate(set = "max dat pe"),
             t1_8_Ra %>% mutate(set = "max dat ra"),
             t1_9_Ra %>% mutate(set = "med dat ra"),
             t1_10_Bo %>% mutate(set = "min dat bo"),
             t1_10_Pe %>% mutate(set = "min dat pe"),
             t1_10_Ra %>% mutate(set = "min dat ra")), aes(n, color = set)) +
  stat_lorenz(desc = F, linewidth = linesize)+
  coord_fixed() +
  geom_abline(linetype = "dashed", linewidth = linesize-.75, color = gray3[3]) +
  theme_tufte()+
  scale_color_manual(values = c("max dat bo" = col_s[2], "max dat pe" = col_m[2], "max dat ra" = col_h[2], 
                                "med dat ra" = col_s[3],
                                "min dat bo" = col_s[4], "min dat pe" = col_m[4], "min dat ra" = col_h[4]),
                     breaks = c("max dat bo", "max dat pe", "max dat ra", 
                                "med dat ra", 
                                "min dat bo", "min dat pe", "min dat ra"))+
  theme(axis.line = element_line(linewidth = axissize, color = axis_col),
        legend.position = "bottom")+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  scale_x_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "Proportion of attribute contributors", y = "Cumulated relative attribute sum",
       color = "Selection")+
  guides(color = guide_legend(nrow = 3, byrow = T))
dev.off()


### 1.2.12 Ditribution of Ginicoefficients----
# t = 1,2,3,4,11
# g = 1,2,3,4,17

t1_ord <- rbind(df1_1, df1_2, df1_3, df1_4) %>% count(Form, Steigung)
t1_general <- rbind(df1_1, df1_2, df1_3, df1_4, df1_11) %>% count(Form, Steigung)

#"Calc. - Data set", "Calc. - Data set (ord.)", "Calc. - Data set (ratio)"

d <- data.frame(g = c(DescTools::Gini(t1_general$n), DescTools::Gini(t1_ord$n), g[1:4], g[17]),
                f = c(rep("Calc. - Dataset", 1),
                      rep("Calc. - Dataset (ord.)", 5),
                      rep("Calc. - Dataset (ratio)", 1)),
                h = c("general", "general", "max\nCalc.", "mean\nCalc.",
                      "med\nCalc.", "min\nCalc.", "general"))

#t1_1_4 <- rbind(df1_1, df1_2, df1_3, df1_4) %>% count(Form, Steigung)
#t1_5_7 <- rbind(df1_5, df1_6, df1_7) %>% count(Form, Steigung)
#t1_8_10 <- rbind(df1_8, df1_9, df1_10) %>% count(Form, Steigung)
#
#t1_8 <- df1_8 %>% count(Form, Steigung)
#t1_9 <- df1_9 %>% count(Form, Steigung)
#t1_10 <- df1_10 %>% count(Form, Steigung)

#d <- rbind(d, data.frame(g = c(DescTools::Gini(t1_1_4$n), DescTools::Gini(t1_5_7$n), DescTools::Gini(t1_8_10$n),
#                               DescTools::Gini(t1_8$n), DescTools::Gini(t1_9$n), DescTools::Gini(t1_10$n)),
#           f = c("Calc. - Dataset", "Calc. - Learner", "Calc. - Approach",
#                 "Calc. - Approach", "Calc. - Approach", "Calc. - Approach"),
#           h = c("over all", "over all", "over all",
#                 "max\nDat", "med\nDat", "min\nDat")))
#d <- d[!(d$h %in% c("max Dat\nBo", "max Dat\nRa", "max Dat\nPe",
#           "med Dat\nBo", "med Dat\nRa", "med Dat\nPe",
#           "min Dat\nBo", "min Dat\nRa", "min Dat\nPe")),]

d$f <- factor(d$f, levels = c("Calc. - Dataset", "Calc. - Learner", "Calc. - Approach"), ordered = T)
d$h <- factor(d$h, levels = c("over all", "max\nCalc.", "mean\nCalc.", "med\nCalc.", "min\nCalc.", "max\nDat", "med\nDat", 
                       "min\nDat", "max Dat\nBo", "max Dat\nRa", "max Dat\nPe",
                       "med Dat\nBo", "med Dat\nRa", "med Dat\nPe", "min Dat\nBo", "min Dat\nRa", "min Dat\nPe"), ordered = T)

#pdf("Grafik1 Gini.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(d, aes(y = g, x = h, fill = h))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(.~ f, space = "free_x", scales = "free_x", switch = "x")+
  labs(y = "Gini coefficient", x = "Different combinations")+
  theme_tufte()+
  theme(axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.placement = "outside", legend.position = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_fill_manual(values = c("general" = gray3[1],
                               "max\nCalc." = gray3[3], "mean\nCalc."  = gray3[3],
                               "med\nCalc."  = gray3[3], "min\nCalc."  = gray3[3]))
dev.off()


d <- data.frame(g = g[8:16],
                f = c(rep("Within Approach", 9)),
                h = c("max Dat\nBo", "max Dat\nRa", "max Dat\nPe",
                      "med Dat\nBo", "med Dat\nRa", "med Dat\nPe",
                      "min Dat\nBo", "min Dat\nRa", "min Dat\nPe"))

d <- rbind(d, data.frame(g = c(DescTools::Gini(t1_8_10$n), DescTools::Gini(t1_8$n),
                               DescTools::Gini(t1_9$n), DescTools::Gini(t1_10$n)),
                         f = c("Outside Approach","Outside Approach",
                               "Outside Approach", "Outside Approach"),
                         h = c("over all", "max\nDat", 
                               "med\nDat", "min\nDat")))

d$f <- factor(d$f, levels = c("Outside Approach", "Within Approach"), ordered = T)
d$h <- factor(d$h, levels = c("over all", "max\nDat", "med\nDat", "min\nDat",
                              "max Dat\nBo", "max Dat\nRa", "max Dat\nPe",
                              "med Dat\nBo", "med Dat\nRa", "med Dat\nPe",
                              "min Dat\nBo", "min Dat\nRa", "min Dat\nPe"), ordered = T)

#pdf("Grafik1 Gini Approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(d, aes(y = g, x = h, fill = h))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(.~ f, space = "free_x", scales = "free_x", switch = "x")+
  labs(y = "DescTools::Gini coefficient", x = "Different combinations")+
  theme_tufte()+
  theme(axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.placement = "outside", legend.position = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_fill_manual(values = c("over all" = gray3[1], "max\nDat" = col_m[2], "med\nDat" = col_m[4], "min\nDat" = col_m[8],
                               "max Dat\nBo" = col_s[2], "max Dat\nRa" = col_s[2], "max Dat\nPe" = col_s[2],
                               "med Dat\nBo" = col_s[4], "med Dat\nRa" = col_s[4], "med Dat\nPe" = col_s[4],
                               "min Dat\nBo" = col_s[8], "min Dat\nRa" = col_s[8], "min Dat\nPe" = col_s[8]))

dev.off()
# two nominale vars, 5 observations
DescTools::Gini(c(2,3))
DescTools::Gini(c(1,4))

DescTools::Gini(c(1,1,3))
DescTools::Gini(c(1,2,2))

# two nominal vars, 6 observations
DescTools::Gini(c(3,3))
DescTools::Gini(c(2,4))
DescTools::Gini(c(1,5))

DescTools::Gini(c(1,1,4))
DescTools::Gini(c(1,2,3))
DescTools::Gini(c(2,2,2))

### 1.2.13 Distribution of Form, gradient and Variability----
d <- y %>% group_by(Form, Steigung, Variabilitaet) %>% count()
m <- d %>% group_by(Form, Steigung) %>% mutate(n = sum(n)) %>% mutate(n = sum(n)) %>% filter(1:n() == 1) %>% mutate(Variabilitaet = "alle")
d <- rbind(d, data.frame(Form = c("exponential", "exponential", "linear", "linear"),
                         Steigung = c("schwach", "stark", "maessig", "schwach"),
                         Variabilitaet = c("maessig", "stark", "schwach", "schwach"),
                         n = c(0, 0, 0, 0)))
d$Variabilitaet <- factor(d$Variabilitaet, levels = c("alle", "schwach", "maessig", "stark"), ordered = T)
d$Steigung <- factor(d$Steigung, levels = c("schwach", "maessig", "stark"), ordered = T)

#pdf("form_steigung_Var_Graf1.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(d,
       aes(y = n, x = Steigung, fill = Variabilitaet))+
  geom_col(linewidth = linesize-.5, position = "dodge", color = "white")+
  facet_grid(.~ Form, space = "free_x", scales = "free_x", switch = "x",
             labeller = as_labeller(c(`linear` = "linear", `exponential` = "exponential", `logarithmisch` = "logarithmic")))+
  theme_tufte()+
  theme(axis.line.y = element_line(linewidth = axissize, color = axis_col),
        axis.ticks.x = element_blank(),
        strip.placement = "outside",
        legend.position = "bottom",
        strip.text.x = element_text(size = 10))+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "Shape and slope", y = "Count")+
  #scale_x_discrete(labels=c("linear" = "linear", "exponential" = "exponential",
  #                          "logarithmisch" = "logarithmic"))+
  scale_x_discrete(labels = c("schwach" = "weak", "maessig" = "moderate", "stark" = "strong"))+
  scale_fill_manual(labels = c("alle" = "over all", "schwach" = "weak", "maessig" = "moderate", "stark" = "strong"), 
                      values = c("alle" = gray2[1], "schwach" = col_s[2], "maessig" = col_m[2], "stark" = col_h[2]))+
  labs(fill = "Variability")
dev.off()

rm(g)
rm(d, df_1, df1_1, df1_2, df1_3, df1_4, df1_5, df1_6,
   df1_7, df1_8, df1_9, df1_10, df1_11,
   g_form,
   t1_1, t1_2, t1_3, t1_4, t1_5, t1_6, t1_7, t1_8_Bo,
   t1_8_Pe, t1_8_Ra, t1_9_Bo, t1_9_Pe, t1_9_Ra, t1_10_Bo,
   t1_10_Pe, t1_10_Ra, t1_11, test, y, g)

# 2. Graph 2----
## 2.1 count ----
y <- df_2 %>% group_by(Typ, Darstellungsart, Grafikname) %>% filter(1:n() == 1)
nrow(y)

### 2.1.1 produced graphics----
length(unique(df_2$Grafikname))

### 2.1.2 Types used----
table(y$Typ)
prop.table(table(y$Typ))

table(y$Typ, y$Darstellungsart)
prop.table(table(y$Typ, y$Darstellungsart))
xtable(table(df_2$Darstellungsart, df_2$Typ), type = "latex", tabular.environment="longtable")
xtable(prop.table(table(df_2$Darstellungsart, df_2$Typ)), type = "latex", tabular.environment="longtable")

### 2.1.3  Direction of gradient----
prop.table(table(df_2$Steigungsrichtung_Al))
prop.table(table(df_2$Steigungsrichtung_Re))
prop.table(table(df_2$Steigungsrichtung_Ra))
prop.table(table(df_2$Steigungsrichtung_Pe))
prop.table(table(df_2$Steigungsrichtung_Bo))

### 1.1.4 Variability----
prop.table(table(df_2$Variabilitaet_Al))
prop.table(table(df_2$Variabilitaet_Re))
prop.table(table(df_2$Variabilitaet_Ra))
prop.table(table(df_2$Variabilitaet_Pe))
prop.table(table(df_2$Variabilitaet_Bo))

### 1.1.5 Form and Gradient----
prop.table(table(y$Form_Al))
prop.table(table(y$Steigung_Al))
prop.table(table(y$Variabilitaet_Al))

prop.table(table(df_2$Form_Al, df_2$Steigung_Al))
prop.table(table(df_2$Form_Re, df_2$Steigung_Re))
prop.table(table(df_2$Form_Ra, df_2$Steigung_Ra))
prop.table(table(df_2$Form_Pe, df_2$Steigung_Pe))
prop.table(table(df_2$Form_Bo, df_2$Steigung_Bo))

### 1.1.7 graphics ultimately used----
q <- rbind(df2_1, df2_2, df2_3, df2_4, df2_5, df2_6, df2_7, df2_8, df2_9, df2_10,
           df2_11, df2_12, df2_13, df2_14, df2_15, df2_16, df2_17, df2_18, df2_19, df2_20)
length(unique(q$Grafikname)) 
length(unique(df_2$Grafikname))
length(unique(q$Grafikname)) / length(unique(y$Grafikname))
rm(q)


## 2.2 Gini----
### 2.2.1 Stratum 1----
t <-  df2_1 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
      !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t1 <- df2_1 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
      !(Form_Al != "konstant" & Steigung_Al == "konstant"))

### 2.2.2 Stratum 2----
t <-  df2_2 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t1 <- df2_2 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

DescTools::Gini(t$n)
DescTools::Gini(t1$n)
p1.2 <- lorenz_plot_strat()
rm(t, t1)

### 2.2.3 Stratum 3----
t <-  df2_3 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t1 <- df2_3 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

DescTools::Gini(t$n)
DescTools::Gini(t1$n)
p1.3 <- lorenz_plot_strat()
rm(t, t1)

### 2.2.4 Stratum 4----
t2_4c <-  df2_4 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al)

t2_4i <- df2_4 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

g_c1 <- DescTools::Gini(t2_4c$n)
g_i1 <- DescTools::Gini(t2_4i$n)

t2_4cv <-  df2_4 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al)

t2_4iv <- df2_4 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

g_c1v <- DescTools::Gini(t2_4cv$n)
g_i1v <- DescTools::Gini(t2_4iv$n)



### 2.2.5 Stratum 5----
t2_5c <-  df2_5 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

t2_5i <- df2_5 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al)

g_c1[2] <- DescTools::Gini(t2_5c$n)
g_i1[2] <- DescTools::Gini(t2_5i$n)


t2_5cv <-  df2_5 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

t2_5iv <- df2_5 %>% 
  filter(y.Achse == "Ibrier"& Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al)

g_c1v[2] <- DescTools::Gini(t2_5cv$n)
g_i1v[2] <- DescTools::Gini(t2_5iv$n)


### 2.2.6 Stratum 6----
t2_6c <-  df2_6 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al)

t2_6i <- df2_6 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))
g_c1[3] <- DescTools::Gini(t2_6c$n)
g_i1[3] <- DescTools::Gini(t2_6i$n)

t2_6cv <-  df2_6 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al)

t2_6iv <- df2_6 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))
g_c1v[3] <- DescTools::Gini(t2_6cv$n)
g_i1v[3] <- DescTools::Gini(t2_6iv$n)

### 2.2.7 Stratum 7----
t2_7c <-  df2_7 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

t2_7i <- df2_7 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

g_c1[4] <- DescTools::Gini(t2_7c$n)
g_i1[4] <- DescTools::Gini(t2_7i$n)


t2_7cv <-  df2_7 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

t2_7iv <- df2_7 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

g_c1v[4] <- DescTools::Gini(t2_7cv$n)
g_i1v[4] <- DescTools::Gini(t2_7iv$n)

### 2.2.8 Stratum 8----
t2_8c <-  df2_8 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al)

t2_8i <- df2_8 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al)

g_c1[5] <- DescTools::Gini(t2_8c$n)
g_i1[5] <- DescTools::Gini(t2_8i$n)

t2_8cv <-  df2_8 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al)

t2_8iv <- df2_8 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al)

g_c1v[5] <- DescTools::Gini(t2_8cv$n)
g_i1v[5] <- DescTools::Gini(t2_8iv$n)

### 2.2.9 Stratum 9----
t2_9c <-  df2_9 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

t2_9i <- df2_9 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

g_c1[6] <- DescTools::Gini(t2_9c$n)
g_i1[6] <- DescTools::Gini(t2_9i$n)

t2_9cv <-  df2_9 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

t2_9iv <- df2_9 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

g_c1v[6] <- DescTools::Gini(t2_9cv$n)
g_i1v[6] <- DescTools::Gini(t2_9iv$n)

#t2_4c$set <- "max Cindex"

### Erster Teil----
ac <- rbind(df2_4, df2_5, df2_6, df2_7, df2_8, df2_9) %>% 
  filter(y.Achse == "Cindex") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

ai <- rbind(df2_4, df2_5, df2_6, df2_7, df2_8, df2_9) %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

acv <- rbind(df2_4, df2_5, df2_6, df2_7, df2_8, df2_9) %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

aiv <- rbind(df2_4, df2_5, df2_6, df2_7, df2_8, df2_9) %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 


d <- data.frame(g = c(DescTools::Gini(ac$n), DescTools::Gini(ai$n), DescTools::Gini(acv$n), DescTools::Gini(aiv$n), g_c1[1:6], g_i1[1:6], g_c1v[1:6], g_i1v[1:6]),
                z = c("Cindex", "Ibrier", "Cindex", "Ibrier", rep(c(rep("Cindex", length(g_c1)), rep("Ibrier", length(g_i1))),2)),
                h = c("over all", "over all", "over all", "over all", rep(c("max\nCindex", "med\nCindex", "min\nCindex", 
                      "max\nIbrier", "med\nIbrier", "min\nIbrier"),4)),
                v = c("With high variability", "With high variability", "Without high variability", "Without high variability",
                      rep("With high variability", 12), rep("Without high variability", 12)))

d$z <- factor(d$z, levels = c("Cindex", "Ibrier"), ordered = T)
d$h <- factor(d$h, levels = c("over all", "max\nCindex", "med\nCindex", "min\nCindex", 
                              "max\nIbrier", "med\nIbrier", "min\nIbrier"), ordered = T)
d$v <- factor(d$v, levels = c("With high variability", "Without high variability"), ordered = T)


#pdf("Grafik2 Gini Approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(d, aes(y = g, x = h, fill = h))+
  geom_bar(stat = "identity", position = "dodge", color = "white")+
  facet_grid(z~ v, space = "free", scales = "free", switch = "x")+
  labs(y = "DescTools::Gini coefficient", x = "Different combinations")+
  theme_tufte()+
  theme(axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_fill_manual(values = c("over all" = gray3[1],
                               "max\nCindex" = gray3[3], "med\nCindex" = gray3[3], "min\nCindex" = gray3[3],
                               "max\nIbrier" = gray3[3], "med\nIbrier" = gray3[3], "min\nIbrier" = gray3[3]))

dev.off()



p1 <- ggplot(rbind(t2_4c,
             t2_5c %>% mutate(set = "median Cindex"),
             t2_6c %>% mutate(set = "min Cindex"),
             t2_7c %>% mutate(set = "max Ibrier"),
             t2_8c %>% mutate(set = "median Ibrier"),
             t2_9c %>% mutate(set = "min Ibrier")), aes(n, color = set)) +
  stat_lorenz(desc = F, linewidth = linesize)+
  coord_fixed() +
  geom_abline(linetype = "dashed", linewidth = linesize-.75, color = gray3[3]) +
  theme_tufte()+
  scale_color_manual(values = c("min Cindex" = col_s[2], "median Cindex" = col_m[2], "max Cindex" = col_h[2], 
                                "min Ibrier" = col_s[5], "median Ibrier" = col_m[5], "max Ibrier" = col_h[5]))+
  theme(axis.line = element_line(linewidth = axissize, color = axis_col),
        legend.position = "bottom")+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  scale_x_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "Proportion of attribute contributors", y = "Cumulated relative attribute sum",
       color = "Function")

#t2_4i$set <- "max Cindex"
p2 <- ggplot(rbind(t2_4i,
             t2_5i %>% mutate(set = "median Cindex"),
             t2_6i %>% mutate(set = "min Cindex"),
             t2_7i %>% mutate(set = "max Ibrier"),
             t2_8i %>% mutate(set = "median Ibrier"),
             t2_9i %>% mutate(set = "min Ibrier")), aes(n, color = set)) +
  stat_lorenz(desc = F, linewidth = linesize)+
  coord_fixed() +
  geom_abline(linetype = "dashed", linewidth = linesize-.75, color = gray3[3]) +
  theme_tufte()+
  scale_color_manual(values = c("min Cindex" = col_s[2], "median Cindex" = col_m[2], "max Cindex" = col_h[2], 
                                "min Ibrier" = col_s[5], "median Ibrier" = col_m[5], "max Ibrier" = col_h[5]))+
  theme(axis.line = element_line(linewidth = axissize, color = axis_col),
        legend.position = "bottom",
        axis.title.y = element_blank())+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  scale_x_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "Proportion of attribute contributors", y = "Cumulated relative attribute sum",
       color = "Function")

#pdf("Lorenz2_9.pdf", width = pdf_w_h[1]+1, height = pdf_w_h[2])
cowplot::plot_grid(p1, p2, nrow = 1, labels = c("Cindex", "Ibrier"), align = "v")
dev.off()

m <- rbind(df2_4, df2_5, df2_6 ,df2_7, df2_8, df2_9)
m %>% filter(y.Achse == "Cindex") %>% count(Variabilitaet_Al)
m %>% filter(y.Achse == "Ibrier") %>% count(Variabilitaet_Al)

#rm(m, g_c1, g_i1)

### 2.2.10 Stratum 10----
t2_10c <-  df2_10 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_10i <- df2_10 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

DescTools::Gini(t2_10c$n)
DescTools::Gini(t2_10i$n)

### 2.2.11 Stratum 11----
t2_11c <-  df2_11 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_11i <- df2_11 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

DescTools::Gini(t2_11c$n)
DescTools::Gini(t2_11i$n)

### 2.2.12 Stratum 12----
t2_12c <-  df2_12 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_12i <- df2_12 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

DescTools::Gini(t2_12c$n)
DescTools::Gini(t2_12i$n)

### 2.2.13 Stratum 13----
t2_13c <-  df2_13 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_13i <- df2_13 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c1[7] <- DescTools::Gini(t2_13c$n)
g_i1[7] <- DescTools::Gini(t2_13i$n)

t2_13cv <-  df2_13 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_13iv <- df2_13 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c1v[7] <- DescTools::Gini(t2_13cv$n)
g_c1v[7] <- 1
g_i1v[7] <- DescTools::Gini(t2_13iv$n)

### 2.2.14 Stratum 14----
t2_14c <-  df2_14 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_14i <- df2_14 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c1[8] <- DescTools::Gini(t2_14c$n)
g_i1[8] <- DescTools::Gini(t2_14i$n)


t2_14cv <-  df2_14 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_14iv <- df2_14 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c1v[8] <- DescTools::Gini(t2_14cv$n)
g_c1v[8] <- 1
g_i1v[8] <- DescTools::Gini(t2_14iv$n)

### 2.2.15 Stratum 15----
t2_15c <-  df2_15 %>% 
  filter(y.Achse == "Cindex" ) %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_15i <- df2_15 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c1[9] <- DescTools::Gini(t2_15c$n)
g_i1[9] <- DescTools::Gini(t2_15i$n)


t2_15cv <-  df2_15 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_15iv <- df2_15 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c1v[9] <- DescTools::Gini(t2_15cv$n)
g_i1v[9] <- DescTools::Gini(t2_15iv$n)

### 2.2.16 Stratum 16----
t2_16c <-  df2_16 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_16i <- df2_16 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c1[10] <- DescTools::Gini(t2_16c$n)
g_i1[10] <- DescTools::Gini(t2_16i$n)

t2_16cv <-  df2_16 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_16iv <- df2_16 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c1v[10] <- DescTools::Gini(t2_16cv$n)
g_i1v[10] <- DescTools::Gini(t2_16iv$n)

### 2.2.17 Stratum 17----
t2_17c <-  df2_17 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_17i <- df2_17 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c1[11] <- DescTools::Gini(t2_17c$n)
g_i1[11] <- DescTools::Gini(t2_17i$n)



t2_17cv <-  df2_17 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_17iv <- df2_17 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c1v[11] <- DescTools::Gini(t2_17cv$n)
g_i1v[11] <- DescTools::Gini(t2_17iv$n)

### 2.2.18 Stratum 18----
t2_18c <-  df2_18 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_18i <- df2_18 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c1[12] <- DescTools::Gini(t2_18c$n)
g_i1[12] <- DescTools::Gini(t2_18i$n)


t2_18cv <-  df2_18 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_18iv <- df2_18 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c1v[12] <- DescTools::Gini(t2_18cv$n)
g_i1v[12] <- DescTools::Gini(t2_18iv$n)

### Second part----
ac <- rbind(df2_13, df2_14, df2_15, df2_16, df2_17, df2_18) %>% 
  filter(y.Achse == "Cindex") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

ai <- rbind(df2_13, df2_14, df2_15, df2_16, df2_17, df2_18) %>% 
  filter(y.Achse == "Ibrier" ) %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

acv <- rbind(df2_13, df2_14, df2_15, df2_16, df2_17, df2_18) %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 

aiv <- rbind(df2_13, df2_14, df2_15, df2_16, df2_17, df2_18) %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) 


d <- data.frame(g = c(DescTools::Gini(ac$n), DescTools::Gini(ai$n), DescTools::Gini(acv$n), DescTools::Gini(aiv$n), g_c1[7:12], g_i1[7:12], g_c1v[7:12], g_i1v[7:12]),
                z = c("Cindex", "Ibrier", "Cindex", "Ibrier", rep(c(rep("Cindex", length(g_c1[7:12])), rep("Ibrier", length(g_i1[7:12]))),2)),
                h = c("over all", "over all", "over all", "over all", rep(c("min\nCindex", "max\nCindex", "med\nCindex", 
                                                                            "min\nIbrier", "max\nIbrier", "med\nIbrier"),4)),
                v = c("With high variability", "With high variability", "Without high variability", "Without high variability",
                      rep("With high variability", 12), rep("Without high variability", 12)))

d$z <- factor(d$z, levels = c("Cindex", "Ibrier"), ordered = T)
d$h <- factor(d$h, levels = c("over all", "max\nCindex", "med\nCindex", "min\nCindex", 
                              "max\nIbrier", "med\nIbrier", "min\nIbrier"), ordered = T)
d$v <- factor(d$v, levels = c("With high variability", "Without high variability"), ordered = T)





#pdf("Grafik2 Gini dataset.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(d, aes(y = g, x = h, fill = h))+
  geom_bar(stat = "identity", position = "dodge", color = "white")+
  facet_grid(z~ v, space = "free", scales = "free", switch = "x")+
  labs(y = "DescTools::Gini coefficient", x = "Different combinations")+
  theme_tufte()+
  theme(axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_fill_manual(values = c("over all" = gray3[1],
                               "max\nCindex" = gray3[3], "med\nCindex" = gray3[3], "min\nCindex" = gray3[3],
                               "max\nIbrier" = gray3[3], "med\nIbrier" = gray3[3], "min\nIbrier" = gray3[3]))

dev.off()




#t2_13c$set <- "min Cindex"
p1 <- ggplot(rbind(t2_13c,
                   t2_14c %>% mutate(set = "max Cindex"),
                   t2_15c %>% mutate(set = "median Cindex"),
                   t2_16c %>% mutate(set = "min Ibrier"),
                   t2_17c %>% mutate(set = "max Ibrier"),
                   t2_18c %>% mutate(set = "median Ibrier")), aes(n, color = set)) +
  stat_lorenz(desc = F, linewidth = linesize)+
  coord_fixed() +
  geom_abline(linetype = "dashed", linewidth = linesize-.75, color = gray3[3]) +
  theme_tufte()+
  scale_color_manual(values = c("min Cindex" = col_s[2], "median Cindex" = col_m[2], "max Cindex" = col_h[2], 
                                "min Ibrier" = col_s[5], "median Ibrier" = col_m[5], "max Ibrier" = col_h[5]))+
  theme(axis.line = element_line(linewidth = axissize, color = axis_col),
        legend.position = "bottom")+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  scale_x_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "Proportion of attribute contributors", y = "Cumulated relative attribute sum",
       color = "Function")

#t2_13i$set <- "min Cindex"
p2 <- ggplot(rbind(t2_13i,
                   t2_14i %>% mutate(set = "max Cindex"),
                   t2_15i %>% mutate(set = "median Cindex"),
                   t2_16i %>% mutate(set = "min Ibrier"),
                   t2_17i %>% mutate(set = "max Ibrier"),
                   t2_18i %>% mutate(set = "median Ibrier")), aes(n, color = set)) +
  stat_lorenz(desc = F, linewidth = linesize)+
  coord_fixed() +
  geom_abline(linetype = "dashed", linewidth = linesize-.75, color = gray3[3]) +
  theme_tufte()+
  scale_color_manual(values = c("min Cindex" = col_s[2], "median Cindex" = col_m[2], "max Cindex" = col_h[2], 
                                "min Ibrier" = col_s[5], "median Ibrier" = col_m[5], "max Ibrier" = col_h[5]))+
  theme(axis.line = element_line(linewidth = axissize, color = axis_col),
        legend.position = "bottom",
        axis.title.y = element_blank())+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  scale_x_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "Proportion of attribute contributors", y = "Cumulated relative attribute sum",
       color = "Function")

#pdf("Lorenz2_18.pdf", width = pdf_w_h[1]+1, height = pdf_w_h[2])
cowplot::plot_grid(p1, p2, nrow = 1, labels = c("Cindex", "Ibrier"), align = "v")
dev.off()


m <- rbind(t2_13c, t2_14c, t2_15c, t2_16c, t2_17c, t2_18c)

sum(m$n[m$Form_Al == "konstant"]) / sum((m$n[m$Form_Al == "logarithmisch"]) +
                                          sum(m$n[m$Form_Al == "exponentiell"]) +
                                          sum(m$n[m$Form_Al == "linear"]))

m <- rbind(t2_13i, t2_14i, t2_15i, t2_16i, t2_17i, t2_18i)
sum(m$n[m$Form_Al == "konstant"]) / sum((m$n[m$Form_Al == "logarithmisch"]) +
                                          sum(m$n[m$Form_Al == "exponentiell"]) +
                                          sum(m$n[m$Form_Al == "linear"]))
### 2.2.19 Stratum 19----
df2_19$Steigungsrichtung_Al <- factor(df2_19$Steigungsrichtung_Al, levels = c("negativ", "positiv", "neutral"))
t2_19c <-  df2_19 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "mittel", "stark")),
         !(Form_Al == "konstant" & Steigungsrichtung_Al %in% c("negativ", "positiv")),
         !(Form_Al != "konstant" & Steigung_Al == "konstant"),
         !(Form_Al != "konstant" & Steigungsrichtung_Al == "neutral"))

t2_19i <- df2_19 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "mittel", "stark")),
         !(Form_Al == "konstant" & Steigungsrichtung_Al %in% c("negativ", "positiv")),
         !(Form_Al != "konstant" & Steigung_Al == "konstant"),
         !(Form_Al != "konstant" & Steigungsrichtung_Al == "neutral"))

g_c1[13] <- DescTools::Gini(t2_19c$n)
g_i1[13] <- DescTools::Gini(t2_19i$n)


t2_19cv <-  df2_19 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "mittel", "stark")),
         !(Form_Al == "konstant" & Steigungsrichtung_Al %in% c("negativ", "positiv")),
         !(Form_Al != "konstant" & Steigung_Al == "konstant"),
         !(Form_Al != "konstant" & Steigungsrichtung_Al == "neutral"))

t2_19iv <- df2_19 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "mittel", "stark")),
         !(Form_Al == "konstant" & Steigungsrichtung_Al %in% c("negativ", "positiv")),
         !(Form_Al != "konstant" & Steigung_Al == "konstant"),
         !(Form_Al != "konstant" & Steigungsrichtung_Al == "neutral"))

g_c1v[13] <- DescTools::Gini(t2_19cv$n)
g_i1v[13] <- DescTools::Gini(t2_19iv$n)

p1 <- ggplot(t2_19c %>% mutate(set = "Cindex"), aes(n)) +
  stat_lorenz(desc = F, linewidth = linesize)+
  coord_fixed() +
  geom_abline(linetype = "dashed", linewidth = linesize-.75, color = gray3[3]) +
  theme_tufte()+
  scale_color_manual(values = c("min Cindex" = col_s[2], "median Cindex" = col_m[2], "max Cindex" = col_h[2], 
                                "min Ibrier" = col_s[5], "median Ibrier" = col_m[5], "max Ibrier" = col_h[5]))+
  theme(axis.line = element_line(linewidth = axissize, color = axis_col),
        legend.position = "bottom")+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  scale_x_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "Proportion of attribute contributors", y = "Cumulated relative attribute sum",
       color = "Function")

p2 <- ggplot(t2_19i %>% mutate(set = "Ibrier"), aes(n)) +
  stat_lorenz(desc = F, linewidth = linesize)+
  coord_fixed() +
  geom_abline(linetype = "dashed", linewidth = linesize-.75, color = gray3[3]) +
  theme_tufte()+
  scale_color_manual(values = c("min Cindex" = col_s[2], "median Cindex" = col_m[2], "max Cindex" = col_h[2], 
                                "min Ibrier" = col_s[5], "median Ibrier" = col_m[5], "max Ibrier" = col_h[5]))+
  theme(axis.line = element_line(linewidth = axissize, color = axis_col),
        legend.position = "bottom",
        axis.title.y = element_blank())+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  scale_x_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "Proportion of attribute contributors", y = "Cumulated relative attribute sum",
       color = "Function")

#pdf("Lorenz2_19.pdf", width = pdf_w_h[1]+1, height = pdf_w_h[2])
cowplot::plot_grid(p1, p2, nrow = 1, labels = c("Cindex", "Ibrier"), align = "v")
dev.off()

### 2.2.20 Stratum 20----
t2_20c <-  df2_20 %>% 
  filter(y.Achse == "Cindex") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "mittel", "stark")),
         !(Form_Al == "konstant" & Steigungsrichtung_Al %in% c("negativ", "positiv")),
         !(Form_Al != "konstant" & Steigung_Al == "konstant"),
         !(Form_Al != "konstant" & Steigungsrichtung_Al == "neutral"))

t2_20i <- df2_20 %>% 
  filter(y.Achse == "Ibrier") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "mittel", "stark")),
         !(Form_Al == "konstant" & Steigungsrichtung_Al %in% c("negativ", "positiv")),
         !(Form_Al != "konstant" & Steigung_Al == "konstant"),
         !(Form_Al != "konstant" & Steigungsrichtung_Al == "neutral"))

g_c1[14] <- DescTools::Gini(t2_20c$n)
g_i1[14] <- DescTools::Gini(t2_20i$n)


t2_20cv <-  df2_20 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "mittel", "stark")),
         !(Form_Al == "konstant" & Steigungsrichtung_Al %in% c("negativ", "positiv")),
         !(Form_Al != "konstant" & Steigung_Al == "konstant"),
         !(Form_Al != "konstant" & Steigungsrichtung_Al == "neutral"))

t2_20iv <- df2_20 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "mittel", "stark")),
         !(Form_Al == "konstant" & Steigungsrichtung_Al %in% c("negativ", "positiv")),
         !(Form_Al != "konstant" & Steigung_Al == "konstant"),
         !(Form_Al != "konstant" & Steigungsrichtung_Al == "neutral"))

g_c1v[14] <- DescTools::Gini(t2_20cv$n)
g_i1v[14] <- DescTools::Gini(t2_20iv$n)

p1 <- ggplot(t2_20c %>% mutate(set = "Cindex"), aes(n)) +
  stat_lorenz(desc = F, linewidth = linesize)+
  coord_fixed() +
  geom_abline(linetype = "dashed", linewidth = linesize-.75, color = gray3[3]) +
  theme_tufte()+
  scale_color_manual(values = c("min Cindex" = col_s[2], "median Cindex" = col_m[2], "max Cindex" = col_h[2], 
                                "min Ibrier" = col_s[5], "median Ibrier" = col_m[5], "max Ibrier" = col_h[5]))+
  theme(axis.line = element_line(linewidth = axissize, color = axis_col),
        legend.position = "bottom")+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  scale_x_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "Proportion of attribute contributors", y = "Cumulated relative attribute sum",
       color = "Function")

p2 <- ggplot(t2_20i %>% mutate(set = "Ibrier"), aes(n)) +
  stat_lorenz(desc = F, linewidth = linesize)+
  coord_fixed() +
  geom_abline(linetype = "dashed", linewidth = linesize-.75, color = gray3[3]) +
  theme_tufte()+
  scale_color_manual(values = c("min Cindex" = col_s[2], "median Cindex" = col_m[2], "max Cindex" = col_h[2], 
                                "min Ibrier" = col_s[5], "median Ibrier" = col_m[5], "max Ibrier" = col_h[5]))+
  theme(axis.line = element_line(linewidth = axissize, color = axis_col),
        legend.position = "bottom",
        axis.title.y = element_blank())+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  scale_x_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "Proportion of attribute contributors", y = "Cumulated relative attribute sum",
       color = "Function")

#pdf("Lorenz2_20.pdf", width = pdf_w_h[1]+1, height = pdf_w_h[2])
cowplot::plot_grid(p1, p2, nrow = 1, labels = c("Cindex", "Ibrier"), align = "v")
dev.off()

# 13 = 7
# 14 = 8
# 15 = 9
# 16 = 10
# 17 = 11
# 18 = 12
# 19 = 13
# 20 = 14

t2_allgc <- rbind(df2_19, df2_20) %>% filter(y.Achse == "Cindex") %>% count(Form_Al, Steigung_Al, Steigungsrichtung_Al)
t2_allgi <- rbind(df2_19, df2_20) %>% filter(y.Achse == "Ibrier") %>% count(Form_Al, Steigung_Al, Steigungsrichtung_Al)

### 2.2.12 Distribution of the Ginicoefficients----
d <- data.frame(g = c(DescTools::Gini(t2_allgc$n), g_c1[7:14], DescTools::Gini(t2_allgi$n),g_i1[7:14]),
                i = c(rep("Cindex", length(g_c1[7:14])+1), rep("Ibrier", length(g_i1[7:14])+1)),
                f = rep(c("Perf. - Dataset", "Perf. - Dataset\n(trans.)", "Perf. - Dataset\n(trans.)",
                      "Perf. - Dataset\n(trans.)", "Perf. - Dataset\n(trans.)", "Perf. - Dataset\n(trans.)",
                      "Perf. - Dataset\n(trans.)", "Perf. - Dataset\n(ord.)", "Perf. - Dataset\n(ratio)"),2),
                h = rep(c("general", "min\nCindex", "max\nCindex", "med\nCindex",
                      "min\nIbrier", "max\nIbrier", "med\nIbrier",
                      "general", "general"),2))
d$f <- factor(d$f, levels = c("Perf. - Dataset",
                              "Perf. - Dataset\n(ord.)",
                              "Perf. - Dataset\n(ratio)",
                              "Perf. - Dataset\n(trans.)"), ordered = T)
#d$h <- factor(d$h, levels = c("max\nCindex", "median\nCindex", "min\nCindex",
#                       "max\nIbrier", "median\nIbrier", "min\nIbrier",
#                       "ordinal\ndataset", "ratio\ndataset"), ordered = T)
#d$h <- forcats::fct_reorder(d$h,d$g) 


p1 <- ggplot(d[d$i == "Cindex",], aes(y = g, x = h, fill = h))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(.~ f, space = "free_x", scales = "free_x", switch = "x")+
  labs(y = "With high variability\nGini coefficient", x = "Different combinations")+
  theme_tufte()+
  theme(axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_fill_manual(values = c("gernal" = gray3[1],
                               "min\nCindex" = gray3[3], "max\nCindex" = gray3[3], "med\nCindex" = gray3[3],
                               "min\nIbrier" = gray3[3], "max\nIbrier" = gray3[3], "med\nIbrier" = gray3[3]))

p2 <- ggplot(d[d$i == "Ibrier",], aes(y = g, x = h, fill = h))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(.~ f, space = "free_x", scales = "free_x", switch = "x")+
  labs(y = "Gini coefficient", x = "Different combinations")+
  theme_tufte()+
  theme(axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_fill_manual(values = c("gernal" = gray3[1],
                               "min\nCindex" = gray3[3], "max\nCindex" = gray3[3], "med\nCindex" = gray3[3],
                               "min\nIbrier" = gray3[3], "max\nIbrier" = gray3[3], "med\nIbrier" = gray3[3]))

t2_allgcv <- rbind(df2_19, df2_20) %>% filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>% count(Form_Al, Steigung_Al, Steigungsrichtung_Al)
t2_allgiv <- rbind(df2_19, df2_20) %>% filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% count(Form_Al, Steigung_Al, Steigungsrichtung_Al)


dv <- data.frame(g = c(DescTools::Gini(t2_allgcv$n), g_c1v[7:14], DescTools::Gini(t2_allgiv$n),g_i1v[7:14]),
                i = c(rep("Cindex", length(g_c1[7:14])+1), rep("Ibrier", length(g_i1[7:14])+1)),
                f = rep(c("Perf. - Dataset", "Perf. - Dataset\n(trans.)", "Perf. - Dataset\n(trans.)",
                          "Perf. - Dataset\n(trans.)", "Perf. - Dataset\n(trans.)", "Perf. - Dataset\n(trans.)",
                          "Perf. - Dataset\n(trans.)", "Perf. - Dataset\n(ord.)", "Perf. - Dataset\n(ratio)"),2),
                h = rep(c("general", "min\nCindex", "max\nCindex", "med\nCindex",
                          "min\nIbrier", "max\nIbrier", "med\nIbrier",
                          "general", "general"),2))
d$f <- factor(d$f, levels = c("Perf. - Dataset",
                              "Perf. - Dataset\n(ord.)",
                              "Perf. - Dataset\n(ratio)",
                              "Perf. - Dataset\n(trans.)"), ordered = T)

p3 <- ggplot(dv[dv$i == "Cindex",], aes(y = g, x = h, fill = h))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(.~ f, space = "free_x", scales = "free_x", switch = "x")+
  labs(y = "Without high variability\nGini coefficient", x = "Different combinations")+
  theme_tufte()+
  theme(axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_fill_manual(values = c("gernal" = gray3[1],
                               "min\nCindex" = gray3[3], "max\nCindex" = gray3[3], "med\nCindex" = gray3[3],
                               "min\nIbrier" = gray3[3], "max\nIbrier" = gray3[3], "med\nIbrier" = gray3[3]))

p4 <- ggplot(dv[dv$i == "Ibrier",], aes(y = g, x = h, fill = h))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(.~ f, space = "free_x", scales = "free_x", switch = "x")+
  labs(y = "Gini coefficient", x = "Different combinations")+
  theme_tufte()+
  theme(axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_fill_manual(values = c("gernal" = gray3[1],
                               "min\nCindex" = gray3[3], "max\nCindex" = gray3[3], "med\nCindex" = gray3[3],
                               "min\nIbrier" = gray3[3], "max\nIbrier" = gray3[3], "med\nIbrier" = gray3[3]))

#pdf("Grafik2 Gini.pdf", width = pdf_w_h[1]*1.5, height = pdf_w_h[2]*1.5)
cowplot::plot_grid(p1, p2, p3, p4 ,nrow = 2, labels = c("Cindex", "Ibrier"), align = "v", label_x = .05)
dev.off()

### 1.2.13 Distribution of Form, Gradient and Variability----
d <- y %>% group_by(Form_Al, Steigung_Al, Variabilitaet_Al) %>% count()
d <- d[!is.na(d$Form_Al),]
m <- d %>% group_by(Form_Al, Steigung_Al) %>% mutate(n = sum(n)) %>% filter(1:n() == 1) %>% mutate(Variabilitaet_Al = "alle")

d <- union(d, m)
d <- rbind(d, data.frame(Form_Al = c("linear", "linear"),
                         Steigung_Al = c("stark", "stark"),
                         Variabilitaet_Al = c("schwach", "stark"),
                         n = c(0, 0)))
d$Form_Al <- factor(d$Form_Al, levels = c("konstant", "exponentiell", "linear", "logarithmisch"), ordered = T)
d$Variabilitaet_Al <- factor(d$Variabilitaet_Al, levels = c("alle", "schwach", "maessig", "stark"), ordered = T)
d$Steigung_Al <- factor(d$Steigung_Al, levels = c("konstant", "schwach", "mittel", "stark"), ordered = T)

#pdf("form_steigung_Var_Graf2.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(d,
       aes(y = n, x = Steigung_Al, fill = Variabilitaet_Al))+
  geom_col(size = linesize-.5, position = "dodge", color = "white")+
  facet_grid( . ~ Form_Al, space = "free_x", scales = "free_x", switch = "x",
             labeller = as_labeller(c(`linear` = "linear", `exponentiell` = "exponential", `logarithmisch` = "logarthmic", `konstant` = "constant")))+
  theme_tufte()+
  theme(axis.line.y = element_line(linewidth = axissize, color = axis_col),
        axis.ticks.x = element_blank(),
        strip.placement = "outside",
        legend.position = "bottom",
        strip.text.x = element_text(size = 10))+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "Shape and slope", y = "Count")+
  #scale_x_discrete(labels=c("linear" = "linear", "exponentiell" = "exponential",
  #                          "logarithmisch" = "logarithmic", "konstant" = "constant"))+
  scale_x_discrete(labels = c("schwach" = "weak", "mittel" = "moderate", "stark" = "strong", "konstant" = "constant"))+
  scale_fill_manual(labels = c("alle" = "over all", "schwach" = "weak", "maessig" = "moderate", "stark" = "strong"), 
                    values = c("alle" = gray2[1], "schwach" = col_s[2], "maessig" = col_m[2], "stark" = col_h[2]))+
  labs(fill = "Variability")
dev.off()

## 2.3 Gini without high Variability----
### 2.2.1 Stratum 1----
t <-  df2_1 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t1 <- df2_1 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

DescTools::Gini(t$n)
DescTools::Gini(t1$n)
rm(t, t1)

### 2.2.2 Stratum 2----
t <-  df2_2 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t1 <- df2_2 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

DescTools::Gini(t$n)
DescTools::Gini(t1$n)
rm(t, t1)

### 2.2.3 Stratum 3----
t <-  df2_3 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t1 <- df2_3 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

DescTools::Gini(t$n)
DescTools::Gini(t1$n)
rm(t, t1)

### 2.2.4 Stratum 4----
t2_4vc <-  df2_4 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_4vi <- df2_4 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c2 <- DescTools::Gini(t2_4vc$n)
g_i2 <- DescTools::Gini(t2_4vi$n)

### 2.2.5 Stratum 5----
t2_5vc <-  df2_5 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_5vi <- df2_5 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c2[2] <- DescTools::Gini(t2_5vc$n)
g_i2[2] <- DescTools::Gini(t2_5vi$n)

### 2.2.6 Stratum 6----
t2_6vc <-  df2_6 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_6vi <- df2_6 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c2[3] <- DescTools::Gini(t2_6vc$n)
g_i2[3] <- DescTools::Gini(t2_6vi$n)

### 2.2.7 Stratum 7----
t2_7vc <-  df2_7 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_7vi <- df2_7 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c2[4] <- DescTools::Gini(t2_7vc$n)
g_i2[4] <- DescTools::Gini(t2_7vi$n)

### 2.2.8 Stratum 8----
t2_8vc <-  df2_8 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_8vi <- df2_8 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c2[5] <- DescTools::Gini(t2_8vc$n)
g_i2[5] <- DescTools::Gini(t2_8vi$n)

### 2.2.9 Stratum 9----
t2_9vc <-  df2_9 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_9vi <- df2_9 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c2[6] <- DescTools::Gini(t2_9vc$n)
g_i2[6] <- DescTools::Gini(t2_9vi$n)

### 2.2.10 Stratum 10----
t2_10vc <-  df2_10 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_10vi <- df2_10 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

DescTools::Gini(t2_10vc$n)
DescTools::Gini(t2_10vi$n)

### 2.2.11 Stratum 11----
t2_11vc <-  df2_11 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_11vi <- df2_11 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

DescTools::Gini(t2_11vc$n)
DescTools::Gini(t2_11vi$n)

### 2.2.12 Stratum 12----
t2_12vc <-  df2_12 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_12vi <- df2_12 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

DescTools::Gini(t2_12vc$n)
DescTools::Gini(t2_12vi$n)

### 2.2.13 Stratum 13----
t2_13vc <-  df2_13 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_13vi <- df2_13 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c2[7] <- DescTools::Gini(t2_13vc$n)
g_i2[7] <- DescTools::Gini(t2_13vi$n)

### 2.2.14 Stratum 14----
t2_14vc <-  df2_14 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_14vi <- df2_14 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c2[8] <- DescTools::Gini(t2_14vc$n)
g_i2[8] <- DescTools::Gini(t2_14vi$n)

### 2.2.15 Stratum 15----
t2_15vc <-  df2_15 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_15vi <- df2_15 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c2[9] <- DescTools::Gini(t2_15vc$n)
g_i2[9] <- DescTools::Gini(t2_15vi$n)

### 2.2.16 Stratum 16----
t2_16vc <-  df2_16 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_16vi <- df2_16 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c2[10] <- DescTools::Gini(t2_16vc$n)
g_i2[10] <- DescTools::Gini(t2_16vi$n)

### 2.2.17 Stratum 17----
t2_17vc <-  df2_17 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_17vi <- df2_17 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c2[11] <- DescTools::Gini(t2_17vc$n)
g_i2[11] <- DescTools::Gini(t2_17vi$n)

### 2.2.18 Stratum 18----
t2_18vc <-  df2_18 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_18vi <- df2_18 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c2[12] <- DescTools::Gini(t2_18vc$n)
g_i2[12] <- DescTools::Gini(t2_18vi$n)


### 2.2.19 Stratum 19----
t2_19vc <-  df2_19 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_19vi <- df2_19 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c2[13] <- DescTools::Gini(t2_19vc$n)
g_i2[13] <- DescTools::Gini(t2_19vi$n)

### 2.2.20 Stratum 20----
t2_20vc <-  df2_20 %>% 
  filter(y.Achse == "Cindex" & Variabilitaet_Al != "stark") %>%
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>%
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

t2_20vi <- df2_20 %>% 
  filter(y.Achse == "Ibrier" & Variabilitaet_Al != "stark") %>% 
  count(Form_Al, Steigung_Al, Steigungsrichtung_Al) %>% 
  filter(!(Form_Al == "konstant" & Steigung_Al %in% c("schwach", "maessig", "stark")) &
           !(Form_Al != "konstant" & Steigung_Al == "konstant"))

g_c2[14] <- DescTools::Gini(t2_20vc$n)
g_i2[14] <- DescTools::Gini(t2_20vi$n)

### 2.2.12 Verteilung der DescTools::Ginikoeffizienten----
mvc <- rbind(t2_4c, t2_5c, t2_6c, t2_7c, t2_8c, t2_9c,
      t2_13c, t2_14c, t2_15c, t2_16c, t2_17c, 
      t2_18c, t2_19c, t2_20c)
ovc <- rbind(t2_4vc, t2_5vc, t2_6vc, t2_7vc, t2_8vc, t2_9vc,
      t2_13vc, t2_14vc, t2_15vc, t2_16vc, t2_17vc,
      t2_18vc, t2_19vc, t2_20vc)

mvi <- rbind(t2_4i, t2_5i, t2_6i, t2_7i, t2_8i, t2_9i,
            t2_13i, t2_14i, t2_15i, t2_16i, t2_17i, 
            t2_18i, t2_19i, t2_20i)
ovi <- rbind(t2_4vi, t2_5vi, t2_6vi, t2_7vi, t2_8vi, t2_9vi,
            t2_13vi, t2_14vi, t2_15vi, t2_16vi, t2_17vi,
            t2_18vi, t2_19vi, t2_20vi)

sum(mvc$n) - sum(ovc$n)
sum(mvi$n) - sum(ovi$n)

1- (sum(ovc$n) / sum(mvc$n))
1- (sum(ovi$n) / sum(mvi$n))

g_c2[7:8] <- 1

a <- data.frame(g = c(g_c1, g_i1, g_c2, g_i2),
                i = c(rep(c(rep("Cindex", length(g_c1)), rep("Ibrier", length(g_i1))),2)),
                f = rep(c(rep(c(rep("Perf. - Approach", 6),
                          rep("Perf. - Dataset (strata)", 6),
                          rep("Perf. - Dataset", 2)),2)),2),
                h = rep(c(rep(c("max\nCindex", "median\nCindex", "min\nCindex", "max\nIbrier",
                          "median\nIbrier", "min\nIbrier", "min\nCindex",
                          "max\nCindex", "median\nCindex", "min\nIbrier",
                          "max\nIbrier", "median\nIbrier", "ordinal\ndataset",
                          "ratio\ndataset"),2)),2),
                j = c(rep("mit Var", 28), rep("ohne Var", 28)))
a$f <- factor(a$f, levels = c("Perf. - Approach",
                              "Perf. - Dataset (strata)",
                              "Perf. - Dataset"), ordered = T)


b <- data.frame(g = c(g_c2 - g_c1, g_i2 - g_i1),
                i = c(rep("Cindex", length(g_c1)), rep("Ibrier", length(g_i1))),
                f = rep(c(rep("Perf. - Approach", 6),
                                rep("Perf. - Dataset (strata)", 6),
                                rep("Perf. - Dataset", 2)),2),
                h = rep(c("max\nCindex", "median\nCindex", "min\nCindex", "max\nIbrier",
                                "median\nIbrier", "min\nIbrier", "min\nCindex",
                                "max\nCindex", "median\nCindex", "min\nIbrier",
                                "max\nIbrier", "median\nIbrier", "ordinal\ndataset",
                                "ratio\ndataset"),2))
b$f <- factor(b$f, levels = c("Perf. - Approach",
                              "Perf. - Dataset (strata)",
                              "Perf. - Dataset"), ordered = T)




p1 <- ggplot(a[a$i == "Cindex",], aes(y = g, x = h, fill = j))+
  geom_bar(stat = "identity", position = "dodge", color = "white")+
  facet_grid(.~ f, space = "free_x", scales = "free_x", switch = "x")+
  labs(y = "DescTools::Gini coefficient", x = "Different combinations")+
  theme_tufte()+
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.text = element_blank(),
        strip.placement = "none",
        legend.position = "bottom",
        axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_fill_manual(values = c("mit Var" = col_s[2], "ohne Var" = col_s[3]),
                    labels = c("with", "without"),
                    name = "High variability")

p2 <- ggplot(b[b$i == "Cindex",], aes(y = g, x = h))+
  geom_hline(yintercept = 0, linewidth = linesize, color = gray3[3])+
  geom_bar(stat = "identity", position = "dodge", fill = gray3[2])+
  facet_grid(.~ f, space = "free_x", scales = "free_x", switch = "x")+
  labs(y = "Difference", x = "Different combinations")+
  theme_tufte()+
  theme(axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.placement = "outside")+
  scale_y_continuous(expand = c(0,0), limits = c(-max(abs(b$g)),max(abs(b$g))))

p3 <- ggplot(a[a$i == "Ibrier",], aes(y = g, x = h, fill = j))+
  geom_bar(stat = "identity", position = "dodge", color = "white")+
  facet_grid(.~ f, space = "free_x", scales = "free_x", switch = "x")+
  labs(y = "DescTools::Gini coefficient", x = "Different combinations")+
  theme_tufte()+
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.text = element_blank(),
        strip.placement = "outside",
        legend.position = "bottom",
        axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_fill_manual(values = c("mit Var" = col_s[2], "ohne Var" = col_s[3]),
                    labels = c("with", "without"),
                    name = "High variability")

p4 <- ggplot(b[b$i == "Ibrier",], aes(y = g, x = h))+
  geom_hline(yintercept = 0, linewidth = linesize, color = gray3[3])+
  geom_bar(stat = "identity", position = "dodge", fill = gray3[2])+
  facet_grid(.~ f, space = "free_x", scales = "free_x", switch = "x")+
  labs(y = "Difference", x = "Different combinations")+
  theme_tufte()+
  theme(axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.placement = "outside")+
  scale_y_continuous(expand = c(0,0), limits = c(-max(abs(b$g)),max(abs(b$g))))

#pdf("Grafik2 Ginivar.pdf", width = pdf_w_h[1]*2, height = pdf_w_h[2]*2)
cowplot::plot_grid(p1, p3, p2, p4, nrow = 2, labels = c("Cindex", "Ibrier"), align = "v", label_x = .05,
                   rel_heights = c(1,1,1,1))
dev.off()

# 2.2 Referenzwerte----
load(file = "Referenzdaten.Rda")

table(ref1$Ref_Km_Al[ref1$y.Achse == "Cindex"])
prop.table(table(ref1$Ref_Km_Al[ref1$y.Achse == "Cindex"]))
DescTools::Gini(table(ref1$Ref_Km_Al[ref1$y.Achse == "Cindex"]))

table(ref1$Ref_Km_Al[ref1$y.Achse == "Ibrier"])
prop.table(table(ref1$Ref_Km_Al[ref1$y.Achse == "Ibrier"]))
DescTools::Gini(table(ref1$Ref_Km_Al[ref1$y.Achse == "Ibrier"]))

gref_1c <- DescTools::Gini(table(ref1$Ref_Km_Al[ref1$y.Achse == "Cindex"]))
gref_1i <- DescTools::Gini(table(ref1$Ref_Km_Al[ref1$y.Achse == "Ibrier"]))

gref_1c[2] <- DescTools::Gini(table(ref1_cmax$Ref_Km_Al[ref1_cmax$y.Achse == "Cindex"]))
gref_1i[2] <- DescTools::Gini(table(ref1_cmax$Ref_Km_Al[ref1_cmax$y.Achse == "Ibrier"]))

gref_1c[3] <- DescTools::Gini(table(ref1_cmed$Ref_Km_Al[ref1_cmed$y.Achse == "Cindex"]))
gref_1i[3] <- DescTools::Gini(table(ref1_cmed$Ref_Km_Al[ref1_cmed$y.Achse == "Ibrier"]))

gref_1c[4] <- DescTools::Gini(table(ref1_cmin$Ref_Km_Al[ref1_cmin$y.Achse == "Cindex"]))
gref_1i[4] <- DescTools::Gini(table(ref1_cmin$Ref_Km_Al[ref1_cmin$y.Achse == "Ibrier"]))

gref_1c[5] <- DescTools::Gini(table(ref1_imax$Ref_Km_Al[ref1_imax$y.Achse == "Cindex"]))
gref_1i[5] <- DescTools::Gini(table(ref1_imax$Ref_Km_Al[ref1_imax$y.Achse == "Ibrier"]))

gref_1c[6] <- DescTools::Gini(table(ref1_imed$Ref_Km_Al[ref1_imed$y.Achse == "Cindex"]))
gref_1i[6] <- DescTools::Gini(table(ref1_imed$Ref_Km_Al[ref1_imed$y.Achse == "Ibrier"]))

gref_1c[7] <- DescTools::Gini(table(ref1_imin$Ref_Km_Al[ref1_imin$y.Achse == "Cindex"]))
gref_1i[7] <- DescTools::Gini(table(ref1_imin$Ref_Km_Al[ref1_imin$y.Achse == "Ibrier"]))

gref_1i[is.na(gref_1i)] <- 1

d <- data.frame(g = c(gref_1c, gref_1i),
                h = c(rep("Cindex", 7), rep("Ibrier", 7)),
                i = c("over all", "cmax", "cmed", "cmin", 
                      "imax", "imed", "imin"))
d$i <- factor(d$i, levels = c("over all", "cmax", "cmed", "cmin", 
                       "imax", "imed", "imin"), ordered = T)

#pdf("Grafik2 Ref Alg.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(d, aes(y = g, x = i, fill = i))+
  geom_bar(stat = "identity", position = "dodge")+
#  facet_grid(.~ f, space = "free_x", scales = "free_x", switch = "x")+
  labs(y = "DescTools::Gini coefficient", x = "Different combinations")+
  theme_tufte()+
  facet_grid(.~h, space = "free_x", scales = "free_x", switch = "x")+
  theme(axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_fill_manual(values = c("over all" = gray3[1], 
                               "cmax" = gray3[3], "cmed" = gray3[3], "cmin" = gray3[3],
                               "imax" = gray3[3], "imed" = gray3[3], "imin" = gray3[3]))

dev.off()





table(ref2$Ref_Km_Al[ref2$y.Achse == "Cindex"])
prop.table(table(ref2$Ref_Km_Al[ref2$y.Achse == "Cindex"]))
DescTools::Gini(table(ref2$Ref_Km_Al[ref2$y.Achse == "Cindex"]))

table(ref2$Ref_Km_Al[ref2$y.Achse == "Ibrier"])
prop.table(table(ref2$Ref_Km_Al[ref2$y.Achse == "Ibrier"]))
DescTools::Gini(table(ref2$Ref_Km_Al[ref2$y.Achse == "Ibrier"]))

gref_2c <- DescTools::Gini(table(ref2$Ref_Km_Al[ref2$y.Achse == "Cindex"]))
gref_2i <- DescTools::Gini(table(ref2$Ref_Km_Al[ref2$y.Achse == "Ibrier"]))

gref_2c[2] <- DescTools::Gini(table(ref2_cmax$Ref_Km_Al[ref2_cmax$y.Achse == "Cindex"]))
gref_2i[2] <- DescTools::Gini(table(ref2_cmax$Ref_Km_Al[ref2_cmax$y.Achse == "Ibrier"]))

gref_2c[3] <- DescTools::Gini(table(ref2_cmed$Ref_Km_Al[ref2_cmed$y.Achse == "Cindex"]))
gref_2i[3] <- DescTools::Gini(table(ref2_cmed$Ref_Km_Al[ref2_cmed$y.Achse == "Ibrier"]))

gref_2c[4] <- DescTools::Gini(table(ref2_cmin$Ref_Km_Al[ref2_cmin$y.Achse == "Cindex"]))
gref_2i[4] <- DescTools::Gini(table(ref2_cmin$Ref_Km_Al[ref2_cmin$y.Achse == "Ibrier"]))

gref_2c[5] <- DescTools::Gini(table(ref2_imax$Ref_Km_Al[ref2_imax$y.Achse == "Cindex"]))
gref_2i[5] <- DescTools::Gini(table(ref2_imax$Ref_Km_Al[ref2_imax$y.Achse == "Ibrier"]))

gref_2c[6] <- DescTools::Gini(table(ref2_imed$Ref_Km_Al[ref2_imed$y.Achse == "Cindex"]))
gref_2i[6] <- DescTools::Gini(table(ref2_imed$Ref_Km_Al[ref2_imed$y.Achse == "Ibrier"]))

gref_2c[7] <- DescTools::Gini(table(ref2_imin$Ref_Km_Al[ref2_imin$y.Achse == "Cindex"]))
gref_2i[7] <- DescTools::Gini(table(ref2_imin$Ref_Km_Al[ref2_imin$y.Achse == "Ibrier"]))

gref_2i[is.na(gref_2i)] <- 1

d <- data.frame(g = c(gref_2c, gref_2i),
                h = c(rep("Cindex", 7), rep("Ibrier", 7)),
                i = c("over all", "cmax", "cmed", "cmin", 
                      "imax", "imed", "imin"))
d$i <- factor(d$i, levels = c("over all", "cmax", "cmed", "cmin", 
                              "imax", "imed", "imin"), ordered = T)

#pdf("Grafik2 Ref KM.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(d, aes(y = g, x = i, fill = i))+
  geom_bar(stat = "identity", position = "dodge")+
  #  facet_grid(.~ f, space = "free_x", scales = "free_x", switch = "x")+
  labs(y = "DescTools::Gini coefficient", x = "Different combinations")+
  theme_tufte()+
  facet_grid(.~h, space = "free_x", scale = "free_x", switch = "x")+
  theme(axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.placement = "outside", 
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_fill_manual(values = c("over all" = gray3[1], 
                               "cmax" = gray3[3], "cmed" = gray3[3], "cmin" = gray3[3],
                               "imax" = gray3[3], "imed" = gray3[3], "imin" = gray3[3]))

dev.off()






table(ref3$Ref_Coxph_Al[ref3$y.Achse == "Cindex"])
prop.table(table(ref3$Ref_Coxph_Al[ref3$y.Achse == "Cindex"]))
DescTools::Gini(table(ref3$Ref_Coxph_Al[ref3$y.Achse == "Cindex"]))

table(ref3$Ref_Coxph_Al[ref3$y.Achse == "Ibrier"])
prop.table(table(ref3$Ref_Coxph_Al[ref3$y.Achse == "Ibrier"]))
DescTools::Gini(table(ref3$Ref_Coxph_Al[ref3$y.Achse == "Ibrier"]))

gref_3c <- DescTools::Gini(table(ref3$Ref_Coxph_Al[ref3$y.Achse == "Cindex"]))
gref_3i <- DescTools::Gini(table(ref3$Ref_Coxph_Al[ref3$y.Achse == "Ibrier"]))

gref_3c[2] <- DescTools::Gini(table(ref3_cmax$Ref_Coxph_Al[ref3_cmax$y.Achse == "Cindex"]))
gref_3i[2] <- DescTools::Gini(table(ref3_cmax$Ref_Coxph_Al[ref3_cmax$y.Achse == "Ibrier"]))

gref_3c[3] <- DescTools::Gini(table(ref3_cmed$Ref_Coxph_Al[ref3_cmed$y.Achse == "Cindex"]))
gref_3i[3] <- DescTools::Gini(table(ref3_cmed$Ref_Coxph_Al[ref3_cmed$y.Achse == "Ibrier"]))

gref_3c[4] <- DescTools::Gini(table(ref3_cmin$Ref_Coxph_Al[ref3_cmin$y.Achse == "Cindex"]))
gref_3i[4] <- DescTools::Gini(table(ref3_cmin$Ref_Coxph_Al[ref3_cmin$y.Achse == "Ibrier"]))

gref_3c[5] <- DescTools::Gini(table(ref3_imax$Ref_Coxph_Al[ref3_imax$y.Achse == "Cindex"]))
gref_3i[5] <- DescTools::Gini(table(ref3_imax$Ref_Coxph_Al[ref3_imax$y.Achse == "Ibrier"]))

gref_3c[6] <- DescTools::Gini(table(ref3_imed$Ref_Coxph_Al[ref3_imed$y.Achse == "Cindex"]))
gref_3i[6] <- DescTools::Gini(table(ref3_imed$Ref_Coxph_Al[ref3_imed$y.Achse == "Ibrier"]))

gref_3c[7] <- DescTools::Gini(table(ref3_imin$Ref_Coxph_Al[ref3_imin$y.Achse == "Cindex"]))
gref_3i[7] <- DescTools::Gini(table(ref3_imin$Ref_Coxph_Al[ref3_imin$y.Achse == "Ibrier"]))

gref_3i[is.na(gref_3i)] <- 1

d <- data.frame(g = c(gref_3c, gref_3i),
                h = c(rep("Cindex", 7), rep("Ibrier", 7)),
                i = c("over all", "cmax", "cmed", "cmin", 
                      "imax", "imed", "imin"))
d$i <- factor(d$i, levels = c("over all", "cmax", "cmed", "cmin", 
                              "imax", "imed", "imin"), ordered = T)

#pdf("Grafik2 Ref Cox.pdf", width = pdf_w_h[1], height = pdf_w_h[2])
ggplot(d, aes(y = g, x = i, fill = i))+
  geom_bar(stat = "identity", position = "dodge")+
  #  facet_grid(.~ f, space = "free_x", scales = "free_x", switch = "x")+
  labs(y = "DescTools::Gini coefficient", x = "Different combinations")+
  theme_tufte()+
  facet_grid(.~h, space = "free_x", scale = "free_x", switch = "x")+
  theme(axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = axissize, color = axis_col),
        strip.placement = "outside", 
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_fill_manual(values = c("over all" = gray3[1], 
                               "cmax" = gray3[3], "cmed" = gray3[3], "cmin" = gray3[3],
                               "imax" = gray3[3], "imed" = gray3[3], "imin" = gray3[3]))

dev.off()
