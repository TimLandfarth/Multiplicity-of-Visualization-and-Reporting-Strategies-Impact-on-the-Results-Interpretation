library(ggplot2)
library(dplyr)
library(cowplot)
library(ggthemes)
library(randomizeR)

# 1. alle Grafiknamen----
graphs <- c(list.files("C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 1/Comparison/Balken"),
            list.files("C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 1/Comparison/clustered Balken"),
            list.files("C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 1/Comparison/Connected Dotplot"),
            list.files("C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 1/Comparison/Heatmap"),
            list.files("C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 1/Comparison/Matrix chart"),
            list.files("C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 1/Korrelation/Bars"),
            list.files("C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 1/Korrelation/Scatterplot"),
            list.files("C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 1/Multiple Verteilungen/Density"),
            list.files("C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 1/Multiple Verteilungen/Linien"),
            list.files("C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 1/Ranking/Boxplots"),
            list.files("C:/Uni/14. Semester/Masterarbeit/Multiple Grafiken/Herrmann/Grafik 1/Ranking/Boxplots")
            )

t <- data.frame(sample(1:length(graphs), 20))
t <- rbind(t, data.frame(sample(1:length(graphs), 20)))
t <- rbind(t, data.frame(sample(1:length(graphs), 20)))
t <- rbind(t, data.frame(sample(1:length(graphs), 20)))
t <- rbind(t, data.frame(sample(1:length(graphs), 20)))
t <- rbind(t, data.frame(sample(1:length(graphs), 20)))
t <- rbind(t, data.frame(sample(1:length(graphs), 20)))
t <- rbind(t, data.frame(sample(1:length(graphs), 20)))
t <- rbind(t, data.frame(sample(1:length(graphs), 20)))
t <- rbind(t, data.frame(sample(1:length(graphs), 20)))
t <- rbind(t, data.frame(sample(1:length(graphs), 20)))
t <- rbind(t, data.frame(sample(1:length(graphs), 20)))
t <- rbind(t, data.frame(sample(1:length(graphs), 20)))



ggplot(data.frame(y = exp(seq(-2.5,2.5,.01)),
                  x = 1:501),
       aes(x = x, y = y))+
  geom_line()+
  xlim(c(0,500))

ggplot(data.frame(y = exp(seq(-5,5,.01)),
                  x = 1:1001),
       aes(x = x, y = y))+
  geom_line()+
  xlim(c(500,1250))+
  ylim(0,150)

ggplot(data.frame(y = exp(seq(-2.5,2.5,.01)),
                  x = 1:501),
       aes(x = x, y = y))+
  geom_line()+
  xlim(c(0,400))+
  theme_tufte()+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line())
