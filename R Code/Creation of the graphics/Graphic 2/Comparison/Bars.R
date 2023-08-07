# 0. Set Wd and libraries----

## 0.1 libraries----
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(cowplot)
library(ggthemes)


## 0.2 WD----
setwd("multi-omics_benchmark_study-master")
gray1 <- "#969696"
gray2 <- c("#636363","#969696")
gray3 <- c("#636363","#969696","#bdbdbd")
col_s <- c("#8C8C8C", "#88BDE6", "#FBB258", "#90CD97", "#F6AAC9", "#BFA554", "#A899C7", "#EDDD46", "#F07E6E")
col_m <- c("#4D4D4D", "#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#B2912F", "#B276B2", "#DECF35", "#F15854")
col_h <- c("#000000", "#265DAB", "#DF5C24", "#059748", "#E5126F", "#9D722A", "#7B3A96", "#C7B42D", "#CB2027")
axis_col <- "grey70"
linesize <- 1.5
pointsize <- 5
axissize <- .5
pdf_w_h <- c((7/3)*4, (7/3) * 3)
scale_fill <-  c("block\nForest" = gray3[3],
                 "Cox\nproportional\nhazard" = gray3[2],
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
scale_fill_ap <-  c("Penalized regression" = gray3[3],
                 "Reference" = gray3[1],
                 "Boosting" = gray3[3],
                 "Random forest" = gray3[3]
)

# Function for calculating the median, whereby the smaller value of both numbers is used instead of the arithmetic mean for even numbers.
mymedian <- function(lst) {
  n <- length(lst)
  s <- sort(lst)
  ifelse(n%%2==1,s[(n+1)/2],s[floor((n+1)/2)])
}

# 1. Graphic paper----
load("data/merged-results.RData")
get_cv_means <- function(df, meas = "cindex.uno") {
  meas <- sym(meas)
  df %>%
    group_by(task.id, learner.id) %>%
    summarise(cv_mean = mean(!! enquo(meas)))
}
cindex <- get_cv_means(df_res) 
ibrier <- get_cv_means(df_res, "ibrier")
spars <-
  get_cv_means(df_res, "featselc_default") %>%
  filter(!learner.id %in% c("Kaplan-Meier",
                            "Clinical only",
                            "grridge",
                            "blockForest",
                            "rfsrc",
                            "ranger"))

x_nams <- c(
  "KM",
  "Lasso",
  "glmB",
  "CoxB",
  "CoxPH",
  "prior",
  "prior_f",
  "CoxB_f",
  "GRr",
  "BF",
  "rfsrc",
  "ranger",
  "IPF"
)

legends <- paste(x_nams, levels(cindex$learner.id), sep = " = ")

levels(cindex$learner.id) <- x_nams
levels(ibrier$learner.id) <- x_nams
levels(spars$learner.id) <- x_nams

order_by_appr <- c("KM", "CoxPH", "Lasso", "prior", "prior_f", "IPF", "GRr", "glmB", "CoxB", "CoxB_f", "rfsrc", "ranger", "BF")
cindex$learner.id <- factor(cindex$learner.id, levels = order_by_appr)
ibrier$learner.id <-  factor(ibrier$learner.id, levels = order_by_appr)
spars$learner.id <- factor(spars$learner.id, levels = order_by_appr)

colours <-
  c(rep("white", 2),
    rep("#56B4E9", 5),
    rep("#E69F00", 3),
    rep("#009E73", 3))


p_cindex <-
  ggplot(data = cindex) +
  geom_boxplot(aes(x = learner.id, y = cv_mean), fill = colours) +
  geom_hline(yintercept = 0.5982247, color = "red") +
  geom_hline(yintercept = 0.5, lty = "dashed") +
  labs(y = "cindex") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 5),
    axis.title.x = element_blank()
  ) 


p_ibrier <- 
  ggplot(data = ibrier) + 
  geom_boxplot(aes(x = learner.id, y = cv_mean), fill = colours) +
  geom_hline(yintercept = 0.1812635, color = "red") +
  geom_hline(yintercept = 0.1989360, lty = "dashed") +
  labs(y = "ibrier") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

colours2 <- c(rep("#56B4E9", 4), rep("#E69F00", 3))

p_spars <- 
  ggplot(data = spars) +
  geom_boxplot(aes(x = learner.id, y = cv_mean), fill = colours2) +
  theme_bw() +
  labs(y = "No. of features") +
  theme(legend.position = "none",
        axis.title.x = element_blank())

#pdf(file = "original.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(p_cindex, p_ibrier, p_spars,
          labels = c('A', 'B', 'C'),
          label_size = 12,
          align = "v",
          nrow = 3)
dev.off()

rm(df_res, p_cindex, p_ibrier, p_spars, colours, colours2, legends, order_by_appr, x_nams)

# 2. Merging Datasets----
df <- left_join(cindex, ibrier, by = c("task.id", "learner.id"), suffix = c("_cindex", "_ibrier"))
df <- left_join(df, spars, by = c("task.id", "learner.id"))
names(df)[5] <- "cv_mean_spars"
names(df)[2] <- "learner.id_k"
df <- df %>% mutate(
  learner.id = case_when(
    learner.id_k == "KM" ~ "Kaplan\nMeier",
    learner.id_k == "CoxPH" ~ "Cox\nproportional\nhazard",
    learner.id_k == "rfsrc" ~ "rfsrc",
    learner.id_k == "ranger" ~ "ranger",
    learner.id_k == "BF" ~ "block\nForest",
    learner.id_k == "CoxB" ~ "CoxBoost",
    learner.id_k == "CoxB_f" ~ "CoxBoost\nfavoring",
    learner.id_k == "glmB" ~ "glmboost",
    learner.id_k == "prior" ~ "priority\nLasso",
    learner.id_k == "prior_f" ~ "priority\nLasso\nfavoring",
    learner.id_k == "GRr" ~ "grridge",
    learner.id_k == "IPF" ~ "ipflasso",
    learner.id_k == "Lasso" ~ "Lasso"
  )
)

df <- df %>% mutate(
  approach = case_when(
    learner.id_k %in% c("KM", "CoxPH") ~ "Reference",
    learner.id_k %in% c("rfsrc", "ranger", "BF") ~ "Random forest",
    learner.id_k %in% c("CoxB", "CoxB_f", "glmB") ~ "Boosting",
    learner.id_k %in% c("prior", "prior_f", 
                      "GRr", "IPF", "Lasso")  ~ "Penalized regression"
  ))


df$n <- NA
df$n[which(df$task.id == "BLCA")] <- 382
df$n[which(df$task.id == "BRCA")] <- 735
df$n[which(df$task.id == "COAD")] <- 191
df$n[which(df$task.id == "ESCA")] <- 106
df$n[which(df$task.id == "HNSC")] <- 443
df$n[which(df$task.id == "KIRC")] <- 249
df$n[which(df$task.id == "KIRP")] <- 167
df$n[which(df$task.id == "LAML")] <- 35
df$n[which(df$task.id == "LGG")] <- 419
df$n[which(df$task.id == "LIHC")] <- 159
df$n[which(df$task.id == "LUAD")] <- 426
df$n[which(df$task.id == "LUSC")] <- 418
df$n[which(df$task.id == "OV")] <- 219
df$n[which(df$task.id == "PAAD")] <- 124
df$n[which(df$task.id == "SARC")] <- 126
df$n[which(df$task.id == "SKCM")] <- 249
df$n[which(df$task.id == "STAD")] <- 295
df$n[which(df$task.id == "UCEC")] <- 405

## saving-----
save(df, file = "data.RData")
save(axis_col, axissize, col_h, col_m, col_s, gray1, gray2, gray3, linesize, pdf_w_h, pointsize,
     scale_fill, scale_fill_ap, file = "stats.RData")

## 2.1 Overview for calculation----
bsp <- df[c(2:4,54:56, 93:95),c(6,1,8,3,4,5)] 
bsp$task.id <- droplevels(bsp$task.id)
bsp$learner.id <- droplevels(as.factor(bsp$learner.id))
levels(bsp$task.id) <- c("A", "B", "C")
levels(bsp$learner.id) <- c("X", "Y", "Z")
bsp <- bsp[order(bsp$learner.id),]
bsp$cv_mean_cindex <- round(bsp$cv_mean_cindex, 2)
bsp$cv_mean_ibrier <- round(bsp$cv_mean_ibrier, 2)
bsp$cv_mean_spars <- round(bsp$cv_mean_spars, 2)
colnames(bsp) <- c("Learner", "Data set (name)", "Data set (no. of observations)", 
                  "Cindex", "Ibrier", "No. of features")


bsp1 <- df[c(2:4,54:56, 93:95),c(6,7,1,8,3,4,5)] 
bsp1$task.id <- droplevels(bsp1$task.id)
bsp1$learner.id <- droplevels(as.factor(bsp1$learner.id))
bsp1$approach <- droplevels(as.factor(bsp1$approach))
levels(bsp1$task.id) <- c("A", "B", "C")
levels(bsp1$learner.id) <- c("X", "Y", "Z")
levels(bsp1$approach) <- c("M", "N")
bsp1 <- bsp1[order(bsp1$learner.id),]
bsp1$cv_mean_cindex <- round(bsp1$cv_mean_cindex, 2)
bsp1$cv_mean_ibrier <- round(bsp1$cv_mean_ibrier, 2)
bsp1$cv_mean_spars <- round(bsp1$cv_mean_spars, 2)
colnames(bsp1) <- c("Learner", "Approach", "Data set (name)", "Data set (no. of observations)", 
                   "Cindex", "Ibrier", "No. of features")

bsp2 <- df[c(2:4,54:56, 93:95),c(1,8,6,7,3,4,5)] 
bsp2$task.id <- droplevels(bsp2$task.id)
bsp2$learner.id <- droplevels(as.factor(bsp2$learner.id))
bsp2$approach <- droplevels(as.factor(bsp2$approach))
levels(bsp2$task.id) <- c("A", "B", "C")
levels(bsp2$learner.id) <- c("Z", "Y", "X")
levels(bsp2$approach) <- c("N", "M")
bsp2 <- bsp2[order(bsp2$task.id),]
bsp2$cv_mean_cindex <- round(bsp2$cv_mean_cindex, 2)
bsp2$cv_mean_ibrier <- round(bsp2$cv_mean_ibrier, 2)
bsp2$cv_mean_spars <- round(bsp2$cv_mean_spars, 2)
colnames(bsp2) <- c("Data set (name)", "Data set (no. of observations)",
                    "Learner", "Approach",
                    "Cindex", "Ibrier", "No. of features")



find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}

g <- tableGrob(bsp)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)

g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)

g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 10, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)


#pdf("learner_colour.pdf", width = 8, height = 3)
grid.draw(g)
dev.off()


g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 5, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 8, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)

g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 6, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 9, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 7, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 10, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 4, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 5, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 8, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)

g$grobs[find_cell( g, 3, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 6, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 9, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 4, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 7, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 10,4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

#pdf("dataset_colour.pdf", width = 8, height = 3)
grid.draw(g)
dev.off()

g <- tableGrob(bsp)

g$grobs[find_cell( g, 2, 5, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 3, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 4, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)

g$grobs[find_cell( g, 5, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 6, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 7, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)

g$grobs[find_cell( g, 8, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)
g$grobs[find_cell( g, 9, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)
g$grobs[find_cell( g, 10, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)

g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)

g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 10, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

#pdf("cindex_colour.pdf", width = 8, height = 3)
grid.draw(g)
dev.off()

g <- tableGrob(bsp)

g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 4, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)

g$grobs[find_cell( g, 5, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)
g$grobs[find_cell( g, 6, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)
g$grobs[find_cell( g, 7, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)

g$grobs[find_cell( g, 8, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)
g$grobs[find_cell( g, 9, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)
g$grobs[find_cell( g, 10, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)

g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)

g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 10, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

#pdf("ibrier_colour.pdf", width = 8, height = 3)
grid.draw(g)
dev.off()

g <- tableGrob(bsp)

g$grobs[find_cell( g, 2, 7, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 3, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 4, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)

g$grobs[find_cell( g, 5, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)
g$grobs[find_cell( g, 6, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)
g$grobs[find_cell( g, 7, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)

g$grobs[find_cell( g, 8, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)
g$grobs[find_cell( g, 9, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)
g$grobs[find_cell( g, 10, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)

g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)

g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 10, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

#pdf("spars_colour.pdf", width = 8, height = 3)
grid.draw(g)
dev.off()

g <- tableGrob(bsp)

g$grobs[find_cell( g, 2, 5, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 3, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 4, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)

g$grobs[find_cell( g, 5, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 6, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 7, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)

g$grobs[find_cell( g, 8, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)
g$grobs[find_cell( g, 9, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)
g$grobs[find_cell( g, 10, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)

g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 4, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)

g$grobs[find_cell( g, 5, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)
g$grobs[find_cell( g, 6, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)
g$grobs[find_cell( g, 7, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)

g$grobs[find_cell( g, 8, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)
g$grobs[find_cell( g, 9, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)
g$grobs[find_cell( g, 10, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)

g$grobs[find_cell( g, 2, 7, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 3, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 4, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)

g$grobs[find_cell( g, 5, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)
g$grobs[find_cell( g, 6, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)
g$grobs[find_cell( g, 7, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)

g$grobs[find_cell( g, 8, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)
g$grobs[find_cell( g, 9, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)
g$grobs[find_cell( g, 10, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)

g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)

g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 10, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

#pdf("alldim_colour.pdf", width = 8, height = 3)
grid.draw(g)
dev.off()

# 2.2 Mit Approach----
bsp1 <- df[c(2:4,54:56, 93:95),c(6,7,1,8,3,4,5)] 
bsp1$task.id <- droplevels(bsp1$task.id)
bsp1$learner.id <- droplevels(as.factor(bsp1$learner.id))
bsp1$approach <- droplevels(as.factor(bsp1$approach))
levels(bsp1$task.id) <- c("A", "B", "C")
levels(bsp1$learner.id) <- c("X", "Y", "Z")
levels(bsp1$approach) <- c("M", "N")
bsp1 <- bsp1[order(bsp1$learner.id),]
bsp1$cv_mean_cindex <- round(bsp1$cv_mean_cindex, 2)
bsp1$cv_mean_ibrier <- round(bsp1$cv_mean_ibrier, 2)
bsp1$cv_mean_spars <- round(bsp1$cv_mean_spars, 2)
colnames(bsp1) <- c("Learner", "Approach", "Data set (name)", "Data set (no. of observations)", 
                    "Cindex", "Ibrier", "No. of features")

g <- tableGrob((bsp1))

find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)

g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)

g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 10, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 5, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 6, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 7, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)

g$grobs[find_cell( g, 8, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)
g$grobs[find_cell( g, 9, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)
g$grobs[find_cell( g, 10, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)


#pdf("learner_approach_colour.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 5, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 6, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 7, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)

g$grobs[find_cell( g, 8, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)
g$grobs[find_cell( g, 9, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)
g$grobs[find_cell( g, 10, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)

g$grobs[find_cell( g, 2, 4, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 5, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 8, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)

g$grobs[find_cell( g, 3, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 6, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 9, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 4, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 7, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 10,4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 5, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 5, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 8, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)

g$grobs[find_cell( g, 3, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 6, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 9, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 4, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 7, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 10, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

#pdf("dataset_approach_colour.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

g <- tableGrob(bsp1)

g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 4, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 5, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 6, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 7, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)

g$grobs[find_cell( g, 8, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)
g$grobs[find_cell( g, 9, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)
g$grobs[find_cell( g, 10, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)

g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)

g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 10, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 5, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 6, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 7, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)

g$grobs[find_cell( g, 8, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)
g$grobs[find_cell( g, 9, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)
g$grobs[find_cell( g, 10, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)

#pdf("cindex_approach_colour.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

g <- tableGrob(bsp1)

g$grobs[find_cell( g, 2, 7, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 3, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 4, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 5, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 6, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 7, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)

g$grobs[find_cell( g, 8, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)
g$grobs[find_cell( g, 9, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)
g$grobs[find_cell( g, 10,7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)

g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)

g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 10, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 5, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 6, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 7, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)

g$grobs[find_cell( g, 8, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)
g$grobs[find_cell( g, 9, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)
g$grobs[find_cell( g, 10, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)

#pdf("ibrier_approach_colour.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

g <- tableGrob(bsp1)

g$grobs[find_cell( g, 2, 8, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 3, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 4, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 5, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 6, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 7, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)

g$grobs[find_cell( g, 8, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)
g$grobs[find_cell( g, 9, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)
g$grobs[find_cell( g, 10,8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)

g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)

g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 10, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 5, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 6, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 7, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)

g$grobs[find_cell( g, 8, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)
g$grobs[find_cell( g, 9, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)
g$grobs[find_cell( g, 10, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)

#pdf("spars_approach_colour.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

g <- tableGrob(bsp1)

g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 4, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 5, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 6, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 7, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)

g$grobs[find_cell( g, 8, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)
g$grobs[find_cell( g, 9, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)
g$grobs[find_cell( g, 10,6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)

g$grobs[find_cell( g, 2, 7, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 3, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 4, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 5, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 6, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 7, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)

g$grobs[find_cell( g, 8, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)
g$grobs[find_cell( g, 9, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)
g$grobs[find_cell( g, 10,7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)

g$grobs[find_cell( g, 2, 8, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 3, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 4, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 5, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 6, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 7, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)

g$grobs[find_cell( g, 8, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)
g$grobs[find_cell( g, 9, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)
g$grobs[find_cell( g, 10,8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)

g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)

g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)
g$grobs[find_cell( g, 10, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 5, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 6, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 7, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)

g$grobs[find_cell( g, 8, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)
g$grobs[find_cell( g, 9, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)
g$grobs[find_cell( g, 10, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)

#pdf("alldim_approach_colour.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

## 2.3 Mit Dataset----
g <- tableGrob(bsp2)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)

g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 10, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)

g$grobs[find_cell( g, 5, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 6, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 7, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 8, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 9, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 10, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 4, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)

g$grobs[find_cell( g, 5, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 6, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 7, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)

g$grobs[find_cell( g, 8, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)
g$grobs[find_cell( g, 9, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)
g$grobs[find_cell( g, 10, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)


#pdf("bars_dataset_cindex.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()


# ibrier
g <- tableGrob(bsp2)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)

g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 10, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)

g$grobs[find_cell( g, 5, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 6, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 7, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 8, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 9, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 10, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 3, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 4, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)

g$grobs[find_cell( g, 5, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)
g$grobs[find_cell( g, 6, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)
g$grobs[find_cell( g, 7, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)

g$grobs[find_cell( g, 8, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)
g$grobs[find_cell( g, 9, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)
g$grobs[find_cell( g, 10,7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)


#pdf("bars_dataset_ibrier.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

# spars
g <- tableGrob(bsp2)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)

g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 10, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)

g$grobs[find_cell( g, 5, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 6, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 7, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 8, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 9, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 10, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 3, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 4, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)

g$grobs[find_cell( g, 5, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)
g$grobs[find_cell( g, 6, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)
g$grobs[find_cell( g, 7, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)

g$grobs[find_cell( g, 8, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)
g$grobs[find_cell( g, 9, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)
g$grobs[find_cell( g, 10,8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)


#pdf("bars_dataset_spars.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

# alle
g <- tableGrob(bsp2)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)

g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 10, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[4], col = col_s[4], lwd=5)

g$grobs[find_cell( g, 5, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 6, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 7, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 8, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 9, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)
g$grobs[find_cell( g, 10, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 4, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)

g$grobs[find_cell( g, 5, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 6, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 7, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)

g$grobs[find_cell( g, 8, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)
g$grobs[find_cell( g, 9, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)
g$grobs[find_cell( g, 10, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)

g$grobs[find_cell( g, 2, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 3, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 4, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[7], col = col_s[7], lwd=5)

g$grobs[find_cell( g, 5, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)
g$grobs[find_cell( g, 6, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)
g$grobs[find_cell( g, 7, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)

g$grobs[find_cell( g, 8, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)
g$grobs[find_cell( g, 9, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)
g$grobs[find_cell( g, 10,7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)

g$grobs[find_cell( g, 2, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 3, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 4, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[9], col = col_s[9], lwd=5)

g$grobs[find_cell( g, 5, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)
g$grobs[find_cell( g, 6, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)
g$grobs[find_cell( g, 7, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)

g$grobs[find_cell( g, 8, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)
g$grobs[find_cell( g, 9, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)
g$grobs[find_cell( g, 10,8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)


#pdf("bars_dataset_all.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

## 2.4 Bilder fuer Uebersicht----
d1 <- bsp[bsp$`Data set (no. of observations)` == max(bsp$`Data set (no. of observations)`),]

g <- tableGrob(d1)
g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 2, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 3, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 4, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)


#pdf("bars_dataset.pdf", width = 8, height = 3)
grid.draw(g)
dev.off()

d2 <- bsp %>% group_by(Learner) %>% filter(Cindex == max(Cindex))

g <- tableGrob(d2)
g$grobs[find_cell( g, 2, 5, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 3, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 4, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)
g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)


#pdf("bars_cindex.pdf", width = 8, height = 3)
grid.draw(g)
dev.off()

d3 <- bsp %>% group_by(Learner) %>% filter(Ibrier == max(Ibrier))

g <- tableGrob(d3)
g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)
g$grobs[find_cell( g, 4, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)
g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

#pdf("bars_ibrier.pdf", width = 8, height = 3)
grid.draw(g)
dev.off()

d4 <- bsp %>% group_by(Learner) %>% filter(`No. of features` == max(`No. of features`))

g <- tableGrob(d4)
g$grobs[find_cell( g, 2, 7, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 3, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)
g$grobs[find_cell( g, 4, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)
g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

#pdf("bars_spars.pdf", width = 8, height = 3)
grid.draw(g)
dev.off()

bsp1 <- bsp
bsp1$`Data set (name)` <- ""
bsp1$`Data set (no. of observations)` <- ""
d5 <- bsp1 %>% group_by(Learner) %>% mutate(Cindex = round(mean(Cindex),2),
                                            Ibrier = round(mean(Ibrier),2),
                                            `No. of features` = round(mean(`No. of features`),2))
d5 <- d5[c(1,4,7),]

g <- tableGrob(d5)

g$grobs[find_cell( g, 2, 7, "core-bg")][[1]][["gp"]] <- gpar(fill= col_m[9], col = col_m[9], lwd=5)
g$grobs[find_cell( g, 3, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)
g$grobs[find_cell( g, 4, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)

g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill= col_m[7], col = col_m[7], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)
g$grobs[find_cell( g, 4, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)

g$grobs[find_cell( g, 2, 5, "core-bg")][[1]][["gp"]] <- gpar(fill= col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 3, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 4, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

#pdf("bars_dim.pdf", width = 8, height = 3)
grid.draw(g)
dev.off()

## 2.4 mit Approach----
d1 <- bsp1[bsp1$`Data set (no. of observations)` == max(bsp1$`Data set (no. of observations)`),]

g <- tableGrob(d1)
g$grobs[find_cell( g, 2, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 3, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 4, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 2, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 3, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 4, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)


#pdf("bars_dataset_approach.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

d2 <- bsp1[bsp1$`Data set (no. of observations)` == max(bsp1$`Data set (no. of observations)`),]

g <- tableGrob(d2)

g$grobs[find_cell( g, 2, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 3, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 4, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 2, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 3, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 4, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)

g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill= col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 4, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)

d3 <- bsp1[bsp1$`Data set (no. of observations)` == max(bsp1$`Data set (no. of observations)`),]
d3 <- d3 %>% group_by(Approach) %>% filter(Cindex == max(Cindex))

g <- tableGrob(d3)

g$grobs[find_cell( g, 2, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 3, 4, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 2, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 3, 5, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[2], col = col_m[2], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)

g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill= col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)


#pdf("bars_dataset_approach3.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

d4 <- bsp1 %>% group_by(Approach) %>% filter(Cindex == max(Cindex))

g <- tableGrob(d4)
g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)


#pdf("bars_cindex_approach.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

d3 <- bsp1 %>% group_by(Approach) %>% filter(Ibrier == max(Ibrier))

g <- tableGrob(d3)
g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)

#pdf("bars_ibrier_approach.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

d4 <- bsp1 %>% group_by(Approach) %>% filter(`No. of features` == max(`No. of features`))

g <- tableGrob(d4)
g$grobs[find_cell( g, 2, 8, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 3, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[2], col = col_s[2], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[2], col = col_h[2], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)

#pdf("bars_spars_approach.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

bsp2 <- bsp1
bsp2$`Data set (name)` <- ""
bsp2$`Data set (no. of observations)` <- ""
bsp2$Learner <- ""

d5 <- bsp2 %>% group_by(Approach) %>% mutate(Cindex = round(mean(Cindex),2),
                                            Ibrier = round(mean(Ibrier),2),
                                            `No. of features` = round(mean(`No. of features`),2)) %>% filter(1:n() == 1)

g <- tableGrob(d5)

g$grobs[find_cell( g, 2, 8, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 3, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)

g$grobs[find_cell( g, 2, 7, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 3, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)

g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_m[3], col = col_m[3], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[3], col = col_h[3], lwd=5)


#pdf("bars_dim_approach.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

rm(cindex, d1, d2, d3, d4, d5, g, ibrier, spars, bsp1)

## 2.5 mit Dataset----
d1 <- bsp2 %>% group_by(`Data set (no. of observations)`) %>% filter(Cindex == max(Cindex))

g <- tableGrob(d1)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 4, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)


#pdf("bars_dataset_cindex1.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

d2 <- bsp2 %>% group_by(`Data set (no. of observations)`) %>% filter(Ibrier == max(Ibrier)) %>% filter(1:n() == 1)

g <- tableGrob(d2)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 7, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 3, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)
g$grobs[find_cell( g, 4, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)


#pdf("bars_dataset_ibrier1.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

d3 <- bsp2 %>% group_by(`Data set (no. of observations)`) %>% filter(`No. of features` == max(`No. of features`)) %>% filter(1:n() == 1)

g <- tableGrob(d3)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 8, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 3, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)
g$grobs[find_cell( g, 4, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)


#pdf("bras_dataset_spars1.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

bsp3 <- bsp2
bsp3$Learner <- ""
bsp3$Approach <- ""

d4 <- bsp3 %>% group_by(`Data set (no. of observations)`) %>% mutate(Cindex = round(mean(Cindex),2),
                                             Ibrier = round(mean(Ibrier),2),
                                             `No. of features` = round(mean(`No. of features`),2)) %>% filter(1:n() == 1)

g <- tableGrob(d4)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[4], col = col_s[4], lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[4], col = col_m[4], lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[4], col = col_h[4], lwd=5)

g$grobs[find_cell( g, 2, 6, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[5], col = col_s[5], lwd=5)
g$grobs[find_cell( g, 3, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[5], col = col_m[5], lwd=5)
g$grobs[find_cell( g, 4, 6, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[5], col = col_h[5], lwd=5)

g$grobs[find_cell( g, 2, 7, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[7], col = col_s[7], lwd=5)
g$grobs[find_cell( g, 3, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[7], col = col_m[7], lwd=5)
g$grobs[find_cell( g, 4, 7, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[7], col = col_h[7], lwd=5)

g$grobs[find_cell( g, 2, 8, "core-bg")][[1]][["gp"]] <- gpar(fill= col_s[9], col = col_s[9], lwd=5)
g$grobs[find_cell( g, 3, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_m[9], col = col_m[9], lwd=5)
g$grobs[find_cell( g, 4, 8, "core-bg")][[1]][["gp"]] <- gpar(fill=col_h[9], col = col_h[9], lwd=5)

#pdf("bars_dataset_alle1.pdf", width = 9, height = 3)
grid.draw(g)
dev.off()

# 1. Comparison----
## 1.1. Bars----
### 1.1.1 Dataset----
#### 1.1.1.1 Minimum----
df_min <- df[df$n == min(df$n),]
df_min$learner.id <- factor(df_min$learner.id, levels = unique(df_min$learner.id[order(df_min$approach, df_min$cv_mean_cindex)]) )
df_min$approach <- factor(df_min$approach, levels = c("Reference",
                                                            "Random forest",
                                                            "Penalized regression",
                                                            "Boosting"))
df_min$cv_mean_spars[is.na(df_min$cv_mean_spars)] <- 0

p1 <- ggplot(data = df_min,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_min$cv_mean_cindex[df_min$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_min$cv_mean_cindex[df_min$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_min,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_min$cv_mean_ibrier[df_min$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_min$cv_mean_ibrier[df_min$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_min ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "dataset_min.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the smallest dataset",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_min)

#### 1.1.1.2 Maximum----
df_max <- df[df$n == max(df$n),]
df_max$learner.id <- factor(df_max$learner.id, levels = unique(df_max$learner.id[order(df_max$approach, df_max$cv_mean_cindex)]) )
df_max$approach <- factor(df_max$approach, levels = c("Reference",
                                                      "Random forest",
                                                      "Penalized regression",
                                                      "Boosting"))
df_max$cv_mean_spars[is.na(df_max$cv_mean_spars)] <- 0

p1 <- ggplot(data = df_max,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_max$cv_mean_cindex[df_max$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_max$cv_mean_cindex[df_max$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_max,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_max$cv_mean_ibrier[df_max$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_max$cv_mean_ibrier[df_max$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_max ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "dataset_max.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the biggest dataset",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_max)

#### 1.1.1.2 Median----
df_med <- df[df$n == mymedian(df$n),]
df_med <- df[df$task.id == "KIRC",]
df_med$learner.id <- factor(df_med$learner.id, levels = unique(df_med$learner.id[order(df_med$approach, df_med$cv_mean_cindex)]) )
df_med$approach <- factor(df_med$approach, levels = c("Reference",
                                                      "Random forest",
                                                      "Penalized regression",
                                                      "Boosting"))
df_med$cv_mean_spars[is.na(df_med$cv_mean_spars)] <- 0

p1 <- ggplot(data = df_med,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_med$cv_mean_cindex[df_med$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_med$cv_mean_cindex[df_med$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_med,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_med$cv_mean_ibrier[df_med$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_med$cv_mean_ibrier[df_med$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_med ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "dataset_median.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the median dataset",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_med)

### 1.1.2 cindex----
#### 1.1.2.1 Minimum----
df_min <- df %>% group_by(learner.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1)
df_min$learner.id <- factor(df_min$learner.id, levels = unique(df_min$learner.id[order(df_min$approach, df_min$cv_mean_cindex)]) )
df_min$approach <- factor(df_min$approach, levels = c("Reference",
                                                      "Random forest",
                                                      "Penalized regression",
                                                      "Boosting"))
df_min$cv_mean_spars[is.na(df_min$cv_mean_spars)] <- 0

p1 <- ggplot(data = df_min,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_min$cv_mean_cindex[df_min$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_min$cv_mean_cindex[df_min$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_min,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_min$cv_mean_ibrier[df_min$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_min$cv_mean_ibrier[df_min$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_min ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "cindex_min.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the smallest cindex",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_min)

#### 1.1.2.2 Maximum----
df_max <- df %>% group_by(learner.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1)
df_max$learner.id <- factor(df_max$learner.id, levels = unique(df_max$learner.id[order(df_max$approach, df_max$cv_mean_cindex)]) )
df_max$approach <- factor(df_max$approach, levels = c("Reference",
                                                      "Random forest",
                                                      "Penalized regression",
                                                      "Boosting"))
df_max$cv_mean_spars[is.na(df_max$cv_mean_spars)] <- 0

p1 <- ggplot(data = df_max,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_max$cv_mean_cindex[df_max$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_max$cv_mean_cindex[df_max$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_max,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_max$cv_mean_ibrier[df_max$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_max$cv_mean_ibrier[df_max$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_max ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "cindex_max.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the largest cindex",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_max)

#### 1.1.2.2 Median----
df_med <- df[1,]

# Median: Bei geraden Zahlen wird der kleinere Wert verwendet 
for(i in 1:length(unique(df$learner.id))){
  m <- mymedian(df$cv_mean_cindex[df$learner.id == unique(df$learner.id)[i]])
  df_med[i,] <- (df[df$learner.id == unique(df$learner.id)[i] & df$cv_mean_cindex == m,])[1,]
}
df_med$learner.id <- factor(df_med$learner.id, levels = unique(df_med$learner.id[order(df_med$approach, df_med$cv_mean_cindex)]))
df_med$approach <- factor(df_med$approach, levels = c("Reference",
                                                      "Random forest",
                                                      "Penalized regression",
                                                      "Boosting"))
df_med$cv_mean_spars[is.na(df_med$cv_mean_spars)] <- 0

p1 <- ggplot(data = df_med,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_med$cv_mean_cindex[df_med$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_med$cv_mean_cindex[df_med$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_med,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_med$cv_mean_ibrier[df_med$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_med$cv_mean_ibrier[df_med$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_med ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "cindex_median.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the median cindex",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_med)

### 1.1.3 ibrier----
#### 1.1.3.1 Minimum----
df_min <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1)
df_min$learner.id <- factor(df_min$learner.id, levels = unique(df_min$learner.id[order(df_min$approach, df_min$cv_mean_cindex)]) )
df_min$approach <- factor(df_min$approach, levels = c("Reference",
                                                      "Random forest",
                                                      "Penalized regression",
                                                      "Boosting"))
df_min$cv_mean_spars[is.na(df_min$cv_mean_spars)] <- 0

p1 <- ggplot(data = df_min,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_min$cv_mean_cindex[df_min$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_min$cv_mean_cindex[df_min$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_min,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_min$cv_mean_ibrier[df_min$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_min$cv_mean_ibrier[df_min$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_min ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "ibrier_min.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the smallest ibrier",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_min)

#### 1.1.3.2 Maximum----
df_max <- df %>% group_by(learner.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1)
df_max$learner.id <- factor(df_max$learner.id, levels = unique(df_max$learner.id[order(df_max$approach, df_max$cv_mean_cindex)]) )
df_max$approach <- factor(df_max$approach, levels = c("Reference",
                                                      "Random forest",
                                                      "Penalized regression",
                                                      "Boosting"))
df_max$cv_mean_spars[is.na(df_max$cv_mean_spars)] <- 0

p1 <- ggplot(data = df_max,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_max$cv_mean_cindex[df_max$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_max$cv_mean_cindex[df_max$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_max,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_max$cv_mean_ibrier[df_max$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_max$cv_mean_ibrier[df_max$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_max ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "ibrier_max.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the largest ibrier",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_max)

#### 1.1.3.2 Median----
df_med <- df[1,]

# Median: Bei geraden Zahlen wird der kleinere Wert verwendet 
for(i in 1:length(unique(df$learner.id))){
  m <- mymedian(df$cv_mean_ibrier[df$learner.id == unique(df$learner.id)[i]])
  df_med[i,] <- (df[df$learner.id == unique(df$learner.id)[i] & df$cv_mean_ibrier == m,])[1,]
}
df_med$learner.id <- factor(df_med$learner.id, levels = unique(df_med$learner.id[order(df_med$approach, df_med$cv_mean_cindex)]))
df_med$approach <- factor(df_med$approach, levels = c("Reference",
                                                      "Random forest",
                                                      "Penalized regression",
                                                      "Boosting"))
df_med$cv_mean_spars[is.na(df_med$cv_mean_spars)] <- 0

p1 <- ggplot(data = df_med,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_med$cv_mean_cindex[df_med$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_med$cv_mean_cindex[df_med$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_med,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_med$cv_mean_ibrier[df_med$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_med$cv_mean_ibrier[df_med$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_med ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "ibrier_median.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the median ibrier",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_med)

### 1.1.4 spars----
#### 1.1.4.1 Minimum----
df1 <- df
df1$cv_mean_spars[is.na(df1$cv_mean_spars)] <- 0
df_min <- df1 %>% group_by(learner.id) %>% filter(cv_mean_spars == min(cv_mean_spars)) %>% filter(1:n() == 1)
df_min$learner.id <- factor(df_min$learner.id, levels = unique(df_min$learner.id[order(df_min$approach, df_min$cv_mean_cindex)]) )
df_min$approach <- factor(df_min$approach, levels = c("Reference",
                                                      "Random forest",
                                                      "Penalized regression",
                                                      "Boosting"))
p1 <- ggplot(data = df_min,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_min$cv_mean_cindex[df_min$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_min$cv_mean_cindex[df_min$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_min,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_min$cv_mean_ibrier[df_min$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_min$cv_mean_ibrier[df_min$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_min ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "spars_min.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the smallest no. of features",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_min)

#### 1.1.4.2 Maximum----
df_max <- df1 %>% group_by(learner.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1)
df_max$learner.id <- factor(df_max$learner.id, levels = unique(df_max$learner.id[order(df_max$approach, df_max$cv_mean_cindex)]) )
df_max$approach <- factor(df_max$approach, levels = c("Reference",
                                                      "Random forest",
                                                      "Penalized regression",
                                                      "Boosting"))

p1 <- ggplot(data = df_max,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_max$cv_mean_cindex[df_max$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_max$cv_mean_cindex[df_max$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_max,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_max$cv_mean_ibrier[df_max$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_max$cv_mean_ibrier[df_max$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_max ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "spars_max.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the largest no. of features",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_max)

#### 1.1.4.2 Median----
df_med <- df[1,]

for(i in 1:length(unique(df$learner.id))){
  m <- mymedian(df1$cv_mean_spars[df1$learner.id == unique(df1$learner.id)[i]])
  df_med[i,] <- (df1[df1$learner.id == unique(df1$learner.id)[i] & df1$cv_mean_spars == m,])[1,]
}

df_med$learner.id <- factor(df_med$learner.id, levels = unique(df_med$learner.id[order(df_med$approach, df_med$cv_mean_cindex)]))
df_med$approach <- factor(df_med$approach, levels = c("Reference",
                                                      "Random forest",
                                                      "Penalized regression",
                                                      "Boosting"))
df_med$cv_mean_spars[is.na(df_med$cv_mean_spars)] <- 0

p1 <- ggplot(data = df_med,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_med$cv_mean_cindex[df_med$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_med$cv_mean_cindex[df_med$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_med,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_med$cv_mean_ibrier[df_med$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_med$cv_mean_ibrier[df_med$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_med ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "spars_median.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the median no. of features",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_med)

### 1.1.5 all dim----
#### 1.1.5.1 Minimum----
df_min_cindex <- df %>% group_by(learner.id, approach) %>% summarise(cv_mean_cindex = min(cv_mean_cindex))
df_min_cindex$learner.id <- factor(df_min_cindex$learner.id, levels = unique(df_min_cindex$learner.id[order(df_min_cindex$approach, df_min_cindex$cv_mean_cindex)]) )
df_min_cindex$approach <- factor(df_min_cindex$approach, levels = c("Reference",
                                                      "Random forest",
                                                      "Penalized regression",
                                                      "Boosting"))
df_min_ibrier <- df %>% group_by(learner.id, approach) %>% summarise(cv_mean_ibrier = min(cv_mean_ibrier))
df_min_ibrier$learner.id <- factor(df_min_ibrier$learner.id, levels = unique(df_min_cindex$learner.id[order(df_min_cindex$approach, df_min_cindex$cv_mean_cindex)]) )
df_min_ibrier$approach <- factor(df_min_ibrier$approach, levels = c("Reference",
                                                                    "Random forest",
                                                                    "Penalized regression",
                                                                    "Boosting"))
df_min_spars <- df %>% group_by(learner.id, approach) %>% summarise(cv_mean_spars = min(cv_mean_spars))
df_min_spars$learner.id <- factor(df_min_spars$learner.id, levels = unique(df_min_cindex$learner.id[order(df_min_cindex$approach, df_min_cindex$cv_mean_cindex)]) )
df_min_spars$approach <- factor(df_min_spars$approach, levels = c("Reference",
                                                                    "Random forest",
                                                                    "Penalized regression",
                                                                    "Boosting"))
df_min_spars$cv_mean_spars[is.na(df_min_spars$cv_mean_spars)] <- 0


p1 <- ggplot(data = df_min_cindex,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_min_cindex$cv_mean_cindex[df_min_cindex$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_min_cindex$cv_mean_cindex[df_min_cindex$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_min_ibrier,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_min_ibrier$cv_mean_ibrier[df_min_ibrier$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_min_ibrier$cv_mean_ibrier[df_min_ibrier$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_min_spars ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "alle_min.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the minimum cindex, minimum ibrier and minimum no. of features",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_min_cindex,df_min_ibrier, df_min_spars)

#### 1.1.5.2 Maximum----
df_max_cindex <- df %>% group_by(learner.id, approach) %>% summarise(cv_mean_cindex = max(cv_mean_cindex))
df_max_cindex$learner.id <- factor(df_max_cindex$learner.id, levels = unique(df_max_cindex$learner.id[order(df_max_cindex$approach, df_max_cindex$cv_mean_cindex)]) )
df_max_cindex$approach <- factor(df_max_cindex$approach, levels = c("Reference",
                                                                    "Random forest",
                                                                    "Penalized regression",
                                                                    "Boosting"))
df_max_ibrier <- df %>% group_by(learner.id, approach) %>% summarise(cv_mean_ibrier = max(cv_mean_ibrier))
df_max_ibrier$learner.id <- factor(df_max_ibrier$learner.id, levels = unique(df_max_cindex$learner.id[order(df_max_cindex$approach, df_max_cindex$cv_mean_cindex)]) )
df_max_ibrier$approach <- factor(df_max_ibrier$approach, levels = c("Reference",
                                                                    "Random forest",
                                                                    "Penalized regression",
                                                                    "Boosting"))
df_max_spars <- df %>% group_by(learner.id, approach) %>% summarise(cv_mean_spars = max(cv_mean_spars))
df_max_spars$learner.id <- factor(df_max_spars$learner.id, levels = unique(df_max_cindex$learner.id[order(df_max_cindex$approach, df_max_cindex$cv_mean_cindex)]) )
df_max_spars$approach <- factor(df_max_spars$approach, levels = c("Reference",
                                                                  "Random forest",
                                                                  "Penalized regression",
                                                                  "Boosting"))
df_max_spars$cv_mean_spars[is.na(df_max_spars$cv_mean_spars)] <- 0


p1 <- ggplot(data = df_max_cindex,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_max_cindex$cv_mean_cindex[df_max_cindex$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_max_cindex$cv_mean_cindex[df_max_cindex$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_max_ibrier,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_max_ibrier$cv_mean_ibrier[df_max_ibrier$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_max_ibrier$cv_mean_ibrier[df_max_ibrier$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_max_spars ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "alle_max.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the maximum cindex, maximum ibrier and maximum no. of features",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_max_cindex,df_max_ibrier, df_max_spars)

#### 1.1.5.3 Median----
df_med_cindex <- df %>% group_by(learner.id, approach) %>% summarise(cv_mean_cindex = mymedian(cv_mean_cindex))
df_med_cindex$learner.id <- factor(df_med_cindex$learner.id, levels = unique(df_med_cindex$learner.id[order(df_med_cindex$approach, df_med_cindex$cv_mean_cindex)]) )
df_med_cindex$approach <- factor(df_med_cindex$approach, levels = c("Reference",
                                                                    "Random forest",
                                                                    "Penalized regression",
                                                                    "Boosting"))
df_med_ibrier <- df %>% group_by(learner.id, approach) %>% summarise(cv_mean_ibrier = mymedian(cv_mean_ibrier))
df_med_ibrier$learner.id <- factor(df_med_ibrier$learner.id, levels = unique(df_med_cindex$learner.id[order(df_med_cindex$approach, df_med_cindex$cv_mean_cindex)]) )
df_med_ibrier$approach <- factor(df_med_ibrier$approach, levels = c("Reference",
                                                                    "Random forest",
                                                                    "Penalized regression",
                                                                    "Boosting"))
df_med_spars <- df %>% group_by(learner.id, approach) %>% summarise(cv_mean_spars = mymedian(cv_mean_spars))
df_med_spars$learner.id <- factor(df_med_spars$learner.id, levels = unique(df_med_cindex$learner.id[order(df_med_cindex$approach, df_med_cindex$cv_mean_cindex)]) )
df_med_spars$approach <- factor(df_med_spars$approach, levels = c("Reference",
                                                                  "Random forest",
                                                                  "Penalized regression",
                                                                  "Boosting"))
df_med_spars$cv_mean_spars[is.na(df_med_spars$cv_mean_spars)] <- 0


p1 <- ggplot(data = df_med_cindex,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_med_cindex$cv_mean_cindex[df_med_cindex$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_med_cindex$cv_mean_cindex[df_med_cindex$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_med_ibrier,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_med_ibrier$cv_mean_ibrier[df_med_ibrier$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_med_ibrier$cv_mean_ibrier[df_med_ibrier$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_med_spars ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "alle_median.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the median cindex, median ibrier and median no. of features",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_med_cindex,df_med_ibrier, df_med_spars)

#### 1.1.5.4 Mean----
df_mean_cindex <- df %>% group_by(learner.id, approach) %>% summarise(cv_mean_cindex = mean(cv_mean_cindex))
df_mean_cindex$learner.id <- factor(df_mean_cindex$learner.id, levels = unique(df_mean_cindex$learner.id[order(df_mean_cindex$approach, df_mean_cindex$cv_mean_cindex)]) )
df_mean_cindex$approach <- factor(df_mean_cindex$approach, levels = c("Reference",
                                                                    "Random forest",
                                                                    "Penalized regression",
                                                                    "Boosting"))
df_mean_ibrier <- df %>% group_by(learner.id, approach) %>% summarise(cv_mean_ibrier = mean(cv_mean_ibrier))
df_mean_ibrier$learner.id <- factor(df_mean_ibrier$learner.id, levels = unique(df_mean_cindex$learner.id[order(df_mean_cindex$approach, df_mean_cindex$cv_mean_cindex)]) )
df_mean_ibrier$approach <- factor(df_mean_ibrier$approach, levels = c("Reference",
                                                                    "Random forest",
                                                                    "Penalized regression",
                                                                    "Boosting"))
df_mean_spars <- df %>% group_by(learner.id, approach) %>% summarise(cv_mean_spars = mean(cv_mean_spars))
df_mean_spars$learner.id <- factor(df_mean_spars$learner.id, levels = unique(df_mean_cindex$learner.id[order(df_mean_cindex$approach, df_mean_cindex$cv_mean_cindex)]) )
df_mean_spars$approach <- factor(df_mean_spars$approach, levels = c("Reference",
                                                                  "Random forest",
                                                                  "Penalized regression",
                                                                  "Boosting"))
df_mean_spars$cv_mean_spars[is.na(df_mean_spars$cv_mean_spars)] <- 0

p1 <- ggplot(data = df_mean_cindex,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_mean_cindex$cv_mean_cindex[df_mean_cindex$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_mean_cindex$cv_mean_cindex[df_mean_cindex$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)


p2 <- ggplot(data = df_mean_ibrier,
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
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", scales="free_x", switch="x")+
  geom_hline(yintercept = df_mean_ibrier$cv_mean_ibrier[df_mean_ibrier$learner.id == "Kaplan\nMeier"], size = linesize, color = gray3[1])+
  geom_hline(yintercept = df_mean_ibrier$cv_mean_ibrier[df_mean_ibrier$learner.id == "Cox\nproportional\nhazard"], size = linesize, color = gray3[2])+
  scale_fill_manual(values = scale_fill)

p3 <- ggplot(data = df_mean_spars ,
             aes(y = cv_mean_spars, x = learner.id, fill = learner.id))+
  geom_col()+
  theme_tufte(base_family = "sans")+
  labs(x = "Learner", y = "No. of features")+
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = axissize, color = axis_col),
        axis.ticks.y = element_line(size = axissize, color = axis_col),
        strip.placement = "outside",
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~ approach, space="free_x", switch="x", scales="free_x")+
  scale_fill_manual(values = scale_fill)

#pdf(file = "alle_mean.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
plot_grid(ggdraw() + draw_label("Performance measures\nlearners summarised by the mean cindex, mean ibrier and mean no. of features",
                                size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
          rel_heights = c(.1,1), ncol = 1)
dev.off()

rm(p1,p2,p3,df_mean_cindex,df_mean_ibrier, df_mean_spars)

### 1.1.6 according to Approach----
approach_fig <- function(dataset1, dataset2, dataset3, label){
  p1 <- ggplot(data = dataset1,
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
    scale_y_continuous(expand = c(0,0))+
    geom_hline(yintercept = dataset1$cv_mean_cindex[dataset1$approach == "Reference"], size = linesize, color = gray3[1])+
    scale_fill_manual(values = scale_fill_ap)
  
  
  p2 <- ggplot(data = dataset2,
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
    scale_y_continuous(expand = c(0,0))+
    geom_hline(yintercept = dataset2$cv_mean_ibrier[dataset1$approach == "Reference"], size = linesize, color = gray3[1])+
    scale_fill_manual(values = scale_fill_ap)
  
  p3 <- ggplot(data = dataset3 ,
               aes(y = cv_mean_spars, x = approach, fill = approach))+
    geom_col()+
    theme_tufte(base_family = "sans")+
    labs(x = "Approach", y = "No. of features")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          strip.placement = "outside",
          legend.position = "none")+
    scale_y_continuous(expand = c(0,0))+
    scale_fill_manual(values = scale_fill_ap)
  
  plot_grid(ggdraw() + draw_label(label,
                                  size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
            rel_heights = c(.1,1), ncol = 1)
}

#### 1.1.6.1 max dataset----
##### 1.1.6.1.1 cindex----
df_max <- df[df$n == max(df$n),]
df_max_cmax <- df_max %>% group_by(approach) %>% summarise(cv_mean_cindex = max(cv_mean_cindex))
df_max_cmax$approach <- factor(df_max_cmax$approach,
                               levels = c("Reference", unique(df_max_cmax$approach[df_max_cmax$approach != "Reference"]
                                                              [order(df_max_cmax$cv_mean_cindex[df_max_cmax$approach != "Reference"])])))
df_max_imax <- df_max %>% group_by(approach) %>% summarise(cv_mean_ibrier = max(cv_mean_ibrier))
df_max_imax$approach <- factor(df_max_imax$approach,
                               levels = c("Reference", unique(df_max_imax$approach[df_max_imax$approach != "Reference"]
                                                              [order(df_max_cmax$cv_mean_cindex[df_max_imax$approach != "Reference"])])))
df_max$cv_mean_spars[is.na(df_max$cv_mean_spars)] <- 0
df_max_smax <- df_max %>% group_by(approach) %>% summarise(cv_mean_spars = max(cv_mean_spars))
df_max_smax$approach <- factor(df_max_smax$approach,
                               levels = c("Reference", unique(df_max_smax$approach[df_max_smax$approach != "Reference"]
                                                              [order(df_max_cmax$cv_mean_cindex[df_max_imax$approach != "Reference"])])))


#pdf(file = "dataset_approach_mmaxmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_max_cmax, df_max_imax, df_max_smax, "Performance measures\napproach summarised by maximum dataset, maximum cindex, maximum ibrier and maximum no. of features")
dev.off()


# cmin
df_max <- df[df$n == max(df$n),]
df_max_cmin <- df_max %>% group_by(approach) %>% filter(cv_mean_cindex == min(cv_mean_cindex))
df_max_cmin$approach <- factor(df_max_cmin$approach,
                               levels = c("Reference", unique(df_max_cmin$approach[df_max_cmin$approach != "Reference"]
                                                              [order(df_max_cmin$cv_mean_cindex[df_max_cmin$approach != "Reference"])])))
df_max_cmin$cv_mean_spars[is.na(df_max_cmin$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_maxmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_max_cmin, df_max_cmin, df_max_cmin, "Performance measures\napproach summarised by maximum dataset and minimum cindex")
dev.off()

# cmed
df_max <- df[df$n == max(df$n),]
df_max_cmed <- df_max %>% group_by(approach) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex))
df_max_cmed$approach <- factor(df_max_cmed$approach,
                               levels = c("Reference", unique(df_max_cmed$approach[df_max_cmed$approach != "Reference"]
                                                              [order(df_max_cmed$cv_mean_cindex[df_max_cmed$approach != "Reference"])])))
df_max_cmed$cv_mean_spars[is.na(df_max_cmed$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_maxmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_max_cmed, df_max_cmed, df_max_cmed, "Performance measures\napproach summarised by maximum dataset and median cindex")
dev.off()


##### 1.1.6.1.2 ibrier----
# imax
df_max <- df[df$n == max(df$n),]
df_max_imax <- df_max %>% group_by(approach) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier))
df_max_imax$approach <- factor(df_max_imax$approach,
                               levels = c("Reference", unique(df_max_imax$approach[df_max_imax$approach != "Reference"]
                                                              [order(df_max_imax$cv_mean_cindex[df_max_imax$approach != "Reference"])])))
df_max_imax$cv_mean_spars[is.na(df_max_imax$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_imaxmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_max_imax, df_max_imax, df_max_imax, "Performance measures\napproach summarised by maximum dataset and maximum ibrier")
dev.off()

# imin
df_max <- df[df$n == max(df$n),]
df_max_imin <- df_max %>% group_by(approach) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier))
df_max_imin$approach <- factor(df_max_imin$approach,
                               levels = c("Reference", unique(df_max_imin$approach[df_max_imin$approach != "Reference"]
                                                              [order(df_max_imin$cv_mean_cindex[df_max_imin$approach != "Reference"])])))
df_max_imin$cv_mean_spars[is.na(df_max_imin$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_imaxmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_max_imin, df_max_imin, df_max_imin, "Performance measures\napproach summarised by maximum dataset and minimum ibrier")
dev.off()

# imed
df_max <- df[df$n == max(df$n),]
df_max_imed <- df_max %>% group_by(approach) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier))
df_max_imed$approach <- factor(df_max_imed$approach,
                               levels = c("Reference", unique(df_max_imed$approach[df_max_imed$approach != "Reference"]
                                                              [order(df_max_imed$cv_mean_cindex[df_max_imed$approach != "Reference"])])))
df_max_imed$cv_mean_spars[is.na(df_max_imed$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_imaxmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_max_imed, df_max_imed, df_max_imed, "Performance measures\napproach summarised by maximum dataset and median ibrier")
dev.off()

##### 1.1.6.1.3 all dim----
# amax
df_max <- df[df$n == max(df$n),]
df_max_cmax <- df_max %>% group_by(approach) %>% summarise(cv_mean_cindex = max(cv_mean_cindex))
df_max_cmax$approach <- factor(df_max_cmax$approach,
                               levels = c("Reference", unique(df_max_cmax$approach[df_max_cmax$approach != "Reference"]
                                                              [order(df_max_cmax$cv_mean_cindex[df_max_cmax$approach != "Reference"])])))
df_max_imax <- df_max %>% group_by(approach) %>% summarise(cv_mean_ibrier = max(cv_mean_ibrier))
df_max_imax$approach <- factor(df_max_imax$approach,
                               levels = c("Reference", unique(df_max_imax$approach[df_max_imax$approach != "Reference"]
                                                              [order(df_max_cmax$cv_mean_cindex[df_max_imax$approach != "Reference"])])))
df_max$cv_mean_spars[is.na(df_max$cv_mean_spars)] <- 0
df_max_smax <- df_max %>% group_by(approach) %>% summarise(cv_mean_spars = max(cv_mean_spars))
df_max_smax$approach <- factor(df_max_smax$approach,
                               levels = c("Reference", unique(df_max_smax$approach[df_max_smax$approach != "Reference"]
                                                              [order(df_max_cmax$cv_mean_cindex[df_max_imax$approach != "Reference"])])))


#pdf(file = "dataset_approach_mmaxmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_max_cmax, df_max_imax, df_max_smax, "Performance measures\napproach summarised by maximum dataset, maximum cindex, maximum ibrier and maximum no. of features")
dev.off()

# amin
df_max <- df[df$n == max(df$n),]
df_max_cmin <- df_max %>% group_by(approach) %>% summarise(cv_mean_cindex = min(cv_mean_cindex))
df_max_cmin$approach <- factor(df_max_cmin$approach,
                               levels = c("Reference", unique(df_max_cmin$approach[df_max_cmin$approach != "Reference"]
                                                              [order(df_max_cmin$cv_mean_cindex[df_max_cmin$approach != "Reference"])])))
df_max_imin <- df_max %>% group_by(approach) %>% summarise(cv_mean_ibrier = min(cv_mean_ibrier))
df_max_imin$approach <- factor(df_max_imin$approach,
                               levels = c("Reference", as.character(unique(df_max_cmin$approach[df_max_cmin$approach != "Reference"][order(df_max_cmin$cv_mean_cindex[df_max_cmin$approach != "Reference"])]))))
df_max$cv_mean_spars[is.na(df_max$cv_mean_spars)] <- 0
df_max_smin <- df_max %>% group_by(approach) %>% summarise(cv_mean_spars = min(cv_mean_spars))
df_max_smin$approach <- factor(df_max_smin$approach,
                               levels = c("Reference", as.character(unique(df_max_cmin$approach[df_max_cmin$approach != "Reference"][order(df_max_cmin$cv_mean_cindex[df_max_cmin$approach != "Reference"])]))))


#pdf(file = "dataset_approach_mmaxmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_max_cmin, df_max_imin, df_max_smin, "Performance measures\napproach summarised by maximum dataset, minimum cindex, minimum ibrier and minimum no. of features")
dev.off()

# mean
df_max <- df[df$n == max(df$n),]
df_max_cmean <- df_max %>% group_by(approach) %>% summarise(cv_mean_cindex = mean(cv_mean_cindex))
df_max_cmean$approach <- factor(df_max_cmean$approach,
                               levels = c("Reference", unique(df_max_cmean$approach[df_max_cmean$approach != "Reference"]
                                                              [order(df_max_cmean$cv_mean_cindex[df_max_cmean$approach != "Reference"])])))
df_max_imean <- df_max %>% group_by(approach) %>% summarise(cv_mean_ibrier = mean(cv_mean_ibrier))
df_max_imean$approach <- factor(df_max_imean$approach,
                               levels = c("Reference", as.character(unique(df_max_cmean$approach[df_max_cmean$approach != "Reference"][order(df_max_cmean$cv_mean_cindex[df_max_cmin$approach != "Reference"])]))))
df_max$cv_mean_spars[is.na(df_max$cv_mean_spars)] <- 0
df_max_smean <- df_max %>% group_by(approach) %>% summarise(cv_mean_spars = mean(cv_mean_spars))
df_max_smean$approach <- factor(df_max_smean$approach,
                               levels = c("Reference", as.character(unique(df_max_cmean$approach[df_max_cmean$approach != "Reference"][order(df_max_cmean$cv_mean_cindex[df_max_cmean$approach != "Reference"])]))))


#pdf(file = "dataset_approach_mmaxmean.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_max_cmean, df_max_imean, df_max_smean, "Performance measures\napproach summarised by maximum dataset, mean cindex, mean ibrier and mean no. of features")
dev.off()

# med
df_max <- df[df$n == max(df$n),]
df_max_cmed <- df_max %>% group_by(approach) %>% summarise(cv_mean_cindex = mymedian(cv_mean_cindex))
df_max_cmed$approach <- factor(df_max_cmed$approach,
                                levels = c("Reference", unique(df_max_cmed$approach[df_max_cmed$approach != "Reference"]
                                                               [order(df_max_cmed$cv_mean_cindex[df_max_cmed$approach != "Reference"])])))
df_max_imed <- df_max %>% group_by(approach) %>% summarise(cv_mean_ibrier = mymedian(cv_mean_ibrier))
df_max_imed$approach <- factor(df_max_imed$approach,
                                levels = c("Reference", as.character(unique(df_max_cmed$approach[df_max_cmed$approach != "Reference"][order(df_max_cmed$cv_mean_cindex[df_max_cmin$approach != "Reference"])]))))
df_max$cv_mean_spars[is.na(df_max$cv_mean_spars)] <- 0
df_max_smed <- df_max %>% group_by(approach) %>% summarise(cv_mean_spars = mymedian(cv_mean_spars))
df_max_smed$approach <- factor(df_max_smed$approach,
                                levels = c("Reference", as.character(unique(df_max_cmed$approach[df_max_cmed$approach != "Reference"][order(df_max_cmed$cv_mean_cindex[df_max_cmean$approach != "Reference"])]))))


#pdf(file = "dataset_approach_mmaxmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_max_cmed, df_max_imed, df_max_smed, "Performance measures\napproach summarised by maximum dataset, median cindex, median ibrier and median no. of features")
dev.off()

#### 1.1.6.2 min data set size----
##### 1.1.6.2.1 cindex----
df_min <- df[df$n == min(df$n),]
df_min_cmax <- df_min %>% group_by(approach) %>% summarise(cv_mean_cindex = max(cv_mean_cindex))
df_min_cmax$approach <- factor(df_min_cmax$approach,
                               levels = c("Reference", unique(df_min_cmax$approach[df_min_cmax$approach != "Reference"]
                                                              [order(df_min_cmax$cv_mean_cindex[df_min_cmax$approach != "Reference"])])))
df_min_imax <- df_min %>% group_by(approach) %>% summarise(cv_mean_ibrier = max(cv_mean_ibrier))
df_min_imax$approach <- factor(df_min_imax$approach,
                               levels = c("Reference", unique(df_min_imax$approach[df_min_imax$approach != "Reference"]
                                                              [order(df_min_cmax$cv_mean_cindex[df_min_imax$approach != "Reference"])])))
df_min$cv_mean_spars[is.na(df_min$cv_mean_spars)] <- 0
df_min_smax <- df_min %>% group_by(approach) %>% summarise(cv_mean_spars = max(cv_mean_spars))
df_min_smax$approach <- factor(df_min_smax$approach,
                               levels = c("Reference", unique(df_min_smax$approach[df_min_smax$approach != "Reference"]
                                                              [order(df_min_cmax$cv_mean_cindex[df_min_imax$approach != "Reference"])])))


#pdf(file = "dataset_approach_cminmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_min_cmax, df_min_imax, df_min_smax, "Performance measures\napproach summarised by minimum dataset, maximum cindex, maximum ibrier and maximum no. of features")
dev.off()


# cmin
df_min <- df[df$n == min(df$n),]
df_min_cmin <- df_min %>% group_by(approach) %>% filter(cv_mean_cindex == min(cv_mean_cindex))
df_min_cmin$approach <- factor(df_min_cmin$approach,
                               levels = c("Reference", unique(df_min_cmin$approach[df_min_cmin$approach != "Reference"]
                                                              [order(df_min_cmin$cv_mean_cindex[df_min_cmin$approach != "Reference"])])))
df_min_cmin$cv_mean_spars[is.na(df_min_cmin$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_cminmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_min_cmin, df_min_cmin, df_min_cmin, "Performance measures\napproach summarised by minimum dataset and minimum cindex")
dev.off()

# cmed
df_min <- df[df$n == min(df$n),]
df_min_cmed <- df_min %>% group_by(approach) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex))
df_min_cmed$approach <- factor(df_min_cmed$approach,
                               levels = c("Reference", unique(df_min_cmed$approach[df_min_cmed$approach != "Reference"]
                                                              [order(df_min_cmed$cv_mean_cindex[df_min_cmed$approach != "Reference"])])))
df_min_cmed$cv_mean_spars[is.na(df_min_cmed$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_minmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_min_cmed, df_min_cmed, df_min_cmed, "Performance measures\napproach summarised by minimum dataset and median cindex")
dev.off()


##### 1.1.6.2.2 ibrier----
# imax
df_min <- df[df$n == min(df$n),]
df_min_imax <- df_min %>% group_by(approach) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier))
df_min_imax$approach <- factor(df_min_imax$approach,
                               levels = c("Reference", unique(df_min_imax$approach[df_min_imax$approach != "Reference"]
                                                              [order(df_min_imax$cv_mean_cindex[df_min_imax$approach != "Reference"])])))
df_min_imax$cv_mean_spars[is.na(df_min_imax$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_iminmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_min_imax, df_min_imax, df_min_imax, "Performance measures\napproach summarised by minimum dataset and maximum ibrier")
dev.off()

# imin
df_min <- df[df$n == min(df$n),]
df_min_imin <- df_min %>% group_by(approach) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier))
df_min_imin$approach <- factor(df_min_imin$approach,
                               levels = c("Reference", unique(df_min_imin$approach[df_min_imin$approach != "Reference"]
                                                              [order(df_min_imin$cv_mean_cindex[df_min_imin$approach != "Reference"])])))
df_min_imin$cv_mean_spars[is.na(df_min_imin$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_iminmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_min_imin, df_min_imin, df_min_imin, "Performance measures\napproach summarised by minimum dataset and minimum ibrier")
dev.off()

# imed
df_min <- df[df$n == min(df$n),]
df_min_imed <- df_min %>% group_by(approach) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier))
df_min_imed$approach <- factor(df_min_imed$approach,
                               levels = c("Reference", unique(df_min_imed$approach[df_min_imed$approach != "Reference"]
                                                              [order(df_min_imed$cv_mean_cindex[df_min_imed$approach != "Reference"])])))
df_min_imed$cv_mean_spars[is.na(df_min_imed$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_iminmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_min_imed, df_min_imed, df_min_imed, "Performance measures\napproach summarised by minimum dataset and median ibrier")
dev.off()

##### 1.1.6.2.3 all dim----
# amax
df_min <- df[df$n == min(df$n),]
df_min_cmax <- df_min %>% group_by(approach) %>% summarise(cv_mean_cindex = max(cv_mean_cindex))
df_min_cmax$approach <- factor(df_min_cmax$approach,
                               levels = c("Reference", unique(df_min_cmax$approach[df_min_cmax$approach != "Reference"]
                                                              [order(df_min_cmax$cv_mean_cindex[df_min_cmax$approach != "Reference"])])))
df_min_imax <- df_min %>% group_by(approach) %>% summarise(cv_mean_ibrier = max(cv_mean_ibrier))
df_min_imax$approach <- factor(df_min_imax$approach,
                               levels = c("Reference", unique(df_min_imax$approach[df_min_imax$approach != "Reference"]
                                                              [order(df_min_cmax$cv_mean_cindex[df_min_imax$approach != "Reference"])])))
df_min$cv_mean_spars[is.na(df_min$cv_mean_spars)] <- 0
df_min_smax <- df_min %>% group_by(approach) %>% summarise(cv_mean_spars = max(cv_mean_spars))
df_min_smax$approach <- factor(df_min_smax$approach,
                               levels = c("Reference", unique(df_min_smax$approach[df_min_smax$approach != "Reference"]
                                                              [order(df_min_cmax$cv_mean_cindex[df_min_imax$approach != "Reference"])])))


#pdf(file = "dataset_approach_mminmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_min_cmax, df_min_imax, df_min_smax, "Performance measures\napproach summarised by minimum dataset, maximum cindex, maximum ibrier and maximum no. of features")
dev.off()

# amin
df_min <- df[df$n == min(df$n),]
df_min_cmin <- df_min %>% group_by(approach) %>% summarise(cv_mean_cindex = min(cv_mean_cindex))
df_min_cmin$approach <- factor(df_min_cmin$approach,
                               levels = c("Reference", unique(df_min_cmin$approach[df_min_cmin$approach != "Reference"]
                                                              [order(df_min_cmin$cv_mean_cindex[df_min_cmin$approach != "Reference"])])))
df_min_imin <- df_min %>% group_by(approach) %>% summarise(cv_mean_ibrier = min(cv_mean_ibrier))
df_min_imin$approach <- factor(df_min_imin$approach,
                               levels = c("Reference", as.character(unique(df_min_cmin$approach[df_min_cmin$approach != "Reference"][order(df_min_cmin$cv_mean_cindex[df_min_cmin$approach != "Reference"])]))))
df_min$cv_mean_spars[is.na(df_min$cv_mean_spars)] <- 0
df_min_smin <- df_min %>% group_by(approach) %>% summarise(cv_mean_spars = min(cv_mean_spars))
df_min_smin$approach <- factor(df_min_smin$approach,
                               levels = c("Reference", as.character(unique(df_min_cmin$approach[df_min_cmin$approach != "Reference"][order(df_min_cmin$cv_mean_cindex[df_min_cmin$approach != "Reference"])]))))


#pdf(file = "dataset_approach_mminmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_min_cmin, df_min_imin, df_min_smin, "Performance measures\napproach summarised by minimum dataset, minimum cindex, minimum ibrier and minimum no. of features")
dev.off()

# amean
df_min <- df[df$n == min(df$n),]
df_min_cmean <- df_min %>% group_by(approach) %>% summarise(cv_mean_cindex = mean(cv_mean_cindex))
df_min_cmean$approach <- factor(df_min_cmean$approach,
                                levels = c("Reference", unique(df_min_cmean$approach[df_min_cmean$approach != "Reference"]
                                                               [order(df_min_cmean$cv_mean_cindex[df_min_cmean$approach != "Reference"])])))
df_min_imean <- df_min %>% group_by(approach) %>% summarise(cv_mean_ibrier = mean(cv_mean_ibrier))
df_min_imean$approach <- factor(df_min_imean$approach,
                                levels = c("Reference", as.character(unique(df_min_cmean$approach[df_min_cmean$approach != "Reference"][order(df_min_cmean$cv_mean_cindex[df_min_cmin$approach != "Reference"])]))))
df_min$cv_mean_spars[is.na(df_min$cv_mean_spars)] <- 0
df_min_smean <- df_min %>% group_by(approach) %>% summarise(cv_mean_spars = mean(cv_mean_spars))
df_min_smean$approach <- factor(df_min_smean$approach,
                                levels = c("Reference", as.character(unique(df_min_cmean$approach[df_min_cmean$approach != "Reference"][order(df_min_cmean$cv_mean_cindex[df_min_cmean$approach != "Reference"])]))))


#pdf(file = "dataset_approach_mminmean.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_min_cmean, df_min_imean, df_min_smean, "Performance measures\napproach summarised by minimum dataset, mean cindex, mean ibrier and mean no. of features")
dev.off()

# amed
df_min <- df[df$n == min(df$n),]
df_min_cmed <- df_min %>% group_by(approach) %>% summarise(cv_mean_cindex = mymedian(cv_mean_cindex))
df_min_cmed$approach <- factor(df_min_cmed$approach,
                               levels = c("Reference", unique(df_min_cmed$approach[df_min_cmed$approach != "Reference"]
                                                              [order(df_min_cmed$cv_mean_cindex[df_min_cmed$approach != "Reference"])])))
df_min_imed <- df_min %>% group_by(approach) %>% summarise(cv_mean_ibrier = mymedian(cv_mean_ibrier))
df_min_imed$approach <- factor(df_min_imed$approach,
                               levels = c("Reference", as.character(unique(df_min_cmed$approach[df_min_cmed$approach != "Reference"][order(df_min_cmed$cv_mean_cindex[df_min_cmin$approach != "Reference"])]))))
df_min$cv_mean_spars[is.na(df_min$cv_mean_spars)] <- 0
df_min_smed <- df_min %>% group_by(approach) %>% summarise(cv_mean_spars = mymedian(cv_mean_spars))
df_min_smed$approach <- factor(df_min_smed$approach,
                               levels = c("Reference", as.character(unique(df_min_cmed$approach[df_min_cmed$approach != "Reference"][order(df_min_cmed$cv_mean_cindex[df_min_cmean$approach != "Reference"])]))))


#pdf(file = "dataset_approach_mminmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_min_cmed, df_min_imed, df_min_smed, "Performance measures\napproach summarised by minimum dataset, median cindex, median ibrier and median no. of features")
dev.off()

#### 1.1.6.3 med dataset----
##### 1.1.6.3.1 cindex----
df_med <- df[df$n == mymedian(df$n),]
df_med_cmax <- df_med %>% group_by(approach) %>% summarise(cv_mean_cindex = max(cv_mean_cindex))
df_med_cmax$approach <- factor(df_med_cmax$approach,
                               levels = c("Reference", unique(df_med_cmax$approach[df_med_cmax$approach != "Reference"]
                                                              [order(df_med_cmax$cv_mean_cindex[df_med_cmax$approach != "Reference"])])))
df_med_imax <- df_med %>% group_by(approach) %>% summarise(cv_mean_ibrier = max(cv_mean_ibrier))
df_med_imax$approach <- factor(df_med_imax$approach,
                               levels = c("Reference", unique(df_med_imax$approach[df_med_imax$approach != "Reference"]
                                                              [order(df_med_cmax$cv_mean_cindex[df_med_imax$approach != "Reference"])])))
df_med$cv_mean_spars[is.na(df_med$cv_mean_spars)] <- 0
df_med_smax <- df_med %>% group_by(approach) %>% summarise(cv_mean_spars = max(cv_mean_spars))
df_med_smax$approach <- factor(df_med_smax$approach,
                               levels = c("Reference", unique(df_med_smax$approach[df_med_smax$approach != "Reference"]
                                                              [order(df_med_cmax$cv_mean_cindex[df_med_imax$approach != "Reference"])])))


#pdf(file = "dataset_approach_cmedmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_med_cmax, df_med_imax, df_med_smax, "Performance measures\napproach summarised by median dataset, maximum cindex, maximum ibrier and maximum no. of features")
dev.off()


# cmin
df_med <- df[df$n == mymedian(df$n),]
df_med_cmin <- df_med %>% group_by(approach) %>% filter(cv_mean_cindex == min(cv_mean_cindex))
df_med_cmin$approach <- factor(df_med_cmin$approach,
                               levels = c("Reference", unique(df_med_cmin$approach[df_med_cmin$approach != "Reference"]
                                                              [order(df_med_cmin$cv_mean_cindex[df_med_cmin$approach != "Reference"])])))
df_med_cmin$cv_mean_spars[is.na(df_med_cmin$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_cmedmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_med_cmin, df_med_cmin, df_med_cmin, "Performance measures\napproach summarised by median dataset and minimum cindex")
dev.off()

# cmed
df_med <- df[df$n == mymedian(df$n),]
df_med_cmed <- df_med %>% group_by(approach) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% filter(1:n() == 1)
df_med_cmed$approach <- factor(df_med_cmed$approach,
                               levels = c("Reference", unique(df_med_cmed$approach[df_med_cmed$approach != "Reference"]
                                                              [order(df_med_cmed$cv_mean_cindex[df_med_cmed$approach != "Reference"])])))
df_med_cmed$cv_mean_spars[is.na(df_med_cmed$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_medmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_med_cmed, df_med_cmed, df_med_cmed, "Performance measures\napproach summarised by median dataset and median cindex")
dev.off()


##### 1.1.6.3.2 ibrier----
# imax
df_med <- df[df$n == mymedian(df$n),]
df_med_imax <- df_med %>% group_by(approach) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier))
df_med_imax$approach <- factor(df_med_imax$approach,
                               levels = c("Reference", unique(df_med_imax$approach[df_med_imax$approach != "Reference"]
                                                              [order(df_med_imax$cv_mean_cindex[df_med_imax$approach != "Reference"])])))
df_med_imax$cv_mean_spars[is.na(df_med_imax$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_imedmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_med_imax, df_med_imax, df_med_imax, "Performance measures\napproach summarised by median dataset and maximum ibrier")
dev.off()

# imin
df_med <- df[df$n == mymedian(df$n),]
df_med_imin <- df_med %>% group_by(approach) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier))
df_med_imin$approach <- factor(df_med_imin$approach,
                               levels = c("Reference", unique(df_med_imin$approach[df_med_imin$approach != "Reference"]
                                                              [order(df_med_imin$cv_mean_cindex[df_med_imin$approach != "Reference"])])))
df_med_imin$cv_mean_spars[is.na(df_med_imin$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_imedmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_med_imin, df_med_imin, df_med_imin, "Performance measures\napproach summarised by median dataset and minimum ibrier")
dev.off()

# imed
df_med <- df[df$n == mymedian(df$n),]
df_med_imed <- df_med %>% group_by(approach) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier))
df_med_imed$approach <- factor(df_med_imed$approach,
                               levels = c("Reference", unique(df_med_imed$approach[df_med_imed$approach != "Reference"]
                                                              [order(df_med_imed$cv_mean_cindex[df_med_imed$approach != "Reference"])])))
df_med_imed$cv_mean_spars[is.na(df_med_imed$cv_mean_spars)] <- 0

#pdf(file = "dataset_approach_imedmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_med_imed, df_med_imed, df_med_imed, "Performance measures\napproach summarised by median dataset and median ibrier")
dev.off()

##### 1.1.6.3.3 all dim----
# amax
df_med <- df[df$n == mymedian(df$n),]
df_med_cmax <- df_med %>% group_by(approach) %>% summarise(cv_mean_cindex = max(cv_mean_cindex))
df_med_cmax$approach <- factor(df_med_cmax$approach,
                               levels = c("Reference", unique(df_med_cmax$approach[df_med_cmax$approach != "Reference"]
                                                              [order(df_med_cmax$cv_mean_cindex[df_med_cmax$approach != "Reference"])])))
df_med_imax <- df_med %>% group_by(approach) %>% summarise(cv_mean_ibrier = max(cv_mean_ibrier))
df_med_imax$approach <- factor(df_med_imax$approach,
                               levels = c("Reference", unique(df_med_imax$approach[df_med_imax$approach != "Reference"]
                                                              [order(df_med_cmax$cv_mean_cindex[df_med_imax$approach != "Reference"])])))
df_med$cv_mean_spars[is.na(df_med$cv_mean_spars)] <- 0
df_med_smax <- df_med %>% group_by(approach) %>% summarise(cv_mean_spars = max(cv_mean_spars))
df_med_smax$approach <- factor(df_med_smax$approach,
                               levels = c("Reference", unique(df_med_smax$approach[df_med_smax$approach != "Reference"]
                                                              [order(df_med_cmax$cv_mean_cindex[df_med_imax$approach != "Reference"])])))


#pdf(file = "dataset_approach_mmedmax.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_med_cmax, df_med_imax, df_med_smax, "Performance measures\napproach summarised by median dataset, maximum cindex, maximum ibrier and maximum no. of features")
dev.off()

# amin
df_med <- df[df$n == mymedian(df$n),]
df_med_cmin <- df_med %>% group_by(approach) %>% summarise(cv_mean_cindex = min(cv_mean_cindex))
df_med_cmin$approach <- factor(df_med_cmin$approach,
                               levels = c("Reference", unique(df_med_cmin$approach[df_med_cmin$approach != "Reference"]
                                                              [order(df_med_cmin$cv_mean_cindex[df_med_cmin$approach != "Reference"])])))
df_med_imin <- df_med %>% group_by(approach) %>% summarise(cv_mean_ibrier = min(cv_mean_ibrier))
df_med_imin$approach <- factor(df_med_imin$approach,
                               levels = c("Reference", as.character(unique(df_med_cmin$approach[df_med_cmin$approach != "Reference"][order(df_med_cmin$cv_mean_cindex[df_med_cmin$approach != "Reference"])]))))
df_med$cv_mean_spars[is.na(df_med$cv_mean_spars)] <- 0
df_med_smin <- df_med %>% group_by(approach) %>% summarise(cv_mean_spars = min(cv_mean_spars))
df_med_smin$approach <- factor(df_med_smin$approach,
                               levels = c("Reference", as.character(unique(df_med_cmin$approach[df_med_cmin$approach != "Reference"][order(df_med_cmin$cv_mean_cindex[df_med_cmin$approach != "Reference"])]))))


#pdf(file = "dataset_approach_mmedmin.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_med_cmin, df_med_imin, df_med_smin, "Performance measures\napproach summarised by median dataset, minimum cindex, minimum ibrier and minimum no. of features")
dev.off()

# amean
df_med <- df[df$n == mymedian(df$n),]
df_med_cmean <- df_med %>% group_by(approach) %>% summarise(cv_mean_cindex = mean(cv_mean_cindex))
df_med_cmean$approach <- factor(df_med_cmean$approach,
                                levels = c("Reference", unique(df_med_cmean$approach[df_med_cmean$approach != "Reference"]
                                                               [order(df_med_cmean$cv_mean_cindex[df_med_cmean$approach != "Reference"])])))
df_med_imean <- df_med %>% group_by(approach) %>% summarise(cv_mean_ibrier = mean(cv_mean_ibrier))
df_med_imean$approach <- factor(df_med_imean$approach,
                                levels = c("Reference", as.character(unique(df_med_cmean$approach[df_med_cmean$approach != "Reference"][order(df_med_cmean$cv_mean_cindex[df_med_cmin$approach != "Reference"])]))))
df_med$cv_mean_spars[is.na(df_med$cv_mean_spars)] <- 0
df_med_smean <- df_med %>% group_by(approach) %>% summarise(cv_mean_spars = mean(cv_mean_spars))
df_med_smean$approach <- factor(df_med_smean$approach,
                                levels = c("Reference", as.character(unique(df_med_cmean$approach[df_med_cmean$approach != "Reference"][order(df_med_cmean$cv_mean_cindex[df_med_cmean$approach != "Reference"])]))))


#pdf(file = "dataset_approach_mmedmean.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_med_cmean, df_med_imean, df_med_smean, "Performance measures\napproach summarised by median dataset, mean cindex, mean ibrier and mean no. of features")
dev.off()

# amed
df_med <- df[df$n == mymedian(df$n),]
df_med_cmed <- df_med %>% group_by(approach) %>% summarise(cv_mean_cindex = mymedian(cv_mean_cindex))
df_med_cmed$approach <- factor(df_med_cmed$approach,
                               levels = c("Reference", unique(df_med_cmed$approach[df_med_cmed$approach != "Reference"]
                                                              [order(df_med_cmed$cv_mean_cindex[df_med_cmed$approach != "Reference"])])))
df_med_imed <- df_med %>% group_by(approach) %>% summarise(cv_mean_ibrier = mymedian(cv_mean_ibrier))
df_med_imed$approach <- factor(df_med_imed$approach,
                               levels = c("Reference", as.character(unique(df_med_cmed$approach[df_med_cmed$approach != "Reference"][order(df_med_cmed$cv_mean_cindex[df_med_cmin$approach != "Reference"])]))))
df_med$cv_mean_spars[is.na(df_med$cv_mean_spars)] <- 0
df_med_smed <- df_med %>% group_by(approach) %>% summarise(cv_mean_spars = mymedian(cv_mean_spars))
df_med_smed$approach <- factor(df_med_smed$approach,
                               levels = c("Reference", as.character(unique(df_med_cmed$approach[df_med_cmed$approach != "Reference"][order(df_med_cmed$cv_mean_cindex[df_med_cmean$approach != "Reference"])]))))


#pdf(file = "dataset_approach_mmedmed.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_med_cmed, df_med_imed, df_med_smed, "Performance measures\napproach summarised by median dataset, median cindex, median ibrier and median no. of features")
dev.off()

#### 1.1.6.4 cindex----
# minimum
df_cmin <- df %>% group_by(approach) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1)
df_cmin$cv_mean_spars[is.na(df_cmin$cv_mean_spars)] <- 0
df_cmin$approach <- factor(df_cmin$approach,
                               levels = c("Reference", as.character(unique(df_cmin$approach[df_cmin$approach != "Reference"][order(df_cmin$cv_mean_cindex[df_cmin$approach != "Reference"])]))))

#pdf(file = "cindex_min_approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_cmin, df_cmin, df_cmin, "Performance measures\napproach summarised by minimum cindex")
dev.off()

# maximum
df_cmax <- df %>% group_by(approach) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1)
df_cmax$cv_mean_spars[is.na(df_cmax$cv_mean_spars)] <- 0
df_cmax$approach <- factor(df_cmax$approach,
                           levels = c("Reference", as.character(unique(df_cmax$approach[df_cmax$approach != "Reference"][order(df_cmax$cv_mean_cindex[df_cmax$approach != "Reference"])]))))

#pdf(file = "cindex_max_approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_cmax, df_cmax, df_cmax, "Performance measures\napproach summarised by maximum cindex")
dev.off()

# median
df_cmed <- df %>% group_by(approach) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% filter(1:n() == 1)
df_cmed$cv_mean_spars[is.na(df_cmed$cv_mean_spars)] <- 0
df_cmed$approach <- factor(df_cmed$approach,
                           levels = c("Reference", as.character(unique(df_cmed$approach[df_cmed$approach != "Reference"][order(df_cmed$cv_mean_cindex[df_cmed$approach != "Reference"])]))))

#pdf(file = "cindex_med_approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_cmed, df_cmed, df_cmed, "Performance measures\napproach summarised by median cindex")
dev.off()

#### 1.1.6.4 ibrier----
# minimum
df_imin <- df %>% group_by(approach) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1)
df_imin$cv_mean_spars[is.na(df_imin$cv_mean_spars)] <- 0
df_imin$approach <- factor(df_imin$approach,
                           levels = c("Reference", as.character(unique(df_imin$approach[df_imin$approach != "Reference"][order(df_imin$cv_mean_cindex[df_imin$approach != "Reference"])]))))

#pdf(file = "ibrier_min_approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_imin, df_imin, df_imin, "Performance measures\napproach summarised by minimum ibrier")
dev.off()

# maximum
df_imax <- df %>% group_by(approach) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1)
df_imax$cv_mean_spars[is.na(df_imax$cv_mean_spars)] <- 0
df_imax$approach <- factor(df_imax$approach,
                           levels = c("Reference", as.character(unique(df_imax$approach[df_imax$approach != "Reference"][order(df_imax$cv_mean_cindex[df_imax$approach != "Reference"])]))))

#pdf(file = "ibrier_max_approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_imax, df_imax, df_imax, "Performance measures\napproach summarised by maximum ibrier")
dev.off()

# median
df_imed <- df %>% group_by(approach) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1)
df_imed$cv_mean_spars[is.na(df_imed$cv_mean_spars)] <- 0
df_imed$approach <- factor(df_imed$approach,
                           levels = c("Reference", as.character(unique(df_imed$approach[df_imed$approach != "Reference"][order(df_imed$cv_mean_cindex[df_imed$approach != "Reference"])]))))

#pdf(file = "ibrier_med_approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_imed, df_imed, df_imed, "Performance measures\napproach summarised by median ibrier")
dev.off()

#### 1.1.6.5 spars----
# minimum
df1 <- df
df1$cv_mean_spars[is.na(df1$cv_mean_spars)] <- 0
df_smin <- df1 %>% group_by(approach) %>% filter(cv_mean_spars == min(cv_mean_spars)) %>% filter(1:n() == 1)
df_smin$approach <- factor(df_smin$approach,
                           levels = c("Reference", as.character(unique(df_smin$approach[df_smin$approach != "Reference"][order(df_smin$cv_mean_cindex[df_smin$approach != "Reference"])]))))

#pdf(file = "spars_min_approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_smin, df_smin, df_smin, "Performance measures\napproach summarised by minimum no. of features")
dev.off()

# maximum
df_smax <- df1 %>% group_by(approach) %>% filter(cv_mean_spars == max(cv_mean_spars)) %>% filter(1:n() == 1)
df_smax$approach <- factor(df_smax$approach,
                           levels = c("Reference", as.character(unique(df_smax$approach[df_smax$approach != "Reference"][order(df_smax$cv_mean_cindex[df_smax$approach != "Reference"])]))))

#pdf(file = "spars_max_approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_smax, df_smax, df_smax, "Performance measures\napproach summarised by maximum no. of features")
dev.off()

# median
df_smed <- df1 %>% group_by(approach) %>% filter(cv_mean_spars == mymedian(cv_mean_spars)) %>% filter(1:n() == 1)
df_smed$approach <- factor(df_smed$approach,
                           levels = c("Reference", as.character(unique(df_smed$approach[df_smed$approach != "Reference"][order(df_smed$cv_mean_cindex[df_smed$approach != "Reference"])]))))

#pdf(file = "spars_med_approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_smed, df_smed, df_smed, "Performance measures\napproach summarised by median no. of features")
dev.off()

#### 1.1.6.5 all dim----
# minimum
df1 <- df
df1$cv_mean_spars[is.na(df1$cv_mean_spars)] <- 0
df_cmin <- df1 %>% group_by(approach) %>% summarise(cv_mean_cindex = min(cv_mean_cindex)) 
df_cmin$approach <- factor(df_cmin$approach,
                           levels = c("Reference", as.character(unique(df_cmin$approach[df_cmin$approach != "Reference"][order(df_cmin$cv_mean_cindex[df_cmin$approach != "Reference"])]))))

df_imin <- df1 %>% group_by(approach) %>% summarise(cv_mean_ibrier = min(cv_mean_ibrier)) 
df_imin$approach <- factor(df_imin$approach,
                           levels = c("Reference", as.character(unique(df_imin$approach[df_imin$approach != "Reference"][order(df_cmin$cv_mean_cindex[df_cmin$approach != "Reference"])]))))

df_smin <- df1 %>% group_by(approach) %>% summarise(cv_mean_spars = min(cv_mean_spars)) 
df_smin$approach <- factor(df_smin$approach,
                           levels = c("Reference", as.character(unique(df_smin$approach[df_smin$approach != "Reference"][order(df_cmin$cv_mean_cindex[df_cmin$approach != "Reference"])]))))

#pdf(file = "alle_min_approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_cmin, df_imin, df_smin, "Performance measures\napproach summarised by minimum Cindex, minimum ibrier and minimum no. of features")
dev.off()

# maximum
df1 <- df
df1$cv_mean_spars[is.na(df1$cv_mean_spars)] <- 0
df_cmax <- df1 %>% group_by(approach) %>% summarise(cv_mean_cindex = max(cv_mean_cindex)) 
df_cmax$approach <- factor(df_cmax$approach,
                           levels = c("Reference", as.character(unique(df_cmax$approach[df_cmax$approach != "Reference"][order(df_cmax$cv_mean_cindex[df_cmax$approach != "Reference"])]))))

df_imax <- df1 %>% group_by(approach) %>% summarise(cv_mean_ibrier = max(cv_mean_ibrier)) 
df_imax$approach <- factor(df_imax$approach,
                           levels = c("Reference", as.character(unique(df_imax$approach[df_imax$approach != "Reference"][order(df_cmax$cv_mean_cindex[df_cmax$approach != "Reference"])]))))

df_smax <- df1 %>% group_by(approach) %>% summarise(cv_mean_spars = max(cv_mean_spars)) 
df_smax$approach <- factor(df_smax$approach,
                           levels = c("Reference", as.character(unique(df_smax$approach[df_smax$approach != "Reference"][order(df_cmax$cv_mean_cindex[df_cmax$approach != "Reference"])]))))

#pdf(file = "alle_max_approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_cmax, df_imax, df_smax, "Performance measures\napproach summarised by maximum Cindex, maximum ibrier and maximum no. of features")
dev.off()

# median
df1 <- df
df1$cv_mean_spars[is.na(df1$cv_mean_spars)] <- 0
df_cmed <- df1 %>% group_by(approach) %>% summarise(cv_mean_cindex = mymedian(cv_mean_cindex)) 
df_cmed$approach <- factor(df_cmed$approach,
                           levels = c("Reference", as.character(unique(df_cmed$approach[df_cmed$approach != "Reference"][order(df_cmed$cv_mean_cindex[df_cmed$approach != "Reference"])]))))

df_imed <- df1 %>% group_by(approach) %>% summarise(cv_mean_ibrier = mymedian(cv_mean_ibrier))
df_imed$approach <- factor(df_imed$approach,
                           levels = c("Reference", as.character(unique(df_imed$approach[df_imed$approach != "Reference"][order(df_cmed$cv_mean_cindex[df_cmed$approach != "Reference"])]))))

df_smed <- df1 %>% group_by(approach) %>% summarise(cv_mean_spars = mymedian(cv_mean_spars)) 
df_smed$approach <- factor(df_smed$approach,
                           levels = c("Reference", as.character(unique(df_smed$approach[df_smed$approach != "Reference"][order(df_cmed$cv_mean_cindex[df_cmed$approach != "Reference"])]))))

#pdf(file = "alle_median_approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_cmed, df_imed, df_smed, "Performance measures\napproach summarised by median Cindex, median ibrier and median no. of features")
dev.off()

# median
df1 <- df
df1$cv_mean_spars[is.na(df1$cv_mean_spars)] <- 0
df_cmean <- df1 %>% group_by(approach) %>% summarise(cv_mean_cindex = mean(cv_mean_cindex)) 
df_cmean$approach <- factor(df_cmean$approach,
                           levels = c("Reference", as.character(unique(df_cmean$approach[df_cmean$approach != "Reference"][order(df_cmean$cv_mean_cindex[df_cmean$approach != "Reference"])]))))

df_imean <- df1 %>% group_by(approach) %>% summarise(cv_mean_ibrier = mean(cv_mean_ibrier))
df_imean$approach <- factor(df_imean$approach,
                           levels = c("Reference", as.character(unique(df_imean$approach[df_imean$approach != "Reference"][order(df_cmean$cv_mean_cindex[df_cmean$approach != "Reference"])]))))

df_smean <- df1 %>% group_by(approach) %>% summarise(cv_mean_spars = mean(cv_mean_spars)) 
df_smean$approach <- factor(df_smean$approach,
                           levels = c("Reference", as.character(unique(df_smean$approach[df_smean$approach != "Reference"][order(df_cmean$cv_mean_cindex[df_cmean$approach != "Reference"])]))))

#pdf(file = "alle_mean_approach.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
approach_fig(df_cmean, df_imean, df_smean, "Performance measures\napproach summarised by mean Cindex, mean ibrier and mean no. of features")
dev.off()


### 1.1.7 dataset vs. features----
datset_fig <- function(dataset1, dataset2, dataset3, label){
  p1 <- ggplot(data = dataset1,
               aes(y = cv_mean_cindex, x = task.id))+
    geom_col(fill = gray1)+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset", y = "Cindex")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          legend.position = "none",
          axis.text.x = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_blank())+
    scale_y_continuous(expand = c(0,0))
  
  
  p2 <- ggplot(data = dataset2,
               aes(y = cv_mean_ibrier, x = task.id))+
    geom_col(fill = gray1)+
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
    scale_y_continuous(expand = c(0,0))
  
  p3 <- ggplot(data = dataset3 ,
               aes(y = cv_mean_spars, x = task.id))+
    geom_col(fill = gray1)+
    theme_tufte(base_family = "sans")+
    labs(x = "Dataset", y = "No. of features")+
    theme(axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = axissize, color = axis_col),
          axis.ticks.y = element_line(size = axissize, color = axis_col),
          strip.placement = "outside",
          legend.position = "none")+
    scale_y_continuous(expand = c(0,0))
  
  plot_grid(ggdraw() + draw_label(label,
                                  size = 11, fontfamily = "sans", hjust = 0, x = .01), cowplot::plot_grid(p1, p2, p3, nrow = 3, align = "v"),
            rel_heights = c(.1,1), ncol = 1)
}

#### 1.1.7.1 cindex----
# minimum
df_cmin <- df %>% group_by(task.id) %>% filter(cv_mean_cindex == min(cv_mean_cindex)) %>% filter(1:n() == 1)
df_cmin$task.id <- factor(df_cmin$task.id, levels = unique(df_cmin$task.id[order(df_cmin$n)]) )

#pdf(file = "dataset_minimum_Cindex.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_cmin, df_cmin, df_cmin, "Performance measures\ndataset summarised by minimum Cindex")
dev.off()

# maximum
df_cmax <- df %>% group_by(task.id) %>% filter(cv_mean_cindex == max(cv_mean_cindex)) %>% filter(1:n() == 1)
df_cmax$task.id <- factor(df_cmax$task.id, levels = unique(df_cmax$task.id[order(df_cmax$n)]) )

#pdf(file = "dataset_maximum_Cindex.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_cmax, df_cmax, df_cmax, "Performance measures\ndataset summarised by maximum Cindex")
dev.off()

# median
df_cmed <- df %>% group_by(task.id) %>% filter(cv_mean_cindex == mymedian(cv_mean_cindex)) %>% filter(1:n() == 1)
df_cmed$task.id <- factor(df_cmed$task.id, levels = unique(df_cmed$task.id[order(df_cmed$n)]) )

#pdf(file = "dataset_median_Cindex.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_cmed, df_cmed, df_cmed, "Performance measures\ndataset summarised by median Cindex")
dev.off()

#### 1.1.7.1 ibrier----
# minimum
df_imin <- df %>% group_by(task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier)) %>% filter(1:n() == 1)
df_imin$task.id <- factor(df_imin$task.id, levels = unique(df_imin$task.id[order(df_imin$n)]) )

#pdf(file = "dataset_minimum_ibrier.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_imin, df_imin, df_imin, "Performance measures\ndataset summarised by minimum Ibrier")
dev.off()

# maximum
df_imax <- df %>% group_by(task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier)) %>% filter(1:n() == 1)
df_imax$task.id <- factor(df_imax$task.id, levels = unique(df_imax$task.id[order(df_imax$n)]) )

#pdf(file = "dataset_maximum_ibrier.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_imax, df_imax, df_imax, "Performance measures\ndataset summarised by maximum Ibrier")
dev.off()

# median
df_imed <- df %>% group_by(task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier)) %>% filter(1:n() == 1)
df_imed$task.id <- factor(df_imed$task.id, levels = unique(df_imed$task.id[order(df_imed$n)]) )

#pdf(file = "dataset_median_ibrier.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_imed, df_imed, df_imed, "Performance measures\ndataset summarised by median Ibrier")
dev.off()

#### 1.1.7.1 spars----
df1 <- df
df1$cv_mean_spars[is.na(df1$cv_mean_spars)] <- 0
# minimum
df_smin <- df1 %>% group_by(task.id) %>% filter(cv_mean_spars == min(cv_mean_spars)) %>% filter(1:n() == 1)
df_smin$task.id <- factor(df_smin$task.id, levels = unique(df_smin$task.id[order(df_smin$n)]) )

#pdf(file = "dataset_minimum_spars.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_smin, df_smin, df_smin, "Performance measures\ndataset summarised by minimum no. of datasets")
dev.off()

# maximum
df_smax <- df1 %>% group_by(task.id) %>% filter(cv_mean_spars == max(cv_mean_spars)) %>% filter(1:n() == 1)
df_smax$task.id <- factor(df_smax$task.id, levels = unique(df_smax$task.id[order(df_smax$n)]) )

#pdf(file = "dataset_maximum_spars.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_smax, df_smax, df_smax, "Performance measures\ndataset summarised by maximum no. of datasets")
dev.off()

# median
df_smed <- df1 %>% group_by(task.id) %>% filter(cv_mean_spars == mymedian(cv_mean_spars)) %>% filter(1:n() == 1)
df_smed$task.id <- factor(df_smed$task.id, levels = unique(df_smed$task.id[order(df_smed$n)]) )

#pdf(file = "dataset_median_spars.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_smed, df_smed, df_smed, "Performance measures\ndataset summarised by median no. of datasets")
dev.off()

#### 1.1.7.1 all dim----
df1 <- df
df1$cv_mean_spars[is.na(df1$cv_mean_spars)] <- 0
# minimum
df_cmin <- df1 %>% group_by(task.id) %>% filter(cv_mean_spars == min(cv_mean_spars))
df_cmin$task.id <- factor(df_cmin$task.id, levels = unique(df_cmin$task.id[order(df_cmin$n)]))

df_imin <- df1 %>% group_by(task.id) %>% filter(cv_mean_ibrier == min(cv_mean_ibrier))
df_imin$task.id <- factor(df_imin$task.id, levels = unique(df_imin$task.id[order(df_imin$n)]))

df_smin <- df1 %>% group_by(task.id) %>% filter(cv_mean_spars == min(cv_mean_spars))
df_smin$task.id <- factor(df_smin$task.id, levels = unique(df_smin$task.id[order(df_smin$n)]))

#pdf(file = "dataset_minimum_allemasse.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_cmin, df_imin, df_smin, "Performance measures\ndataset summarised by minimum Cindex, minimum Ibrier and minimum no. of datasets")
dev.off()

# maximum
df_cmax <- df1 %>% group_by(task.id) %>% filter(cv_mean_spars == max(cv_mean_spars))
df_cmax$task.id <- factor(df_cmax$task.id, levels = unique(df_cmax$task.id[order(df_cmax$n)]))

df_imax <- df1 %>% group_by(task.id) %>% filter(cv_mean_ibrier == max(cv_mean_ibrier))
df_imax$task.id <- factor(df_imax$task.id, levels = unique(df_imax$task.id[order(df_imax$n)]))

df_smax <- df1 %>% group_by(task.id) %>% filter(cv_mean_spars == max(cv_mean_spars))
df_smax$task.id <- factor(df_smax$task.id, levels = unique(df_smax$task.id[order(df_smax$n)]))

#pdf(file = "dataset_maximum_allemasse.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_cmax, df_imax, df_smax, "Performance measures\ndataset summarised by maximum Cindex, maximum Ibrier and maximum no. of datasets")
dev.off()

# median
df_cmed <- df1 %>% group_by(task.id) %>% filter(cv_mean_spars == mymedian(cv_mean_spars))
df_cmed$task.id <- factor(df_cmed$task.id, levels = unique(df_cmed$task.id[order(df_cmed$n)]))

df_imed <- df1 %>% group_by(task.id) %>% filter(cv_mean_ibrier == mymedian(cv_mean_ibrier))
df_imed$task.id <- factor(df_imed$task.id, levels = unique(df_imed$task.id[order(df_imed$n)]))

df_smed <- df1 %>% group_by(task.id) %>% filter(cv_mean_spars == mymedian(cv_mean_spars))
df_smed$task.id <- factor(df_smed$task.id, levels = unique(df_smed$task.id[order(df_smed$n)]))

#pdf(file = "dataset_median_allemasse.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_cmed, df_imed, df_smed, "Performance measures\ndataset summarised by median Cindex, median Ibrier and median no. of datasets")
dev.off()

# median
df1$task.id <- factor(df1$task.id, levels = unique(df1$task.id[order(df1$n)]))
df_cmean <- df1 %>% group_by(task.id) %>% summarise(cv_mean_cindex = mean(cv_mean_cindex))

df_imean <- df1 %>% group_by(task.id) %>% summarise(cv_mean_ibrier = mean(cv_mean_ibrier))

df_smean <- df1 %>% group_by(task.id) %>% summarise(cv_mean_spars = mean(cv_mean_spars))

#pdf(file = "dataset_mean_allemasse.pdf", width = pdf_w_h[1], height = pdf_w_h[2] * 1.5)
datset_fig(df_cmean, df_imean, df_smean, "Performance measures\ndataset summarised by mean Cindex, mean Ibrier and mean no. of datasets")
dev.off()
