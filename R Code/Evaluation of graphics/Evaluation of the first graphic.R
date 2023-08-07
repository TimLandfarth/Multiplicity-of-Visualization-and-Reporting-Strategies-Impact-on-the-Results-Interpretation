library(ggplot2)

ev <- function(Typ, Darstellungsart, x , y, Strat, Beob_Strata, ref, Steigungsrichtung, Variabilitaet, Form, Steigung, Grafikname){
  data <- data.frame(Typ = NA, Darstellungsart = NA, x.Achse = NA, y.Achse = NA, Stratifiziert = NA, Beob_Strata = NA, Referenz = NA, Steigungsrichtung = NA,
                     Variabilitaet = NA, Form = NA, Steigung = NA, Grafikname = NA)
  data$Typ <- Typ
  data$Darstellungsart <- Darstellungsart
  data$x.Achse <- ifelse(x == "d", "Dataset", ifelse(x == "t", "Calculation time", ifelse(x == "l", "learner", ifelse(x == "dt", "Dataset and Calculation time", NA))))
  data$y.Achse <- ifelse(y == "d", "Dataset", ifelse(y == "t", "Calculation time", "learner"))
  data$Stratifiziert <- ifelse(Strat == 1, "Nein", "Ja")
  data$Referenz <- ifelse(ref == 1, "Nein", "Ja")
  if(Steigungsrichtung == 1){ 
    data$Steigungsrichtung <- "positiv"
  }
  if(Steigungsrichtung == 2){
    data$Steigungsrichtung <- "negativ"
  }
  data$Variabilitaet <- ifelse(Variabilitaet == 1, "schwach", ifelse(Variabilitaet == 2, "maessig", "stark"))
  data$Form <- ifelse(Form == 1, "exponential", ifelse(Form == 2, "logarithmisch", "linear"))
  data$Steigung <- ifelse(Steigung == 1, "schwach", ifelse(Steigung == 2, "maessig", "stark"))
  data$Grafikname <- Grafikname
  data$Beob_Strata <- Beob_Strata
  return(data)
}

# 1. Comparison----
## 1.1 Bars----
(df <- ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "d",
          Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
          Form = 2, Steigung = 3, Grafikname = "comp_bars_dataset_max"))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "d",
          Strat = 1, Beob_Strata = NA, ref = 2, Steigungsrichtung = 1, Variabilitaet = 3,
          Form = 2, Steigung = 3, Grafikname = "comp_bars_dataset_max_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "d",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "comp_bars_dataset_mean")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "d",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "comp_bars_dataset_mean_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "d",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "comp_bars_dataset_median")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "d",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "comp_bars_dataset_min")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "d",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "comp_bars_dataset_sum")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "d",
                Strat = 1, Beob_Strata = NA, ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "comp_bars_dataset_sum_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 3, Grafikname = "comp_bars_learner_max")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 2, Grafikname = "comp_bars_learner_max")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 3, Steigung = 1, Grafikname = "comp_bars_learner_max")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 3, Grafikname = "comp_bars_learner_max_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "comp_bars_learner_max_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 3, Steigung = 1, Grafikname = "comp_bars_learner_max_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "comp_bars_learner_maxDat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "comp_bars_learner_maxDat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "comp_bars_learner_maxDat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 3, Grafikname = "comp_bars_learner_mean")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 2, Grafikname = "comp_bars_learner_mean")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 3, Steigung = 1, Grafikname = "comp_bars_learner_mean")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 1, Grafikname = "comp_bars_learner_mean_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 2, Grafikname = "comp_bars_learner_mean_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "comp_bars_learner_mean_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "comp_bars_learner_medDat_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "comp_bars_learner_medDat_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "comp_bars_learner_medDat_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 3, Steigung = 3, Grafikname = "comp_bars_learner_median")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 3, Grafikname = "comp_bars_learner_median")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "comp_bars_learner_median")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 3, Steigung = 3, Grafikname = "comp_bars_learner_median_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "comp_bars_learner_median_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 3, Steigung = 1, Grafikname = "comp_bars_learner_median_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 3, Grafikname = "comp_bars_learner_medianDat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 2, Grafikname = "comp_bars_learner_medianDat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 3, Steigung = 1, Grafikname = "comp_bars_learner_medianDat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 3, Steigung = 3, Grafikname = "comp_bars_learner_min")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "comp_bars_learner_min")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 1, Grafikname = "comp_bars_learner_min")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 3, Grafikname = "comp_bars_learner_min_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 3, Grafikname = "comp_bars_learner_min_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "comp_bars_learner_min_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 3, Grafikname = "comp_bars_learner_minDat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 3, Grafikname = "comp_bars_learner_minDat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "comp_bars_learner_minDat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 3, Steigung = 3, Grafikname = "comp_bars_learner_minDat_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 3, Grafikname = "comp_bars_learner_minDat_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 1, Grafikname = "comp_bars_learner_minDat_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "comp_bars_learner_nsub_max")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "comp_bars_learner_nsub_max_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "comp_bars_learner_nsub_maxDat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "comp_bars_learner_nsub_maxDat_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "comp_bars_learner_nsub_mean")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "comp_bars_learner_nsub_mean_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "comp_bars_learner_nsub_median")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "comp_bars_learner_nsub_median_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "comp_bars_learner_nsub_medianDat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "comp_bars_learner_nsub_medianDat_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 1, Grafikname = "comp_bars_learner_nsub_min")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 1, Grafikname = "comp_bars_learner_nsub_min_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "comp_bars_learner_nsub_minDat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "comp_bars_learner_nsub_minDat_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "comp_bars_learner_nsub_sum")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "comp_bars_learner_nsub_sum_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "d",
                Strat = 1, Beob_Strata = NA, ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "nomcomp_bars_dataset_median_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "d",
                Strat = 1, Beob_Strata = NA, ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "nomcomp_bars_dataset_min_ref")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 3, Grafikname = "nomcomp_bars_learner_sum")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regressuib", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "nomcomp_bars_learner_sum")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "nomcomp_bars_learner_sum")))

## 1.2 clustered Bars----
(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 2, Grafikname = "nomcomp_bars_learner_sum")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "LAML", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 1, Grafikname = "nomcomp_bars_learner_sum")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "KIRC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 2, Grafikname = "nomcomp_bars_learner_sum")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "BRCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "nomcomp_bars_learner_sum")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Allgemein_Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 3, Grafikname = "clustbar_Learner_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "LAML_Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 3, Grafikname = "clustbar_Learner_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "KIRC_Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 3, Steigung = 3, Grafikname = "clustbar_Learner_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "BRCA_Boosting", ref = 1, Steigungsrichtung = 2, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "clustbar_Learner_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Allgemein_Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 2, Grafikname = "clustbar_Learner_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "LAML_Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "clustbar_Learner_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "KIRC_Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 2, Grafikname = "clustbar_Learner_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "BRCA_Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "clustbar_Learner_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Allgemein_Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 1, Grafikname = "clustbar_Learner_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "LAML_Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 1, Grafikname = "clustbar_Learner_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "KIRC_Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "clustbar_Learner_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "BRCA_Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "clustbar_Learner_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "clustbar_Learner_nsub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "LAML", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 3, Grafikname = "clustbar_Learner_nsub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "KIRC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "clustbar_Learner_nsub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "l",
                Strat = 2, Beob_Strata = "BRCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "clustbar_Learner_nsub")))


(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 3, Grafikname = "clustbar_max")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 3, Grafikname = "clustbar_max")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "clustbar_max")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 3, Grafikname = "clustbar_max")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 2, Grafikname = "clustbar_mean")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "clustbar_mean")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "clustbar_mean")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 2, Grafikname = "clustbar_mean")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 3, Grafikname = "clustbar_median")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "clustbar_median")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "clustbar_median")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "clustbar_median")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "clustbar_min")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "clustbar_min")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 2, Grafikname = "clustbar_min")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "clustbar_min")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 3, Grafikname = "clustbar_sum")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "clustbar_sum")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 3, Grafikname = "clustbar_sum")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "clustered Balken", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "clustbar_sum")))

## 1.3 Connected Dotplot----

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "connected Dotplot", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Allgemein_Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 3, Grafikname = "condotplot_dat_ord1")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "connected Dotplot", x = "t", y = "l",
                Strat = 2, Beob_Strata = "LAML_Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 3, Grafikname = "condotplot_dat_ord1")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "connected Dotplot", x = "t", y = "l",
                Strat = 2, Beob_Strata = "BRCA_Boosting", ref = 1, Steigungsrichtung = 2, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "condotplot_dat_ord1")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "connected Dotplot", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Allgemein_Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 2, Grafikname = "condotplot_dat_ord1")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "connected Dotplot", x = "t", y = "l",
                Strat = 2, Beob_Strata = "LAML_Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "condotplot_dat_ord1")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "connected Dotplot", x = "t", y = "l",
                Strat = 2, Beob_Strata = "BRCA_Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "condotplot_dat_ord1")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "connected Dotplot", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Allgemein_Random forest", ref = 1, Steigungsrichtung = 2, Variabilitaet = 3,
                Form = 3, Steigung = 3, Grafikname = "condotplot_dat_ord1")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "connected Dotplot", x = "t", y = "l",
                Strat = 2, Beob_Strata = "LAML_Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "condotplot_dat_ord1")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "connected Dotplot", x = "t", y = "l",
                Strat = 2, Beob_Strata = "BRCA_Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "condotplot_dat_ord1")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "connected Dotplot", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 3, Grafikname = "condotplot_dat_ord2")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "connected Dotplot", x = "t", y = "l",
                Strat = 2, Beob_Strata = "LAML", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "condotplot_dat_ord2")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "connected Dotplot", x = "t", y = "l",
                Strat = 2, Beob_Strata = "BRCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 2, Grafikname = "condotplot_dat_ord2")))

## 1.4 Heatmap----
(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "blockForest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "grridge", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "rfsrc", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 2, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "ipflasso", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "prioritylasso", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "prioritylasso favoring", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "CoxBoost favoring", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "CoxBoost", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "glmboost", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "ranger", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "Lasso", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LAML", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "ESCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "PAAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "SARC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LIHC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "KIRP", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "COAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "OV", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "KIRC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "SKCM", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "STAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "BLCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "UCEC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LUSC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LGG", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LUAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "HNSC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "BRCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "heatm_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "heatm_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 1, Grafikname = "heatm_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "heatm_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "BRCA_Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "heatm_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "BRCA_Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "heatm_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "BRCA_Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "heatm_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LAML_Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "heatm_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LAML_Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "heatm_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LAML_Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "heatm_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "KIRC_Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "heatm_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "KIRC_Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "heatm_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = "l",
                Strat = 2, Beob_Strata = "KIRC_Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "heatm_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "BRCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "HNSC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "LUAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "LGG", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "LUSC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "UCEC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "BLCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "STAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "SKCM", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "KIRC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 2, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "OV", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "COAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "KIRP", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 2, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "LIHC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "SARC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "PAAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "ESCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "LAML", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "Lasso", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "ranger", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "glmboost", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "CoxBoost", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "CoxBoost favoring", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "priority Lasso", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 2, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "priority Lasso favoring", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 2, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "ipflasso", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "rfsrc", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "grridge", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "blockForest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 2, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "heatm_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 1, Grafikname = "heatm_lea_dat")))

## 1.5 Matrix chart----
(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "blockForest", ref = 1, Steigungsrichtung = 2, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "grridge", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "rfsrc", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "ipflasso", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "prioritylasso", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "prioritylasso favoring", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "CoxBoost favoring", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "CoxBoost", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "glmboost", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "ranger", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "Lasso", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 3, Steigung = 2, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LAML", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "ESCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "PAAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "SARC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LIHC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "KIRP", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 2, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "COAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 2, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "OV", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "KIRC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "SKCM", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "STAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "BLCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "UCEC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LUSC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LGG", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LUAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "HNSC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "BRCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "matrixcha_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "matrixcha_dat_lea_sub")))


(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "BRCA_Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "BRCA_Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "BRCA_Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LAML_Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "matrixcha_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LAML_Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "matrixcha_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "LAML_Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "matrixcha_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "KIRC_Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "KIRC_Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "matrixcha_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = "l",
                Strat = 2, Beob_Strata = "KIRC_Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "matrixcha_dat_lea_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "BRCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "HNSC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "LUAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "LGG", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "LUSC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "UCEC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "BLCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "STAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "SKCM", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "KIRC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "OV", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "COAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "KIRP", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "LIHC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "SARC", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "PAAD", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "ESCA", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "LAML", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "Lasso", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 3, Steigung = 3, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "ranger", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "glmboost", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "CoxBoost", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "CoxBoost favoring", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "prioritylasso favoring", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 2, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "prioritylasso", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "ipflasso", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "rfsrc", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "grridge", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "blockForest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_lea_dat")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "matrixcha_lea_dat_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_lea_dat_sub")))

(df <- rbind(df,
             ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "matrixcha_lea_dat_sub")))

# 2. Correlation----
## 2.1 Bars----
(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Dataset", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 2, Grafikname = "bars_mean")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Calculation time", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "bars_mean")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Dataset", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 2, Grafikname = "bars_mean_re")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Calculation time", ref = 2, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "bars_mean_re")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Dataset", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 2, Grafikname = "bars_median")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Calculation time", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "bars_median")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Dataset", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 3, Grafikname = "bars_median_ref")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Calculation time", ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "bars_median_ref")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Dataset", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 1, Grafikname = "bars_min")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Calculation time", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "bars_min")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Dataset", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 2, Grafikname = "bars_min_ref")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Calculation time", ref = 2, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 1, Grafikname = "bars_min_ref")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Dataset", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 2, Grafikname = "bars_sum")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Calculation time", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "bars_sum")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Dataset", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 2, Grafikname = "bars_sum_sub")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Calculation time", ref = 2, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "bars_sum_sub")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Dataset", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 2, Grafikname = "bars_max")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Calculation time", ref = 2, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "bars_max")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Dataset", ref = 2, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 2, Grafikname = "bars_max_ref")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Bars", x = "dt", y = "d",
                Strat = 2, Beob_Strata = "Calculation time", ref = 2, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "bars_max_ref")))

## 2.2 Scatterplot----

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Scatterplot", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "corr_loess")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Scatterplot", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "corr_loess")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Scatterplot", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "corr_loess")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Scatterplot", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "corr_loess")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Scatterplot", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 2, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "corr_loess_ref")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Scatterplot", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 2, Grafikname = "corr_ps")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Scatterplot", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 3, Grafikname = "corr_ps")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Scatterplot", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "corr_ps")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Scatterplot", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 1, Grafikname = "corr_ps")))

(df <- rbind(df,
             ev(Typ = "Korrelation", Darstellungsart = "Scatterplot", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 2, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "corr_ps_ref")))

# 3. Multiple Distributions----
## 3.1 Density----
(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Density", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 2, Grafikname = "multdist_loess")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Density", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 3, Grafikname = "multdist_loess")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Density", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "multdist_loess")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Density", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "multdist_loess")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Density", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 2, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "multdist_loess_ref")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Density", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 2, Grafikname = "multdist_ps")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Density", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 3, Grafikname = "multdist_ps")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Density", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "multdist_ps")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Density", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 1, Grafikname = "multdist_ps")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Density", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 2, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "multdist_ps_ref")))

## 3.2 Lines----

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 1, Steigung = 2, Grafikname = "multdist_min")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 3, Grafikname = "multdist_min")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 1, Grafikname = "multdist_min")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "multdist_min")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 2, Grafikname = "multdist_median")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "multdist_median")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "multdist_median")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 2, Grafikname = "multdist_median")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 1, Steigung = 1, Grafikname = "multdist_mean")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "multdist_mean")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "multdist_mean")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 3, Grafikname = "multdist_mean")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "multdist_max")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 1, Grafikname = "multdist_max")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "multdist_max")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 3, Steigung = 2, Grafikname = "multdist_max")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "multdist_sum")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 2, Grafikname = "multdist_sum")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 1, Grafikname = "multdist_sum")))

(df <- rbind(df,
             ev(Typ = "Multiple Verteilungen", Darstellungsart = "Linien", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "multdist_sum")))

# 4. Ranking----
## 4.1 Boxplots----

(df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplots", x = "t", y = "d",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 2, Steigung = 3, Grafikname = "ranking_box_dataset")))

(df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplots", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Allgemein", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "ranking_box_dataset_approach")))

(df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplots", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "ranking_box_dataset_approach")))

(df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplots", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 3,
                Form = 2, Steigung = 3, Grafikname = "ranking_box_dataset_approach")))

(df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplots", x = "t", y = "d",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 3, Grafikname = "ranking_box_dataset_approach")))

(df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplots", x = "t", y = "l",
                Strat = 1, Beob_Strata = NA, ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 2, Steigung = 3, Grafikname = "ranking_box_learner")))

(df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplots", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Boosting", ref = 1, Steigungsrichtung = 1, Variabilitaet = 1,
                Form = 3, Steigung = 1, Grafikname = "ranking_box_learner")))

(df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplots", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Penalized regression", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 1, Steigung = 3, Grafikname = "ranking_box_learner")))

(df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplots", x = "t", y = "l",
                Strat = 2, Beob_Strata = "Random forest", ref = 1, Steigungsrichtung = 1, Variabilitaet = 2,
                Form = 3, Steigung = 2, Grafikname = "ranking_box_learner")))





# 5. Saving----
save(df, file="eval_Grafik1.Rda")

