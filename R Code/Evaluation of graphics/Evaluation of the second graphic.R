library(ggplot2)

ev <- function(Typ, Darstellungsart, x , y,
               Ref_Km_Al , Ref_Coxph_Al , Steigungsrichtung_Al , Variabilitaet_Al , Form_Al , Steigung_Al , 
               Ref_Km_Re , Ref_Coxph_Re , Steigungsrichtung_Re , Variabilitaet_Re , Form_Re , Steigung_Re , 
               Ref_Km_Ra , Ref_Coxph_Ra , Steigungsrichtung_Ra , Variabilitaet_Ra , Form_Ra , Steigung_Ra , 
               Ref_Km_Pe , Ref_Coxph_Pe , Steigungsrichtung_Pe , Variabilitaet_Pe , Form_Pe , Steigung_Pe , 
               Ref_Km_Bo , Ref_Coxph_Bo , Steigungsrichtung_Bo , Variabilitaet_Bo , Form_Bo , Steigung_Bo ,
               Grafikname){
  data <- data.frame(Typ = NA, Darstellungsart = NA, x.Achse = NA, y.Achse = NA, 
                     Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA, Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                     Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA, Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                     Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA, Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
                     Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA, Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                     Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
                     Grafikname = NA)
  data$Typ <- Typ
  data$Darstellungsart <- Darstellungsart
  data$x.Achse <- ifelse(x == "d", "Dataset", ifelse(x == "t", "Calculation time", ifelse(x == "l", "learner", ifelse(x == "dt", "Dataset and Calculation time", NA))))
  data$y.Achse <- ifelse(y == 1, "Cindex", ifelse(y == 2, "Ibrier", "Spars"))
  data$Ref_Km_Al <- ifelse(Ref_Km_Al == 1, "darunter", ifelse(Ref_Km_Al == 2, "gleich", "darüber"))
  data$Ref_Coxph_Al <- ifelse(Ref_Coxph_Al == 1, "darunter", ifelse(Ref_Coxph_Al == 2, "gleich", "darüber"))
  data$Steigungsrichtung_Al <- ifelse(Steigungsrichtung_Al == 1, "positiv", ifelse(Steigungsrichtung_Al == 2, "neutral", "negativ"))
  data$Variabilitaet_Al <- ifelse(Variabilitaet_Al == 1, "schwach", ifelse(Variabilitaet_Al == 2, "maessig", "stark"))
  data$Form_Al <- ifelse(Form_Al == 1, "exponentiell", ifelse(Form_Al == 2, "logarithmisch", "linear"))
  data$Steigung_Al <- ifelse(Steigung_Al == 1, "schwach", ifelse(Steigung_Al == 2, "mittel", "stark"))

  data$Ref_Km_Re <- ifelse(Ref_Km_Re == 1, "darunter", ifelse(Ref_Km_Re == 2, "gleich", "darüber"))
  data$Ref_Coxph_Re <- ifelse(Ref_Coxph_Re == 1, "darunter", ifelse(Ref_Coxph_Re == 2, "gleich", "darüber"))
  data$Steigungsrichtung_Re <- ifelse(Steigungsrichtung_Re == 1, "positiv", ifelse(Steigungsrichtung_Re == 2, "neutral", "negativ"))
  data$Variabilitaet_Re <- ifelse(Variabilitaet_Re == 1, "schwach", ifelse(Variabilitaet_Re == 2, "maessig", "stark"))
  data$Form_Re <- ifelse(Form_Re == 1, "exponentiell", ifelse(Form_Re == 2, "logarithmisch", "linear"))
  data$Steigung_Re <- ifelse(Steigung_Re == 1, "schwach", ifelse(Steigung_Re == 2, "mittel", "stark"))
  
  data$Ref_Km_Ra <- ifelse(Ref_Km_Ra == 1, "darunter", ifelse(Ref_Km_Ra == 2, "gleich", "darüber"))
  data$Ref_Coxph_Ra <- ifelse(Ref_Coxph_Ra == 1, "darunter", ifelse(Ref_Coxph_Ra == 2, "gleich", "darüber"))
  data$Steigungsrichtung_Ra <- ifelse(Steigungsrichtung_Ra == 1, "positiv", ifelse(Steigungsrichtung_Ra == 2, "neutral", "negativ"))
  data$Variabilitaet_Ra <- ifelse(Variabilitaet_Ra == 1, "schwach", ifelse(Variabilitaet_Ra == 2, "maessig", "stark"))
  data$Form_Ra <- ifelse(Form_Ra == 1, "exponentiell", ifelse(Form_Ra == 2, "logarithmisch", "linear"))
  data$Steigung_Ra <- ifelse(Steigung_Ra == 1, "schwach", ifelse(Steigung_Ra == 2, "mittel", "stark"))
  
  data$Ref_Km_Pe <- ifelse(Ref_Km_Pe == 1, "darunter", ifelse(Ref_Km_Pe == 2, "gleich", "darüber"))
  data$Ref_Coxph_Pe <- ifelse(Ref_Coxph_Pe == 1, "darunter", ifelse(Ref_Coxph_Pe == 2, "gleich", "darüber"))
  data$Steigungsrichtung_Pe <- ifelse(Steigungsrichtung_Pe == 1, "positiv", ifelse(Steigungsrichtung_Pe == 2, "neutral", "negativ"))
  data$Variabilitaet_Pe <- ifelse(Variabilitaet_Pe == 1, "schwach", ifelse(Variabilitaet_Pe == 2, "maessig", "stark"))
  data$Form_Pe <- ifelse(Form_Pe == 1, "exponentiell", ifelse(Form_Pe == 2, "logarithmisch", "linear"))
  data$Steigung_Pe <- ifelse(Steigung_Pe == 1, "schwach", ifelse(Steigung_Pe == 2, "mittel", "stark"))
  
  data$Ref_Km_Bo <- ifelse(Ref_Km_Bo == 1, "darunter", ifelse(Ref_Km_Bo == 2, "gleich", "darüber"))
  data$Ref_Coxph_Bo <- ifelse(Ref_Coxph_Bo == 1, "darunter", ifelse(Ref_Coxph_Bo == 2, "gleich", "darüber"))
  data$Steigungsrichtung_Bo <- ifelse(Steigungsrichtung_Bo == 1, "positiv", ifelse(Steigungsrichtung_Bo == 2, "neutral", "negativ"))
  data$Variabilitaet_Bo <- ifelse(Variabilitaet_Bo == 1, "schwach", ifelse(Variabilitaet_Bo == 2, "maessig", "stark"))
  data$Form_Bo <- ifelse(Form_Bo == 1, "exponentiell", ifelse(Form_Bo == 2, "logarithmisch", "linear"))
  data$Steigung_Bo <- ifelse(Steigung_Bo == 1, "schwach", ifelse(Steigung_Bo == 2, "mittel", "stark"))
  
  data$Grafikname <- Grafikname
  
  return(data)
}

# 1. Comparison----
## 1.1 Bars----
df <- ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1, 
Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
Variabilitaet_Pe = 1, Form_Pe = 3, Steigung_Pe = 1, 
Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
Variabilitaet_Bo = 1, Form_Bo = 3, Steigung_Bo = 1,
Grafikname = "alle_max")

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 1, Steigung_Bo = 2,
               
               Grafikname = "alle_max"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "alle_max"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "alle_max_approach"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "alle_max_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "alle_max_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 3,
               
               Grafikname = "alle_mean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "alle_mean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "alle_mean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "alle_mean_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "alle_mean_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "alle_mean_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 1, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 2, Steigung_Bo = 3,
               
               Grafikname = "alle_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 3, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "alle_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "alle_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "alle_median_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "alle_median_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "alle_median_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 3, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 2, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 3,
               
               Grafikname = "alle_min"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "alle_min"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 2,
               
               Grafikname = "alle_min"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "alle_min_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "alle_min_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "alle_min_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "cindex_max"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 3, 
               
               Ref_Km_Pe = 3, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 1, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "cindex_max"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 3, Steigung_Bo = 3,
               
               Grafikname = "cindex_max"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "cindex_max_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "cindex_max_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "cindex_max_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "cindex_med_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "cindex_med_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "cindex_med_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 1, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 2,
               
               Grafikname = "cindex_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 3, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 2, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 3, Steigung_Bo = 2,
               
               Grafikname = "cindex_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 3,
               
               Grafikname = "cindex_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 3, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 3, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 2, Steigung_Bo = 3,
               
               Grafikname = "cindex_min"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 2, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 3, Steigung_Bo = 2,
               
               Grafikname = "cindex_min"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "cindex_min"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "cindex_min_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "cindex_min_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "cindex_min_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmaxmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmaxmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmaxmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmedmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmedmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmedmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmedmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmedmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmedmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cminmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cminmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cminmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cminmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cminmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cminmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imaxmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imaxmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imaxmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imaxmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imaxmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imaxmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imaxmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imaxmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imaxmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imedmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imedmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imedmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imedmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imedmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imedmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imedmin"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imedmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imedmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_iminmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_iminmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_iminmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_iminmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_iminmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_iminmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_iminmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_iminmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_iminmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_maxmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_maxmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_maxmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_maxmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_maxmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_maxmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_maxmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_maxmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_maxmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_medmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_medmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_medmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_minmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_minmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_minmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmaxmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmaxmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmaxmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmaxmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmaxmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmaxmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmaxmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmaxmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmaxmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmaxmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmaxmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmaxmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmedmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmedmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmedmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmedmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmedmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmedmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmedmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmedmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmedmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmedmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmedmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mmedmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mminmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mminmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mminmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mminmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mminmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mminmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mminmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mminmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mminmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mminmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mminmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_mminmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 3, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 2, Steigung_Bo = 3,
               
               Grafikname = "dataset_max"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "dataset_max"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 2,
               
               Grafikname = "dataset_max"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_maximum_allemasse"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_maximum_allemasse"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_maximum_allemasse"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_maximum_Cindex"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_maximum_Cindex"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_maximum_Cindex"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_maximum_ibrier"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_maximum_ibrier"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_maximum_ibrier"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_maximum_spars"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_maximum_spars"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_maximum_spars"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_mean_allemasse"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_mean_allemasse"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_mean_allemasse"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 2, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "dataset_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 3, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_median_allemasse"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_median_allemasse"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_median_allemasse"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_median_Cindex"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_median_Cindex"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_median_Cindex"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_median_ibrier"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_median_ibrier"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_median_ibrier"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_median_spars"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_median_spars"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_median_spars"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 2, Steigung_Bo = 3,
               
               Grafikname = "dataset_min"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "dataset_min"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "dataset_min"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_minimum_allemasse"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_minimum_allemasse"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_minimum_allemasse"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_minimum_Cindex"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_minimum_Cindex"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_minimum_Cindex"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_minimum_ibrier"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_minimum_ibrier"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_minimum_ibrier"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_minimum_spars"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_minimum_spars"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_minimum_spars"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 1, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 3,
               
               Grafikname = "ibrier_max"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "ibrier_max"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "ibrier_max"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "ibrier_max_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "ibrier_max_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "ibrier_max_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "ibrier_med_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "ibrier_med_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "ibrier_med_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 2, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "ibrier_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 3, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "ibrier_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 3, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "ibrier_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 2, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 2, Steigung_Bo = 3,
               
               Grafikname = "ibrier_min"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "ibrier_min"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 2,
               
               Grafikname = "ibrier_min"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "ibrier_min_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "ibrier_min_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "ibrier_min_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 3,
               
               Grafikname = "spars_max"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "spars_max"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 1, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "spars_max"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "spars_max_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "spars_max_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "spars_max_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "spars_med_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "spars_med_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "spars_med_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 3,
               
               Grafikname = "spars_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 3, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 1, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "spars_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 1, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "spars_median"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 2, Steigung_Bo = 3,
               
               Grafikname = "spars_min"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 3, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "spars_min"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "spars_min"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "spars_min_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "spars_min_approach"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Balken", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "spars_min_approach"))

## 1.2 Clustered Barchart----

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_allemed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_allemed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_allemed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 3, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_allemax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_allemax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_allemax"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_allemin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_allemin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_allemin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_smed"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 3, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_cmed"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_cmed"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 3, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "dataset_approach_cmin"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 2, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 2, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 2,
               
               Grafikname = "approach_dataset_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 3, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 2,
               
               Grafikname = "approach_dataset_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allemed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 3, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allemed"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allemed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allemax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 2, Steigung_Re = 2, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 2, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allemax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 1, Form_Pe = 2, Steigung_Pe = 3, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 2,
               
               Grafikname = "approach_dataset_allemax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 2,
               
               Grafikname = "approach_dataset_allemin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 2,
               
               Grafikname = "approach_dataset_allemin"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA, 
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 2,
               
               Grafikname = "approach_dataset_allemin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 2, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 2,
               
               Grafikname = "approach_dataset_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1, 
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 2,
               
               Grafikname = "approach_dataset_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 3, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 3, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 3, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 1, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 2,
               
               Grafikname = "approach_dataset_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 1, Form_Pe = 1, Steigung_Pe = 3, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 2, Steigung_Re = 2, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 2, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 1, Form_Pe = 1, Steigung_Pe = 3, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 2, Steigung_Bo = 3,
               
               Grafikname = "approach_dataset_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 2, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmin"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 3, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 2,
               
               Grafikname = "learner_dataset_allmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_allmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_allmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_allmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_allmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_allmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_allmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_allmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_allmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_allmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_allmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_allmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 3,
               
               Grafikname = "learner_dataset_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "clusterec barchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset_cmin"))

## 1.3 Connected Dotplot----

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "approach_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 2, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 2,
               
               Grafikname = "learner"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 2, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "learner"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Connected Dotplot", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 1, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "learner"))

## 1.4 Heatmap----
df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 3, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 3, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 1,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 2,
               
               Grafikname = "approach_dataset_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 3, Form_Re = 1, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 2,
               
               Grafikname = "approach_dataset_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo = 1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 3, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 3, Form_Bo = 3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo =  1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 2,
               
               Grafikname = "learner_dataset"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 1, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo = NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_learner"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_learner"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Heatmap", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_learner"))

## 1.5 Matrixchart----
df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 3, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 1, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 1, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 1, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 1, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 1, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo =  2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
               Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_allmin"))


df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 3, Form_Bo =  1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
               Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
               Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
               
               Grafikname = "approach_dataset_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
               Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
               Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
               
               Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
               Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
               
               Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
               
               Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 2, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "l", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
               Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 2, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
               Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
               
               Grafikname = "learner_dataset"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_allemean"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_cmin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_imin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smax"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smed"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_approach_smin"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 1,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_learner"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 2,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_learner"))

df <- rbind(df,
            ev(Typ = "Comparison", Darstellungsart = "Matrixchart", x = "d", y = 3,
               
               Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "dataset_learner"))

# 2. Deviation----
## 2.1 Bars----

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemean"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemean"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
              
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_allemin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
              
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_imin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_cmin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_allemean"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_allemean"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_allemed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_allemed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_allemax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_allemax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_allemin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_allemin"))


df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_imed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_imed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_imax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_imax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_imin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_imin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_cmed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_cmed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_cmax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_cmax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_cmin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmed_cmin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_allemean"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_allemean"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_allemed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_allemed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_allemax"))


df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_allemax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_allemin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_allemin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_imed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_imed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_imax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_imax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_imin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_imin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_cmed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_cmed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_cmax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_cmax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_cmin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmax_cmin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_allemean"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_allemean"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_allemed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_allemed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_allemax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_allemax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_allemin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_allemin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_imed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_imed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_imax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_imax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_imin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_imin"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_cmed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_cmed"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 2, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_cmax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
               
               Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
               Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_cmax"))

df <- rbind(df,
            ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
               
               Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
               Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 3, 
               
               Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
               Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
               
               Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
               Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
               
               Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
               Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
               
               Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
               Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
               
               Grafikname = "approach_dmin_cmin"))

 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "approach_dmin_cmin"))

 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 3, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 2,
                
                Grafikname = "learner_ph_dmax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_dmax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 3,
                
                Grafikname = "learner_ph_dmin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 1, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_dmin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 3, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 2, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 2,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 1, Form_Bo =  1, Steigung_Bo = 3,
                
                Grafikname = "learner_ph_allemed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 3, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_allemed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 3, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_allemax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 3, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_allemax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 3, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 2,
                
                Grafikname = "learner_ph_allemin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_allemin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 2, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 2,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 2, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 1, Form_Bo =  1, Steigung_Bo = 3,
                
                Grafikname = "learner_ph_imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 3, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 3, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 2, Form_Bo =  2, Steigung_Bo = 3,
                
                Grafikname = "learner_ph_imax"))
 
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_imax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 2,
                
                Grafikname = "learner_ph_imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 1, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 2,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 2,
                
                Grafikname = "learner_ph_cmed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_cmed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 3, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_cmax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 3, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
                
                Ref_Km_Ra = 3, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 2,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_cmax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 3, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 3,
                
                Grafikname = "learner_ph_cmin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 2, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_cmin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 3,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 1, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_dmed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 3, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 3, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_dmed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 3,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_allemean"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 1, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "learner_ph_allemean"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 3, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 2,
                
                Grafikname = "learner_dmax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "learner_dmax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 3,
                
                Grafikname = "learner_dmin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 1, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "learner_dmin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 3, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 2, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 2,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 1, Form_Bo =  1, Steigung_Bo = 3,
                
                Grafikname = "learner_allemed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 3, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "learner_allemed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 3, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "learner_allemax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 3, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "learner_allemax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 3, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 2,
                
                Grafikname = "learner_allemin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "learner_allemin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 2, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 2,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 2, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 1, Form_Bo =  1, Steigung_Bo = 3,
                
                Grafikname = "learner_imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 3, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 3, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "learner_imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 2, Form_Bo =  2, Steigung_Bo = 3,
                
                Grafikname = "learner_imax"))
 
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "learner_imax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 2,
                
                Grafikname = "learner_imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 1, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "learner_imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 2,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 2,
                
                Grafikname = "learner_cmed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "learner_cmed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 3, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "learner_cmax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 3, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
                
                Ref_Km_Ra = 3, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 2,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "learner_cmax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 1, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 3, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 3,
                
                Grafikname = "learner_cmin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 2, Ref_Coxph_Re = 2, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 2, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "learner_cmin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 3,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 1, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "learner_dmed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 3, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 3, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "learner_dmed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 3, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 3,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "learner_allemean"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Bars", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = 1, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 1, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "learner_allemean"))
 
 
 ## 2.2 Lines----
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 2, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "cmax"))

 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 3, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 2, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmean_imean"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmean_imean"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmed_imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmed_imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 1, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmin"))
 

 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 1, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 1, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "imax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "imax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 3, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 1, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 1, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 1, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 1, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_cmax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 1, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_cmax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_cmean_imean"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_cmean_imean"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_cmed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_cmed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_cmed_imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_cmed_imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_cmin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 1, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_cmin"))
 
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 1, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 1, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_imax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_imax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 1, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 1, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 1, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "km_imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_cmax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_cmax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 1, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 3, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_cmean_imean"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_cmean_imean"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 3, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_cmed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_cmed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 3, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_cmed_imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_cmed_imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 3, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_cmin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_cmin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 3, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 3, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_imax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 1, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 1, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 1, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_imax"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 3, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_imed"))
 
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_imed"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_imin"))
 
 df <- rbind(df,
             ev(Typ = "Deviation", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "ph_imin"))
 
 # 3. Multiple Distributions----
 ## 3.1 Lines----
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cmax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 3, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmed_imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cmed_imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cmed_imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cmed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cmin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cph_cmax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cph_cmax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cph_cmax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 2, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cph_cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cph_cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cph_cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cph_cmed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cph_cmed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cph_cmed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 3, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  1, Steigung_Bo = 2,
                
                Grafikname = "cph_cmin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cph_cmin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cph_cmin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cph_cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cph_cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cph_cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cph_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cph_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cph_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cph_imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cph_imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cph_imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cph_imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "cph_imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "cph_imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 2, 
                Variabilitaet_Bo = 2, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 1, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 1, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 3, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 2, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmax_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmin_imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmed_imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmed_imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_cmed_imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 2, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_imax"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 2, Form_Re = 1, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_imed"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_imin"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "Linien", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "km_imin"))
 
 ## 3.2 smooth----
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "smooth", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 2,
                Variabilitaet_Ra = 2, Form_Ra = 3, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 2, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "loess"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "smooth", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 2, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 1, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 1, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "loess"))

 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "smooth", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "loess"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "smooth", x = "d", y = 1,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 2,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 2, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 1, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 3, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 2, Form_Bo =  1, Steigung_Bo = 1,
                
                Grafikname = "ps"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "smooth", x = "d", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 1, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 2, Steigung_Re = 1, 
                
                Ref_Km_Ra = 2, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = 3,
                Variabilitaet_Ra = 1, Form_Ra = 2, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 3,
                Variabilitaet_Pe = 1, Form_Pe = 2, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 3, 
                Variabilitaet_Bo = 1, Form_Bo =  2, Steigung_Bo = 1,
                
                Grafikname = "ps"))
 
 df <- rbind(df,
             ev(Typ = "Multiple Distributions", Darstellungsart = "smooth", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = NA,
                Variabilitaet_Al = NA, Form_Al = NA, Steigung_Al = NA, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 3, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "ps"))
 # 4. Ranking----
 ## 4.1 Boxplots----
 
 
 df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplot", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 2, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "approach"))
 
 df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplot", x = "l", y = 2,
                
                Ref_Km_Al = 2, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "approach"))
 
 df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplot", x = "l", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "approach"))
 
 df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplot", x = "d", y = 1,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "dataset"))
 
 df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplot", x = "d", y = 2,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "dataset"))
 
 df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplot", x = "d", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = NA,
                Variabilitaet_Pe = NA, Form_Pe = NA, Steigung_Pe = NA, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = NA, 
                Variabilitaet_Bo = NA, Form_Bo =  NA, Steigung_Bo = NA,
                
                Grafikname = "dataset"))
 
 df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplot", x = "l", y = 1,
                
                Ref_Km_Al = 1, Ref_Coxph_Al = 3, Steigungsrichtung_Al = 3,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 1, Ref_Coxph_Re = 3, Steigungsrichtung_Re = 1,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 2, 
                
                Ref_Km_Ra = 1, Ref_Coxph_Ra = 3, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 2, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 1, Ref_Coxph_Pe = 3, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 2, Ref_Coxph_Bo = 3, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 1, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "learner"))
 
 df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplot", x = "l", y = 2,
                
                Ref_Km_Al = 3, Ref_Coxph_Al = 2, Steigungsrichtung_Al = 2,
                Variabilitaet_Al = 2, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = 3, Ref_Coxph_Re = 1, Steigungsrichtung_Re = 3,
                Variabilitaet_Re = 1, Form_Re = 3, Steigung_Re = 1, 
                
                Ref_Km_Ra = 3, Ref_Coxph_Ra = 2, Steigungsrichtung_Ra = 1,
                Variabilitaet_Ra = 1, Form_Ra = 1, Steigung_Ra = 1,
                
                Ref_Km_Pe = 2, Ref_Coxph_Pe = 2, Steigungsrichtung_Pe = 2,
                Variabilitaet_Pe = 3, Form_Pe = 3, Steigung_Pe = 1, 
                
                Ref_Km_Bo = 1, Ref_Coxph_Bo = 2, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 1, Form_Bo =  2, Steigung_Bo = 2,
                
                Grafikname = "learner"))
 
 df <- rbind(df,
             ev(Typ = "Ranking", Darstellungsart = "Boxplot", x = "l", y = 3,
                
                Ref_Km_Al = NA, Ref_Coxph_Al = NA, Steigungsrichtung_Al = 1,
                Variabilitaet_Al = 3, Form_Al = 3, Steigung_Al = 1, 
                
                Ref_Km_Re = NA, Ref_Coxph_Re = NA, Steigungsrichtung_Re = NA,
                Variabilitaet_Re = NA, Form_Re = NA, Steigung_Re = NA, 
                
                Ref_Km_Ra = NA, Ref_Coxph_Ra = NA, Steigungsrichtung_Ra = NA,
                Variabilitaet_Ra = NA, Form_Ra = NA, Steigung_Ra = NA,
                
                Ref_Km_Pe = NA, Ref_Coxph_Pe = NA, Steigungsrichtung_Pe = 1,
                Variabilitaet_Pe = 1, Form_Pe = 2, Steigung_Pe = 2, 
                
                Ref_Km_Bo = NA, Ref_Coxph_Bo = NA, Steigungsrichtung_Bo = 1, 
                Variabilitaet_Bo = 1, Form_Bo =  3, Steigung_Bo = 1,
                
                Grafikname = "learner"))
 
 # 5. Saving----
 save(df, file="eval_Grafik2.Rda")
 