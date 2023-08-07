# 1. Loading Data----
library(DescTools)
load(file="eval_Grafik1.Rda")
df_1 <- df
rm(df)
load(file="eval_Grafik2.Rda")
df_2 <- df
rm(df)

# 2. Processing----
## 2.1 Fixing Error----
df_1$Grafikname[df_1$Grafikname == "nomcomp_bars_dataset_median_ref"] <- "comp_bars_dataset_median_ref"
df_1$Grafikname[df_1$Grafikname == "nomcomp_bars_dataset_min_ref"] <- "comp_bars_dataset_min_ref"

## 2.2 Code neutral observations accordingly----
df_2$Form_Al[df_2$Steigungsrichtung_Al == "neutral" &  !is.na(df_2$Steigungsrichtung_Al)] <- "konstant"
df_2$Steigung_Al[df_2$Steigungsrichtung_Al == "neutral" &  !is.na(df_2$Steigungsrichtung_Al)] <- "konstant"

df_2$Form_Re[df_2$Steigungsrichtung_Re == "neutral" &  !is.na(df_2$Steigungsrichtung_Re)] <- "konstant"
df_2$Steigung_Re[df_2$Steigungsrichtung_Re == "neutral" &  !is.na(df_2$Steigungsrichtung_Re)] <- "konstant"

df_2$Form_Ra[df_2$Steigungsrichtung_Ra == "neutral" &  !is.na(df_2$Steigungsrichtung_Ra)] <- "konstant"
df_2$Steigung_Ra[df_2$Steigungsrichtung_Ra == "neutral" &  !is.na(df_2$Steigungsrichtung_Ra)] <- "konstant"

df_2$Form_Pe[df_2$Steigungsrichtung_Pe == "neutral" &  !is.na(df_2$Steigungsrichtung_Pe)] <- "konstant"
df_2$Steigung_Pe[df_2$Steigungsrichtung_Pe == "neutral" &  !is.na(df_2$Steigungsrichtung_Pe)] <- "konstant"

df_2$Form_Bo[df_2$Steigungsrichtung_Bo == "neutral" &  !is.na(df_2$Steigungsrichtung_Bo)] <- "konstant"
df_2$Steigung_Bo[df_2$Steigungsrichtung_Bo == "neutral" &  !is.na(df_2$Steigungsrichtung_Bo)] <- "konstant"

## 2.2 adding factors----
### 2.2.1 Slope----
df_1$Steigung <- factor(df_1$Steigung, levels = c("schwach", "maessig", "stark"), ordered = T)
df_2$Steigung_Al <- factor(df_2$Steigung_Al, levels = c("konstant", "schwach", "mittel", "stark"), ordered = T)
df_2$Steigung_Re <- factor(df_2$Steigung_Re, levels = c("konstant", "schwach", "mittel", "stark"), ordered = T)
df_2$Steigung_Ra <- factor(df_2$Steigung_Ra, levels = c("konstant", "schwach", "mittel", "stark"), ordered = T)
df_2$Steigung_Pe <- factor(df_2$Steigung_Pe, levels = c("konstant", "schwach", "mittel", "stark"), ordered = T)
df_2$Steigung_Bo <- factor(df_2$Steigung_Bo, levels = c("konstant", "schwach", "mittel", "stark"), ordered = T)

### 2.2.2 Form----
df_1$Form <- factor(df_1$Form, levels = c("linear", "exponential", "logarithmisch"))
df_2$Form_Al <- factor(df_2$Form_Al)
df_2$Form_Re <- factor(df_2$Form_Re)
df_2$Form_Ra <- factor(df_2$Form_Ra)
df_2$Form_Pe <- factor(df_2$Form_Pe)
df_2$Form_Bo <- factor(df_2$Form_Bo)

## 2.1 Adding summary----
df_1$Zsf <- NA
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[1]] <- "max Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[2]] <- "max Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[3]] <- "mean Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[4]] <- "mean Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[5]] <- "median Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[6]] <- "min Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[7]] <- "sum Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[8]] <- "sum Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[9]] <- "max Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[10]] <- "max Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[11]] <- "Calc|max Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[12]] <- "mean Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[13]] <- "mean Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[14]] <- "Calc|median Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[15]] <- "median Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[16]] <- "median Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[17]] <- "Calc|median Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[18]] <- "min Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[19]] <- "min Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[20]] <- "Calc|min Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[21]] <- "Calc|min Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[22]] <- "max Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[23]] <- "max Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[24]] <- "Calc|max Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[25]] <- "Calc|max Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[26]] <- "mean Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[27]] <- "mean Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[28]] <- "median Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[29]] <- "median Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[30]] <- "Calc|median Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[31]] <- "Calc|median Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[32]] <- "min Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[33]] <- "min Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[34]] <- "Calc|min Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[35]] <- "Calc|min Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[36]] <- "sum Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[37]] <- "sum Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[38]] <- "median Calc| Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[39]] <- "min Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[40]] <- "sum Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[41]] <- "Calc|minmedmax Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[42]] <- "max Calc|Approach"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[43]] <- "mean Calc|Approach"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[44]] <- "median Calc|Approach"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[45]] <- "min Calc|Approach"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[46]] <- "sum Calc|Approach"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[47]] <- "Calc|minmax Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[48]] <- "Calc|minmax Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[49]] <- "original"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[50]] <- "original"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[51]] <- "original"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[52]] <- "original"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[53]] <- "original"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[54]] <- "original"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[55]] <- "original"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[56]] <- "mean Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[57]] <- "mean Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[58]] <- "median Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[59]] <- "median Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[60]] <- "min Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[61]] <- "min Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[62]] <- "sum Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[63]] <- "sum Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[64]] <- "max Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[65]] <- "max Calc|Dataset"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[66]] <- "smoothed"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[67]] <- "smoothed"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[68]] <- "smoothed"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[69]] <- "smoothed"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[70]] <- "smoothed"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[71]] <- "smoothed"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[72]] <- "smoothed"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[73]] <- "smoothed"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[74]] <- "smoothed"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[75]] <- "smoothed"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[76]] <- "smoothed"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[77]] <- "smoothed"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[78]] <- "smoothed"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[79]] <- "Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[80]] <- "Calc|Learner"
df_1$Zsf[df_1$Grafikname == unique(df_1$Grafikname)[81]] <- "Calc|Dataset"

df_2$Zsf <- NA
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[1]] <- "maxC maxI maxS|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[2]] <- "maxC maxI maxS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[3]] <- "meanC meanI meanS|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[4]] <- "meanC meanI meanS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[5]] <- "medianC medianI medianS|Learner"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[6]] <- "medianC medianI medianS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[7]] <- "minC minI minS|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[8]] <- "minC minI minS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[9]] <- "maxC|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[10]] <- "maxC|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[11]] <- "medianC|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[12]] <- "medianC|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[13]] <- "minC|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[14]] <- "minC|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[15]] <- "maxC maxD|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[16]] <- "maxC maxI maxS medianD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[17]] <- "minC medianD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[18]] <- "maxC maxI maxS minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[19]] <- "minC minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[20]] <- "maxI maxD|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[21]] <- "maxI maxD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[22]] <- "minI maxD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[23]] <- "maxI medianD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[24]] <- "medianI medianD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[25]] <- "minI medianD|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[26]] <- "maxI minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[27]] <- "medianI minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[28]] <- "minI minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[29]] <- "maxI minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[30]] <- "medianC maxD|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[31]] <- "minC maxD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[32]] <- "medianC medianD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[33]] <- "medianC minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[34]] <- "maxC maxI maxS maxD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[35]] <- "meanC meanI meanS maxD|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[36]] <- "medianC medianI medianS maxD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[37]] <- "minC minI minS maxD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[38]] <- "maxC maxI maxS medianD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[39]] <- "meanC meanI meanS medianD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[40]] <- "medianC medianI medianS medianD|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[41]] <- "minC minI minS medianD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[42]] <- "maxC maxI maxS minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[43]] <- "meanC meanI meanS minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[44]] <- "medianC medianI medianS minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[45]] <- "minC minI minS minD|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[46]] <- "maxD|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[47]] <- "maxC maxI maxS|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[48]] <- "maxC|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[49]] <- "maxI|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[50]] <- "maxS|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[51]] <- "meanC meanI meanS|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[52]] <- "medD|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[53]] <- "medianC medianI medianS|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[54]] <- "medianC|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[55]] <- "medianI|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[56]] <- "medianS|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[57]] <- "minD|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[58]] <- "minC minI minS|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[59]] <- "minC|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[60]] <- "minI|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[61]] <- "minS|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[62]] <- "maxI|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[63]] <- "maxI|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[64]] <- "medianI|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[65]] <- "medianI|Learner"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[66]] <- "minI|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[67]] <- "minI|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[68]] <- "maxI|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[69]] <- "maxS|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[70]] <- "maxS|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[71]] <- "medianS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[72]] <- "medianS|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[73]] <- "minS|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[74]] <- "meanC meanI meanS|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[75]] <- "medianC medianI medianS|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[76]] <- "maxC maxI maxS|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[77]] <- "minC minI minS|Dataset "
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[78]] <- "medianS|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[79]] <- "maxS|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[80]] <- "minS|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[81]] <- "medianI|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[82]] <- "maxI|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[83]] <- "minI|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[84]] <- "medianC|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[85]] <- "maxC|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[86]] <- "minC|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[87]] <- "meanC meanI meanS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[88]] <- "medianC medianI medianS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[89]] <- "maxC maxI maxS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[90]] <- "minC minI minS|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[91] & df_2$Darstellungsart == "clusterec barchart"] <- "medianS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[91] & df_2$Darstellungsart == "Heatmap"] <- "medianS|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[91] & df_2$Darstellungsart == "Matrixchart"] <- "medianS|Approach Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[92] & df_2$Darstellungsart == "clusterec barchart"] <- "maxS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[92] & df_2$Darstellungsart == "Heatmap"] <- "maxS|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[92] & df_2$Darstellungsart == "Matrixchart"] <- "maxS|Approach Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[93] & df_2$Darstellungsart == "clusterec barchart"] <- "minS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[93] & df_2$Darstellungsart == "Heatmap"] <- "minS|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[93] & df_2$Darstellungsart == "Matrixchart"] <- "minS|Approach Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[94] & df_2$Darstellungsart == "clusterec barchart"] <- "medianI|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[94] & df_2$Darstellungsart == "Heatmap"] <- "medianI|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[94] & df_2$Darstellungsart == "Matrixchart"] <- "medianI|Approach Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[95] & df_2$Darstellungsart == "clusterec barchart"] <- "maxI|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[95] & df_2$Darstellungsart == "Heatmap"] <- "maxI|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[95] & df_2$Darstellungsart == "Matrixchart"] <- "maxI|Approach Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[96] & df_2$Darstellungsart == "clusterec barchart"] <- "minI|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[96] & df_2$Darstellungsart == "Heatmap"] <- "minI|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[96] & df_2$Darstellungsart == "Matrixchart"] <- "minI|Approach Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[97] & df_2$Darstellungsart == "clusterec barchart"] <- "medianC|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[97] & df_2$Darstellungsart == "Heatmap"] <- "medianC|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[97] & df_2$Darstellungsart == "Matrixchart"] <- "medianC|Approach Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[98] & df_2$Darstellungsart == "clusterec barchart"] <- "maxC|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[98] & df_2$Darstellungsart == "Heatmap"] <- "maxC|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[98] & df_2$Darstellungsart == "Matrixchart"] <- "maxC|Approach Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[99] & df_2$Darstellungsart == "clusterec barchart"] <- "minC|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[99] & df_2$Darstellungsart == "Heatmap"] <- "minC|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[99] & df_2$Darstellungsart == "Matrixchart"] <- "minC|Approach Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[100]] <- "meanC meanI meanS|Learner"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[101]] <- "medianC medianI medianS|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[102]] <- "maxC maxI maxS|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[103]] <- "minC minI minS|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[104]] <- "medianS|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[105]] <- "maxS|Learner"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[106]] <- "minS|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[107]] <- "medianS|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[108]] <- "maxI|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[109]] <- "minI|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[110]] <- "medianC|Learner"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[111]] <- "maxC|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[112]] <- "minC|Learner"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[113] & df_2$Darstellungsart == "Connected Dotplot"] <- "maxC maxI maxS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[113] & df_2$Darstellungsart == "Bars"] <- "maxC maxI|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[114] & df_2$Darstellungsart == "Connected Dotplot"] <- "meanC meanI meanS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[114] & df_2$Darstellungsart == "Bars"] <- "meanC meanI|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[115] & df_2$Darstellungsart == "Connected Dotplot"] <- "medianC medianI medianS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[115] & df_2$Darstellungsart == "Bars"] <- "medianC medianI|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[116] & df_2$Darstellungsart == "Connected Dotplot"] <- "minC minI minS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[116] & df_2$Darstellungsart == "Bars"] <- "minC minI|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[117] & df_2$Darstellungsart == "Connected Dotplot"] <- "maxC|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[117] & df_2$Darstellungsart == "Bars"] <- "maxC|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[118] & df_2$Darstellungsart == "Connected Dotplot"] <- "medianC|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[118] & df_2$Darstellungsart == "Bars"] <- "medianC|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[119] & df_2$Darstellungsart == "Connected Dotplot"] <- "minC|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[119] & df_2$Darstellungsart == "Bars"] <- "minC|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[120] & df_2$Darstellungsart == "Connected Dotplot"] <- "maxI|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[120] & df_2$Darstellungsart == "Bars"] <- "maxI|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[121] & df_2$Darstellungsart == "Connected Dotplot"] <- "medianI|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[121] & df_2$Darstellungsart == "Bars"] <- "medianI|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[122] & df_2$Darstellungsart == "Connected Dotplot"] <- "minI|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[122] & df_2$Darstellungsart == "Bars"] <- "minI|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[123] & df_2$Darstellungsart == "Connected Dotplot"] <- "maxS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[124] & df_2$Darstellungsart == "Connected Dotplot"] <- "medianS|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[125] & df_2$Darstellungsart == "Connected Dotplot"] <- "minS|Approach"


df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[126] & df_2$Darstellungsart == "Connected Dotplot"] <- "|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[126] & df_2$Darstellungsart == "Boxplot"] <- "|Learner"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[127] & df_2$Darstellungsart == "Heatmap"] <- "meanC meanI meanS|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[127] & df_2$Darstellungsart == "Matrixchart"] <- "meanC meanI meanS|Approach Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[128] & df_2$Darstellungsart == "Heatmap"] <- "medianC medianI medianS|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[128] & df_2$Darstellungsart == "Matrixchart"] <- "medianC medianI medianS|Approach Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[129] & df_2$Darstellungsart == "Heatmap"] <- "maxC maxI maxS|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[129] & df_2$Darstellungsart == "Matrixchart"] <- "maxC maxI maxS|Approach Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[130] & df_2$Darstellungsart == "Heatmap"] <- "minC minI minS|Approach Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[130] & df_2$Darstellungsart == "Matrixchart"] <- "minC minI minS|Approach Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[131] & df_2$Darstellungsart == "Heatmap"] <- "|Learner Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[131] & df_2$Darstellungsart == "Matrixchart"] <- "|Learner Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[132]] <- "meanC meanI|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[133]] <- "medianC medianI|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[134]] <- "maxC maxI|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[135]] <- "minC minI|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[136]] <- "medianI medianD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[137]] <- "maxI medianD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[138]] <- "minI medianD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[139]] <- "medianC medianD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[140]] <- "maxC medianD|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[141]] <- "medianC medianD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[142]] <- "meanC meanI maxD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[143]] <- "medianC medianI maxD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[144]] <- "maxC maxI maxD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[145]] <- "minC minI maxD|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[146]] <- "medianI maxD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[147]] <- "maxI maxD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[148]] <- "minI maxD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[149]] <- "medC maxD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[150]] <- "maxC maxD|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[151]] <- "minC maxD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[152]] <- "meanC meanI minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[153]] <- "medianC medianI minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[154]] <- "maxC maxI minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[155]] <- "minC minI minD|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[156]] <- "medI minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[157]] <- "maxI minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[158]] <- "minI minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[159]] <- "minC minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[160]] <- "maxC minD|Approach"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[161]] <- "minC minD|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[162]] <- "phREF maxD|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[163]] <- "phREF minD|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[164]] <- "medC medI phREF|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[165]] <- "maxC maxI phREF|Learner"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[166]] <- "minC minI phREF|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[167]] <- "medianI phREF|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[168]] <- "maxI phREF|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[169]] <- "minI phREF|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[170]] <- "medianC phREF|Learner"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[171]] <- "maxC phREF|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[172]] <- "medianD phREF|Learner"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[173]] <- "meanC meanI phREF|Learner"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[174] & df_2$Typ == "Deviation"] <- "maxC|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[174] & df_2$Typ == "Multiple Distributions"] <- "maxC|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[175] & df_2$Typ == "Deviation"] <- "maxC maxI|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[175] & df_2$Typ == "Multiple Distributions"] <- "maxC maxI|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[176] & df_2$Typ == "Deviation"] <- "meanC meanI|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[177] & df_2$Typ == "Deviation"] <- "medianC|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[177] & df_2$Typ == "Multiple Distributions"] <- "medianC|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[178]] <- "medianC medianI|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[179]] <- "minC|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[180]] <- "minC minI|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[181]] <- "maxI|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[182]] <- "medianI|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[183]] <- "minI|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[184]] <- "maxC kmREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[185]] <- "maxC maxI kmREF|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[186]] <- "meanC meanI kmREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[187]] <- "medianC kmREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[188]] <- "medianC medianI kmREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[189]] <- "minC kmREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[190]] <- "minC minI kmREF|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[191]] <- "maxI kmREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[192]] <- "medianI kmREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[193]] <- "minI kmREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[194]] <- "maxC phREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[195]] <- "maxC maxI phREF|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[196]] <- "meanC meanI phREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[197]] <- "medianC phREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[198]] <- "minI kmREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[199]] <- "minC phREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[200]] <- "minC minI phREF|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[201]] <- "maxI phREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[202]] <- "medianI phREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[203]] <- "minI phREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[204]] <- "maxC phREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[205]] <- "maxC maxI phREF|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[206]] <- "medianC phREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[207]] <- "minC phREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[208]] <- "minC minI phREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[209]] <- "maxI phREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[210]] <- "medianI phREF|Dataset"

df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[211]] <- "minI phREF|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[212]] <- "|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[213]] <- "|Dataset"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[214]] <- "|Approach"
df_2$Zsf[df_2$Grafikname == unique(df_2$Grafikname)[215]] <- "|Dataset"

## 2.3 More accurate summary----
### 2.3.1 Graphic 1----
df1_1 <- df_1[df_1$Grafikname %in% c("comp_bars_dataset_max",
                                          "comp_bars_dataset_max_ref"),]
df1_1 <- rbind(df1_1, df_1[df_1$Grafikname == "clustbar_max" & df_1$Beob_Strata == "Allgemein",])
df1_1 <- rbind(df1_1, df_1[df_1$Grafikname %in% c("bars_max", "bars_max_ref") & df_1$Beob_Strata == "Calculation time",])
df1_1 <- rbind(df1_1, df_1[df_1$Grafikname == "multdist_max" & df_1$Beob_Strata == "Allgemein",])



df1_2 <- df_1[df_1$Grafikname %in% c("comp_bars_dataset_mean",
                                     "comp_bars_dataset_mean_ref"),]
df1_2 <- rbind(df1_2, df_1[df_1$Grafikname == "clustbar_mean" & df_1$Beob_Strata == "Allgemein",])
df1_2 <- rbind(df1_2, df_1[df_1$Grafikname %in% c("bars_mean", "bars_mean_re") & df_1$Beob_Strata == "Calculation time",])
df1_2 <- rbind(df1_2, df_1[df_1$Grafikname == "multdist_mean" & df_1$Beob_Strata == "Allgemein",])

df1_3 <- df_1[df_1$Grafikname %in% c("comp_bars_dataset_median",
                                 "comp_bars_dataset_median_ref"),]
df1_3 <- rbind(df1_3, df_1[df_1$Grafikname == "clustbar_median" & df_1$Beob_Strata == "Allgemein",])
df1_3 <- rbind(df1_3, df_1[df_1$Grafikname %in% c("bars_median", "bars_median_ref") & df_1$Beob_Strata == "Calculation time",])
df1_3 <- rbind(df1_3, df_1[df_1$Grafikname == "multdist_median" & df_1$Beob_Strata == "Allgemein",])

df1_4 <- df_1[df_1$Grafikname %in% c("comp_bars_dataset_min",
                                 "comp_bars_dataset_min_ref"),]
df1_4 <- rbind(df1_4, df_1[df_1$Grafikname == "clustbar_min" & df_1$Beob_Strata == "Allgemein",])
df1_4 <- rbind(df1_4, df_1[df_1$Grafikname %in% c("bars_min", "bars_min_ref") & df_1$Beob_Strata == "Calculation time",])
df1_4 <- rbind(df1_4, df_1[df_1$Grafikname == "multdist_min" & df_1$Beob_Strata == "Allgemein",])


df1_5 <- df_1[df_1$Grafikname %in% c("comp_bars_learner_nsub_maxDat", "comp_bars_learner_nsub_maxDat_ref"),]
df1_5 <- rbind(df1_5, df_1[df_1$Grafikname == "clustbar_Learner_nsub" & df_1$Beob_Strata == "BRCA",])
df1_5 <- rbind(df1_5, df_1[df_1$Grafikname == "condotplot_dat_ord2" & df_1$Beob_Strata == "BRCA",]) 
df1_5 <- rbind(df1_5, df_1[df_1$Grafikname == "heatm_dat_lea" & df_1$Beob_Strata == "BRCA",])
df1_5 <- rbind(df1_5, df_1[df_1$Grafikname == "matrixcha_dat_lea" & df_1$Beob_Strata == "BRCA",])

df1_6 <- df_1[df_1$Grafikname %in% c("comp_bars_learner_nsub_medianDat", "comp_bars_learner_nsub_medianDat_ref"),]
df1_6 <- rbind(df1_6, df_1[df_1$Grafikname == "clustbar_Learner_nsub" & df_1$Beob_Strata == "KIRC",])
df1_6 <- rbind(df1_6, df_1[df_1$Grafikname == "heatm_dat_lea" & df_1$Beob_Strata == "KIRC",])
df1_6 <- rbind(df1_6, df_1[df_1$Grafikname == "matrixcha_dat_lea" & df_1$Beob_Strata == "KIRC",])

df1_7 <- df_1[df_1$Grafikname %in% c("comp_bars_learner_nsub_minDat", "comp_bars_learner_nsub_minDat_ref"),]
df1_7 <- rbind(df1_7, df_1[df_1$Grafikname == "clustbar_Learner_nsub" & df_1$Beob_Strata == "LAML",])
df1_7 <- rbind(df1_7, df_1[df_1$Grafikname == "condotplot_dat_ord2" & df_1$Beob_Strata == "LAML",])
df1_7 <- rbind(df1_7, df_1[df_1$Grafikname == "heatm_dat_lea" & df_1$Beob_Strata == "LAML",])
df1_7 <- rbind(df1_7, df_1[df_1$Grafikname == "matrixcha_dat_lea" & df_1$Beob_Strata == "LAML",])

df1_8 <- df_1[df_1$Grafikname %in% c("comp_bars_learner_maxDat", "comp_bars_learner_maxDat_ref"),]
df1_8 <- rbind(df1_8, df_1[df_1$Grafikname == "clustbar_Learner_sub" & df_1$Beob_Strata %like% "BRCA%",])
df1_8 <- rbind(df1_8, df_1[df_1$Grafikname == "condotplot_dat_ord1" & df_1$Beob_Strata %like% "BRCA%",])
df1_8 <- rbind(df1_8, df_1[df_1$Grafikname == "heatm_dat_lea_sub" & df_1$Beob_Strata %like% "BRCA%",]) 
df1_8 <- rbind(df1_8, df_1[df_1$Grafikname == "matrixcha_dat_lea_sub" & df_1$Beob_Strata %like% "BRCA%",])

df1_9 <- df_1[df_1$Grafikname %in% c("comp_bars_learner_medianDat"),]
df1_9 <- rbind(df1_9, df_1[df_1$Grafikname == "clustbar_Learner_sub" & df_1$Beob_Strata %like% "KIRC%",])
df1_9 <- rbind(df1_9, df_1[df_1$Grafikname == "condotplot_dat_ord1" & df_1$Beob_Strata %like% "KIRC%",])
df1_9 <- rbind(df1_9, df_1[df_1$Grafikname == "heatm_dat_lea_sub" & df_1$Beob_Strata %like% "KIRC%",])
df1_9 <- rbind(df1_9, df_1[df_1$Grafikname == "matrixcha_dat_lea_sub" & df_1$Beob_Strata %like% "KIRC%",])

df1_10 <- df_1[df_1$Grafikname %in% c("comp_bars_learner_minDat"),]
df1_10 <- rbind(df1_10, df_1[df_1$Grafikname == "clustbar_Learner_sub" & df_1$Beob_Strata %like% "LAML%",])
df1_10 <- rbind(df1_10, df_1[df_1$Grafikname == "condotplot_dat_ord1" & df_1$Beob_Strata %like% "LAML%",])
df1_10 <- rbind(df1_10, df_1[df_1$Grafikname == "heatm_dat_lea_sub" & df_1$Beob_Strata %like% "LAML%",])
df1_10 <- rbind(df1_10, df_1[df_1$Grafikname == "matrixcha_dat_lea_sub" & df_1$Beob_Strata %like% "LAML%",])

df1_11 <- df_1[df_1$Grafikname %in% c("corr_loess", "corr_loess_ref", "corr_ps", "corr_ps_ref", "multdist_loess",
                                 "multdist_loess_ref", "multdist_ps", "multdist_ps_ref",
                                 "ranking_box_dataset", "ranking_box_dataset_approach"),]

### Matrixcha_dat_lea resp. matrixcha_dat_lea and associated record belongs to:
### 5 and 8 and 11 and 12
### and
### 6 and 9 and 11 and 12
### and
### 7 and 10 and 11 and 12

### 2.3.1 graphic 2----
#### 2.3.1.1 Strat 1----
df2_1 <- df_2[df_2$Grafikname %in% c("alle_max",
                                     "alle_max_approach",
                                     "learner_dataset_allmax",
                                     "approach_dataset_allemax",
                                     "learner_ph_allemax"),]
df2_1 <- rbind(df2_1, df_2[df_2$Grafikname == "approach_allemax" & df_2$Darstellungsart == "Connected Dotplot",])
df2_1 <- rbind(df2_1, df_2[df_2$Grafikname == "approach_dataset_allmax" & df_2$Darstellungsart == "Heatmap",])
df2_1 <- rbind(df2_1, df_2[df_2$Grafikname == "approach_dataset_allmax" & df_2$Darstellungsart == "Matrixchart",])
df2_1 <- rbind(df2_1, df_2[df_2$Grafikname == "approach_allemax" & df_2$Darstellungsart == "Bars",])

#### 2.3.1.2 Strat 2----
df2_2 <- df_2[df_2$Grafikname %in% c("alle_min",
                                     "alle_min_approach",
                                     "learner_dataset_allmin",
                                     "approach_dataset_allemin",
                                     "learner_ph_allemin"),]
df2_2 <- rbind(df2_2, df_2[df_2$Grafikname == "approach_allemin" & df_2$Darstellungsart == "Connected Dotplot",])
df2_2 <- rbind(df2_2, df_2[df_2$Grafikname == "approach_dataset_allmin" & df_2$Darstellungsart == "Heatmap",])
df2_2 <- rbind(df2_2, df_2[df_2$Grafikname == "approach_dataset_allmin" & df_2$Darstellungsart == "Matrixchart",])
df2_2 <- rbind(df2_2, df_2[df_2$Grafikname == "approach_allemin" & df_2$Darstellungsart == "Bars",])

#### 2.3.1.3 Strat 3----
df2_3 <- df_2[df_2$Grafikname %in% c("alle_median",
                                     "alle_median_approach",
                                     "learner_dataset_allmed",
                                     "approach_dataset_allemed",
                                     "learner_ph_allemed"),]
df2_3 <- rbind(df2_3, df_2[df_2$Grafikname == "approach_allemed" & df_2$Darstellungsart == "Connected Dotplot",])
df2_3 <- rbind(df2_3, df_2[df_2$Grafikname == "approach_dataset_allmed" & df_2$Darstellungsart == "Heatmap",])
df2_3 <- rbind(df2_3, df_2[df_2$Grafikname == "approach_dataset_allmed" & df_2$Darstellungsart == "Matrixchart",])
df2_3 <- rbind(df2_3, df_2[df_2$Grafikname == "approach_allemed" & df_2$Darstellungsart == "Bars",])

#### 2.3.1.4 Strat 4----
df2_4 <- df_2[df_2$Grafikname %in% c("cindex_max",
                                     "cindex_max_approach",
                                     "learner_dataset_cmax",
                                     "approach_dataset_cmax",
                                     "approach_cmax",
                                     "learner_ph_cmax"),]

#### 2.3.1.5 Strat 5----
df2_5 <- df_2[df_2$Grafikname %in% c("cindex_median",
                                     "cindex_med_approach",
                                     "learner_dataset_cmed",
                                     "approach_dataset_cmed",
                                     "approach_cmed",
                                     "learner_ph_cmed"),]

#### 2.3.1.6 Strat 6----
df2_6 <- df_2[df_2$Grafikname %in% c("cindex_min",
                                     "cindex_min_approach",
                                     "learner_dataset_cmin",
                                     "approach_dataset_cmin",
                                     "approach_cmin",
                                     "learner_ph_cmin"),]

#### 2.3.1.7 Strat 7----
df2_7 <- df_2[df_2$Grafikname %in% c("ibrier_max",
                                     "ibrier_max_approach",
                                     "learner_dataset_imax",
                                     "approach_dataset_imax",
                                     "approach_imax",
                                     "learner_ph_imax"),]

#### 2.3.1.8 Strat 8----
df2_8 <- df_2[df_2$Grafikname %in% c("ibrier_median",
                                     "ibrier_med_approach",
                                     "learner_dataset_imed",
                                     "approach_dataset_imed",
                                     "approach_imed",
                                     "learner_ph_imed"),]

#### 2.3.1.9 Strat 9----
df2_9 <- df_2[df_2$Grafikname %in% c("ibrier_min",
                                     "ibrier_min_approach",
                                     "learner_dataset_imin",
                                     "approach_dataset_imin",
                                     "approach_imin",
                                     "learner_ph_imin"),]

#### 2.3.1.10 Strat 10----
df2_10 <- df_2[df_2$Grafikname %in% c("dataset_maximum_allemasse",
                                     "dataset_approach_allemax",
                                     "cmax_imax",
                                     "ph_cmax_imax",
                                     "cph_cmax_imax"),]

#### 2.3.1.11 Strat 11----
df2_11 <- df_2[df_2$Grafikname %in% c("dataset_minimum_allemasse",
                                      "dataset_approach_allemin",
                                      "cmin_imin",
                                      "ph_cmin_imin",
                                      "cph_cmin_imin"),]

#### 2.3.1.12 Strat 12----
df2_12 <- df_2[df_2$Grafikname %in% c("dataset_median_allemasse",
                                      "dataset_approach_allemed",
                                      "cmed_imed",
                                      "ph_cmed_imed",
                                      "cph_cmin_imin"),]

#### 2.3.1.13 Strat 13----
df2_13 <- df_2[df_2$Grafikname %in% c("dataset_minimum_Cindex",
                                      "dataset_approach_cmin",
                                      "cmin",
                                      "cph_cmin",
                                      "ph_cmin"),]


#### 2.3.1.14 Strat 14----
df2_14 <- df_2[df_2$Grafikname %in% c("dataset_maximum_Cindex",
                                      "dataset_approach_cmax",
                                      "cmax",
                                      "cph_cmax",
                                      "ph_cmax"),]


#### 2.3.1.15 Strat 15----
df2_15 <- df_2[df_2$Grafikname %in% c("dataset_median_Cindex",
                                      "dataset_approach_cmed",
                                      "cmed",
                                      "cph_cmax",
                                      "ph_cmed"),]


#### 2.3.1.15 Strat 16----
df2_16 <- df_2[df_2$Grafikname %in% c("dataset_minimum_ibrier",
                                      "dataset_approach_imin",
                                      "imin",
                                      "cph_imin",
                                      "ph_imin"),]

#### 2.3.1.16 Strat 17----
df2_17 <- df_2[df_2$Grafikname %in% c("dataset_maximum_ibrier",
                                      "dataset_approach_imax",
                                      "imax",
                                      "cph_imax",
                                      "ph_imax"),]


#### 2.3.1.17 Strat 18----
df2_18 <- df_2[df_2$Grafikname %in% c("dataset_median_ibrier",
                                      "dataset_approach_imed",
                                      "imed",
                                      "cph_imed",
                                      "ph_imed"),]

#### 2.3.1.18 Strat 19----
df2_19 <- df_2[df_2$Grafikname %like% "dataset_maximum%" & df_2$y.Achse != "Spars",]
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname %like% "dataset_minimum%" & df_2$y.Achse != "Spars",])
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname %like% "dataset_median_%" & df_2$y.Achse != "Spars",])
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname %in% c("dataset") & df_2$y.Achse != "Spars",])
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname %like% "dataset_approach%" & df_2$Darstellungsart == "clusterec barchart"
                             & df_2$Grafikname != "dataset_approach_allemean" & df_2$y.Achse != "Spars",])
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname %like% "dataset_approach%" & df_2$Darstellungsart == "Heatmap"
                             & df_2$y.Achse != "Spars",])
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname %like% "approach_dataset%" & df_2$Darstellungsart == "Matrixchart"
                             & df_2$y.Achse != "Spars",])
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname == "dataset_learner" & df_2$Darstellungsart == "Matrixchart"
                             & df_2$y.Achse != "Spars",])
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname == "dataset_learner" & df_2$Darstellungsart == "Heatmap"
                             & df_2$y.Achse != "Spars",])


#### 2.3.1.19 Strat 20----
df2_20 <- df_2[df_2$Grafikname %in% c("cmax_imax",
                                      "cmax",
                                      "cmed",
                                      "cmed_imed",
                                      "cmin",
                                      "cmin_imin",
                                      "imax",
                                      "imed",
                                      "imin",
                                      "loess",
                                      "ps"),]

save(df_1, df_2, file = "Aufbereitete Daten.Rda")

save(df1_1, df1_2, df1_3, df1_4, df1_5, df1_6, df1_7, df1_8, df1_9, df1_10, df1_11,
     file = "Data1 sub.Rda")

save(df2_1, df2_2, df2_3, df2_4, df2_5, df2_6, df2_7, df2_8, df2_9, df2_10, df2_11, df2_12, df2_13, df2_14, df2_15, df2_16, df2_17,
     df2_18, df2_19, df2_20, file = "Data2 sub.Rda")

rm(df1_1, df1_2, df1_3, df1_4, df1_5, df1_6, df1_7, df1_8, df1_9, df1_10, df1_11,
   df2_1, df2_2, df2_3, df2_4, df2_5, df2_6, df2_7, df2_8, df2_9, df2_10, df2_11,
   df2_12, df2_13, df2_14, df2_15, df2_16, df2_17, df2_18, df2_19, df2_20)

### 2.3.2 Reference comparison----

ref1  <- df_2[df_2$Grafikname %in% c("cindex_max_approach", "cindex_med_approach", "cindex_min_approach",
                            "dataset_approach_cmaxmax", "dataset_approach_cmedmax", "dataset_approach_cmedmin", 
                            "dataset_approach_cminmax", "dataset_approach_cminmin", "dataset_approach_imaxmax", 
                            "dataset_approach_imaxmed", "dataset_approach_imaxmin", "dataset_approach_imedmax",
                            "dataset_approach_imedmed", "dataset_approach_imedmin", "dataset_approach_iminmax",
                            "dataset_approach_iminmed", "dataset_approach_iminmin", "ibrier_max_approach",
                            "ibrier_med_approach", "ibrier_min_approach") & df_2$y.Achse != "Spars",]

ref1  <- rbind(ref1, df_2[df_2$Grafikname %in% c("approach_dataset_cmax", "approach_dataset_cmed", "approach_dataset_cmin",
                                     "approach_dataset_imax", "approach_dataset_imed", "approach_dataset_imin")
              & df_2$y.Achse != "Spars",])

ref1 <- rbind(ref1, df_2[df_2$Grafikname %in% c("approach_cmax", "approach_cmed", "approach_cmin",
                                    "approach_imax", "approach_imed", "approach_imin")
             & df_2$y.Achse != "Spars",])

ref1 <- rbind(ref1, df_2[df_2$Grafikname %in% c("approach_dmed_imed", "approach_dmed_imax", "approach_dmed_imin",
                                                "approach_dmed_cmed", "approach_dmed_cmax", "approach_dmed_cmin",
                                                "approach_dmax_imed", "approach_dmax_imax", "approach_dmax_imin",
                                                "approach_dmax_cmed", "approach_dmax_cmax", "approach_dmax_cmin",
                                                "approach_dmin_imed", "approach_dmin_imax", "approach_dmin_imin",
                                                "approach_dmin_cmed", "approach_dmin_cmax", "approach_dmin_cmin")
                         & df_2$y.Achse != "Spars",])
ref1 <- rbind(ref1, df_2[df_2$Grafikname == "approach" & df_2$y.Achse != "Spars",])
ref1 <- rbind(ref1, df_2[df_2$Grafikname %in% c("loess", "ps", "cmax", "cmed", "cmin",
                                                "imax", "imed", "imin")
                         & df_2$y.Achse != "Spars",])


ref2  <- df_2[df_2$Grafikname %in% c("cindex_max", "cindex_median", "cindex_min",
                                     "dataset_max", "dataset_median", "dataset_min",
                                     "ibrier_max", "ibrier_median", "ibrier_min") & df_2$y.Achse != "Spars",]
ref2 <- rbind(ref2, df_2[df_2$Grafikname %in% c("learner_dataset_cmax", "learner_dataset_cmed", "learner_dataset_cmin",
                                                "learner_dataset_imax", "learner_dataset_imed", "learner_dataset_imin")
                         & df_2$y.Achse != "Spars",])
ref2 <- rbind(ref2, df_2[df_2$Grafikname %in% c("learner")
                         & df_2$y.Achse != "Spars",])
ref2 <- rbind(ref2, df_2[df_2$Grafikname %in% c("learner_dataset")
                         & df_2$y.Achse != "Spars",])
ref2 <- rbind(ref2, df_2[df_2$Grafikname %in% c("learner_imed", "learner_imax", "learner_imin",
                                                "learner_cmed", "learner_cmax", "learner_cmin",
                                                "learner_dmed", "learner_dmax", "learner_dmin")
                         & df_2$y.Achse != "Spars",])
ref2 <- rbind(ref2, df_2[df_2$Grafikname %in% c("km_cmax", "km_cmed", "km_cmin",
                                                "km_imax", "km_imed", "km_imin")
                         & df_2$y.Achse != "Spars",])


ref3  <- df_2[df_2$Grafikname %in% c("cindex_max", "cindex_median", "cindex_min",
                                     "dataset_max", "dataset_median", "dataset_min",
                                     "ibrier_max", "ibrier_median", "ibrier_min") & df_2$y.Achse != "Spars",]
ref3 <- rbind(ref3, df_2[df_2$Grafikname %in% c("learner_dataset_cmax", "learner_dataset_cmed", "learner_dataset_cmin",
                                                "learner_dataset_imax", "learner_dataset_imed", "learner_dataset_imin")
                         & df_2$y.Achse != "Spars",])
ref3 <- rbind(ref3, df_2[df_2$Grafikname %in% c("learner")
                         & df_2$y.Achse != "Spars",])
ref3 <- rbind(ref3, df_2[df_2$Grafikname %in% c("learner_dataset")
                         & df_2$y.Achse != "Spars",])
ref3 <- rbind(ref3, df_2[df_2$Grafikname %in% c("learner_imed", "learner_imax", "learner_imin",
                                                "learner_cmed", "learner_cmax", "learner_cmin",
                                                "learner_dmed", "learner_dmax", "learner_dmin")
                         & df_2$y.Achse != "Spars",])
ref3 <- rbind(ref3, df_2[df_2$Grafikname %in% c("ph_cmax", "ph_cmed", "ph_cmin",
                                                "ph_imax", "ph_imed", "ph_imin")
                         & df_2$y.Achse != "Spars",])
ref3 <- rbind(ref3, df_2[df_2$Grafikname %in% c("cph_cmax", "cph_cmed", "cph_cmin",
                                                "cph_imax", "cph_imed", "cph_imin")
                         & df_2$y.Achse != "Spars",])


ref1_cmax  <- ref1[ref1$Grafikname %in% c("cindex_max_approach", "cmax", "approach_dataset_cmax",
                            "approach_cmax"),]
ref1_cmed <- ref1[ref1$Grafikname %in% c("cindex_med_approach", "cmed", "approach_dataset_cmed",
                                         "approach_cmed"),]
ref1_cmin <- ref1[ref1$Grafikname %in% c("cindex_min_approach", "cmin", "approach_dataset_cmin",
                                         "approach_cmin"),]
ref1_imax <- ref1[ref1$Grafikname %in% c("ibrier_min_approach", "imin", "approach_dataset_imin",
                                      "approach_imin"),]
ref1_imed <- ref1[ref1$Grafikname %in% c("ibrier_med_approach", "imed", "approach_dataset_imed",
                                         "approach_imed"),]
ref1_imin <- ref1[ref1$Grafikname %in% c("ibrier_min_approach", "imin", "approach_dataset_imin",
                                         "approach_imin"),]

ref2_cmax <- ref2[ref2$Grafikname %in% c("cindex_max", "learner_dataset_cmax", "learner_cmax",
                                         "km_cmax"),] 
ref2_cmed <- ref2[ref2$Grafikname %in% c("cindex_median", "learner_dataset_cmed", "learner_cmed",
                                         "km_cmed"),]
ref2_cmin <- ref2[ref2$Grafikname %in% c("cindex_min", "learner_dataset_cmin", "learner_cmin",
                                         "km_cmin"),]
ref2_imax <- ref2[ref2$Grafikname %in% c("ibrier_max", "learner_dataset_imax", "learner_imin",
                                         "km_imin"),]
ref2_imed <- ref2[ref2$Grafikname %in% c("ibrier_median", "learner_dataset_imed", "learner_imed",
                                         "km_imed"),]
ref2_imin <- ref2[ref2$Grafikname %in% c("ibrier_min", "learner_dataset_imin", "learner_imin",
                                         "km_imin"),]

ref3_cmax <- ref3[ref3$Grafikname %in% c("cindex_max", "learner_dataset_cmax", "learner_cmax",
                                         "ph_cmax", "cph_cmax"),] 
ref3_cmed <- ref3[ref3$Grafikname %in% c("cindex_median", "learner_dataset_cmed", "learner_cmed",
                                         "ph_cmed", "cph_cmed"),]
ref3_cmin <- ref3[ref3$Grafikname %in% c("cindex_min", "learner_dataset_cmin", "learner_cmin",
                                         "ph_cmin", "cph_cmin"),]
ref3_imax <- ref3[ref3$Grafikname %in% c("ibrier_max", "learner_dataset_imax", "learner_imin",
                                         "ph_imin", "cph_imin"),]
ref3_imed <- ref3[ref3$Grafikname %in% c("ibrier_median", "learner_dataset_imed", "learner_imed",
                                         "ph_imed", "cph_imed"),]
ref3_imin <- ref3[ref3$Grafikname %in% c("ibrier_min", "learner_dataset_imin", "learner_imin",
                                         "ph_imin", "cph_imin"),]

save(ref1, ref1_cmax, ref1_cmed, ref1_cmin, ref1_imax, ref1_imed, ref1_imin,
     ref2, ref2_cmax, ref2_cmed, ref2_cmin, ref2_imax, ref2_imed, ref2_imin,
     ref3, ref3_cmax, ref3_cmed, ref3_cmin, ref3_imax, ref3_imed, ref3_imin,
     file = "Referenzdaten.Rda")

df2_19 <- df_2[df_2$Grafikname %like% "dataset_maximum%" & df_2$y.Achse != "Spars",]
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname %like% "dataset_minimum%" & df_2$y.Achse != "Spars",])
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname %like% "dataset_median_%" & df_2$y.Achse != "Spars",])
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname %in% c("dataset") & df_2$y.Achse != "Spars",])
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname %like% "dataset_approach%" & df_2$Darstellungsart == "clusterec barchart"
                             & df_2$Grafikname != "dataset_approach_allemean" & df_2$y.Achse != "Spars",])
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname %like% "dataset_approach%" & df_2$Darstellungsart == "Heatmap"
                             & df_2$y.Achse != "Spars",])
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname %like% "approach_dataset%" & df_2$Darstellungsart == "Matrixchart"
                             & df_2$y.Achse != "Spars",])
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname == "dataset_learner" & df_2$Darstellungsart == "Matrixchart"
                             & df_2$y.Achse != "Spars",])
df2_19 <- rbind(df2_19, df_2[df_2$Grafikname == "dataset_learner" & df_2$Darstellungsart == "Heatmap"
                             & df_2$y.Achse != "Spars",])

# 100. Testzone----
#table(df_1$Zsf)
#df_1$Form[df_1$Zsf == unique(df_1$Zsf)[2]]
#table(str_c(df_1$Steigungsrichtung, df_1$Variabilitaet, df_1$Form, sep = " "))

### Subsetting with stringr
#table(df_2$Zsf)
#library(stringr)

#test <- df_2[str_detect(df_2$Zsf, "minI") & str_detect(df_2$Zsf, "Approach") & df_2$y.Achse == "Ibrier",]

#table(test$Ref_Km_Al, useNA = "always")
#table(test$Ref_Coxph_Al, useNA = "always")
#table(test$Steigungsrichtung_Al, useNA = "always")
#table(test$Variabilitaet_Al, useNA = "always")
#table(test$Form_Al, useNA = "always")

#unique(df_2$Typ[!is.na(test)])

### Betrachten von Verteilungen
#test <- df_1[df_1$Variabilitaet != "stark" & df_1$Zsf == "Calc|Learner",]
#df_1$Form <- factor(df_1$Form)
#df_1$Steigung <- factor(df_1$Steigung, levels = c("schwach", "maessig", "stark"), ordered = T)
#t <- test %>% count(Form, Steigung, .drop = T)
#Gini(t$n)
#g <- NA
#d <- NA
#for(i in 1:length(unique(df_1$Zsf))){
#  d <- rbind(d, data.frame(df_1 %>% filter(Variabilitaet != "stark" & Zsf == unique(df_1$Zsf)[i]) %>% count(Form, Steigung), 
#                           Zsf = rep(unique(df_1$Zsf)[i], nrow(df_1 %>% filter(Variabilitaet != "stark" & Zsf == unique(df_1$Zsf)[i]) %>% count(Form, Steigung)))))
#  g[i] <- Gini(d$n)
#}

