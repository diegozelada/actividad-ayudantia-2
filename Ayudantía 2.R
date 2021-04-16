library(quanteda)
library(dplyr)
library(tidyverse)
library(utf8)
library(ggplot2)


setwd("D:/Universidad/Minería de datos 2/actividad-ayudantia-2")
primer_tiempo2020 <- read_csv("Primer_Tiempo2020.csv", col_names = TRUE)

#str(primer_tiempo2020)
#attach(primer_tiempo2020)
summary(primer_tiempo2020)

primer_tiempo2020

primer_tiempo2020 <- primer_tiempo2020[,!(colnames(primer_tiempo2020) %in% c("id_partido", "fasepartido", "local", "tiempo","formationUsed", "torneo"))]
primer_tiempo2020


fh2020 <- primer_tiempo2020[order(primer_tiempo2020$accuratePass, decreasing = TRUE),]
fh2020


fh2020_pases = fh2020[,colnames(primer_tiempo2020) %in% c("equipo", "partido", "accuratePass", "totalPass", "precision_pases")]
fh2020_pases = fh2020_pases[order(fh2020_pases$precision_pases, decreasing = TRUE),]

fh2020_pases

fh2020_tiros <- NULL

fh2020_tiros = fh2020[,colnames(primer_tiempo2020) %in% c("equipo", "partido", "goals", "ontargetScoringAtt", "totalScoringAtt", "blockedScoringAtt", "shotOffTarget", "precision_tiros")]
fh2020_tiros = fh2020_tiros[order(fh2020_tiros$goals, decreasing = TRUE),]
fh2020_tiros


Uchile <- filter(primer_tiempo2020, equipo == "Universidad de Chile")
Uchile_tiros <- filter(fh2020_tiros, equipo == "Universidad de Chile")
Uchile_pases <- filter(fh2020_pases, equipo == "Universidad de Chile")


Uchile_pases <- huachipato_pases[,!(colnames(Uchile_pases) %in% c("equipo"))] 

Promedios_Pas <- c("Promedio Pases",mean(Uchile_pases$accuratePass),mean(Uchile_pases$totalPass),mean(Uchile_pases$precision_pases))
huachipato_pases <- rbind(huachipato_pases, Promedios_Pas)

Max_Pas <- c("Max Pases",max(huachipato_pases$accuratePass),max(huachipato_pases$totalPass),max(huachipato_pases$precision_pases))
huachipato_pases <- rbind(huachipato_pases, Max_Pas)

Min_Pas <- c("Min Pases",min(huachipato_pases$accuratePass),min(huachipato_pases$totalPass),min(huachipato_pases$precision_pases))
huachipato_pases <- rbind(huachipato_pases, Min_Pas)

huachipato_pases

pases_hua <- huachipato$accuratePass
huachipato2 <- huachipato[order(huachipato$accuratePass, decreasing = FALSE),]

#dotchart(huachipato$totalPass, labels = huachipato$partido, cex=0.5, xlab = "Pases", ylab = "Partido")

dotchart(huachipato$totalPass, labels = utf8_encode(huachipato$partido), cex=0.5, xlab = "Pases", ylab = "Partido")


dotchart(huachipato$accuratePass, labels = utf8_encode(huachipato$partido), cex=0.5, xlab = "Pases", ylab = "Partido")



dotchart(huachipato2$totalPass, labels = utf8_encode(huachipato$partido), cex=0.5, xlab = "Pases", ylab = "Partido")


dotchart(huachipato2$totalPass, labels = utf8_encode(huachipato$partido), main="Pases Acertados Huachipato", pch = 16, col=c("darkblue","dodgerblue"),lcolor="gray90", cex=0.8, xlab = "Pases", ylab = "Partido", cex.main=2,cex.lab=1.5)



texto <- primer_tiempo2020$partido
texto <- char_tolower(texto)
texto <- iconv(texto, to = "ASCII//TRANSLIT")

a <- dfm(texto, remove = c(stopwords("es"), "vs", "Universidad"))
dim(a)