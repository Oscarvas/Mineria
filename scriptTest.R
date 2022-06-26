# install.packages(c("questionr","car","Hmisc","corrplot","ggplot","tidyr"))
# install.packages("readxl")
source("FuncionesMineriaAida.R")
library(questionr)
library(car)
library(Hmisc)
library(readxl)
library(corrplot)
library(ggplot2)
library(tidyr)

# Variable de intervalo: % de nulos
# Variable binaria: 1 - Hubo más votos nulos que blancos; 0 - EOC

original <- read_excel("datosEleccionesEuropeas2019.xlsx")

# rechazo el primer conjunto de variables que no voy a usar
datos <- as.data.frame(original[,-(6:12)])

# incluyo mis variables objetivo
datos$PcteNulos <- datos$Nulos / datos$VotosEmitidos * 100
datos$PSOE_prop<-datos$PSOE/datos$VotosEmitidos*100

# cosas que pueden influir en los %nulos: densidad poblacional, partido, tipo de actividad economica del municipio

#datos$NuloSobreBlanco <- ifelse(datos$Nulos > datos$Blancos, 1, 0)

datos <- as.data.frame(datos[,-(2:5)])

# primer estudio de las variables
str(datos)

# transformar categóricas a factor
datos[,c(2,19)] <- lapply(datos[,c(2,19)], as.factor)

# las numéricas han de tener mas de 10 valores diferentes
sapply(Filter(is.numeric, datos),function(x) length(unique(x)))

# análisis descriptivo
summary(datos)

#variables que presentan errores posiblemente: 72% de mujeres ? media de 44k empresas por cada 1k habitantes ?
# Existen 22 municipios con medias superiores a 1k empresas por cada 1k habitantes, saldrán luego en los outliers


# Rechazo varibales con más del 50% de NA's (4055): Industria, Construcción, ComercTTE, Servicios
datos <- as.data.frame(datos[,-(11:14)])

# 300% de unemployment ? 359% de autonomos en 1 municipio
datos$UnemploymentPtge<-replace(datos$UnemploymentPtge, which((datos$UnemploymentPtge < 0)|(datos$UnemploymentPtge>100)), NA)
datos$AutonomosPtge<-replace(datos$AutonomosPtge, which((datos$AutonomosPtge < 0)|(datos$AutonomosPtge>100)), NA)

#estudio de la variable de CCAA
freq(datos$CCAA)

#nueva verificacion de los datos
summary(datos)

aggregate(PcteNulos~CCAA, data = datos, mean)
boxplot_cuantcuali(datos$PcteNulos,datos$CCAA,"Nulos")

datos$CCAA<-car::recode(datos$CCAA, "c('Andalucía','Melilla','Ceuta','Canarias')='AndalucíaAfrica';c('ComValenciana','Murcia','Baleares')='EsteBalear';c('Navarra','PaísVasco','Rioja','Cantabria','Asturias')='CachopoVino'")
