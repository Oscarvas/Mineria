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
datos <- as.data.frame(original)

# incluyo mis variables objetivo
#datos$PcteNulos <- datos$Nulos / datos$VotosEmitidos * 100
#datos$PSOE_prop<-datos$PSOE/datos$VotosEmitidos*100

datos$PcteAbstenciones <- datos$Abstenciones / datos$Censo * 100

# cosas que pueden influir en los %nulos: densidad poblacional, partido, tipo de actividad economica del municipio

#datos$NuloSobreBlanco <- ifelse(datos$Nulos > datos$Blancos, 1, 0)

datos <- as.data.frame(datos[,-(2:12)])

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
# reducimos su dimension acorde a criterios geograficos
datos$CCAA<-car::recode(datos$CCAA, "c('Andalucía','Melilla','Ceuta','Canarias')='AndalucíaAfrica';c('ComValenciana','Murcia','Baleares')='EsteBalear';c('Navarra','PaísVasco','Rioja','Cantabria','Asturias')='CachopoVino'")

#nueva verificacion de los datos
summary(datos)

#aggregate(PcteNulos~CCAA, data = datos, mean)
#boxplot_cuantcuali(datos$PcteNulos,datos$CCAA,"Nulos")

# Tratamiento de datos atípicos
varObjCont<-datos$PcteAbstenciones
input<-as.data.frame(datos[,-c(1,25)])
row.names(input)<-datos$CodigoINE

# bucle para gestionar los datos atipicos
for (i in names(which(sapply(input, class)=="numeric"))){
  outliers(paste0("input$",i))
}

# se eliminan los outliers inferiores al 3% (max 2,4)

# gestion de datos ausentes:
input$prop_missings<-rowMeans(is.na(input))
summary(input$prop_missings)
# no hay ningún municipio con más del 50% de datos ausentes


# verificar nulos de cada variable
(prop_missingsVars<-colMeans(is.na(input)))
# no hay ninguna variable con más de un 5%

input[,as.vector(which(sapply(input, class)=="numeric"))]<-
  sapply(Filter(is.numeric, input),function(x) impute(x,"random"))
input[,as.vector(which(sapply(input, class)=="factor"))]<-
  sapply(Filter(is.factor, input),function(x) impute(x,"random"))
# Se cambia el tipo de factor a character al imputar, así que hay que corregirlo
input[,as.vector(which(sapply(input, class)=="character"))] <-
  lapply(input[,as.vector(which(sapply(input, class)=="character"))] , as.factor)


length(unique(input$prop_missings)) # hay 6 valores distintos
input$prop_missings<-as.factor(input$prop_missings)
freq(input$prop_missings)


input$prop_missings<-car::recode(input$prop_missings, "c(0.0869565217391304,0.130434782608696,0.173913043478261,0.434782608695652)='>0.08';c(0.0434782608695652)='0.04'")
freq(input$prop_missings)

summary(input)

saveRDS(data.frame(varObjCont,input),"eleccionesDep")
###################### evaluar resultados

boxplot_cuantcuali(varObjCont,input$PartidoCCAA,"varObjCont")
boxplot_cuantcuali(varObjCont,input$CCAA,"varObjCont")

dispersion(Filter(is.numeric, input),varObjCont)
corrplot.mixed(cor(data.frame(varObjCont,Filter(is.numeric, input)),
                   use="pairwise", method="pearson"), tl.pos = "lt",
               lower = "number", upper = "ellipse",lower.col = "black", number.cex = .8, number.digits = 1)

# estudiando las graficas tenemos:
# Existe relación positiva con las siguientes variables: Menores de 19, % universitario y personas inmueble
# relación negativa con: Mayores de 65 y Municipios con mayor explotación agricola

# Geograficamente: Menos abstenciones en castillaMancha y extremadura (menor al 25%)
# Mayor abstencion en Galicia y Madrid (sobre el 25%)
par(mar=c(12, 5.1, 4.1, 1.1)) #Para ajustar los márgenes del gráfico
input$aleatorio<-runif(nrow(input))
input$aleatorio2<-runif(nrow(input))
graficoVcramer(input,varObjCont)


saveRDS(data.frame(varObjCont,input),"eleccionesDep")
