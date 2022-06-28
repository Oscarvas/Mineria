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

##########################################################################
################################## Regresion lineal
##########################################################################
#install.packages("caret")
library(caret)
#library(car)

datos<-readRDS("eleccionesDep")

summary(datos)

# train-test
set.seed(12345)
trainIndex <- createDataPartition(datos$varObjCont, p=0.8, list=FALSE)
data_train <- datos[trainIndex,]
data_test <- datos[-trainIndex,]

#Construyo un modelo preliminar con todas las variables
modelo1<-lm(varObjCont~.,data=data_train)
summary(modelo1)

# evaluamos el modelo con la particion de prueba
Rsq(modelo1,"varObjCont",data_train) # 0.3072121

Rsq(modelo1,"varObjCont",data_test) # 0.2685207

# en el caso de que r2 valga 1, significa que la suma de errores es 0 y por tanto tenemos un modelo perfecto
# el modelo será mejor si el r2 del train y test se parecen, ya que tiene menos varianza
# par(mar=c(7, 9, 4.1, 2.1))
par(mar=c(3, 12, 4.1, 2.1))
importanciaVariables(modelo1)

modelo2<-lm(varObjCont~CCAA+Censo+UnemploymentPtge+ForeignersPtge+Age_under19_Ptge+Age_over65_Ptge+ConstructionUnemploymentPtge+
              prop_missings+UniversityPtge+UnemployMore40_Ptge+ServicesUnemploymentPtge,data=data_train)
Rsq(modelo2,"varObjCont",data_train)
## [1] 0.2465246
Rsq(modelo2,"varObjCont",data_test)
## [1] 0.2199156
importanciaVariables(modelo2)

modelo3<-lm(varObjCont~CCAA+Censo+Population+Age_under19_Ptge+Age_over65_Ptge+
              ForeignersPtge+UniversityPtge+Empresas+
              Densidad+PersonasInmueble+Explotaciones+
              PartidoCCAA+UnemploymentPtge+WomenUnemploymentPtge+UnemployLess25_Ptge+
              UnemployMore40_Ptge+AgricultureUnemploymentPtge+
              ConstructionUnemploymentPtge+ServicesUnemploymentPtge+
              prop_missings,data=data_train)
Rsq(modelo3,"varObjCont",data_train)
## [1] 0.306887
Rsq(modelo3,"varObjCont",data_test)
## [1] 0.2691425
importanciaVariables(modelo3)

# eliminadas modelo3: womanPopulation, autonomosPtge, IndustryUnemploymentPtge, aleatorio, aleatorio2, PobChange_pct


modelo4<-lm(varObjCont~CCAA+Censo+Population+Age_under19_Ptge+Age_over65_Ptge+
              ForeignersPtge++Empresas+
              Densidad+PersonasInmueble+Explotaciones+
              PartidoCCAA+UnemploymentPtge+UnemployLess25_Ptge+
              AgricultureUnemploymentPtge+
              ConstructionUnemploymentPtge+
              prop_missings,data=data_train)
Rsq(modelo4,"varObjCont",data_train)
## [1] 0.3059955
Rsq(modelo4,"varObjCont",data_test)
## [1] 0.268138
importanciaVariables(modelo4)

# elimino del m4 : ServicesUnemploymentPtge, UniversityPtge, UnemployMore40_Ptge, WomenUnemploymentPtge

# a partir del modelo 3 empiezo a ver que el sector agrícola empieza a cobrar más importancia

modelo5<-lm(varObjCont~CCAA+Censo+Population+Age_under19_Ptge+Age_over65_Ptge+
              ForeignersPtge++Empresas+
              Densidad+PersonasInmueble+Explotaciones+
              PartidoCCAA+UnemploymentPtge+
              AgricultureUnemploymentPtge+
              ConstructionUnemploymentPtge,data=data_train)
Rsq(modelo5,"varObjCont",data_train)
## [1] 0.3043884
Rsq(modelo5,"varObjCont",data_test)
## [1] 0.2718368
importanciaVariables(modelo5) # eliminada: UnemployLess25_Ptge, prop_missings

# introducimos interacciones
modelo6<-lm(varObjCont~CCAA+Censo+Population+Age_under19_Ptge+Age_over65_Ptge+
              ForeignersPtge++Empresas+
              Densidad+PersonasInmueble+Explotaciones+
              PartidoCCAA+UnemploymentPtge+
              AgricultureUnemploymentPtge+
              ConstructionUnemploymentPtge+
              AgricultureUnemploymentPtge:Explotaciones,data=data_train)
Rsq(modelo6,"varObjCont",data_train)
## [1] 0.3043884
Rsq(modelo6,"varObjCont",data_test)
## [1] 0.2718368
importanciaVariables(modelo6) # desempleo agricultura con actividad media agricultura (oximoron)

modelo7<-lm(varObjCont~CCAA+Censo+Population+Age_under19_Ptge+Age_over65_Ptge+
              ForeignersPtge++Empresas+
              Densidad+PersonasInmueble+Explotaciones+
              PartidoCCAA+UnemploymentPtge+
              AgricultureUnemploymentPtge+
              ConstructionUnemploymentPtge+
              CCAA:Explotaciones,data=data_train)
Rsq(modelo7,"varObjCont",data_train)
## [1] 0.3214676
Rsq(modelo7,"varObjCont",data_test)
## [1] 0.2843576
importanciaVariables(modelo7) # comprobamos que las CCAA con más actividad agrícola mejoran el modelo

modelo8<-lm(varObjCont~CCAA+Censo+Population+Age_under19_Ptge+Age_over65_Ptge+
              ForeignersPtge++Empresas+
              Densidad+PersonasInmueble+Explotaciones+
              PartidoCCAA+UnemploymentPtge+
              AgricultureUnemploymentPtge+
              ConstructionUnemploymentPtge+
              Age_under19_Ptge:Age_over65_Ptge,data=data_train)
Rsq(modelo8,"varObjCont",data_train)
## [1] 0.3274342
Rsq(modelo8,"varObjCont",data_test)
## [1] 0.2898215
importanciaVariables(modelo8) # comprobamos como se comportan los extremos poblacionales

modelos<-list(modelo1,modelo2,modelo3,modelo4,modelo5,modelo6,modelo7,modelo8)
sapply(modelos,function(x) x$rank) 
# [1] 37 21 31 27 24 25 33 25

sapply(modelos,function(x) Rsq(x,"varObjCont",data_train))
# [1] 0.3072121 0.2465246 0.3068870 0.3059955 0.3043884 0.3043988 0.3214676 0.3274342

sapply(modelos,function(x) Rsq(x,"varObjCont",data_test))
# [1] 0.2685207 0.2199156 0.2691425 0.2681380 0.2718368 0.2716129 0.2843576 0.2898215


# Diferencias de los modelos entre train y test
# [1] 0.0386914 0.0266090 0.0377445 0.0378575 0.0325516 0.0327859 0.0371100 0.0376127

# Aunque los resultados arrojan el modelo2 como más simple, elijo el modelo5 por presentar un r2 algo más elevado
# y tener la siguiente varianza reducida

coef(modelo5)
