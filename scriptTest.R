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
              prop_missings+UniversityPtge+UnemployMore40_Ptge+ServicesUnemploymentPtge+Explotaciones,data=data_train)
Rsq(modelo2,"varObjCont",data_train)
## [1] 0.2641553
Rsq(modelo2,"varObjCont",data_test)
## [1] 0.2348144
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
              Age_under19_Ptge:CCAA,data=data_train)
Rsq(modelo8,"varObjCont",data_train)
## [1] 0.3222164
Rsq(modelo8,"varObjCont",data_test)
## [1] 0.283913
importanciaVariables(modelo8) # comprobamos como se comportan los extremos poblacionales

modelos<-list(modelo1,modelo2,modelo3,modelo4,modelo5,modelo6,modelo7,modelo8)
sapply(modelos,function(x) x$rank) 
# [1] 37 21 31 27 24 25 33 25

sapply(modelos,function(x) Rsq(x,"varObjCont",data_train))
# [1] 0.3072121 0.2641553 0.3068870 0.3059955 0.3043884 0.3043988 0.3214676 0.3222164

sapply(modelos,function(x) Rsq(x,"varObjCont",data_test))
# [1] 0.2685207 0.2348144 0.2691425 0.2681380 0.2718368 0.2716129 0.2843576 0.2839130


# Diferencias de los modelos entre train y test
# [1] 0.0386914 0.0266090 0.0377445 0.0378575 0.0325516 0.0327859 0.0371100 0.0383034

# Teniendo en cuenta el valor del r2 y la varianza respecto al conjunto de datos de test, podemos determinar que el 
# mejor modelo manual es el 2, puesto que tiene la menor complejidad, estima un 26% de los datos y sobre todo
# es el que tiene la menor varianza de todos los modelos planteados y pore tanto es mas fiable

coef(modelo2)

##################### seleccion de variables y modelos de regresion
#install.packages('OneR')
library(OneR)
summary(input)
TransfCont<-Transf_Auto(Filter(is.numeric, input[,-c(25,26)]),varObjCont)
names(TransfCont)

# discretizacion
discCont<-droplevels(optbin(data.frame(Filter(is.numeric, input[,-c(25,26)]),
                                       bin(varObjCont,nbins=5,method = "content"))))[,
                                      -(ncol(Filter(is.numeric, input[,-c(25,26)]))+1)]
names(discCont)<-paste("disc", names(discCont), sep = "_")


apply(discCont,2,freq)

# consideramos la reagrupacion de aquellos valores inferiores al 4.2%
# disc_Age_over65_Ptge
#disc_UniversityPtge
#disc_Empresas
#disc_UnemployLess25_Ptge
#disc_UnemployMore40_Ptge
#disc_AgricultureUnemploymentPtge
#disc_IndustryUnemploymentPtge
#disc_AutonomosPtge

aggregate(varObjCont, by=list(discCont$disc_Age_over65_Ptge), mean)
discCont$disc_Age_over65_Ptge<-car::recode(discCont$disc_Age_over65_Ptge,
                                        "c('(29,30.1]','(30.1,33.9]')='(29,33.9]'")
freq(discCont$disc_Age_over65_Ptge)
##
freq(discCont$disc_UniversityPtge)
aggregate(varObjCont, by=list(discCont$disc_UniversityPtge), mean)
discCont$disc_UniversityPtge<-car::recode(discCont$disc_UniversityPtge,
                                           "c('(7.94,9.94]','(9.94,10]')='(7.94,10]'")
freq(discCont$disc_UniversityPtge)
###
freq(discCont$disc_Empresas)
aggregate(varObjCont, by=list(discCont$disc_Empresas), mean)
discCont$disc_Empresas<-car::recode(discCont$disc_Empresas,
                                          "c('(56.3,56.9]','(56.9,62.6]')='(56.3,62.6]'")
freq(discCont$disc_Empresas)
#########
freq(discCont$disc_UnemployLess25_Ptge)
aggregate(varObjCont, by=list(discCont$disc_UnemployLess25_Ptge), mean)
discCont$disc_UnemployLess25_Ptge<-car::recode(discCont$disc_UnemployLess25_Ptge,
                                    "c('(5.49,6.43]','(6.43,6.97]')='(5.49,6.97]'")
freq(discCont$disc_UnemployLess25_Ptge)
##########
freq(discCont$disc_UnemployMore40_Ptge)
aggregate(varObjCont, by=list(discCont$disc_UnemployMore40_Ptge), mean)
discCont$disc_UnemployMore40_Ptge<-car::recode(discCont$disc_UnemployMore40_Ptge,
                                               "c('(51.1,52.3]','(52.3,55.1]')='(51.1,55.1]'")
freq(discCont$disc_UnemployMore40_Ptge)
##########
freq(discCont$disc_AgricultureUnemploymentPtge)
aggregate(varObjCont, by=list(discCont$disc_AgricultureUnemploymentPtge), mean)
discCont$disc_AgricultureUnemploymentPtge<-car::recode(discCont$disc_AgricultureUnemploymentPtge,
                                               "c('(5.44,6.77]','(6.77,7.47]','(7.47,7.6]')='(5.44,7.6]'")
freq(discCont$disc_AgricultureUnemploymentPtge)
################
freq(discCont$disc_IndustryUnemploymentPtge)
aggregate(varObjCont, by=list(discCont$disc_IndustryUnemploymentPtge), mean)
discCont$disc_IndustryUnemploymentPtge<-car::recode(discCont$disc_IndustryUnemploymentPtge,
                                               "c('(6.54,8.74]','(8.74,9.65]')='(6.54,9.65]'")
freq(discCont$disc_IndustryUnemploymentPtge)
##############
freq(discCont$disc_AutonomosPtge)
aggregate(varObjCont, by=list(discCont$disc_AutonomosPtge), mean)
discCont$disc_AutonomosPtge<-car::recode(discCont$disc_AutonomosPtge,
                                               "c('(11,11.4]','(11.4,11.7]')='(11,11.7]'")
freq(discCont$disc_AutonomosPtge)


### Por último, unimos en un mismo dataFrame la variable objetivo y las variables input originales, transformadas
# y las discretizadas:
datos_todocont<-data.frame(varObjCont,input,TransfCont,discCont)
names(datos_todocont)

## seleccion de variables automaticas
set.seed(12345)
trainIndex <- createDataPartition(datos_todocont$varObjCont, p=0.8, list=FALSE)
data_train <- datos_todocont[trainIndex,]
data_test <- datos_todocont[-trainIndex,]


# recuperamos el mejor modelo manual obtenido antes (modelo2)
modeloManual<-lm(varObjCont~CCAA+Censo+UnemploymentPtge+ForeignersPtge+Age_under19_Ptge+Age_over65_Ptge+ConstructionUnemploymentPtge+
              prop_missings+UniversityPtge+UnemployMore40_Ptge+ServicesUnemploymentPtge+Explotaciones,data=data_train)

Rsq(modeloManual,"varObjCont",data_train)  # 0.2641553
Rsq(modeloManual,"varObjCont",data_test) # 0.2348144
modeloManual$rank # 22

# Selección de variables con las input originales (eliminar trace=F para observar el proceso de la selección):
null<-lm(varObjCont~1, data=data_train) #Modelo minimo
full<-lm(varObjCont~., data=data_train[,c(1:27)])

# MOdelo step
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both")
Rsq(modeloStepAIC,"varObjCont",data_test) # 0.2688144
modeloStepAIC$rank # 29

# Modelo backwards
modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward")
Rsq(modeloBackAIC,"varObjCont",data_test) # 0.2691425
modeloBackAIC$rank # 31

#### ambos modelos AIC son bastante similares

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",
                    k=log(nrow(data_train)))
Rsq(modeloStepBIC,"varObjCont",data_test) # 0.2718368
modeloStepBIC$rank # 24, mejora un poco al manual sin añadir mucha complejidad

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",
                    k=log(nrow(data_train)))
Rsq(modeloBackBIC,"varObjCont",data_test) # 0.2718368
modeloBackBIC$rank # 24

##### ambos modelos BIC son iguales

## Selección de variables con las input originales e interacciones:

fullInt<-lm(varObjCont~.^2,data=data_train[,c(1:27)])
#El backward puede ser muy lento (o incluso no funcionar) cuando hay muchos posibles efectos
# No lo realizo por abreviar, pero sería recomendable probarlo
modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",
                        trace=F)
# en mi máquina tarda unos 3-4 minutos
Rsq(modeloStepAIC_int,"varObjCont",data_test) # 0.3547828
modeloStepAIC_int$rank # 161 ------- estamos locos

##
modeloStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",
                        k=log(nrow(data_train))) # 45 segundos
Rsq(modeloStepBIC_int,"varObjCont",data_test) # 0.3389092
modeloStepBIC_int$rank # 46 .....

#Selección de variables con las input originales y transformadas:
fullT<-lm(varObjCont~., data=data_train[,c(1:46)])
modeloStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both"
                          ,trace=F)
Rsq(modeloStepAIC_trans,"varObjCont",data_test) # 0.3774476
modeloStepAIC_trans$rank # 36 ... nos acercamos a otros modelos manuales
##
modeloStepBIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both",
                          k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC_trans,"varObjCont",data_test) # 0.3736192
modeloStepBIC_trans$rank # 23 .... bastaaaante mejor al modelo manual elegido


## Selección de variables con las input originales, transformadas y discretizadas:
fulltodo<-lm(varObjCont~., data=data_train)
modeloStepAIC_todo<-step(null, scope=list(lower=null, upper=fulltodo), direction="both",trace=F)
Rsq(modeloStepAIC_todo,"varObjCont",data_test) # 0.384078
modeloStepAIC_todo$rank # 60... mucha complejidad

#
modeloStepBIC_todo<-step(null, scope=list(lower=null, upper=fulltodo), direction="both",
                         k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC_todo,"varObjCont",data_test) # 0.3830574
modeloStepBIC_todo$rank # 33


### Selección de variables con las input originales, transformadas, discretizadas e interacciones (pruebo sólo
#con BIC pues es más exigente con el número de parámetros y, por tanto, tarda menos en ejecutarse,
# pero podrían probarse ambos

fullIntT<-lm(varObjCont~.^2, data=data_train)
modeloStepBIC_todoInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both",
                            k=log(nrow(data_train)),trace=F) # 3-4 minutos de ejecucion
Rsq(modeloStepBIC_todoInt,"varObjCont",data_test) # 0.4043833
modeloStepBIC_todoInt$rank # 40

# Comparación de modelos de regresión lineal
modelos<-list(modeloManual,modeloStepAIC,modeloStepBIC,modeloStepAIC_int,modeloStepBIC_int,
              modeloStepAIC_trans,modeloStepBIC_trans,modeloStepAIC_todo,modeloStepBIC_todo,
              modeloStepBIC_todoInt) #incluir los modelos que se desee comparar

sapply(modelos,function(x) x$rank)
# [1]  22  29  24 161  46  36  23  60  33  40

sapply(modelos,function(x) Rsq(x,"varObjCont",data_test))
#  [1] 0.2348144 0.2688144 0.2718368 0.3547828 0.3389092 0.3774476 0.3736192 0.3840780 0.3830574 0.4043833

sapply(modelos,function(x) Rsq(x,"varObjCont",data_train))
#  [1] 0.2641553 0.3065001 0.3043884 0.4361305 0.3907899 0.4130105 0.4057227 0.4295213 0.4191222 0.4319629

xxx <- sapply(modelos,function(x) Rsq(x,"varObjCont",data_train))
yyy <- sapply(modelos,function(x) Rsq(x,"varObjCont",data_test))
zzz <- xxx - yyy
zzz # calculando la varianza
# [1] 0.02934091 0.03768572 0.03255160 0.08134771 0.05188068 0.03556284 0.03210342 0.04544329 0.03606476 0.02757961

### Finalmente, llevamos a cabo validación cruzada repetida para evaluar los modelos, tanto en sesgo como en varianza:
vcrTodosModelos<-list()
formulaModelos<-sapply(modelos,formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(formulaModelos[[i]]), data = data_train,
             method = "lm",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20)
  )
  vcrTodosModelos[[i]]<-vcr
}
names(vcrTodosModelos)<-paste0("Model",1:length(modelos),
                               "_",sapply(modelos,function(x) x$rank))
bwplot(resamples(vcrTodosModelos),metric=c("Rsquared"))

summary(resamples(vcrTodosModelos),metric=c("Rsquared"))
