
# Inspirado en: https://datascienceplus.com/identify-describe-plot-and-removing-the-outliers-from-the-dataset/
outliers <- function(tt) {
  var_name<-eval(parse(text = tt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  par(mfrow=c(1, 2), oma=c(0,0,3,0))
  hist(var_name, main="All observations", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  na2 <- sum(is.na(var_name))
  hist(var_name, main=c("Outliers removed",paste0(round((na2 - na1) / tot*100,2),"%")), xlab=NA, ylab=NA)
  title(paste0("Outlier Check for ",tt), outer=TRUE)
  message("Outliers identified in ", tt, ": ", na2 - na1, " from ", tot, " observations")
  response <- readline(prompt="Do you want to replace outliers with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    eval(parse(text = paste0(tt, " <<- invisible(var_name)")))
    message("Outliers successfully removed", "\n")
  } else{
    message("Nothing changed", "\n")
  }
  par(mfrow=c(1, 1), oma=c(0,0,0,0))
}
# Calcula el V de Cramer
Vcramer<-function(v,target){
  if (is.numeric(v)){
    v<-cut(v,breaks=unique(quantile(v,probs = seq(0,1,0.2))),include.lowest=T)
  }
  if (is.numeric(target)){
    target<-cut(target,breaks=unique(quantile(target,probs = seq(0,1,0.2))),include.lowest=T)
  }
  cramer.v(table(v,target))
}

# Gráfico con el V de cramer de todas las variables input para saber su importancia
graficoVcramer<-function(matriz, target){
  salidaVcramer<-sapply(matriz,function(x) Vcramer(x,target))
  bb<-barplot(sort(salidaVcramer,decreasing =T),las=2,ylim=c(0,1))
  text(bb,sort(salidaVcramer,decreasing =T)+0.1,labels = round(sort(salidaVcramer,decreasing =T),2),srt=90)
}

# Diagrama de cajas para las variables cuantitativas y variable objetivo binaria
boxplot_cuantcuali<-function(cuant,cuali,nombreEje){
  dataaux<-data.frame(cuant,cuali)
  ggplot(dataaux,aes(x=cuali,y=cuant))+
    geom_boxplot(aes(fill=cuali), notch=FALSE) +
    stat_summary(fun=mean, geom="point", shape=8) +
    labs(x = "", y = nombreEje) + theme(legend.position = "none")
}

# Gráfico mosaico para las variables cualitativas y variable objetivo binaria
mosaico<-function(var,target,nombreEje){
  ds <- table(var, target)
  ord <- order(apply(ds, 1, sum), decreasing=TRUE)
  mosaicplot(ds[ord,], color=c("darkturquoise","indianred1"), las=2, main="",xlab=nombreEje)
}

#Gráficos de dispersión de la objetivo con las input numéricas
dispersion<-function(matriz, target,propDatos){
  dataaux.tidy <- gather(data.frame(matriz,target), Vble, Valor, -target)
  ggplot(dataaux.tidy,aes(x=Valor,y=target))+geom_point(shape = ".")+ facet_wrap(. ~ Vble, scales = "free_x")+geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se=FALSE)+
    labs(x = "", y = "")
}


# Busca la transformación de variables input de intervalo que maximiza la correlación con la objetivo continua
mejorTransfCorr<-function(vv,target){
  vv<-scale(vv)
  vv<-vv+abs(min(vv,na.rm=T))*1.0001
  posiblesTransf<-data.frame(x=vv,log_=log(vv),exp_=exp(vv),sqr_=vv^2,sqrt_=sqrt(vv),cuarta_=vv^4,raiz4_=vv^(1/4),inv_=1/vv)
  return(list(colnames(posiblesTransf)[which.max(abs(cor(target,posiblesTransf, use="complete.obs")))],posiblesTransf[,which.max(abs(cor(target,posiblesTransf, use="complete.obs")))]))
}

# Busca la transformación de variables input de intervalo que maximiza la Verosimilitud de la reg. logística
mejorTransfLogistica<-function(vv,target){
  vv<-scale(vv)
  vv<-vv+abs(min(vv,na.rm=T))*1.0001
  posiblesTransf<-data.frame(x=vv,log_=log(vv),exp_=exp(vv),sqr_=vv^2,sqrt_=sqrt(vv),cuarta_=vv^4,raiz4_=vv^(1/4),inv_=1/vv)
  aux<-apply(posiblesTransf,2,function(x) glm(target~x,family=binomial)$deviance)
  return(list(colnames(posiblesTransf)[which.min(aux)],posiblesTransf[,which.min(aux)]))
}

# Detecta el tipo de variable objetivo y busca la mejor transformación de las variables input continuas automáticamente
Transf_Auto<-function(matriz,target){
  if (is.numeric(target)){
    aux<-data.frame(apply(matriz,2,function(x) mejorTransfCorr(x,target)[[2]]))
    aux2<-apply(matriz,2,function(x) mejorTransfCorr(x,target)[[1]])
  } else {
    aux<-data.frame(apply(matriz,2,function(x) mejorTransfLogistica(x,target)[[2]]))
    aux2<-apply(matriz,2,function(x) mejorTransfLogistica(x,target)[[1]])   
  }
  if (any(aux2=="x")){
    aux<-aux[,-which(aux2=="x")]
    names(aux)<-paste0(aux2[-which(aux2=="x")],names(aux2[-which(aux2=="x")]))
  } else {
    names(aux)<-paste0(aux2,names(aux2))
  }
  return(aux)
}

#Para evaluar el R2 en regr. lineal en cualquier particion (se necesita la libreria caret)
Rsq<-function(modelo,varObj,datos){
    R2(predict(modelo,datos),datos[,varObj])
}

#Procesa la información de la función Anova(), transforma las SS tipo III en R2 y lo representa graficamente
importanciaVariables<-function(modelo){
  a<-as.matrix(Anova(modelo,type = "III"))
  aa<-summary(modelo)
  cc<-sort(a[-c(1,nrow(a)),1]/(a[nrow(a),1]/(1-aa$r.squared)),decreasing =T)
  bb<-barplot(cc,horiz=T,las=2,main="Importancia de las variables (R2)",xlim=c(0,max(cc)*1.2))
  text(lapply(cc,function(x) x+max(cc)*0.1),bb,labels = round(cc,4))
}


#Para evaluar el pseudo-R2 en regr. logística en cualquier conjunto de datos
pseudoR2<-function(modelo,dd,nombreVar){
  pred.out.link <- predict(modelo, dd, type = "response")
  mod.out.null <- glm(as.formula(paste(nombreVar,"~1")), family = binomial, data = dd)
  pred.out.linkN <- predict(mod.out.null, dd, type = "response")
  1-sum((dd[,nombreVar]==1)*log(pred.out.link+1e-300)+log(1-pred.out.link+1e-300)*(1-(dd[,nombreVar]==1)))/
    sum((dd[,nombreVar]==1)*log(pred.out.linkN)+log(1 -pred.out.linkN)*(1-(dd[,nombreVar]==1)))
}

#Gráfico con la importancia de las variables en regr. logística
importanciaVariablesLog<-function(modelo){
  a<-as.matrix(Anova(modelo,type = "III"))
  aa<-summary(modelo)
  cc<-sort((a[,1])/(modelo$null.deviance),decreasing =T)
  bb<-barplot(cc,horiz=T,las=2,main="Importancia de las variables (Pseudo-R2)",xlim=c(0,max(cc)*1.2))
  text(lapply(cc,function(x) x+max(cc)*0.1),bb,labels = round(cc,4))
}

#Calcula medidas de calidad para un punto de corte dado
sensEspCorte<-function(modelo,dd,nombreVar,ptoCorte,evento){
  probs <-predict(modelo,newdata=dd,type="response")
  cm<-confusionMatrix(data=factor(ifelse(probs>=ptoCorte,1,0)), reference=dd[,nombreVar],positive=evento)
  c(cm$overall[1],cm$byClass[1:2])
} 


