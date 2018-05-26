
#=====================================================================================================================
#  Obtengo Resultados Promedios
#=====================================================================================================================

dirRaiz<-"C:/compendioResultados/resultados"
listaArchivosResultados<-list.files(dirRaiz, pattern=NULL, all.files=FALSE,full.names=FALSE,recursive = TRUE)
library(readr)

#creo contenedor
resultados <-
  data.frame(
    escenario =character(), 
    nrofold = numeric(), 
    datasetSel = character(), 
    metClasificacion = character(),
    missingclass = character(),
    acuJer = numeric(),
    opcion = character(),
    topdownType = character(),
    
    #Guardo Tiempos en Minutos
    tiempoEntrenaNuevoCla = numeric(),
    tiempoReentrena = numeric(),
    tiempoReentrenaCreaArbol= numeric(),
    
    stringsAsFactors = FALSE
  )



for(i in listaArchivosResultados){
  datosExtraidos <- read_csv(paste("C:/compendioResultados/resultados/",i,sep = ""),col_types = cols(X = col_skip()))
  datosExtraidos <-datosExtraidos[,-1]
  resultados<- rbind(resultados,datosExtraidos)
}

#Elimino los registros duplicados
#Creo un id=escenario + nrofold + datasetSel + metClasificacion + missingclass
resultados["Identificador"]<-paste(resultados$escenario,"_",resultados$nrofold,"_",resultados$datasetSel,"_",resultados$metClasificacion,"_",resultados$missingclass,"_",resultados$opcion,sep = "")
resultados<-resultados[!duplicated(resultados["Identificador"]),]

#Quito columnas inservibles:
resultados <- resultados[,(!names(resultados) %in% c("topdownType"))]
resultados <- resultados[,(!names(resultados) %in% c("Identificador"))]

#Guardo los datos de los experimentos en un archivo unico
write.csv(resultados, file = "C:/compendioResultados/Resumenes/resultadosCompletos.csv")



#Obtengo Promedios x Missing Class
promediosXMissingClass <- aggregate(x = resultados[c("acuJer", 
                                    "tiempoEntrenaNuevoCla",
                                    "tiempoReentrena",
                                    "tiempoReentrenaCreaArbol")],  
                       by = resultados[c("escenario", "datasetSel","metClasificacion","missingclass","opcion")], 
                       FUN = function(valoresFilas){
                         round(mean(pmax(valoresFilas, 0)),digits = 3)
                       })

#Cambio el tiempo a minutos
# promediosXMissingClass$tiempoEntrenaNuevoCla<-promediosXMissingClass$tiempoEntrenaNuevoCla/60 #Tiempo en Minutos
# promediosXMissingClass$tiempoReentrena<-promediosXMissingClass$tiempoReentrena/60 #Tiempo en Minutos
# promediosXMissingClass$tiempoReentrenaCreaArbol<-promediosXMissingClass$tiempoReentrenaCreaArbol/60 #Tiempo en Minutos

colnames(promediosXMissingClass)[colnames(promediosXMissingClass)=="tiempoEntrenaNuevoCla"] <- "tiempoEntrenaNuevoCla(seg)"
colnames(promediosXMissingClass)[colnames(promediosXMissingClass)=="tiempoReentrena"] <- "tiempoReentrenaDesdeCero(seg)"
colnames(promediosXMissingClass)[colnames(promediosXMissingClass)=="tiempoReentrenaCreaArbol"] <- "tiempoReentrenaCreaArbol(seg)"


promediosXMissingClass$opcion<-factor(promediosXMissingClass$opcion)
summary(promediosXMissingClass)


#Obtengo Promedios
promediosXDataSet <- aggregate(x = resultados[c("acuJer", 
                                                     "tiempoEntrenaNuevoCla",
                                                     "tiempoReentrena",
                                                     "tiempoReentrenaCreaArbol")],  
                                    by = resultados[c("escenario", "datasetSel","metClasificacion","opcion")], 
                                    FUN = function(valoresFilas){
                                      round(mean(pmax(valoresFilas, 0)),digits = 3)
                                    })

#Cambio el tiempo a minutos
# promediosXDataSet$tiempoEntrenaNuevoCla<-promediosXDataSet$tiempoEntrenaNuevoCla/60 #Tiempo en Minutos
# promediosXDataSet$tiempoReentrena<-promediosXDataSet$tiempoReentrena/60 #Tiempo en Minutos
# promediosXDataSet$tiempoReentrenaCreaArbol<-promediosXDataSet$tiempoReentrenaCreaArbol/60 #Tiempo en Minutos

colnames(promediosXDataSet)[colnames(promediosXDataSet)=="tiempoEntrenaNuevoCla"] <- "tiempoEntrenaNuevoCla(seg)"
colnames(promediosXDataSet)[colnames(promediosXDataSet)=="tiempoReentrena"] <- "tiempoReentrenaDesdeCero(seg)"
colnames(promediosXDataSet)[colnames(promediosXDataSet)=="tiempoReentrenaCreaArbol"] <- "tiempoReentrenaCreaArbol(seg)"





write.csv(promediosXDataSet, file = "C:/compendioResultados/Resumenes/promediosXDataSet.csv")



#Necesito agregar a la columna el porcentaje de instancias por dataframe
listdatasets<-unique(promediosXMissingClass$datasetSel)
#Creo la columna: (con valores por defecto=0)
promediosXMissingClass["porcentaje"]<-c(rep(0,nrow(promediosXMissingClass)))

dirRaiz<-"C:/clasificadoresJerarquicosAddClass"
setwd(dirRaiz)


for(dsetTemporal in listdatasets){
  source('preprocesamiento_accesoDataSet.R')
  data <- cargarDatos(nombreDataSet = dsetTemporal)
  
  #Obtengo Porcentajes por etiqueta
  etiquetasConteos<-table(data$claseAPredecir)
  etiquetas<-names(etiquetasConteos)
  nivelesCont<-etiquetasConteos/sum(as.numeric(etiquetasConteos))
  
  for(contTemp in 1:length(etiquetas)){
    ubicaciones<-which(promediosXMissingClass$missingclass==etiquetas[contTemp]&promediosXMissingClass$datasetSel==dsetTemporal)
    #Asigno este porcentaje a cada registro de experimento
    promediosXMissingClass$porcentaje[ubicaciones]<-as.numeric(nivelesCont[contTemp])
  }
  
}

#----------------------------------------------------------------------------------------------------
#Graficamos y analizamos la informacion de los resultados
dbEsc1<-promediosXMissingClass[promediosXMissingClass$escenario=='Escenario_1',]
dbEsc2<-promediosXMissingClass[promediosXMissingClass$escenario=='Escenario_2',]

dbEsc1RA<-promediosXMissingClass[promediosXMissingClass$escenario=='Escenario_1' & promediosXMissingClass$opcion=='Reframing' ,]
dbEsc1TA<-promediosXMissingClass[promediosXMissingClass$escenario=='Escenario_1' & promediosXMissingClass$opcion=='Retraining' ,]

dbEsc2RA<-promediosXMissingClass[promediosXMissingClass$escenario=='Escenario_2' & promediosXMissingClass$opcion=='Reframing' ,]
dbEsc2TA<-promediosXMissingClass[promediosXMissingClass$escenario=='Escenario_2' & promediosXMissingClass$opcion=='Retraining' ,]

t.test(dbEsc1RA$acuJer,dbEsc1TA$acuJer)



dbEsc2Refrm<-promediosXMissingClass[promediosXMissingClass$escenario=='Escenario_2'&promediosXMissingClass$opcion=="Reframing",]

library(ggplot2)
library(ggExtra)

ggplot( dbEsc2, aes(x=porcentaje , y=acuJer, color=escenario)) + geom_point(size=4)

ggplot(dbEsc2, aes(porcentaje, acuJer, size = acuJer)) +
  theme_bw()+
  geom_point(alpha=0.1)
  
ggplot(dbEsc2, aes(porcentaje, acuJer,color=missingclass)) +
  geom_point(alpha=0.5)


dbTextureEsc2<-promediosXMissingClass[promediosXMissingClass$escenario=='Escenario_2' & promediosXMissingClass$datasetSel=="Frogs",]
ggplot(dbTextureEsc2, aes(porcentaje, acuJer,color=missingclass)) +
  geom_point(alpha=0.5)+  geom_jitter(width = .01, size=1)

dbEsc2$opcion
g <- ggplot(dbEsc2, aes(porcentaje,acuJer,color=opcion)) + 
  geom_count() + 
  geom_smooth(method="lm", se=F)
  

ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")

library(corrplot)
tempdatanumeric<-dbEsc2Refrm
tempdatanumeric<-tempdatanumeric[-1]
tempdatanumeric<-tempdatanumeric[-1]
tempdatanumeric<-tempdatanumeric[-1]
tempdatanumeric<-tempdatanumeric[-1]
tempdatanumeric<-tempdatanumeric[-1]
cor.test(tempdatanumeric$acuJer,tempdatanumeric$porcentaje)
#0,20 correlacion mediobaja
corrplot(cor(tempdatanumeric))
#---------------------------------------------------------------------------------------------------


write.csv(promediosXMissingClass, file = "C:/compendioResultados/Resumenes/promediosXMissingClass.csv")

#=====================================================================================================================
#  Obtengo Resultados modelo Base
#=====================================================================================================================

dirRaiz<-"C:/compendioResultados/resultados_O"
listaArchivosResultados<-list.files(dirRaiz, pattern=NULL, all.files=FALSE,full.names=FALSE,recursive = TRUE)
library(readr)

#creo contenedor
resultados <-
  data.frame(
    escenario =character(), 
    nrofold = numeric(), 
    datasetSel = character(), 
    metClasificacion = character(),
    missingclass = character(),
    acuJer = numeric(),
    opcion = character(),
    topdownType = character(),
    stringsAsFactors = FALSE
  )



for(i in listaArchivosResultados){
  datosExtraidos <- read_csv(paste("C:/compendioResultados/resultados_O/",i,sep = ""),col_types = cols(X = col_skip()))
  datosExtraidos <-datosExtraidos[,-1]
  resultados<- rbind(resultados,datosExtraidos)
}

#Elimino los registros duplicados
#Creo un id=escenario + nrofold + datasetSel + metClasificacion + missingclass
resultados["Identificador"]<-paste(resultados$escenario,"_",resultados$nrofold,"_",resultados$datasetSel,"_",resultados$metClasificacion,"_",resultados$missingclass,"_",resultados$opcion,sep = "")
resultados<-resultados[!duplicated(resultados["Identificador"]),]

#Quito columnas inservibles:
resultados <- resultados[,(!names(resultados) %in% c("topdownType"))]
resultados <- resultados[,(!names(resultados) %in% c("Identificador"))]


#Guardo los datos de los experimentos en un archivo unico
write.csv(resultados, file = "C:/compendioResultados/Resumenes/resultadosBaseCompletos.csv")



#Obtengo Promedios x Missing Class
promediosXMissingClass <- aggregate(x = resultados[c("acuJer")],  
                                    by = resultados[c("escenario", "datasetSel","metClasificacion","missingclass","opcion")], 
                                    FUN = function(valoresFilas){
                                      round(mean(pmax(valoresFilas, 0)),digits = 3)
                                    })

write.csv(promediosXMissingClass, file = "C:/compendioResultados/Resumenes/promediosXMissingClass_O.csv")

#Obtengo Promedios
promediosXDataSet <- aggregate(x = resultados[c("acuJer")],  
                               by = resultados[c("escenario", "datasetSel","metClasificacion","opcion")], 
                               FUN = function(valoresFilas){
                                 round(mean(pmax(valoresFilas, 0)),digits = 3)
                               })

write.csv(promediosXDataSet, file = "C:/compendioResultados/Resumenes/promediosXDataSet_O.csv")




#=====================================================================================================================
#  Obtengo Estadisticos Promedios Experimentos
#=====================================================================================================================


dirRaiz<-"C:/compendioResultados/estadisticos"
listaArchivosEstadisticos<-list.files(dirRaiz, pattern=NULL, all.files=FALSE,full.names=FALSE,recursive = TRUE)

#creo contenedor
estadisticasTiempos <-
  data.frame(
    nombreMaquina = character(), 
    nrofold = numeric(), 
    datasetSel = character(), 
    metClasificacion = character(),
    escenario = character(),
    
    tiempoXCiclo= numeric(), #Tiempo en Minutos
    
    stringsAsFactors = FALSE
  )




for(i in listaArchivosEstadisticos){
  datosExtraidos <- read_csv(paste("C:/compendioResultados/estadisticos/",i,sep = ""),col_types = cols(X = col_skip()))
  datosExtraidos <-datosExtraidos[,-1]
  estadisticasTiempos<- rbind(estadisticasTiempos,datosExtraidos)
}

#Elimino los registros duplicados
#Creo un id=escenario + nrofold + datasetSel + metClasificacion + missingclass
estadisticasTiempos["Identificador"]<-paste(estadisticasTiempos$nombreMaquina,"_",estadisticasTiempos$nrofold,"_",estadisticasTiempos$datasetSel,"_",estadisticasTiempos$metClasificacion,"_",estadisticasTiempos$escenario,sep = "")
estadisticasTiempos<-estadisticasTiempos[!duplicated(estadisticasTiempos["Identificador"]),]

#Quito columnas inservibles:
estadisticasTiempos <- estadisticasTiempos[,(!names(estadisticasTiempos) %in% c("Identificador"))]


#Obtengo Promedios
promediosTiemposCiclo <- aggregate(x = estadisticasTiempos[c("tiempoXCiclo")],  
                       by = estadisticasTiempos[c("nombreMaquina", "datasetSel","metClasificacion","escenario")], 
                       FUN = function(valoresFilas){
                         round(mean(pmax(valoresFilas, 0)),digits = 3) #Tiempo en Segundos
                       })
colnames(promediosTiemposCiclo)[colnames(promediosTiemposCiclo)=="tiempoXCiclo"] <- "tiempoXCiclo(seg)"

write.csv(promediosTiemposCiclo, file = "C:/compendioResultados/Resumenes/result_EstadisticosxMetCla.csv")




#Estadisticas:

accEscTA<-dbEsc1TA$acuJer
accEscRA<-dbEsc1RA$acuJer
dbTA<-cbind(accEscTA,accEscRA)

write.csv(dbTA, file = "C:/compendioResultados/Resumenes/dbTA.csv",row.names = FALSE)


