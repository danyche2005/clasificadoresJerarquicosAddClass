#Utilizado para Procesar Textos
funTrainingLNP3 <- function(graph, dataTraining, nivel="raiz", tecnicaEntrenamiento,probabilitiesOp=FALSE,balanceOpt=FALSE){
  
  #library(caret)
  library(dendextend)
  library(igraph)
  
  hijos<-graph$count
  
  #Si el nodo tiene hijos creo el clasificador por ese nodo padre.
  
  if(hijos>1){
    #1) Selecciono y Ajusto los datos para el entrenamiento 
    tempTraining<-{}
    
    for(i in 1:hijos){
      #Valores del Hijo
      nodoHijo<-graph$children[[i]]
      nodosDescendientes = names(nodoHijo$Get('level'))
      hijoDataTraining<-dataTraining[dataTraining$claseAPredecir %in% nodosDescendientes,]
      
      #Actualiza los datos de sus descendientes
      if(!nodoHijo$isLeaf){
        
        #Renombro todos los descendientes con el nombre del nodo padre
        renomTraining<-hijoDataTraining
        renomTraining$claseAPredecir<-rep(nodosDescendientes[1],nrow(renomTraining))
        
        #Adjunto datos renombrados
        tempTraining <- rbind(tempTraining, renomTraining) 
        #Recursion para niveles inferiores, solo con los datos relevantes para
        #entrenar al clasificador Nodo del hijo
        funTrainingLNP3(nodoHijo,hijoDataTraining,nivel = nodosDescendientes[1], tecnicaEntrenamiento,probabilitiesOp)
      }else{
        #Adjunto los datos sin renombrar
        tempTraining<-rbind(tempTraining,hijoDataTraining)
      }
    }
    #Muestro Grafica del Clasificador        
    #plot(as.dendrogram(graph))
    #plot(as.igraph(graph, directed = TRUE, direction = "climb"))
    
    #2) Creo el clasificador
    tempTraining$claseAPredecir<-factor(tempTraining$claseAPredecir)
    
    if(balanceOpt==FALSE){
      clasificadorNodo <- entrenarLibTenicas(train = tempTraining,metodo = tecnicaEntrenamiento,withProbabilities = probabilitiesOp)
    }else{
      
      freqclaseAPredecir<-count(tempTraining,claseAPredecir)
      minority<- freqclaseAPredecir[which(freqclaseAPredecir$n==min(freqclaseAPredecir[,2])),][[1]]
      majority<- freqclaseAPredecir[which(freqclaseAPredecir$n==max(freqclaseAPredecir[,2])),][[1]]
      tempTraining<-within(tempTraining,levels(claseAPredecir)[levels(claseAPredecir)==minority]<-1)
      tempTraining<-within(tempTraining,levels(claseAPredecir)[levels(claseAPredecir)==majority]<-0)
      n<-ncol(tempTraining)
      data<-ubSMOTE(tempTraining[,-n], tempTraining$claseAPredecir)
      
      # data<-ubSMOTE(tempTraining[,-n], tempTraining$claseAPredecir, perc.under = 100)
      balancedTraining<-cbind(data$X, data$Y)
      colnames(balancedTraining)[n]<-"claseAPredecir"
      
      #balancedTraining<-within(balancedTraining,levels(claseAPredecir)[levels(claseAPredecir)=="1"]<-as.character(minority))
      #balancedTraining<-within(balancedTraining,levels(claseAPredecir)[levels(claseAPredecir)=="0"]<-as.character(majority))
      
      levels(balancedTraining$claseAPredecir)[levels(balancedTraining$claseAPredecir)=="1"]<-as.character(minority)
      levels(balancedTraining$claseAPredecir)[levels(balancedTraining$claseAPredecir)=="0"]<-as.character(majority) 
      
      clasificadorNodo <- entrenarLibTenicas(train = balancedTraining,metodo = tecnicaEntrenamiento,withProbabilities = probabilitiesOp)
      
      tempTraining<-balancedTraining
      
    }
    
    
    salidasPosibles<-(paste(levels(tempTraining$claseAPredecir), collapse="#"))
    
    #Tambien guardo el clasificador en el grafico
    graph$clasificador<-clasificadorNodo
    graph$entrada<-nivel
    graph$salidas<-salidasPosibles
    table(tempTraining$claseAPredecir)
    graph$cantEntrenamiento<-(paste(paste(names(table(tempTraining$claseAPredecir)),sep = " ")," :",as.character(table(tempTraining$claseAPredecir)), collapse=" | "))
    
  }
}
