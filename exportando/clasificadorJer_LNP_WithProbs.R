#Utilizado para Procesar Textos
funTrainingLNP3 <- function(graph, dataTraining, nivel="raiz", tecnicaEntrenamiento,probabilitiesOp=FALSE,balanceOpt=FALSE,adaptarModelo=FALSE){
  
  #library(caret)
  library(dendextend)
  library(igraph)
  
  hijos<-graph$count
  
  #Si el nodo tiene hijos creo el clasificador por ese nodo padre.
  
  if(hijos>1){
    #1) Selecciono y Ajusto los datos para el entrenamiento 
    tempTraining<-{}
    
    for(i in 1:hijos){
      
      #i<-1
      #Valores del Hijo
      nodoHijo<-graph$children[[i]]
      
      #En la adaptacion del modelo se actualiza el nodo actual y el hijo llamado cNew
      if(adaptarModelo){
        if(nodoHijo$name=="cNew"){
          condicionAD<-TRUE
        }else{
          condicionAD<-FALSE
        }
      }else{
        condicionAD<-TRUE
      }
      
      
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
        
        if(condicionAD){
          funTrainingLNP3(graph = nodoHijo,dataTraining = hijoDataTraining,nivel = nodosDescendientes[1],tecnicaEntrenamiento =  tecnicaEntrenamiento,probabilitiesOp = probabilitiesOp,balanceOpt = balanceOpt,adaptarModelo = adaptarModelo  )
        }
        
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
    


    if(balanceOpt){     # if(balanceOpt==TRUE && graph$name == "cNew"){
      
      opBalanceo<-"balanceo-DMwR"
      
      #Balanceo utilizando ubSMOTE:
      
      if(opBalanceo=="balanceo-ubSMOTE"){
        
        freqclaseAPredecir<-count(tempTraining,claseAPredecir)
        minority<- freqclaseAPredecir[which(freqclaseAPredecir$n==min(freqclaseAPredecir[,2])),][[1]]
        majority<- freqclaseAPredecir[which(freqclaseAPredecir$n==max(freqclaseAPredecir[,2])),][[1]]
        
        tempTraining<-within(tempTraining,levels(claseAPredecir)[levels(claseAPredecir)==minority]<-1)
        tempTraining<-within(tempTraining,levels(claseAPredecir)[levels(claseAPredecir)==majority]<-0)
        
        #obtengo el los Inputs
        tempTrainingInputs <- tempTraining[,(!names(tempTraining) %in% c("claseAPredecir"))]
        set.seed(123)
        data<-ubSMOTE(tempTrainingInputs, tempTraining$claseAPredecir)
        n<-ncol(tempTraining)
        balancedTraining<-cbind(data$X, data$Y)
        colnames(balancedTraining)[colnames(balancedTraining)=="data$Y"] <- "claseAPredecir"
        
        levels(balancedTraining$claseAPredecir)[levels(balancedTraining$claseAPredecir)=="1"]<-as.character(minority)
        levels(balancedTraining$claseAPredecir)[levels(balancedTraining$claseAPredecir)=="0"]<-as.character(majority) 

      }
      
      #Balanceo utilizando DMwR para Balanceo:
      
      if(opBalanceo=="balanceo-DMwR"){
        library(DMwR)
        balancedTraining <- SMOTE(claseAPredecir~..,data=tempTraining)
        
        balancedTraining <- balancedTraining[complete.cases(balancedTraining),]
      }
      

      clasificadorNodo <- entrenarLibTenicas(train = balancedTraining,metodo = tecnicaEntrenamiento,withProbabilities = probabilitiesOp)
      tempTraining<-balancedTraining

      
    }else{
      
      clasificadorNodo <- entrenarLibTenicas(train = tempTraining,metodo = tecnicaEntrenamiento,withProbabilities = probabilitiesOp) 
      
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
