funTrainingLNP <- function(graph, dataTraining, nivel="raiz", modeloEntrenamiento="J48"){

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
              funTrainingLNP(nodoHijo,hijoDataTraining, nivel = nodosDescendientes[1], modeloEntrenamiento)
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
        
        # # Validacion Cruzada: Esto es lo de los experimentos de Daniel ICCS
        # fitControl <- trainControl(## 10-fold CV
          # method = "repeatedcv",
          # number = 10,
          # ## repeated ten times
          # repeats = 10)
        
        #clasificadorNodo <- train(claseAPredecir ~., data = tempTraining , method = modeloEntrenamiento, trControl = fitControl) 
        clasificadorNodo <- rpart(claseAPredecir ~., data = tempTraining)
        
        salidasPosibles<-(paste(levels(tempTraining$claseAPredecir), collapse="#"))
        
        #Tambien guardo el clasificador en el grafico
        graph$clasificador<-clasificadorNodo
        graph$entrada<-nivel
        graph$salidas<-salidasPosibles
        table(tempTraining$claseAPredecir) 
        graph$cantEntrenamiento<-(paste(paste(names(table(tempTraining$claseAPredecir)),sep = " ")," :",as.character(table(tempTraining$claseAPredecir)), collapse=" | "))
        
    }
}


#Utilizado para Procesar Textos
funTrainingLNPText <- function(graph, dataTraining, nivel="raiz", modeloEntrenamiento="J48"){
  
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
        funTrainingLNPText(nodoHijo,hijoDataTraining,nivel = nodosDescendientes[1], modeloEntrenamiento)
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
    
    # Validacion Cruzada:
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = 10,
      ## repeated ten times
      repeats = 10)
    
    clasificadorNodo <- entrenarLibT(tempTraining,modeloEntrenamiento)
    salidasPosibles<-(paste(levels(tempTraining$claseAPredecir), collapse="#"))
    
    #Tambien guardo el clasificador en el grafico
    graph$clasificador<-clasificadorNodo
    graph$entrada<-nivel
    graph$salidas<-salidasPosibles
    table(tempTraining$claseAPredecir) 
    graph$cantEntrenamiento<-(paste(paste(names(table(tempTraining$claseAPredecir)),sep = " ")," :",as.character(table(tempTraining$claseAPredecir)), collapse=" | "))
    
  }
}




#Para Bases de Datos Documentales

funTrainingLNPDOC <- function(graph, dataTraining,dataTesting, nivel="raiz", modeloEntrenamiento="J48"){
  
  library (RTextTools)
  library(dendextend)
  library(igraph)
  
  dataTraining$claseAPredecir<-factor(dataTraining$claseAPredecir)
  
  
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
        funTrainingLNPDOC(nodoHijo,hijoDataTraining,dataTesting,nivel = nodosDescendientes[1], modeloEntrenamiento)
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
    
    
    
    #Opcion 1
    #Realizo la clasificacion
    train<-dataTraining
    test<-dataTesting
    
    bdTempCorpus<-rbind(train,test)
    
    library(RTextTools)
    
    doc_matrix <- create_matrix(bdTempCorpus$textosNoticias, language="english", 
                                removeNumbers=TRUE,
                                stemWords=TRUE, removeSparseTerms=.97)
    
    container <- create_container(doc_matrix, bdTempCorpus$claseAPredecir, 
                                  trainSize=1:nrow(train),
                                  testSize=(nrow(train)+1):nrow(bdTempCorpus), 
                                  virgin=FALSE)
    
    
    ptm <- proc.time()
    print(modeloEntrenamiento)
    clasificadorNodo <- train_model(container,modeloEntrenamiento)
    print(proc.time() - ptm)
    
    
    salidasPosibles<-(paste(levels(tempTraining$claseAPredecir), collapse="#"))
    
    #Tambien guardo el clasificador en el grafico
    graph$clasificador<-clasificadorNodo
    graph$entrada<-nivel
    graph$salidas<-salidasPosibles
    table(tempTraining$claseAPredecir) 
    graph$cantEntrenamiento<-(paste(paste(names(table(tempTraining$claseAPredecir)),sep = " ")," :",as.character(table(tempTraining$claseAPredecir)), collapse=" | "))
    
  }
}

  
