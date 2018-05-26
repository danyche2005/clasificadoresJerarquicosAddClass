####The next function creates the classifier tree using the validation set
funTrainingLNP <- function(graph, dataTraining, nivel="raiz"){

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
              funTrainingLNP(nodoHijo,hijoDataTraining, nivel = nodosDescendientes[1])
            }else{
              #Adjunto los datos sin renombrar
              tempTraining<-rbind(tempTraining,hijoDataTraining)
            }
        }##for
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
        #Guardamos el clasificador como parte de la estructura jerarquica que modificaremos aplicando SMOTE para balancear las clases del nodo que añadamos
        graph$clasificadorBalanced<-clasificadorNodo
        graph$entrada<-nivel
        graph$salidas<-salidasPosibles
        table(tempTraining$claseAPredecir) 
        graph$cantEntrenamiento<-(paste(paste(names(table(tempTraining$claseAPredecir)),sep = " ")," :",as.character(table(tempTraining$claseAPredecir)), collapse=" | "))
        
    
    }
}




topDownTesting <- function(datoTest,graph,metodo){
  
  #Recorro la estructura con la estrategia TopDown.
  hijos<-graph$count
  
  #clasePredicha<-as.character(predict(graph$clasificador,newdata = datoTest,type="class"))
  clasePredicha<-as.character(prediceLibTecnicas(modelo=graph$clasificador,datosTest=datoTest,metodo,obtenerProb=FALSE))
  
  
  #Copruebo que la clase que estoy prediciendo es una Hoja.
  nodosHoja<-paste(graph$Get('name', filterFun = isLeaf))
  if(clasePredicha %in% nodosHoja)
    return(clasePredicha)
  
  #Debo bajar un nivel en la jerarquia
  if(hijos>1){
    for(i in 1:hijos){
      if(clasePredicha==graph$children[[i]]$name){
        #Valores del Hijo
        nodoHijo<-graph$children[[i]]
        
        if(!is.null(nodoHijo$clasificador)){
          
          clasePredicha<-topDownTesting(datoTest,nodoHijo,metodo)
          
          if(clasePredicha %in% paste(graph$Get('name', filterFun = isLeaf)))
            return(clasePredicha)  
          
        }
      }
      
    }
  }
}  

#Daniel
#TopDown-BottomUp Implementation

topDownBottomUpTesting <- function(datoTest,graph,metodo,probabisList=c(),resultados=c()){
  
  if(length(probabisList)==0){
    probabisList <- data.frame(nodo1=character(), nodo2=character(), probabilidad=numeric(),stringsAsFactors=FALSE) 
    resultados<- data.frame(claseLabel=character(), probabilidad=numeric(),stringsAsFactors=FALSE)
  }
  
  nrohijos<-graph$count
  
  if(nrohijos>0){
    
    #Solamente cuando tiene hijos hago la prediccion (LNP)
    #Asumo que el clasificador siempre es un nodo padre
    prediccionRes<-prediceLibTecnicas(datosTest = datoTest,modelo = graph$clasificador,metodo = metodo,obtenerProb=TRUE)
    
    probabisNew<-(attr(prediccionRes,"probabilities"))
    if(is.null(probabisNew)){
      probabisNew<-prediccionRes
    }
    
    
    #Guardo el valor de la probabilidad en el dataframe
    for(i in 1:ncol(probabisNew)){
      fila <- nrow(probabisList) + 1
      probabisList[fila,] <-list(as.character(graph$name),as.character(colnames(probabisNew)[i]),as.numeric(probabisNew[i]))
    }
    
    for(i in 1:nrohijos){
      
      #Valores del Hijo
      nodoHijo<-graph$children[[i]]
      
      #Analizo al Hijo
      resultados<-topDownBottomUpTesting(datoTest = datoTest,graph = nodoHijo,metodo = metodo,probabisList = probabisList,resultados = resultados)
      
    }
    
  }else{
    #Es Hoja
    labelHoja<-paste(graph$Get('name', filterFun = isLeaf))
    probClase<-ObtenerProbClase(probabisList = probabisList,labelHoja = labelHoja)
    #print(probabisList)
    #print(probClase)
    #print(labelHoja)  
    
    #Guardo la probabilidad de cada clase
    filaRes <- nrow(resultados) + 1
    resultados[filaRes,] <-list(labelHoja,probClase)
    
  }
  
  return(resultados)
}


topDownBottomUpTestingB <- function(datoTest,graph,metodo,probabisList=c(),resultados=c()){
  
  if(length(probabisList)==0){
    probabisList <- data.frame(nodo1=character(), nodo2=character(), probabilidad=numeric(),stringsAsFactors=FALSE) 
    resultados<- data.frame(claseLabel=character(), probabilidad=numeric(),stringsAsFactors=FALSE)
  }
  
  nrohijos<-graph$count
  
  if(nrohijos>0){
    
    #Solamente cuando tiene hijos hago la prediccion (LNP)
    #Asumo que el clasificador siempre es un nodo padre
    prediccionRes<-prediceLibTecnicas(datosTest = datoTest,modelo = graph$clasificadorBalanced,metodo = metodo,obtenerProb=TRUE)
    
    probabisNew<-(attr(prediccionRes,"probabilities"))
    if(is.null(probabisNew)){
      probabisNew<-prediccionRes
    }
    
    
    #Guardo el valor de la probabilidad en el dataframe
    for(i in 1:ncol(probabisNew)){
      fila <- nrow(probabisList) + 1
      probabisList[fila,] <-list(as.character(graph$name),as.character(colnames(probabisNew)[i]),as.numeric(probabisNew[i]))
    }
    
    for(i in 1:nrohijos){
      
      #Valores del Hijo
      nodoHijo<-graph$children[[i]]
      
      #Analizo al Hijo
      resultados<-topDownBottomUpTestingB(datoTest = datoTest,graph = nodoHijo,metodo = metodo,probabisList = probabisList,resultados = resultados)
      
    }
    
  }else{
    #Es Hoja
    labelHoja<-paste(graph$Get('name', filterFun = isLeaf))
    probClase<-ObtenerProbClase(probabisList = probabisList,labelHoja = labelHoja)
    #print(probabisList)
    #print(probClase)
    #print(labelHoja)  
    
    #Guardo la probabilidad de cada clase
    filaRes <- nrow(resultados) + 1
    resultados[filaRes,] <-list(labelHoja,probClase)
    
  }
  
  return(resultados)
}



#Obtengo la probabilidad para esta clase
#Recorro el nodo hasta la hoja y multiplico las probabilidades
ObtenerProbClase <- function(probabisList,labelHoja,probAnt=1){
  
  #Busco la existencia clase
  listTemp<-probabisList[probabisList$nodo2==labelHoja,]
  for(i in 1:nrow(listTemp)) {
    registro <- listTemp[i,]
    
    probabilidad<-probAnt*registro$probabilidad
    
    if(registro$nodo1=="Root"){
      break
    }else{
      probabilidad<-ObtenerProbClase(probabisList = probabisList,labelHoja=registro$nodo1,probAnt = probabilidad)
    }
  }
  
  return(probabilidad)
}

prediccionConJerarquiaProbs<-function(jerarquiaClases,testing,metodo,obtenerClases=FALSE){
  
  etiquetas<-as.character(jerarquiaClases$Get("name", filterFun = isLeaf))
  etiquetas<-etiquetas[order(etiquetas)]
  predicciones <- read.table(textConnection(""), col.names = etiquetas,colClasses = "numeric")
  
  contador<-nrow(testing)
  #print(paste("Son Predicciones Nro:",contador))
  for(i in 1:nrow(testing)){
    if(i%%500==0){
      #print(i)
    }
    #print(i)
    #Realizo el recorrido TopDown
    resultadosIns<-topDownBottomUpTesting(datoTest = testing[i,],graph = jerarquiaClases,metodo = metodo)
    resultadosIns<-resultadosIns[order(resultadosIns$claseLabel),]
    
    #Ajusto los resultados
    n <- resultadosIns$claseLabel
    resultadosInsP <- as.data.frame(t(resultadosIns[-1]))
    colnames(resultadosInsP) <- n
    
    
    predicciones<-rbind(predicciones,resultadosInsP)
    
    
  }
  
  #Obtengo las Clases Unicamente
  if(obtenerClases){
    listaClases<-{}
    for(i in 1:nrow(predicciones)){
      prediccion<-predicciones[i,]
      numCol<-which(prediccion==max(prediccion))  
      claseEncontrada<-names(prediccion)[numCol]
      listaClases[i]<-claseEncontrada
    }  
    predicciones<-listaClases
  }
  
  
  
  return(predicciones)
}



prediccionConJerarquiaProbsB<-function(jerarquiaClases,testing,metodo,obtenerClases=FALSE){
  
  etiquetas<-as.character(jerarquiaClases$Get("name", filterFun = isLeaf))
  etiquetas<-etiquetas[order(etiquetas)]
  predicciones <- read.table(textConnection(""), col.names = etiquetas,colClasses = "numeric")
  
  contador<-nrow(testing)
  #print(paste("Son Predicciones Nro:",contador))
  for(i in 1:nrow(testing)){
    if(i%%500==0){
      #print(i)
    }
    #print(i)
    #Realizo el recorrido TopDown
    resultadosIns<-topDownBottomUpTesting(datoTest = testing[i,],graph = jerarquiaClases,metodo = metodo)
    resultadosIns<-resultadosIns[order(resultadosIns$claseLabel),]
    
    #Ajusto los resultados
    n <- resultadosIns$claseLabel
    resultadosInsP <- as.data.frame(t(resultadosIns[-1]))
    colnames(resultadosInsP) <- n
    
    
    predicciones<-rbind(predicciones,resultadosInsP)
    
    
  }
  
  #Obtengo las Clases Unicamente
  if(obtenerClases){
    listaClases<-{}
    for(i in 1:nrow(predicciones)){
      prediccion<-predicciones[i,]
      numCol<-which(prediccion==max(prediccion))  
      claseEncontrada<-names(prediccion)[numCol]
      listaClases[i]<-claseEncontrada
    }  
    predicciones<-listaClases
  }
  
  
  
  return(predicciones)
}

