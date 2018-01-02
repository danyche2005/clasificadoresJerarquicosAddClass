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




topDownTesting <- function(datoTest,graph){
  
  #Recorro la estructura con la estrategia TopDown.
  hijos<-graph$count
  
  clasePredicha<-as.character(predict(graph$clasificador,newdata = datoTest,type="class"))
  
  
  
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
          
          clasePredicha<-topDownTesting(datoTest,nodoHijo)
          
          if(clasePredicha %in% paste(graph$Get('name', filterFun = isLeaf)))
            return(clasePredicha)  
          
        }
      }
      
    }
  }
}  

