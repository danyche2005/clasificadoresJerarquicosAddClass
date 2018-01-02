
####The next function creates the new node in the classifier tree using the training and validation sets and applying SMOTE
 funTrainingLNP2 <- function(graph, dataTraining, nivel="raiz"){
  
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
   # }
    #2) Creo el clasificador
    
    #To apply SMOTE (from the unbalanced package) we should code the minority class as 1 and the other class as 0
    tempTraining$claseAPredecir<-factor(tempTraining$claseAPredecir)
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
    balancedTraining<-within(balancedTraining,levels(claseAPredecir)[levels(claseAPredecir)=="1"]<-as.character(minority))
    balancedTraining<-within(balancedTraining,levels(claseAPredecir)[levels(claseAPredecir)=="0"]<-as.character(majority))
    
    
    # # Validacion Cruzada: Esto es lo de los experimentos de Daniel ICCS
    # fitControl <- trainControl(## 10-fold CV
    # method = "repeatedcv",
    # number = 10,
    # ## repeated ten times
    # repeats = 10)
    
    #clasificadorNodo <- train(claseAPredecir ~., data = tempTraining , method = modeloEntrenamiento, trControl = fitControl) 
    clasificadorNodo <- rpart(claseAPredecir ~., data = balancedTraining)
    
    salidasPosibles<-(paste(levels(tempTraining$claseAPredecir), collapse="#"))
    
    #Tambien guardo el clasificador en el grafico
    graph$clasificadorBalanced<-clasificadorNodo
    graph$entradaBalanced<-nivel
    graph$salidasBalanced<-salidasPosibles
    table(tempTraining$claseAPredecir) 
    graph$cantEntrenamientoBalanced<-(paste(paste(names(table(tempTraining$claseAPredecir)),sep = " ")," :",as.character(table(tempTraining$claseAPredecir)), collapse=" | "))
    
    
  }
}

topDownTesting2 <- function(datoTest,graph){
  
  #Recorro la estructura con la estrategia TopDown.
  hijos<-graph$count
  
  clasePredicha<-as.character(predict(graph$clasificadorBalanced,newdata = datoTest,type="class"))
  
  
  
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
        
        if(!is.null(nodoHijo$clasificadorBalanced)){
          
          clasePredicha<-topDownTesting2(datoTest,nodoHijo)
          
          if(clasePredicha %in% paste(graph$Get('name', filterFun = isLeaf)))
            return(clasePredicha)  
          
        }
      }
      
    }
  }
}  

