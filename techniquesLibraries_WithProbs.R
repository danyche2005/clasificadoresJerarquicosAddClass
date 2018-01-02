#Realizo el Entrenamiento 
entrenarLibTenicas<-function(trainData,metodo,withProbabilities=FALSE){

  #library(depmixS4)
#  library(CRF)
 # library(mclust)
  
  
  #print(paste("Prediccion",metodo))
  if(withProbabilities){
    tipoResultado<-"probs"
  }else{
    tipoResultado<-"class"
  }

  
  modelo<-NA
  bandera<-FALSE
  
  ptm <- proc.time()
  
  if(metodo=="RF-EX"){
    
    library(randomForest)
    
    # Support Vector Machine Svm : 
    # Rejerencia 20NewsGroup (Accuracy: 0.6552553)   154 seg y 15seg prediccion
    modelo <- randomForest(formula = claseAPredecir~., data=trainData)

    bandera<-TRUE
  }
  
  
  if(metodo=="SVM-EX"){
    
    library(e1071) 
    
    # Support Vector Machine Svm : 
    # Rejerencia 20NewsGroup (Accuracy: 0.6552553)   154 seg y 15seg prediccion
    
    if(withProbabilities){
      modelo <- svm(claseAPredecir~., data=trainData, probability=withProbabilities)
    }else{
      modelo <- svm(claseAPredecir~., data=trainData)
    }
    
    

    bandera<-TRUE
  }
  
  if(metodo=="C50"){
    
    library(C50)
    
    # C50: 
    # Referencia 20NewsGroup Acu: 0.42222 Entrenar 58.25 segn Predecir 8.25 seg

    modelo <- C5.0(claseAPredecir ~ ., data = trainData)
    

    bandera<-TRUE
  }
  
  
  if(metodo=="BOOST-EX"){
    
    library(adabag)
    
    # BOOST: 
    # Referencia 20NewsGroup Acu:0.3633 Entrena 2878.55 segundos 21 Seg en Predecir

    modelo <- boosting(claseAPredecir ~ ., data = trainData)

    bandera<-TRUE
  }
  
  if(metodo=="NB-EX"){
    
    library(e1071)
    
    # NB: 
    # Referencia 20NewsGroup (Accuracy: 0.4054) Entrenamiento 2.84 Prediccion 56.92

    modelo<-naiveBayes(claseAPredecir~.,data = trainData)

    bandera<-TRUE
    
  }
  
  if(metodo=="RPART"){
    
    library(rpart)
    # PART: 
    modelo <- rpart(claseAPredecir~., data=trainData)
    bandera<-TRUE
    
  }
  
  if(metodo=="MAXENT-EX"){
    
    library(maxent)
    
    # PART: 
    # Referencia 20NewsGroup (Accuracy: 0.4054) Entrenamiento 2.84 Prediccion 56.92
    tempsinClaseAPredecir<-trainData[,(!names(trainData) %in% c("claseAPredecir"))]
    modelo<-maxent(tempsinClaseAPredecir,trainData$claseAPredecir)
    bandera<-TRUE
    
  }
  
  
  if(metodo=="HoeffdingTree"){
    require(RMOA)
    
    
    hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
    hdt
    
    #Creo el stream desde la base de datos
    dstream <- datastream_dataframe(data=trainData)
    
    ## train the HoeffdingTree on the iris dataset
    modelo <- trainMOA(model = hdt,
                        formula = claseAPredecir~., 
                        data = dstream)
    bandera<-TRUE
    
  }
 
  
  if(metodo=="HoeffdingAdaptiveTree"){
    require(RMOA)
    
    
    hdt <- HoeffdingAdaptiveTree(numericEstimator = "GaussianNumericAttributeClassObserver")
    hdt
    
    #Creo el stream desde la base de datos
    dstream <- datastream_dataframe(data=trainData)
    
    ## train the HoeffdingTree on the iris dataset
    modelo <- trainMOA(model = hdt,
                       formula = claseAPredecir~., 
                       data = dstream)
    bandera<-TRUE
    
  }
  

 
  if(!bandera){
    
    library(caret)
    library(nnet)
    library(knncat)
    
    #Si no esta en las anteriores es porque es parte de Caret
    modelo <-
      caret::train(
        claseAPredecir ~ ., data = trainData , method = metodo
      )
    
  }
  
  
  
  
  tiempoEntrenamiento <- (proc.time() - ptm)[3]
  
  print(paste("Entrena ",metodo," en: ",format(round(tiempoEntrenamiento, 3), nsmall = 3)," seg."))
  
  return(modelo)
} 




#Realizo el Entrenamiento 
prediceLibTecnicas<-function(datosTest,modelo,metodo,obtenerProb=FALSE){
  
  #Test sin Clase
  testExamples <- datosTest[,(!names(datosTest) %in% c("claseAPredecir"))]
  
  bandera<-FALSE
  
  if(metodo=="SVM-EX"){
    
    if(obtenerProb){
      resultados<-predict(modelo,testExamples,probability=obtenerProb)
    }else{
      resultados<-predict(modelo,testExamples,type="class")
    }
    bandera<-TRUE
  }
  
  if(metodo=="NB-EX"){
    # NB: 
    # 20NewsGroup (Accuracy: 0.4054) Entrenamiento 2.84 Prediccion 56.92

    if(obtenerProb){
      resultados<-predict(modelo,testExamples,type="raw")
    }else{
      resultados<-predict(modelo,testExamples,type="class")
    }
    
    bandera<-TRUE
  }
  
  
  if(metodo=="BOOST-EX"){
    # BOOST: 
    resultadosPaq<-predict(modelo,testExamples,type="class")
    resultados<-resultadosPaq[[4]]
    bandera<-TRUE
  }
  
  
  if(metodo=="MAXENT-EX"){
    # PART: 
    resultados<-predict(modelo,testExamples,type="class")
    resultados<-resultados[,1]
    bandera<-TRUE
    
  }
  
  if(metodo=="RF-EX"| metodo=="RPART"|metodo=="C50"){
    
    if(obtenerProb){
      resultados <- predict(modelo, newdata=testExamples, type="prob")
    }else{
      resultados <- predict(modelo, newdata=testExamples, type="class")
      resultados<-as.character(resultados)
    }
    
    bandera<-TRUE
    
  }
  
  
  
  if(metodo=="HoeffdingTree"){
    require(RMOA)
  
    if(obtenerProb){
      resultados <- predict(modelo, newdata=testExamples, type="votes")
    }else{
      resultados <- predict(modelo, newdata=testExamples, type="response")
    }
    
    bandera<-TRUE
  }
  
  
  if(metodo=="HoeffdingAdaptiveTree"){
    require(RMOA)
    
    if(obtenerProb){
      resultados <- predict(modelo, newdata=testExamples, type="votes")
    }else{
      resultados <- predict(modelo, newdata=testExamples, type="response")
    }
    
    bandera<-TRUE
  }
  
  
  if(metodo=="J48"||metodo=="PART"||metodo=="JRip"||metodo=="fda"||metodo=="knn"||metodo=="nnet"){
    
    library(caret)

    if(obtenerProb){
      resultados<-predict(modelo,newdata = testExamples,type="prob")
    }else{
      resultados<-predict(modelo,newdata = testExamples,type="raw")
    }

    bandera<-TRUE
  }
  
 
 
  #Si no esta en las anteriores es porque es parte de Caret
  if(!bandera){
    
    library(caret)
    resultados <- predict(modelo,newdata = testExamples,type="class")
    
  }
  
  return(resultados)
} 


#Se realiza una prueba simple con todos los metodos propuestos para la clasificacion.
pruebaSimpleModelos<-function(datasetslista,metClasificacion,dirRaiz,versionDataSet){
 
   for (opcionBaseDatos in 1:length(datasetslista)) {
    print("######################################################")
    print(paste("Usando el DataSet:",datasetslista[opcionBaseDatos]))
    print("######################################################")
    
    ptm <- proc.time()
    baseSeleccionada <- cargarDatos(opcionDataSet=datasetslista[opcionBaseDatos],dirRaiz=dirRaiz,version=versionDataSet)
    tiempoCargaDataset <- (proc.time() - ptm)[3]
    
    
    ## 90/10 % of the sample size
    smp_size <- floor(0.90 * nrow(baseSeleccionada))
    ## set the seed to make your partition reproductible
    set.seed(123)
    trainData_ind <- sample(seq_len(nrow(baseSeleccionada)), size = smp_size)
    trainData <- baseSeleccionada[trainData_ind, ]
    test <- baseSeleccionada[-trainData_ind, ]
    
    print(paste("El Nro-Instancias trainData: ",nrow(trainData)," El Nro-Instancias Test: ",nrow(test),sep=""))
    
    
    for (msel in metClasificacion)
    {
      #Creo el Modelo
      ptm <- proc.time()
      modPlano <- entrenarLibT(trainData,msel)
      tiempoFlatEntrenamiento <- (proc.time() - ptm)[3] 
      #Realizo la Prediccion:
      ptm <- proc.time()
        predicciones<-prediceLibT(datosTest=test,modelo=modPlano,metodo=msel)
      tiempoFlatPrediccion <- (proc.time() - ptm)[3]
      
      cfmatrixJerarquico <-confusionMatrix(predicciones,test$claseAPredecir)
      
      print(paste("  El tiempo entrenamiento es de: ",format(round(tiempoFlatEntrenamiento, 3), nsmall = 3)," segs con trainDataDataset.",sep=""))
      print(paste("  El tiempo prediccion es de: ",format(round(tiempoFlatPrediccion, 3), nsmall = 3)," segs para todo el TestDataset.",sep=""))
      print(paste("   * Accuracy: ",format(round(cfmatrixJerarquico$overall[1], 3), nsmall = 3)," <---|.",sep=""))
      

    }
    print("######################################################")
    print(paste("El tiempo de carga BaseDatos ",datasetslista[opcionBaseDatos]," es de: ",format(round(tiempoCargaDataset, 3), nsmall = 3)," segs.",sep=""))
    print("######################################################")
    
  }
  
}


pruebasLibreria<-function(){
    
  #Pruebas
  #Fijo conjuntos de entrenamiento y de test
  ## 75% of the sample size
  baseSeleccionada<-dataSetUse
  smp_size <- floor(0.75 * nrow(baseSeleccionada))
  smp_size
  set.seed(123)
  trainData_ind <- sample(seq_len(nrow(baseSeleccionada)), size = smp_size)
  trainData <- baseSeleccionada[trainData_ind, ]
  test <- baseSeleccionada[-trainData_ind, ]
  
  #Creo el modelo
  metodo<-"BOOST"
  modelo<-entrenarLibT(trainData,test,metodo)
  
  #Realizo la prediccion
  ptm <- proc.time()
  resultados<-prediceLibT(test,modelo,metodo)
  tiempoPrediccion<-print(proc.time() - ptm)[3]
  
  #Obtengo el Acurracy
  cmatrix<-confusionMatrix(resultados,test$claseAPredecir)
  acu<-as.numeric(cmatrix[[3]][1])
  
  #Muestro la matriz de confusion en tabla de colores
  confusion <- table(test$claseAPredecir,resultados)
  image(confusion[,ncol(confusion):1], axes=FALSE)
  axis(2, at = seq(0, 1, length=length(colnames(confusion))), labels=colnames(confusion))

}




  
  