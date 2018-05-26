####The next function creates the classifier tree using the validation set
funTrainingLNP <- function(arbol, dataTraining, nivel="raiz"){

    #library(caret)
    library(dendextend)
    library(igraph)
  

    hijos<-arbol$count
    
    #Si el nodo tiene hijos creo el clasificador por ese nodo padre.
    
    if(hijos>1){
        #1) Selecciono y Ajusto los datos para el entrenamiento 
        tempTraining<-{}
       
        for(i in 1:hijos){
            #Valores del Hijo
            nodoHijo<-arbol$children[[i]]
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
        arbol$clasificador<-clasificadorNodo
        #Guardamos el clasificador como parte de la estructura jerarquica que modificaremos aplicando SMOTE para balancear las clases del nodo que añadamos
        arbol$clasificadorBalanced<-clasificadorNodo
        arbol$entrada<-nivel
        arbol$salidas<-salidasPosibles
        table(tempTraining$claseAPredecir) 
        arbol$cantEntrenamiento<-(paste(paste(names(table(tempTraining$claseAPredecir)),sep = " ")," :",as.character(table(tempTraining$claseAPredecir)), collapse=" | "))
        
    
    }
}




topDownTesting <- function(datoTest,arbol,metodo){
  
  #Recorro la estructura con la estrategia TopDown.
  hijos<-arbol$count
  
  #clasePredicha<-as.character(predict(arbol$clasificador,newdata = datoTest,type="class"))
  clasePredicha<-as.character(prediceLibTecnicas(modelo=arbol$clasificador,datosTest=datoTest,metodo,obtenerProb=FALSE))
  
  
  #Copruebo que la clase que estoy prediciendo es una Hoja.
  nodosHoja<-paste(arbol$Get('name', filterFun = isLeaf))
  if(clasePredicha %in% nodosHoja)
    return(clasePredicha)
  
  #Debo bajar un nivel en la jerarquia
  if(hijos>1){
    for(i in 1:hijos){
      if(clasePredicha==arbol$children[[i]]$name){
        #Valores del Hijo
        nodoHijo<-arbol$children[[i]]
        
        if(!is.null(nodoHijo$clasificador)){
          
          clasePredicha<-topDownTesting(datoTest,nodoHijo,metodo)
          
          if(clasePredicha %in% paste(arbol$Get('name', filterFun = isLeaf)))
            return(clasePredicha)  
          
        }
      }
      
    }
  }
}  

#Daniel
#TopDown-BottomUp Implementation

topDownBottomUpTesting <- function(datoTest,arbol,metodo,probabisList=c(),resultados=c()){
  
  if(length(probabisList)==0){
    probabisList <- data.frame(nodo1=character(), nodo2=character(), probabilidad=numeric(),stringsAsFactors=FALSE) 
    resultados<- data.frame(claseLabel=character(), probabilidad=numeric(),stringsAsFactors=FALSE)
  }
  
  nrohijos<-arbol$count
  
  if(nrohijos>0){
    
    #Solamente cuando tiene hijos hago la prediccion (LNP)
    #Asumo que el clasificador siempre es un nodo padre
    prediccionRes<-prediceLibTecnicas(datosTest = datoTest,modelo = arbol$clasificador,metodo = metodo,obtenerProb=TRUE)
    
    
    probabisNew<-(attr(prediccionRes,"probabilities"))
    if(is.null(probabisNew)){
      probabisNew<-prediccionRes
    }
    
    
    #Guardo el valor de la probabilidad en el dataframe
    for(i in 1:ncol(probabisNew)){
      fila <- nrow(probabisList) + 1
      
      probabisList[fila,] <-list(as.character(arbol$name),as.character(colnames(probabisNew)[i]),as.numeric(probabisNew[i]))
      
      #Reemplazo si existen equivalencias
      if(!is.null(arbol$equivalencia)){
        if(!(arbol$equivalencia=="")){
          tempA<-unlist(strsplit(arbol$equivalencia, "[ ]"))
          tempA<-unlist(strsplit(tempA, "[#]"))
          objetivoLabel<-tempA[1]
          reemplazarCon<-tempA[2]
          
          if(probabisList[fila,2]==objetivoLabel){
            probabisList[fila,2]<-reemplazarCon
          }
        }
      }
      
    }
    
    for(i in 1:nrohijos){
      
      #Valores del Hijo
      nodoHijo<-arbol$children[[i]]
      
      #Analizo al Hijo
      resultados<-topDownBottomUpTesting(datoTest = datoTest,arbol = nodoHijo,metodo = metodo,probabisList = probabisList,resultados = resultados)
      
    }
    
  }else{
    #Es Hoja
    labelHoja<-paste(arbol$Get('name', filterFun = isLeaf))
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

topDownBottomUpTestingPoten <- function(datoTest,arbol,metodo,probabisList=c(),resultados=c()){
  
  if(length(probabisList)==0){
    probabisList <- data.frame(nodo1=character(), nodo2=character(), probabilidad=numeric(),clasificadorNumero=numeric(),nroInstancias=numeric(),stringsAsFactors=FALSE) 
    resultados<- data.frame(claseLabel=character(), probabilidad=numeric(),stringsAsFactors=FALSE)
  }
  
  nrohijos<-arbol$count
  
  if(nrohijos>0){
    
    #Solamente cuando tiene hijos hago la prediccion (LNP)
    #Asumo que el clasificador siempre es un nodo padre
    prediccionRes<-prediceLibTecnicas(datosTest = datoTest,modelo = arbol$clasificador,metodo = metodo,obtenerProb=TRUE)
    prediccionResAdapt<-prediceLibTecnicas(datosTest = datoTest,modelo = arbol$clasificadorAdapt,metodo = metodo,obtenerProb=TRUE)
    
    
    probabisNew<-(attr(prediccionRes,"probabilities"))
    if(is.null(probabisNew)){
      probabisNew<-prediccionRes
    }
    
    probabisNewAdapt<-(attr(prediccionResAdapt,"probabilities"))
    if(is.null(probabisNewAdapt)){
      probabisNewAdapt<-prediccionResAdapt
    }
    
    nrInstancias<-unlist(strsplit(arbol$instancias,"[#]"))
    names(nrInstancias)<-unlist(strsplit(arbol$salidas,"[#]"))
    
    #Guardo el valor de la probabilidad en el dataframe
    for(i in 1:ncol(probabisNew)){
      fila <- nrow(probabisList) + 1
      labelObj<-as.character(colnames(probabisNew)[i])
      probabisList[fila,] <-list(as.character(arbol$name),labelObj,as.numeric(probabisNew[i]),0,nrInstancias[labelObj])
      
      
      
      
      #Reemplazo si existen equivalencias
      #Excluyo el arbol adaptado por que este ya tiene las referencias correctas
      if(!is.null(arbol$equivalencia)){
        if(!(arbol$equivalencia=="")){
          tempA<-unlist(strsplit(arbol$equivalencia, "[ ]"))
          tempA<-unlist(strsplit(tempA, "[#]"))
          objetivoLabel<-tempA[1]
          reemplazarCon<-tempA[2]
          
          if(probabisList[fila,2]==objetivoLabel & probabisList[fila,4]!='cNew'){
            probabisList[fila,2]<-reemplazarCon
          }
        }
      }
      
    }
    
    for(i in 1:ncol(probabisNew)){
      fila <- nrow(probabisList) + 1
      labelObj<-as.character(colnames(probabisNew)[i])
      probabisList[fila,] <-list(as.character(arbol$name),labelObj,as.numeric(probabisNewAdapt[i]),1,nrInstancias[labelObj])
      
    }
    
    
    for(i in 1:nrohijos){
      
      #Valores del Hijo
      nodoHijo<-arbol$children[[i]]
      
      #Analizo al Hijo
      resultados<-topDownBottomUpTestingPoten(datoTest = datoTest,arbol = nodoHijo,metodo = metodo,probabisList = probabisList,resultados = resultados)
      
    }
    
  }else{
    #Es Hoja
    labelHoja<-paste(arbol$Get('name', filterFun = isLeaf))
    
    
    #Filtro el valor final
    
    probClase<-ObtenerProbClasePoten(probabisList = probabisList,labelHoja = labelHoja)
    #print(probabisList)
    #print(probClase)
    #print(labelHoja)  
    
    #Guardo la probabilidad de cada clase
    filaRes <- nrow(resultados) + 1
    resultados[filaRes,] <-list(labelHoja,probClase)
    
  }
  
  return(resultados)
}


topDownBottomUpTestingB <- function(datoTest,arbol,metodo,probabisList=c(),resultados=c()){
  
  if(length(probabisList)==0){
    probabisList <- data.frame(nodo1=character(), nodo2=character(), probabilidad=numeric(),stringsAsFactors=FALSE) 
    resultados<- data.frame(claseLabel=character(), probabilidad=numeric(),stringsAsFactors=FALSE)
  }
  
  nrohijos<-arbol$count
  
  if(nrohijos>0){
    
    #Solamente cuando tiene hijos hago la prediccion (LNP)
    #Asumo que el clasificador siempre es un nodo padre
    prediccionRes<-prediceLibTecnicas(datosTest = datoTest,modelo = arbol$clasificadorBalanced,metodo = metodo,obtenerProb=TRUE)
    
    probabisNew<-(attr(prediccionRes,"probabilities"))
    if(is.null(probabisNew)){
      probabisNew<-prediccionRes
    }
    
    
    #Guardo el valor de la probabilidad en el dataframe
    for(i in 1:ncol(probabisNew)){
      fila <- nrow(probabisList) + 1
      probabisList[fila,] <-list(as.character(arbol$name),as.character(colnames(probabisNew)[i]),as.numeric(probabisNew[i]))
    }
    
    for(i in 1:nrohijos){
      
      #Valores del Hijo
      nodoHijo<-arbol$children[[i]]
      
      #Analizo al Hijo
      resultados<-topDownBottomUpTestingB(datoTest = datoTest,arbol = nodoHijo,metodo = metodo,probabisList = probabisList,resultados = resultados)
      
    }
    
  }else{
    #Es Hoja
    labelHoja<-paste(arbol$Get('name', filterFun = isLeaf))
    
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

ObtenerProbClasePoten <- function(probabisList,labelHoja,probAnt=1){
  
  #Busco la existencia clase
  listTemp<-probabisList[probabisList$nodo2==labelHoja,]
  
  
   if(nrow(listTemp)==1){
    
      registro <- listTemp[1,]
      probabilidad<-probAnt*registro$probabilidad
    
    }
    
    if(nrow(listTemp)>1){
    
      # for(i in 1:nrow(listTemp)) {
      registro_0 <- listTemp[1,]
      registro_1 <- listTemp[2,]
      
      Prob_A<-registro_0$probabilidad*as.numeric(registro_0$nroInstancias)
      Prob_B<-registro_1$probabilidad*as.numeric(registro_1$nroInstancias)
      
      if(Prob_A>Prob_B){
        registro<-registro_0
        probabilidad<-probAnt*registro_0$probabilidad
      }
      if(Prob_B>Prob_A){
        registro<-registro_1
        probabilidad<-probAnt*registro_1$probabilidad
      }
      if(Prob_A==Prob_B){
        registro<-registro_0
        probabilidad<-probAnt*registro_0$probabilidad
        
      }
      
    }
    
    if(registro$nodo1=="Root"){
    
    }else{
      probabilidad<-ObtenerProbClase(probabisList = probabisList,labelHoja=registro$nodo1,probAnt = probabilidad)
    }
  # }
  
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
    resultadosIns<-topDownBottomUpTesting(datoTest = testing[i,],arbol = jerarquiaClases,metodo = metodo)
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


prediccionConJerarquiaProbsPoten<-function(jerarquiaClases,testing,metodo,obtenerClases=FALSE){
  
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
    resultadosIns<-topDownBottomUpTestingPoten(datoTest = testing[i,],arbol = jerarquiaClases,metodo = metodo)
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
    resultadosIns<-topDownBottomUpTesting(datoTest = testing[i,],arbol = jerarquiaClases,metodo = metodo)
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

