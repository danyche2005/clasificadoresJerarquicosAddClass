# args=commandArgs(trailingOnly=TRUE)
# listEscenario=c(args[1])
# nombreDatasetOps=c(args[2])
# techniqueClassifOps=c(args[3])
# minNumCicles=as.numeric(args[4])
# maxNumCicles=as.numeric(args[5])
# etiquetaEspecifica<-""
# Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

nombreDatasetOps<-c("optdigits")#glass vehicle  optdigits
#"flare","letters","forest","sports",dermatology
techniqueClassifOps<-c("nnet")#"RPART","nb","SVM-EX","knn","nnet","RF-EX","C50"
listEscenario<-c("Escenario_2")
minNumCicles<-10
maxNumCicles<-10
etiquetaEspecifica<-""

#Cargo las librerias
.lib<- c("data.tree","ggvis","shiny","caret","C50","plyr","rpart","dplyr","dendextend","gplots","nnet","igraph","unbalanced")
.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst])
lapply(.lib, require, character.only=TRUE)


#Especificacion de Directorio Raiz:
    if (Sys.info()[1] == "Windows") {
  #Windows
      dirRaiz<-"C:/clasificadoresJerarquicosAddClass"
    }else{
  #MAC
      if (Sys.info()[1] == "Darwin") {
        dirRaiz<-"/Users/deliagabrielagrandajuca/clasificadoresJerarquicosAddClass"
      }else{
  #Linux
        #UPV
        dirRaiz<-"/home/dsilva/clasificadoresJerarquicosAddClass"
        #Ofina
        #dirRaiz<-"/home/daniel/clasificadoresJerarquicosAddClass"
      }
    }
setwd(dirRaiz)

#Requiered Libraries:
source('functions.R')
source('matrizConfusion_to_matrizDiferencias.R')
source('myclasificadorJer_LNP.R')
source('myBalancedClasificadorJer_LNP.R')
source('preprocesamiento_accesoDataSet.R')

#With Probabilities:
source('clasificadorJer_LNP_WithProbs.R')
source('techniquesLibraries_WithProbs.R')



#Contenedor de resultados
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
    
    #Guardo Tiempos
    tiempoEntrenaNuevoCla = numeric(),
    tiempoReentrena = numeric(),
    tiempoReentrenaCreaArbol= numeric(),
    
    stringsAsFactors = FALSE
  )

#Contenedor de Datos Estadisticos
estadisticasTiempos <-
  data.frame(
    nombreMaquina = character(), 
    nrofold = numeric(), 
    datasetSel = character(), 
    metClasificacion = character(),
    escenario = character(),
    tiempoXCiclo= numeric(),
    stringsAsFactors = FALSE
  )

##############################################
# Prepare the experiments
##############################################

#Creo 10 (nfolds) Folds Cross Validation
library(caret)

#Parametros
#---------------------------------------------
topDownTypeOps<-c("TopDown-BottomUp")#"TopDown","TopDown-BottomUp"
balanceOpt<-FALSE #SMOTE



tiemposXCiclos<-list()

#Genero Id Randomico para procesamiento en paralelo
idExperimento<-genRandomString()

for(escenario in listEscenario){
for(nombreDataset in nombreDatasetOps){
for(topDownType in topDownTypeOps){

  #  nombreDataset <- nombreDatasetOps[1]
  #  topDownType <- topDownTypeOps[1]
  
  #Para TopDownBottomUp opciones de crear clasificadores con probabilidades
  if(topDownType=="TopDown-BottomUp"){probabilityOptions<-TRUE}else{probabilityOptions<-FALSE} 
  
for(techniqueClassif in techniqueClassifOps){ 

  #techniqueClassif <- techniqueClassifOps[1]
  
##############################################
#Load the data and make the partitions
##############################################
  
data <- cargarDatos(nombreDataset)
str(data)
set.seed(123)

  
nombreIndex<-paste("indices/",nombreDataset,"_indx.RData",sep = '')

if(!file.exists(nombreIndex)){
listIndexes<-list()
  # 10 Repetitions
  for (ik in 1:10){
    
    partIndex50Perct <- createDataPartition(data$claseAPredecir, p = .5, list = FALSE, times = 1)
    partitionB <- data[-partIndex50Perct,]  #Particion B 50%
    partIndex25Perct <- createDataPartition(partitionB$claseAPredecir, p = .5, list = FALSE, times = 1)
    
    #Guardo los indices de la Repetition para que sea reproducible
    listIndexes[[ik]]<-list(partIndex50Perct,partIndex25Perct)
  }
  
  #Guardo los indices de las particiones
  save(listIndexes, file=paste(nombreIndex))

}

#Cargo los indices de la particiones
load(nombreIndex)	
#print('Carga los indices')



for (ik in minNumCicles:maxNumCicles){
ptm4 <- proc.time()
  
  partIndex50Perct<-listIndexes[[ik]][1][[1]]
  partIndex25Perct<-listIndexes[[ik]][2][[1]]
  
  partitionA <- data[partIndex50Perct,] #Particion A 50%
  partitionB <- data[-partIndex50Perct,]  #Particion B 50%
  
  dataSetTrain<-partitionA #50% of the dataset
  dataSetVal<-partitionB[partIndex25Perct,] #25% of dataset
  dataSetTest<-partitionB[-partIndex25Perct,] #25% of dataset

################################################################################
###Original Framework: we train from the data after removing one class each time 
##and proceed as in the ICCS paper
###############################################################################

#We create n training sets by removing one class eah time. 
#As this dataset has 10 classes we obtain 10 training data
#we use a list to store the 10 training data
u.s <- levels(factor(dataSetTrain$claseAPredecir))
trainData<-lapply(u.s, function(x) filter(dataSetTrain, claseAPredecir != x)) 

#Change to factor class column
n=length(trainData)

if(!(etiquetaEspecifica=="")){
  n_i<-which(u.s==etiquetaEspecifica)
  n_end<-which(u.s==etiquetaEspecifica)
}else{
  n_i<-1
  n_end<-n
}

for (i in (n_i:n_end)){
  trainData[[i]]$claseAPredecir=factor(trainData[[i]]$claseAPredecir)
}

names(trainData)=u.s

#We train a decision tree for each training data using the classification method selected. 
#We store the 10 models in a list we call modelT
modelT<-lapply(trainData, function(x) entrenarLibTenicas(trainData = x,metodo = techniqueClassif,withProbabilities = FALSE))


#Obtain the predictions for the training set
#We store the 10 predictions in a list we call predTrainT

predTrainT=mapply(function(x,y) {list(prediceLibTecnicas(modelo = x,datosTest =y,metodo = techniqueClassif,obtenerProb = FALSE))}, x=modelT, y=trainData)


#Calculate the 10 confusion matrices for the 10 training sets
allConfMatrix=mapply(function(x,y) {list(confusionMatrix(x,y$claseAPredecir)$table)}, x=predTrainT, y=trainData)

#Calculate the similarity matrices using our method for the training sets
#Again, we store the 10 matrices in a list called allDistancesMatrix
allDistancesMatrix<-lapply(allConfMatrix, function(x) mconfusion2oMDiferencias(x,"propio"))
names(allDistancesMatrix)<-u.s

#Generate the class hierarchy using our semi-metric, hierarchical clustering and complete linkage distance. We use the method in ICCS using class estimates
#Each element is a list containing the compressed class hierarchy and the original one. This is done using the compression proposed in the ICCS paper.

listClassH<-lapply(allDistancesMatrix, function(x) classHierachy(x,linkage="complete"))
allClassHierar<-lapply(listClassH, function(x) x[[1]])
allClassDendrogram<-lapply(listClassH, function(x) x[[2]])

# saveRDS(allClassDendrogram,file=paste("rds/classDendrograms_",minNumCicles,"_",maxNumCicles,"_",nombreDataset,techniqueClassif,"_",idExperimento,".rds",sep = ""))

##Generate the hierarchy of classifiers (and a copy for the balanced case)
for (i in (n_i:n_end)){
  #funTrainingLNP(allClassHierar[[i]],train[[i]])
  funTrainingLNP3(arbol = allClassHierar[[i]],dataTraining = trainData[[i]],tecnicaEntrenamiento = techniqueClassif,probabilitiesOp = probabilityOptions )
}

# saveRDS(allClassHierar,file=paste("rds/classHerarchies",minNumCicles,"_",maxNumCicles,"_",nombreDataset,techniqueClassif,"_",idExperimento,".rds",sep = ""))
# allClassHierar<-readRDS(paste("rds/classHerarchies",minNumCicles,"_",maxNumCicles,"_",nombreDataset,techniqueClassif,"_",idExperimento,".rds",sep = ""))

################################################################################
#REFRAMING APPROACH:
#Apply the flat models to the validation set 
#add the missing class to the class hierarchy
##add the new classifier to the tree of classifiers
##Two versions: keeping the original proportion of 
#classes and applying SMOTE for balanced the set
################################################################################

#Obtain the predictions for the validation set
#predVal<-lapply(modelT, function(x) prediceLibTecnicas(modelo = x,datosTest = Val,metodo = techniqueClassif,obtenerProb = FALSE))
allConfMatrixVal<-list()
if(escenario=="Escenario_1"){
  for (i in (n_i:n_end)){
    predVal<-c()
    predValT<-c()
    AccuracyVal<-c()
    m=nrow(rbind(trainData[[i]],dataSetVal))
    for (j in (1:m)){
      if(j%%100==0){
        print(j)
      }
      predValT<-prediccionConJerarquiaProbs(jerarquiaClases = allClassHierar[[i]],testing = rbind(trainData[[i]],dataSetVal)[j,],metodo = techniqueClassif,obtenerClases = TRUE )
      predVal<-c(predVal,predValT)
    }
    real<-rbind(trainData[[i]],dataSetVal)$claseAPredecir
    predVal<-factor(predVal,levels = unique(levels(factor(real))))
    allConfMatrixVal[i]<-list(confusionMatrix(predVal,real)$table)
    # AccuracyVal[i]<-cfmatrixRetraining$overall[[1]]
  }
}


if(escenario=="Escenario_2"){
  for (i in (n_i:n_end)){
    predVal<-c()
    predValT<-c()
    AccuracyVal<-c()
    m=nrow(dataSetVal)
    for (j in (1:m)){
      if(j%%100==0){
        print(j)
      }
      predValT<-prediccionConJerarquiaProbs(jerarquiaClases = allClassHierar[[i]],testing = rbind(dataSetVal)[j,],metodo = techniqueClassif,obtenerClases = TRUE )
      predVal<-c(predVal,predValT)
    }
    real<-dataSetVal$claseAPredecir
    predVal<-factor(predVal,levels = unique(levels(factor(real))))
    allConfMatrixVal[i]<-list(confusionMatrix(predVal,real)$table)
    # AccuracyVal[i]<-cfmatrixRetraining$overall[[1]]
  }
}


# if(escenario=="Escenario_1"){
  # predVal1 = mapply(function(x,y) {list(prediceLibTecnicas(modelo = x,datosTest = rbind(y,dataSetVal),metodo = techniqueClassif,obtenerProb = FALSE))}, x=modelT, y=trainData)
  # #Calculate the confusion matrices using our method
  # allConfMatrixVal1=  mapply(function(x,y) {list(confusionMatrix(x,c(as.character(y$claseAPredecir),as.character(dataSetVal$claseAPredecir)))$table)}, x=predVal1, y=trainData)
#  
# }
# if(escenario=="Escenario_2"){
#   predVal<-lapply(modelT, function(x) prediceLibTecnicas(modelo = x,datosTest = dataSetVal,metodo = techniqueClassif,obtenerProb = FALSE))
#   #Calculate the confusion matrices using our method
#   allConfMatrixVal<-lapply(predVal, function(x) confusionMatrix(x,dataSetVal$claseAPredecir)$table)
# 
# }


#Determine for each missing class which is the  most similar 
#training class from the confusion matrix for the validation set
source('functions.R')
missClassList<- lapply(allConfMatrixVal, function(x) missingClass(x))#list of missing classes
missingClass<-c("method")

for (i in (n_i:n_end)){
  #i<-1
  #print(paste("Agrega la similar a ",missClassList[i]))
  
  #add to the class hierarchy the missing class as sibling of its most similar class
  tree<-allClassHierar[[i]]
  mis<- missClassList[[i]]
  missingClass<-c(missingClass,mis)
  sim=similarClass(cf = allConfMatrixVal[[i]],class = mis)
  
  if(escenario=="Escenario_1"){
    newT<-rbind(trainData[[i]],dataSetVal)
  }
  
  if(escenario=="Escenario_2"){
    newT<-dataSetVal
  } 
  
  if(sim=="Root"){ 
    # Option 1
    # Cuando no encuentra un similar lo agrega a la raiz 
    # Es conveniente cuando se tienen los datos originales
    if(escenario=="Escenario_1"){
      tree$AddChild(mis)
      tree$name="cNew"
    }
    
    #Option 2
    
    # Cuando no encuentra un similar creo un nodo padre sobre el nodo root
    # Es conveniente cuando se tienen acceso unicamente a los datos nuevos (Validation)
    
    if(escenario=="Escenario_2"){
      padreRoot <- Node$new("cNew")
      padreRoot$AddChild(mis)
      
      padreRoot$AddChild("Root")
      padreRoot$Root$clasificador <- tree$clasificador
      padreRoot$Root$entrada <- tree$entrada
      padreRoot$Root$salidas <- tree$salidas
      
      addClassTreeToNode(padreRoot$Root,arbolClasificadores = tree)
      tree<-padreRoot
    }
    
    
    ptm1 <- proc.time() 
      allClassHierar[[i]]<-funTrainingLNP3(arbol = tree, dataTraining = newT, tecnicaEntrenamiento = techniqueClassif, probabilitiesOp = probabilityOptions, balanceOpt = balanceOpt,adaptarModelo = TRUE)
    tiempoEntrenaNuevoCla <- (proc.time() - ptm1)[3]
        
  }else{
    node<-FindNode(tree,sim)
    node$AddChild(sim)
    node$AddChild(mis)
    node$name="cNew"
    
    #Pongo la equivalencia
    node$parent$equivalencia<-paste(node$parent$equivalencia,paste(sim,'#','cNew',' ',sep = ''))
  
    datosEntrenamiento<-newT[(newT$claseAPredecir==sim|newT$claseAPredecir==mis),]
    datosEntrenamiento$claseAPredecir<-factor(datosEntrenamiento$claseAPredecir)

    
      # library(DMwR)
      # datosEntrenamiento <- SMOTE(claseAPredecir~..,data=datosEntrenamiento)
      # datosEntrenamiento <- datosEntrenamiento[complete.cases(datosEntrenamiento),]
 
        
    ptm1 <- proc.time() 
    node$clasificador<- entrenarLibTenicas(train = datosEntrenamiento,metodo = techniqueClassif,withProbabilities = probabilityOptions)
    
    tiempoEntrenaNuevoCla <- (proc.time() - ptm1)[3]

    node$entrada<-node$parent$name
    salidasPosibles<-(paste(levels(datosEntrenamiento$claseAPredecir), collapse="#"))
    instancias<-(paste(as.numeric(table(datosEntrenamiento$claseAPredecir)), collapse="#"))
    node$salidas<-salidasPosibles
    node$instancias<-instancias
    #funTrainingLNP3(arbol = node$parent, dataTraining = newT, tecnicaEntrenamiento = techniqueClassif, probabilitiesOp = probabilityOptions, balanceOpt = balanceOpt,adaptarModelo = TRUE)
    
    
    #Agrego mas informacion al arbol
    datosAdaptacion<-newT[(!(newT$claseAPredecir==sim)|!(newT$claseAPredecir==mis)),]
    #potenciarModelo(arbol = tree,dataTraining = datosAdaptacion,tecnicaEntrenamiento = techniqueClassif,probabilitiesOp = probabilityOptions)
    plot(tree)
    
  }
  
  
  
  #Creacion de nuevos clasificadores con el resto de datos obtenidos del validation
  
  #Recorro topdown el arbol
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------

  
  #Si el nodo tiene hijos creo el clasificador por ese nodo padre.
  

        
  
  #------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------
  
  #add to the tree of classifiers the new classifier
  #funTrainingLNP2(arbol = node$parent, dataTraining = Val, tecnicaEntrenamiento = techniqueClassif, probabilitiesOp = probabilityOptions)
  
  #second option: to use the train and validation sets applying SMOTE (if needed) to balance the instances of classes mis and sim
   
  #Del Training filtrar por sim
   # sTrain<-filter(dataSetTrain, claseAPredecir == sim)
   # nVal<-filter(Val, claseAPredecir == mis)
   # sVal<-filter(Val, claseAPredecir == sim)
   # newT<-rbind(sTrain,nVal,sVal)
                                               
   
   summary(node$parent$cNew$clasificador)
}

#print(allClassHierar[[1]]$clasificador)

#REFRAIMING MODEL IS IN: allClassHierar

###########################################################################
###RETRAINING APPROACH
##A new model is learnt from the validation set 
#(using the same learning method as in the reframing approach) and from
##the confusion matrix we learn the class hierarchy and the tree of classifiers
############################################################################


classHierarVal<-{}

for (i in (n_i:n_end)){

  #i<-1
  
  if(escenario=="Escenario_1"){
    dataforRetraining<-rbind(trainData[[i]],dataSetVal)
  }
  
  if(escenario=="Escenario_2"){
    dataforRetraining<-rbind(dataSetVal)
  } 
  
  
  ptm2 <- proc.time() 
  
  modelValT<-entrenarLibTenicas(trainData = dataforRetraining,metodo = techniqueClassif,withProbabilities = FALSE)
  
  #Obtain the predictions for the validation set
  predValT<- applyModel(model = modelValT, set = dataforRetraining,metodo = techniqueClassif,obtenerProb = FALSE )
  
  #Calculate the confusion matrix for the validation set
  confMatrixVal<-confusionMatrix(predValT, dataforRetraining$claseAPredecir)$table
  
  #Calculate the distance matrix using our method for the Test set
  distMatrixVal<-mconfusion2oMDiferencias(confMatrixVal,"propio")
  
  #Generate the class hierarchy using our semi-metric, hierarchical clustering and complete linkage distance.
  #Each element is a list containing the compressed class hierarchy and the original one
  
  classHierarVal[i]<-classHierachy(matrix = distMatrixVal,linkage="complete")
  
  #Generate the tree of classifiers
  ptm3 <- proc.time()
   funTrainingLNP3(arbol = classHierarVal[i][[1]], dataTraining = dataforRetraining, tecnicaEntrenamiento = techniqueClassif, probabilitiesOp = probabilityOptions)
  tiempoReentrenaCreaArbol <- (proc.time() - ptm3)[3]
  
  
  tiempoReentrena <- (proc.time() - ptm2)[3]
  
}

#RETRAINING MODEL IS IN: classHierarVal



########################################################################################
#Guardo Jerarquias
########################################################################################

##save as png files  the dendrograms learnt from the refraime classifiers 

#Creo carpetas de Jerarquias si no existen
ifelse(!dir.exists(file.path(paste(dirRaiz,"/Jerarquias",sep = ""), escenario)), dir.create(file.path(paste(dirRaiz,"/Jerarquias",sep=''), escenario)), FALSE)
ifelse(!dir.exists(file.path(paste(dirRaiz,"/Jerarquias/",escenario,sep = ""), nombreDataset)), dir.create(file.path(paste(dirRaiz,"/Jerarquias/",escenario,sep=''), nombreDataset)), FALSE)
ifelse(!dir.exists(file.path(paste(dirRaiz,"/Jerarquias/",escenario,"/",nombreDataset,sep = ""), techniqueClassif)), dir.create(file.path(paste(dirRaiz,"/Jerarquias/",escenario,"/",nombreDataset,sep=''), techniqueClassif)), FALSE)
for (i in (n_i:n_end)){
  ifelse(!dir.exists(file.path(paste(dirRaiz,"/Jerarquias/",escenario,"/",nombreDataset,"/",techniqueClassif,sep = ""), u.s[i])), dir.create(file.path(paste(dirRaiz,"/Jerarquias/",escenario,"/",nombreDataset,"/",techniqueClassif,sep = ""), u.s[i])), FALSE)
  
  dirHierarchies<-paste(dirRaiz,"/Jerarquias/",escenario,"/",nombreDataset,"/",techniqueClassif,"/",u.s[i],sep = "")
  nombreArchivo<-paste(nombreDataset,"_",techniqueClassif,"_",ik,"_",u.s[i],"_","Refraime",".png",sep="")
  guardarImagenJerarquia(arbol = allClassHierar[[i]],nombreArchivo = nombreArchivo,directorio = dirHierarchies)
  nombreArchivo<-paste(nombreDataset,"_",techniqueClassif,"_",ik,"_",u.s[i],"_","Retrain",".png",sep="")
  guardarImagenJerarquia(arbol = classHierarVal[i][[1]],nombreArchivo = nombreArchivo,directorio = dirHierarchies)
  
}




#############################################################################################################
###APPLICATION OF THE THREE APPROACHES TO THE TEST SET (RETRAINING, REFRAIMING, REFRAIMING+BALANCED)
#############################################################################################################

real<-factor(dataSetTest$claseAPredecir)
m=nrow(dataSetTest)

# #Apply the REFRAMING tree of classifiers to the Test set

predReframing<-list()
cfmatrixReframing<-list()
headRes<-as.character(u.s)
AccReframing<-c()



for (i in (n_i:n_end)){
   pred<-c()
  for (j in (1:m)){


    if(topDownType=="TopDown"){
    #Sin Probabilidades (TopDown)
    #predclass<-topDownTesting(Test[j,], allClassHierar[[i]])
    predclass<-topDownTesting(datoTest = dataSetTest[j,],arbol =  allClassHierar[[i]],metodo = techniqueClassif)

    }else if (topDownType=="TopDown-BottomUp"){
    #Con Probabilidades (TopDown-BottomUp)
    predclass<-prediccionConJerarquiaProbs(jerarquiaClases = allClassHierar[[i]],testing = dataSetTest[j,],metodo = techniqueClassif,obtenerClases = TRUE )
    
    if(j%%100==0){
      print(j)
    }
    
    }


    pred<-c(pred,predclass)
  }
  predReframing[[i]]<-pred

  prediction <- factor(predReframing[[i]],levels = levels(real))
  cfmatrixReframing[[i]]<-confusionMatrix(prediction,real)
  AccReframing<-c(AccReframing,cfmatrixReframing[[i]]$overall[1])
}

for(i in 1:length(AccReframing)){
  names(AccReframing)[i]<-headRes[i]  
}


#Apply the RETRAINING tree of classifiers to the Test set
#---------------------------------------------------------
AccRetraining <- c()

for (i in (n_i:n_end)){

  predRetraining<-c()
  for (j in (1:m)){

    if(topDownType=="TopDown"){
      #Sin Probabilidades (TopDown)
      predclass<-topDownTesting(datoTest = dataSetTest[j,], arbol =  classHierarVal[i][[1]], metodo = techniqueClassif)

    }else if (topDownType=="TopDown-BottomUp"){
      #Con Probabilidades (TopDown-BottomUp)
      predclass<-prediccionConJerarquiaProbs(testing = dataSetTest[j,],jerarquiaClases =  classHierarVal[i][[1]],metodo = techniqueClassif,obtenerClases = TRUE)
    }

    predRetraining<-c(predRetraining,predclass)
  }
  predicts <- factor(predRetraining,levels = levels(real))
  cfmatrixRetraining<-confusionMatrix(predicts,real)
  AccRetraining[i]<-cfmatrixRetraining$overall[[1]]

}

for(i in 1:length(AccRetraining)){
  names(AccRetraining)[i]<-headRes[i]  
}

###########################################################################
#Save the accuracy results in a csv file
###########################################################################

#Guardo Resultados en la tabla de resultados:

#Guardo Datos de Refraiming
for(i in n_i:n_end){
    fila <- nrow(resultados) + 1
    missingClass<-names(AccReframing[i])
    accuracyValue<-AccReframing[[i]]
    opcionTecnica<-"Reframing"
    resultados[fila,] <-
      list(
        escenario,
        ik,nombreDataset, techniqueClassif,
        missingClass,accuracyValue,opcionTecnica,
        topDownType,
        
        tiempoEntrenaNuevoCla,
        tiempoReentrena,
        tiempoReentrenaCreaArbol
      )
}


#Guardo Datos de Retraining

  
  for (i in (n_i:n_end)){

    fila <- nrow(resultados) + 1
    missingClass<-names(AccRetraining[i])
    accuracyValue<-AccRetraining[i]
    opcionTecnica<-"Retraining"
    resultados[fila,] <-
      list(
        escenario,
        ik,nombreDataset, techniqueClassif,
        missingClass, accuracyValue,opcionTecnica,
        topDownType,

        tiempoEntrenaNuevoCla,
        tiempoReentrena,
        tiempoReentrenaCreaArbol
      )
  }



  ExperimentoLabel<-"Esc"
  if(escenario=="Escenario_1"){
    ExperimentoLabel<-paste(ExperimentoLabel,"1","_")
  }
  if(escenario=="Escenario_2"){
    ExperimentoLabel<-paste(ExperimentoLabel,"2","_")
  }
  
#Creo Carpetas si no existen  
ifelse(!dir.exists(file.path(paste(dirRaiz,"/resultados",sep = ""), escenario)), dir.create(file.path(paste(dirRaiz,"/resultados",sep=''), escenario)), FALSE)
ifelse(!dir.exists(file.path(paste(dirRaiz,"/resultados/",escenario,sep = ""), nombreDataset)), dir.create(file.path(paste(dirRaiz,"/resultados/",escenario,sep=''), nombreDataset)), FALSE)
ifelse(!dir.exists(file.path(paste(dirRaiz,"/resultados/",escenario,"/",nombreDataset,sep = ""), techniqueClassif)), dir.create(file.path(paste(dirRaiz,"/resultados/",escenario,"/",nombreDataset,sep=''), techniqueClassif)), FALSE)
dirResultados<-paste(dirRaiz,"/resultados/",escenario,"/",nombreDataset,"/",techniqueClassif,sep = "")


write.csv(
  resultados, paste(
    dirResultados,"/",ExperimentoLabel,as.character(format(Sys.time(), "%d%b%y_%H%M")),"_Exp_",ik,"_",minNumCicles,"_",maxNumCicles,"_",nombreDataset,techniqueClassif,topDownType,".csv"
    ,sep = ""
  )
)




nombreMachine<-Sys.info()[[4]]
tiemposXCiclo<- (proc.time() - ptm4)[3]
fila <- nrow(estadisticasTiempos) + 1
estadisticasTiempos[fila,] <-
    list(
      nombreMachine,ik,nombreDataset, techniqueClassif,
      escenario,tiemposXCiclo
    )

# for(x1 in 1:nrow(estadisticasTiempos)){
#   print(paste("El tiempo de",nombreDataset,"con la tecnica",techniqueClassif,"es",tiemposXCiclos[x1],"."))
# }

ifelse(!dir.exists(file.path(paste(dirRaiz,"/estadisticos",sep = ""), nombreMachine)), dir.create(file.path(paste(dirRaiz,"/estadisticos",sep=''), nombreMachine)), FALSE)
ifelse(!dir.exists(file.path(paste(dirRaiz,"/estadisticos/",nombreMachine,sep = ""), idExperimento)), dir.create(file.path(paste(dirRaiz,"/estadisticos/",nombreMachine,sep=''), idExperimento)), FALSE)
dirEstadisticos<-paste(dirRaiz,"/estadisticos/",nombreMachine,"/",idExperimento,sep = "")

#GuardoValoresEstadisticos de este experimento
write.csv(
  estadisticasTiempos, paste(
    dirEstadisticos,"/",ExperimentoLabel,as.character(format(Sys.time(), "%d%b%y_%H%M")),".csv"
    ,sep = ""
  )
)



}

}
}
}
}

#quit(save = "no")

#Borro todas las variables excepto la que contiene los resultados de los experimentos
# rm(list = ls(envir = globalenv()),envir = globalenv()) #clear Vars from global enviroment
# gc()  #grabage colector
# cat("\014") #clc
# .rs.restartR(afterRestartCommand = "source('MainExperiments.R')") #clear session


