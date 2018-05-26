
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
    nrofold = numeric(), 
    datasetSel = character(), 
    metClasificacion = character(),
    missingclass = character(),
    acuJer = numeric(),
    opcion = character(),
    topdownType = character(),
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
techniqueClassifOps<-c("C50")#"RPART","nb","SVM-EX","knn","nnet","RF-EX","C50"
nombreDatasetOps<-c("Frog")#"flare","letters","forest","sports",dermatology
balanceOpt<-FALSE #SMOTE

escenario<-"Escenario 1"

#Genero Id Randomico para procesamiento en paralelo
idExperimento<-genRandomString()



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
folds <- createFolds(data$claseAPredecir, k = 10)
minNumFold<-1
maxNumFold<-10
   
for (ik in minNumFold:maxNumFold){

  #ik<-1
  
  indiceStrat <- folds[[ik]]
  totalTrain <- data[-indiceStrat,]
  TestVal <- data[indiceStrat,]

  set.seed(1)
  valIndex <- createDataPartition(TestVal$claseAPredecir, p = .5, list = FALSE, times = 1)
  Test<-TestVal[valIndex,]
  Val<-TestVal[-valIndex,]
  saveRDS(totalTrain,file=paste("rds/data.train_",minNumFold,"_",maxNumFold,"_",nombreDataset,techniqueClassif,"_",idExperimento,".rds",sep = ""))
  saveRDS(Test,file=paste("rds/data.test_",minNumFold,"_",maxNumFold,"_",nombreDataset,techniqueClassif,"_",idExperimento,".rds",sep = ""))
  saveRDS(Val,file=paste("rds/data.val_",minNumFold,"_",maxNumFold,"_",nombreDataset,techniqueClassif,"_",idExperimento,".rds",sep = ""))

  ##########################################################
  #Start here with the data partition previously done
  #This is only to avoid loading the data and making 
  #the partition each time we execute the code while we 
  #are depuring the code
  ##########################################################
  
  totalTrain<-readRDS(paste("rds/data.train_",minNumFold,"_",maxNumFold,"_",nombreDataset,techniqueClassif,"_",idExperimento,".rds",sep = ""))
  Test<-readRDS(paste("rds/data.test_",minNumFold,"_",maxNumFold,"_",nombreDataset,techniqueClassif,"_",idExperimento,".rds",sep = ""))
  Val<-readRDS(paste("rds/data.val_",minNumFold,"_",maxNumFold,"_",nombreDataset,techniqueClassif,"_",idExperimento,".rds",sep = ""))

################################################################################
###Original Framework: we train from the data after removing one class each time 
##and proceed as in the ICCS paper
###############################################################################

#We create n training sets by removing one class eah time. 
#As this dataset has 10 classes we obtain 10 training data
#we use a list to store the 10 training data
u.s <- unique(totalTrain[, "claseAPredecir"])
trainData<-lapply(u.s, function(x) filter(totalTrain, claseAPredecir != x)) 

#Change to factor class column
n=length(trainData)
for (i in (1:n)){
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

saveRDS(allClassDendrogram,file=paste("rds/classDendrograms_",minNumFold,"_",maxNumFold,"_",nombreDataset,techniqueClassif,"_",idExperimento,".rds",sep = ""))

##Generate the hierarchy of classifiers (and a copy for the balanced case)
for (i in (1:n)){
  #funTrainingLNP(allClassHierar[[i]],train[[i]])
  funTrainingLNP3(graph = allClassHierar[[i]],dataTraining = trainData[[i]],tecnicaEntrenamiento = techniqueClassif,probabilitiesOp = probabilityOptions )
}

saveRDS(allClassHierar,file=paste("rds/classHerarchies",minNumFold,"_",maxNumFold,"_",nombreDataset,techniqueClassif,"_",idExperimento,".rds",sep = ""))
allClassHierar<-readRDS(paste("rds/classHerarchies",minNumFold,"_",maxNumFold,"_",nombreDataset,techniqueClassif,"_",idExperimento,".rds",sep = ""))

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

if(escenario=="Escenario 1"){
 predVal = mapply(function(x,y) {list(prediceLibTecnicas(modelo = x,datosTest = rbind(y,Val),metodo = techniqueClassif,obtenerProb = FALSE))}, x=modelT, y=trainData)
 #Calculate the confusion matrices using our method
 allConfMatrixVal=  mapply(function(x,y) {list(confusionMatrix(x,c(as.character(y$claseAPredecir),as.character(Val$claseAPredecir)))$table)}, x=predVal, y=trainData)
 
}
if(escenario=="Escenario 2"){
  predVal<-lapply(modelT, function(x) prediceLibTecnicas(modelo = x,datosTest = Val,metodo = techniqueClassif,obtenerProb = FALSE))
  #Calculate the confusion matrices using our method
  allConfMatrixVal<-lapply(predVal, function(x) confusionMatrix(x,Val$claseAPredecir)$table)

}


#Determine for each missing class which is the  most similar 
#training class from the confusion matrix for the validation set
source('functions.R')
missClassList<- lapply(allConfMatrixVal, function(x) missingClass(x))#list of missing classes
missingClass<-c("method")

for (i in (1:n)){
  #i<-2
  print(paste("Agrega la similar a ",missClassList[i]))
  
  #add to the class hierarchy the missing class as sibling of its most similar class
  tree<-allClassHierar[[i]]
  mis<- missClassList[[i]]
  missingClass<-c(missingClass,mis)
  sim=similarClass(cf = allConfMatrixVal[[i]],class = mis)
  
  if(escenario=="Escenario 1"){
    newT<-rbind(trainData[[i]],Val)
  }
  
  if(escenario=="Escenario 2"){
    newT<-Val
  } 
  
  if(sim=="Root"){ 
    # Option 1
    # Cuando no encuentra un similar lo agrega a la raiz 
    # Es conveniente cuando se tienen los datos originales
    if(escenario=="Escenario 1"){
      tree$AddChild(mis)
      tree$name="cNew"
    }
    
    #Option 2
    
    # Cuando no encuentra un similar creo un nodo padre sobre el nodo root
    # Es conveniente cuando se tienen acceso unicamente a los datos nuevos (Validation)
    
    if(escenario=="Escenario 2"){
      padreRoot <- Node$new("cNew")
      padreRoot$AddChild(mis)
      
      padreRoot$AddChild("Root")
      padreRoot$Root$clasificador <- tree$clasificador
      padreRoot$Root$entrada <- tree$entrada
      padreRoot$Root$salidas <- tree$salidas
      
      addClassTreeToNode(padreRoot$Root,arbolClasificadores = tree)
      tree<-padreRoot
    }
    
    allClassHierar[[i]]<-funTrainingLNP3(graph = tree, dataTraining = newT, tecnicaEntrenamiento = techniqueClassif, probabilitiesOp = probabilityOptions, balanceOpt = balanceOpt,adaptarModelo = TRUE)
    
  }else{
    node<-FindNode(tree,sim)
    node$AddChild(sim)
    node$AddChild(mis)
    node$name="cNew"

    funTrainingLNP3(graph = node$parent, dataTraining = newT, tecnicaEntrenamiento = techniqueClassif, probabilitiesOp = probabilityOptions, balanceOpt = balanceOpt,adaptarModelo = TRUE)
    
  }
  
  
  #add to the tree of classifiers the new classifier
  #funTrainingLNP2(graph = node$parent, dataTraining = Val, tecnicaEntrenamiento = techniqueClassif, probabilitiesOp = probabilityOptions)
  
  #second option: to use the train and validation sets applying SMOTE (if needed) to balance the instances of classes mis and sim
   
  #Del Training filtrar por sim
   # sTrain<-filter(totalTrain, claseAPredecir == sim)
   # nVal<-filter(Val, claseAPredecir == mis)
   # sVal<-filter(Val, claseAPredecir == sim)
   # newT<-rbind(sTrain,nVal,sVal)
                                               
   
   summary(node$parent$cNew$clasificador)
}

print(allClassHierar[[1]]$clasificador)

#REFRAIMING MODEL IS IN: allClassHierar

###########################################################################
###RETRAINING APPROACH
##A new model is learnt from the validation set 
#(using the same learning method as in the reframing approach) and from
##the confusion matrix we learn the class hierarchy and the tree of classifiers
############################################################################


classHierarVal<-{}

for (i in (1:n)){

  #i<-1
  
  if(escenario=="Escenario 1"){
    dataforRetraining<-rbind(trainData[[i]],Val)
  }
  
  if(escenario=="Escenario 2"){
    dataforRetraining<-rbind(Val)
  } 
  
  
  
  
  modelValT<-entrenarLibTenicas(trainData = dataforRetraining,metodo = techniqueClassif,withProbabilities = FALSE)
  
  #Obtain the predictions for the validation set
  predValT<- applyModel(model = modelValT, set = dataforRetraining,metodo = techniqueClassif,obtenerProb = FALSE )
  
  #Calculate the confusion matrix for the validation set
  confMatrixVal<-confusionMatrix(predValT, dataforRetraining$claseAPredecir)$table
  
  #Calculate the distance matrix using our method for the Test set
  distMatrixVal<-mconfusion2oMDiferencias(confMatrixVal,"propio")
  
  #Generate the class hierarchy using our semi-metric, hierarchical clustering and complete linkage distance.
  #Each element is a list containing the compressed class hierarchy and the original one
  
  classHierarVal[i]<-classHierachy(distMatrixVal,linkage="complete")
  
  #Generate the tree of classifiers
  funTrainingLNP3(graph = classHierarVal[i][[1]], dataTraining = dataforRetraining, tecnicaEntrenamiento = techniqueClassif, probabilitiesOp = probabilityOptions)

}

#RETRAINING MODEL IS IN: classHierarVal

#############################################################################################################
###APPLICATION OF THE THREE APPROACHES TO THE TEST SET (RETRAINING, REFRAIMING, REFRAIMING+BALANCED)
#############################################################################################################

real<-factor(Test$claseAPredecir)
m=nrow(Test)

# #Apply the REFRAMING tree of classifiers to the Test set
# #--------------------------------------------------------
predReframing<-list()
cfmatrixReframing<-list()
headRes<-as.character(u.s)
AccReframing<-c()

for (i in (1:n)){
   pred<-c()
  for (j in (1:m)){


    if(topDownType=="TopDown"){
    #Sin Probabilidades (TopDown)
    #predclass<-topDownTesting(Test[j,], allClassHierar[[i]])
    predclass<-topDownTesting(datoTest = Test[j,],graph =  allClassHierar[[i]],metodo = techniqueClassif)

    }else if (topDownType=="TopDown-BottomUp"){
    #Con Probabilidades (TopDown-BottomUp)
    predclass<-prediccionConJerarquiaProbs(jerarquiaClases = allClassHierar[[i]],testing = Test[j,],metodo = techniqueClassif,obtenerClases = TRUE )
    }


    pred<-c(pred,predclass)
  }
  predReframing[[i]]<-pred

  prediction <- factor(predReframing[[i]],levels = levels(real))
  cfmatrixReframing[[i]]<-confusionMatrix(prediction,real)
  AccReframing<-c(AccReframing,cfmatrixReframing[[i]]$overall[1])
}

names(AccReframing)<-headRes

#Apply the REFRAMING BALANCED tree of classifiers to the Test set
#----------------------------------------------------------------
# predReframingB<-list()
# cfmatrixReframingB<-list()
# headRes<-as.character(u.s)
# AccReframingB<-c()
# for (i in (1:n)){
#   pred2<-c()
#   for (j in (1:m)){
#     
#     if(topDownType=="TopDown"){
#       #Sin Probabilidades (TopDown)
#       predclass2<-topDownTesting2(datoTest = Test[j,],graph =  allClassHierar[[i]],metodo = techniqueClassif)
#       
#     }else if (topDownType=="TopDown-BottomUp"){
#       #Con Probabilidades (TopDown-BottomUp)
#       predclass<-prediccionConJerarquiaProbsB(jerarquiaClases = allClassHierar[[i]],testing = Test[j,],metodo = techniqueClassif,obtenerClases = TRUE )
#     }
#     
#     
#     pred2<-c(pred2,predclass2)
#   }
#   predReframingB[[i]]<-pred2
# 
#   prediction2 <- factor(predReframingB[[i]],levels = levels(real))
#   cfmatrixReframingB[[i]]<-confusionMatrix(prediction2,real)
#   AccReframingB<-c(AccReframingB,cfmatrixReframingB[[i]]$overall[1])
# }
# names(AccReframingB)<-headRes


#Apply the RETRAINING tree of classifiers to the Test set
#---------------------------------------------------------
AccRetraining <- c()

for (i in (1:n)){

  predRetraining<-c()
  for (j in (1:m)){
    
    if(topDownType=="TopDown"){
      #Sin Probabilidades (TopDown)
      predclass<-topDownTesting(datoTest = Test[j,], graph =  classHierarVal[i][[1]], metodo = techniqueClassif)
      
    }else if (topDownType=="TopDown-BottomUp"){
      #Con Probabilidades (TopDown-BottomUp)
      predclass<-prediccionConJerarquiaProbs(testing = Test[j,],jerarquiaClases =  classHierarVal[i][[1]],metodo = techniqueClassif,obtenerClases = TRUE)
    }
    
    predRetraining<-c(predRetraining,predclass)
  }
  predicts <- factor(predRetraining,levels = levels(real))
  cfmatrixRetraining<-confusionMatrix(predicts,real)
  AccRetraining[i]<-cfmatrixRetraining$overall[[1]]

}
names(AccRetraining)<-headRes

###########################################################################
#Save the accuracy results in a csv file
###########################################################################

#Guardo Resultados en la tabla de resultados:

#Guardo Datos de Refraiming
for(i in 1:n){
    fila <- nrow(resultados) + 1
    missingClass<-names(AccReframing[i])
    accuracyValue<-AccReframing[[i]]
    opcionTecnica<-"Reframing"
    resultados[fila,] <-
      list(
        ik,nombreDataset, techniqueClassif,
        missingClass,accuracyValue,opcionTecnica,
        topDownType
      )
}

#Guardo Datos de Refraiming Balanced
# for(i in 1:n){
#   fila <- nrow(resultados) + 1
#   missingClass<-names(AccReframingB[i])
#   accuracyValue<-AccReframingB[[i]]
#   opcionTecnica<-"ReframingBalanced"
#   resultados[fila,] <-
#     list(
#       ik,nombreDataset, techniqueClassif,
#       missingClass, accuracyValue, opcionTecnica,
#       topDownType
#     )
# }

#Guardo Datos de Retraining
  
 

  
  for (i in (1:n)){
    
    fila <- nrow(resultados) + 1
    missingClass<-names(AccRetraining[i])
    accuracyValue<-AccRetraining[i]
    opcionTecnica<-"Retraining"
    
    
    resultados[fila,] <-
      list(
        ik,nombreDataset, techniqueClassif,
        missingClass, accuracyValue,opcionTecnica,
        topDownType
      )
  
  }


##save as png files  the dendrograms learnt from the flat classifiers 

# g<-getwd()
# for (i in (1:n)){
#   name<-paste("Flat_Missing_",ik,u.s[i],sep="")
#   file<-paste(g,"/Jerarquias/",name,".png",sep="")
#   png(file)
#   hcd = as.dendrogram(allClassDendrogram[[i]])
#   plot(hcd,main=name)
#   dev.off()
# 
# }

##save as png files  the dendrograms learnt from the refraime classifiers 
# 
# g<-getwd()
# for (i in (1:n)){
#   name<-paste("Refraime_Missing_",ik,u.s[i],sep="")
#   file<-paste(g,"/Jerarquias/",name,".pdf",sep="")
#   pdf(file)
#   plot(allClassHierar[[i]],main=name)
#   dev.off()
# 
# }
# 


###save as pdf files the confusion matrices given by the flat classifiers for the Validation set 
# for (i in (1:n)){
#   file<-paste(g,"/Jerarquias/","ConfMat_Val_",ik,u.s[i],".pdf",sep="")
#   pdf(file)
#   textplot(t(allConfMatrixVal[[i]]))
#   dev.off()
# }


##save as pdf files the dendrograms learnt using the classifier trained with the validation set
# name<-paste("Validation Hierarchy (retraining)")
# file<-paste(g,"/Jerarquias/","val_hierarchy_",ik,".png",sep="")
# png(file)
# hcd = as.dendrogram(classHierarVal[[2]])
# plot(hcd,main=name)
# dev.off()
  
  ExperimentoLabel<-"Esc"
  if(escenario=="Escenario 1"){
    ExperimentoLabel<-paste(ExperimentoLabel,"1","_")
  }
  if(escenario=="Escenario 2"){
    ExperimentoLabel<-paste(ExperimentoLabel,"2","_")
  }

write.csv(
  resultados, paste(
    getwd(),"/resultados/",ExperimentoLabel,as.character(format(Sys.time(), "%d%b%y_%H%M")),"_Exp_",ik,"_",minNumFold,"_",maxNumFold,"_",nombreDataset,techniqueClassif,topDownType,".csv"
    ,sep = ""
  )
)

# Elimino los archivos temporales RDS
listaArchivosRDS<-list.files("rds/")
listaArchivosRDS[grepl(idExperimento,listaArchivosRDS)]
for(archivoRDS in listaArchivosRDS){
  file.remove(paste("rds/",archivoRDS,sep = ""))
}         


}
}
}
}

#Borro todas las variables excepto la que contiene los resultados de los experimentos
# listaValEntorno<-ls()
# rm(list = listaValEntorno[!grepl("resultados",listaValEntorno)])




