# args=commandArgs(trailingOnly=TRUE)
# listEscenario=c(args[1])
# nombreDatasetOps=c(args[2])
# techniqueClassifOps=c(args[3])
# minNumCicles=as.numeric(args[4])
# maxNumCicles=as.numeric(args[5])
# etiquetaEspecifica<-""
# Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

nombreDatasetOps<-c("texture")#glass vehicle  optdigits
#"flare","letters","forest","sports",dermatology
techniqueClassifOps<-c("SVM-EX")#"RPART","nb","SVM-EX","knn","nnet","RF-EX","C50"
listEscenario<-c("Escenario_1")
minNumCicles<-9
maxNumCicles<-9
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


    # if(topDownType=="TopDown"){
    # #Sin Probabilidades (TopDown)
    # #predclass<-topDownTesting(Test[j,], allClassHierar[[i]])
    # predclass<-topDownTesting(datoTest = dataSetTest[j,],arbol =  allClassHierar[[i]],metodo = techniqueClassif)
    # 
    # }else if (topDownType=="TopDown-BottomUp"){
    #Con Probabilidades (TopDown-BottomUp)
    predclass<-prediccionConJerarquiaProbs(jerarquiaClases = allClassHierar[[i]],testing = dataSetTest[j,],metodo = techniqueClassif,obtenerClases = TRUE )
    
    if(j%%100==0){
      print(j)
    }
    
    # }


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
        topDownType
        
       
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
ifelse(!dir.exists(file.path(paste(dirRaiz,"/resultados_O",sep = ""), escenario)), dir.create(file.path(paste(dirRaiz,"/resultados_O",sep=''), escenario)), FALSE)
ifelse(!dir.exists(file.path(paste(dirRaiz,"/resultados_O/",escenario,sep = ""), nombreDataset)), dir.create(file.path(paste(dirRaiz,"/resultados_O/",escenario,sep=''), nombreDataset)), FALSE)
ifelse(!dir.exists(file.path(paste(dirRaiz,"/resultados_O/",escenario,"/",nombreDataset,sep = ""), techniqueClassif)), dir.create(file.path(paste(dirRaiz,"/resultados_O/",escenario,"/",nombreDataset,sep=''), techniqueClassif)), FALSE)
dirResultados<-paste(dirRaiz,"/resultados_O/",escenario,"/",nombreDataset,"/",techniqueClassif,sep = "")


write.csv(
  resultados, paste(
    dirResultados,"/",ExperimentoLabel,as.character(format(Sys.time(), "%d%b%y_%H%M")),"_Exp_",ik,"_",minNumCicles,"_",maxNumCicles,"_",nombreDataset,techniqueClassif,topDownType,".csv"
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


