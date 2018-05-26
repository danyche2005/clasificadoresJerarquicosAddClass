
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
    stringsAsFactors = FALSE
  )

##############################################
# Prepare the experiments
##############################################

#Creo 10 (nfolds) Folds Cross Validation
library(caret)

#Parametros
#---------------------------------------------
techniqueClassifOps<-c("SVM-EX")#"RPART","NB-EX","SVM-EX","knn","nnet","RF-EX","C50"
nombreDatasetOps<-c("Frogs")#"flare","letters","forest","sports",dermatology

#Genero Id Randomico para procesamiento en paralelo
idExperimento<-genRandomString()

for (nombreDataset in nombreDatasetOps) {
  #  nombreDataset <- nombreDatasetOps[1]
  
  for (techniqueClassif in techniqueClassifOps) {
    #techniqueClassif <- techniqueClassifOps[1]
    
    ##############################################
    #Load the data and make the partitions
    ##############################################
    
    data <- cargarDatos(nombreDataSet = nombreDataset)
    str(data)
    set.seed(123)
    folds <- createFolds(data$claseAPredecir, k = 10)
    minNumFold <- 1
    maxNumFold <- 10
    
    for (ik in minNumFold:maxNumFold) {
      #ik<-1
      
      indiceStrat <- folds[[ik]]
      totalTrain <- data[-indiceStrat, ]
      TestVal <- data[indiceStrat, ]
      
      set.seed(1)
      valIndex <-
        createDataPartition(
          TestVal$claseAPredecir,
          p = .5,
          list = FALSE,
          times = 1
        )
      Test <- TestVal[valIndex, ]
      Val <- TestVal[-valIndex, ]
     
      
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
      n = length(trainData)
      for (i in (1:n)) {
        trainData[[i]]$claseAPredecir = factor(trainData[[i]]$claseAPredecir)
      }
      
      names(trainData) = u.s
      
      headRes <- as.character(u.s)
      
     

      real <- factor(Test$claseAPredecir)
      m = nrow(Test)
      
      AccRetraining <- c()
      
      for (i in (1:n)) {
        #i<-1
        
        dataforRetraining <- rbind(trainData[[i]], Val)
        
        #We train a decision tree for each training data using the classification method selected.
        #We store the 10 models in a list we call modelT
        modelT <-entrenarLibTenicas(
              trainData = dataforRetraining,
              metodo = techniqueClassif,
              withProbabilities = FALSE
            )
        
       
        #Obtain the predictions for the validation set
        predValT <-
          applyModel(
            model =  modelT,
            set = Test,
            metodo = techniqueClassif,
            obtenerProb = FALSE
          )
        
      
        #Ordeno los niveles para que tengan los mismos niveles:
        predValT<-factor(predValT, levels = levels(Test$claseAPredecir))
        
        
        #Calculate the confusion matrix for the validation set
        confMatrixVal <- confusionMatrix(predValT,Test$claseAPredecir)
        
        AccRetrainingTemp <- confMatrixVal$overall[[1]]
        
        #Guardo Datos de Retraining
        
        fila <- nrow(resultados) + 1
        missingClass <- names(trainData)[i]
        accuracyValue <- AccRetrainingTemp
        opcionTecnica <- "Plano"
        
        
        resultados[fila, ] <-
          list(
            ik,
            nombreDataset,
            techniqueClassif,
            missingClass,
            accuracyValue,
            opcionTecnica
            
          )

      }
      
      write.csv(
        resultados,
        paste(
          getwd(),
          "/resultados/",
          as.character(format(Sys.time(), "%d%b%y_%H%M")),
          "_Exp_Plano_",
          ik,
          "_",
          minNumFold,
          "_",
          maxNumFold,
          "_",
          nombreDataset,
          techniqueClassif,
          ".csv"
          ,
          sep = ""
        )
      )
      
    
      
      
    }
  }
  
}
