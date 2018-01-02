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
#source('librerias/clasificadorJer_LNP_WithProbs.R')
#source('librerias/libEntText_WithProbs.R')


##############################################
#Load the data and make the partitions
##############################################
nombreDataset<-"sports"
data <- cargarDatos(nombreDataset)

str(data)
set.seed(123)

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
folds <- createFolds(data$claseAPredecir, k = 10)
maxNumFold<-10


for (ik in 1:maxNumFold)
{
  indiceStrat <- folds[[ik]]
  totalTrain <- data[-indiceStrat,]
  TestVal <- data[indiceStrat,]

  set.seed(1)
  valIndex <- createDataPartition(TestVal$claseAPredecir, p = .5, list = FALSE, times = 1)
  Test<-TestVal[valIndex,]
  Val<-TestVal[-valIndex,]
  saveRDS(totalTrain,file="data.train.rds")
  saveRDS(Test,file="data.test.rds")
  saveRDS(Val,file="data.val.rds")

  ##########################################################
  #Start here with the data partition previously done
  #This is only to avoid loading the data and making 
  #the partition each time we execute the code while we 
  #are depuring the code
  ##########################################################
  
  totalTrain<-readRDS("data.train.rds")
  Test<-readRDS("data.test.rds")
  Val<-readRDS("data.val.rds")

################################################################################
###Original Framework: we train from the data after removing one class each time 
##and proceed as in the ICCS paper
###############################################################################

#We create n training sets by removing one class eah time. 
#As this dataset has 10 classes we obtain 10 training data
#we use a list to store the 10 training data
u.s <- unique(totalTrain[, "claseAPredecir"])
train<-lapply(u.s, function(x) filter(totalTrain, claseAPredecir != x))  

#Change to factor class column
n=length(train)
for (i in (1:n)){
  train[[i]]$claseAPredecir=factor(train[[i]]$claseAPredecir)
}

names(train)=u.s

#We train a decision tree for each training data using rpart. 
#We store the 10 models in a list we call modelT
modelT<-lapply(train, function(x) rpart(claseAPredecir~.,data=x))


#Obtain the predictions for the training set
#We store the 10 predictions in a list we call predTrainT
predTrainT=mapply(function(x,y) {list(applyModel(x,y))}, x=modelT, y=train)


#Calculate the 10 confusion matrices for the 10 training sets
allConfMatrix=mapply(function(x,y) {list(confusionMatrix(x,y$claseAPredecir)$table)}, x=predTrainT, y=train)

#Calculate the similarity matrices using our method for the training sets
#Again, we store the 10 matrices in a list called allDistancesMatrix
allDistancesMatrix<-lapply(allConfMatrix, function(x) mconfusion2oMDiferencias(x,"propio"))
names(allDistancesMatrix)<-u.s

#Generate the class hierarchy using our semi-metric, hierarchical clustering and complete linkage distance. We use the method in ICCS using class estimates
#Each element is a list containing the compressed class hierarchy and the original one. This is done using the compression proposed in the ICCS paper.

listClassH<-lapply(allDistancesMatrix, function(x) classHierachy(x,linkage="complete"))
allClassHierar<-lapply(listClassH, function(x) x[[1]])
allClassDendrogram<-lapply(listClassH, function(x) x[[2]])

saveRDS(allClassDendrogram,file="classDendrograms.rds")

##Generate the hierarchy of classifiers (and a copy for the balanced case)
for (i in (1:n)){
  funTrainingLNP(allClassHierar[[i]],train[[i]])
}

saveRDS(allClassHierar,file="classHerarchies.rds")
allClassHierar<-readRDS("classHerarchies.rds")

################################################################################
#REFRAMING APPROACH:
#Apply the flat models to the validation set 
#add the missing class to the class hierarchy
##add the new classifier to the tree of classifiers
##Two versions: keeping the original proportion of 
#classes and applying SMOTE for balanced the set
################################################################################

#Obtain the predictions for the validation set
predVal<-lapply(modelT, function(x) applyModel(x,Val))

#Calculate the confusion matrices using our method
allConfMatrixVal<-lapply(predVal, function(x) confusionMatrix(x,Val$claseAPredecir)$table)


#Determine for each missing class which is the  most similar 
#training class from the confusion matrix for the validation set
source('functions.R')
missClassList<- lapply(allConfMatrixVal, function(x) missingClass(x))#list of missing classes
missingClass<-c("method")

for (i in (1:n)){
  #add to the class hierarchy the missing class as sibling of its most similar class
  tree<-allClassHierar[[i]]
  mis<- missClassList[[i]]
  missingClass<-c(missingClass,mis)
  sim=similarClass(allConfMatrixVal[[i]],mis)
  node<-FindNode(tree,sim)
  node$AddChild(sim)
  node$AddChild(mis)
  node$name="c"
  #add to the tree of classifiers the new classifier
  #funTrainingLNP2(node$parent,Val)
  #second option: to use the train and validation sets applying SMOTE (if needed) to balance the instances of classes mis and sim
  newT<-rbind(train[[i]],Val)
  funTrainingLNP(node$parent,newT)
}

#REFRAIMING MODEL IS IN: allClassHierar

###########################################################################
###RETRAINING APPROACH
##A new model is learnt from the validation set 
#(using the same learning method as in the reframing approach) and from
##the confusion matrix we learn the class hierarchy and the tree of classifiers
############################################################################
modelValT<-rpart(claseAPredecir~.,data=Val)

#Obtain the predictions for the validation set
predValT<- applyModel(modelValT, Val)

#Calculate the confusion matrix for the validation set
confMatrixVal<-confusionMatrix(predValT,Val$claseAPredecir)$table

#Calculate the distance matrix using our method for the Test set
distMatrixVal<-mconfusion2oMDiferencias(confMatrixVal,"propio")

#Generate the class hierarchy using our semi-metric, hierarchical clustering and complete linkage distance.
#Each element is a list containing the compressed class hierarchy and the original one

classHierarVal<-classHierachy(distMatrixVal,linkage="complete")

#Generate the tree of classifiers
funTrainingLNP(classHierarVal[[1]],Val)

#RTRAINING MODEL IS IN: classHierarVal

#############################################################################################################
###APPLICATION OF THE THREE APPROACHES TO THE TEST SET (RETRAINING, REFRAIMING, REFRAIMING+BALANCED)
#############################################################################################################

real<-factor(Test$claseAPredecir)
m=nrow(Test)

#Apply the REFRAMING tree of classifiers to the Test set
#--------------------------------------------------------
predReframing<-list()
cfmatrixReframing<-list()
headRes<-as.character(u.s)
AccReframing<-c()
for (i in (1:n)){
   pred<-c()
  for (j in (1:m)){
    predclass<-topDownTesting(Test[j,], allClassHierar[[i]])
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
predReframingB<-list()
cfmatrixReframingB<-list()
#headRes<-as.character(u.s)
AccReframingB<-c()
for (i in (1:n)){
  pred2<-c()
  for (j in (1:m)){
    predclass2<-topDownTesting2(Test[j,], allClassHierar[[i]])
    pred2<-c(pred2,predclass2)
  }
  predReframingB[[i]]<-pred2
  
  prediction2 <- factor(predReframingB[[i]],levels = levels(real))
  cfmatrixReframingB[[i]]<-confusionMatrix(prediction2,real)
  AccReframingB<-c(AccReframingB,cfmatrixReframingB[[i]]$overall[1])
}
names(AccReframingB)<-headRes


#Apply the RETRAINING tree of classifiers to the Test set
#---------------------------------------------------------
predRetraining<-c()
for (j in (1:m)){
  predclass<-topDownTesting(Test[j,], classHierarVal[[1]])
  predRetraining<-c(predRetraining,predclass)
}
predicts <- factor(predRetraining,levels = levels(real))
cfmatrixRetraining<-confusionMatrix(predicts,real)
AccRetraining<-cfmatrixRetraining$overall[[1]]

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
        ik,nombreDataset,"rpart",
        missingClass,accuracyValue,opcionTecnica
      )
}

#Guardo Datos de Refraiming Balanced
for(i in 1:n){
  fila <- nrow(resultados) + 1
  missingClass<-names(AccReframingB[i])
  accuracyValue<-AccReframingB[[i]]
  opcionTecnica<-"ReframingBalanced"
  resultados[fila,] <-
    list(
      ik,nombreDataset,"rpart",
      missingClass,accuracyValue,opcionTecnica
    )
}

#Guardo Datos de Retraining
  fila <- nrow(resultados) + 1
  missingClass<-""
  accuracyValue<-AccRetraining
  opcionTecnica<-"Retraining"
  resultados[fila,] <-
    list(
      ik,nombreDataset,"rpart",
      missingClass,accuracyValue,opcionTecnica
    )


##save as png files  the dendrograms learnt from the flat classifiers 

g<-getwd()
for (i in (1:n)){
  name<-paste("Flat_Missing_",ik,u.s[i],sep="")
  file<-paste(g,"/Jerarquias/",name,".png",sep="")
  png(file)
  hcd = as.dendrogram(allClassDendrogram[[i]])
  plot(hcd,main=name)
  dev.off()

}

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


}

#Escribo el resultado en un archivo dentro del directorio de resultados.

write.csv(
  resultados, paste(
    getwd(),"/resultados/",as.character(format(Sys.time(), "%d%b%y_%H%M")),"_Exp_",nombreDataset,".csv"
    ,sep = ""
  )
)
