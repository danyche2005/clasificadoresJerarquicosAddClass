dsetTraining<-trainData[[1]]
dsetTraining$claseAPredecir<-as.character(dsetTraining$claseAPredecir)
dsetTraining[(dsetTraining$claseAPredecir=='Ameerega'|dsetTraining$claseAPredecir=='Dendropsophus'),]$claseAPredecir<-'c11'
dsetTraining[(dsetTraining$claseAPredecir=='Hypsiboas'|dsetTraining$claseAPredecir=='Osteocephalus'),]$claseAPredecir<-'c8'
dsetTraining[(dsetTraining$claseAPredecir=='Leptodactylus'|dsetTraining$claseAPredecir=='Rhinella'),]$claseAPredecir<-'c4'
dsetTraining$claseAPredecir<-factor(dsetTraining$claseAPredecir)

table(dsetTraining$claseAPredecir)

dsetTesting<-rbind(trainData[[i]],dataSetVal)
modeloJerarquicoCla<-modeloJerarquico$clasificador

#Creo el modelo
modeloPlano<-entrenarLibTenicas(trainData = dsetTraining,metodo = techniqueClassif,withProbabilities = FALSE)


#Predigo con el modelo
pred=prediceLibTecnicas(modelo = modeloPlano,datosTest =dsetTesting,metodo = techniqueClassif,obtenerProb = FALSE)
pred<-factor(pred)
nivelesA<-c(levels(pred),levels(dsetTesting$claseAPredecir))


pred = factor(pred,levels = unique(nivelesA))
table(pred)

dsetTesting$claseAPredecir<-factor(dsetTesting$claseAPredecir,levels = unique(nivelesA))
table(dsetTesting$claseAPredecir)


#Calculate the 10 confusion matrices for the 10 training sets
allConfMatrix=confusionMatrix(pred,dsetTesting$claseAPredecir)
