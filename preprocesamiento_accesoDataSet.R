#
#1) Especificacion de Fuente de Datos
# Seleccion una Opcion de Fuente de Datos

#Ejemplo con la base Zoo
cargarDatos<-function(nombreDataSet){
  
  if(nombreDataSet=="sports"){
  # Obtencion de los Datos
     datasetnfilt <- read.csv("data/datasetnfilt.csv", stringsAsFactors=FALSE,header=TRUE,row.names = NULL)
    # Quito columna del ID
     datasetnfilt <- datasetnfilt[,(!names(datasetnfilt) %in% c("X"))]

    #Especifico la variable dependiente
    colnames(datasetnfilt)[colnames(datasetnfilt)=="sport"] <- "claseAPredecir"
    
    datasetnfilt$claseAPredecir<-factor(datasetnfilt$claseAPredecir)
    dataSetUse<-datasetnfilt
    return(dataSetUse)
  }  
  
  
  if(nombreDataSet=="letters"){
  # Obtencion de los Datos
    letter <- read.csv("data/letter-recognition.data", header=FALSE)
    # Asigno los nombres de las columnas
    colnames(letter)[colnames(letter)=="V1"] <- "claseAPredecir"
    
    #Operaciones de Descripcion de datos
    letter1 <- letter[complete.cases(letter),]
    
    #Simplifico la prueba+
    letter$claseAPredecir<-factor(letter$claseAPredecir)
    
    dataSetUse<-letter
    return(dataSetUse)
  }
  

  if(nombreDataSet=="flare"){
    #Obtain dataset train and test from the repository
    flare1 <- read.table("data/flare.data1", quote="\"", comment.char="", stringsAsFactors=FALSE)
    flare2 <- read.table("data/flare.data2", quote="\"", comment.char="", stringsAsFactors=FALSE)
    
    #Unifico los datos en un dataset
    flare<-rbind(flare1,flare2)
    
    # Asigno los nombres de las columnas
    colnames(flare)[colnames(flare)=="V1"] <- "claseAPredecir"
    flare$claseAPredecir<-factor(flare$claseAPredecir)

    #For every unique value in the string column, create a new 1/0 column
    #This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
    for(level in unique(flare$V2)){
      flare[paste("D", level, sep = "")] <- ifelse(flare$V2 == level, 1, 0)
    }
    flare<-flare[,-2]
    flare
    for(level in unique(flare$V3)){
      flare[paste("D", level, sep = "")] <- ifelse(flare$V3 == level, 1, 0)
    }
    flare<-flare[,-2]
    head(flare)
    summary(flare)
    flare <- flare[complete.cases(flare),]
    
    
    dataSetUse<-flare
    return(dataSetUse)
  }
  
  #Ejemplo Forest Type
  if(nombreDataSet=="forest"){
    foresttypeTrain <- read.csv("data/trainingForest.csv")
    foresttypeTest <- read.csv("data/testingForest.csv")
    
    #Unifico los datos en un dataset
    forestType<-rbind(foresttypeTrain,foresttypeTest)
    
    #Especifico la variable dependiente
    colnames(forestType)[colnames(forestType)=="tipo"] <- "claseAPredecir"
    
    #Obtengo los ejemplos con datos incompletos
    forestType <- forestType[complete.cases(forestType),]
    
    dataSetUse<-forestType
    return(dataSetUse)
  }
  
  
}

