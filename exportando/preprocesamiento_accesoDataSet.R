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
    
    
    etiquetas<-flare$claseAPredecir
    flare[] <- lapply(flare, function(x) as.numeric(as.character(x)))
    flare$claseAPredecir<-etiquetas
    
    
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
  
  
  
  
  #Fuente de Datos Dermatology
  if(nombreDataSet=="dermatology"){
    dermatology <- read.csv("data/dermatology.txt", header=FALSE, stringsAsFactors=FALSE)
    
    colnames(dermatology)[colnames(dermatology)=="V35"] <- "claseAPredecir"
    
    #Reemplazando por el nombre de las clases
    x<-dermatology$claseAPredecir
    x[x==1]<-"psoriasis"
    x[x==2]<-"seboreic dermatitis"
    x[x==3]<-"lichen planus"
    x[x==4]<-"pityriasis rosea"
    x[x==5]<-"cronic dermatitis"
    x[x==6]<-"pityriasis rubra pilaris"
    dermatology$claseAPredecir<-x
    dermatology$claseAPredecir<-factor(dermatology$claseAPredecir)
    
    #Atributo Edad
    dermatology$V34 <- as.numeric(as.character(dermatology$V34))
    dermatology <- dermatology[complete.cases(dermatology),]
    
    
    dataSetUse<-dermatology
    return(dataSetUse)
  }
  
  
  
  # Ejemplo con el Dataset Segmentation
  if(nombreDataSet=="segmentation"){
    segmentationTrain <- read.csv("data/segmentation.data", stringsAsFactors=FALSE)
    segmentationTest <- read.csv("data/segmentation.test", stringsAsFactors=FALSE)
    
    #Unifico los datos en un dataset
    segmentation<-rbind(segmentationTrain,segmentationTest)
    
    
    colnames(segmentation)[colnames(segmentation)=="CLASS"] <- "claseAPredecir"
    
    #Operaciones de Descripcion de datos
    segmentation <- segmentation[complete.cases(segmentation),]
    
    
    #Creo como factor el nombre de las clases
    segmentation$claseAPredecir<-factor(segmentation$claseAPredecir)
    
    # Muestro la composicion de los datos
    #summary(segmentation)
    #str(segmentation)
    
    #Quito variables que no ayudan a la clasificacion
    # segmentation <- segmentation[,(!names(segmentation) %in% c("REGION.PIXEL.COUNT"))]
    
    #Muestro la relacion entre variables a traves de un dendograma:
    #     cc <- cor(segmentation[2:19],use="pairwise",method="pearson")
    #     thc <- hclust(dist(cc), method="average")
    #     dn <- as.dendrogram(thc)
    #     plot(dn, horiz = TRUE)
    
    dataSetUse<-segmentation
    
    return(dataSetUse)
  }
  
  
  #svmguide4.sdx
  #Original data: an application on traffic light signals from Georges Bonga at University of Applied Sciences, Berlin. 
  #mnist
  if(nombreDataSet=="svmguide4"){
    source('archivoScala_to_dataframe.R')
    
    #Obtengo el archivo de formato Scala a R
    datosOriginal <- read.delim("data/svmguide4.sdx", header=FALSE, stringsAsFactors=FALSE)
    
    #Se toma las primeras 2 posiciones como la clase
    #Tomar en cuenta si son mas de 10 clases del problema de clasificacion
    clases<-substr(datosOriginal$V1,1,2)
    datosOriginal['claseAPredecir']<-clases
    
    #Leer archivos formato libsvm
    
    # Saco una muestra de los datos
    #Obtener una Muestra (Se realizo inicialmente)
    #datosOriginal<-datosOriginal[sample(nrow(datosOriginal),size=300, replace=FALSE),]
    
    #Cortar los primeros registros
    #Asi se presenta en la informacion del dataset y los estudios previos, tambien de esta forma las clases estan balanceadas.
    #mnist<-datosOriginal[1:11340,]
    
    
    mnist<-datosOriginal
    
    mnist<-as.data.frame(mnist,stringsAsFactors=FALSE)
    
    #Defino la estructura de la base de datos
    dataFrameTemp <- data.frame(claseAPredecir=character())
    #Agrego dinamicamente las columnas
    nroColumnas<-10
    for(i in 1:nroColumnas){
      dataFrameTemp[,paste("V",i,sep = "")]<-numeric()  
    }
    
    
    ptm <- proc.time()
    mnist <- cargarDatosScala(mnist,dataFrameTemp)
    (proc.time() - ptm)
    
    mnist <- mnist[complete.cases(mnist),]
    
    dataSetUse<-mnist
    
    #Realizo un muestreo por categoria
    #     categorias<-levels(dataSetUse$claseAPredecir)
    #     nuevodf<-dataSetUse[dataSetUse$claseAPredecir=="cualquier",]
    #     limitedeRegistros<-300
    #     for(i in 1:length(categorias)){
    #       if(nrow(dataSetUse[dataSetUse$claseAPredecir==categorias[i],])>limitedeRegistros){
    #         dfxcat<-dataSetUse[dataSetUse$claseAPredecir==categorias[i],]
    #         nuevodf<-rbind(nuevodf,dfxcat[sample(nrow(dfxcat),size=limitedeRegistros, replace=FALSE),]) 
    #       }else{
    #         nuevodf<-rbind(nuevodf,dataSetUse[dataSetUse$claseAPredecir==categorias[i],])
    #         
    #       }
    #     }
    #     dataSetUse<-nuevodf
    #     
    #Pongo un nombre a las clases
    dataSetUse$claseAPredecir<-as.character(dataSetUse$claseAPredecir)
    
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='-1']<-'A'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='-2']<-'B'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='-3']<-'C'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='1']<-'D'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='2']<-'E'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='3']<-'F'
    dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
    
    dataSetUse <- dataSetUse[complete.cases(dataSetUse),]
    
    
    #Quito las columnas sin informacion
    #     dataSetUse <- dataSetUse[,(!names(dataSetUse) %in% c("V21"))]
    #     dataSetUse <- dataSetUse[,(!names(dataSetUse) %in% c("V22"))]
    #     dataSetUse <- dataSetUse[,(!names(dataSetUse) %in% c("V23"))]
    #     dataSetUse <- dataSetUse[,(!names(dataSetUse) %in% c("V29"))]
    #     dataSetUse <- dataSetUse[,(!names(dataSetUse) %in% c("V39"))]
    #     dataSetUse <- dataSetUse[,(!names(dataSetUse) %in% c("V50"))]
    #     dataSetUse <- dataSetUse[,(!names(dataSetUse) %in% c("V42"))]
    
    return(dataSetUse)
    
  }
  
  # # Ejemplo con el Dataset Pendigits
  if(nombreDataSet=="pendigits"){
    pendigits <- read.csv("data/pendigits.tra", header=FALSE, stringsAsFactors=FALSE)
    
    colnames(pendigits)[colnames(pendigits)=="V17"] <- "claseAPredecir"
    
    x<-as.character(pendigits$claseAPredecir)
    x[x==0]<-"a"
    x[x==1]<-"b"
    x[x==2]<-"c"
    x[x==3]<-"d"
    x[x==4]<-"e"
    x[x==5]<-"f"
    x[x==6]<-"g"
    x[x==7]<-"h"
    x[x==8]<-"i"
    x[x==9]<-"j"
    
    pendigits$claseAPredecir<-x
    pendigits$claseAPredecir<-factor(pendigits$claseAPredecir)
    
    
    
    # Asigno los nombres de las columnas
    
    pendigits$claseAPredecir<-factor(pendigits$claseAPredecir)
    
    #Operaciones de Descripcion de datos
    pendigits <- pendigits[complete.cases(pendigits),]
    
    # Muestro la composicion de los datos
    #summary(pendigits)
    #str(pendigits)
    
    
    #Muestro la relacion entre variables a traves de un dendograma:
    #     cc <- cor(pendigits[1:16],use="pairwise",method="pearson")
    #     thc <- hclust(dist(cc), method="average")
    #     dn <- as.dendrogram(thc)
    #     plot(dn, horiz = TRUE)
    
    
    #Dejo todas las variables como factores
    variablesTemp<-names(pendigits)
    for(x in 1:16){
      pendigits[,x]<-as.integer(pendigits[,x])
    }
    
    
    
    
    
    
    dataSetUse<-pendigits
    
    # 
    # 
    # if(nrow(dataSetUse)>1000){  
    #   
    #   #Separo las filas que tienen muy poco ejemplos
    #   ttable<-table(dataSetUse$claseAPredecir)
    #   selectCat<-names(ttable[which(ttable<100)])
    #   dataSetUse_1Part<-dataSetUse[dataSetUse$claseAPredecir %in% selectCat,]
    #   
    #   #De los otros valores obtengo los datos obtengo los registros de dataSetUse
    #   dataSetUse_2Part<-dataSetUse[!dataSetUse$claseAPredecir %in% selectCat,]
    #   
    #   dataSetUse_2Part$claseAPredecir<-factor(dataSetUse_2Part$claseAPredecir)
    #   levels(dataSetUse$claseAPredecir)
    #   
    #   #Obtengo el 5% del total por clase
    #   library(caret)
    #   selTest <- createDataPartition(dataSetUse_2Part$claseAPredecir, p = 0.20, list = FALSE)
    #   dataSetUse_2Part <- dataSetUse_2Part[ selTest,]
    #   
    #   #Junto todas las partes
    #   dataSetUse<-rbind(dataSetUse_1Part,dataSetUse_2Part)
    #   dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)  
    #   #filtTesting<-filtTesting[sample(nrow(filtTesting), size = 1000, replace = FALSE),]
    # }
    
    dataSetUse <- dataSetUse[complete.cases(dataSetUse),]
    
    dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
    
    
    
    return(dataSetUse)
  }  
  
  
  if(nombreDataSet=="vertebral"){
    
    column_2C <- read.table("data/column_2C.dat", quote="\"", comment.char="")
    column_3C <- read.table("data/column_3C.dat", quote="\"", comment.char="")
    vertebral<-rbind(column_2C,column_3C)
    
    colnames(vertebral)[colnames(vertebral)=="V7"] <- "claseAPredecir"
    
    
    #summary(vertebral)
    dataSetUse<-vertebral
    dataSetUse <- dataSetUse[complete.cases(dataSetUse),]
    return(dataSetUse)
    
  }
  
  
  if(nombreDataSet=="zoo"){
    
    # Obtencion de los Datos
    zoo <- read.csv("data/zoo.data", header=FALSE,stringsAsFactors=FALSE)
    
    # Asigno los nombres de las columnas
    colnames(zoo)<-c("nombre","pelo","plumas","huevos","leche","aereo","acuatico","depredador","dentada","espinadorsal","respira","venenosa","aletas","nropiernas","cola","domestico","catsize","claseAPredecir")
    
    #Pongo los nombres a las clases
    zoo$claseAPredecir[zoo$claseAPredecir=='1']<-'group1'
    zoo$claseAPredecir[zoo$claseAPredecir=='2']<-'group2'
    zoo$claseAPredecir[zoo$claseAPredecir=='3']<-'group3'
    zoo$claseAPredecir[zoo$claseAPredecir=='4']<-'group4'
    zoo$claseAPredecir[zoo$claseAPredecir=='5']<-'group5'
    zoo$claseAPredecir[zoo$claseAPredecir=='6']<-'group6'
    zoo$claseAPredecir[zoo$claseAPredecir=='7']<-'group7'
    zoo$claseAPredecir<-factor(zoo$claseAPredecir)
    
    #Quito las columnas sin informacion
    zoo <- zoo[,(!names(zoo) %in% c("nombre"))]
    
    
    dataSetUse<-zoo
    return(dataSetUse)
  }
  
  
  
}

