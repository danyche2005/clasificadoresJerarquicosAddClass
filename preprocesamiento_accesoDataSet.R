#
#1) Especificacion de Fuente de Datos
# Seleccion una Opcion de Fuente de Datos

#Ejemplo con la base Zoo
cargarDatos<-function(nombreDataSet){
  
  print(paste('Cargando',nombreDataSet))
  
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
  

  

  #Base de datos no viable para experimentos.
  if(nombreDataSet=="flare"){
    #Obtain dataset train and test from the repository
    #flare1 <- read.table("data/flare.data1", quote="\"", comment.char="", stringsAsFactors=FALSE)
    #flare2 <- read.table("data/flare.data2", quote="\"", comment.char="", stringsAsFactors=FALSE)
    
    #Unifico los datos en un dataset
    #flare<-rbind(flare1,flare2)
    #flare<-flare2
    
    library(readr)
    flare <- read_csv("data/flare.dat") #Version del http://sci2s.ugr.es/keel/dataset.php?cod=98   

    colnames(flare)[colnames(flare)=="C-class"] <- "clasec"
    colnames(flare)[colnames(flare)=="M-class"] <- "clasem"
    colnames(flare)[colnames(flare)=="X-class"] <- "clasex"
    
    # Asigno los nombres de las columnas
    colnames(flare)[colnames(flare)=="Class"] <- "claseAPredecir"
    flare$claseAPredecir<-factor(flare$claseAPredecir)
    
    
    #For every unique value in the string column, create a new 1/0 column
    #This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
    for(level in unique(flare$LargestSpotSize)){
      flare[paste("D", level, sep = "")] <- ifelse(flare$LargestSpotSize == level, 1, 0)
    }
    
    for(level in unique(flare$SpotDistribution)){
      flare[paste("S", level, sep = "")] <- ifelse(flare$SpotDistribution == level, 1, 0)
    }
    flare<-flare[,-1]
    flare<-flare[,-1]
    head(flare)
    summary(flare)
    flare <- flare[complete.cases(flare),]
    
    dataSetUse<-flare
    
    levels(flare$claseAPredecir)
    
    
    

    
    
    
    return(dataSetUse)
  }
  
  #Base de datos no viable para experimentos.
  if(nombreDataSet=="abalone"){
    #Obtain dataset train and test from the repository
    #flare1 <- read.table("data/flare.data1", quote="\"", comment.char="", stringsAsFactors=FALSE)
    #flare2 <- read.table("data/flare.data2", quote="\"", comment.char="", stringsAsFactors=FALSE)
    
    #Unifico los datos en un dataset
    #flare<-rbind(flare1,flare2)
    #flare<-flare2
    
    library(readr)
    abalone <- read_csv("data/abalone.dat") #Version del http://sci2s.ugr.es/keel/dataset.php?cod=52
    
    # Asigno los nombres de las columnas
    colnames(abalone)[colnames(abalone)=="Rings"] <- "claseAPredecir"
    abalone$claseAPredecir<-factor(abalone$claseAPredecir)
    
    
    #For every unique value in the string column, create a new 1/0 column
    #This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
    for(level in unique(abalone$Sex)){
      abalone[paste("S", level, sep = "")] <- ifelse(abalone$Sex == level, 1, 0)
    }
    
    abalone<-abalone[,-1]
    head(abalone)
    summary(abalone)
    abalone <- abalone[complete.cases(abalone),]
    
    dataSetUse<-abalone
    
    
    
    return(dataSetUse)
  }
  
  
  #Base de datos no viable para experimentos.
  if(nombreDataSet=="movement_libras"){
    #Obtain dataset train and test from the repository
    #flare1 <- read.table("data/flare.data1", quote="\"", comment.char="", stringsAsFactors=FALSE)
    #flare2 <- read.table("data/flare.data2", quote="\"", comment.char="", stringsAsFactors=FALSE)
    
    #Unifico los datos en un dataset
    #flare<-rbind(flare1,flare2)
    #flare<-flare2
    
    library(readr)
    dataSetUse <- read_csv("data/movement_libras.dat") #Version del http://sci2s.ugr.es/keel/dataset.php?cod=52
    
    # # Asigno los nombres de las columnas
     colnames(dataSetUse)[colnames(dataSetUse)=="Class"] <- "claseAPredecir"
     dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
    # 
    # 
    # #For every unique value in the string column, create a new 1/0 column
    # #This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
    # for(level in unique(dataSetUse$Sex)){
    #   abalone[paste("S", level, sep = "")] <- ifelse(abalone$Sex == level, 1, 0)
    # }
    # 
    # dataSetUse<-dataSetUse[,-1]
    head(dataSetUse)
    summary(dataSetUse)
    dataSetUse <- dataSetUse[complete.cases(dataSetUse),]
    
    dataSetUse$claseAPredecir<-as.character(dataSetUse$claseAPredecir)
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='1']<-'a'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='2']<-'b'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='3']<-'c'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='4']<-'d'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='5']<-'e'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='6']<-'f'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='7']<-'g'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='8']<-'h'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='9']<-'i'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='10']<-'j'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='11']<-'k'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='12']<-'l'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='13']<-'m'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='14']<-'n'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='15']<-'o'
    dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
 
    return(dataSetUse)
  }
  
  #Base de datos no viable para experimentos.
  if(nombreDataSet=="texture"){
    #Obtain dataset train and test from the repository
    #flare1 <- read.table("data/flare.data1", quote="\"", comment.char="", stringsAsFactors=FALSE)
    #flare2 <- read.table("data/flare.data2", quote="\"", comment.char="", stringsAsFactors=FALSE)
    
    #Unifico los datos en un dataset
    #flare<-rbind(flare1,flare2)
    #flare<-flare2
    
    library(readr)
    dataSetUse <- read_csv("data/texture.dat") #Version del http://sci2s.ugr.es/keel/dataset.php?cod=52
    
    # # Asigno los nombres de las columnas
    colnames(dataSetUse)[colnames(dataSetUse)=="Class"] <- "claseAPredecir"
    dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
    # 
    # 
    # #For every unique value in the string column, create a new 1/0 column
    # #This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
    # for(level in unique(dataSetUse$Sex)){
    #   abalone[paste("S", level, sep = "")] <- ifelse(abalone$Sex == level, 1, 0)
    # }
    # 
    # dataSetUse<-dataSetUse[,-1]
    head(dataSetUse)
    summary(dataSetUse)
    dataSetUse <- dataSetUse[complete.cases(dataSetUse),]
    
    
    dataSetUse$claseAPredecir<-as.character(dataSetUse$claseAPredecir)
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='2']<-'a'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='3']<-'b'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='4']<-'c'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='6']<-'d'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='7']<-'e'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='8']<-'f'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='9']<-'g'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='10']<-'h'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='12']<-'i'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='13']<-'j'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='14']<-'k'
    dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
    
    
    
    return(dataSetUse)
  }
  
  
  #Base de datos no viable para experimentos.
  if(nombreDataSet=="vowel"){
    #Obtain dataset train and test from the repository
    #flare1 <- read.table("data/flare.data1", quote="\"", comment.char="", stringsAsFactors=FALSE)
    #flare2 <- read.table("data/flare.data2", quote="\"", comment.char="", stringsAsFactors=FALSE)
    
    #Unifico los datos en un dataset
    #flare<-rbind(flare1,flare2)
    #flare<-flare2
    
    library(readr)
    dataSetUse <- read_csv("data/vowel.dat") #Version del http://sci2s.ugr.es/keel/dataset.php?cod=52
    
    # # Asigno los nombres de las columnas
    colnames(dataSetUse)[colnames(dataSetUse)=="Class"] <- "claseAPredecir"
    dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
    # 
    # 
    # #For every unique value in the string column, create a new 1/0 column
    # #This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
    # for(level in unique(dataSetUse$Sex)){
    #   abalone[paste("S", level, sep = "")] <- ifelse(abalone$Sex == level, 1, 0)
    # }
    # 
    # dataSetUse<-dataSetUse[,-1]
    head(dataSetUse)
    summary(dataSetUse)
    dataSetUse <- dataSetUse[complete.cases(dataSetUse),]
    
    
    dataSetUse$claseAPredecir<-as.character(dataSetUse$claseAPredecir)
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='0']<-'a'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='1']<-'b'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='2']<-'c'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='3']<-'d'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='4']<-'e'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='5']<-'f'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='6']<-'g'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='7']<-'h'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='8']<-'i'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='9']<-'j'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='10']<-'k'
    
    dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
    
    
    
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
    
    dataSetUse$claseAPredecir<-as.character(dataSetUse$claseAPredecir)
    
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='d ']<-'d'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='h ']<-'h'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='o ']<-'o'
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='s ']<-'s'
    
    dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
    
    
    
    return(dataSetUse)
  }
  
  
  
  
  #Fuente de Datos Dermatology
  if(nombreDataSet=="dermatology"){
    dermatology <- read.csv("data/dermatology.txt", header=FALSE, stringsAsFactors=FALSE)
    
    colnames(dermatology)[colnames(dermatology)=="V35"] <- "claseAPredecir"
    
    #Reemplazando por el nombre de las clases
    x<-dermatology$claseAPredecir
    x[x==1]<-"psorias"
    x[x==2]<-"seborder"
    x[x==3]<-"lichenpl"
    x[x==4]<-"pityrros"
    x[x==5]<-"cronider"
    x[x==6]<-"pitrubrp"
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
    
    #column_2C <- read.table("data/column_2C.dat", quote="\"", comment.char="")
    column_3C <- read.table("data/column_3C.dat", quote="\"", comment.char="")
    #vertebral<-rbind(column_2C,column_3C)
    
    vertebral<-column_3C
    
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
  
  
  # Ejemplo con el Dataset Glass
  if(nombreDataSet=="glass"){
    glass <- read.csv("data/glass.data", header=FALSE, stringsAsFactors=FALSE)
    
    # Asigno los nombres de las columnas
    colnames(glass)[colnames(glass)=="V11"] <- "claseAPredecir"
    colnames(glass)[colnames(glass)=="V2"] <- "RI"
    colnames(glass)[colnames(glass)=="V3"] <- "Na"
    colnames(glass)[colnames(glass)=="V4"] <- "Mg"
    colnames(glass)[colnames(glass)=="V5"] <- "Al"
    colnames(glass)[colnames(glass)=="V6"] <- "Si"
    colnames(glass)[colnames(glass)=="V7"] <- "K"
    colnames(glass)[colnames(glass)=="V8"] <- "Ca"
    colnames(glass)[colnames(glass)=="V9"] <- "Ba"
    colnames(glass)[colnames(glass)=="V10"] <- "Fe"
    
    #Reemplazando por el nombre de las clases
    x<-glass$claseAPredecir
    x[x==1]<-"buildwinfloatproc"
    x[x==2]<-"buildwinnonfloat"
    x[x==3]<-"vehwindowsfloat"
    x[x==4]<-"vehwindowsnofloat"
    x[x==5]<-"containers"
    x[x==6]<-"tableware"
    x[x==7]<-"headlamps"
    
    glass$claseAPredecir<-x
    glass$claseAPredecir<-factor(glass$claseAPredecir)
    
    # Muestro la composicion de los datos
    #summary(glass)
    #str(glass)
    
    #Quito columna de ids
    glass <- glass[,(!names(glass) %in% c("V1"))]
    
    
    #Operaciones de Descripcion de datos
    glass <- glass[complete.cases(glass),]
    
    #Muestro la relacion entre variables a traves de un dendograma:
    #     cc <- cor(glass[1:9],use="pairwise",method="pearson")
    #     thc <- hclust(dist(cc), method="average")
    #     dn <- as.dendrogram(thc)
    #     plot(dn, horiz = TRUE)
    
    dataSetUse<-glass
    return(dataSetUse)
    
  }
  
  
  
  #SatImage
  if(nombreDataSet=="satimage"){
    source('archivoScala_to_dataframe.R')
    
    
    
    #Obtengo el archivo de formato Scala a R
    satimageTr <- read.delim("data/satimage.scale", header=FALSE, stringsAsFactors=FALSE)
    #satimageTest <- read.delim("datos/satimage.scale.t", header=FALSE, stringsAsFactors=FALSE)
    
    #Unifico los datos en un dataset
    #satimage<-rbind(satimageTr,satimageTest)
    satimage<-satimageTr
    
    clases<-substr(satimage$V1,1,2)
    satimage['claseAPredecir']<-clases
    
    
    dataSetUse<-satimage
    
    #Separo la muestra por categoria con la que quiero trabajar
    #     dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
    #     categorias<-levels(dataSetUse$claseAPredecir)
    #     nuevodf<-dataSetUse[dataSetUse$claseAPredecir=="cualquier",]
    #     limitedeRegistros<-600
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
    
    
    
    #Defino la estructura de la base de datos
    #Defino la estructura de la base de datos
    dataFrameTemp <- data.frame(claseAPredecir=character())
    #Agrego dinamicamente las columnas
    nroColumnas<-36
    for(i in 1:nroColumnas){
      dataFrameTemp[,paste("V",i,sep = "")]<-numeric()  
    }
    
    
    #     dataFrameTemp <- data.frame(claseAPredecir=character(),V1=numeric(),V2=numeric(),V3=numeric(),V4=numeric(),V5=numeric(),V6=numeric(),V7=numeric(),V8=numeric(),V9=numeric(),V10=numeric()
    #                           ,V11=numeric(),V12=numeric(),V13=numeric(),V14=numeric(),V15=numeric(),V16=numeric(),V17=numeric(),V18=numeric(),V19=numeric(),V20=numeric()
    #                           ,V21=numeric(),V22=numeric(),V23=numeric(),V24=numeric(),V25=numeric(),V26=numeric(),V27=numeric(),V28=numeric(),V29=numeric(),V30=numeric()
    #                           ,V31=numeric(),V32=numeric(),V33=numeric(),V34=numeric(),V35=numeric(),V36=numeric())
    #     
    #dataFrameTemp <- data.frame()
    
    satimage <- cargarDatosScala(dataSetUse,dataFrameTemp)
    
    #La guardo como una variable global
    satimage <- satimage[complete.cases(satimage),]
    
    # 
     dataSetUse<-satimage
    # #Nuevamente filtro 300 por categoria por que no sabia cual era la categoria faltante
    # dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
    # categorias<-levels(dataSetUse$claseAPredecir)
    # nuevodf<-dataSetUse[dataSetUse$claseAPredecir=="cualquier",]
    # limitedeRegistros<-300
    # for(i in 1:length(categorias)){
    #   if(nrow(dataSetUse[dataSetUse$claseAPredecir==categorias[i],])>limitedeRegistros){
    #     dfxcat<-dataSetUse[dataSetUse$claseAPredecir==categorias[i],]
    #     set.seed(1)
    #     nuevodf<-rbind(nuevodf,dfxcat[sample(nrow(dfxcat),size=limitedeRegistros, replace=FALSE),])
    #   }else{
    #     nuevodf<-rbind(nuevodf,dataSetUse[dataSetUse$claseAPredecir==categorias[i],])
    #     
    #   }
    # }
    # dataSetUse<-nuevodf
    
    #Pongo un nombre a las clases
    dataSetUse$claseAPredecir<-as.character(dataSetUse$claseAPredecir)
    
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='1']<-'red_soil'                     #grupo1
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='2']<-'cotton_crop'                  #grupo2
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='3']<-'grey_soil'                    #grupo3
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='4']<-'damp_greysoil'               #grupo4
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='5']<-'soil_veg' #grupo5
    dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='6']<-'very_greysoil'          #grupo6
    
    dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
    
    return(dataSetUse)
    
  }
  
  
  
  if(nombreDataSet=="actRecogSingAccel"){
    
    # `1` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/1.csv", header=FALSE)
    # `2` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/2.csv", header=FALSE)
    # `3` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/3.csv", header=FALSE)
    # `4` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/4.csv", header=FALSE)
    # `5` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/5.csv", header=FALSE)
    # `6` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/6.csv", header=FALSE)
    # `7` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/7.csv", header=FALSE)
    # `8` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/8.csv", header=FALSE)
    # `9` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/9.csv", header=FALSE)
    # `10` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/10.csv", header=FALSE)
    # `11` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/11.csv", header=FALSE)
    # `12` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/12.csv", header=FALSE)
    # `13` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/13.csv", header=FALSE)
    # `14` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/14.csv", header=FALSE)
    # `15` <- read.csv("C:/clasificadoresJerarquicosAddClass/data/ActivityRecognitionSingleChestAccelerometer/15.csv", header=FALSE)
    # 
    # actRecogSingAccel<-rbind(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`)
    # colnames(actRecogSingAccel)[colnames(actRecogSingAccel)=="V5"] <- "claseAPredecir"
    # 
    # #Reemplazando por el nombre de las clases
    # x<-actRecogSingAccel$claseAPredecir
    # x[x==0]<-NA
    # x[x==1]<-"WorkingComputer"
    # x[x==2]<-"StandingWalkGoUpStair"
    # x[x==3]<-"Standing"
    # x[x==4]<-"Walking"
    # x[x==5]<-"GoingUpDownStairs "
    # x[x==6]<-"WalkingandTalking"
    # x[x==7]<-"TalkingwhileStand"
    # actRecogSingAccel$claseAPredecir<-x
    # 
    # 
    # actRecogSingAccel <- actRecogSingAccel[,(!names(actRecogSingAccel) %in% c("V1"))]
    # 
    # actRecogSingAccel <- actRecogSingAccel[complete.cases(actRecogSingAccel),]
    # 
    # actRecogSingAccel$claseAPredecir<-factor(actRecogSingAccel$claseAPredecir)
    # 
    # table(actRecogSingAccel$claseAPredecir)
    # 
    # 
    # summary(actRecogSingAccel)
    # 
    # write.csv(actRecogSingAccel, "actRecogSingAccel.csv", sep = ",", col.names = F)
    # 
    
    
    
    # Obtencion de los Datos
    actRecogSingAccel <- read.csv("data/actRecogSingAccel.csv", header=TRUE,stringsAsFactors=TRUE)
    
    #Quito las columnas sin informacion
    actRecogSingAccel <- actRecogSingAccel[,(!names(actRecogSingAccel) %in% c("X"))]
    
    
    dataSetUse<-actRecogSingAccel
    return(dataSetUse)
  }
  
  
  
  #A database for handwritten text recognition research
  
  if(nombreDataSet=="usps"){
    source('archivoScala_to_dataframe.R')
    
    #Obtengo el archivo de formato Scala a R
    #uspsTr <- read.delim("datos/usps", header=FALSE, stringsAsFactors=FALSE)
    #uspsTest <- read.delim("datos/usps.t", header=FALSE, stringsAsFactors=FALSE)
    
    #dataSetUse<- uspsTr
    
    #Unifico los datos en un dataset
    #datosOriginal<-rbind(uspsTr,uspsTest)
    library(readr)
    dataSetUse <- read_csv("data/usps_pro.csv")
    dataSetUse <- dataSetUse[,(!names(dataSetUse) %in% c("X1"))]
    
    #Se toma las primeras 2 posiciones como la clase
    #Tomar en cuenta si son mas de 10 clases del problema de clasificacion
    # clases<-substr(dataSetUse$V1,1,2)
    # dataSetUse['claseAPredecir']<-clases
    # 
    # dataSetUse<-datosOriginal
    
    #Separo la muestra por categoria con la que quiero trabajar
    # dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
    # categorias<-levels(dataSetUse$claseAPredecir)
    # nuevodf<-dataSetUse[dataSetUse$claseAPredecir=="cualquier",]
    # limitedeRegistros<-300
    # for(i in 1:length(categorias)){
    #   if(nrow(dataSetUse[dataSetUse$claseAPredecir==categorias[i],])>limitedeRegistros){
    #     dfxcat<-dataSetUse[dataSetUse$claseAPredecir==categorias[i],]
    #     set.seed(1)
    #     nuevodf<-rbind(nuevodf,dfxcat[sample(nrow(dfxcat),size=limitedeRegistros, replace=FALSE),])
    #   }else{
    #     nuevodf<-rbind(nuevodf,dataSetUse[dataSetUse$claseAPredecir==categorias[i],])
    # 
    #   }
    # }
    
    #nuevodf <- dataSetUse
    
    # mnist<-as.data.frame(nuevodf,stringsAsFactors=FALSE)
    # 
    # #Defino la estructura de la base de datos
    # dataFrameTemp <- data.frame(claseAPredecir=character())
    # #Agrego dinamicamente las columnas
    # nroColumnas<-256
    # for(i in 1:nroColumnas){
    #   dataFrameTemp[,paste("V",i,sep = "")]<-numeric()
    # }
    # 
    # 
    # ptm <- proc.time()
    # mnistTemp <- cargarDatosScala(mnist,dataFrameTemp)
    # (proc.time() - ptm)
    # 
    # set.seed(1)
    # mnistTemp <- mnistTemp[sample(1:nrow(mnistTemp)), ]
    # 
    # mnist <- mnistTemp[complete.cases(mnistTemp),]
    # dataSetUse<-mnist
    # 
    # 
    # 
    # 
    # #Pongo un nombre a las clases
    # dataSetUse$claseAPredecir<-as.character(dataSetUse$claseAPredecir)
    # dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='1']<-'A'
    # dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='2']<-'B'
    # dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='3']<-'C'
    # dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='4']<-'D'
    # dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='5']<-'E'
    # dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='6']<-'F'
    # dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='7']<-'G'
    # dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='8']<-'H'
    # dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='9']<-'I'
    # dataSetUse$claseAPredecir[dataSetUse$claseAPredecir=='10']<-'J'
    
    
    
    dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
    
    #dataSetUse$claseAPredecir<-factor(dataSetUse$claseAPredecir)
    dataSetUse <- dataSetUse[complete.cases(dataSetUse),]
    
    
    return(dataSetUse)
    
  }
  
  
  if(nombreDataSet=="Frogs"){
    
    Frogs_MFCCs <- read.csv("data/Frogs_MFCCs.csv")
    
    Frogs_MFCCs <- Frogs_MFCCs[,(!names(Frogs_MFCCs) %in% c("Family"))]
    Frogs_MFCCs <- Frogs_MFCCs[,(!names(Frogs_MFCCs) %in% c("Species"))]
    Frogs_MFCCs <- Frogs_MFCCs[,(!names(Frogs_MFCCs) %in% c("RecordID"))]
    
    
    colnames(Frogs_MFCCs)[colnames(Frogs_MFCCs)=="Genus"] <- "claseAPredecir"
    
    #summary(vertebral)
    dataSetUse<-Frogs_MFCCs
    dataSetUse <- dataSetUse[complete.cases(dataSetUse),]
    return(dataSetUse)
    
  }
  
  if(nombreDataSet=="optdigits"){
    
    # Obtencion de los Datos
    library(readr)
    
    optdigits <- read_csv("data/optdigits.tra", col_names = FALSE, col_types = cols(X65 = col_character()))
    
    
    
    # Asigno los nombres de las columnas
    colnames(optdigits)[colnames(optdigits)=="X65"] <- "claseAPredecir"
    
    
    optdigits$claseAPredecir[optdigits$claseAPredecir=='0']<-'A'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='1']<-'B'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='2']<-'C'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='3']<-'D'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='4']<-'E'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='5']<-'F'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='6']<-'G'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='7']<-'H'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='8']<-'I'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='9']<-'J'
    
    optdigits$claseAPredecir<-factor(optdigits$claseAPredecir)
    
    #Operaciones de Descripcion de datos
    optdigits <- optdigits[complete.cases(optdigits),]
    
    dataSetUse<-optdigits
    return(dataSetUse)
  }
  
  
  
  if(nombreDataSet=="wifiloc"){
    
    wifi_loc <- read.delim("data/wifi_localization.txt", header=FALSE)
    
    colnames(wifi_loc)[colnames(wifi_loc)=="V8"] <- "claseAPredecir"
    wifi_loc$claseAPredecir<-factor(wifi_loc$claseAPredecir)
    
    
    #summary(vertebral)
    dataSetUse<-wifi_loc
    dataSetUse <- dataSetUse[complete.cases(dataSetUse),]
    return(dataSetUse)
    
  }
  
  
  if(nombreDataSet=="loans"){
    
    library(readr)
    loansdataset <- read_table2("data/out.prosper-loans",col_names = FALSE)
    
    
    colnames(loansdataset)[colnames(loansdataset)=="X9"] <- "claseAPredecir"
    loansdataset$claseAPredecir<-factor(loansdataset$claseAPredecir)
    loansdataset$X6<-factor(loansdataset$X6)
    
    #summary(vertebral)
    dataSetUse<-loansdataset
    dataSetUse <- dataSetUse[complete.cases(dataSetUse),]
    return(dataSetUse)
    
  }
  
  if(nombreDataSet=="anneal"){
    
    # Obtencion de los Datos
    anneal <- read.csv("data/anneal.data", header=FALSE)
    annealT <- read.csv("data/anneal.test", header=FALSE)
    
    anneal<-rbind(anneal,annealT)
    
    
    # Asigno los nombres de las columnas
    colnames(anneal)[colnames(anneal)=="V39"] <- "claseAPredecir"
    
    
    #Quito columnas que tienen el mismo valor
    
    # Quito columna del ID
    anneal <- anneal[,(!names(anneal) %in% c("V2"))]
    anneal <- anneal[,(!names(anneal) %in% c("V19"))]
    anneal <- anneal[,(!names(anneal) %in% c("V23"))]
    anneal <- anneal[,(!names(anneal) %in% c("V26"))]
    anneal <- anneal[,(!names(anneal) %in% c("V29"))]
    anneal <- anneal[,(!names(anneal) %in% c("V30"))]
    anneal <- anneal[,(!names(anneal) %in% c("V31"))]
    
    #Operaciones de Descripcion de datos
    anneal <- anneal[complete.cases(anneal),]
    
    #Simplifico la prueba+
    anneal$claseAPredecir<-factor(anneal$claseAPredecir)
    
    dataSetUse<-anneal
    return(dataSetUse)
  }
  
  if(nombreDataSet=="bands"){
    
    # Obtencion de los Datos
    bands <- read.csv("data/bands.data", header=FALSE)
    
    
    # Asigno los nombres de las columnas
    colnames(anneal)[colnames(anneal)=="V39"] <- "claseAPredecir"
    
    
    #Quito columnas que tienen el mismo valor
    
    # Quito columna del ID
    anneal <- anneal[,(!names(anneal) %in% c("V2"))]
    anneal <- anneal[,(!names(anneal) %in% c("V19"))]
    anneal <- anneal[,(!names(anneal) %in% c("V23"))]
    anneal <- anneal[,(!names(anneal) %in% c("V26"))]
    anneal <- anneal[,(!names(anneal) %in% c("V29"))]
    anneal <- anneal[,(!names(anneal) %in% c("V30"))]
    anneal <- anneal[,(!names(anneal) %in% c("V31"))]
    
    #Operaciones de Descripcion de datos
    anneal <- anneal[complete.cases(anneal),]
    
    #Simplifico la prueba+
    anneal$claseAPredecir<-factor(anneal$claseAPredecir)
    
    dataSetUse<-anneal
    return(dataSetUse)
  }
  
  
  if(nombreDataSet=="vehicle"){
    
    # Obtencion de los Datos
    veh <- read.csv("data/vehiculo/xaa.dat", header=FALSE)
    
    library(readr)
    
    nombresArchivos<-c('xab','xac','xad','xae','xaf','xag','xah','xai')
    
    dStemp <- read_table2("data/vehiculo/xaa.dat",col_names = FALSE, col_types = cols(X20 = col_skip()))
    for(ix in nombresArchivos){
      dStemp <- rbind(dStemp,read_table2(paste("data/vehiculo/",ix,".dat",sep = ""),col_names = FALSE, col_types = cols(X20 = col_skip())))  
    }
    
    # Asigno los nombres de las columnas
    colnames(dStemp)[colnames(dStemp)=="X19"] <- "claseAPredecir"
    
    dStemp$claseAPredecir<-factor(dStemp$claseAPredecir)
    
    #Operaciones de Descripcion de datos
    dStemp <- dStemp[complete.cases(dStemp),]
    
    dataSetUse<-dStemp
    return(dataSetUse)
  }
  
  
  
  if(nombreDataSet=="page-blocks"){
    
    # Obtencion de los Datos
    library(readr)
    
    page_blocks <- read_table2("data/page-blocks.data",col_names = FALSE, col_types = cols(X11 = col_character()))
    
    
    # Asigno los nombres de las columnas
    colnames(page_blocks)[colnames(page_blocks)=="X11"] <- "claseAPredecir"
    
    
    page_blocks$claseAPredecir[page_blocks$claseAPredecir=='1']<-'A'
    page_blocks$claseAPredecir[page_blocks$claseAPredecir=='2']<-'B'
    page_blocks$claseAPredecir[page_blocks$claseAPredecir=='3']<-'C'
    page_blocks$claseAPredecir[page_blocks$claseAPredecir=='4']<-'D'
    page_blocks$claseAPredecir[page_blocks$claseAPredecir=='5']<-'E'
    
    page_blocks$claseAPredecir<-factor(page_blocks$claseAPredecir)
    
    #Operaciones de Descripcion de datos
    page_blocks <- page_blocks[complete.cases(page_blocks),]
    
    
    
    
    dataSetUse<-page_blocks
    return(dataSetUse)
  }
  
  
  
  
  if(nombreDataSet=="aloi"){
    
    # Obtencion de los Datos
    library(readr)
    
    optdigits <- read_csv("data/aloi.data", col_names = FALSE, col_types = cols(X65 = col_character()))
    
    
    
    # Asigno los nombres de las columnas
    colnames(optdigits)[colnames(optdigits)=="X65"] <- "claseAPredecir"
    
    
    optdigits$claseAPredecir[optdigits$claseAPredecir=='0']<-'A'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='1']<-'B'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='2']<-'C'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='3']<-'D'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='4']<-'E'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='5']<-'F'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='6']<-'G'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='7']<-'H'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='8']<-'I'
    optdigits$claseAPredecir[optdigits$claseAPredecir=='9']<-'J'
    
    optdigits$claseAPredecir<-factor(optdigits$claseAPredecir)
    
    #Operaciones de Descripcion de datos
    optdigits <- optdigits[complete.cases(optdigits),]
    
    dataSetUse<-optdigits
    return(dataSetUse)
  }
  
}

