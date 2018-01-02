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
  
  
}

