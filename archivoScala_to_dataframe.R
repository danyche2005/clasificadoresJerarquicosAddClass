
cargarDatosScala<-function(satimage,nuevodf,valorPerdidos=NA){
    
#   #Tomar un archivo en scala a un dataset simple
#   satimage <- read.delim("C:/Users/DanielAndres/Downloads/satimage.scale", header=FALSE, stringsAsFactors=FALSE)
#   
#   #Defino la estructura de una fila
#   nuevodf <- data.frame(claseAPredecir=character(),V1=numeric(),V2=numeric,V3=numeric(),V4=numeric,V5=numeric(),V6=numeric,V7=numeric(),V8=numeric,V9=numeric(),V10=numeric()
#                           ,V11=numeric(),V12=numeric,V13=numeric(),V14=numeric,V15=numeric(),V16=numeric,V17=numeric(),V18=numeric,V19=numeric(),V20=numeric()
#                           ,V21=numeric(),V22=numeric,V23=numeric(),V24=numeric,V25=numeric(),V26=numeric,V27=numeric(),V28=numeric,V29=numeric(),V30=numeric()
#                           ,V31=numeric(),V32=numeric,V33=numeric(),V34=numeric,V35=numeric(),V36=numeric)
#   
  
  #Recorro cada fila que se representa como un string
  for(i in 1:nrow(satimage)){
    #fila<-satimage[i,]
    fila<-satimage[i,]
    
    datosCaracteres<-fila[,1]
    
    elementos<-strsplit(datosCaracteres, " ")[[1]]
    
    
    #Obtengo las clase y los atributos
    clase<-elementos[1]
    otrosElementos<-elementos[-1]

    
    #Guardo la Clase
    atributo<-c()
    atributo['claseAPredecir']<-clase
      
    #Guardo los atributos
    for(i in 1:length(otrosElementos)){
      eleTemp<-strsplit(otrosElementos[i], ":")[[1]]
      #Cuando los nombres son Numeros: V+1
      atributo[paste('V',eleTemp[1],sep = "")]<-as.numeric(eleTemp[2])   
    }  
    
    #Pongo los valores vacios
    for(i in 1:(ncol(nuevodf)-1)){
      if(is.na(atributo[paste('V',i,sep = "")])){
        atributo[paste('V',i,sep = "")]<-valorPerdidos
      }
    }
    
    A <- matrix( atributo,nrow=1,ncol=ncol(nuevodf),byrow = TRUE)
    colnames(A) = names(atributo)
    
    #Guardo el registro en el dataframe
    nuevodf<-rbind(nuevodf,data.frame(A,stringsAsFactors = FALSE,check.names = TRUE))
    
    if((i %% 100)==0){
      print(i)  
    }
    
    
  }
  
  #Arreglo el formato de la tabla
  for(i in 2:ncol(nuevodf)){
    nuevodf[,i]<-as.numeric(nuevodf[,i])
  }
  nuevodf$claseAPredecir<-factor(nuevodf$claseAPredecir)

return(nuevodf)
}


 


