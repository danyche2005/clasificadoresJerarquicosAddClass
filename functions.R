#from Daniel's code.
#The clustering uses the complete linkage
#require(dendextend)
#require(data.tree)
#require(nnet)

#Obtain the predictions for a set
applyModel<-function (model,set,metodo,obtenerProb=FALSE){
  source('techniquesLibraries_WithProbs.R')
  #p<- predict(model,set,type=t )
  p<- prediceLibTecnicas(datosTest = set,modelo = model,metodo = metodo,obtenerProb=obtenerProb)
  
  return(p)
}

#Determine the predicted class with the greatest missclassification error for a given real class
missingClass<- function(cf){
  n=attributes(cf)$dim[1]
  empty=rep(0,n)
  i=1
  flag=!all(cf[i,]==empty)
  while (flag){
    i=i+1
    flag=!all(cf[i,]==empty)
  }
  return(attributes(cf)$dimnames$Prediction[i])
}

similarClass <- function(cf,class){
    v<-cf[,class]
    index<-which.is.max(v)
    
    
    if(v[index]==0){
      #No se encuentra un similar. Se agrega este nodo a la raiz.
      nombreSim<-"Root"
      
    }else{
      nombreSim<-attributes(v)$names[index]
    }
    
    return(nombreSim)#class name
}

classHierachy<- function(matrix, linkage="complete"){
  d<-as.dist(matrix)#convert the matrix in a distance matrix
  hc=hclust(d,method=linkage)#apply hierarchical clustering
  dendograma<-as.dendrogram(hc)
  jerarquiaClases<-as.Node(dendograma)
  #str(jerarquiaClases)
  #Pongo diferentes valores en los nombres de nodos
  nombresJerarquias<-jerarquiaClases$Get("name")
  nombresJerarquias
  nombresNuevos<-{}
  for(i in 1:length(nombresJerarquias)){
    if(!is.na(as.numeric(nombresJerarquias[[i]]))){
      nombresNuevos[i]<-paste('c',i,sep='')
      nombresNuevos[[i]]<-paste('c',i,sep='')
    }else{
      nombresNuevos[i]<-nombresJerarquias[i]  
    }
  }
  names(nombresNuevos)<-nombresNuevos
  #nombresNuevos
  jerarquiaClases$Set(name = nombresNuevos)
  
  jerarquiaClases<-comprimirArbol(jerarquiaClases)
  
  #plot(jerarquiaClases)
  
  procJerarquia=list()
  procJerarquia[[1]]<-jerarquiaClases
  procJerarquia[[2]]<-hc
  
  return(procJerarquia)
  
}


#Recorro el arbol y verifico cada nodo
comprimirArbol<-function(nodo){
  
  #Verifico y comprimo el nivel si es necesario.
  nodo<-verificarNivel(nodo)
  nroHijos<-length(nodo$children)
  
  if(nroHijos>0){
    for(i in 1:nroHijos){
      nodoHijo<-nodo$children[[i]]
      #Comprimo sus nodos descendientes y lo reemplazo en la estructura
      nodo$children[[i]]<-comprimirArbol(nodoHijo)
      
    }
  }
  return(nodo)
}





#Busco Nodos misma altura padre hijos
verificarNivel<-function(nivelNodo){
  condicion<-TRUE
  
  while(condicion){
    condicion<-FALSE
    nroHijos<-length(nivelNodo$children)
    #Recorro todos los nodos hijos
    for(i in 1:nroHijos){
      #Tienen la misma altura y tienen relacion padre-hijo
      #Antes verifico que el nodo tenga el atributo
      if(!is.null(nivelNodo$plotHeight) & !is.null(nivelNodo$children[[i]]$plotHeight)){
        
        alturaNodo<-nivelNodo$plotHeight
        alturaHijo<-nivelNodo$children[[i]]$plotHeight
        #Aqui da un error No tomar en cuenta por que 
        #se asume que existe la propiedad alturaHijo en el Nodo
        if(alturaNodo==alturaHijo){
          nivelNodo<-unirNodos(nivelNodo,nodoHijo=nivelNodo$children[[i]])
          condicion<-TRUE
          break
        }
      }
      
    }
    #Cuando ya no encuentre Similares en el nivel 
    #Se puede decir que el nivel esta verificado
  }
  return(nivelNodo)
}



unirNodos<-function(nodoPadre,nodoHijo){
  #Obtengo las relaciones del hijo
  descendientes<-nodoHijo$children
  #Quito el nodo hijo al arbol
  nodoPadre$RemoveChild(c(nodoHijo$name))
  #Agrego al nodo las relaciones
  for(i in 1:length(descendientes)){
    nodoPadre$AddChildNode(descendientes[[i]])
  }
  return(nodoPadre)
}


genRandomString <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}




addClassTreeToNode <- function(padreNode,arbolClasificadores) {
  
  nroHijos<-length(arbolClasificadores$children)
  
  if(nroHijos>0){
    for(i in 1:nroHijos){
      
      nodoHijo<-arbolClasificadores$children[[i]]    
      #Creo una copia del nodo
      #copiaHijo <- Node$new(nodoHijo$name)
      
      #Sin es un nodo interno se debe copiar el clasificador
      if(!nodoHijo$isLeaf){
        # copiaHijo$clasificador <- nodoHijo$clasificador
        # copiaHijo$entrada <- nodoHijo$entrada
        # copiaHijo$salidas <- nodoHijo$salidas
        #   
        padreNode$AddChild(nodoHijo$name, clasificador = nodoHijo$clasificador, entrada = nodoHijo$entrada, salidas=nodoHijo$salidas )
        #padreRoot$children[nodoHijo$name]<-addClassTreeToNode(FindNode(padreNode,nodoHijo$name),nodoHijo)
        nodoAgregado<-FindNode(padreNode,nodoHijo$name)
        addClassTreeToNode(nodoAgregado,nodoHijo)
        
      }else{
        padreNode$AddChild(nodoHijo$name)
      }
      

    }
  }

  
}


#Guarda Imagenes del Plot en archivos

guardarImagenJerarquia<-function(arbol,nombreArchivo,directorio){
  #Guarda una imagen de plot
  
  if (Sys.info()[1] == "Windows") {
    mypath <-
      file.path(
        directorio,
        nombreArchivo
      )
  }else{
    # Mac y Linux
    mypath <-
      file.path(
        directorio,
        nombreArchivo
      )
    
  }
  png(file = mypath)
  mytitle = paste(nombreArchivo,sep = "")
  textplot(print(arbol)[,1])
  dev.off()
  
  
}

# 
# for (i in (1:n)){
#   file<-paste(directorio,"/Jerarquias/","ConfMat_Val_",ik,u.s[i],".png",sep="")
#   png(file)
#   textplot(print(allClassHierar[[i]])[,1])
#   dev.off()
# }


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


