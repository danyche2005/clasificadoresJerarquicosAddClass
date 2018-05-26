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
    return(attributes(v)$names[index])#class name
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

