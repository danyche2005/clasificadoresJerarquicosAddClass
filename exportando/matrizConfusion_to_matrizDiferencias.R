mconfusion2oMDiferencias <- function(tablaConfusion,metodo="chi"){
tablaConfusion<-t(tablaConfusion)
  
        #Metodo Propuesto
        # 1
        if(metodo=="propio"){        
                #Genero la matriz Totales:
                nrocolumnas<-ncol(tablaConfusion)
                nrofilas<-nrow(tablaConfusion)
                matrizTotales<-{}
               
                for(j in 1:nrofilas){
                        matrizTotales[j]<-sum(tablaConfusion[j,])
                }
                ##print(matrizTotales)
                
                #Genero Matriz de Normalizada
                nrocolumnas<-ncol(tablaConfusion)
                nrofilas<-nrow(tablaConfusion)
                matrizNormalizada<-tablaConfusion
                for(i in 1:nrocolumnas){
                        for(j in 1:nrofilas){
                                
                                #Evito la division para cero
                                if(matrizTotales[j]==0){
                                        matrizNormalizada[j,i]<-0
                                }else{
                                        matrizNormalizada[j,i]<-tablaConfusion[j,i]/matrizTotales[j]        
                                }
                                
                                
                                
                        }
                }
                ##print(matrizNormalizada)
                
                #Genero Matriz de Similitud
                nrocolumnas<-ncol(matrizNormalizada)
                nrofilas<-nrow(matrizNormalizada)
                matrizSimilitud<-matrizNormalizada
                matrizSimilitud[,]<-0
                for(i in 1:nrocolumnas){
                        for(j in 1:nrofilas){
                                if(j>i){
                                        confusiones<-matrizNormalizada[j,i]+matrizNormalizada[i,j]
                                        aciertos<-matrizNormalizada[j,j]+matrizNormalizada[i,i]
                                        matrizSimilitud[j,i]<-confusiones/2
                                        
                                }
                                
                        }
                }    
                ##print(matrizSimilitud)
                
                matrizDistancias<-1-matrizSimilitud
                ##print(matrizDistancias)
        }
        
        
        
        #Metodo Propuesto Modificado
        # 1.1
        if(metodo=="propioModificado"){        
          #Genero la matriz Totales:
          nrocolumnas<-ncol(tablaConfusion)
          nrofilas<-nrow(tablaConfusion)
          
          
          #Obtengo el total de toda la matriz
          valorTotal<- sum(tablaConfusion[,])
          
          #Genero Matriz de Normalizada
          matrizNormalizada<-tablaConfusion/valorTotal
          
          
          print(matrizNormalizada)
          
          #Genero Matriz de Similitud
          nrocolumnas<-ncol(matrizNormalizada)
          nrofilas<-nrow(matrizNormalizada)
          matrizSimilitud<-matrizNormalizada
          matrizSimilitud[,]<-0
          for(i in 1:nrocolumnas){
            for(j in 1:nrofilas){
              if(j>i){
                confusiones<-matrizNormalizada[j,i]+matrizNormalizada[i,j]
                aciertos<-matrizNormalizada[j,j]+matrizNormalizada[i,i]
                matrizSimilitud[j,i]<-confusiones/2
              }
            }
          }
          
          print(matrizSimilitud)
          matrizDistancias<-1-matrizSimilitud
          print(matrizDistancias)
        }




        #Metodo Distancia Euclidea
        # 2
        if(metodo=="euclidea"){        
                #Genero la matriz Totales:
                nrocolumnas<-ncol(tablaConfusion)
                nrofilas<-nrow(tablaConfusion)
                matrizTotales<-{}
                
                for(j in 1:nrofilas){
                        matrizTotales[j]<-sum(tablaConfusion[j,])
                }
                print(matrizTotales)
                
                #Genero Matriz de Normalizada
                nrocolumnas<-ncol(tablaConfusion)
                nrofilas<-nrow(tablaConfusion)
                matrizNormalizada<-tablaConfusion
                for(i in 1:nrocolumnas){
                        for(j in 1:nrofilas){
                                
                                #Evito la division para cero
                                if(matrizTotales[j]==0){
                                        matrizNormalizada[j,i]<-0
                                }else{
                                        matrizNormalizada[j,i]<-tablaConfusion[j,i]/matrizTotales[j]        
                                }
                                

                                
                                
                        }
                }
                print(matrizNormalizada)
                
                #Genero Matriz de Distancia Euclidea
                nrocolumnas<-ncol(matrizNormalizada)
                nrofilas<-nrow(matrizNormalizada)
                distancias<-{}
               
                
                #Distancia Euclidea:
                for(j in 1:(nrofilas-1)){
                        ini<-j+1
                        for(k in ini:nrofilas){
                                vectResta<-matrizNormalizada[j,]-matrizNormalizada[k,]
                                vectCuadrado<-(vectResta)^2
                                vectTot<-sum(vectCuadrado)
                                distancia<-(vectTot)^(1/2)
                                distancias<-c(distancias,distancia)
                        }
                }
                
                
                #Creo la matriz de distancias
                matrizDistancias<-matrizNormalizada
                matrizDistancias[,]<-1
                
                k<-1
                for(i in 1:(nrocolumnas-1)){
                        ini<-i+1
                        for(j in ini:nrofilas){
                                matrizDistancias[j,i]<-distancias[k]
                                k<-k+1
                                
                        }
                }
                         
               
                print(matrizDistancias)
        }
        
        
        #Metodo Distancia Euclidea Modificada
        # 2
        if(metodo=="euclideaModificado"){        
          #Genero la matriz Totales:
          nrocolumnas<-ncol(tablaConfusion)
          nrofilas<-nrow(tablaConfusion)
          
          
          #Genero Matriz de Normalizada
          #Obtengo el total de toda la matriz
          valorTotal<- sum(tablaConfusion[,])
          
          #Genero Matriz de Normalizada
          matrizNormalizada<-tablaConfusion/valorTotal
          print(matrizNormalizada)
          
          #Genero Matriz de Distancia Euclidea
          nrocolumnas<-ncol(matrizNormalizada)
          nrofilas<-nrow(matrizNormalizada)
          distancias<-{}
          
          
          #Distancia Euclidea:
          for(j in 1:(nrofilas-1)){
            ini<-j+1
            for(k in ini:nrofilas){
              vectResta<-matrizNormalizada[j,]-matrizNormalizada[k,]
              vectCuadrado<-(vectResta)^2
              vectTot<-sum(vectCuadrado)
              distancia<-(vectTot)^(1/2)
              distancias<-c(distancias,distancia)
            }
          }
          
          
          #Creo la matriz de distancias
          matrizDistancias<-matrizNormalizada
          matrizDistancias[,]<-1
          
          k<-1
          for(i in 1:(nrocolumnas-1)){
            ini<-i+1
            for(j in ini:nrofilas){
              matrizDistancias[j,i]<-distancias[k]
              k<-k+1
              
            }
          }
          
          
          print(matrizDistancias)
        }
        
        
        #Metodo Distancia Bray-Curtis
        # 3
        if(metodo=="bray"){        
                
                #Genero la matriz Totales:
                nrocolumnas<-ncol(tablaConfusion)
                nrofilas<-nrow(tablaConfusion)
                matrizTotales<-{}
                
                for(j in 1:nrofilas){
                        matrizTotales[j]<-sum(tablaConfusion[j,])
                }
                print(matrizTotales)
                
                #No se realiza una normalizacion en este caso
                matrizNormalizada<-tablaConfusion
                
                #Genero Matriz de Distancias
                nrocolumnas<-ncol(matrizNormalizada)
                nrofilas<-nrow(matrizNormalizada)
                distancias<-{}
                
                
                #Distancia Bray Curtis:
                for(j in 1:(nrofilas-1)){
                        ini<-j+1
                        for(k in ini:nrofilas){
                                vectResta<-abs(matrizNormalizada[j,]-matrizNormalizada[k,])
                                total<-sum(vectResta)
                                distancia<-total/(matrizTotales[j]+matrizTotales[k])
                                distancias<-c(distancias,distancia)
                        }
                }
                
                
                #Creo la matriz de distancias
                matrizDistancias<-matrizNormalizada
                matrizDistancias[,]<-1
                
                k<-1
                for(i in 1:(nrocolumnas-1)){
                        ini<-i+1
                        for(j in ini:nrofilas){
                                matrizDistancias[j,i]<-distancias[k]
                                k<-k+1
                                
                        }
                }
                
                
                print(matrizDistancias)
        }
        
        
        
        #Metodo Distancia Chi-Square
        # 4
        if(metodo=="chi"){        
                
                #Genero la matriz Totales:
                nrocolumnas<-ncol(tablaConfusion)
                nrofilas<-nrow(tablaConfusion)
                
                matrizTotalesFilas<-{}
                for(j in 1:nrofilas){
                        matrizTotalesFilas[j]<-sum(tablaConfusion[j,])
                }
                print(matrizTotalesFilas)
                
                matrizTotalesCol<-{}
                for(i in 1:nrocolumnas){
                        matrizTotalesCol[i]<-sum(tablaConfusion[,i])
                }
                print(matrizTotalesCol)
                
                totalFilasCol<-sum(matrizTotalesFilas)
                
                
                #Genero Matriz de Normalizada
                nrocolumnas<-ncol(tablaConfusion)
                nrofilas<-nrow(tablaConfusion)
                matrizNormalizada<-tablaConfusion
                for(i in 1:nrocolumnas){
                        for(j in 1:nrofilas){
                                
                                
                                #Evito la division para cero
                                if(matrizTotalesFilas[j]==0){
                                        matrizNormalizada[j,i]<-0
                                }else{
                                        matrizNormalizada[j,i]<-tablaConfusion[j,i]/matrizTotalesFilas[j]
                                }
                                
                        }
                }
                print(matrizNormalizada)
                
                #Obtengo Promedio
                vectPromedios<-{}
                for(i in 1:nrocolumnas){
                        vectPromedios[i]<-matrizTotalesCol[i]/totalFilasCol
                }
                print(vectPromedios)
                
                
                #Genero Matriz de Distancia 
                nrocolumnas<-ncol(matrizNormalizada)
                nrofilas<-nrow(matrizNormalizada)
                
                distancias<-{}
                #Distancia Chi Square:
                for(j in 1:(nrofilas-1)){
                        ini<-j+1
                        for(k in ini:nrofilas){
                                vectResta<-matrizNormalizada[j,]-matrizNormalizada[k,]
                                vectCuadrado<-(vectResta)^2
                                vectTot<-vectCuadrado/vectPromedios
                                valSuma<-sum(vectTot)
                                distancia<-(valSuma)^(1/2)
                                distancias<-c(distancias,distancia)
                        }
                }
                
                
                #Creo la matriz de distancias
                matrizDistancias<-matrizNormalizada
                matrizDistancias[,]<-1
                
                k<-1
                for(i in 1:(nrocolumnas-1)){
                        ini<-i+1
                        for(j in ini:nrofilas){
                                matrizDistancias[j,i]<-distancias[k]
                                k<-k+1
                                
                        }
                }
                
                
                print(matrizDistancias)
        }



        #Metodo Distancia Chi-Square Modificado
        # 4
        if(metodo=="chiModificado"){        
          
          #Genero la matriz Totales:
          nrocolumnas<-ncol(tablaConfusion)
          nrofilas<-nrow(tablaConfusion)
          
          matrizTotalesCol<-{}
          for(i in 1:nrocolumnas){
            matrizTotalesCol[i]<-sum(tablaConfusion[,i])
          }
          print(matrizTotalesCol)
          

          matrizNormalizada<-tablaConfusion
          print(matrizNormalizada)
          
     
          #Genero Matriz de Distancia 
          nrocolumnas<-ncol(matrizNormalizada)
          nrofilas<-nrow(matrizNormalizada)
          
          distancias<-{}
          #Distancia Chi Square:
          for(j in 1:(nrofilas-1)){
            ini<-j+1
            for(k in ini:nrofilas){
              vectResta<-matrizNormalizada[j,]-matrizNormalizada[k,]
              vectCuadrado<-(vectResta)^2
              vectTot<-vectCuadrado/matrizTotalesCol
              valSuma<-sum(vectTot)
              distancia<-(valSuma)^(1/2)
              distancias<-c(distancias,distancia)
            }
          }
          
          
          #Creo la matriz de distancias
          matrizDistancias<-matrizNormalizada
          matrizDistancias[,]<-1
          
          k<-1
          for(i in 1:(nrocolumnas-1)){
            ini<-i+1
            for(j in ini:nrofilas){
              matrizDistancias[j,i]<-distancias[k]
              k<-k+1
              
            }
          }
          
          
          print(matrizDistancias)
        }


        return(matrizDistancias)
        
}


#Pruebas de los Metodos:
# tablaConfusion<-matrix(c(59, 2, 3, 7, 15, 1,65,1,12,9,17,9,52,2,10,12,8,3,81,9,3,6,5,2,73), nrow=5, ncol=5) 
# 
# mresp1<-mconfusion2oMDiferencias(tablaConfusion,metodo = "propio")
# d<-as.dist(round(mresp1,3))
# d
# mresp2<-mconfusion2oMDiferencias(tablaConfusion,metodo = "propioModificado")
# d<-as.dist(round(mresp2,3))
# d
# mresp3<-mconfusion2oMDiferencias(tablaConfusion,metodo = "euclidea")
# d<-as.dist(round(mresp3,3))
# d
# mresp4<-mconfusion2oMDiferencias(tablaConfusion,metodo = "euclideaModificado")
# d<-as.dist(round(mresp4,3))
# d
# mresp5<-mconfusion2oMDiferencias(tablaConfusion,metodo = "bray")
# d<-as.dist(round(mresp5,3))
# d
# mresp6<-mconfusion2oMDiferencias(tablaConfusion,metodo = "chi")
# d<-as.dist(round(mresp6,3))
# d
# mresp7<-mconfusion2oMDiferencias(tablaConfusion,metodo = "chiModificado")
# d<-as.dist(round(mresp7,3))
# d
# 
# 
