#cercanos.R
#Descripción: Determina todos los dispositivos que estan a menos r metros de distancia que el punto p=[x,y,z]
#IN: 
  #p el punto para el cual se requiere determinar sus puntos cercanos
  #r el radio de transmisión al cual se buscaran los vecinos
  #V matriz con las coordenadas de los puntos en los que se debe determinar si son o no vecinos
#OUT: La lista de dispositivos vecinos al punto p

cercanos <-function(p,V,r){
  #indice para guardar los puntos vecinos
  vecinos<-c()
  #punto respecto al cual se quieren determinar los vecinos
  p<-data.frame(p)
  colnames(p)<-c("x","y")
  #posibles vecinos al punto p
  V<-data.frame(V)
  colnames(V)<-c("X","Y")
  
  #Dentro del cuadro, pero no necesariamente a r metros pues pueden quedar en las esquinas
  vector<-intersect(which((V$X-p$x)<r),which((V$Y-p$y)<r))
  #Al medir la distancia a cada uno se determinar los que estan realmente a r metros
  for(i in 1:(length(vector))){
    #print(c(i, distancia(p$x,p$y,V$X[vector[i]],V$Y[vector[i]]) ))
    if(distancia(p$x,p$y,V$X[vector[i]],V$Y[vector[i]])<=r){
      vecinos<-rbind(vecinos,vector[i])
    }
  }
  return(vecinos)
}

