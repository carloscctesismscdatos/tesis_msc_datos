#distancia.R
#Descripción: calcula la distancia euclidiana entre cualesquiera dos puntos en coordenadas UTM
#IN: Las coordenadas de los puntos origen-destino
#OUT: Regresa la distancia en metros, entre los puntos de origen-destino
distancia<-function(x_origen,y_origen,x_destino,y_destino){
  return(sqrt(((y_destino-y_origen)^2)+((x_destino-x_origen)^2)))
}

