#limites_matriz.R
#Descripción: con base en el periodo de seleccion, esta matriz nos entrega los pasos (segundo) en 
#los que se selecciona una nueva palanca y el tiempo en los que debe ser utilizada
#IN:
  #messages: matriz de mensajes a transmitir
  #iteraciones: numero total de iteraciones a considerar en la simulacion (una iteracion tiene un total de pasos igual al periodo de seleccion)
  #periodo: periodo de seleccion
#OUT: Regresa la distancia en metros, entre los puntos de origen-destino

limites_matriz <- function(messages,iteraciones,periodo){
  msn <- filter(messages, tiempo_envio == min(messages$tiempo_envio))
  seg <- msn$tiempo_envio[1]
  #iteraciones <- 150
  
  limites <- data.frame(matrix(0,iteraciones,2))
  colnames(limites) <- c("minimo","maximo")
  
  for(i in 1:iteraciones){
    limites$minimo[i] <- (seg + 1) + (periodo * (i -1))
    limites$maximo[i] <- (seg + periodo) + (periodo * (i -1))
  }
  return(limites)
}