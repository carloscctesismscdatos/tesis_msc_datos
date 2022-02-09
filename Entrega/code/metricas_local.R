#metricas_local.R
#Descripción: calcula el numero de mensajes transmitidos por cada dispositivo
#IN
  #P_final: una matriz con el registros de todos los intercambios de mensajes en la simulacion
  #P_final: [ID portador del mensaje, num de mensaje, tiempo en que se entrego, fuente (remitente original), destino(destinatario),iteracion]
#OUT: una matriz con el numero de mensajes transmitidos por usuario

metricas_local <- function(P_final,H){
  #Matriz para acumular los datos
  met <- data.frame(matrix(0,max(H$indice),2))
  #mensajes_fuente es el numero de nuevos mensajes que fueron transmitidos a un usuario
  colnames(met) <- c("total_mensajes_entregados_destinatario","mensajes_fuente")
  #agregamos un consecutivo
  met$mensajes_fuente[as.numeric(row.names(table(P_final$fuente)))] <-  table(P_final$fuente)
  return(met)
}