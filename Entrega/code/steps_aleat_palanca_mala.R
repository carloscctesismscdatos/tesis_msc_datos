#steps_aleat_palanca_mala.R
#Descripción: esta rutina cumple con varias funciones: 
  #se actualiza el numero de mensajes enviados desde el remitente original
  #se comparten nuevos mensajes en dispositivos vecinos que esten cercanos
  #ejecuta la transmision de mensajes con base en el algoritmo epsilon-miope
  #actualiza la lista de mensajes que estan en circulacion


#IN: seg,messages,H,inicial,packet,vetado,parametros,lim_msg,palanca_mala
  #seg: segundo(paso dado por el usuario) en el que va la iteracion
  #messages: lista de mensajes que se van a transmitir durante la simulacion
  #Matriz con todos los pasos, coordenadas, iteraciones e ID de dispositivo en toda la simulacion
  #inicial: contador de referencia
        #1 nos indica si es la primera iteracion y por lo tanto update_messages == msn; se salta esta actualizacion; 0 e.o.c.
  #packet: matriz con la lista de mensajes inrtecambiados en la iteracion
  #parametros: palancas seleccionadas por cada usuario (a utilizar hasta el sig periodo de seleccion)
  #lim_msg: limite de mensajes que se pueden almacenar en la memoria
  #palanca_mala: el indice de la palanca ineficiente (con un valor igual a 0)

#OUT: Regresa una matriz actualizada sobre los mensajes transmitidos hasta el paso correspondiente en la simulacion


steps_aleat_palanca_mala <- function(seg,messages,H,inicial,packet,vetado,parametros,lim_msg,palanca_mala){
  
  M <- filter(H,iteracion == seg)#Pedazo de matriz que vamos a utilizar en la simulación
  
  #Verificamos si, ademas de los mensajes que tiene msn, hay nuevos envios que se realicen en esta iteracion
  update_messages <- filter(messages, tiempo_envio == seg)
  #vamos a borrar aquellos mensajes por enviar que estan en poder de alguien con palanca_mala en la iteracion
  no_transmiten <- as.matrix(filter(parametros, tipo_parametro == palanca_mala) %>% select(usuario))#usuarios que no participan
  update_messages <- filter(update_messages, !origen %in% as.matrix(no_transmiten))
  
  if( (nrow(update_messages) > 0)){
    #Paso los nuevos mensajes al formato Packet
    packet_bis <- data.frame(matrix(0,nrow(update_messages),3))
    colnames(packet_bis) <- c("portador","num_mensaje","tiempo_recepcion")
    packet_bis$portador <- update_messages$origen
    packet_bis$num_mensaje <- update_messages$num_mensaje
    packet_bis$tiempo_recepcion <- update_messages$tiempo_envio
    packet_bis$fuente <- update_messages$origen#esta columna asi aplica porque son nuevos mensajes enviados (originalmente)
    packet_bis$destino <- update_messages$destino
  } 
  
  if (inicial != 1 & (nrow(update_messages) > 0)){
    packet <- rbind(packet,packet_bis)#actualizacion de nuevos mensajes a enviar
  }
  
  #Ahora vamos a transmitir, recibir todos los mensajes por interacción (cercanía)
  packet_future <- data.frame(matrix(0,0,ncol(packet)))#vamos a ir acumulando las neuvas iteraciones para que la matriz
  #original msn no crezca y altere el ciclo de transmision
  colnames(packet_future) <- colnames(packet)
  
  for (i in 1:nrow(packet)){
    cerca <- cercanos(M[packet$portador[i],5:6],select(M,X_cart,Y_cart),r)
    #quito a todos los usuarios que ya tienen el mensaje y al propio usuario (para no duplicar envios/conteo)
    cerca <- setdiff(cerca,filter(packet,num_mensaje == packet$num_mensaje[i]) %>% select(portador) %>% as.matrix())
    cerca <- setdiff(cerca,packet$portador[i])
    #quitamos los mensajes que estan vetados
    cerca <- setdiff(cerca,vetado$usuario[which(vetado$num_mensaje == packet$num_mensaje[i])])
    
    #En esta parte vamos a ver quien va a recibir los mensajes y con que probabilidad
    #Aqui se implementa el algoritmo epsilon-miope para la transmision de mensajes
    veto <- c()
    if(length(cerca)>0){
      for (k in 1:length(cerca)){
        if (filter(parametros,usuario == cerca[k])[nrow(filter(parametros,usuario == cerca[k])),3] < runif(1)){#no recibo - veto
          veto <- rbind(veto,cerca[k])
        }
      }
    }
    
    #Actualizamos a los mensajes
    cerca <- setdiff(cerca,veto)
    #vamos a actualizar la lista de los mensajes
    if(length(veto)>0){
      vetado_update <- data.frame(matrix(0,length(veto),3))
      colnames(vetado_update) <- c("usuario","num_mensaje","tiempo_veto")
      vetado_update$usuario <- veto
      vetado_update$num_mensaje <- packet$num_mensaje[i]
      vetado_update$tiempo_veto <- seg
      vetado <- rbind(vetado,vetado_update)
    }
    
    #En esta parte tendriamos que ver si no le enviamos ya el mismo mensaje a un mismo usuario cercano, esto es redundante
    if(length(cerca) >0){#si existen mensajes que debamos transmitir
      packet_update <- data.frame(matrix(0,length(cerca),3))
      colnames(packet_update) <- c("portador","num_mensaje","tiempo_recepcion")
      packet_update$portador <- cerca
      packet_update$num_mensaje <- packet$num_mensaje[i]
      packet_update$tiempo_recepcion <- seg
      packet_update$fuente <- packet$portador[i]#de donde nos llego este nuevo mensaje
      packet_update$destino <- packet$destino[i]
      #Actualizo
      packet_future <- rbind(packet_future,packet_update)
    }
  }
  #Acumulo todo lo nuevo de msn
  packet <- rbind(packet, packet_future)
  
  #Quitamos todo aquello que esta repetido (a excepcion del tiempo de envio, pues es importante para las metricas de la simulacion0)
  packet$cve <- paste(packet$portador,packet$destino,packet$num_mensaje,sep='-')
  packet <- packet[!duplicated(packet$cve), ]
  packet <- select(packet,-cve)#quito la columna de clave pues solo es auxiliar en algunas iteraciones

  return(list(packet,vetado))
}