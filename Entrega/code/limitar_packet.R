#limitar_packet.R
#Descripción: Limita el tamaño de los packet de mensajes que cada usuario puede cargar
#IN: 
  #packet matriz con info sobre los mensajes que carga cada usuario
  #lim_msg: el número máximo de mensajes que un usuario puede cargar en memoria
#OUT: el packet en el que cada usuario carga un número de mensajes menor o igual a lim_msg

limitar_packet <- function(packet,lim_msg){
  msg_portador <- packet %>% group_by(portador) %>% arrange(portador,tiempo_recepcion) %>% data.frame()
  msg_portador_tab <- data.frame(table(msg_portador$portador))
  colnames(msg_portador_tab) <- c("portador","mensajes")
  
  #Portadores de mensajes en exceso (se borraran algunos)
  quitar_msg <- filter(msg_portador_tab,mensajes > lim_msg) %>% select(portador) %>% as.matrix() %>% as.numeric()
  #Portadores con espacio aun
  dejar_msg <- filter(msg_portador_tab,mensajes <= lim_msg) %>% select(portador) %>% as.matrix() %>% as.numeric()
  
  #Rehacemos a matriz de packet, con los respectivos limites
  packet_lim <- data.frame(matrix(0,0,ncol(msg_portador)))
  colnames(packet_lim) <- colnames(msg_portador)
  #Acumulamos los portadores que aun tienen espacio
  packet_lim <- rbind(packet_lim,filter(msg_portador,portador %in% dejar_msg))
  #Limitamos aquellos portadores que han excedido el limite
  if(length(quitar_msg) > 0){
    for (i in 1:(length(quitar_msg))){
      packet_lim <- rbind(packet_lim,filter(msg_portador,portador == quitar_msg[i]) %>% arrange(-tiempo_recepcion) %>% head(lim_msg))
    }
  }
  
  #Reordenamos nuestra matriz
  packet_lim <- packet_lim %>% group_by(portador) %>% arrange(portador,tiempo_recepcion) %>% data.frame()
  return(packet_lim)
}

