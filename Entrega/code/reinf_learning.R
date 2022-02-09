#reinf_learning.R
#Descripción: esta rutina se encarga de ejecutar el proceso completo de transmision de mensajes en la simulacion
#IN:  
  #r: radio de transmision (puede ser desde 40 a 100 metros)
  #periodo: periodo de seleccion (numero de pasos hasta que los usuarios vuelven a escoger una nueva palanca)
  #epsilon: valor del epsilon del algoritmo epsilon-miope
  #lim_msg: limite de mensajes (puede ser 3 o 6)
#OUT: matrices con los resultados y calculos a lo largo de la simulacion

reinf_learning <- function(r,periodo,epsilon,lim_msg,valor_param){

#Datos
#Coordenadas, id e iteracion de los pasos que dan todos los usuarios de la simulacion
H <- read_csv("https://www.dropbox.com/s/d3nbw94we00ldxx/Master_3000.csv?dl=1")#3,000 pasos exactos; 249 usuarios
#Lista de 97 mensajes que van a intercambiar los usuarios de la simulacion
messages <- read_csv("https://www.dropbox.com/s/q605tplmey33hgu/messages_3000.csv?dl=1")#messages con los de 3,000 pasos

##############################################################################################################
##############################################################################################################
######################           Inicio de la simulacion         #############################################
##############################################################################################################
##############################################################################################################

#Numero de veces que se jala la palanca (veces en las que se escogen nuevas tasas de transmision con el algoritmo epsilon-miope)
iteraciones <- 25 

#Primero mensajes a enviar
msn <- filter(messages, tiempo_envio == min(messages$tiempo_envio))
seg <- msn$tiempo_envio[1]#este es el momento en el que inicia la simulacion, el momento del envio del primer mensaje

#es un auxiliar parametro que vamos a enviar a otras funciones auxiliares
inicial = 1#la primera iteracion

#Packet es una matriz con informacion sobre los mensajes que van siendo enviados/recibidos
packet <- data.frame(matrix(0,nrow(msn),3))
colnames(packet) <- c("portador","num_mensaje","tiempo_recepcion")
packet$portador <- msn$origen
packet$num_mensaje <- msn$num_mensaje
packet$tiempo_recepcion <- msn$tiempo_envio
packet$fuente <- msn$origen#usuario desde el que llego el mensaje - si fuente == portador, el mismo usuario escribio
packet$destino <- msn$destino
vetado <- data.frame(matrix(0,0,3))
colnames(vetado) <- c("usuario","num_mensaje","tiempo_veto")

#Tipo de usuario (de acuerdo a la palanca que van a utilizar)
#Los usuarios escoger una palanca que utilizaran durante el primer periodo de seleccion
parametros <- data.frame(matrix(0,length(unique(H$indice)),3))
colnames(parametros) <- c("usuario","tipo_parametro","valor")
parametros$usuario <- unique(H$indice)
parametros$tipo_parametro <- sample.int(n = length(valor_param),size = length(unique(H$indice)),replace = TRUE)
parametros$valor <- valor_param[parametros$tipo_parametro]
parametros$cve <- paste(parametros$usuario,parametros$tipo_parametro,sep = "-")
#Palanca ineficiente y con valor igual a cero
palanca_mala <- max(parametros$tipo_parametro)


##############################################################################################################
##############################################################################################################
######################  Reinforcement learning         #######################################################
##############################################################################################################
##############################################################################################################

#Limites para las matrices
#Calcula los pasos (segundos) a considerar de acuerdo al periodo de seleccion
lim_matriz <- limites_matriz(messages,iteraciones,periodo)

#Primera iteracion
packet <- data.frame(matrix(0,nrow(msn),5))
colnames(packet) <- c("portador","num_mensaje","tiempo_recepcion","fuente","destino")
packet$portador <- msn$origen
packet$fuente <- msn$origen
packet$destino <- msn$destino
packet$num_mensaje <- msn$num_mensaje
packet$tiempo_recepcion <- msn$tiempo_envio

#P acumula todas las iteraciones de los packet
P <- packet
P$iteracion <- seg

seg <- seg + 1#actualizo el momento de la iteracion
inicial = 0#en adelante ya tenemos otro estatus en la iteracion

#Hacemos la primera iteracion en donde se transmiten y reciben los primeros mensajes entre los usuarios
for (i in 1:periodo){
  paquete <- steps_aleat_palanca_mala(seg,messages,H,inicial,packet,vetado,parametros,lim_msg,palanca_mala)
  packet <- paquete[[1]]
  vetado <- paquete[[2]]
  
  packet$consecutivo <- 1:nrow(packet)
  no_transmitir <- filter(parametros, tipo_parametro == palanca_mala) %>% select(usuario) %>% as.matrix()
  quitar <- filter(packet,tiempo_recepcion == seg, portador %in% no_transmitir | fuente %in% no_transmitir) %>% select(consecutivo) %>% as.matrix()
  packet <- packet[setdiff(1:nrow(packet),quitar),] %>% select(-consecutivo)
  
  #Limitamos el tamaño del packet en cada usuario, el cual debe ser menor o igual a lim_mgs
  packet <- limitar_packet(packet,lim_msg)
  Q <- packet
  #Acumulacion de los packet en cada iteracion
  Q$iteracion <- seg
  P <- rbind(P,Q)
  
  seg <- seg + 1#actualizo el momento de la iteracion
  i <- i+1
}


################################################################################################################
# En este paso hacemos el resto de las iteraciones (veces en que se jala la palanca y se actualiza para todos los usuarios)

it <- 1# primera iteracion

#Matriz auxiliar que proviene de la matriz P
hola <- filter(P, tiempo_recepcion == iteracion,iteracion >= lim_matriz$minimo[it], iteracion <= lim_matriz$maximo[it])
met <- metricas_local(hola,H)

################################################################################################################
# Registros de la matriz de aprendizaje reforzado

#Creamos una matriz para almacenar el rendimiento de cada palanca a lo largo de la simulación
reinf_matrix <- data.frame(matrix(0,max(H$indice) * max(parametros$tipo_parametro),5))
colnames(reinf_matrix) <- c("iter","brazo","usuario","conteo","sumas")
reinf_matrix$iter <- it
reinf_matrix$brazo <- rep(1:length(valor_param),max(H$indice))
reinf_matrix$usuario <- sort(rep(1:max(H$indice),max(parametros$tipo_parametro)))

#Vector para actualizar los resultados de la matriz
#Aqui estan todas las entradas de brazo+usuario que nos interesan para actializar la reinf_matrix
vec <- c()
for (j in 1:max(H$indice)){
  vec <- rbind(vec,which(paste(reinf_matrix$usuario,reinf_matrix$brazo,sep = "-") == parametros$cve[j]))
}

#Registro que en la iteracion se transmitio en una iteracion
reinf_matrix$conteo[vec] <- reinf_matrix$conteo[vec] + 1
#Registro el numero de mensajes transmitidos en la iteracion
reinf_matrix$sumas[vec] <- met$mensajes_fuente

#tasa es el promedio de mensajes entregados en cada conteo
reinf_matrix$tasa <- 0
reinf_matrix$tasa[which((reinf_matrix$sumas / reinf_matrix$conteo) > 0)] <- reinf_matrix$sumas[which((reinf_matrix$sumas / reinf_matrix$conteo) > 0)] / reinf_matrix$conteo[which((reinf_matrix$sumas / reinf_matrix$conteo) > 0)]

#Palanca que maximiza el rendimiento
palanca_max <- reinf_matrix %>% group_by(usuario) %>% summarise(mejor = which.max(tasa))

#Actualizo los parametros de acuerdo al algoritmo epsilon - miope
for (user in 1:nrow(parametros)){
  if(runif(1) <= epsilon){
    #explorar
    parametros$tipo_parametro[user] <- sample.int(length(valor_param), 1)
    parametros$valor[user] <- valor_param[parametros$tipo_parametro[user]]
  } else {
    #explotar
    #mejor
    parametros$tipo_parametro[user] <- palanca_max$mejor[user]
    parametros$valor[user] <- valor_param[palanca_max$mejor[user]]
  }
}
parametros$cve <- paste(parametros$usuario,parametros$tipo_parametro,sep = "-")

#Reinforcement learning duracion
total <- periodo * iteraciones 

while(it <= iteraciones){
  for(per in 1:periodo){
    paquete <- steps_aleat_palanca_mala(seg,messages,H,inicial,packet,vetado,parametros,lim_msg,palanca_mala)
    packet <- paquete[[1]]
    vetado <- paquete[[2]]
    
    packet$consecutivo <- 1:nrow(packet)
    no_transmitir <- filter(parametros, tipo_parametro == palanca_mala) %>% select(usuario) %>% as.matrix()
    quitar <- filter(packet,tiempo_recepcion == seg, portador %in% no_transmitir | fuente %in% no_transmitir) %>% select(consecutivo) %>% as.matrix()
    packet <- packet[setdiff(1:nrow(packet),quitar),] %>% select(-consecutivo)
    
    #Limitamos el tamaño del packet en cada usuario
    packet <- limitar_packet(packet,lim_msg)
    
    Q <- packet
    #Acumulacion de los packet
    Q$iteracion <- seg
    P <- rbind(P,Q)
    
    seg <- seg + 1#actualizo el momento de la iteracion
    print(c(it,per))
  }
  it <- it +1#iteraciones
  
  hola <- filter(P, tiempo_recepcion == iteracion, iteracion <= lim_matriz$maximo[it], iteracion >= lim_matriz$minimo[it])
  met <- metricas_local(hola,H)
  
  #Actualizamos los resultados en una matriz de aprendizaje reforzado auxiliar que despues vamos a incorporar
  reinf_update <- data.frame(matrix(0,max(H$indice) * length(valor_param),5))
  colnames(reinf_update) <- c("iter","brazo","usuario","conteo","sumas")
  reinf_update$iter <- it
  reinf_update$brazo <- rep(1:length(valor_param),max(H$indice))
  reinf_update$usuario <- sort(rep(1:max(H$indice),length(valor_param)))
  
  #Vector para actualizar los resultados de la matriz
  vec <- c()
  for (j in 1:max(H$indice)){
    vec <- rbind(vec,which(paste(reinf_update$usuario,reinf_update$brazo,sep = "-") == parametros$cve[j]))
  }
  
  #Cargamos los valores de la iteracion anterior
  reinf_update$conteo <- filter(reinf_matrix, iter == it -1) %>% select(conteo) %>% as.matrix()
  reinf_update$sumas <- filter(reinf_matrix, iter == it -1) %>% select(sumas) %>% as.matrix()
  
  #Actualizamos con los datos del periodo, vec son aquellos datos que si deben actualizarse
  reinf_update$conteo[vec] <- reinf_update$conteo[vec] + 1
  reinf_update$sumas[vec] <- reinf_update$sumas[vec] + met$mensajes_fuente
  #tasa es el promedio de mensajes entregados en cada conteo
  reinf_update$tasa <- 0
  reinf_update$tasa[which((reinf_update$sumas / reinf_update$conteo) > 0)] <- reinf_update$sumas[which((reinf_update$sumas / reinf_update$conteo) > 0)] / reinf_update$conteo[which((reinf_update$sumas / reinf_update$conteo) > 0)]
  
  #Palanca que maximiza el rendimiento
  palanca_max <- reinf_update %>% group_by(usuario) %>% summarise(mejor = which.max(tasa))
  
  #Actualizo los parametros de acuerdo al algoritmo epsilon - miope
  for (user in 1:nrow(parametros)){
    if(runif(1) <= epsilon){
      #explorar
      parametros$tipo_parametro[user] <- sample.int(length(valor_param), 1)
      parametros$valor[user] <- valor_param[parametros$tipo_parametro[user]]
    } else {
      #explotar
      #mejor
      parametros$tipo_parametro[user] <- palanca_max$mejor[user]
      parametros$valor[user] <- valor_param[palanca_max$mejor[user]]
    }
  }
  parametros$cve <- paste(parametros$usuario,parametros$tipo_parametro,sep = "-")
  
  reinf_matrix <- rbind(reinf_matrix,reinf_update)
  
  print(it)
}

return(list(P,reinf_matrix))
}
  