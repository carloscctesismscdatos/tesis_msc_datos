Códigos de simulación de Tesis
Maestría en Ciencia de Datos ITAM
Carlos Castro Correa 

En el folder Entrega se encuentra la rutina correr_ejemplo.R
Se incluye un ejemplo de los parámetros del main y los valores utilizados en las simulaciones que se reportaron en la tesis.

En el folder Entrega/code se encuentran todas las rutinas programadas en R.
Todas las rutinas se llaman con el main.R:

reinf_learning.R: esta rutina se encarga de ejecutar el proceso completo de transmision de mensajes en la simulacion
metricas_local.R: calcula el numero de mensajes transmitidos por cada dispositivo
limites_matriz.R: con base en el periodo de seleccion, esta matriz nos entrega los pasos (segundo) en los que se selecciona una nueva palanca y el tiempo en los que debe ser utilizada
steps_aleat_palanca_mala.R: esta rutina cumple con varias funciones: se actualiza el numero de mensajes enviados desde el remitente original se comparten nuevos mensajes en dispositivos vecinos que esten cercanos ejecuta la transmision de mensajes con base en el algoritmo epsilon-miope actualiza la lista de mensajes que estan en circulacion
distancia.R: calcula la distancia euclidiana entre cualesquiera dos puntos en coordenadas UTM
cercanos.R: Determina todos los dispositivos que estan a menos r metros de distancia que el punto p
limitar_packet: Limita el tamaño de los packet de mensajes que cada usuario puede cargar

