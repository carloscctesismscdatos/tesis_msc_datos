main <- function(r,periodo,epsilon,lim_msg,valor_param){
  #Paquetes a utilizar para la simulacion  
  library(readr)
  library(dplyr)
  library(tidyverse)
  
  #Funciones auxiliares
  source('cercanos.R')
  source('distancia.R')
  source('limitar_packet.R')
  source('limites_matriz.R')
  source('metricas_local.R')
  source('steps_aleat_palanca_mala.R')
  source('reinf_learning.R')
  
  #P,reinf_matrix)
  paquete <- reinf_learning(r,periodo,epsilon,lim_msg,valor_param)
  #paquete[[1]]: matriz P (historico de todos los intercambios efectivos de mensajes)
  #paquete[[1]]: matriz de aprendizaje reforzado (preferencias y rendimientos por palancas)
}
