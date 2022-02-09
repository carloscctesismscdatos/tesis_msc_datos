#Parámetros utilizados para correr las simulaciones reportadas en la tesis

r <- 80
periodo <- 20
epsilon <- .5
lim_msg <- 3
valor_param <-  c(1,.7,.3,0)

#Lugar en donde estan todas las rutinas
setwd("Entrega/code")
source('main.R')

paquete <- main(r,periodo,epsilon,lim_msg,valor_param)