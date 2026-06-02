#___________________________________________________________________
#-------------------------SIMULACION MONETECARLO--------------------
#___________________________________________________________________
#simulacion montecarlo 
#En repetir o duplicar una caracteristica o comportamiento de un sistema real asemejar 
#lo mas posible a algo real de manera teorica
#Objetivo para reproducir comportamientos que serian muy dificiles de realizar de manera 
#fisica o muy peligrosos 
#entrenamiento de los reactores nucleares 
#Para esto se realizan las simulaciones ficticias 
#la simulacion de montecarlo intenta imitar el comotamiento de una variable 
#del compotamiento futuro
#introduccion de nuevos productos y la probabilidad de un evento 
#politicas de inventario calcular costos y el grado de aceptacion de nuestro publico 
# que tanto tenemos que tener en el almacen segun el requerimiento de cada zona 
#lineas de espera mas o menos realizar una accion 
#Finanzas analisis de riesgo de porcesos financieros 
#Para ver como se comportan las ecuaciones 
#calcular pi 
# crear puntos aleatorios para representar un evento 
##smontecarlo.R file ###

randNum <- function(min, max)
{        
  rango<- max - min
  choice <- runif(1) 
  return(min + rango*choice)
}

smontecarlo <- function(Fx, min, max, smp= 5000 , ...)
{         
  #estimacion
  suma <- 0
  runnig_total <- 0
  for(i in 1:smp)
  {
    x <- randNum(min , max)
    suma <- suma + Fx(x,...)
    x<- randNum(0,max)
    runnig_total <- runnig_total + Fx(x,...)^2
  }
  estimacion <- (max-min)*(suma/smp)##calcular la estimacion del valor
  sumSqr <- runnig_total *(max/smp)# promedio de cuadrados
  sqrAvr <-((runnig_total*max)/smp)^2 #runnig # promedio cuadrado
  varianza <- sumSqr- sqrAvr
  error <- sqrt(abs(varianza/smp))
  return(list(estimacion = estimacion, varianza=varianza,error = error))
}

#_________________________________________________________________________
#--------------------------IMPLEMENTACION---------------------------------
#_________________________________________________________________________
digits <- 5

limI <- -10
limS <- 10
samples <- 100000
fx <- function(x) {return((((x^2+3)^5+x)^2)/(2+((((x^2+1)^4)^3)^4)))}
simulacion <- smontecarlo(fx,limI, limS, samples)


cat(c("Estimacion: ",round(simulacion$estimacion,digits),
      "\n Varianza: ",round(simulacion$varianza,digits),
      "\n error ",round(simulacion$error,digits)),"%")

#__________________________________________________________________________
#-----------------------------------EXTRA----------------------------------
#__________________________________________________________________________

limI <- -10
limS <- 10
samples <- 100000
fx <- function(x) {return(1/((x-3)^2))}
simulacion <- smontecarlo(fx,limI, limS, samples)

cat(c("Estimacion: ",round(simulacion$estimacion,digits),
      "\n Varianza: ",round(simulacion$varianza,digits),
      "\n error ",round(simulacion$error,digits)),"%")

