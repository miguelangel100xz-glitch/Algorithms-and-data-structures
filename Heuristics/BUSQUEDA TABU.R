#___________________________________________________________________
#----------------------------BUSQUEDA TABU--------------------------
#___________________________________________________________________

##TABU.R file ##
##revision doble 
#-------------------------------------
#source("BUSQUEDACIEGACOMPLETA.R")
#-------------------------------------

fsearch<-function(sol,Fx,type="min", ...){
  
  x<- apply(sol,1,Fx,...)
  ib <- switch(type ,min= which.min(x), max=which.max(x))
  return(list(index=ib, sol=sol[ib],eval=x[ib]))
}  



#______________________________________________________________________
#----------------------------------------------------------------------
##funcion de recocido simulado 
## temperaturas altas es montecarlotemperaturas bajas ascenso a la colina
#----------------------------------------------------------------------
#______________________________________________________________________

#source("annealingk.R")  
##annealingk.R file###
##llamar fsearch en un momento 
#source("BUSQUEDACIEGACOMPLETA.r")
##vecindario
#N numero de muestras 

neighborhood <- function(lower,upper, N)
{
  D <- length(lower)
  s <- matrix(nrow=N, ncol= D)
  for(i in 1:D) s[,i] <- runif(N,lower[i],upper[i])
  return(s)
} 

#_______________________________________________________________
#---------------------------------------------------------------

sannealing <- function(X,Fx,lower,upper,type="min",w=10,N=1000,maxiter=10000,...)
{
  best <- fsearch(neighborhood(lower,upper,N),Fx,type,...) 
  best_1<- list(sol=X, eval=Fx(X,...))
  l <- 0  
  while(l < maxiter)
  {
    if(type=="min" && best$eval < best_1$eval && l < maxiter){
      for(i in 1:w){
        if(best$eval < best_1$eval) best_1 <- best else best <- fsearch(neighborhood(lower,upper,N),Fx,type,...) 
        l <- l+1
      }
    }else if(type=="max" && best$eval > best_1$eval && l < maxiter){
      for(i in 1:w){
        if(best$eval > best_1$eval) best_1 <- best else best <- fsearch(neighborhood(lower,upper,N),Fx,type,...) 
        l <- l+1
      }
    }else{
      break
    }
    
  }
  
  return(best_1)
  
}

#_______________________________________________________
#-------------------------------------------------------
#_______________________________________________________
#Este algoritmo maneja un metodo en donde 
#almacena los maximos locales y los compara para encontrar 
#el maximo real de la funcion y guarda las evaluaciones para 
#no repetir las busquedas. Y compara y retorna el mejor resultao 
#minimo y maximo de la funcion evaluada 

tabu <- function(x, Fx, lower, upper, type="min", L=10, N=100, maxiter=100, ...)
{
  lista <- vector("list", L)
  l <- 0
  s <- list(sol=x, eval=Fx(x, ...))
  while(l < maxiter)
  {    
    clist <- c()
    for(i in 1:N)
    {
      s1 <- fsearch(neighborhood(lower, upper, N), Fx, type, ...)
      for(j in 1:length(lista))
      {
        if(s1$sol %in% lista[[j]]) invisible() else clist <- c(clist, s1$sol)
      }
    }
    s1 <- fsearch(matrix(clist, nrow = length(clist)), Fx, type, ...)
    if(type == "min" && s1$eval < s$eval)
    {
      lista <- c(lista[2:length(lista)],list(s1))
      s <- s1
    }else
    {
      break
    }
    l <- l +1
  }
  return(s)
}

#_______________________________________________________________________
#----------------------------IMPLEMENTACION-----------------------------
#_______________________________________________________________________

limI<- -10
limS<- 10
var <- 1
upper <- rep (limS,var)
lower <- rep(limI,var)

x <- 3

fx1<-function(x){return(sqrt(2*x^2-7))}

fx <- fx1
label <- "funcion 1"

min<- tabu(x,fx,lower,upper,type="min")
max<- tabu(x,fx,lower,upper,type="max")

cat(c(label,"Solucion minima: ",round(min$sol,3),"Evaluacion: ",round(min$eval,3),"\n"))
cat(c(label,"Solucion maxima: ",round(max$sol,3),"Evaluacion: ",round(max$eval,3),"\n"))

#______________________________________________________________________
#--------------------------------VARIACIONES---------------------------
#______________________________________________________________________
#Es importante revisar los decimales ya que si quiere 
#una mejor aproximacion se le puede eliminar el rond 
## VARIACIONES DE LA IMPLEMENTACION
#En estas variaciones se le implmenta otros valores 
#distintos a los de la funcion original.

limI<- -10
limS<- 10
var <- 1
upper <- rep (limS,var)
lower <- rep(limI,var)

x <- 3

fx1<-function(x){return(sqrt(2*x^2-7))}

fx <- fx1
label <- "funcion 1"

min<- tabu(x,fx,lower,upper,type="min",L=15,N=50,maxiter=10)
max<- tabu(x,fx,lower,upper,type="max",L=15,N=50,maxiter=10)

cat(c(label,"Solucion minima: ",round(min$sol,3),"Evaluacion: ",round(min$eval,3),"\n"))
cat(c(label,"Solucion maxima: ",round(max$sol,3),"Evaluacion: ",round(max$eval,3),"\n"))

