#_______________________________________________________________
#---------------------ANNEALING---------------------------------
#_______________________________________________________________

##annealingk.R file###
##llamar fsearch en un momento 
#source("BUSQUEDACIEGACOMPLETA.r")
fsearch<-function(sol,Fx,type="min", ...){
  
  x<- apply(sol,1,Fx,...)
  ib <- switch(type ,min= which.min(x), max=which.max(x))
  return(list(index=ib, sol=sol[ib],eval=x[ib]))
  
}  

##vecindario
#N numero de muestras 
neighborhood <- function(lower,upper, N)
{
  D <- length(lower)
  s <- matrix(nrow=N, ncol= D)
  for(i in 1:D) s[,i] <- runif(N,lower[i],upper[i])
  return(s)
}  

##funcion de recocido simulado 
## temperaturas altas es montecarlotemperaturas bajas ascenso a la colina
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

#_____________________________________________________________
#---------------------IMPLEMENTACIÓN--------------------------
#_____________________________________________________________


limI<- -10
limS<- 10
var <- 1
upper <- rep (limS,var)
lower <- rep(limI,var)

x <- 3

fx1<-function(x){return((4*x-1)^3)}

fx <- fx1
label <- "funcion 1"

min<- sannealing(x,fx,lower,upper,type="min")
max<- sannealing(x,fx,lower,upper,type="max")

cat(c(label,"Solucion minima: ",round(min$sol,3),"Evaluacion: ",round(min$eval,3),"\n"))
cat(c(label,"Solucion maxima: ",round(max$sol,3),"Evaluacion: ",round(max$eval,3),"\n"))

#____________________________________________________________________
#-----------------------------VARIACIONES----------------------------
#____________________________________________________________________
#si queremos cambiar el numero de vecindario en busqueda y numero de muestras y numero de iteraciones 
#w= numero de donde buscara 
#N = numero de muestras 
#y= numero de iteraiones maxiter 





limI<- -10
limS<- 10
var <- 1
upper <- rep (limS,var)
lower <- rep(limI,var)

x <- 3

fx1<-function(x){return((4*x-1)^3)}

fx <- fx1
label <- "funcion 1"

min<- sannealing(x,fx,lower,upper,type="min",w=25,N= 5000,maxiter= 15000)
max<- sannealing(x,fx,lower,upper,type="max",w=25,N= 5000,maxiter= 15000)

cat(c(label,"Solucion minima: ",round(min$sol,3),"Evaluacion: ",round(min$eval,3),"\n"))
cat(c(label,"Solucion maxima: ",round(max$sol,3),"Evaluacion: ",round(max$eval,3),"\n"))














