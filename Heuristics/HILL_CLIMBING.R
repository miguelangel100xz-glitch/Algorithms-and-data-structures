#______________________________________________________
#----------------HILL CLIMBING-------------------------
#______________________________________________________

#fsearchgrid.R

fsearch<-function(sol,Fx,type="min", ...){
  
  x<- apply(sol,1,Fx,...)
  ib <- switch(type ,min= which.min(x), max=which.max(x))
  return(list(index=ib, sol=sol[ib],eval=x[ib]))
 }

##hillclimbing.R  file###
#source("fsearchgrid.R")

hclimbing <- function(x, Fx,lower, upper,type="min", N=1000,...)
{
  D <- length(lower)          #var num 
  s <- matrix(nrow= N, ncol=D)
  for(i in 1:N) s[i,] = runif(D,lower, upper)
  best <- fsearch(sol=s, Fx=Fx, type=type,...)
  best_x <- Fx(x,...)
  
  if(type == "min")
  {
    if(best_x < best$eval) return(list(sol=x,eval=best_x)) else hclimbing(best$sol,Fx,lower,upper,type,N,...)
  }else{
    if(best_x > best$eval) return(list(sol=x,eval=best_x)) else hclimbing(best$sol,Fx,lower,upper,type,N,...)
    
  }
  
}

#_______________________________________________________________________
#----------------------------IMPLEMENTACION-----------------------------
#_______________________________________________________________________

N= 1000
limI<- -10
limS<- 10
var <- 1
upper <- rep (limS,var)
lower <- rep(limI,var)

x <- 1
fx1<-function(x){return(2*x+3)}

fx <- fx1
label <- "funcion 1"

min<- hclimbing(x,fx,lower,upper,type="min",N)
max<- hclimbing(x,fx,lower,upper,type="max",N)

cat(c(label,"Solucion minima: ",min$sol,"Evaluacion: ",min$eval,"\n"))
cat(c(label,"Solucion maxima: ",max$sol,"Evaluacion: ",max$eval,"\n"))

#____________________________________________________________________
#--------------------------EXTRA-------------------------------------
#____________________________________________________________________

N= 10000
limI<- -10
limS<- 10
var <- 1
upper <- rep (limS,var)
lower <- rep(limI,var)

x <- 1
fx1<-function(x){return((4*x-1)^3)}

fx <- fx1
label <- "funcion 1"

min<- hclimbing(x,fx,lower,upper,type="min",N)
max<- hclimbing(x,fx,lower,upper,type="max",N)

cat(c(label,"Solucion minima: ",min$sol,"Evaluacion: ",min$eval,"\n"))
cat(c(label,"Solucion maxima: ",max$sol,"Evaluacion: ",max$eval,"\n"))








