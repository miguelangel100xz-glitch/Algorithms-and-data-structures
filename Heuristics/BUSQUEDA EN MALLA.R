#__________________________________________________
#----------------BUSQUEDA EN MALLA-----------------
#__________________________________________________

#fsearchgrid.R

fsearch<-function(sol,Fx,type="min", ...){
  
  x<- apply(sol,1,Fx,...)
  ib <- switch(type ,min= which.min(x), max=which.max(x))
  return(list(index=ib, sol=sol[ib],eval=x[ib]))
}

##gridrev.R
#source("fsearchgrid.R")

gsearch <- function(step, lower, upper, FUN, type="min",...)
{
  D <- length(step)
  domain <-vector("list",D)
  L <- vector(length=D)
  for(i in 1:D)
  {
    domain[[i]] <- seq(lower[i], upper[i], by=step[i])
    L[i] <- length(domain[[i]])
  }
  Ls <- prod(L)
  sol <- matrix(ncol=D, nrow=Ls)
  for( i in 1:D)
  {
    if(i == 1) E <-1 else E <- E + L[i + 1]
    sol[,i] <- rep(domain[[i]], length.out=Ls, each=E)
  }
  fsearch(sol=sol, Fx=FUN, type=type, ...)
}

#________________________________________________________________
#---------------------IMPLEMENTACION-----------------------------
#________________________________________________________________

fx1 <- function(x){return(x^3 + 6*x -1)}

limI <- -2
limS <- 3
steps <- 0.01
vars <- 1

upper <- rep(limS, vars)
lower <- rep(limI, vars)
step <- rep(steps, vars)

min <- gsearch(step,lower, upper,fx1, type="min")
max <- gsearch(step,lower, upper,fx1, type="max")
cat(c("solucion maxima", max$sol,"\n","Evalucion",max$eval,"\n"))
cat(c("Solucion minima",min$sol,"\n","Evaluacion",min$eval))

#________________________________________________________________
#_---------------------------GRAFICAS----------------------------
#________________________________________________________________
##graficos.R

library(ggplot2)
graph<- function(limI, limS, steps, Fx, min, max)
{
  x <- seq(limI,limS,steps)
  y<- c()
  for(i in seq_along(x))
  {
    y[i] <- Fx(x[i])
  }
  df <-data.frame(x,y)
  ggplot(data=df, mapping=aes(x=x, y=y))+
    geom_line(color=4)+
    annotate(geom="text", x=min$sol-3, y=min$eval,label="minimo")+
    annotate(geom="point", x=min$sol, y=min$eval,size=5,shape=14,fill="transparent")+
    annotate(geom="text", x=max$sol-3, y=max$eval,label="maximo")+
    annotate(geom="point", x=max$sol, y=max$eval,size=5,shape=14,fill="transparent")
}

graph(limI,limS, steps,fx1,min,max)



