#____________________________________________________________
#---------------------MONTECARLO-----------------------------
#____________________________________________________________

###fsearchmontecarlo.R

fsearch<-function(sol,Fx,type="min", ...){
  
  x<- apply(sol,1,Fx,...)
  ib <- switch(type ,min= which.min(x), max=which.max(x))
  return(list(index=ib, sol=sol[ib],eval=x[ib]))
  
}

###montecarlo.R 
#file

#source("fsearchmontecarlo.R")

mcsearch<- function(N,lower, upper,Fx, type="min",...)
{
  D <- length(lower)        #vars
  s<- matrix(nrow=N,ncol=D) #espacio de busqueda
  for(i in 1:N) s[i,] = runif(D, lower, upper)   #numeros aleatorios
  fsearch(sol=s, Fx=Fx, type=type, ...)
  
}  
#___________________________________________________________________
#------------------------------IMPLEMENTACION-----------------------
#___________________________________________________________________

N<- 1000
var<- 1
limI <- -10
limS<- 10
upper<- rep(limS,var)
lower<- rep(limI,var)
x<-1

fx1<- function(x){return(x^2+3*x-2)}
label="Ecuacion 1"
fx<- fx1

min1=mcsearch(N,lower,upper,fx,"min")
max1=mcsearch(N,lower,upper,fx,"max")
cat(c("solucion minima: ",min1$sol,"\n","Evaluacion: ",min1$eval,"\n"))
cat(c("solucion maxima: ",max1$sol,"\n","Evaluacion: ",max1$eval,"\n"))

#_____________________________________________________________________
#--------------------------MAS DE UNA VARIABLE------------------------
#_____________________________________________________________________

N<- c(10,100)
D<- c(2,3,10,50)

fx1<-function(x){return((x^2 + 3)*(x^2 -4*x))}

label="Ecuacion 1"
fx<- fx1
for (j in 1:length(N)){
  for (i in 1:length(D)){
    min=mcsearch(N[j],rep(-5.2,D[i]),rep(5.2,D[i]),fx,"min")
    max=mcsearch(N[j],rep(-5.2,D[i]),rep(5.2,D[i]),fx,"max")
    cat("\n",label,"D:",D[i],"N:",N[j],"n")
    cat(c("solucion minima: ",min$sol,"\n","Evaluacion: ",min$eval,"\n"))
    cat(c("solucion maxima: ",max$sol,"\n","Evaluacion: ",max$eval,"\n"))
    
  }
}
