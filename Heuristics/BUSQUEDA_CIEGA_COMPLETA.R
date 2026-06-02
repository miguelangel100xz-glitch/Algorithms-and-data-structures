#____________________________________________________________
#------------------BUSQUEDA CIEGA COMPLETA-------------------
#____________________________________________________________


fsearch<-function(sol,Fx,type="min", ...){
  
  x<- apply(sol,1,Fx,...)
  ib <- switch(type ,min= which.min(x), max=which.max(x))
  return(list(index=ib, sol=sol[ib],eval=x[ib]))
  
}  


#_____________________________________________________________
#----------------------IMPLEMENTACION-------------------------
#_____________________________________________________________

fx<-function(x)
{
  return(2*x+3 )
}
#funcion a evaluar

m <- 5
n <- 5

dominio <- matrix(data=sample(0:9,m*n,TRUE),n,m)
dominio 
print("espacio de busqueda")
min<- fsearch(sol=dominio, Fx=fx,type="min")
min$sol
min$eval

max<-fsearch(sol=dominio,Fx=fx,type="max")
max$sol
max$eval
cat(c("solucion maxima", max$sol, "\n","Evalucion",max$eval ,"\n"))
cat(c("Solucion minima",min$sol, "\n","Evaluacion",min$eval ,"\n"))





