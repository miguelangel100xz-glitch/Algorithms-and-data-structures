#______________________________________________________
#----------------BUSQUEDA CIEGA PROFUNDA---------------
#______________________________________________________

dfsearch<-function (dominio, Fx, l = 1, b = 1, type = "min", D = length(dominio),
                    x = rep(NA, D), bsol = switch(type, min = list(sol = NULL,
                                                                   eval = Inf), max = list(sol = NULL, eval = -Inf)), ...)
{  if ((l - 1) == D) 
{        f <- Fx(x, ...)
fb <- bsol$eval
ib <- switch(type, min = which.min(c(fb, f)), max = which.max(c(fb, f)))
if (ib == 1)  return(bsol)  else return(list(index = ib, sol = x, eval = f, bsol = x[ib],
                                             beval = f[ib]))} 
  else {        for (j in 1:length(dominio[[l]])) {          x[l] <- dominio[[l]][j]
  bsol <- dfsearch(dominio, Fx, l = l + 1, b = j, type = type,
                   D = D, x = x, bsol = bsol, ...)   }
    return(bsol) 
  }
}

#___________________________________________________________
#------------------IMPLEMENTACION DE LA FUNCION-------------
#___________________________________________________________

#creamos un espacio de busqueda 

dfx1<-function (x)
{   return(2 * x + 3)}
m<- 6#numero de variables
n<- 6#tamaÃ±o del espacio
#definicion del espacio de busqueda
dominio<-matrix(data=sample(0:9, m*n,TRUE),n,m)
print("Espacio de busqueda")

dominio

#Busqueda ciega en profundidad
min<-dfsearch(dominio=dominio, Fx=dfx1, type="min")
max<-dfsearch(dominio=dominio, Fx=dfx1, type="max")

##aqui solo se imprime el resultado 
cat(c("solucion minima:", min$sol, "\n", "Evaluacion:", min$eval,"\n"))
cat(c("solucion maxima:", max$sol, "\n", "Evaluacion:", max$eval,"\n"))

cat(c("solucion minima:", min$bsol, "\n", "Evaluacion:", min$beval,"\n"))
cat(c("solucion maxima:", max$bsol, "\n", "Evaluacion:", max$beval,"\n"))





