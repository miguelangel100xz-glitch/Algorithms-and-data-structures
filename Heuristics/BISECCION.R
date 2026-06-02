#____________________________________________
#----------------Biseccion-------------------
#____________________________________________

library("ggplot2")

### documentacion de codigo 15-01-2021
#Este codigo es el explicado y formulado por el profesor

#
# Biseccion 
#Este metodo consiste en dividir el espacio de busqueda a la mitad 
#y evaluar si el cambio de signo esta cerca. Asi se 
#aplica de forma iterativa hasta superar el rango de error.

biseccion <- function(Fx,limI,limS, tol=1.0e-6){
  repeat
  {
    x <- (limI + limS)/ 2     #punto medio
    L <- (limS - limI)/ 2     #intervalo
    
    if(Fx(x) == 0)
    {
      return(x)
      break
      
    }else if(Fx(limI)*Fx(x)<0)
    {
      
      limS <- x         #[a,x]
      
    }else 
    {
      limI <- x         #[x,b]
    }
    if(L < tol)
    {
      return(x)
      break
    } 
    
  }
}
#_______________________________________________
#------------------Implementación---------------
#_______________________________________________

###Este apartado es para definir las funciones
##Primero se define el espacio de Busqueda

fx <- function(x)
{
  return((x^2 / 2) - 3*x + 2)##ES MUY IMPORTANTE QUE SUSTITUYAS EN 
  #ESTE LUGAR LA FUNCION-RAIZ QUE DESEAS ENCONTRAR.
}

limS <- 10  ## este es para declarar el limite 
#superior de la funcion 

limI <- -10 #Al igual este es para el limite inferior 
#notese que el valor es negativo

steps <- 0.01 # aqui se esta definiendo de cuanto 
#en cuanto se va a caminar la funciom


x <- seq(limI , limS, steps)
y <- c()

for( i in seq_along(x)){y[i] <- fx(x[i])} 
df <- data.frame(x,y)


raiz <- biseccion(fx, limI , limS)   ## se llama a la funcion y se obtiene la raiz 


##graficacion 
## para este apartado es necesario que se cargue la libreria 
##library(ggplot2) Porque si no no graficara nada del codigo siguiente 


ggplot(data= df, mapping=aes(x=x , y=y))+
  geom_line(color=4)+
  annotate(geom="text",x=raiz,y=5,label="Raiz")+
  annotate(geom="point", x=raiz, y=0,size=5, shape=14,fill="transparent")













