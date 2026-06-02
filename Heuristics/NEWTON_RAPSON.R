#__________________________________________________
#-------------NEWTON RAPSON------------------------
#__________________________________________________


# METODO NEWTON RAPSON 

#Este metodo consiste en sacar la derivada desde un punto y 
#comparar si la derivada es mejor que el punto inicial y ir sustituyendo 
#y repetir el proceso con el nuevo resultado encontrado hasta que 
#sea el mejor encontrado en donde se encuentra la raiz .

##NEWTONPROFE.R
##REVISION 17-10-2020 NEWTON RAPSON

library(ggplot2)

newton <- function(Fx,df,p_0,maxiter=50,tol=1.0e-6)
{
  i <- 1                # se inicializa el contador
  while(i <= maxiter)
  {
    p <- p_0 - (Fx(p_0)/df(p_0)) # se calcula p
    if(abs(p - p_0) < tol)
    {
      return(p)          # regresa el valor de p
      break              # termina el ciclo
    }else
    {
      i <- i + 1        # aumento de contador
      p_0 <- p          # se redefine p_0
    }
  }
}

#_________________________________________________________
#---------------------IMPLEMENTACION----------------------
#_________________________________________________________

### DESVENTAJA LINEA 9 DIVISION ENTRE CERO 
## SE PUEDE X SALIR DEL RANGO A SALIRSE DE INTERVALO DEL 
##   QUE SE ESTA EVALUANDO

### SE DEFINIRA EL ESPACIO DE BUSQUEDA TOMESE COMO EJEMPLO 

fx <- function(x){return(cos(x) + sin(x))}

## esta parte es entrada
# y definida por el problema 

dfx <- function(x){return((-sin(x))+cos(x))}

# se define el espacio de busqueda 

a<- 10
b <- -10                        ## se estan definiendo los intervalos en donde se buscara la raiz
steps <- 0.01
x <- seq(b,a,steps)
y <- c()

for(i in seq_along(x)){y[i] <- fx(x[i])}

p_0 <- -10                    # punto inicial
raiz <- newton(fx,dfx,p_0)   #obtencion de la raiz

#Encontrar multiples raices
#raiz1<- newton(fx,dfx, 0)

df <- data.frame(x,y)

#Graficacion 

ggplot(data= df, mapping=aes(x=x,y=y))+
  geom_line(color="red")+
  annotate(geom="text", x=raiz, y=5, label="Raiz")+
  annotate(geom="point",x=raiz,y=0,size=5, shape=14, fill="transparent")#+

### para encontar multiples raices solo quitese # y tambien el comentario anterior [#+] solodejse + 
# si no nograficara doble y dara error 
#annotate(geom="text", x=raiz1, y=5, label="Raiz1")+
#annotate(geom="point",x=raiz1,y=0,size=5, shape=14, fill="transparent")





