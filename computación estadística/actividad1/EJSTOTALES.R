                                 #PRACTICA 2





#Ejercicio 1
""" Crea una función que devuelva un vector con las medidas numéricas más importantes para una variable cuantitativa. Introduce un argumento que permita mostrar o no, el resumen por pantalla. Aplícalo a la variable P1 de Pulso."""

resumen<- function(vari,decision)
{
  if(decision==1)  return(c(summary(vari)))
}
resumen(Pulso$P1,0)
help("dpois")
#Ejercicio 2
""" Calcula el percentil 95 de la variable P2 para los chicos y las chicas por separado, utilizando una llamada a una función de tipo apply."""
tapply(Pulso$P2, INDEX=as.factor(Pulso$S),FUN=function(x) quantile(x,p=0.95))

#Ejercicio 3
"""  Crea una función que compruebe si una matriz es cuadrada y si lo es, devuelva la matriz traspuesta (pero sin utilizar la función t)."""
matA<-rbind(c(1,2,6),c(3,4,5),c(2,3,4))
cuadrada<- function(mat)
{
  if (ncol(mat)==nrow(mat)) 
  {
    n=ncol(mat)
    print("La matriz es cuadrada")
    maT <- matrix(0, ncol(mat), nrow(mat))
    for (i in 1:n)
    {
      for (j in 1:n)
      {
        maT[i][j]=mat[j][i]
      }
    }
    
    return(maT)
  }
  
  else print("La matriz no es cuadrada")
  
}
matA
cuadrada(matA)  


#Ejercicio 4
""" Realiza un análisis descriptivo de la variable Altura del fichero Pulso; transfórmala primero a centímetros, ya que está medida en pulgadas.

Realiza un análisis descriptivo de la variable Peso del fichero Pulso; transfórmala primero a kg, ya que está medida en libras."""
load("Pulso.RData")
Pulso$Altcm<-Pulso$Alt*2.54
Pulso$Altcm
summary(Pulso$Altcm)
Pulso$kg<-Pulso$Peso*0.453592
Pulso$kg
summary(Pulso$kg)

#Ej5
"""Realiza un análisis descriptivo de la relación existente entre las variables P1 y P2.

Realiza un análisis descriptivo de la relación existente entre las variables C (que indica los que han corrido) y el pulso P2.

Realiza un análisis descriptivo de la relación existente entre el factor sexo FS y el factor FF, que indica si el estudiante fuma o no. Haz los gráficos usando tanto la función Barplot como la función barplot. """
cor(Pulso$P1,Pulso$P2)
lm1<-lm(Pulso$P1~Pulso$P2)
summary(lm1)

Pulso$cc<-factor(Pulso$C,labels=c("Dos veces", "Una vez"))
par(mfrow=c(1,1))
Hist(Pulso$P2,Pulso$cc)
Boxplot(Pulso$P2,Pulso$cc)

Pulso$FF<- factor(Pulso$F, labels = c("Fumador", "NoFumador"))
Tabla <- table(Pulso$FF,Pulso$SF)
addmargins(Tabla)

#Usando la función tapply y un factor, calcula la suma de los números impares y pares entre 1 y 100.
n<-seq(1:100)
factor_numeros <- factor(n %% 2, labels=c("Par", "Impar"))
factor_numeros
suma_numeros <- tapply(n, factor_numeros, sum)
suma_numeros

#Ejercicio

Pulso$cm<-Pulso$Alt*2.54
hist(Pulso$cm)

#Calcula una función cuyo argumento sea una matriz de dimensión X
#y que calcule la suma de los elementos de cada fila de la matriz y devuelva el vector resultante como objeto de salida. Aplícalo a una matriz de 4×4
#en la que la primera fila sea todo unos, la segunda todo doses….
suma_por_fila <- function(mat) {
  n <- nrow(mat)
  resultado <- numeric(n)
  for (i in 1:n) {
    resultado[i] <- sum(mat[i, ]) 
  }
  return(resultado) 
}
M <- matrix(1:4,nrow=4, ncol = 4)
resultados <- suma_por_fila(M)
M
resultados

#EJERCICIO 3.7 IMPORTANTE
"""En este ejercicio usaremos la base de datos NHANES, que contiene información médica, económica y social de 10000 individuos. Para cargarla, escribe library(NHANES) y tendrás disponible el data.frame NHANES con todos los datos. Si quieres información de las variables, escribe help(NHANES).

Genera un nuevo dataframe con las siguientes características:
Los individuos (filas) son los individuos (filas) de NHANES que no tienen valor NA ni en Height ni en Weight y en Age tienen entre 20 y 79 años (ambos inclusive).
Las variables (columnas) son Age, Height, Weight, Gender y una nueva variable (factor) con el nombre GEdad que tenga niveles “Joven”, “Adulto” y “Mayor”, construida a partir de Age (Joven=[20,30), Adulto=[30,65), Mayor=[65-79]). En la variable Gender cambia el nombre de los niveles de “female” y “male” a “mujer” y “hombre”.
En los apartados b, c, d y e, trabaja con este nuevo data.frame.

Realiza un boxplot de la variable Height separado por Gender.

Crea una tabla de doble entrada con el número de individuos en cada cruce de GEdad x Gender, y otra tabla con los porcentajes de Gender en cada nivel de GEdad. Dibuja un gráfico de barras en el que queden representados estos porcentajes.

Dibuja un gráfico de dispersión (scatterplot) de las variables Height y Weight y añade la recta de regressión de Weight sobre Height.

Dibuja un gráfico de dispersión (scatterplot) de las variables Height y Weight representando a los hombres en color azul y a las mujeres en color verde, junto con las correspondientes rectas de regresión. Añade una leyenda explicativa.

El índice de pobreza de una población se define como el porcentaje de la población cuya renta se encuentra por debajo del 60% de la mediana de la renta de esa población. Escribe una función en R cuyo argumento sea un vector “Ingresos” y un factor “Grupos” y devuelva el índice de pobreza en cada uno de esos grupos. Ten en cuenta que tanto el vector Ingresos como el factor Grupos pueden tener valores NA, por lo que la función deberá eliminar esos casos antes de calcular el índice de pobreza. Aplica la función a las variables HHIncomeMid y Race3 de NHANES.

 """
library(NHANES)
nuevoNHANES<-NHANES[!is.na(NHANES$Height & NHANES$Weight) & NHANES$Age>=20 & NHANES$Age<=79,]
nuevoNHANES$GEdad<-cut(nuevoNHANES$Age,breaks=c(20,30,65,79),labels=c("Joven","Adulto","Mayor"),include.lowst=TRUE)
levels(nuevoNHANES$Gender)<-c("mujer","hombre")
dat<-subset(nuevoNHANES,select=c("Age","Height","Weight","Gender","GEdad"))
Boxplot(dat$Height,dat$Gender)
Tabla<-table(dat$Gender,dat$GEdad)
Tabla
prop.table(Tabla,margin=2)*100
Barplot(dat$Gender,by=dat$GEdad)
help(scatterplot)
scatterplot(dat$Height,dat$Weight)
lm1<-lm(dat$Weight~dat$Height)
abline(a=lm1$coefficients[1],b=lm1$coefficients[2],col="black")
scatterplot(dat$Height,dat$Weight,groups=factor(dat$Gender),col=c("green","blue"))
tapply(dat,INDEX=as.factor(dat$Gender),function(x) lm(dat$Weight~dat$Height))
help(tapply)

#3.6
""" Define una función con dos argumentos, X
 (un vector numérico) y F
 (un factor) que calcule:

El número de observaciones en cada nivel del factor F
 (se puede suponer que ni X
 ni F
 tienen valores NA).

La media de la variable X
 en cada nivel del factor F
.

La posición de cada nivel en cuanto a la diferencia de la media de la variable X
 en el nivel y la media global de X
 (es decir, un 1 para el nivel con la media más cercana a la global, un 2 para el nivel con la siguiente media más cercana…
).

El número de observaciones en cada nivel cuyo valor de X
 sea mayor que la media global.

La función devolverá una matriz con cuatro filas y tantas columnas como el número de niveles de F
. Las filas serán los resultados de los cálculos anteriores: Fila 1 =
 número de observaciones de X
 en cada nivel; Fila 2 =
 media de la variable X
 en cada nivel; Fila 3 =
 posición de cercanía de la media en el nivel a la media global; Fila 4 =
 número de observaciones en cada nivel con valor mayor que la media global.

Además, la función hará un gráfico de caja (boxplot) de la variable X
 por niveles de F
, y se añadirá al gráfico una línea horizontal indicando la media global de la variable X
.

Muestra el resultado de la función aplicada a las variables Peso (X) y Grupo_Edad (F) del data.frame Datos, que está en el fichero Datos.RData.

Define una función con dos argumentos, X
 (un vector numérico) y F
 (un factor) que devuelva la media de la variable X
 en cada nivel del factor F
, teniendo en cuenta que en X
 puede haber valores NA. La función debe calcular las medias sin tener en cuenta esos valores NA. Muestra el resultado de la función aplicada a las variables Altura (X) y Grupo_Edad (F) del data.frame Datos. """
load("Datos.RData")
ordena<-function(x,fila2)
{
  f1<-fila2-mean(x)
  sort(f1)
  
}

f1<-tapply(Datos$Peso,Datos$Grupo_Edad,mean)-mean(Datos$Peso)
hola<-sort(abs(f1))
for (i in 1:3)
{
  hola[i]<-i
}
hola


funcion<-function(x,f)
{
  fila1<-table(f)
  fila2<-tapply(x,f,mean)
  fila3<-hola
  
  
  matriz<-rbind(fila1,fila2,fila3)
  return(matriz)
}  
funcion(Datos$Peso,Datos$Grupo_Edad)  
mean(Datos$Peso)
x1<-Datos$Peso[Datos$Peso>mean(x)]







                              #PRACTICA 3








#Aproximar binomial con poisson
x<-seq(0,25,length.out=1000)
plot(x,ppois(x,1.8),type="l")
lines(x,pbinom(x,90,0.02),type="l",col="red")

#3.1
""" Representa gráficamente (en el mismo gráfico) la función de densidad de una Normal(1,1) y las probabilidades de una Poisson(1). Escoge primero un rango que sea adecuado para representar ambas distribuciones. Realiza un gráfico análogo pero ahora con una Normal(50,50−−√
) y una Poisson (50). Crees que una Poisson se puede aproximar por una Normal? ¿En qué condiciones?"""
help(ppois)
x<-seq(-2,4,length.out=1000)
plot(x,dnorm(x,1,1),type="l")
lines(-2:4,dpois(-2:4,1),type="l",col="red")

x<-seq(30,70,length.out=1000)
plot(x,dnorm(x,50,sqrt(50)),type="l")
lines(30:70,dpois(30:70,50),type="l",col="red")

#3.2
""" Comprueba mediante simulación que la distribución χ2n
 se define como la suma de n
 variables N(0,1)
 al cuadrado, que sean independientes. Para ello, genera muestras de variables N(0,1)
 independientes, y a partir de ellas una muestra de una variable χ2n
, estima a partir de esa muestra la función de densidad, y compara la estimación con la función de densidad de una χ2n
. Calcula la media y la desviación típica de la muestra y compara los valores obtenidos con la media y la desviación típica de una χ2n
. Utiliza por ejemplo, n=4
.

Realiza el mismo ejercicio para una variable t4
."""
n<-4
N<-5000
x<-seq(-2,20,length.out=1000)
plot(x,dchisq(x,4),type="l")
x1<-rnorm(N,0,1)
x2<-rnorm(N,0,1)
x3<-rnorm(N,0,1)
x4<-rnorm(N,0,1)
chi<-x1^2+x2^2+x3^2+x4^2
lines(density(chi),col="red")
lines(density(tetas),col="blue")

x<-seq(-5,5,length.out=1000)
Z<-rnorm(N,0,1)
Y<-rchisq(N,n)
tetas<-Z/sqrt(Y/n)
dx<-density(tetas)
plot(dx,col="red")
lines(x,dt(x,4),col="blue")

#3.3
""" Se pueden generar muestras aleatorias de una distribución de probabilidad cualquiera si se dispone de la función cuantil correspondiente (es decir, la inversa de la función de distribución acumulada) y de un generador de muestras aleatorias con Distribución U(0,1)
. Diseña un algoritmo que permita esta generación, por ejemplo para generar muestras con distribución exponencial."""
generomuestras <- function(n, lambda) {
  p <- runif(n,0,1)
  muestras <- -1/lambda * log(1 - p)
  return(muestras)
}
n <- 10000
lambda <- 0.5
muestrasexp <- generomuestras(n, lambda)
hist(muestrasexp, breaks = 30, freq = FALSE)

#3.4
"""Usando el dataframe Pulso.RData, estima la distribución muestral de la variable P1 a partir de la función de distribución empírica y dibújala. Calcula la media y varianza muestrales de P1. Dibuja en el mismo gráfico la función de distribución empírica de una Poisson con parámetro igual a la media y de una normal con la media y varianzas anteriores. """
load("Pulso.RData")
empF<-function(x)
{
  n<-length(x)
  x<-sort(x)#con esto los ordeno
  return(data.frame(x, fdex=rank(x, ties.method='max')/n))#la funcion rank le 
  #da a cada numero su posicion de menor a mayor. Si ya estan ordenadors de 
  #menor a mayor deberían dar los valores 1 2 3... SI hay empate, dos valores
  #iguales, nos tiene que dar el rango valor. lo hacemso con el ties.method
}
n<-200
x<-Pulso$P1

Fx<-empF(x)
media<-mean(Fx[,1])
desv<-sd(Fx[,1])
plot(Fx$x, Fx$fdex, type='s')#type s de step, escalon, para que sea como la
#funcion empirica
lines(Fx$x, ppois(Fx$x,media), col='red')#dibujamos la funcion normal para ver que la 
lines(Fx$x, pnorm(Fx$x,media,(desv)), col='blue')#dibujamos la funcion normal para ver que la 

.







                               #PRÁCTICA 4









#Ejercicio 4.3
""" Realiza una función que permita realizar todos los pasos de la simulación del TCL descrita en la sección 4, y donde n
, el número de variables que interviene en X¯n
 y el tamaño de muestra generado m
, sean argumentos de la función. Repite el ejercicio de simulación utilizando distintos valores de n=5,25,200
. ¿En qué casos la aproximación Normal de X¯n
 será mejor?"""
n<-100
m<-1000
x1<-rexp(n, 0.1)
y=seq(-3,3,0.01)
plot(y,dnorm(y,0,1),type='l')


aprox =  function( n,x1)
{
  MatExp=matrix(rexp(n*m,rate=0.1),nrow=m)
  xbarra<-rowMeans(MatExp)
  res<-(xbarra-10)/(10/sqrt(n))
  lines(density(res),col="red")
  return(1)
}
sapply(c(3,20,500),MatExp,FUN=aprox )


#Ej4.1. 
""" El problema de Monty Hall. Monty Hall es el nombre de un famoso problema de probabilidad que se basa en el concurso televisivo “Let’s Make a Deal” de la televisión de EE.UU. (1963-1986) presentado por Monty Hall. El problema se plantea de la siguiente manera:
Un concursante se encuentra frente a tres puertas. Detrás de dos de ellas hay cabras, y detrás de la tercera hay un coche. El concursante elige una de las tres puertas, pero no se descubre lo que hay detrás. Después de que el concursante ha elegido, el presentador (Monty Hall) abre una de las dos puertas que el concursante no eligió y en la que hay una cabra. Una vez ha abierto la puerta, el presentador ofrece al concursante la posibilidad de cambiar su elección a la puerta que no había seleccionado. El problema consiste en si el concursante debería cambiar de puerta de cara a maximizar la probabilidad de llevarse el coche."""
n=1000
p<-1/3
Monty<-function(n,p)
{
  #JUEGO 1: SI ELIGE NO CAMBIAR LA PROB ES 1/3.
  x<-rbinom(n,1,p)
  y<-x[x==1]
  length(y)
  
  #JUEGO 2: SI ELIGE CAMBIAR
  x2<-rbinom(n,1,p)
  y2<-x2
  for(i in 1:length(x2))
  {
    if(x2[i]==1)
    {y2[i]=0}
    else{y2[i]=1}
  }
  yF<-y2[y2==1]
  length(yF)
  
  cat("Aciertos en los 1000 juegos del primer tipo: ", length(y), "; Aciertos en los 1000 juegos del segundo tipo: ", length(yF))
}   
Monty(n,p)

#4.3
"""Realiza una función que permita realizar todos los pasos de la simulación del TCL descrita en la sección 4, y donde n
, el número de variables que interviene en X¯n
 y el tamaño de muestra generado m
, sean argumentos de la función. Repite el ejercicio de simulación utilizando distintos valores de n=5,25,200
. ¿En qué casos la aproximación Normal de X¯n
 será mejor?

 """
landa=1/10
desv=landa
x<-seq(-5,5,length.out=10000)
aproxnormal<-function(n,m)
{
  datos<-matrix(rexp(n*m,landa),nrow=n)
  Xns<-colMeans(datos)
  simplifico<-(Xns-1/landa)*sqrt(n)*landa
  plot(x,dnorm(x,0,1),type="l")
  lines(density(simplifico),col="red")
  
  
}

aproxnormal(100,500)

#4.4
""" Realiza una simulación del TCL pero utilizando ahora una distribución Uniforme(0,1)
. ¿Cuando Xi∼Uniforme(0,1)
, son necesarios los mismos valores de n
 para obtener una buena aproximación Normal de X¯n
? ¿Por qué?"""
media=0.5
x<-seq(-5,5,length.out=1000)
aproxnormal<-function(n,m)
{
  datos<-matrix(runif(n*m,0,1),nrow=n)
  Xns<-colMeans(datos)
  simplifico<-(Xns-media)*sqrt(n)*sqrt(12)
  plot(x,dnorm(x,0,1),type="l")
  lines(density(simplifico),col="red")
  
  
}

aproxnormal(1000,500)

#4.5
"""Comprueba la convergencia en distribución de X¯n−μSn/n√
, para varios casos de la distribución de las variables Xi
 (por ejemplo, uniforme, exponencial, Poisson). """
#PARA LA EXP
landa=1/10
desv=landa
x<-seq(-5,5,length.out=10000)
aproxnormal<-function(n,m)
{
  datos<-matrix(rexp(n*m,landa),nrow=n)
  Xns<-colMeans(datos)
  Sn<-apply(datos,2,sum)
  simplifico<-(Xns-1/landa)*sqrt(n)/Sn
  plot(x,dnorm(x,0,1),type="l")
  lines(density(simplifico),col="red")
  
  
}

aproxnormal(1000,50)

#PARA LA UNIF

media=0.5
x<-seq(-5,5,length.out=1000)
aproxnormal<-function(n,m)
{
  datos<-matrix(runif(n*m,0,1),nrow=n)
  Xns<-colMeans(datos)
  Sn<-apply(datos,2,sum)
  simplifico<-(Xns-media)*sqrt(n)/Sn
  plot(x,dnorm(x,0,1),type="l")
  lines(density(simplifico),col="red")
  
  
}

aproxnormal(1000,500)

#4.6
""" Comprueba por simulación que S2n⟶c.s.σ2
, para varios casos de la distribución de las variables Xi
 (por ejemplo, uniforme, exponencial, Poisson). ¿Puedes encontrar algún ejemplo de distribución para la que no haya convergencia?"""
genmuestra<-function(n)
{
  x<-rnorm(n,0,1)
  Smu<-(sum((x-0)**2))/n
  return(Smu)
}
genmuestra(10000)
rango<-seq(10,10000,by=10)
desviaciones<-sapply(rango,FUN= genmuestra)
plot(rango,desviaciones,type="l")
abline(h=1,col="red")


#4.7









                               #PRACTICA 5







"""1. Comprueba con un método de Monte Carlo, si los estimadores σ^21 y σ^22de σ2, la varianza 
de una variable son insesgados. Utiliza de nuevo una distribución U(0,20)Comprueba la propiedad
utilizando primero un valor del tamaño de muestra n pequeño y luego un valor nalto."""
n<-100
m<-10000
x<-runif(n*m,0,20)
matsim<-matrix(x,nrow=n)
est1=apply(matsim,2,function(x) (1/(length(x)-1)*(sum((x-mean(x))**2))))
est2=apply(matsim,2,function(x) (1/(length(x))*(sum((x-mean(x))**2))))

sesgo1=(mean(est1)-20**2/12)^2
sesgo2=(mean(est2)-20**2/12)^2

"""2. Utiliza un procedimiento de bootstrap paramétrico para estimar la varianza de los estimadores
definidos en la Sección 1."""
sdR=((20^2)/12)**0.5
desvT<-apply(matsim,2,sd)
sdP=mean(desvT)
sd1=sd(est1)
sd2=sd(est2)
#La varianza será 3 en los dos porque sale 3.001 y 2.98 o así


"""3.Los MLE tienen muy buenas propiedades, en particular son asintóticamente insesgados, 
y su varianza asintótica es la inversa de la matriz de información de Fisher dividida por
n. Comprueba estas propiedades utilizando una muestra simulada con una distribución Exp(λ)
 con λ=2 . Suponiendo que sabes que la muestra “observada” tiene una distribución exponencial, 
estima la esperanza y la varianza del MLE utilizando un bootstrap paramétrico. """

n<-2000
x<-rexp(n,2)
B<-10000
media<-mean(x)
de<-sd(x)


mx<-matrix(rexp(n*B,1/media),nrow=n)
bootPmedia<-apply(mx,2,mean)
bootPsd<-apply(mx,2,sd)
mean(bootPmedia)
mean(bootPsd)

"""4.Considera de nuevo el problema planteado en la Sección 3.2 sobre la estimación del 
parámetro p. Crea una función que calcule la función de verosimilitud (negativa) y calcula
el MLE de p utilizando esa función, en lugar de la función de logverosimilitud. Compara
el estimador con el obtenido en la Sección 3.2. ¿Por qué crees que es preferible utilizar 
la función de logverosimilitud en lugar de la función de verosimilitud? """
p<-1/6
N<-10
m<-10
x<-rbinom(m,N,p)
pp<-seq(0.001,1,by=0.01)
llik=function(p,x,N)
{
  return(prod(dbinom(x,size=N,prob=p,log=FALSE)))
}
LBp=sapply(pp,FUN=llik,x=x,N=N)
hatp<-pp[which(LBp==max(LBp))]
hatp
plot(pp,LBp,type="l",xlab="p",ylab="verosimilitud")
abline(v=hatp,col="red")
"""Porque con la similutud multiplicas y entonces para valroes muy proximos al cero, al final
se te acabo yendo todo al 0. Ademas la formula tiene numeros combinatorios que tienen
factoriales, lo cual hace que los nuemros sean muy garndes y haya muchos errores"""

#CON LA OTRA FORMA 
mlogLBin<-function(p, x, N){
  mlogLB<-prod(dbinom(x, size=N, prob=p, log=FALSE))
  return(-mlogLB)  }
est1<-nlminb(objective=mlogLBin,  start=c(p=0.5), x=x, N=N, lower=0, upper=1)
# el primer parámetro de la función tiene que ser sobre el que se va a minimizar
# el argumento start permite especificar un valor inicial de los parámetros a estimar
# los argumentos lower y upper indican el rango de valores posibles  del parámetro

#elementos del objeto salida est1
est1$convergence  # código de convergencia, que es 0 si se ha alcanzado
est1$par  #MLE
est1$objective  #-logL
LBp
#DE ESTA FORMA NO SALE BIEN PORQUE SE VA AL CERO Y ENTONCES NO FUNCIONA

"""5.  El conjunto de datos BootData.RData contiene una muestra de tamaño n=150 de tres 
variables X1, X2 y X3, pero en este ejercicio sólo utilizaremos la primera. A partir de
esa muestra, utiliza bootstrap no paramétrico para estimar la esperanza, la varianza y 
la función de densidad del estadístico () Estima su función de densidad.Utiliza B=1000. """

load("BootData.RData")
n=length(x1)
B<-1000
mean(x1)
EsT<-function(x1)
{return((sum(log(abs(x1))))/sum(sqrt(abs(x1))))}
mx3=matrix(sample(x1,n*B,replace=TRUE),nrow=n)
aplicoEstimador<-apply(mx3,2,FUN=EsT)
mean(aplicoEstimador)
sd(aplicoEstimador)
den<-density(aplicoEstimador)
plot(den,col="blue")

"""6. La media armónica (para variables positivas) se define como: Genera una muestra de tamaño 
n=1000 con una distribución Gamma(a,p); por ejemplo, con parámetros a=1/2 y p=3.Implementa un 
bootstrap no parámetrico para estimar E(T), Var(T) y su función de densidad con B=2000."""
n=1000
B<-2000
x<-rgamma(n,3,0.5)
EsT<-function(x)
{return(n/(sum(1/x)))}
mx3=matrix(sample(x,n*B,replace=TRUE),nrow=n)
aplicoEstimador<-apply(mx3,2,FUN=EsT)
mean(aplicoEstimador)
sd(aplicoEstimador)
den<-density(aplicoEstimador)
plot(den,col="blue")


#EJERCICIOS IMPORTANTES:

"""7. Dada una muestra (X1,…,Xn) de una población con E(X)=μ, consideramos el estadístico S2μ=1n∑ni=1(Xi−μ)2
.a.- Si la distribución de las variables X es N(3,5) y el tamaño de la muestra es igual a 4, comprueba usando simulación que 4S2μ/25
 tiene distribución χ2. ¿De cuántos grados de libertad? (Obtén 500 valores del estadístico, estima con ellos la función de densidad del mismo y compárala gráficamente con la de la χ2
).b.- Si la distribución de las variables X es Exp(1/3), comprueba usando simulación que S2μ
, con μ=3, converge casi seguramente. ¿A qué valor? (Usa 50 trayectorias del estadístico cuando la muestra tiene n=1,2,…,1000
 elementos).c.- Si en el apartado (b) la distribución de las variables X es t
 de Student con 2 grados de libertad, comprueba usando simulación si hay convergencia casi segura de S2μ
 con μ=0. ¿Qué está ocurriendo en este caso?"""

#4.7.a
n<-4
B<-500
x<-rnorm(n*B,3,5)
media<-mean(x)
matriz<-matrix(x,nrow=n)
Smu<-function(x,n)
{return((sum((x-media)**2))/length(x))}
est<-apply(matriz,2,Smu,n=n)
dens<-density(4*est/25)
plot(dens)
lines(density(rchisq(B,n)),col="red")
lines(seq(0,10, by = 0.01), dchisq(seq(0,10, by = 0.01), n),col="green")

#4.7.b
m = 1000
r = 1/4
#dibujar un plot del 0 al 1000 en la x y del 0 al 40 en la y
plot(1:m,type = "n", xlim = c(0,m) ,ylim = c(0, 40))


estadistico = function( n, r){
  data = rexp(n, rate = r)
  return(Smu(data, 1/r))
}

simular = function(aux, m, rate){
  lines(1:m,sapply(1:m, FUN = estadistico, rate),type = "l",col="blue")
}

v = sapply(X = 1:50, FUN = simular, m = m, rate = r)
lines(1:m, rep(1/r^2, m), col = "red")

#4.8
""" En el archivo Zeolitas.RData se encuentra el data.frame zeolitas que tiene datos de 299 zeolitas (un tipo de roca). Entre otras variables, está la variable Si, que indica el porcentaje de Silicio de la roca. Un proceso industrial en el que se utilizan zeolitas requiere que el contenido en Si
 sea menor que 80. Se sabe que el contenido de Si de las rocas que cumplen este requisito tienen una distribución lognormal de parámetros μ
 (desconocido) y σ=0.075
. La distribución lognormal es aquella tal que su logaritmo es normal, es decir, si X
 tiene distribución N(μ,σ)
, entonces Y=eX
 tiene distribución lognormal de parámetros μ
 y σ
. Su función de densidad es
fY(y)=1yσ2π−−√e−(log(y)−μ)22σ2,y>0
y su esperanza es eμ+σ22
.

a.- Utilizando la muestra correspondiente a esa población (contenido en Si
 menor que 80), obtén el estimador del parámetro μ
 de la variable utilizando el método de los momentos, μ^m
, y el método máximo versímil, μ^MLE
.

b.- Estima con un procedimiento bootstrap paramétrico y con otro no paramético la varianza de μ^m
.

c.- Suponiendo ahora que σ
 es también desconocido, calcula el estimador máximo verosímil de los parámetros (μ,σ)
 usando una función de optimización de R."""

#a
sigma = 0.075

load("Zeolitas.RData")
x = zeolitas$Si[zeolitas$Si < 80]

#Metodo de momentos
u_momentos = log(mean(x)) - sigma^2/2

#Metodo de maxima verosimilitud
log_densidad = function(mu, x, sigma){
  return (-(log(x)  - mu)^2/(2*sigma^2)-log(x*sigma*sqrt(2*pi)))
}
densidad = function(mu, x, sigma){
  return (exp(log_densidad(mu, x, sigma)))
}

mlogLBin<-function(mu, x){
  mlogLB<-sum(log_densidad(mu, x, sigma))
  return(-mlogLB)  }

est1<-nlminb(objective=mlogLBin,  start=c(mu = 5), x=x,  lower=2, upper=8)
est1$par

#b

n = length(x)
B = 1000
data = exp(matrix(rnorm(n*B, u_momentos, sigma), nrow = n))
data2 = matrix (sample(data, n*B, replace = TRUE), nrow = n)
bootstrap=apply(data, 2, FUN = function(x) log(mean(x) - sigma**2/2))
bootstrap2 = apply(data2, 2, FUN = function(x) log(mean(x) - sigma**2/2))
var(bootstrap)
var(bootstrap2)

#c

log_densidad = function(p ,x ){
  return (-(log(x)  - p[1])^2/(2*p[2]^2)-log(x*p[2]*sqrt(2*pi)))
}

mlogLBin<-function(parametros, x ){
  mlogLB<-sum(log_densidad(parametros, x ))
  return(-mlogLB)  }

est1<-nlminb(objective=mlogLBin,  start=c(mu = 5, s = 0.5), x=x,  lower=c(0,0.000001), upper=c(8, 1))
est1$par




              #PRACTICA 6



#3
#a
""" Una moneda tiene probabilidad de cara p
, que suponemos aleatoria, con distribución a priori Beta(2,2)
. Lanzamos la moneda n=20
 veces y obtenemos una proporción de caras del 40%.

a.- Comprueba que la distribución a posteriori de p
 también es Beta (¿de qué parámetros?). Calcula el estimador Bayes de p
 basado en una función de pérdida cuadrática. Calcula un intervalo de credibilidad para p
 con nivel 95%. Compáralos con el estimador máximo verosímil y el intervalo de confianza que se obtendrían si no usásemos estadística bayesiana. ¿Qué pasa cuando n
 se hace muy grande? ¿Cómo interpretas esto?

b.- Calcula (numéricamente) el estimador Bayes de p
 cuando la función de pérdida es e(p−p^)2
, es decir, calcula el valor de p^
 que minimiza E(e(p−p^)2)
 donde p
 es una variable aleatoria que sigue la distribución a posteriori."""

lambdax = seq(0, 1, by = 0.01)
priorlambda <- dbeta(lambdax, 2, 2)

llik<-function(x){
  dbinom(0.4*20, 20, x)
}

lliklambda <- llik(lambdax)

k<-sum(lliklambda*priorlambda*0.01)
postlambda_aprox<-lliklambda*priorlambda/k


plot(lambdax,postlambda_aprox, type='l', xlab=expression(lambda),ylab="posterior density", ylim = c(0, 4), xlim=c(0,1))
lines(lambdax, priorlambda, col='red')
lines(lambdax, dbeta(lambdax, 10,14), col = 'blue')

aproximacion = sum(lambdax*postlambda_aprox*0.01)
alpha = 0.025
acumulada=cumsum(postlambda_aprox*0.01)
cat(lambdax[max(which(acumulada<alpha/2))],lambdax[max(which(acumulada<1-alpha/2))],fill=TRUE)

#b
exponencial = function(p_gorro,p ){
  return (mean(exp((lambdax-p_gorro)^2)*postlambda_aprox))
}


p_g= seq(0,1, by = 0.01)
minimo = p_g[which.min(sapply(p_g, FUN = exponencial, p = postlambda_aprox))]
