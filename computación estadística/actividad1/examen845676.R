load("Salud.RData")
Tabla<-table(Salud$Sexo,Salud$Gcol)
Tabla
prop.table(Tabla,margin=2)*100
Barplot(Salud$Sexo,by=Salud$Gcol)

for(i in 1:200)
{
  if(Salud$Gcol[i]=="Bajo")
  {
  color[i]=c("blue")
  }
  else if(Salud$Gcol[i]=="Medio")
  {
  color[i]=c("green")
  }
  else
  {
  color[i]=c("red")
  }
}
plot(Salud$Peso,Salud$Triglic,xlab="peso",ylab="triglic",col=color)
Salud$Globulina
x<-Salud$Globulina[Salud$Globulina=="NA"]
length(x)
y<-Salud$Globulina[!is.na(Salud$Globulina)]
mean(y)
sd(y)
quantile(y,probs=0.95)

media<-mean(Salud$Colesterol)
desv<-sd(Salud$Colesterol)
media-desv
media+desv
Salud$Factor<-cut(Salud$Colesterol,breaks=c(0,77.57985,112.1091,100000),
                  labels=c("Bajo","Medio","Alto"),right=TRUE)
help(cut)

for (i in 1:200)
{
  if(Salud$Gcol[i]==Salud$Factor[i])
  {
  }
  else{
    cat(i,": NO\n\n")
  }
}


x<-seq(1,3,length.out=10000)
n<-500
m<-200
D<-sqrt(18/(7*n))
datos<-matrix(rbeta(n*m,2,4),nrow=n)
xnE<-apply(datos,2,mean)
Est<-4*xnE/(1-xnE)
plot(x,dnorm(x,2,D),type="l")
lines(density(Est),col="red")


load("Estbeta.RData")
n<-500
b<-4
mean(datos)
B<-100
mx1<-matrix(sample(datos,size=n*B,replace=TRUE),nrow=n)
estadistico<-function(x)
{
  return(4*mean(x)/(1-mean(x)))
}
bootmedia=apply(mx1,2,FUN=estadistico)
var(bootmedia)



lambdax = seq(0, 1, by = 0.01)
priorlambda <- dunif(lambdax,1,5)


N<-1000
datos<-matrix(runif(N*2,0,7),nrow=2)
menor<-apply(datos,2,min)
mayor<-apply(datos,2,max)
t1<-menor
t2<-mayor-menor
t3<-7-mayor
maslargo<-max(t1,t2,t3)
mascorto<-min(t1,t2,t3)
contador<-0
for(i in 1:N){ 
if(maslargo>2*mascorto)
{contador=contador+1}}
contador/N
mean(mascorto)
