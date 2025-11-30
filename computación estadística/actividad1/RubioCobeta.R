media<-0.5
x<-seq(-5,5,length.out=1000)
aproxnormal<-function(n,m)
{
  datos<-matrix(runif(n*m,0,1),nrow=n)
  Xns<-colMeans(datos)
  simplifico<-(Xns-media)*sqrt(n) *sqrt(12)
  plot(x, dnorm(x,0,1),type="l",main= paste("Aproximación con=",n))
  lines(density(simplifico),col="red")
  legend("topright",legend=c("NormalTeórica","SimulaciónUniforme"),
         col=c("black","red"),lty=1,cex=0.8,bty="n")
}
aproxnormal(10000,50090)
