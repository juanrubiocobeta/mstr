##########################################
# Definici�n del proceso Wiener standard #
# n-> Tama�o de la trayectoria           #
# tra-> N�mero de trayectorias           #
# h-> paso en la simulaci�n              #
##########################################

Wiener<-function(n,tra,h)
{
  Wiener<-array(0,c(n,tra))
  for(i in 1:tra) Wiener[,i]<-cumsum(c(0,rnorm(n-1,0,sqrt(h))))
  Wiener
}

#############################
# Simulamos 50 trayectorias #
# con 301 datos y h=0.1     # 
#############################

Win<-Wiener(301,50,.1)
win.graph()
matplot(Win,main="Trayectorias simuladas de un proceso Wiener Standard",lty=1,lwd=1,type="l",xlab="Tiempo",ylab="Trayectorias")
media<-rep(0,301)
for(i in 1:301) media[i]<-mean(Wiener[i,])
lines(media, main="Media del proceso Wiener simulado",lty=1,lwd=3,type="l")

###############################################
# Definici�n del proceso Wiener con tendencia #
# n-> Tama�o de la trayectoria                #
# tra-> N�mero de trayectorias                #
# h-> paso en la simulaci�n                   #
###############################################

Wiener2<-function(n,tra,h,t0,x0,mu,sigma)
{
  W<-Wiener(n,tra,h)
  Tiempo<-seq(t0,t0+(n-1)*h,h)
  Wiener2<-x0+mu*(Tiempo-t0)+sigma*W  
}

Tiempo<-seq(10,10+(301-1)*.1,.1)
Win<-Wiener2(301,50,.1,10,5,1,1)
win.graph()
matplot(Tiempo,Win,main="Trayectorias simuladas de un proceso Wiener con tendencia",lty=1,lwd=1,type="l",xlab="Tiempo",ylab="Trayectorias")

media<-rep(0,301)
for(i in 1:301) media[i]<-mean(Win[i,])
lines(Tiempo,media, main="Media del proceso Wiener simulado",lty=1,lwd=3,type="l")

#####################################################################
#                                                                   #
# Funci�n que genera trayectorias del proceso lognormal homog�neo   #
# Ini: Dist. Inicial (0: degenerada en x0; 1: lognormal(mo,sigma0)) #
# n: Tama�o de la trayectoria                                       #
# tra: n�mero de trayectorias                                       #
# h: paso entre dos instantes sucesivos                             # 
# t0: instante inicial                                              #
# m, sigma: par�metros de los momentos infinitesimales              #
#                                                                   #
#####################################################################

LognorHomo<-function(Ini,x0,n,tra,h,t0,m,sigma,m0,sigma0)
{
  Win<-Wiener(n,tra,h)
  if (Ini==0) Inicial<-rep(x0,tra) else Inicial<-rlnorm(tra,m0,sigma0)
  LognorHomo<-array(0,c(n,tra))
  Tiempo<-seq(t0,t0+(n-1)*h,h)
  LognorHomo<-t(Inicial*t(exp((m-sigma^2/2)*(Tiempo-t0)+sigma*Win)))
  LognorHomo
}

Tiempo<-seq(10,10+(301-1)*.1,.1)
Lognor1<-LognorHomo(1,0,301,50,.1,10,.1,.05,1,.25)
win.graph()
matplot(Tiempo,Lognor1,main="Trayectorias simuladas de un proceso Lognormal Homog�neo",lty=1,lwd=1,type="l",xlab="Tiempo",ylab="Trayectorias")

media<-rep(0,301)
for(i in 1:301) media[i]<-mean(Lognor1[i,])
lines(Tiempo,media, main="Media del proceso Lognormal Homog�neo simulado",lty=1,lwd=3,type="l")


#####################################################################
# Funci�n que genera trayectorias del proceso Richards              #
# Ini: Dist. Inicial (0: degenerada en x0; 1: lognormal(mo,sigma0)) #
# n: Tama�o de la trayectoria                                       #
# tra: n�mero de trayectorias                                       #
# h: paso entre dos instantes sucesivos                             # 
# t0: instante inicial                                              #
# q, kappa, eta, sigma: par�metros de los momentos infinitesimales  #
#####################################################################

####################################
#  Definici�n de la curva Richards #
####################################

Richards<-function(t0,t,eta,kappa,q)((eta+kappa^t0)/(eta+kappa^t))^q

GeneraRichards<-function(Ini,x0,n,tra,h,t0,eta,kappa,q,sigma,m0,sigma0)
{    
  Win<-Wiener(n,tra,h)   
  if (Ini==0) Inicial<-rep(x0,tra) else Inicial<-rlnorm(tra,m0,sigma0)   
  GeneraRichards<-array(0,c(n,tra))
  Tiempo<-seq(t0,t0+(n-1)*h,h)
  GeneraRichards<-t(Inicial*t(Richards(t0,Tiempo,eta,kappa,q)*exp(sigma*Win-(sigma^2/2)*(Tiempo-t0)))) 
  GeneraRichards
}

Tiempo<-seq(10,10+(501-1)*.1,.1)
Richards1<-GeneraRichards(1,0,501,50,.1,10,.05,.8,1.25,0.01,1,.1)
win.graph()
matplot(Tiempo,Richards1,main="Trayectorias simuladas de un proceso Richards",lty=1,lwd=1,type="l",xlab="Tiempo",ylab="Trayectorias")

media<-rep(0,501)
for(i in 1:501) media[i]<-mean(Richards1[i,])
lines(Tiempo,media, main="Media del proceso Richards simulado",lty=1,lwd=3,type="l")



############################################################
#                                                          #
# Funci�n que genera trayectorias del proceso de Rayleigh  #
# Distribuci�n Inicial Degenerada en x0                    #
# n: Tama�o de la trayectoria                              #
# tra: n�mero de trayectorias                              #
# h: paso entre dos instantes sucesivos                    # 
# a, b, sigma: par�metros de los momentos infinitesimales  #
#                                                          #
############################################################



GeneraRayleigh<-function(x0,n,tra,h,a,b,sigma)
{
  GeneraRayleigh<-array(0,c(n,tra))
  GeneraRayleigh[1,]<-rep(x0,tra)
  for(i in 1:tra) 
  {
    U1<-rnorm(n-1,0,1)
    U2<-rnorm(n-1,0,1)
    V1<-U1*sqrt(h)
    V2<-(h^(3/2)/2)*(U1+U2/sqrt(3))
    V3<-rnorm(n-1,0,h^2/(2*sqrt(3))) 
    for(j in 1:(n-1))
    {
      z<-GeneraRayleigh[j,i]
      z1<-h*(a/z+b*z)+sigma*V1[j]+(h^2)*(b^2*z-(a^2/z^3))
      z2<-sigma*V2[j]*(b-a/z^2)+(a*sigma^2/z^3)*(V1[j]*V2[j]-V3[j])
      GeneraRayleigh[j+1,i]<-z+z1+z2
    }
  }
  GeneraRayleigh
}
####################################################### 
#                                                     # 
# Generaci�n de trayectorias del proceso de Rayleigh  #
# Distribuci�n inicial degenerada en x0=15            #
# n=501: Tama�o de cada trayectoria                   #
# tra=25: n�mero de trayectorias                      #
# h=.1: paso entre dos instantes sucesivos            #
# a=2                                                 # 
# b=-.1                                               #
# sigma=.8                                            # 
#                                                     #
#######################################################

Tiempo<-seq(10,10+(501-1)*.1,.1)
Rayleigh1<-GeneraRayleigh(15,501,25,.1,2,-.1,.4)
win.graph()
matplot(Tiempo,Rayleigh1,main="Trayectorias simuladas de un proceso de Rayleigh",lty=1,lwd=1,type="l",xlab="Tiempo",ylab="Trayectorias")

media<-rep(0,501)
for(i in 1:501) media[i]<-mean(Rayleigh1[i,])
lines(Tiempo,media, main="Media del proceso de Rayleigh simulado",lty=1,lwd=3,type="l",xlab="Tiempo",ylab="Media")


