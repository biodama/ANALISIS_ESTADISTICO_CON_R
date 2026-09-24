set.seed(1234)
n=1000 #tamaño muestral
fuma=sample(c(TRUE,FALSE),n,replace=TRUE) #la madre fuma o no 
gestacion=32+10*runif(n)
sigma=100
peso = 2000 + (gestacion-32)*(100*(fuma==TRUE) + 150*(fuma==FALSE)) + rnorm(n,0,sigma) 

simu=data.frame(fuma=fuma,peso=peso,gestacion=gestacion)

plot(peso~gestacion,data=simu,cex=.1)

plot(peso~gestacion,data=simu,cex=.1,col=fuma+1)

fit0=lm(peso~gestacion,data=simu)
par(mfrow=c(2,2))
plot(fit0,cex=.2)

fit1=lm(peso~gestacion*fuma,data=simu)
summary(fit1)
plot(fit1,cex=.2)
