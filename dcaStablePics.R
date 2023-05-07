###########################################
#RUN FIRST
###########################################
library(stats)
library(car)      
library(MASS)     
library(graphics)
library(openxlsx)
library(ks)
library(spatstat)
library(GoFKernel)
library(stable)
library(maxLik)
library(pracma)
library(StableEstim)
setwd("C:/Users/17753/Downloads")
x<-read.xlsx(xlsxFile = 'data7.xlsx', sheet = 1, skipEmptyRows = TRUE)

#builds inflation adjusted returns
par(mfrow=c(1,3))
L<-nrow(x)
d<-x[1:(L-1),c('Year','Dividend','Index','CPI','1YTR')]
d$Inflation<-x$CPI[2:L]/x$CPI[1:(L-1)]
s<-((x$Index[2:L]+x$Dividend[1:(L-1)])/x$Index[1:(L-1)])/d$Inflation
ls<-log(s) #all in stocks

#QQ plots of log-normal annual returns (all in stocks)
KPE<-KoutParametersEstim(ls,theta0=c(1.91,1,.115,mean(ls)),tol=.001,spacing='Kout')
th<-c(1.91,1,.115,.0658) #initial estimate used in KPE
dev.new()
par(mfrow=c(1,2))
plot(stable::qstable(seq(.01,.99,.01),loc=KPE$Estim$par[4],disp=KPE$Estim$par[3],skew=KPE$Estim$par[2],tail=KPE$Estim$par[1]),
     quantile(ls,seq(0.01,.99,.01)),
     type='l',xlab='S(1.89, 1.00, .110, .0658) quantiles',ylab='log-return quantiles',main='Stable vs log-returns QQ plot')
lines(seq(-1,1,.01),seq(-1,1,.01),lty=2)
plot(qnorm(seq(.01,.99,.01),mean=0.0658,sd=0.1690),
     quantile(ls,seq(0.01,.99,.01)),
     type='l',xlab=expression('N(.0658, .169'^2*') quantiles'),ylab='log-return quantiles',main='Normal vs log-returns QQ plot')
lines(seq(-1,1,.01),seq(-1,1,.01),lty=2)

#QQ plots of log-normal annual returns (not all in stocks)
#p in stocks, 1-p in inflation protected bonds
dev.new()
par(mfrow=c(1,2))
ls<-log(.6*s+.4)
mean(ls)
sd(ls)
KPE<-KoutParametersEstim(ls,theta0=c(1.91,1,.115,mean(ls)),tol=.001,spacing='Kout')
plot(stable::qstable(seq(.01,.99,.01),loc=KPE$Estim$par[4],disp=KPE$Estim$par[3],skew=KPE$Estim$par[2],tail=KPE$Estim$par[1]),
     quantile(ls,seq(0.01,.99,.01)),
     type='l',xlab='Stable quantiles',ylab='log-return quantiles',main='Stable vs log-returns QQ plot')
ls<-log(.4*s+.6)
mean(ls)
sd(ls)
KPE<-KoutParametersEstim(ls,theta0=c(1.91,1,.115,mean(ls)),tol=.001,spacing='Kout')
lines(stable::qstable(seq(.01,.99,.01),loc=KPE$Estim$par[4],disp=KPE$Estim$par[3],skew=KPE$Estim$par[2],tail=KPE$Estim$par[1]),
     quantile(ls,seq(0.01,.99,.01)),col='red')
lines(seq(-1,1,.01),seq(-1,1,.01),lty=2)
legend('topleft',c('p = .6','p = .4'),lty=c(1,1),col=c('black','red'))
ls<-log(.6*s+.4)
plot(qnorm(seq(.01,.99,.01),mean=mean(ls),sd=sd(ls)),
     quantile(ls,seq(0.01,.99,.01)),
     type='l',xlab=expression('Normal quantiles'),ylab='log-return quantiles',main='Normal vs log-returns QQ plot')
ls<-log(.4*s+.6)
lines(qnorm(seq(.01,.99,.01),mean=mean(ls),sd=sd(ls)),
     quantile(ls,seq(0.01,.99,.01)),col='red')
lines(seq(-1,1,.01),seq(-1,1,.01),lty=2)
legend('topleft',c('p = .6','p = .4'),lty=c(1,1),col=c('black','red'))


######################################################
#lump sum discount for continuous DCA
######################################################
f<-function(x,mu,alpha){
  return(((1-exp(-mu*x))/(1-exp(-mu)))^alpha)
}

MU<-seq(-1.0001,1,.01)
ALPHA<-seq(1.1,2,.3)
dev.new()
par(mfrow=c(1,2))
#plot x
for(i in 1:length(ALPHA)){
  alpha<-ALPHA[i]
  y<-c()
  for(mu in MU) y<-c(y,(exp(mu)-1)/(mu*exp(mu*romberg(f,0,1,maxit=25,tol=1e-12,mu,alpha)$value)))
  if(i==1) plot(MU,y,type='l',xlab=expression(mu),ylab='x',main='size of LS investment',ylim=c(.8,1.2),col=i,lty=i)#,lty=i
  else lines(MU,y,col=i,lty=i)
}
lgd<-c()
for(i in 1:length(ALPHA)) lgd<-c(lgd,paste0('alpha=',ALPHA[i]))
legend('topleft',lgd,lty=1:4,col=1:4)
#plot s
for(i in 1:length(ALPHA)){
  alpha<-ALPHA[i]
  y<-c()
  for(mu in MU) y<-c(y,romberg(f,0,1,maxit=25,tol=1e-12,mu,alpha)$value)
  if(i==1) plot(MU,y,type='l',xlab=expression(mu),ylab='s',main='length of LS investment',ylim=c(.2,.6),col=i,lty=i)
  else lines(MU,y,col=i,lty=i)
}
lgd<-c()
for(i in 1:length(ALPHA)) lgd<-c(lgd,paste0('alpha=',ALPHA[i]))
legend('topleft',lgd,lty=1:4,col=1:4)

#################
f<-function(x,mu,alpha){
  return(((1-exp(mu*x))/(1-exp(mu)))^alpha)
}
MU<-seq(.01,5,.01)
getMaxSharpe<-function(C,alpha,beta){
  y<-c()
  for(mu in MU) y<-c(y,(romberg(f,0,1,maxit=25,tol=1e-12,mu,alpha)$value)^(1/alpha))
  location0<-log((exp(-MU)-1)/(-MU))
  Q<-stable::qstable(C,loc=0,disp=1,skew= -1*beta,tail=alpha)
  maxSharpe<-c() #maxSharpe[k] stores max Sharpe ratio for alpha=Alpha[i] and k withdrawls
  for(i in 1:length(MU)){
    L<-location0[i]
    S<-y[i]
    for(sharpe in seq(20,.01,-.01)){
      if(exp(L+Q*S*MU[i]/sharpe)>1){
        maxSharpe<-c(maxSharpe,sharpe)
        break
      }
    }
  }
  return(maxSharpe)
}
C<-.95
dev.new()
par(mfrow=c(2,2))
SIGMA<-MU/getMaxSharpe(C,1.25,-1)
plot(MU,SIGMA,ylab=expression('min '*sigma),xlab=expression(mu),main=expression(alpha*'=1.25'),type='l',lty=1,lwd=1,col=1)
SIGMA<-MU/getMaxSharpe(C,1.25,0)
lines(MU,SIGMA,type='l',lty=2,lwd=1,col=2)
SIGMA<-MU/getMaxSharpe(C,1.25,1)
lines(MU,SIGMA,type='l',lty=3,lwd=1,col=3)
legend('topleft',c(expression(beta*'=-1'),expression(beta*'=0'),expression(beta*'=1')),lty=1:3,lwd=rep(1,3),col=1:3)

SIGMA<-MU/getMaxSharpe(C,1.5,-1)
plot(MU,SIGMA,ylab=expression('min '*sigma),xlab=expression(mu),main=expression(alpha*'=1.5'),type='l',lty=1,lwd=1,col=1)
SIGMA<-MU/getMaxSharpe(C,1.5,0)
lines(MU,SIGMA,type='l',lty=2,lwd=1,col=2)
SIGMA<-MU/getMaxSharpe(C,1.5,1)
lines(MU,SIGMA,type='l',lty=3,lwd=1,col=3)
legend('topleft',c(expression(beta*'=-1'),expression(beta*'=0'),expression(beta*'=1')),lty=1:3,lwd=rep(1,3),col=1:3)

SIGMA<-MU/getMaxSharpe(C,1.75,-1)
plot(MU,SIGMA,ylab=expression('min '*sigma),xlab=expression(mu),main=expression(alpha*'=1.75'),type='l',lty=1,lwd=1,col=1)
SIGMA<-MU/getMaxSharpe(C,1.75,0)
lines(MU,SIGMA,type='l',lty=2,lwd=1,col=2)
SIGMA<-MU/getMaxSharpe(C,1.75,1)
lines(MU,SIGMA,type='l',lty=3,lwd=1,col=3)
legend('topleft',c(expression(beta*'=-1'),expression(beta*'=0'),expression(beta*'=1')),lty=1:3,lwd=rep(1,3),col=1:3)

SIGMA<-MU/getMaxSharpe(C,2,-1)
plot(MU,SIGMA,ylab=expression('min '*sigma),xlab=expression(mu),main=expression(alpha*'=2'),type='l',lty=1,lwd=1,col=1)
SIGMA<-MU/getMaxSharpe(C,2,0)
lines(MU,SIGMA,type='l',lty=2,lwd=1,col=2)
SIGMA<-MU/getMaxSharpe(C,2,1)
lines(MU,SIGMA,type='l',lty=3,lwd=1,col=3)
legend('topleft',c(expression(beta*'=-1'),expression(beta*'=0'),expression(beta*'=1')),lty=1:3,lwd=rep(1,3),col=1:3)


dev.new()
par(mfrow=c(2,2))
plot(MU/maxSharpe,MU,ylab=expression(mu),xlab=expression(sigma),main=expression(alpha*'=1.25'),type='l',lty=2,lwd=2)
X1<-(MU/maxSharpe)^1
X2<-(MU/maxSharpe)^2
X3<-(MU/maxSharpe)^3
model<-lm(MU~X1+X2+X3)
Yhat<-model$fitted.values
lines(MU/maxSharpe,model$fitted.values)
legend('topleft',c('actual bound','fitted bound'),lty=c(2,1),lwd=c(2,1))
polygon(x=c(MU/maxSharpe,rev(MU/maxSharpe)),y=c(Yhat,rep(0,length(MU/maxSharpe))),col='gray')

########################################################
f<-function(x,mu,alpha){
  return(((1-exp(mu*x))/(1-exp(mu)))^alpha)
}
CC<-seq(.6,.99,.001)
alpha<-1.89
beta<-1
buildP<-function(n){
  sigma<-.110*(n^(1/alpha))
  mu<-.0658*n
  P<-c()
  y<-(romberg(f,0,1,maxit=25,tol=1e-12,mu,alpha)$value)^(1/alpha)
  L<-log((exp(-mu)-1)/(-mu))
  for(C in CC){
    Q<-exp(L+y*sigma*stable::qstable(C,loc=0,disp=1,skew= -1*beta,tail=alpha))
    P<-c(P,Q)
  }
  return(P)
}

dev.new()
count<-1
cols<-c('black','green','red','purple','orange','blue')
for(n in c(2,6,12,20,30,42)){
  P<-buildP(n)
  if(count==1) plot(CC,P,xlab='C',ylab='P',main=paste0('P, given C'),type='l',ylim=c(.3,1.7))
  else lines(CC,P,col=cols[count])
  count<-count+1
}
legend('topleft',c('n=2','n=6','n=12','n=20','n=30','n=42'),col=cols,lty=rep(1,6))



##########################################################
#DCA lower bound quantiles
##########################################################
#builds location parameter mu_k
Location<-function(mu,k){
  return(mu+log((exp(mu*k)-1)/(exp(mu)-1)))
}

#builds sigma_k/sigma, which is scale parameter divided by sigma
Scale<-function(alpha,mu,k){
  temp<-1
  for(j in 1:(k-1)){
    temp<-temp+(1-(exp(mu*j)-1)/(exp(mu*k)-1))^alpha
  }
  return(temp^(1/alpha))
}

K<-c(2,6,12,20,30,42)
dev.new()
par(mfrow=c(2,3))
Q<-seq(.01,.99,.01)
for(k in K){
  xQ<-stable::qstable(Q,loc=Location(0.0658,k)-log(k),disp=(0.1690/sqrt(2))*Scale(2,.0658,k),skew=0,tail=2)
  yQ<-stable::qstable(Q,loc=Location(KPE$Estim$par[4],k)-log(k),disp=KPE$Estim$par[3]*Scale(KPE$Estim$par[1],KPE$Estim$par[4],k),skew=KPE$Estim$par[2],tail=KPE$Estim$par[1])
  plot(xQ,yQ,type='l',main=paste('invest for',k,'years'),ylab='new log-LB quantiles',xlab=expression('log-LB quantiles (Normal log-returns)'))
  lines(seq(-50,50,.01),seq(-50,50,.01),lty=2)
  points(xQ[c(5,95)],yQ[c(5,95)],pch=19)
}

######################################################
#General application for withdrawls
######################################################
#builds location parameter mu_k
#for equal withdrawls at equidistant time steps
Location<-function(mu,k){
  return(-mu+log((exp(-mu*k)-1)/(exp(-mu)-1)))
}

#builds sigma_k/sigma, which is scale parameter divided by sigma
#for equal withdrawls at equidistant time steps
Scale<-function(alpha,mu,k){
  temp<-1
  for(j in 1:(k-1)){
    temp<-temp+(1-(exp(-mu*j)-1)/(exp(-mu*k)-1))^alpha
  }
  return(temp^(1/alpha))
}
mu<-seq(.001,.5,.001) #set of mu's
Alpha<-seq(1.01,2,.01) #set of alpha's
#Alpha<-c(1.89) #just for alpha=1.89, beta= 1
K<-seq(1,60,1) #number of withdrawls

#builds max sharpe ratio (mu/sigma)
C<-.95
Sharpe<-list() #Sharpe[[i]] stores the max Sharpe ratios for alpha=Alpha[i]
for(i in 1:length(Alpha)){
  print(i/length(Alpha))
  alpha<-Alpha[i]
  Q<-stable::qstable(C,loc=0,disp=1,skew= 0,tail=alpha)
  #Q<-stable::qstable(C,loc=0,disp=1,skew= -1,tail=alpha) #just for alpha=1.89, beta= 1
  maxSharpe<-c() #maxSharpe[k] stores max Sharpe ratio for alpha=Alpha[i] and k withdrawls
  for(k in K){
    L<-Location(mu,k)
    S<-Scale(alpha,mu,k)
    for(sharpe in seq(20,.01,-.01)){
      if(sum(exp(L+Q*S*mu/sharpe)>k)==length(mu)){
        maxSharpe<-c(maxSharpe,sharpe)
        break
      }
    }
  }
  Sharpe[[i]]<-maxSharpe
}

#Builds best fit curve for Sharpe[[i]][-1] as a function of K[-1]
#leave out 1st element because there is an outlier at k=1
X1<-K[-1]^.5
X2<-K[-1]^.25
X3<-K[-1]^.125
for(i in 1:length(Alpha)){
  Y<-Sharpe[[i]][-1]
  model<-lm(Y~X1+X2+X3)
  if(i==1) coeff<-as.vector(model$coefficients)
  else coeff<-rbind(coeff,as.vector(model$coefficients))
}
#check best fit; choose i from 1 to length(Alpha)
par(mfrow=c(1,1))
i<-100
Y<-Sharpe[[i]][-1]
plot(K[-1],Y,type='l',col='green')
Yhat<-coeff[i,1]+coeff[i,2]*X1+coeff[i,3]*X2+coeff[i,4]*X3
lines(K[-1],Yhat)
min(Y-Yhat)

#just for alpha=1.89, beta=1
dev.new()
par(mfrow=c(1,1))
i<-1
Y<-Sharpe[[i]][-1]
coeff<-round(coeff,4)
coeff[1]<-coeff[1]-.0065
Yhat<-coeff[1]+coeff[2]*X1+coeff[3]*X2+coeff[4]*X3
plot(K[-1],Y,type='l',lty=2,lwd=2,main=expression(alpha*'=1.89, '*beta*'=1'),xlab='k',ylab=expression(mu/sigma),ylim=c(0,max(Y)))
lines(K[-1],Yhat)
legend('topright',c('actual bound','fitted bound'),lty=c(2,1),lwd=c(2,1))
polygon(x=c(K[-1],rev(K[-1])),y=c(Yhat,rep(0,length(K[-1]))),col='gray')   
any(Y-Yhat<0)

#Builds best fit curve for columns of coeff as functions of Alpha
for(colID in 1:4){
  Y<-coeff[,colID]
  model<-lm(Y~poly(Alpha,5,raw=T)) 
  if(colID==1) coAlpha<-as.vector(model$coefficients)
  else coAlpha<-rbind(coAlpha,as.vector(model$coefficients))
}
#check fits; choose colID from 1 to 4
colID<-1
Y<-coeff[,colID]
model<-lm(Y~poly(Alpha,5,raw=T)) 
plot(Alpha,Y,type='l',col='green')
lines(Alpha,model$fitted.values)
plot(model$residuals)

#Adjust intercept on functions of alpha so the fit is a lower bound
coAlpha2<-round(coAlpha,4) #easier to read 
difs<-c()
for(i in 1:length(Alpha)){
  Y<-Sharpe[[i]][-1]
  alpha<-Alpha[i]
  co<-c() #will store coefficients given alpha
  for(j in 1:4) co<-c(co,sum(coAlpha2[j,]*(rep(alpha,6)^(0:5))))
  Yhat<-co[1]+co[2]*X1+co[3]*X2+co[4]*X3
  difs<-c(difs,min(Y-Yhat))
}
plot(Alpha,difs)
model<-lm(difs~poly(Alpha,5,raw=T))
lines(Alpha,model$fitted.values-.01)

coAlpha2<-coAlpha
coAlpha2[1,]<-coAlpha2[1,]+model$coefficients
coAlpha2[1,1]<-coAlpha2[1,1]-.0087
coAlpha2<-round(coAlpha2,4)
difs<-c()
for(i in 1:length(Alpha)){
  Y<-Sharpe[[i]][-1]
  alpha<-Alpha[i]
  co<-c() #will store coefficients given alpha
  for(j in 1:4) co<-c(co,sum(coAlpha2[j,]*(rep(alpha,6)^(0:5))))
  Yhat<-co[1]+co[2]*X1+co[3]*X2+co[4]*X3
  difs<-c(difs,min(Y-Yhat))
}
plot(difs)
any(difs<0)
#check adjustment; choose i from 1 to length(Alpha)
i<-1
Y<-Sharpe[[i]][-1]
alpha<-Alpha[i]
co<-c() #will store coefficients given alpha
for(j in 1:4) co<-c(co,sum(coAlpha2[j,]*(rep(alpha,6)^(0:5))))
Yhat<-co[1]+co[2]*X1+co[3]*X2+co[4]*X3
plot(K[-1],Y,type='l',col='green')
lines(K[-1],Yhat)

dif<-c()
for(i in 1:length(Alpha)){
  Y<-Sharpe[[i]][-1]
  alpha<-Alpha[i]
  co<-c() #will store coefficients given alpha
  for(j in 1:4) co<-c(co,sum(coAlpha2[j,]*(rep(alpha,6)^(0:5))))
  Yhat<-co[1]+co[2]*X1+co[3]*X2+co[4]*X3
  dif<-c(dif,max(Y-Yhat))
}

dev.new()
par(mfrow=c(2,2))
i<-25
Y<-Sharpe[[i]][-1]
alpha<-Alpha[i]
co<-c() #will store coefficients given alpha
for(j in 1:4) co<-c(co,sum(coAlpha2[j,]*(rep(alpha,6)^(0:5))))
Yhat<-co[1]+co[2]*X1+co[3]*X2+co[4]*X3
plot(K[-1],Y,type='l',lty=2,lwd=2,main=expression(alpha*'=1.25'),xlab='k',ylab=expression(mu/sigma),ylim=c(0,max(Y)))
lines(K[-1],Yhat)
legend('topright',c('actual bound','fitted bound'),lty=c(2,1),lwd=c(2,1))
polygon(x=c(K[-1],rev(K[-1])),y=c(Yhat,rep(0,length(K[-1]))),col='gray')
i<-50
Y<-Sharpe[[i]][-1]
alpha<-Alpha[i]
co<-c() #will store coefficients given alpha
for(j in 1:4) co<-c(co,sum(coAlpha2[j,]*(rep(alpha,6)^(0:5))))
Yhat<-co[1]+co[2]*X1+co[3]*X2+co[4]*X3
plot(K[-1],Y,type='l',lty=2,lwd=2,main=expression(alpha*'=1.5'),xlab='k',ylab=expression(mu/sigma),ylim=c(0,max(Y)))
lines(K[-1],Yhat)
legend('topright',c('actual bound','fitted bound'),lty=c(2,1),lwd=c(2,1))
polygon(x=c(K[-1],rev(K[-1])),y=c(Yhat,rep(0,length(K[-1]))),col='gray')
i<-75
Y<-Sharpe[[i]][-1]
alpha<-Alpha[i]
co<-c() #will store coefficients given alpha
for(j in 1:4) co<-c(co,sum(coAlpha2[j,]*(rep(alpha,6)^(0:5))))
Yhat<-co[1]+co[2]*X1+co[3]*X2+co[4]*X3
plot(K[-1],Y,type='l',lty=2,lwd=2,main=expression(alpha*'=1.75'),xlab='k',ylab=expression(mu/sigma),ylim=c(0,max(Y)))
lines(K[-1],Yhat)
legend('topright',c('actual bound','fitted bound'),lty=c(2,1),lwd=c(2,1))
polygon(x=c(K[-1],rev(K[-1])),y=c(Yhat,rep(0,length(K[-1]))),col='gray')     
i<-100
Y<-Sharpe[[i]][-1]
alpha<-Alpha[i]
co<-c() #will store coefficients given alpha
for(j in 1:4) co<-c(co,sum(coAlpha2[j,]*(rep(alpha,6)^(0:5))))
Yhat<-co[1]+co[2]*X1+co[3]*X2+co[4]*X3
plot(K[-1],Y,type='l',lty=2,lwd=2,main=expression(alpha*'=2'),xlab='k',ylab=expression(mu/sigma),ylim=c(0,max(Y)))
lines(K[-1],Yhat)
legend('topright',c('actual bound','fitted bound'),lty=c(2,1),lwd=c(2,1))
polygon(x=c(K[-1],rev(K[-1])),y=c(Yhat,rep(0,length(K[-1]))),col='gray')

###############################################
#upper bound on success probability for p=.6,.4
###############################################
dev.new()
par(mfrow=c(1,1))
p<-.6
ls<-log(p*s+1-p)
K<-2:50
P<-c()
for(k in K){
  P<-c(P,pnorm(log(k),Location(mean(ls),k),sd(ls)*Scale(2,mean(ls),k)))
}
plot(K,P,pch=19,xlab='k',ylab='C',main='C vs k',ylim=c(.7,1))
p<-.4
ls<-log(p*s+1-p)
P<-c()
for(k in K){
  P<-c(P,pnorm(log(k),Location(mean(ls),k),sd(ls)*Scale(2,mean(ls),k)))
}
points(K,P,col='red',pch=19)
grid()
#adjusted
p<-.6
ls<-log(p*s+1-p)
K<-2:50
P<-c()
for(k in K){
  P<-c(P,pnorm(log(k),Location(mean(ls),k),sd(ls)*Scale(2,mean(ls),k))+1-(1-pnorm(log(p*.6+1-p),mean(ls),sd(ls)))^k)
}
points(K,P,pch=4)
p<-.4
ls<-log(p*s+1-p)
P<-c()
for(k in K){
  P<-c(P,pnorm(log(k),Location(mean(ls),k),sd(ls)*Scale(2,mean(ls),k))+1-(1-pnorm(log(p*.6+1-p),mean(ls),sd(ls)))^k)
}
points(K,P,col='red',pch=4)
legend('topleft',c('p = .4 (adjusted)','p = .6 (adjusted)','p = .4','p = .6'),pch=c(4,4,19,19),col=c('red','black','red','black'))
grid()


#######################################################
#stable package info
#######################################################
#Use this to read about the stable package and how quantiles are computed
##run next line in r
#untar(download.packages(pkgs = "stable",destdir = ".",type = "source")[,2])
##run next line in windows command line (adjust path first)
#tar -xvzf C:\Users\17753\Downloads\stable_1.1.4.tar.gz -C C:\Users\17753\Downloads




###########################################
#All code below this point is just leftover 
#and miscelaneous code.
###########################################





















################################################################
#Best fit with constraints
################################################################

polyf<-function(par,X=K[-1]) {
  X1<-X^.125
  X2<-X^.25
  X3<-X^.5
  return(par[1]+par[2]*X1+par[3]*X2+par[4]*X3)
}

#penalized sum of errors
objectiveF <- function(par,y=Y) {
  temp<-y-polyf(par)
  temp[temp<0]<-100
  return(-temp)
}

#non-penalized function
res<-maxNR(objectiveF, start=model$coefficients-1)
plot(K[-1],Y)
lines(K[-1],polyf(coef(res)))

#Builds best fit curve for Sharpe[[i]][-1] as a function of K[-1]
#leave out 1st element because there is an outlier at k=1
X1<-K[-1]^.5
X2<-K[-1]^.25
X3<-K[-1]^.125
for(i in 1:length(Alpha)){
  Y<-Sharpe[[i]][-1]
  model<-lm(Y~X1+X2+X3)
  temp<-.5
  model<-lm(Y-temp*max(model$residuals)^.5~X1+X2+X3)
  if(i==1) coeff<-as.vector(model$coefficients)
  else coeff<-rbind(coeff,as.vector(model$coefficients))
}
#check best fit; choose i from 1 to length(Alpha)
i<-100
Y<-Sharpe[[i]][-1]
#model<-lm(Y~X1+X2+X3)
plot(K[-1],Y,type='l',col='green')
#lines(K[-1],model$fitted.values)
lines(K[-1],coeff[i,1]+coeff[i,2]*X1+coeff[i,3]*X2+coeff[i,4]*X3)
#plot(model$residuals)

#Builds best fit curve for columns of coeff as functions of Alpha
for(colID in 1:4){
  Y<-coeff[,colID]
  model<-lm(Y~poly(Alpha,5,raw=T)) 
  #model<-lm(Y-mean(model$residuals)~poly(Alpha,5,raw=T)) 
  if(colID==1) coAlpha<-as.vector(model$coefficients)
  else coAlpha<-rbind(coAlpha,as.vector(model$coefficients))
}
#check fits; choose colID from 1 to 4
colID<-1
Y<-coeff[,colID]
model<-lm(Y~poly(Alpha,5,raw=T)) 
plot(Alpha,coeff[,colID],type='l',col='green')
lines(Alpha,model$fitted.values)
plot(model$residuals)

#Adjust intercept on functions of alpha so the fit is a lower bound
coAlpha2<-round(coAlpha,4) #easier to read
for(i in 1:length(Alpha)){
  Y<-Sharpe[[i]][-1]
  alpha<-Alpha[i]
  co<-c() #will store coefficients given alpha
  for(j in 1:4) co<-c(co,sum(coAlpha2[j,]*(rep(alpha,6)^(0:5))))
  Yhat<-co[1]+co[2]*X1+co[3]*X2+co[4]*X3
  if(sum(Y-Yhat>0)!=length(Y)){
    print(paste('Failure at alpha=',alpha))
  }
}
#check adjustment; choose i from 1 to length(Alpha)
i<-100
Y<-Sharpe[[i]][-1]
alpha<-Alpha[i]
co<-c() #will store coefficients given alpha
for(j in 1:4) co<-c(co,sum(coAlpha2[j,]*(rep(alpha,6)^(0:5))))
Yhat<-co[1]+co[2]*X1+co[3]*X2+co[4]*X3
plot(K[-1],Y,type='l',col='green')
lines(K[-1],Yhat)











#build R_k moments 
MR<-function(c0,c,N=9){
  #max Nth moment
  EY<-list()
  ER<-list()
  for(i in 1:N){ #i is used in place of n in the proposition
    EY[[i]]<-c(c0^i*exp(i*mu+(i^2)*(s^2)/2))
    ER[[i]]<-c(exp(i*mu+(i^2)*(s^2)/2))
    for(k in 2:(length(c)+1)){
      temp<-c[k-1]^i
      for(j in 1:i){
        temp<-temp+cmb(i,j)*(c[k-1]^(i-j))*EY[[j]][k-1]
      }
      EY[[i]][k]<-exp(i*mu+(i^2)*(s^2)/2)*temp
      ER[[i]][k]<-(EY[[i]][k])/((c0+sum(c[1:k-1]))^i)
    }
  }
  return(ER)
}

#build m
make.m<-function(c0,c){
  m<-c(mu)
  v<-c(s^2)
  a<-c(1)
  b<-c(1)
  for (k in 2:(length(c)+1)) {
    if(k==2) sc<-c0 else sc<-c0+sum(c[1:(k-2)])
    a[k]<-sc*exp(m[k-1])
    b[k]<-a[k]/(a[k]+c[k-1])
    m[k]<-log((a[k]+c[k-1])/((sc+c[k-1])*(a[k]^b[k])))+mu+b[k]*(m[k-1]+log(sc))
    v[k]<-s^2+(b[k]^2)*v[k-1]
  }
  return(m)
}

#build v
make.v<-function(c0,c){
  m<-c(mu)
  v<-c(s^2)
  a<-c(1)
  b<-c(1)
  for (k in 2:(length(c)+1)) {
    if(k==2) sc<-c0 else sc<-c0+sum(c[1:(k-2)])
    a[k]<-sc*exp(m[k-1])
    b[k]<-a[k]/(a[k]+c[k-1])
    m[k]<-log((a[k]+c[k-1])/((sc+c[k-1])*(a[k]^b[k])))+mu+b[k]*(m[k-1]+log(sc))
    v[k]<-s^2+(b[k]^2)*v[k-1]
  }
  return(v)
}

#expected error
ERR<-function(er,m,v){
  er-exp(m+v/2)
}

#expected log error upper bound
#runs through multiple upper bounds and picks the min at each time step
logERR<-function(ER,k,m,N=9,Y1= -2,Y2=2){
  JJ<-seq(1,N,2) #degrees of Taylor series
  Y<-ER[[1]][k]*exp(seq(Y1,Y2,.01)) #values for y
  for(J in JJ){
    for(y in Y){
      temp<-log(y)
      for(j in 1:J){
        temp2<- (-y)^j
        for(i in 1:j){
          temp2<-temp2+cmb(j,i)*ER[[i]][k]*((-y)^(j-i))
        }
        temp<-temp+temp2/(((-1)^(j-1))*j*y^j)
      }
      if(J==JJ[1] & y==Y[1]) lub<-temp #stores least upper bounds on expected log R_k
      else lub<-min(lub,temp)
    }
  }
  return(lub-m)
}


###########################################
#plot quantiles for returns
#RUN THIS THIRD
###########################################
m<-make.m(c0,c)
v<-make.v(c0,c)
par(mfrow=c(1,2))
#actual scale
plot(1:n,exp(m),ylim=c(min(exp(m-2*sqrt(v))),max(exp(m+2*sqrt(v)))),type='l',col='green',xlab='years invested',ylab='return',main='LB of DCA returns')
lines(exp(m+2*sqrt(v)))
lines(exp(m-2*sqrt(v)))
for(k in 1:n){
  segments(k,exp(m[k]-2*sqrt(v[k])),k,exp(m[k]+2*sqrt(v[k])),lty=3)
}
#log scale
plot(1:n,m,ylim=c(min(m-2*sqrt(v)),max(m+2*sqrt(v))),type='l',col='green',xlab='years invested',ylab='log-return',main='LB of DCA log-returns')
lines(m+2*sqrt(v))
lines(m-2*sqrt(v))
for(k in 1:n){
  segments(k,m[k]-2*sqrt(v[k]),k,m[k]+2*sqrt(v[k]),lty=3)
}

###########################################
#plot log-sharpe ratio
###########################################
par(mfrow=c(1,1))
plot(1:n,m/sqrt(v),xlab='years invested',ylab='Sharpe ratio',main='Sharpe ratio for LB of DCA log-returns',pch=19)
grid()

###########################################
#plot expected error
###########################################
ER<-MR(c0,c)
par(mfrow=c(1,2))
#expected actual error
E<-c()
for(k in 1:n){
  E[k]<-ERR(ER[[1]][k],m[k],v[k])
}
plot(1:n,E,xlab='years invested',ylab='expected error',main='DCA expected error',type='l')
grid()

#expected log error upper bound
lE<-c() 
for(k in 1:n){
  lE[k]<-logERR(ER,k,m[k])
}
plot(1:n,lE,xlab='years invested',ylab='expected log-error',main='UB of DCA expected log-error',type='l')
grid()

###########################################
#plot lump sum discount
###########################################
#lump sum discount 
par(mfrow=c(1,1))
t<-v/(s^2)
x<-exp(log(1:n)+m-mu*t)
k<-1:n
plot(x/k,t/k,ylab='relative time invested',xlab='relative total invested',main='Lump sum discount',pch=20)
lines(x/k,t/k)
points(x[1],t[1],col='green',pch=20)
points(x[n]/n,t[n]/n,col='red',pch=20)
grid()

#lump sum discount with actual total invested
t<-v/(s^2)
x<-exp(log(1:n)+m-mu*t)
k<-1:n
plot(x,t/k,ylab='relative time invested',xlab='total invested',main='Lump sum discount with actual total invested',pch=20)
lines(x,t/k)
points(x[1],t[1],col='green',pch=20)
points(x[n],t[n]/n,col='red',pch=20)
grid()

#lump sum discount for continuous DCA sigma=1
z<-seq(.001,6,.001)
t<-((2*z-3)*exp(2*z)+4*exp(z)-1)/(2*z*((exp(z)-1)^2))
x<-exp(log((exp(z)-1)/z)-z*t)
plot(x,t,ylab='relative time invested',xlab='relative total invested',main='Lump sum discount for continuous DCA',type='l')
points(x[1],t[1],col='green',pch=20)
points(x[length(x)],t[length(t)],col='red',pch=20)
for(i in seq(.2,5.8,.2)){
  points(x[1000*i],t[1000*i],pch=4)
}
grid()

#cost of continuous DCA r1 and r2
par(mfrow=c(1,1))
z<-seq(.001,10,.001)
t<-((2*z-3)*exp(2*z)+4*exp(z)-1)/(2*z*((exp(z)-1)^2))
x<-log((exp(z)-1)/z)/z
plot(z,t,ylab='',xlab=expression(mu),main=expression('r'[1]*' and r'[2]*' for continuous DCA'),type='l')
lines(z,x,lty=2)
grid()

#limit of x_k
par(mfrow=c(1,1))
z<-seq(.01,.15,.001)
t<-exp(z*((exp(2*z)+2*exp(z))/(exp(2*z)-1)))/(exp(z)-1)
plot(z,t,ylab=expression('x'[k]*' as k'%->%infinity),xlab=expression(mu),main=expression('limit of x'[k]*' for DCA'),type='l')
grid()


###########################################
#lump sum and dollar cost averaging combo
#re-run earlier blocks after using this
###########################################
#first investment is lump sum and the rest are DCA
#same total investment
#ratio between LS and DCA investments is varied
#par(mar=c(3.8,4,2,2))
par(mfrow=c(2,3))
for(K in c(2,6,12,20,30,42)){
  C0<-c(seq(min(1/K,.01),1/K,min(1/K,.01)),seq(1/K+min(1/K,.01),1,min(1/K,.01))) #1 is total investment
  M<-c()
  V<-c()
  for(c0 in C0){
    c<-rep((1-c0)/(K-1),(K-1))
    m<-make.m(c0,c)
    v<-make.v(c0,c)
    M[which(c0==C0)]<-m[K]
    V[which(c0==C0)]<-v[K]
  }
  plot(C0,M,ylim=c(min(M-2*sqrt(V)),max(M+2*sqrt(V))),col='green',type='l',
       main=paste('Invest for',K,'years'),
       xlab='LS investment',ylab='log-return')
  lines(C0,M+2*sqrt(V))
  lines(C0,M-2*sqrt(V))
  segments(1/K,-5,1/K,10,1,lty=3)
  #grid()
}
#plot error and log-error for the above
par(mfrow=c(2,3))
for(K in c(2,6,12,20,30,42)){
  C0<-c(seq(min(1/K,.01),1/K,min(1/K,.01)),seq(1/K+min(1/K,.01),1,min(1/K,.01))) #1 is total investment
  M<-c()
  V<-c()
  E<-c()
  lE<-c()
  for(c0 in C0){
    c<-rep((1-c0)/(K-1),(K-1))
    m<-make.m(c0,c)
    v<-make.v(c0,c)
    ER<-MR(c0,c)
    E[which(c0==C0)]<-ERR(ER[[1]][K],m[K],v[K])
    lE[which(c0==C0)]<-logERR(ER,K,m[K])
  }
  plot(C0,E,ylim=c(min(c(E,lE)),max(c(E,lE))),type='l',
       main=paste('Invest for',K,'years'),
       xlab='LS investment',ylab='error')
  lines(C0,lE,lty=2)
  segments(1/K,-5,1/K,10,1,lty=3)
  #grid()
}
par(mar=c(5.1,4.1,4.1,2.1))

#first investment is lump sum and the rest are DCA
#different total investment
#ratio between LS and DCA investments is varied
par(mfrow=c(2,3))
c0<-1
C<-seq(0,2,.01)
for(K in c(2,6,12,20,30,42)){
  M<-c()
  V<-c()
  for(cc in C){
    c<-rep(cc,(K-1))
    m<-make.m(c0,c)
    v<-make.v(c0,c)
    M[which(cc==C)]<-m[K]
    V[which(cc==C)]<-v[K]
  }
  plot(C,M,ylim=c(min(M-2*sqrt(V)),max(M+2*sqrt(V))),col='green',type='l',
       main=paste('Invest for',K,'years'),
       xlab='basic DCA investment',ylab='log-return')
  lines(C,M+2*sqrt(V))
  lines(C,M-2*sqrt(V))
  segments(1,-5,1,10,1,lty=3)
  #grid()
}
#plot error and log-error for the above
par(mfrow=c(2,3))
c0<-1
C<-seq(0,2,.01)
for(K in c(2,6,12,20,30,42)){
  M<-c()
  V<-c()
  E<-c()
  lE<-c()
  for(cc in C){
    c<-rep(cc,(K-1))
    m<-make.m(c0,c)
    v<-make.v(c0,c)
    ER<-MR(c0,c)
    E[which(cc==C)]<-ERR(ER[[1]][K],m[K],v[K])
    lE[which(cc==C)]<-logERR(ER,K,m[K])
  }
  plot(C,E,ylim=c(min(c(E,lE)),max(c(E,lE))),type='l',
       main=paste('Invest for',K,'years'),
       xlab='basic DCA investment',ylab='error')
  lines(C,lE,lty=2)
  segments(1,-5,1,10,1,lty=3)
  #grid()
}

