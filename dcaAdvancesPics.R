###########################################
#check S&P for geometric Brownian motion RUN FIRST
###########################################
library(stats)
library(car)      
library(MASS)     
library(graphics)
library(openxlsx)
library(ks)
library(spatstat)
library(GoFKernel)
setwd("C:/Users/17753/Downloads")
x<-read.xlsx(xlsxFile = 'data7.xlsx', sheet = 1, skipEmptyRows = TRUE)

#builds inflation adjusted returns
par(mfrow=c(1,3))
L<-nrow(x)
d<-x[1:(L-1),c('Year','Dividend','Index','CPI','1YTR')]
d$Inflation<-x$CPI[2:L]/x$CPI[1:(L-1)]
s<-((x$Index[2:L]+x$Dividend[1:(L-1)])/x$Index[1:(L-1)])/d$Inflation
ls<-log(s)

qqnorm(ls,col='blue',main=paste0('S&P log-returns QQ plot'),xlab='N(0,1) quantiles',ylab='log-return quantiles')  
qqline(ls)   
grid() 
acfs<-acf(s,plot=F)
plot(acfs[1:21],main='S&P returns ACF')
pacf(s,main='S&P returns PACF')

ks.test(ls,'pnorm',mean(ls),sd(ls))


###########################################
#parameters and functions to RUN SECOND

###########################################
mu<-0.06577757
s<-0.1689873
n<-50 #max investment length
c0<-1
c<-rep(1,n-1)

#combination function
cmb<-function(m,k){
  if(m==k) return(1)
  if(k==0) return(1)
  prod(1:m)/(prod(1:k)*prod(1:(m-k)))
}

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




