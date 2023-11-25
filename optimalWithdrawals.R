library(ghyp)
#library(TempStable)
library(openxlsx)
library(kde1d)
library(statmod)

setwd("C:/Users/17753/Downloads")
x<-read.xlsx(xlsxFile = 'data7.xlsx', sheet = 1, skipEmptyRows = TRUE)

#builds inflation adjusted returns
par(mfrow=c(1,3))
L<-nrow(x)
d<-x[1:(L-1),c('Year','Dividend','Index','CPI','1YTR')]
d$Inflation<-x$CPI[2:L]/x$CPI[1:(L-1)]
s<-((x$Index[2:L]+x$Dividend[1:(L-1)])/x$Index[1:(L-1)])/d$Inflation
ls<-log(s)

#independence tests
Box.test(s,lag=1,type='Ljung-Box')
Box.test(s,lag=5,type='Ljung-Box')
Box.test(ls,lag=1,type='Ljung-Box')
Box.test(ls,lag=5,type='Ljung-Box')
Box.test(abs(ls),lag=1,type='Ljung-Box')
Box.test(abs(ls),lag=5,type='Ljung-Box')
#acf and pacf
par(mfrow=c(1,2))
acfs<-acf(ls,plot=F)
plot(acfs[1:21],main='S&P log-returns ACF')
pacf(ls,main='S&P log-returns PACF')
#qq plots
X<-seq(.01,.99,.01)
plot(qnorm(X,mean=mean(s),sd=sd(s)),quantile(s,probs=X,type=7),ylab='return quantiles',main='S&P returns QQ plot',xlab=expression('N(1.083, .1753'^2*') quantiles'))
lines(seq(-1,2,.01),seq(-1,2,.01))
plot(log(qnorm(X,mean=mean(s),sd=sd(s))),quantile(ls,probs=X,type=7),ylab='log-return quantiles',main='S&P log-returns QQ plot',xlab=expression('N(1.083, .1753'^2*') log-quantiles'))
lines(seq(-1,2,.01),seq(-1,2,.01))
#density plots
par(mfrow=c(1,1))
hist(s,probability=T,ylim=c(0,2.5),breaks=10,main='Probability histogram of S&P returns vs. Normal pdf',xlab='return',ylab='density')
lines(seq(-1,2,.01),dnorm(seq(-1,2,.01),mean=mean(s),sd=sd(s)),lty=1,lwd=2)
legend('topleft',c(expression('N(1.083, .1753'^2*') pdf')),lwd=c(2),lty=c(1),col=c('black'))
#check log-mean and log-sd
set.seed(1)
mean(ls)
mean(log(rnorm(100000,mean=mean(s),sd=sd(s))))
sd(ls)
sd(log(rnorm(100000,mean=mean(s),sd=sd(s))))

#############################
#Withdrawals 
#############################
#portfolios consist of cash and the log-normal r.v.
#(1-q) in cash and q in the r.v.
#choose which f to run
#this f helps make the v_i produced by buildV 
f<-function(q,mu,std,r,x,D,v,c,w,M){
  vdensity<-dnorm((D-c+(1+r)*(q-1)*x)/(q*x),mean=mu,sd=std)
  if(sum(vdensity)<10^(-10)) return(0) #to avoid errors
  return((1-pnorm((0-c+(1+r)*(q-1)*x)/(q*x),mean=mu,sd=std))*sum(v*vdensity)/sum(vdensity))
}
#implements algorithm 1
#c is vector of investments/withdrawals
#mu,std is mean,standard deviation of returns
#other parameters are specified in algorithm
buildV<-function(c,w,mu,std,M,r){
  k<-length(c)-1 
  G1<-seq(.01,.99,.01)
  D<-list()
  for(i in k:1){#i here means i-1 in algorithm because 0 can't be an index
    D[[i]]<-(1:(2*M))*w[i]/(M) #for lower bound f 
  }
  V<-list() #stores v_i over i
  Q<-list() #stores q*_i over i
  #initialize i, v and q*
  Q[[k]]<-c(rep(1,M-1),rep(0,M+1)) 
  V[[k]]<-1-pnorm(w[k]*(1+r)/D[[k]],mean=mu,sd=std)
  V[[k]][M:(2*M)]<-1 #V[[k]] here is v_{k-1} in algorithm
  #build successive v and q*
  for(i in (k-1):1){#i here means i-1 in algorithm because 0 can't be an index
    oldv<-V[[i+1]]
    oldD<-D[[i+1]]
    cnt<-1 #indicates position in D
    v<-c() #v_i
    q<-c() #q*_i
    for(x in D[[i]]){
      #this loop finds q*_i(x) and v_i(x) by trying different q*_i(x) and then picking the best
      #remember that i here is i-1 in algorithm
      #try q*_i(x)=0 first
      q[cnt]<-0
      if(x>=w[i]-w[i]/(4*M)){#covers borderline >= cases
        v[cnt]<-1
        cnt<-cnt+1
        next
      } 
      temp<-(1+r)*x+c[i+1]
      if(temp<oldD[1]){ 
        v[cnt]<-0 #this is a lower bound for q=0
      }else if(temp>= w[i+1]){
        v[cnt]<-1 #this is exact for q=0
      }else{
        y<-tail(which(temp-oldD>=0),1)
        v[cnt]<-oldv[y] #this is a lower bound for q=0
      }
      #try q*_i(x) in (0,1] next
      temp<-unlist(lapply(G1,f,mu,std,r,x,oldD,oldv,c[i+1],w[i+1],M))
      if(max(temp)<10^(-13)){ #handles the situations when no max is apparent to the computer
        q[cnt]<-1
        v[cnt]<-f(1,mu,std,r,x,oldD,oldv,c[i+1],w[i+1],M)
        cnt<-cnt+1
        next
      }
      q1<-G1[which.max(temp)]
      G2<-seq(q1-.001*9,q1+.001*9,.001)
      temp<-unlist(lapply(G2,f,mu,std,r,x,oldD,oldv,c[i+1],w[i+1],M))
      q2<-G2[which.max(temp)]
      G3<-seq(q2-.0001*9,q2+.0001*10,.0001)
      temp<-unlist(lapply(G3,f,mu,std,r,x,oldD,oldv,c[i+1],w[i+1],M))
      q3<-G3[which.max(temp)]
      temp<-max(temp)
      #pick best q*_i(x) and v_i(x); remember that i here is i-1 in algorithm
      if(temp>v[cnt]){
        v[cnt]<-temp
        q[cnt]<-q3
      }
      cnt<-cnt+1
    }
    V[[i]]<-v #store v_i
    Q[[i]]<-q #store q*_i
    print(i)
  }
  return(list(V,Q,D))
}

#set parameters for buildV
k<-50 
c<-c(1,rep(-1,k)) #stores c_0 through c_k, but c_i=c[i+1] here
w<-c() #holds w_0,...,w_k 
w[k+1]<-0
for(i in k:1){
  w[i]<-(w[i+1]-c[i+1])/(1+r)
}
r<-0
M<-200
mu<-mean(s)
std<-sd(s)

#compute V,Q,D (use this for much of the subsequent code)
temp<-buildV(c,w,mu,std,M,r)
V<-temp[[1]]
Q<-temp[[2]]
D<-temp[[3]]

##########################################
#plot v_0 and pi_0 for different k
#uses above V, Q, D, w
##########################################
dev.new()
par(mfrow=c(2,2))
for(k in c(20,30,40,50)){
  idx<-which(w==k)
  v<-V[[idx]][1:M]
  q<-Q[[idx]][1:M]
  z<-D[[idx]][1:M]
  plot(z,rep(.95,length(z)),lty=3,type='l',xlab='x',ylim=c(0,1),ylab='',main=paste0('k = ',k))
  grid()
  lines(z,v,col='black',lwd=2)
  lines(z,q,col='black',lwd=2,lty=2)
}

##################################
#90%, 95% and 99% x values
#uses above V, D and w
##################################
dev.new()
par(mfrow=c(1,1))
x1<-c()
x2<-c()
x3<-c()
for(k in 1:50){
  idx<-which(w==k)
  v<-V[[idx]]
  z<-D[[idx]]
  x1<-c(x1,z[which(v>.9)[1]])
  x2<-c(x2,z[which(v>.95)[1]])
  x3<-c(x3,z[which(v>.99)[1]])
}
plot(1:50,x3,type='l',main=expression('Necessary and sufficient initial investments to withdraw\n       1 unit, annually, for k years with confidence C'),col='black',xlim=c(0,50),ylim=c(0,40),lwd=2,lty=3,xlab='k',ylab=expression('min  x  s.t.  v '[0]*'(x)'>='C'))
grid()
lines(1:50,x2,col='black',lwd=2)
lines(1:50,x1,col='black',lwd=2,lty=2)
legend('topleft',c('C = .99','C = .95','C = .9'),lwd=c(2,2,2),lty=c(3,1,2),col=c('black','black','black'))

################################################
#error check for k=50
#also compares optimal portfolio with fixed weight portfolios
#uses above V, Q, D, mu, std, r, c, w
################################################
dev.new()
par(mfrow=c(1,1))
K<-length(c)-1
k<-50
idx<-which(w==k)
v<-V[[idx]][1:M]
z<-D[[idx]][1:M]
plot(z,v,type='l',xlab='x',ylab='',main=expression('v'[0]*'(x) and various u'[0]*'(x) for k = 50'))
grid()
lines(z,v,lwd=2) #to cover grid marks
legend('topleft',c(expression('v'[0]*'(x) from Algorithm 3'),expression('simulated v'[0]*'(x)'),expression('u'[0]*'(x) for '*pi[i1]*' = 1'),expression('u'[0]*'(x) for '*pi[i1]*' = .9'),expression('u'[0]*'(x) for '*pi[i1]*' = .6')),lty=c(1,NA,NA,NA,NA),pch=c(NA,3,0,20,1),col=c(1,'red',1,1,1))
#use Q to simulate v_0
u<-c()
L<-100000
X<-seq(k/25,k,k/25)
for(x in X){
  x<-rep(x,L)
  for(i in (K-k+1):K){
    q<-c(1,Q[[i]],0)
    z<-c(-1000,D[[i]],1000) #adds big end intervals for cut function so error isn't produced
    a<-cut(x,z,labels=F)
    q<-(x-z[a])*(q[a+1]-q[a])/(z[a+1]-z[a])+q[a]
    Y<-rnorm(L,mu,std)
    x<-(q*Y+(1-q)*(1+r))*x+c[i+1]
    x[x<0]<- -1 #to avoid errors
  }
  u<-c(u,mean(x>=0))
}
points(X,u,pch=3,col='red')
#60-40 portfolio
u<-c()
for(x in X){
  x<-rep(x,L)
  for(i in (K-k+1):K){
    Y<-rnorm(L,mu,std)
    x<-(.6*Y+(1-.6)*(1+r))*x+c[i+1]
    x[x<0]<- -1
  }
  u<-c(u,mean(x>=0))
}
points(X,u,pch=1)
#90-10 portfolio
u<-c()
for(x in X){
  x<-rep(x,L)
  for(i in (K-k+1):K){
    Y<-rnorm(L,mu,std)
    x<-(.9*Y+(1-.9)*(1+r))*x+c[i+1]
    x[x<0]<- -1
  }
  u<-c(u,mean(x>=0))
}
points(X,u,pch=20)
#100-0 portfolio
u<-c()
for(x in X){
  x<-rep(x,L)
  for(i in (K-k+1):K){
    Y<-rnorm(L,mu,std)
    x<-(1*Y+(1-1)*(1+r))*x+c[i+1]
    x[x<0]<- -1
  }
  u<-c(u,mean(x>=0))
}
points(X,u,pch=0)

#plot v_0 for different k (not in manuscript)
dev.new()
par(mfrow=c(1,1))
plot(0:50,rep(.95,51),lty=3,type='l',xlab='x',ylim=c(0,1),ylab='',main=paste0('k = ',k))
grid()
for(k in seq(1,50,1)){
  idx<-which(w==k)
  v<-V[[idx]]
  z<-D[[idx]]
  lines(z,v,col='black',lwd=1)
}

######################################################
#compare v_40 with other means and standard deviations of returns
#uses c, w, mu, std, r from above
######################################################
K<-length(c)-1
k<-40
M<-100
temp<-buildV(c,w,mu,std,M,r)
V<-temp[[1]]
Q<-temp[[2]]
D<-temp[[3]]
u2<-V[[which(w==40)]] #from earlier run of buildV
temp<-buildV(c,w,mu,std-.02,M,r)
V<-temp[[1]]
u1<-V[[which(w==40)]] 
temp<-buildV(c,w,mu,std+.02,M,r)
V<-temp[[1]]
u3<-V[[which(w==40)]] 

temp<-buildV(c,w,mu-.01,std,M,r)
V<-temp[[1]]
u0<-V[[which(w==40)]] 
temp<-buildV(c,w,mu+.01,std,M,r)
V<-temp[[1]]
u4<-V[[which(w==40)]] 

dev.new()
par(mfrow=c(1,2))
Z<-D[[which(w==40)]][1:M]

plot(Z,u2[1:M],type='l',lwd=2,xlab='x',ylab=expression('v'[0]*'(x)'),main=expression('v'[0]*'(x) for '*mu~'= 1.083 and various '*sigma~'(k = 40)'))
grid()
lines(Z,u1[1:M],lty=2,lwd=2)
lines(Z,u3[1:M],lty=3,lwd=2)
legend('topleft',c(expression(sigma~'= .1553'),expression(sigma~'= .1753'),expression(sigma~'= .1953')),lwd=c(2,2,2),lty=c(2,1,3))
u<-c()
L<-100000
X<-seq(k/25,k,k/25)
for(x in X){
  x<-rep(x,L)
  for(i in (K-k+1):K){
    q<-c(1,Q[[i]],0)
    z<-c(-1000,D[[i]],1000) #adds big end intervals for cut function so error isn't produced
    a<-cut(x,z,labels=F)
    q<-(x-z[a])*(q[a+1]-q[a])/(z[a+1]-z[a])+q[a]
    Y<-rnorm(L,mu,std-.02)
    x<-(q*Y+(1-q)*(1+r))*x+c[i+1]
    x[x<0]<- -1 #to avoid errors
    i<-i+1
    mean(x>=0)
  }
  u<-c(u,mean(x>=0))
}
points(X,u,pch=3,col='red',lwd=1)
u<-c()
for(x in X){
  x<-rep(x,L)
  for(i in (K-k+1):K){
    q<-c(1,Q[[i]],0)
    z<-c(-1000,D[[i]],1000) #adds big end intervals for cut function so error isn't produced
    a<-cut(x,z,labels=F)
    q<-(x-z[a])*(q[a+1]-q[a])/(z[a+1]-z[a])+q[a]
    Y<-rnorm(L,mu,std+.02)
    x<-(q*Y+(1-q)*(1+r))*x+c[i+1]
    x[x<0]<- -1 #to avoid errors
    i<-i+1
    mean(x>=0)
  }
  u<-c(u,mean(x>=0))
}
points(X,u,pch=4,col='red',lwd=1)

plot(Z,u2[1:M],type='l',lwd=2,xlab='x',ylab=expression('v'[0]*'(x)'),main=expression('v'[0]*'(x) for '*sigma~'= .1753 and various '*mu~'(k = 40)'))
grid()
lines(Z,u0[1:M],lty=2,lwd=2)
lines(Z,u4[1:M],lty=3,lwd=2)
legend('topleft',c(expression(mu~'= 1.073'),expression(mu~'= 1.083'),expression(mu~'= 1.093')),lwd=c(2,2,2),lty=c(2,1,3))
u<-c()
L<-100000
X<-seq(k/25,k,k/25)
for(x in X){
  x<-rep(x,L)
  for(i in (K-k+1):K){
    q<-c(1,Q[[i]],0)
    z<-c(-1000,D[[i]],1000) #adds big end intervals for cut function so error isn't produced
    a<-cut(x,z,labels=F)
    q<-(x-z[a])*(q[a+1]-q[a])/(z[a+1]-z[a])+q[a]
    Y<-rnorm(L,mu-.01,std)
    x<-(q*Y+(1-q)*(1+r))*x+c[i+1]
    x[x<0]<- -1 #to avoid errors
    i<-i+1
    mean(x>=0)
  }
  u<-c(u,mean(x>=0))
}
points(X,u,pch=3,col='red',lwd=1)
u<-c()
for(x in X){
  x<-rep(x,L)
  for(i in (K-k+1):K){
    q<-c(1,Q[[i]],0)
    z<-c(-1000,D[[i]],1000) #adds big end intervals for cut function so error isn't produced
    a<-cut(x,z,labels=F)
    q<-(x-z[a])*(q[a+1]-q[a])/(z[a+1]-z[a])+q[a]
    Y<-rnorm(L,mu+.01,std)
    x<-(q*Y+(1-q)*(1+r))*x+c[i+1]
    x[x<0]<- -1 #to avoid errors
    i<-i+1
    mean(x>=0)
  }
  u<-c(u,mean(x>=0))
}
points(X,u,pch=4,col='red',lwd=1)


##########################
#dca then equal withdrawals (used for experimentation)
#used to make a table
##########################
c<-c(rep(.14,50),rep(-1,30)) #stores c_0 through c_k, but c_i=c[i+1] here (first rep() holds dca, second rep() holds withdrawals)
k<-length(c)-1
w<-c()
w[k+1]<-0
for(i in k:1){
  w[i]<-(w[i+1]-c[i+1])/(1+r)
}
r<-0
M<-round(w[1]/c[1]) #produces a good final domain for v_0 (first element of domain matches or almost matches dca amount) must make M an integer or won't work properly
if(M<100) M<-ceiling(100/M)*M #makes M large enough to get accurate results (at least 100)
temp<-buildV(c,w,mu,std,M,r)
V<-temp[[1]]
Q<-temp[[2]]
D<-temp[[3]]
i<-k-1
plot(D[[k-i]][1:M],V[[k-i]][1:M])
D[[k-i]][1]
#i<-k-5
#plot(D[[k-i]][1:M],Q[[k-i]][1:M])

#check v_0(c_0) accuracy from above
L<-100000
x<-rep(c[1],L)
for(i in 1:k){
  z<-c(-1000,D[[i]],1000) #adds big end intervals for cut function so error isn't produced
  a<-cut(x,z,labels=F)
  q<-c(1,Q[[i]],0)
  q<-(x-z[a])*(q[a+1]-q[a])/(z[a+1]-z[a])+q[a]
  Y<-rnorm(L,mu,std)
  x<-(q*Y+(1-q)*(1+r))*x+c[i+1]
  x[x<0]<- -1 #to avoid errors
}
mean(x>=0)

#check portfolio that is all-in stock
c<-c(rep(.14,50),rep(-1,30)) 
k<-length(c)-1
L<-100000
x<-rep(c[1],L)
for(i in 1:k){
  q<-rep(1,L)
  Y<-rnorm(L,mu,std)
  x<-(q*Y+(1-q)*(1+r))*x+c[i+1]
  x[x<0]<- -1 #to avoid errors
}
mean(x>=0)


###################################
#extension to a mortality distribution (functions and set-up)
#assumes w_k=0
###################################
setwd("C:/Users/17753/Documents/DCAconditional")
lt<-read.xlsx(xlsxFile = 'lifeTablesF.xlsx', sheet = 1, skipEmptyRows = TRUE)
lt<-lt[lt$United.States.life.table.functions.and.actuarial.functions.at.2.3.percent.interest=='2017',]
dr<-lt$X3[as.numeric(lt$X2)>0] #death rates for ages 1+
dr<-as.numeric(dr[-1])

#pm2 gives probability that i-1<tau<=i given tau>i-1
pm2<-function(i,l){ #l is starting age
  return(dr[l+i-1])
} 
#plot female death rate
plot(1:length(dr),dr,xlab='age',main='2017 female death rate',ylab='',pch=20,ylim=c(0,1))
grid()
points(1:length(dr),dr,pch=20)

#builds \bar{v}_0 
buildVbar<-function(c,w,mu,std,M,r,l){#l is starting age; barORhat indicates whether to produce \bar{v}_0 or \hat{v}_0 
  k<-length(c)-1 
  G1<-seq(.01,.99,.01)
  D<-list()
  for(i in k:1){
    D[[i]]<-(1:(2*M))*w[i]/(M) #i here is i-1 in algorithm
  }
  V<-list() #stores v_i over i
  Q<-list() #stores q*_i over i
  #initialize i, v and q* 
  Q[[k]]<-c(rep(1,M-1),rep(0,M+1)) 
  V[[k]]<-1-pnorm(w[k]*(1+r)/D[[k]],mean=mu,sd=std) 
  V[[k]][M:(2*M)]<-1
  V[[k]]<-(1-pm2(k,l))*V[[k]]+pm2(k,l)*1 #this is \bar{v}_{k-1} in algorithm
  #build successive v and q*
  for(i in (k-1):1){#i here means i-1 in algorithm because 0 can't be an index
    oldv<-V[[i+1]]
    oldD<-D[[i+1]]
    cnt<-1 #indicates position in D
    v<-c() #v_i
    q<-c() #q*_i
    for(x in D[[i]]){
      #this loop finds q*_i(x) and \bar{v}_i(x) or \hat{v}_i(x) by trying different q*_i(x) and then picking the best
      #remember that i here is i-1 in algorithm
      #try q*_i(x)=0 first
      z<-which.min(abs(x-oldD)) #closest element in oldD to x
      q[cnt]<-0
      if(x>=w[i]-w[i]/(4*M)){#covers borderline >= cases
        v[cnt]<-1
        cnt<-cnt+1
        next
      } 
      temp<-(1+r)*x+c[i+1]
      if(temp<oldD[1]){ 
        v[cnt]<-pm2(i,l) #lower bound for q=0
      }else if(temp>= w[i+1]){
        v[cnt]<-1 #this is exact for q=0
      }else{
        y<-tail(which(temp-oldD>=0),1) #closest smaller element in oldD to temp
        v[cnt]<-(1-pm2(i,l))*oldv[y]+pm2(i,l) #approximate for q=0
      }
      #then try q in (0,1]
      temp<-unlist(lapply(G1,f,mu,std,r,x,oldD,oldv,c[i+1],w[i+1],M))
      q1<-G1[which.max(temp)]
      G2<-seq(q1-.001*9,q1+.001*9,.001)
      temp<-unlist(lapply(G2,f,mu,std,r,x,oldD,oldv,c[i+1],w[i+1],M))
      q2<-G2[which.max(temp)]
      G3<-seq(q2-.0001*9,q2+.0001*10,.0001)
      temp<-unlist(lapply(G3,f,mu,std,r,x,oldD,oldv,c[i+1],w[i+1],M))
      q3<-G3[which.max(temp)]
      temp<-(1-pm2(i,l))*max(temp)+pm2(i,l)
      #pick best q*_i(x) and \bar{v}_i(x) or \hat{v}_i(x)
      if(temp>v[cnt]){
        v[cnt]<-temp
        q[cnt]<-q3
      }
      cnt<-cnt+1
    }
    V[[i]]<-v
    Q[[i]]<-q
    #plot(D[[i]][1:M],V[[i]][1:M])
    print(i)
  }
  #V[[1]]<-V[[1]]-(1-pm(k,k,c,l)) #makes lower bound
  return(list(V,Q,D))
}

###################################
#extension to a mortality distribution 
#compare \bar{v}_0(x) with \bar{u}_0(x) having constant portfolio weights
###################################
l<-20 #start age of DCA
dca<-1 #number of years of dca before starting withdrawals
c<-c(rep(1,dca),rep(-1,length(dr)-l-dca+2)) #stores c_0 through c_k, but c_i=c[i+1] here; number of DCA investments + number of withdrawals + starting age 
k<-length(c)-1 #k=length(dr)-l+1; death rates (dr) are used from index l to length(dr), which gives k=length(dr)-l+1
r<-0
w<-c() #holds w_0,...,w_k 
w[k+1]<-0
for(i in k:1){
  w[i]<-(w[i+1]-c[i+1])/(1+r)
}
M<-100 
tempBAR<-buildVbar(c,w,mu,std,M,r,l)
V<-tempBAR[[1]]
Q<-tempBAR[[2]]
D<-tempBAR[[3]]

dev.new()
par(mfrow=c(1,1))
v<-V[[1]][1:M]
z<-D[[1]][1:M]
plot(z,v,type='l',xlab='x',ylab='',main=expression(bar(v)[0]*'(x) and various '*bar(u)[0]*'(x) for a starting age of 65'))
grid()
lines(z,v,lwd=2) #to cover grid marks
legend('bottomright',c(expression(bar(v)[0]*'(x) from Algorithm 4'),expression('simulated '*bar(v)[0]*'(x)'),expression(bar(u)[0]*'(x) for '*pi[i1]*' = 1'),expression(bar(u)[0]*'(x) for '*pi[i1]*' = .9'),expression(bar(u)[0]*'(x) for '*pi[i1]*' = .6')),lty=c(1,NA,NA,NA,NA),pch=c(NA,3,0,20,1),col=c(1,'red',1,1,1))
L<-100000 #number of realizations for simulation
#get realizations of tau
ptau<-c(0,1-cumprod(1-dr[l:length(dr)]),1) #cdf values for tau 
u<-runif(L)
g<-function(u){
  return(tail(which(u>ptau),1)-1)
}
tau<-unlist(lapply(u,g))+.5
#use Q to simulate v_0
u<-c()
X<-seq(k/25,k,k/25)
for(x in X){
  x<-rep(x,L)
  for(i in 1:k){
    q<-c(1,Q[[i]],0)
    z<-c(-1000,D[[i]],1000) #adds big end intervals for cut function so error isn't produced
    a<-cut(x,z,labels=F)
    q<-(x-z[a])*(q[a+1]-q[a])/(z[a+1]-z[a])+q[a]
    Y<-rnorm(L,mu,std)
    x<-((q*Y+(1-q)*(1+r))*x+c[i+1])*(tau>i)+x*(tau<=i)
    x[x<0]<- -1 #to avoid errors
  }
  u<-c(u,mean(x>=0))
}
points(X,u,pch=3,col='red')
#60-40 portfolio
u<-c()
for(x in X){
  x<-rep(x,L)
  for(i in 1:k){
    Y<-rnorm(L,mu,std)
    x<-((.6*Y+(1-.6)*(1+r))*x+c[i+1])*(tau>i)+x*(tau<=i)
  }
  u<-c(u,mean(x>=0))
}
points(X,u,pch=1)
#90-10 portfolio
u<-c()
for(x in X){
  x<-rep(x,L)
  for(i in 1:k){
    Y<-rnorm(L,mu,std)
    x<-((.9*Y+(1-.9)*(1+r))*x+c[i+1])*(tau>i)+x*(tau<=i)
    x[x<0]<- -1
  }
  u<-c(u,mean(x>=0))
}
points(X,u,pch=20)
#100-0 portfolio
u<-c()
for(x in X){
  x<-rep(x,L)
  for(i in 1:k){
    Y<-rnorm(L,mu,std)
    x<-((1*Y+(1-1)*(1+r))*x+c[i+1])*(tau>i)+x*(tau<=i)
    x[x<0]<- -1
  }
  u<-c(u,mean(x>=0))
}
points(X,u,pch=0)

###################################
#extension to a mortality distribution 
#check \bar{v}_0(c_0) for DCA then withdrawals 
###################################
#set parameters for buildV
l<-20 #start age of DCA
dca<-50 #number of years of dca before starting withdrawals
c<-c(rep(.1,dca),rep(-1,length(dr)-l-dca+2)) #stores c_0 through c_k, but c_i=c[i+1] here; number of DCA investments + number of withdrawals + starting age 
k<-length(c)-1 #k=length(dr)-l+1; death rates (dr) are used from index l to length(dr), which gives k=length(dr)-l+1
r<-0
w<-c() #holds w_0,...,w_k 
w[k+1]<-0
for(i in k:1){
  w[i]<-(w[i+1]-c[i+1])/(1+r)
}
M<-round(w[1]/c[1]) #produces a good final domain for \bar{v}_0 (first element of domain matches or almost matches dca amount) must make M an integer or won't work properly
if(M<100) M<-ceiling(100/M)*M #makes M large enough to get accurate results (at least 100)
#build V,Q,D
tempBAR<-buildVbar(c,w,mu,std,M,r,l)
V<-tempBAR[[1]]
Q<-tempBAR[[2]]
D<-tempBAR[[3]]

#check if \bar{v}_0(c_0)=.95
len<-sum(D[[1]]<10)
plot(D[[1]][1:len],V[[1]][1:len],ylim=c(.9,1))
lines(D[[1]][1:len],rep(.95,len))
lines(c(c[1],c[1]),c(0,1))
grid()

#check if \bar{v}_0(c_0)=.95 via simulation
L<-100000
ptau<-c(0,1-cumprod(1-dr[l:length(dr)]),1) #cdf values for tau 
u<-runif(L)
g<-function(u){
  return(tail(which(u>ptau),1)-1)
}
tau<-unlist(lapply(u,g)) 
x<-rep(c[1],L)
for(i in 1:k){
  q<-c(1,Q[[i]],0)
  z<-c(-1000,D[[i]],1000) #adds big end intervals for cut function so error isn't produced
  a<-cut(x,z,labels=F)
  q<-(x-z[a])*(q[a+1]-q[a])/(z[a+1]-z[a])+q[a]
  q<-rep(1,L) #to check all stock portfolio
  Y<-rnorm(L,mu,std)
  x<-((q*Y+(1-q)*(1+r))*x+c[i+1])*(tau>i)+x*(tau<=i)
  x[x<0]<- -1 #to avoid errors
}
print(mean(x>=0))

###################################
#extension to a mortality distribution 
#x such that \bar{v}_0(x)=.9,.95,.99 for different starting ages
###################################
l<-20 #start age of DCA
dca<-1 #number of years of dca before starting withdrawals
c<-c(rep(1,dca),rep(-1,length(dr)-l-dca+2)) #stores c_0 through c_k, but c_i=c[i+1] here; number of DCA investments + number of withdrawals + starting age 
k<-length(c)-1 #k=length(dr)-l+1; death rates (dr) are used from index l to length(dr), which gives k=length(dr)-l+1
r<-0
w<-c() #holds w_0,...,w_k 
w[k+1]<-0
for(i in k:1){
  w[i]<-(w[i+1]-c[i+1])/(1+r)
}
M<-300 
tempBAR<-buildVbar(c,w,mu,std,M,r,l)
V<-tempBAR[[1]]
Q<-tempBAR[[2]]
D<-tempBAR[[3]]

dev.new()
par(mfrow=c(1,1))
x1<-c()
x2<-c()
x3<-c()
for(s in 1:70){
  v<-V[[s]]
  z<-D[[s]]
  idx<-which(v>.9)[1]
  temp<-z[idx-1]+(z[idx]-z[idx-1])*(.9-v[idx-1])/(v[idx]-v[idx-1])
  x1<-c(x1,temp)
  idx<-which(v>.95)[1]
  temp<-z[idx-1]+(z[idx]-z[idx-1])*(.95-v[idx-1])/(v[idx]-v[idx-1])
  x2<-c(x2,temp)
  idx<-which(v>.99)[1]
  temp<-z[idx-1]+(z[idx]-z[idx-1])*(.99-v[idx-1])/(v[idx]-v[idx-1])
  x3<-c(x3,temp)
  #below are the unsmoothed versions
  #x1<-c(x1,z[which(v>.9)[1]])
  #x2<-c(x2,z[which(v>.95)[1]])
  #x3<-c(x3,z[which(v>.99)[1]])
}
plot(l:(l+70-1),x3,type='l',main=expression('Necessary and sufficient initial investments to withdraw\n       1 unit, annually, until death with confidence C'),col='black',ylim=c(0,45),lwd=2,lty=3,xlab='starting age',ylab=expression('min  x  s.t.  '*bar(v)[0]*'(x)'>='C'))
grid()
lines(l:(l+70-1),x2,col='black',lwd=2)
lines(l:(l+70-1),x1,col='black',lwd=2,lty=2)
legend('topright',c('C = .99','C = .95','C = .9'),lwd=c(2,2,2),lty=c(3,1,2),col=c('black','black','black'))

