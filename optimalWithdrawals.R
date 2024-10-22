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

#optional to check independence of subsets of data
k<-10 #number of years in subset
PV<-c() #holds p-values for ljung-box tests over subsets of the entire time period
for(j in 1:(length(s)-k)){
  PV<-c(PV,min(c(Box.test(s[j:(j+k-1)],lag=1,type='Ljung-Box')$p.value,
                 Box.test(s[j:(j+k-1)],lag=5,type='Ljung-Box')$p.value,
                 Box.test(ls[j:(j+k-1)],lag=1,type='Ljung-Box')$p.value,
                 Box.test(ls[j:(j+k-1)],lag=5,type='Ljung-Box')$p.value,
                 Box.test(abs(ls[j:(j+k-1)]),lag=1,type='Ljung-Box')$p.value,
                 Box.test(abs(ls[j:(j+k-1)]),lag=5,type='Ljung-Box')$p.value)))
}
plot(log10(PV))
lines(c(-10,200),c(-1.3,-1.3),lty=3)

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
  a<-(0-c+(1+r)*(q-1)*x)/(q*x)
  I<-(pnorm(a+w/(q*x),mean=mu,sd=std)-pnorm(a,mean=mu,sd=std))*sum(v*vdensity)/sum(vdensity)
  return(I+1-pnorm(a+w/(q*x),mean=mu,sd=std))
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
    D[[i]]<-(1:(M-1))*w[i]/(M) #for lower bound f 
  }
  V<-list() #stores v_i over i
  Q<-list() #stores q*_i over i
  #initialize i, v and q*
  Q[[k]]<-rep(1,M-1)
  V[[k]]<-1-pnorm(w[k]*(1+r)/D[[k]],mean=mu,sd=std) #V[[k]] here is v_{k-1} in algorithm
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

#buildU is like buildV but for constant weight portfolios
uf<-function(q,mu,std,r,x,D,v,c,w,M){
  vdensity<-dnorm((D-c+(1+r)*(q-1)*x)/(q*x),mean=mu,sd=std)
  if(sum(vdensity)<10^(-10)) return(0) #to avoid errors
  return((1-pnorm((0-c+(1+r)*(q-1)*x)/(q*x),mean=mu,sd=std))*sum(v*vdensity)/sum(vdensity))
}
#qs is proportion invested in stock
buildU<-function(c,w,mu,std,M,r,qs){
  k<-length(c)-1 
  D<-list()
  for(i in k:1){#i here means i-1 in algorithm because 0 can't be an index
    D[[i]]<-(1:(4*M))*w[i]/(M) #for lower bound f 
  }
  V<-list() #stores v_i over i
  #initialize i, v and q*
  V[[k]]<-1-pnorm((1+r)*(1+(w[k]/D[[k]]-1)/qs),mean=mu,sd=std) #V[[k]] here is v_{k-1} in algorithm
  #build successive v and q*
  for(i in (k-1):1){#i here means i-1 in algorithm because 0 can't be an index
    oldv<-V[[i+1]]
    oldD<-D[[i+1]]
    cnt<-1 #indicates position in D
    v<-c() #v_i
    for(x in D[[i]]){
      #remember that i here is i-1 in algorithm
      v[cnt]<-uf(qs,mu,std,r,x,oldD,oldv,c[i+1],w[i+1],M)
      cnt<-cnt+1
    }
    V[[i]]<-v #store v_i
  }
  for(i in 1:k){#truncate lists to have M-1 points
    D[[i]]<-D[[i]][1:(M-1)]
    V[[i]]<-V[[i]][1:(M-1)]
  }
  return(list(V,D))
}

#set parameters for buildV
k<-50 
c<-c(1,rep(-1,k)) #stores c_0 through c_k, but c_i=c[i+1] here
w<-c() #holds w_0,...,w_k 
w[k+1]<-0
r<-0.0
for(i in k:1){
  w[i]<-(w[i+1]-c[i+1])/(1+r)
}
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
#par(oma = c(4,1,1,1), mfrow = c(2, 2), mar = c(2, 2, 1, 1))
par(mfrow=c(2,2),mar=c(5.1-1, 4.1-1, 4.1+1.5, 2.1)) #top legend
par(mfrow=c(2,2),mar=c(5.1+1.5, 4.1-1, 4.1-1.5, 2.1)) #bottom legend
par(mfrow=c(2,2),mar=c(5.1+1, 4.1, 4.1, 2.1)) #center legend
#c(5.1, 4.1, 4.1, 2.1) default margins

for(k in c(20,30,40,50)){
  idx<-which((length(w)-1):0==k) #selects the index for V,Q,D corresponding to k
  v<-V[[idx]]
  q<-Q[[idx]]
  z<-D[[idx]]
  plot(z,rep(.95,length(z)),lty=3,type='l',xlab='x',ylim=c(0,1),ylab='',main=paste0('k = ',k))
  grid()
  lines(z,v,col='black',lwd=2)
  lines(z,q,col='black',lwd=2,lty=2)
}

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('center',legend = c(bquote('v '[0]^''*' ( x )'),bquote('q '[0]^'*'*' ( x )'), ".95 level line"), col = c("black","black", "black"),lty=c(1,2,3), lwd = c(1,1,1), xpd = TRUE, horiz = TRUE, cex = 1, seg.len=1, bty = 'o')

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
  idx<-which((length(w)-1):0==k) #selects the index for V,D corresponding to k
  v<-V[[idx]]
  z<-D[[idx]]
  idx2<-which(v>.9)[1]
  if(is.na(idx2)) x1<-c(x1,k)
  else x1<-c(x1,z[idx2-1]+(z[idx2]-z[idx2-1])*(.9-v[idx2-1])/(v[idx2]-v[idx2-1]))
  idx2<-which(v>.95)[1]
  if(is.na(idx2)) x2<-c(x2,k)
  else x2<-c(x2,z[idx2-1]+(z[idx2]-z[idx2-1])*(.95-v[idx2-1])/(v[idx2]-v[idx2-1]))
  idx2<-which(v>.99)[1]
  if(is.na(idx2)) x3<-c(x3,k)
  else x3<-c(x3,z[idx2-1]+(z[idx2]-z[idx2-1])*(.99-v[idx2-1])/(v[idx2]-v[idx2-1]))
  #upper bounds instead of linear interpolation
  #x1<-c(x1,z[which(v>.9)[1]])
  #x2<-c(x2,z[which(v>.95)[1]])
  #x3<-c(x3,z[which(v>.99)[1]])
}
plot(1:50,x3,type='l',main=expression('Necessary and sufficient initial investments to withdraw\n       1 unit, annually, for k years with confidence C'),col='black',xlim=c(0,50),ylim=c(0,45),lwd=2,lty=3,xlab='k',ylab='')#expression('min  x  s.t.  v '[0]*'(x)'>='C'))
grid()
lines(1:50,x2,col='black',lwd=2)
lines(1:50,x1,col='black',lwd=2,lty=2)
legend('topleft',c(expression('C = .99, constant '*pi[i1]),expression('C = .99, optimal '*pi[i1]),expression('C = .95, constant '*pi[i1]),expression('C = .95, optimal '*pi[i1]),expression('C = .9, constant '*pi[i1]),expression('C = .9, optimal '*pi[i1])),lwd=c(2,2,2,2,2,2),lty=c(3,3,1,1,2,2),col=c('red','black','red','black','red','black'))

#compare with constant portfolios
bigU<-list() #contains the probability of success for the best constant portfolios
for(i in 1:50){
  bigU[[i]]<-rep(0,M-1) #2*M
}
for(qs in seq(.01,1,.01)){
  tempU<-buildU(c,w,mu,std,M,r,qs)
  U<-tempU[[1]]
  for(i in 1:50){
    bigU[[i]]<-pmax(bigU[[i]],U[[i]])
  }
  print(qs)
}
y1<-c()
y2<-c()
y3<-c()
for(k in 1:50){
  idx<-which((length(w)-1):0==k) #selects the index for bigU,D corresponding to k
  v<-bigU[[idx]]
  z<-D[[idx]]
  idx2<-which(v>.9)[1]
  if(is.na(idx2)) y1<-c(y1,k)
  else y1<-c(y1,z[idx2-1]+(z[idx2]-z[idx2-1])*(.9-v[idx2-1])/(v[idx2]-v[idx2-1]))
  idx2<-which(v>.95)[1]
  if(is.na(idx2)) y2<-c(y2,k)
  else y2<-c(y2,z[idx2-1]+(z[idx2]-z[idx2-1])*(.95-v[idx2-1])/(v[idx2]-v[idx2-1]))
  idx2<-which(v>.99)[1]
  if(is.na(idx2)) y3<-c(y3,k)
  else y3<-c(y3,z[idx2-1]+(z[idx2]-z[idx2-1])*(.99-v[idx2-1])/(v[idx2]-v[idx2-1]))
}
lines(1:50,y3,type='l',col='red',lwd=2,lty=3)
grid()
lines(1:50,y2,col='red',lwd=2)
lines(1:50,y1,col='red',lwd=2,lty=2)

#for curiosity (not in manuscript) (relative benefit for given confidence levels of using optimal instead of constant portfolio weights)
plot(1:50,y3/x3-1,type='l',col='red',lwd=2,lty=3,ylim=c(0,.15))
grid()
lines(1:50,y2/x2-1,col='red',lwd=2)
lines(1:50,y1/x1-1,col='red',lwd=2,lty=2)

#new figure to compare confidence levels for same initial investment
par(mfrow=c(2,2),mar=c(5.1+1, 4.1, 4.1, 2.1))
for(k in c(5,10,20,40)){
  idx<-which((length(w)-1):0==k) #selects the index for V,bigU,D corresponding to k
  v<-V[[idx]]
  u<-bigU[[idx]]
  z<-D[[idx]]
  plot(z,v-u,type='l',ylab='',xlab='x',main=paste0('k = ',k),ylim=c(0,max(v-U[[idx]])))
  grid()
  lines(z,v-u,lwd=2)
  lines(z,v-U[[idx]],lty=2)
}

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('center',legend = c(bquote(min('','q')*'[ v '[0]^''*' ( x ) - u '[0]^'q'*' ( x ) ]     '),bquote('v '[0]^''*' ( x ) - u '[0]^'1'*' ( x )')), col = c("black","black"),lty=c(1,2), lwd = c(2,1), xpd = TRUE, horiz = F, cex = 1, seg.len=1, bty = 'o')

################################################
#error check for k=50
#also compares optimal portfolio with fixed weight portfolios
#uses above V, Q, D, mu, std, r, c, w
################################################
dev.new()
par(mfrow=c(1,1))
K<-length(c)-1
k<-50
idx<-which((length(w)-1):0==k) #selects the index for V,Q,D corresponding to k
v<-V[[idx]]
z<-D[[idx]]
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
    q<-c(1,1,Q[[i]],0,0)
    z<-c(-1000,0,D[[i]],w[i],1000) #adds big end intervals for cut function so error isn't produced
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

#new figure comparing end wealth for optimal vs all-in stock
par(mfrow=c(1,2))
K<-length(c)-1
k<-10
idx<-which((length(w)-1):0==k) #selects the index for V,Q,D corresponding to k
z<-D[[idx]]
#use Q to simulate v_0
L<-100000
x<-rep(8,L)
for(i in (K-k+1):K){
  q<-c(1,1,Q[[i]],0,0)
  z<-c(-1000,0,D[[i]],w[i],1000) #adds big end intervals for cut function so error isn't produced
  a<-cut(x,z,labels=F)
  q<-(x-z[a])*(q[a+1]-q[a])/(z[a+1]-z[a])+q[a]
  Y<-rnorm(L,mu,std)
  x<-(q*Y+(1-q)*(1+r))*x+c[i+1]
  x[x<0]<- -1 #to avoid errors
}
hist(x,probability=T,main=expression('optimal '*pi[i1]),breaks=seq(-1,max(x)+1,1),xlab='remaining wealth after 10 years')
x<-rep(8,L)
for(i in (K-k+1):K){
  Y<-rnorm(L,mu,std)
  sWT<-1
  x<-(sWT*Y+(1-sWT)*(1+r))*x+c[i+1]
  x[x<0]<- -1
}
hist(x,probability=T,main=expression(pi[i1]*' = 1 for all i'),xlim=c(0,25),breaks=seq(-1,max(x)+1,1),xlab='remaining wealth after 10 years')


#new figure comparing end wealth for (optimal/all-in stock) hybrid vs all-in stock
par(mfrow=c(1,2))
K<-length(c)-1
k<-12
idx<-which((length(w)-1):0==k) #selects the index for V,Q,D corresponding to k
z<-D[[idx]]
#use Q to simulate v_0
L<-100000
x<-rep(8,L)
hp<-.5 #hybrid proportion
for(i in (K-k+1):K){
  q<-c(1,1,Q[[i]],0,0)
  z<-c(-1000,0,D[[i]],w[i],1000) #adds big end intervals for cut function so error isn't produced
  a<-cut(x,z,labels=F)
  q<-(x-z[a])*(q[a+1]-q[a])/(z[a+1]-z[a])+q[a]
  q<-hp*q+(1-hp)*1
  Y<-rnorm(L,mu,std)
  x<-(q*Y+(1-q)*(1+r))*x+c[i+1]
  x[x<0]<- -1 #to avoid errors
}
hist(x,probability=T,main=expression('hybrid '*pi[i1]),breaks=seq(-1,max(x)+1,1),xlim=c(0,25),xlab='remaining wealth after 10 years')
x<-rep(8,L)
for(i in (K-k+1):K){
  Y<-rnorm(L,mu,std)
  sWT<-1
  x<-(sWT*Y+(1-sWT)*(1+r))*x+c[i+1]
  x[x<0]<- -1
}
hist(x,probability=T,main=expression(pi[i1]*' = 1 for all i'),xlim=c(0,25),breaks=seq(-1,max(x)+1,1),xlab='remaining wealth after 10 years')

#plot v_0 for different k (not in manuscript)
dev.new()
par(mfrow=c(1,1))
plot(0:50,rep(.95,51),lty=3,type='l',xlab='x',ylim=c(0,1),ylab='',main=paste0('k = ',k))
grid()
for(k in seq(1,50,1)){
  idx<-which((length(w)-1):0==k) #selects the index for V,D corresponding to k
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
M<-200
temp<-buildV(c,w,mu,std,M,r)
V<-temp[[1]]
Q<-temp[[2]]
D<-temp[[3]]
u2<-V[[which(K:0==40)]] #from earlier run of buildV
temp<-buildV(c,w,mu,std-.02,M,r)
V<-temp[[1]]
u1<-V[[which(K:0==40)]] 
temp<-buildV(c,w,mu,std+.02,M,r)
V<-temp[[1]]
u3<-V[[which(K:0==40)]] 

temp<-buildV(c,w,mu-.01,std,M,r)
V<-temp[[1]]
u0<-V[[which(K:0==40)]] 
temp<-buildV(c,w,mu+.01,std,M,r)
V<-temp[[1]]
u4<-V[[which(K:0==40)]] 

dev.new()
par(mfrow=c(1,2))
Z<-D[[which(K:0==40)]]

plot(Z,u2,type='l',lwd=2,xlab='x',ylab=expression('v'[0]*'(x)'),main=expression('v'[0]*'(x) for '*mu~'= 1.083 and various '*sigma~'(k = 40)'))
grid()
lines(Z,u1,lty=2,lwd=2)
lines(Z,u3,lty=3,lwd=2)
legend('bottomright',c(expression(sigma~'= .1553 (alg. 3)'),expression(sigma~'= .1553 (approx.)'),expression(sigma~'= .1753 (alg. 3)'),expression(sigma~'= .1953 (alg. 3'),expression(sigma~'= .1953 (approx.)')),lwd=c(2,1,2,2,1),lty=c(2,NA,1,3,NA),pch=c(NA,3,NA,NA,4),col=c('black','red','black','black','red'))
u<-c()
L<-100000
X<-seq(k/25,k,k/25)
for(x in X){
  x<-rep(x,L)
  for(i in (K-k+1):K){
    q<-c(1,1,Q[[i]],0,0)
    z<-c(-1000,0,D[[i]],w[i],1000) #adds big end intervals for cut function so error isn't produced
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
    q<-c(1,1,Q[[i]],0,0)
    z<-c(-1000,0,D[[i]],w[i],1000) #adds big end intervals for cut function so error isn't produced
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

plot(Z,u2,type='l',lwd=2,xlab='x',ylab=expression('v'[0]*'(x)'),main=expression('v'[0]*'(x) for '*sigma~'= .1753 and various '*mu~'(k = 40)'))
grid()
lines(Z,u0,lty=2,lwd=2)
lines(Z,u4,lty=3,lwd=2)
legend('bottomright',c(expression(mu~'= 1.073 (alg. 3)'),expression(mu~'= 1.073 (approx.)'),expression(mu~'= 1.083 (alg. 3)'),expression(mu~'= 1.093 (alg. 3)'),expression(mu~'= 1.093 (approx.)')),lwd=c(2,1,2,2,1),lty=c(2,NA,1,3,NA),pch=c(NA,3,NA,NA,4),col=c('black','red','black','black','red'))
u<-c()
L<-100000
X<-seq(k/25,k,k/25)
for(x in X){
  x<-rep(x,L)
  for(i in (K-k+1):K){
    q<-c(1,1,Q[[i]],0,0)
    z<-c(-1000,0,D[[i]],w[i],1000) #adds big end intervals for cut function so error isn't produced
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
    q<-c(1,1,Q[[i]],0,0)
    z<-c(-1000,0,D[[i]],w[i],1000) #adds big end intervals for cut function so error isn't produced
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
r<-0
w<-c()
w[k+1]<-0
for(i in k:1){
  w[i]<-(w[i+1]-c[i+1])/(1+r)
}
M<-round(w[1]/c[1]) #produces a good final domain for v_0 (first element of domain matches or almost matches dca amount) must make M an integer or won't work properly
if(M<100) M<-ceiling(100/M)*M #makes M large enough to get accurate results (at least 100)
temp<-buildV(c,w,mu,std,M,r)
V<-temp[[1]]
Q<-temp[[2]]
D<-temp[[3]]
i<-k-1
plot(D[[k-i]],V[[k-i]])
D[[k-i]][1]
#i<-k-5
#plot(D[[k-i]],Q[[k-i]])

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


#check optimal vs all-in stock for actual S&P returns
r<-.0
K<-length(c)-1
par(mfrow=c(2,2))
for(k in c(5,10,15,20)){
  X<-NULL
  strt<-82
  for(j in strt:(length(s)-k)){
    x<-seq(k/50,k-k/50,k/50)
    cnt<-0
    for(i in (K-k+1):K){
      q<-c(1,1,Q[[i]],0,0)
      z<-c(-1000,0,D[[i]],w[i],1000) #adds big end intervals for cut function so error isn't produced
      a<-cut(x,z,labels=F)
      q<-(x-z[a])*(q[a+1]-q[a])/(z[a+1]-z[a])+q[a]
      Y<-s[j+cnt]
      x<-(q*Y+(1-q)*(1+r))*x+c[i+1]
      x[x<0]<- -1 #to avoid errors
      cnt<-cnt+1
    }
    X<-rbind(X,x)
  }
  ZZ<-rep(0,49) #stores best Z
  for(q in seq(.0,1,.01)){
    Z<-NULL
    for(j in strt:(length(s)-k)){
      x<-seq(k/50,k-k/50,k/50)
      cnt<-0
      for(i in (K-k+1):K){
        #q<-1
        #q<-rep(1,length(x))
        #q[x>=(K-i+1)]<-0
        Y<-s[j+cnt]
        x<-(q*Y+(1-q)*(1+r))*x+c[i+1]
        x[x<0]<- -1 #to avoid errors
        cnt<-cnt+1
      }
      Z<-rbind(Z,x)
    }
    ZZ<-pmax(ZZ,colMeans(Z>=0))
  }
  plot(seq(k/50,k-k/50,k/50),colMeans(X>=0)-ZZ,ylim=c(0,max(colMeans(X>=0)-colMeans(Z>=0))),pch=19,xlab='x',ylab='',main=paste0('k = ',k))
  grid()
  points(seq(k/50,k-k/50,k/50),colMeans(X>=0)-ZZ,pch=19) #dif using best constant weight portfolio
  points(seq(k/50,k-k/50,k/50),colMeans(X>=0)-colMeans(Z>=0)) #all stock
}

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
    D[[i]]<-(1:(M-1))*w[i]/(M) #i here is i-1 in algorithm
  }
  V<-list() #stores v_i over i
  Q<-list() #stores q*_i over i
  #initialize i, v and q* 
  Q[[k]]<-rep(1,M-1) 
  V[[k]]<-1-pnorm(w[k]*(1+r)/D[[k]],mean=mu,sd=std) 
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
    print(i)
  }
  return(list(V,Q,D))
}

#builds \bar{u}_0 for constant portfolio have proportion qs in stock
buildUbar<-function(c,w,mu,std,M,r,l,qs){#l is starting age; barORhat indicates whether to produce \bar{v}_0 or \hat{v}_0 
  k<-length(c)-1 
  D<-list()
  for(i in k:1){
    D[[i]]<-(1:(4*M))*w[i]/(M) #i here is i-1 in algorithm
  }
  V<-list() #stores v_i over i
  #initialize i, v and q* 
  V[[k]]<-1-pnorm((1+r)*(1+(w[k]/D[[k]]-1)/qs),mean=mu,sd=std)
  V[[k]]<-(1-pm2(k,l))*V[[k]]+pm2(k,l)*1 #this is \bar{v}_{k-1} in algorithm
  #build successive v and q*
  for(i in (k-1):1){#i here means i-1 in algorithm because 0 can't be an index
    oldv<-V[[i+1]]
    oldD<-D[[i+1]]
    cnt<-1 #indicates position in D
    v<-c() #v_i
    for(x in D[[i]]){
      #remember that i here is i-1 in algorithm
      temp<-f(qs,mu,std,r,x,oldD,oldv,c[i+1],w[i+1],M)
      v[cnt]<-(1-pm2(i,l))*temp+pm2(i,l)
      cnt<-cnt+1
    }
    V[[i]]<-v
  }
  for(i in 1:k){#truncate lists to have M-1 points
    D[[i]]<-D[[i]][1:(M-1)]
    V[[i]]<-V[[i]][1:(M-1)]
  }
  return(list(V,D))
}

###################################
#extension to a mortality distribution 
#compare \bar{v}_0(x) with \bar{u}_0(x) having constant portfolio weights
###################################
l<-65 #start age of DCA
dca<-1 #number of years of dca before starting withdrawals
c<-c(rep(1,dca),rep(-1,length(dr)-l-dca+2)) #stores c_0 through c_k, but c_i=c[i+1] here; number of DCA investments + number of withdrawals + starting age 
k<-length(c)-1 #k=length(dr)-l+1; death rates (dr) are used from index l to length(dr), which gives k=length(dr)-l+1
r<-0.0
w<-c() #holds w_0,...,w_k 
w[k+1]<-0
for(i in k:1){
  w[i]<-(w[i+1]-c[i+1])/(1+r)
}
M<-200 
tempBAR<-buildVbar(c,w,mu,std,M,r,l)
V<-tempBAR[[1]]
Q<-tempBAR[[2]]
D<-tempBAR[[3]]

dev.new()
par(mfrow=c(1,1))
v<-V[[1]]
z<-D[[1]]
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
    q<-c(1,1,Q[[i]],0,0)
    z<-c(-1000,0,D[[i]],w[i],1000) #adds big end intervals for cut function so error isn't produced
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

#separate figure comparing terminal wealth for optimal vs all-in stock
z<-D[[1]]
L<-100000 #number of realizations for simulation
#get realizations of tau
ptau<-c(0,1-cumprod(1-dr[l:length(dr)]),1) #cdf values for tau 
u<-runif(L)
g<-function(u){
  return(tail(which(u>ptau),1)-1)
}
tau<-unlist(lapply(u,g))+.5
#use Q to simulate v_0
par(mfrow=c(1,2))
x<-rep(22,L)
for(i in 1:k){
  q<-c(1,1,Q[[i]],0,0)
  z<-c(-1000,0,D[[i]],w[i],1000) #adds big end intervals for cut function so error isn't produced
  a<-cut(x,z,labels=F)
  q<-(x-z[a])*(q[a+1]-q[a])/(z[a+1]-z[a])+q[a]
  Y<-rnorm(L,mu,std)
  x<-((q*Y+(1-q)*(1+r))*x+c[i+1])*(tau>i)+x*(tau<=i)
  x[x<0]<- -1 #to avoid errors
}
hist(x,probability=T,main=expression('optimal '*pi[i1]),breaks=seq(-1,max(x)+1,1),xlab='remaining wealth after death')
#100-0 portfolio
x<-rep(22,L)
for(i in 1:k){
  Y<-rnorm(L,mu,std)
  x<-((1*Y+(1-1)*(1+r))*x+c[i+1])*(tau>i)+x*(tau<=i)
  x[x<0]<- -1
}
hist(x,breaks=c(seq(-1,100,1),max(x)+1),xlim=c(0,100),probability=T,main=expression(pi[i1]*' = 1 for all i'),xlab='remaining wealth after death')


###################################
#extension to a mortality distribution 
#check \bar{v}_0(c_0) for DCA then withdrawals 
###################################
#set parameters for buildV
l<-20 #start age of DCA
dca<-1 #number of years of dca before starting withdrawals
c<-c(rep(.1,dca),rep(-1,length(dr)-l-dca+2)) #stores c_0 through c_k, but c_i=c[i+1] here; number of DCA investments + number of withdrawals + starting age 
k<-length(c)-1 #k=length(dr)-l+1; death rates (dr) are used from index l to length(dr), which gives k=length(dr)-l+1
r<-0.0
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
  q<-c(1,1,Q[[i]],0,0)
  z<-c(-1000,0,D[[i]],w[i],1000) #adds big end intervals for cut function so error isn't produced
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
r<-0.0
w<-c() #holds w_0,...,w_k 
w[k+1]<-0
for(i in k:1){
  w[i]<-(w[i+1]-c[i+1])/(1+r)
}
M<-200 
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
  if(is.na(idx)) x1<-c(x1,w[s])
  else{
    temp<-z[idx-1]+(z[idx]-z[idx-1])*(.9-v[idx-1])/(v[idx]-v[idx-1])
    x1<-c(x1,temp)
  } 
  idx<-which(v>.95)[1]
  if(is.na(idx)) x2<-c(x2,w[s])
  else{
    temp<-z[idx-1]+(z[idx]-z[idx-1])*(.95-v[idx-1])/(v[idx]-v[idx-1])
    x2<-c(x2,temp)
  } 
  idx<-which(v>.99)[1]
  if(is.na(idx)) x3<-c(x3,w[s])
  else{
    temp<-z[idx-1]+(z[idx]-z[idx-1])*(.99-v[idx-1])/(v[idx]-v[idx-1])
    x3<-c(x3,temp)
  } 
  #below are the unsmoothed versions
  #x1<-c(x1,z[which(v>.9)[1]])
  #x2<-c(x2,z[which(v>.95)[1]])
  #x3<-c(x3,z[which(v>.99)[1]])
}
plot(l:(l+70-1),x3,type='l',main=expression('Necessary and sufficient initial investments to withdraw\n       1 unit, annually, until death with confidence C'),col='black',ylim=c(0,55),lwd=2,lty=3,xlab='starting age',ylab='')#expression('min  x  s.t.  '*bar(v)[0]*'(x)'>='C'))
grid()
lines(l:(l+70-1),x2,col='black',lwd=2)
lines(l:(l+70-1),x1,col='black',lwd=2,lty=2)
legend('topright',c(expression('C = .99, constant '*pi[i1]),expression('C = .99, optimal '*pi[i1]),expression('C = .95, constant '*pi[i1]),expression('C = .95, optimal '*pi[i1]),expression('C = .9, constant '*pi[i1]),expression('C = .9, optimal '*pi[i1])),lwd=c(2,2,2,2,2,2),lty=c(3,3,1,1,2,2),col=c('red','black','red','black','red','black'))


#compare with constant portfolios
bigU<-list()
for(sa in 1:70){
  bigU[[sa]]<-rep(0,M-1)
}
for(qs in seq(.01,1,.01)){
  tempU<-buildUbar(c,w,mu,std,M,r,l,qs)
  U<-tempU[[1]]
  for(sa in 1:70){
    bigU[[sa]]<-pmax(bigU[[sa]],U[[sa]])
  }
  print(qs)
}
x1<-c()
x2<-c()
x3<-c()
for(sa in 1:70){
  v<-bigU[[sa]]
  z<-D[[sa]]
  idx<-which(v>.9)[1]
  if(is.na(idx)) x1<-c(x1,w[sa])
  else{
    temp<-z[idx-1]+(z[idx]-z[idx-1])*(.9-v[idx-1])/(v[idx]-v[idx-1])
    x1<-c(x1,temp)
  } 
  idx<-which(v>.95)[1]
  if(is.na(idx)) x2<-c(x2,w[sa])
  else{
    temp<-z[idx-1]+(z[idx]-z[idx-1])*(.95-v[idx-1])/(v[idx]-v[idx-1])
    x2<-c(x2,temp)
  } 
  idx<-which(v>.99)[1]
  if(is.na(idx)) x3<-c(x3,w[sa])
  else{
    temp<-z[idx-1]+(z[idx]-z[idx-1])*(.99-v[idx-1])/(v[idx]-v[idx-1])
    x3<-c(x3,temp)
  } 
}
lines(l:(l+70-1),x3,type='l',col='red',lwd=2,lty=3)
lines(l:(l+70-1),x2,col='red',lwd=2)
lines(l:(l+70-1),x1,col='red',lwd=2,lty=2)

#new figure to compare confidence levels for same initial investment
#par(mfrow=c(2,2))
par(mfrow=c(2,2),mar=c(5.1+1, 4.1, 4.1, 2.1))
for(sa in c(1,21,36,51)){
  v<-V[[sa]]
  u<-bigU[[sa]]
  z<-D[[sa]]
  plot(z,v-u,type='l',ylab='',xlab='x',main=paste0('starting age = ',l+sa-1),ylim=c(0,max(v-U[[sa]])))
  grid()
  lines(z,v-u,lwd=2)
  lines(z,v-U[[sa]],lty=2)
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('center',legend = c(bquote(min('','q')*'[ '*bar('v')*' '[0]^''*' ( x ) - '*bar('u')*' '[0]^'q'*' ( x ) ]     '),bquote(bar('v')*' '[0]^''*' ( x ) - '*bar('u')*' '[0]^'1'*' ( x )')), col = c("black","black"),lty=c(1,2), lwd = c(2,1), xpd = TRUE, horiz = F, cex = 1, seg.len=1, bty = 'o')


par(mfrow=c(2,2))
par(mfrow=c(2,2),mar=c(5.1+1, 4.1, 4.1, 2.1))
for(sa in c(1,21,61,66)){
  v<-V[[sa]]
  u<-bigU[[sa]]
  z<-D[[sa]]
  plot(z,v-u,type='l',ylab='',xlab='x',main=paste0('starting age = ',l+sa-1))
  grid()
  lines(z,v-u,lwd=2)
}



#check optimal vs all-in stock for actual S&P returns (with mortality)
#build v with l=20
par(mfrow=c(2,2),mar=c(5.1+.5, 4.1, 4.1+1, 2.1))
r<-.0
U<-buildUbar(c,w,mu,std,M,r,20,.6)[[1]]
rr<-(1+d$'1YTR'/100)/d$Inflation-1
strt<-82 #starting year index
par(mfrow=c(2,2))
for(l in seq(65,80,5)){
  #build mortality variable tau
  ptau<-c(0,1-cumprod(1-dr[l:length(dr)]),1) #cdf values for tau 
  MT<-which(ptau>.99)[1]
  #optimal
  X<-c()
  for(j in strt:(length(s)-MT)){
    x<-D[[l-20+1]][which.max(V[[l-20+1]]-U[[l-20+1]])]#mean(tau)
    cnt<-0
    for(i in (l-20+1):(l-20+1+MT)){
      q<-c(1,1,Q[[i]],0,0)
      z<-c(-1000,0,D[[i]],w[i],1000) #adds big end intervals for cut function so error isn't produced
      a<-cut(x,z,labels=F)
      q<-(x-z[a])*(q[a+1]-q[a])/(z[a+1]-z[a])+q[a]
      Y<-s[j+cnt]
      x<-((q*Y+(1-q)*(1+r))*x+c[i+1])
      #x<-((q*Y+(1-q)*(1+rr[j+cnt]))*x+c[i+1])
      cnt<-cnt+1
      if(x<0) break
    }
    X<-c(X,ptau[cnt+1])
  }
  #constant
  ZZ<-NULL
  for(q in c(0,.6,1)){#seq(.0,1,.01)){
    Z<-c()
    for(j in strt:(length(s)-MT)){
      x<-D[[l-20+1]][which.max(V[[l-20+1]]-U[[l-20+1]])]#mean(tau)
      cnt<-0
      for(i in (l-20+1):(l-20+1+MT)){
        Y<-s[j+cnt]
        x<-((q*Y+(1-q)*(1+r))*x+c[i+1])
        #x<-((q*Y+(1-q)*(1+rr[j+cnt]))*x+c[i+1])
        cnt<-cnt+1
        if(x<0) break
      }
      Z<-c(Z,ptau[cnt+1])
    }
    ZZ<-rbind(ZZ,Z)
  }
  xax<-d$Year[strt:(length(s)-MT)]
  plot(xax,X,pch=3,ylim=c(min(c(X,ZZ[1,],ZZ[2,],ZZ[3,])),max(c(X,ZZ[1,],ZZ[2,],ZZ[3,]))),xlab='starting year',ylab='',main=bquote('starting age = '*.(l)*', c'[0]*' = '~.(round(D[[l-20+1]][which.max(V[[l-20+1]]-U[[l-20+1]])],1))))#paste0('starting age = ',l))
  grid()
  points(xax,X,pch=3)
  points(xax,ZZ[1,],pch=20) #dif using best constant weight portfolio
  points(xax,ZZ[2,],pch=0)
  points(xax,ZZ[3,],pch='O')
  print(round(c(mean(X),rowMeans(ZZ)),3))
}
#par(mfrow=c(1,1),mar=c(5.1+1, 4.1, 4.1, 2.1),new=T)
par(fig = c(0, 1, 0, 1), oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
plot(0,0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('center',c(expression('optimal '*pi['i']),expression(pi['i']*' = (0,1)'),expression(pi['i']*' = (.6,.4)'),expression('O  '*pi['i']*' = (1,0)')),xpd = TRUE, horiz = T, seg.len=1, bty = 'o',pch=c(3,20,0,NA))
#legend('center',c(expression(bar('u')*' '[0]^'*'*' ( c'[0]*' )'),expression(bar('u')*' '[0]^'0'*' ( c'[0]*' )'),expression(bar('u')*' '[0]^'.6'*' ( c'[0]*' )'),expression(bar('u')*' '[0]^'1'*' ( c'[0]*' )')),pch=c(3,20,0,1),horiz = T)
mtext("Probability to complete annual withdrawals of 1 unit until death",                   # Add main title
      side = 3,
      line = - 1.2,
      outer = TRUE,cex=1.2)

#plot bond rates
par(mfrow=c(1,1))
plot(d$Year[82:length(rr)],rr[82:length(rr)],main='6-month bond rates',xlab='year',ylab='',pch=19)
grid()
points(d$Year[82:length(rr)],rr[82:length(rr)],pch=19)