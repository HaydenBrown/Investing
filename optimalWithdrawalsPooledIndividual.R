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
acfs<-acf(s,plot=F)
plot(acfs[1:21],main='S&P returns ACF')
X<-seq(.01,.99,.01)
plot(qnorm(X,mean=mean(s),sd=sd(s)),quantile(s,probs=X,type=7),ylab='return quantiles',main='S&P returns QQ plot',xlab=expression('N(1.083, .1753'^2*') quantiles'))
lines(seq(-1,2,.01),seq(-1,2,.01))


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
#portfolios consist of cash and the normal r.v.
#(1-q) in cash and q in the r.v.
#this ghat helps make the v_i produced by buildV 
ghat<-function(q,mu,std,r,x,D,v,a,w,m,M,d){ #handles q\neq0
  temp<-0
  for(l in 0:(a-1)){
    b<-1+r-(1+r)/q+((a-l)*w)/(q*x)
    if(m[a-l]==0){
      temp<-temp+choose(a-1,l)*(d^l)*((1-d)^(a-1-l))*(1-pnorm(b,mean=mu,sd=std))
      next
    }
    if((pnorm(b+m[a-l]/(q*x),mean=mu,sd=std)-pnorm(b,mean=mu,sd=std))<10^(-10)){
      temp<-temp+choose(a-1,l)*(d^l)*((1-d)^(a-1-l))*(1-pnorm(b+m[a-l]/(q*x),mean=mu,sd=std)) #to avoid errors
      next
    } 
    fs<-dnorm(b+D[[a-l]]/(q*x),mean=mu,sd=std) #density values
    temp<-temp+choose(a-1,l)*(d^l)*((1-d)^(a-1-l))*((pnorm(b+m[a-l]/(q*x),mean=mu,sd=std)-pnorm(b,mean=mu,sd=std))*sum(v[[a-l]]*fs)/sum(fs)+1-pnorm(b+m[a-l]/(q*x),mean=mu,sd=std))
  }
  return(temp)
}
g0hat<-function(mu,std,r,x,D,v,a,w,m,M,d){ #handles q=0
  temp<-0
  for(l in 0:(a-1)){
    theta<-(1+r)*x-(a-l)*w
    if(m[a-l]==0){
      if(theta>=0){
        temp<-temp+choose(a-1,l)*(d^l)*((1-d)^(a-1-l))*1
      }
      next
    }
    if(theta<D[[a-l]][1]){
      vapprox<-0
    }else if(theta>=m[a-l]){
      vapprox<-1
    }else{
      idx<-sum(D[[a-l]]<=theta)
      vapprox<-v[[a-l]][idx]
    }
    temp<-temp+choose(a-1,l)*(d^l)*((1-d)^(a-1-l))*vapprox
  }
  return(temp)
}

setwd("C:/Users/17753/Documents/DCAconditional")
lt<-read.xlsx(xlsxFile = 'lifeTablesF.xlsx', sheet = 1, skipEmptyRows = TRUE)
lt<-lt[lt$United.States.life.table.functions.and.actuarial.functions.at.2.3.percent.interest=='2017',]
dr<-lt$X3[as.numeric(lt$X2)>0] #death rates for ages 1+
dr<-as.numeric(dr[-1])

#implements algorithm 1
#w is vector of withdrawals
#mu,std is mean,standard deviation of returns
#sa is starting age of annuitants
#other parameters are specified in algorithm
buildVhat<-function(w,m,mu,std,M,r,sa){
  k<-length(w)
  G1<-seq(.1,.9,.1)
  D<-list() #domain for first variable of the v_i(x,a)
  for(i in k:1){#i here means i-1 in algorithm because 0 can't be an index
    D[[i]]<-list()
    for(a in 1:A0){
      D[[i]][[a]]<-(1:(M-1))*m[[i]][a]/(M)
    }
  }
  V<-list() #stores v_i over i (again, i here means i-1 in algorithm)
  Q<-list() #stores q*_i over i
  #initialize i, v and q*
  D[[k+1]]<-list()
  Q[[k+1]]<-list() 
  V[[k+1]]<-list() #V[[k]] here is v_{k-1} in algorithm
  for(a in 1:A0){
    D[[k+1]][[a]]<-c(0)
    Q[[k+1]][[a]]<-c(1)#rep(1,M-1) 
    V[[k+1]][[a]]<-c(1)#unlist(lapply(D[[k]][[a]],gkm1,mu,std,r,a,m[[k]],M,dr[sa+k-1]))
  }
  #build successive v and q*
  for(i in k:1){#i here means i-1 in algorithm because 0 can't be an index
    V[[i]]<-list()
    Q[[i]]<-list()
    for(a in 1:A0){
      oldv<-V[[i+1]]
      oldD<-D[[i+1]]
      cnt<-1 #indicates position in D
      v<-c() #v_i
      q<-c() #q*_i
      for(x in D[[i]][[a]]){
        #this loop finds q*_i(x) and v_i(x) by trying different q*_i(x) and then picking the best
        #remember that i here is i-1 in algorithm
        #try q*_i(x)=0 first
        q[cnt]<-0
        v[cnt]<-(1-dr[sa+i-1])*g0hat(mu,std,r,x,oldD,oldv,a,w[i],m[[i+1]],M,dr[sa+i-1])+dr[sa+i-1]
        #try q*_i(x) in (0,1] next
        temp<-unlist(lapply(G1,ghat,mu,std,r,x,oldD,oldv,a,w[i],m[[i+1]],M,dr[sa+i-1]))
        q1<-G1[which.max(temp)]
        G3<-seq(q1-.01*9,q1+.01*10,.01)
        temp<-unlist(lapply(G3,ghat,mu,std,r,x,oldD,oldv,a,w[i],m[[i+1]],M,dr[sa+i-1]))
        q3<-G3[which.max(temp)]
        temp<-(1-dr[sa+i-1])*max(temp[!is.na(temp)])+dr[sa+i-1]
        #pick best q*_i(x) and v_i(x); remember that i here is i-1 in algorithm
        if(temp>v[cnt]){
          v[cnt]<-temp
          q[cnt]<-q3
        }
        cnt<-cnt+1
      }
      V[[i]][[a]]<-v #store v_i
      Q[[i]][[a]]<-q #store q*_i
    }
    print(i)
  }
  return(list(V,Q,D))
}

#set parameters for buildV
sa<-65
k<-length(dr)-sa+1
w<-rep(1,k) #stores schedule of withdrawals; w[i] here is w_i in manuscript
A0<-100
r<-0
m<-list() #m[[i+1]][a] here is m_{a,i} in manuscript
m[[k+1]]<-rep(0,A0) 
for(i in k:1){
  m[[i]]<-(m[[i+1]]+(1:A0)*w[i])/(1+r)
}
M<-100
mu<-mean(s)
std<-sd(s)

#compute V,Q,D (use this for much of the subsequent code)
temphat<-buildVhat(w,m,mu,std,M,r,sa) 
V<-temphat[[1]]
Q<-temphat[[2]]
D<-temphat[[3]]

#for curiosity
par(mfrow=c(1,1))
idx<-1
a<-1
a2<-50
plot(D[[idx]][[a]],V[[idx]][[a]],type='l',ylim=c(0,1))
#plot(D[[idx]][[a]],Q[[idx]][[a]])
lines(D[[idx]][[a2]]/a2,V[[idx]][[a2]],col='red')

##########################################
#plot max[v_0(ax,a)-v_0((a-1)x,a-1)] over x for different a
##########################################
dev.new()
par(mfrow=c(1,1))
idx<-1
maxDif<-c()
for(i in 2:A0){
  maxDif<-c(maxDif,max(V[[idx]][[i]]-V[[idx]][[i-1]])) #which.max is 26,27,28 or 29
  #maxDif<-c(maxDif,V[[idx]][[i]][30]-V[[idx]][[i-1]][30]) #just for curiosity
}
plot(log10(2:A0),log10(maxDif),xlab=expression('log'[10]*'(a)'),ylab='',main=expression('Max increase in v'[0]*'(ax,a) for each increase in a'))#expression('log'[10]*'(max'[' x>0 ']*'[v'[0]*'(ax,a) - v'[0]*'((a-1)x,a-1)])')
LM<-lm(log10(maxDif)~log10(2:A0)) #fits via least squares
lines(log10(2:A0),LM$fitted.values,lwd=2)
print(D[[idx]][[i]][30]/i)
print(V[[idx]][[i]][30])

#############################################
#plot max[v_0(ax,a)-v_0(x,1)] over x for different a
#############################################
dev.new()
par(mfrow=c(1,1))
idx<-1
maxDif<-c()
for(i in 2:A0){
  maxDif<-c(maxDif,max(V[[idx]][[i]]-V[[idx]][[1]])) #max happens at index 29 most of the time
  #maxDif<-c(maxDif,V[[idx]][[i]][30]-V[[idx]][[1]][30]) #for curiosity
}
plot((2:A0),(maxDif),xlab=expression('a'),ylab='',main=expression('Max difference between v'[0]*'(ax,a) and v'[0]*'(x,1)'))#expression('log'[10]*'(max'[' x>0 ']*'[v'[0]*'(ax,a) - v'[0]*'((a-1)x,a-1)])')
grid()
#lines(2:A0,.1-.123/sqrt(2:A0))

#plot((2:A0),(.17-maxDif)*(2:A0)^(1/2),xlab=expression('a'),ylab='',main=expression('Asymptotics of v'[0]*'(ax,a) - v'[0]*'(x,1)'))
#lines(2:A0,rep(.123,A0-1),lty=2)
#grid()

##########################################
#compare v_0(30x,30) from algorithm with simulated version and probability of success for other common portfolios
##########################################
sa<-65
k<-length(dr)-sa+1
w<-rep(1,k) #stores schedule of withdrawals; w[i] here is w_i in manuscript
A0<-100
r<-0
m<-list() #m[[i+1]][a] here is m_{a,i} in manuscript
m[[k+1]]<-rep(0,A0) 
for(i in k:1){
  m[[i]]<-(m[[i+1]]+(1:A0)*w[i])/(1+r)
}
M<-100
mu<-mean(s)
std<-sd(s)
#temp2<-buildV(w,m,mu,std,M,r,sa)
#V<-temp2[[1]]
#Q<-temp2[[2]]
#D<-temp2[[3]]
#a<-30
A0<-30
idx<-1
plot(D[[idx]][[A0]]/A0,V[[idx]][[A0]],xlim=c(0,20),type='l',ylim=c(0,1),lwd=2,xlab='P',ylab='',main=expression('Withdrawal success probabilities for A'[0]*' = 30 and various portfolios'))
grid() 
lines(c(0,D[[idx]][[A0]]/A0),c(0,V[[idx]][[A0]])) #so grid doesn't cover curve
legend('topleft',c(expression('v'[0]*'(30P,30)'),expression('optimal '*pi[i]),expression(pi[i]*' = (1,0)'),expression(pi[i]*' = (.9,.1)'),expression(pi[i]*' = (.6,.4)')),lty=c(1,NA,NA,NA,NA),pch=c(NA,3,0,20,1),col=c(1,'red',1,1,1))
#all stock
u<-c()
L<-10000
WW<-A0*seq(20/10,20,20/10) 
for(W in WW){
  print(which(WW==W)/length(WW))
  W<-rep(W,L)
  A<-rep(A0,L)
  I<-rep(1,L)
  for(i in 1:k){
    Y<-rnorm(L,mu,std)
    dI<-unlist(lapply(I,rbinom,n=1,prob=dr[i+sa-1]))
    I<-I-dI
    B<-unlist(lapply(A-1,rbinom,n=1,prob=dr[i+sa-1]))
    B[is.na(B)]<-0 #handles cases where A=0
    A<-A-B-dI
    W<-W*Y-A*I*w[i]
    W[W<0]<- -1
  }
  u<-c(u,mean(W>=0))
}
points(WW/A0,u,pch=0)
#90-10 stock-bond
u<-c()
for(W in WW){
  print(which(WW==W)/length(WW))
  W<-rep(W,L)
  A<-rep(A0,L)
  I<-rep(1,L)
  for(i in 1:k){
    Y<-rnorm(L,mu,std)
    dI<-unlist(lapply(I,rbinom,n=1,prob=dr[i+sa-1]))
    I<-I-dI
    B<-unlist(lapply(A-1,rbinom,n=1,prob=dr[i+sa-1]))
    B[is.na(B)]<-0 #handles cases where A=0
    A<-A-B-dI
    W<-W*(.9*Y+.1*(1+r))-A*I*w[i]
    W[W<0]<- -1
  }
  u<-c(u,mean(W>=0))
}
points(WW/A0,u,pch=20)
#60-40 stock-bond
u<-c()
for(W in WW){
  print(which(WW==W)/length(WW))
  W<-rep(W,L)
  A<-rep(A0,L)
  I<-rep(1,L)
  for(i in 1:k){
    Y<-rnorm(L,mu,std)
    dI<-unlist(lapply(I,rbinom,n=1,prob=dr[i+sa-1]))
    I<-I-dI
    B<-unlist(lapply(A-1,rbinom,n=1,prob=dr[i+sa-1]))
    B[is.na(B)]<-0 #handles cases where A=0
    A<-A-B-dI
    W<-W*(.6*Y+.4*(1+r))-A*I*w[i]
    W[W<0]<- -1
  }
  u<-c(u,mean(W>=0))
}
points(WW/A0,u,pch=1)
#use optimal portfolio weights to similate v_0(30x,30)
u<-c()
for(W in WW){
  print(which(WW==W)/length(WW))
  W<-rep(W,L)
  A<-rep(A0,L)
  I<-rep(1,L)
  for(i in 1:k){
    uA<-unique(A[A!=0])
    q<-rep(0,L)
    for(a in uA){
      aq<-c(1,Q[[i]][[a]],0)
      z<-c(-10,D[[i]][[a]],10^10) #adds big end intervals for cut function so error isn't produced
      aW<-W[A==a]
      iz<-cut(aW,z,labels=F)
      q[A==a]<-(aW-z[iz])*(aq[iz+1]-aq[iz])/(z[iz+1]-z[iz])+aq[iz]
    }
    Y<-rnorm(L,mu,std)
    Y[Y<0]<-0
    dI<-unlist(lapply(I,rbinom,n=1,prob=dr[i+sa-1]))
    I<-I-dI
    B<-unlist(lapply(A-1,rbinom,n=1,prob=dr[i+sa-1]))
    B[is.na(B)]<-0 #handles cases where A=0
    A<-A-B-dI
    W<-W*(q*Y+(1-q)*(1+r))-A*I*w[i]
    W[W<0]<- -1
  }
  u<-c(u,mean(W>=0))
}
points(WW/A0,u,pch=3,col='red')

##########################################
#plot v_0(ax,a) over x for different a
##########################################
dev.new()
par(mfrow=c(1,1))
idx<-1
a1<-1
a2<-2
a3<-30
plot(D[[idx]][[a1]],V[[idx]][[a1]],xlim=c(0,30),type='l',ylim=c(0,1),lwd=2,xlab='x',ylab=expression('v'[0]*'(ax,a)'),main=expression('v'[0]*'(ax,a) for various a'))
grid()
lines(D[[idx]][[a2]]/a2,V[[idx]][[a2]],lty=2,lwd=2)
lines(D[[idx]][[a3]]/a3,V[[idx]][[a3]],lty=3,lwd=2)
legend('topleft',c(paste0('a = ',a3),paste0('a = ',a2),paste0('a = ',a1)),lwd=c(2,2,2),lty=c(3,2,1),col=c('black','black','black'))

##########################################
#plot v_0(ax,a) over a for different x
##########################################
dev.new()
par(mfrow=c(1,1))
plot(NULL,ylim=c(.8,1),xlim=c(1,30),main=expression('v'[0]*'(ax,a) for various x'),ylab=expression('v'[0]*'(ax,a)'),xlab='a')
grid()
idx<-1
xs<-c(15,18,21)
cnt<-1
for(x in xs){
  xidx<-which.min(abs(x-D[[idx]][[1]]))
  y<-c()
  for(a in 1:30){
    y<-c(y,V[[idx]][[a]][xidx])
  }
  lines(1:30,y,lwd=2,lty=cnt)
  cnt<-cnt+1
}
legend('bottomright',c(paste0('x = ',xs[3]),paste0('x = ',xs[2]),paste0('x = ',xs[1])),lwd=c(2,2,2),lty=c(3,2,1),col=c('black','black','black'))


############################################
#min x s.t. v_0(ax,x)>=C (plotted over a) for various starting ages
############################################
dev.new()
par(mfrow=c(2,2))
for(sa in c(65,70,75,80)){
  plot(NULL,main=paste0('starting age = ',sa),ylim=c(8,28),xlim=c(1,a),xlab='a',ylab=expression('min  x  s.t.  v'[0]*'(ax,a)'>='C'))
  grid()
  i<-sa-65+1
  x1<-c()
  x2<-c()
  x3<-c()
  for(a in 1:30){
    v<-V[[i]][[a]]
    z<-D[[i]][[1]]
    idx<-which(v>.9)[1]
    tempx<-z[idx-1]+(z[idx]-z[idx-1])*(.9-v[idx-1])/(v[idx]-v[idx-1])
    x1<-c(x1,tempx)
    idx<-which(v>.95)[1]
    tempx<-z[idx-1]+(z[idx]-z[idx-1])*(.95-v[idx-1])/(v[idx]-v[idx-1])
    x2<-c(x2,tempx)
    idx<-which(v>.99)[1]
    tempx<-z[idx-1]+(z[idx]-z[idx-1])*(.99-v[idx-1])/(v[idx]-v[idx-1])
    x3<-c(x3,tempx)
  }
  lines(1:a,x3,col='black',lwd=2,lty=3)
  lines(1:a,x2,col='black',lwd=2)
  lines(1:a,x1,col='black',lwd=2,lty=2)
  legend('topright',c('C = .99','C = .95','C = .9'),lwd=c(2,2,2),lty=c(3,1,2),col=c('black','black','black'))
}

#plot(l:(l+70-1),x3,type='l',main=expression('Necessary and sufficient initial investments to withdraw\n       1 unit, annually, until death with confidence C'),col='black',ylim=c(0,45),lwd=2,lty=3,xlab='starting age',ylab=expression('min  x  s.t.  '*bar(v)[0]*'(x)'>='C'))


############################################
#min x s.t. v_0(ax,x)>=.95 (plotted over a for different starting ages) (plotted over starting age for different a)
############################################
dev.new()
par(mfrow=c(1,2))
plot(NULL,main=bquote(atop('min  x  s.t.  v'[0]*'(ax,a)'>='.95','for various starting ages')),ylim=c(8,22),xlim=c(1,30),xlab='a',ylab=expression('min  x  s.t.  v '[0]*'(ax,a)'>='.95'))
grid()
cnt<-1
for(sa in c(65,70,75,80)){
  i<-sa-65+1
  x1<-c()
  for(a in 1:30){
    v<-V[[i]][[a]]
    z<-D[[i]][[1]]
    idx<-which(v>.95)[1]
    tempx<-z[idx-1]+(z[idx]-z[idx-1])*(.95-v[idx-1])/(v[idx]-v[idx-1])
    x1<-c(x1,tempx)
  }
  lines(1:30,x1,lty=cnt,lwd=2)
  cnt<-cnt+1
}
legend('topright',c('s = 65','s = 70','s = 75','s = 80'),lwd=c(2,2,2),lty=1:4,col=c('black','black','black'))

plot(NULL,main=bquote(atop('min  x  s.t.  v'[0]*'(ax,a)'>='.95','for various a')),ylim=c(8,22),xlim=c(65,80),xlab='s',ylab=expression('min  x  s.t.  v '[0]*'(ax,a)'>='.95'))
grid()
cnt<-1
for(a in c(1,2,30,100)){
  x1<-c()
  for(sa in 65:80){
    v<-V[[sa-65+1]][[a]]
    z<-D[[sa-65+1]][[1]]
    idx<-which(v>.95)[1]
    tempx<-z[idx-1]+(z[idx]-z[idx-1])*(.95-v[idx-1])/(v[idx]-v[idx-1])
    x1<-c(x1,tempx)
  }
  lines(65:80,x1,lty=cnt,lwd=2)
  cnt<-cnt+1
}
legend('topright',c('a = 1','a = 2','a = 30','a = 100'),lwd=c(2,2,2),lty=1:4,col=c('black','black','black'))

###############################################
#adjust withdrawal amount and portfolio weights to get 95% confidence of success at each rebalancing time
###############################################
getWA<-function(A0){
  L<-100000
  W<-A0
  W<-rep(W,L)
  A<-rep(A0,L)
  WA<-list() #withdrawal amounts index [[i]] indicates per individual withdrawal amounts for time t_i
  for(i in 1:(k-1)){
    Y<-rnorm(L,mu,std)
    Y[Y<0]<-0
    WA[[i]]<-rep(NA,L)
    if(sum(A)==0) next
    uA<-unique(A[A!=0])
    for(a in uA){
      v<-V[[i]][[a]]
      z<-D[[i]][[1]]
      qa<-Q[[i]][[a]]
      idx<-which(v>.95)[1]
      q<-qa[idx-1]+(qa[idx]-qa[idx-1])*(.95-v[idx-1])/(v[idx]-v[idx-1])
      W[A==a]<-W[A==a]*(q*Y[A==a]+(1-q)*(1+r))
    }
    W[A==0]<-W[A==0]*(1+r)
    B<-unlist(lapply(A,rbinom,n=1,prob=dr[i+sa-1]))
    A<-A-B
    if(sum(A)==0) next
    uA<-unique(A[A!=0])
    for(a in uA){
      v<-V[[i+1]][[a]]
      z<-D[[i+1]][[1]]
      idx<-which(v>.95)[1]
      x<-z[idx-1]+(z[idx]-z[idx-1])*(.95-v[idx-1])/(v[idx]-v[idx-1]) #min x s.t. v_i(ax,a)>=.95
      WA[[i]][A==a]<-W[A==a]/(a*(1+x))
      W[A==a]<-W[A==a]-a*WA[[i]][A==a]
    }
    W[W<0]<- -1
  }
  return(WA)
}

getWAquants<-function(WA){
  y<-WA[[1]]
  WAquants<-quantile(y[!is.na(y)],c(.05,.5,.75))
  for(i in 2:25){
    y<-WA[[i]]
    WAquants<-rbind(WAquants,quantile(y[!is.na(y)],c(.05,.5,.75)))
  }
  return(WAquants)
}

WAq1<-getWAquants(getWA(1))
WAq2<-getWAquants(getWA(2))
WAq30<-getWAquants(getWA(100))

par(mfrow=c(1,3))
lbls<-c(.05,.5,.75)
ylims<-c(.025,.055,.045,.105,.05,.14)
for(i in 1:3){
  plot(NULL,main=paste(lbls[i],'quantile of withdrawal amounts'),xlab='year',ylab='quantile',xlim=c(1,25),ylim=c(ylims[2*i-1],ylims[2*i]))
  grid()
  lines(1:25,WAq30[1:25,i],lwd=2,lty=3)
  lines(1:25,WAq2[1:25,i],lwd=2,lty=2)
  lines(1:25,WAq1[1:25,i],lwd=2)
  if(i==1) legend('topright',c(expression('A'[0]*' = 30'),expression('A'[0]*' = 2'),expression('A'[0]*' = 1')),lwd=c(2,2,2),lty=c(3,2,1),col=c('black','black','black'))
  else legend('topleft',c(expression('A'[0]*' = 30'),expression('A'[0]*' = 2'),expression('A'[0]*' = 1')),lwd=c(2,2,2),lty=c(3,2,1),col=c('black','black','black'))
}
legend('topleft',c(expression('A'[0]*' = 30'),expression('A'[0]*' = 2'),expression('A'[0]*' = 1')),lwd=c(2,2,2),lty=c(3,2,1),col=c('black','black','black'))
plot(NULL,main='Quantiles of withdrawal amounts',xlab='year',ylab='quantile',xlim=c(11,17),ylim=c(.03,.12))
grid()
for(i in 1:3){
  lines(11:17,WAq30[11:17,i],lwd=2,lty=3)
  lines(11:17,WAq2[11:17,i],lwd=2,lty=2)
  lines(11:17,WAq1[11:17,i],lwd=2)
}
legend('topleft',c(expression('A'[0]*' = 30'),expression('A'[0]*' = 2'),expression('A'[0]*' = 1')),lwd=c(2,2,2),lty=c(3,2,1),col=c('black','black','black'))
plot(NULL,main='Quantiles of withdrawal amounts',xlab='year',ylab='quantile',xlim=c(18,25),ylim=c(.03,.15))
grid()
for(i in 1:3){
  lines(18:25,WAq30[18:25,i],lwd=2,lty=3)
  lines(18:25,WAq2[18:25,i],lwd=2,lty=2)
  lines(18:25,WAq1[18:25,i],lwd=2)
}
legend('topleft',c(expression('A'[0]*' = 30'),expression('A'[0]*' = 2'),expression('A'[0]*' = 1')),lwd=c(2,2,2),lty=c(3,2,1),col=c('black','black','black'))
#mtext('Quantiles of withdrawal amounts',side=3,outer=T,line=-2)
#mtext('quantile',side=2,line=5)


###################################################
#try different means and variances
###################################################
sa<-65
k<-length(dr)-sa+1
w<-rep(1,k) #stores schedule of withdrawals; w[i] here is w_i in manuscript
A0<-30
r<-0
m<-list() #m[[i+1]][a] here is m_{a,i} in manuscript
m[[k+1]]<-rep(0,A0) 
for(i in k:1){
  m[[i]]<-(m[[i+1]]+(1:A0)*w[i])/(1+r)
}
M<-100

u2<-temphat[[1]][[1]][[30]] #from earlier run of buildV
temp0<-buildVhat(w,m,mu,std-.02,M,r,sa)
u1<-temp0[[1]][[1]][[30]]
temp0<-buildVhat(w,m,mu,std+.02,M,r,sa)
u3<-temp0[[1]][[1]][[30]]

temp0<-buildVhat(w,m,mu-.01,std,M,r,sa)
u0<-temp0[[1]][[1]][[30]]
temp0<-buildVhat(w,m,mu+.01,std,M,r,sa)
u4<-temp0[[1]][[1]][[30]]

dev.new()
par(mfrow=c(1,2))
Z<-D[[1]][[1]]

plot(Z,u2,xlim=c(0,20),type='l',lwd=2,xlab='x',ylab=expression('v'[0]*'(30x,30)'),main=expression('v'[0]*'(30x,30) for '*mu~'= 1.083 and various '*sigma))
grid()
lines(Z,u1,lty=2,lwd=2)
lines(Z,u3,lty=3,lwd=2)
legend('topleft',c(expression(sigma~'= .1553'),expression(sigma~'= .1753'),expression(sigma~'= .1953')),lwd=c(2,2,2),lty=c(2,1,3))
u<-c()
L<-10000
WW<-A0*seq(20/10,20,20/10) 
for(W in WW){
  print(which(WW==W)/length(WW))
  W<-rep(W,L)
  A<-rep(A0,L)
  I<-rep(1,L)
  for(i in 1:k){
    uA<-unique(A[A!=0])
    q<-rep(0,L)
    for(a in uA){
      aq<-c(1,Q[[i]][[a]],0)
      z<-c(-10,D[[i]][[a]],10^10) #adds big end intervals for cut function so error isn't produced
      aW<-W[A==a]
      iz<-cut(aW,z,labels=F)
      q[A==a]<-(aW-z[iz])*(aq[iz+1]-aq[iz])/(z[iz+1]-z[iz])+aq[iz]
    }
    Y<-rnorm(L,mu,std-.02)
    Y[Y<0]<-0
    dI<-unlist(lapply(I,rbinom,n=1,prob=dr[i+sa-1]))
    I<-I-dI
    B<-unlist(lapply(A-1,rbinom,n=1,prob=dr[i+sa-1]))
    B[is.na(B)]<-0 #handles cases where A=0
    A<-A-B-dI
    W<-W*(q*Y+(1-q)*(1+r))-A*I*w[i]
    W[W<0]<- -1
  }
  u<-c(u,mean(W>=0))
}
points(WW/A0,u,pch=3,col='red')

u<-c()
for(W in WW){
  print(which(WW==W)/length(WW))
  W<-rep(W,L)
  A<-rep(A0,L)
  I<-rep(1,L)
  for(i in 1:k){
    uA<-unique(A[A!=0])
    q<-rep(0,L)
    for(a in uA){
      aq<-c(1,Q[[i]][[a]],0)
      z<-c(-10,D[[i]][[a]],10^10) #adds big end intervals for cut function so error isn't produced
      aW<-W[A==a]
      iz<-cut(aW,z,labels=F)
      q[A==a]<-(aW-z[iz])*(aq[iz+1]-aq[iz])/(z[iz+1]-z[iz])+aq[iz]
    }
    Y<-rnorm(L,mu,std+.02)
    Y[Y<0]<-0
    dI<-unlist(lapply(I,rbinom,n=1,prob=dr[i+sa-1]))
    I<-I-dI
    B<-unlist(lapply(A-1,rbinom,n=1,prob=dr[i+sa-1]))
    B[is.na(B)]<-0 #handles cases where A=0
    A<-A-B-dI
    W<-W*(q*Y+(1-q)*(1+r))-A*I*w[i]
    W[W<0]<- -1
  }
  u<-c(u,mean(W>=0))
}
points(WW/A0,u,pch=4,col='red')

plot(Z,u2,xlim=c(0,20),type='l',lwd=2,xlab='x',ylab=expression('v'[0]*'(30x,30)'),main=expression('v'[0]*'(30x,30) for '*sigma~'= .1753 and various '*mu))
grid()
lines(Z,u0,lty=2,lwd=2)
lines(Z,u4,lty=3,lwd=2)
legend('topleft',c(expression(mu~'= 1.073'),expression(mu~'= 1.083'),expression(mu~'= 1.093')),lwd=c(2,2,2),lty=c(2,1,3))
u<-c()
for(W in WW){
  print(which(WW==W)/length(WW))
  W<-rep(W,L)
  A<-rep(A0,L)
  I<-rep(1,L)
  for(i in 1:k){
    uA<-unique(A[A!=0])
    q<-rep(0,L)
    for(a in uA){
      aq<-c(1,Q[[i]][[a]],0)
      z<-c(-10,D[[i]][[a]],10^10) #adds big end intervals for cut function so error isn't produced
      aW<-W[A==a]
      iz<-cut(aW,z,labels=F)
      q[A==a]<-(aW-z[iz])*(aq[iz+1]-aq[iz])/(z[iz+1]-z[iz])+aq[iz]
    }
    Y<-rnorm(L,mu-.01,std)
    Y[Y<0]<-0
    dI<-unlist(lapply(I,rbinom,n=1,prob=dr[i+sa-1]))
    I<-I-dI
    B<-unlist(lapply(A-1,rbinom,n=1,prob=dr[i+sa-1]))
    B[is.na(B)]<-0 #handles cases where A=0
    A<-A-B-dI
    W<-W*(q*Y+(1-q)*(1+r))-A*I*w[i]
    W[W<0]<- -1
  }
  u<-c(u,mean(W>=0))
}
points(WW/A0,u,pch=3,col='red')

u<-c()
for(W in WW){
  print(which(WW==W)/length(WW))
  W<-rep(W,L)
  A<-rep(A0,L)
  I<-rep(1,L)
  for(i in 1:k){
    uA<-unique(A[A!=0])
    q<-rep(0,L)
    for(a in uA){
      aq<-c(1,Q[[i]][[a]],0)
      z<-c(-10,D[[i]][[a]],10^10) #adds big end intervals for cut function so error isn't produced
      aW<-W[A==a]
      iz<-cut(aW,z,labels=F)
      q[A==a]<-(aW-z[iz])*(aq[iz+1]-aq[iz])/(z[iz+1]-z[iz])+aq[iz]
    }
    Y<-rnorm(L,mu+.01,std)
    Y[Y<0]<-0
    dI<-unlist(lapply(I,rbinom,n=1,prob=dr[i+sa-1]))
    I<-I-dI
    B<-unlist(lapply(A-1,rbinom,n=1,prob=dr[i+sa-1]))
    B[is.na(B)]<-0 #handles cases where A=0
    A<-A-B-dI
    W<-W*(q*Y+(1-q)*(1+r))-A*I*w[i]
    W[W<0]<- -1
  }
  u<-c(u,mean(W>=0))
}
points(WW/A0,u,pch=4,col='red')

############################################
#simulate using optimal portfolio weights for a dif number of starting annuitants
############################################
A0<-2
idx<-1
plot(D[[idx]][[A0]]/A0,V[[idx]][[A0]],xlim=c(0,20),type='l',ylim=c(0,1),lwd=2,xlab='P',ylab='',main=expression('Withdrawal success probabilities for A'[0]*' = 30 and various portfolios'))
grid() 
lines(c(0,D[[idx]][[A0]]/A0),c(0,V[[idx]][[A0]]),lwd=2) #so grid doesn't cover curve
legend('topleft',c(expression('v'[0]*'(30P,30)'),expression('optimal '*pi[i]),expression(pi[i]*' = (1,0)'),expression(pi[i]*' = (.9,.1)'),expression(pi[i]*' = (.6,.4)')),lty=c(1,NA,NA,NA,NA),pch=c(NA,3,0,20,1),col=c(1,'red',1,1,1))
#all stock
u<-c()
L<-10000
ogA0<-A0
A0<-50
WW<-A0*seq(20/10,20,20/10) 
#use optimal portfolio weights to similate v_0(30x,30)
u<-c()
for(W in WW){
  print(which(WW==W)/length(WW))
  W<-rep(W,L)
  A<-rep(A0,L)
  I<-rep(1,L)
  for(i in 1:k){
    uA<-unique(A[A!=0 & A<ogA0])
    q<-rep(0,L)
    for(a in uA){
      aq<-c(1,Q[[i]][[a]],0)
      z<-c(-10,D[[i]][[a]],10^10) #adds big end intervals for cut function so error isn't produced
      aW<-W[A==a]
      iz<-cut(aW,z,labels=F)
      q[A==a]<-(aW-z[iz])*(aq[iz+1]-aq[iz])/(z[iz+1]-z[iz])+aq[iz]
    }
    if(sum(A>=ogA0)>0){
      uA<-unique(A[A>=ogA0])
      aq<-c(1,Q[[i]][[ogA0]],0)
      for(a in uA){
        z<-c(-10,D[[i]][[ogA0]]*a/ogA0,10^10) #adds big end intervals for cut function so error isn't produced
        aW<-W[A==a]
        iz<-cut(aW,z,labels=F)
        q[A==a]<-(aW-z[iz])*(aq[iz+1]-aq[iz])/(z[iz+1]-z[iz])+aq[iz]
      }
    }
    
    Y<-rnorm(L,mu,std)
    Y[Y<0]<-0
    dI<-unlist(lapply(I,rbinom,n=1,prob=dr[i+sa-1]))
    I<-I-dI
    B<-unlist(lapply(A-1,rbinom,n=1,prob=dr[i+sa-1]))
    B[is.na(B)]<-0 #handles cases where A=0
    A<-A-B-dI
    W<-W*(q*Y+(1-q)*(1+r))-A*I*w[i]
    W[W<0]<- -1
  }
  u<-c(u,mean(W>=0))
}
points(WW/A0,u,pch=3,col='red')



#########################################
#Everything below this line is just a bunch of miscellaneous leftovers
#########################################





############################################
#vhat
############################################


#set parameters for buildV
sa<-65
k<-length(dr)-sa+1
w<-rep(1,k) #stores schedule of withdrawals; w[i] here is w_i in manuscript
A0<-100
r<-0
m<-list() #m[[i+1]][a] here is m_{a,i} in manuscript
m[[k+1]]<-rep(0,A0) 
for(i in k:1){
  m[[i]]<-(m[[i+1]]+(1:A0)*w[i])/(1+r)
}
M<-100
mu<-mean(s)
std<-sd(s)









########################################
getWAsumQuants<-function(WA,A0){
  ySum<-rep(0,L)
  for(i in 1:(k-1)){
    wdl<-WA[[i]]
    wdl[is.na(wdl)]<-0
    ySum<-ySum+wdl
  }
  return(quantile(ySum/A0,c(.05,.5,.75)))
}

WAsumQuants<-getWAsumQuants(getWA(1),1)
for(i in 2:50){
  WAsumQuants<-rbind(WAsumQuants,getWAsumQuants(getWA(i),i))
}

mlt<-cbind(1:50,1:50)
mlt<-cbind(1:50,mlt)
y<-WAsumQuants/mlt
par(mfrow=c(1,1))
plot(y[,1],ylim=c(0,.5))
lines(y[,2],lty=2)
lines(y[,3],lty=3)






###############################################


A0<-30
L<-100000
WW<-A0*c(10,15,20,25) 
WWA<-list()
cnt<-1
for(W in WW){
  print(which(WW==W)/length(WW))
  W<-rep(W,L)
  A<-rep(A0,L)
  WA<-list() #withdrawal amounts
  for(i in 1:(k-1)){
    Y<-rnorm(L,mu,std)
    Y[Y<0]<-0
    WA[[i]]<-rep(NA,L)
    if(sum(A)==0) next
    uA<-unique(A[A!=0 & A<=100])
    for(a in uA){
      v<-V[[i]][[a]]
      z<-D[[i]][[1]]
      qa<-Q[[i]][[a]]
      idx<-which(v>.95)[1]
      q<-qa[idx-1]+(qa[idx]-qa[idx-1])*(.95-v[idx-1])/(v[idx]-v[idx-1])
      W[A==a]<-W[A==a]*(q*Y[A==a]+(1-q)*(1+r))
    }
    W[A==0]<-W[A==0]*(1+r)
    B<-unlist(lapply(A,rbinom,n=1,prob=dr[i+sa-1]))
    A<-A-B
    if(sum(A)==0) next
    uA<-unique(A[A!=0 & A<=100])
    for(a in uA){
      v<-V[[i+1]][[a]]
      z<-D[[i+1]][[1]]
      idx<-which(v>.95)[1]
      x<-z[idx-1]+(z[idx]-z[idx-1])*(.95-v[idx-1])/(v[idx]-v[idx-1]) #min x s.t. v_i(ax,a)>=.95
      WA[[i]][A==a]<-W[A==a]/(a*(1+x))
      W[A==a]<-W[A==a]-a*WA[[i]][A==a]
    }
    W[W<0]<- -1
    i<-i+1
  }
  WWA[[cnt]]<-WA
  cnt<-cnt+1
}
par(mfrow=c(2,2))

for(P in WW/A0){
  for(i in 1:40)
}
j<-4
y<-WWA[[j]][[13]]
quantile(y[!is.na(y)],c(.05,.1,.5,.8))*A0/WW[j]
sum(is.na(y))
hist(y[!is.na(y)],breaks=20)




#################################


x1<-c()
x2<-c()
x3<-c()
for(a in 1:30){
  
  idx<-which(v>.9)[1]
  temp<-z[idx-1]+(z[idx]-z[idx-1])*(.9-v[idx-1])/(v[idx]-v[idx-1])
  x1<-c(x1,temp)
  idx<-which(v>.95)[1]
  temp<-z[idx-1]+(z[idx]-z[idx-1])*(.95-v[idx-1])/(v[idx]-v[idx-1])
  x2<-c(x2,temp)
  idx<-which(v>.99)[1]
  temp<-z[idx-1]+(z[idx]-z[idx-1])*(.99-v[idx-1])/(v[idx]-v[idx-1])
  x3<-c(x3,temp)
}
lines(1:a,x3,col='black',lwd=2,lty=3)
lines(1:a,x2,col='black',lwd=2)
lines(1:a,x1,col='black',lwd=2,lty=2)
legend('topright',c('C = .99','C = .95','C = .9'),lwd=c(2,2,2),lty=c(3,1,2),col=c('black','black','black'))














plot(2:A0,-log10(2:A0))



a1<-1
a2<-2
a3<-30
plot(D[[idx]][[1]],V[[idx]][[30]]-V[[idx]][[29]],xlim=c(0,30),type='l',lwd=2,xlab='x',ylab=expression('v'[0]*'(ax,a)'),main=expression('v'[0]*'(ax,a) for various a'))
grid()
lines(D[[idx]][[a2]]/a2,V[[idx]][[a2]],lty=2,lwd=2)
lines(D[[idx]][[a3]]/a3,V[[idx]][[a3]],lty=3,lwd=2)
legend('bottomright',c(paste0('a = ',a1),paste0('a = ',a2),paste0('a = ',a3)),lwd=c(2,2,2),lty=c(1,2,3),col=c('black','black','black'))


#############

#############
u<-c()
L<-1000
WW<-A0*seq(5,20,3) 
for(W in WW){
  print(which(WW==W)/length(WW))
  W<-rep(W,L)
  A<-rep(A0,L)
  for(i in 1:k){
    oldv<-V[[i+1]]
    oldD<-D[[i+1]]
    q<-c()
    cnt<-1
    for(x in W){
      tempq<-0
      a<-A[cnt]
      if(a==0){
        q<-c(q,0)
        next
      }
      v<-g0(mu,std,r,x,oldD,oldv,a,w[i],m[[i+1]],M,dr[sa+i-1])
      #try q*_i(x) in (0,1] next
      tempV<-unlist(lapply(G1,g,mu,std,r,x,oldD,oldv,a,w[i],m[[i+1]],M,dr[sa+i-1]))
      q1<-G1[which.max(tempV)]
      G3<-seq(q1-.01*9,q1+.01*10,.01)
      tempV<-unlist(lapply(G3,g,mu,std,r,x,oldD,oldv,a,w[i],m[[i+1]],M,dr[sa+i-1]))
      q3<-G3[which.max(tempV)]
      tempV<-max(tempV[!is.na(tempV)])
      #pick best q*_i(x); remember that i here is i-1 in algorithm
      if(tempV>v){
        tempq<-q3
      }
      q<-c(q,tempq)
      cnt<-cnt+1
    }
    Y<-rnorm(L,mu,std)
    B<-unlist(lapply(A,rbinom,n=1,prob=dr[i+sa-1]))
    A<-A-B
    W<-W*(q*Y+(1-q)*(1+r))-A*w[i]
    W[W<0]<- -1
  }
  u<-c(u,mean(W>=0))
}
points(WW/A0,u,pch=3,col='green')



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

