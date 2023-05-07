# Here is the code used to produce figures 
# for the long term performance of leveraged
# etfs
f<-function(y,m1,y0,L,L0,r){
  a<-(1/(y-y0))*(log((1+L*(exp(y0)-1))/(1+L*(exp(y)-1)))/(y-y0)+L*exp(y)/(1+L*(exp(y)-1)))
  b<-L*exp(y)/(1+L*(exp(y)-1))-2*a*y
  c<-log(1+L*(exp(y0)-1))-a*y0^2-b*y0-log(1+r/252)
  s<-sqrt(((L0-b)*m1-c)/a-m1^2)
  return(s)
}

g<-function(m1,y0,L,L0,r){
  y<-seq(y0+.001,abs(y0),.001)
  return(max(unlist(lapply(y,f,m1,y0,L,L0,r)),na.rm=T))
}

r<-.0095
E<-seq(.01,.2,.01)
dev.new()
par(mar=c(5,5,4,2))
par(mfrow=c(2,2))
plot(E,lapply(E/252,g,log(1-.1),2,0,0),xlab=expression('252m'[1]),ylab='s',main=expression('L'[0]~'= 0,  L'~'= 2'),type='l')
lines(E,lapply(E/252,g,log(1-.2),2,0,0),lty=2)
lines(E,lapply(E/252,g,log(1-.1),2,0,r),col='red')
lines(E,lapply(E/252,g,log(1-.2),2,0,r),lty=2,col='red')
grid()
plot(E,lapply(E/252,g,log(1-.1),3,0,0),xlab=expression('252m'[1]),ylab='s',main=expression('L'[0]~'= 0,  L'~'= 3'),type='l')
lines(E,lapply(E/252,g,log(1-.2),3,0,0),lty=2)
lines(E,lapply(E/252,g,log(1-.1),3,0,r),col='red')
lines(E,lapply(E/252,g,log(1-.2),3,0,r),lty=2,col='red')
grid()
plot(E,lapply(E/252,g,log(1-.1),2,1,0),xlab=expression('252m'[1]),ylab='s',main=expression('L'[0]~'= 1,  L'~'= 2'),type='l')
lines(E,lapply(E/252,g,log(1-.2),2,1,0),lty=2)
lines(E,lapply(E/252,g,log(1-.1),2,1,r),col='red')
lines(E,lapply(E/252,g,log(1-.2),2,1,r),lty=2,col='red')
grid()
plot(E,lapply(E/252,g,log(1-.1),3,1,0),xlab=expression('252m'[1]),ylab='s',main=expression('L'[0]~'= 1,  L'~'= 3'),type='l')
lines(E,lapply(E/252,g,log(1-.2),3,1,0),lty=2)
lines(E,lapply(E/252,g,log(1-.1),3,1,r),col='red')
lines(E,lapply(E/252,g,log(1-.2),3,1,r),lty=2,col='red')
grid()

dev.new()
par(mar=c(5,5,4,2))
par(mfrow=c(2,2))
plot(E,lapply(E/252,g,log(1-.1),2,1.25,0),xlab=expression('252m'[1]),ylab='s',main=expression('L'[0]~'= 1.25,  L'~'= 2'),type='l')
lines(E,lapply(E/252,g,log(1-.2),2,1.25,0),lty=2)
lines(E,lapply(E/252,g,log(1-.1),2,1.25,r),col='red')
lines(E,lapply(E/252,g,log(1-.2),2,1.25,r),lty=2,col='red')
grid()
plot(E,lapply(E/252,g,log(1-.1),3,1.25,0),xlab=expression('252m'[1]),ylab='s',main=expression('L'[0]~'= 1.25,  L'~'= 3'),type='l')
lines(E,lapply(E/252,g,log(1-.2),3,1.25,0),lty=2)
lines(E,lapply(E/252,g,log(1-.1),3,1.25,r),col='red')
lines(E,lapply(E/252,g,log(1-.2),3,1.25,r),lty=2,col='red')
grid()
plot(E,lapply(E/252,g,log(1-.1),2,1.5,0),xlab=expression('252m'[1]),ylab='s',main=expression('L'[0]~'= 1.5,  L'~'= 2'),type='l')
lines(E,lapply(E/252,g,log(1-.2),2,1.5,0),lty=2)
lines(E,lapply(E/252,g,log(1-.1),2,1.5,r),col='red')
lines(E,lapply(E/252,g,log(1-.2),2,1.5,r),lty=2,col='red')
grid()
plot(E,lapply(E/252,g,log(1-.1),3,1.5,0),xlab=expression('252m'[1]),ylab='s',main=expression('L'[0]~'= 1.5,  L'~'= 3'),type='l')
lines(E,lapply(E/252,g,log(1-.2),3,1.5,0),lty=2)
lines(E,lapply(E/252,g,log(1-.1),3,1.5,r),col='red')
lines(E,lapply(E/252,g,log(1-.2),3,1.5,r),lty=2,col='red')
grid()

h<-function(E,y0,L0,r){
  t1<-unlist(lapply(E/252,g,y0,2,L0,r))
  t2<-unlist(lapply(E/252,g,y0,3,L0,r))
  w1<-which(t1>t2)
  w2<-which(t1<=t2)
  lines(E[w1],t1[w1],col='green')
  lines(E[w2],t2[w2])
}

E<-seq(.01,.1,.0001)
dev.new()
par(mfrow=c(1,1))
plot(NULL,xlab=expression('252m'[1]),ylab='s', main="2x vs 3x Leveraged ETF",
     xlim=c(min(E),max(E)), ylim=c(0,.0175))
for(i in seq(1,2,.1)){
  h(E,log(1-.2),i,r)
}
grid()


#negative L
g<-function(m1,y0,L,L0,r){
  y<-seq(-1*y0,y0-.001,.001)
  return(max(unlist(lapply(y,f,m1,y0,L,L0,r)),na.rm=T))
}

r<-.0095
dev.new()
par(mar=c(5,5,4,2))
par(mfrow=c(3,2))
E<-seq(-.4,-.01,.01)
plot(E,lapply(E/252,g,log(1+.1),-2,-1,0),xlab=expression('252m'[1]),ylab='s',main=expression('L'[0]~'= -1,  L'~'= -2'),type='l')
lines(E,lapply(E/252,g,log(1+.15),-2,-1,0),lty=2)
lines(E,lapply(E/252,g,log(1+.1),-2,-1,r),col='red')
lines(E,lapply(E/252,g,log(1+.15),-2,-1,r),lty=2,col='red')
grid()
plot(E,lapply(E/252,g,log(1+.1),-3,-1,0),xlab=expression('252m'[1]),ylab='s',main=expression('L'[0]~'= -1,  L'~'= -3'),type='l')
lines(E,lapply(E/252,g,log(1+.15),-3,-1,0),lty=2)
lines(E,lapply(E/252,g,log(1+.1),-3,-1,r),col='red')
lines(E,lapply(E/252,g,log(1+.15),-3,-1,r),lty=2,col='red')
grid()
plot(E,lapply(E/252,g,log(1+.1),-2,-1.25,0),xlab=expression('252m'[1]),ylab='s',main=expression('L'[0]~'= -1.25,  L'~'= -2'),type='l')
lines(E,lapply(E/252,g,log(1+.15),-2,-1.25,0),lty=2)
lines(E,lapply(E/252,g,log(1+.1),-2,-1.25,r),col='red')
lines(E,lapply(E/252,g,log(1+.15),-2,-1.25,r),lty=2,col='red')
grid()
plot(E,lapply(E/252,g,log(1+.1),-3,-1.25,0),xlab=expression('252m'[1]),ylab='s',main=expression('L'[0]~'= -1.25,  L'~'= -3'),type='l')
lines(E,lapply(E/252,g,log(1+.15),-3,-1.25,0),lty=2)
lines(E,lapply(E/252,g,log(1+.1),-3,-1.25,r),col='red')
lines(E,lapply(E/252,g,log(1+.15),-3,-1.25,r),lty=2,col='red')
grid()
plot(E,lapply(E/252,g,log(1+.1),-2,-1.5,0),xlab=expression('252m'[1]),ylab='s',main=expression('L'[0]~'= -1.5,  L'~'= -2'),type='l')
lines(E,lapply(E/252,g,log(1+.15),-2,-1.5,0),lty=2)
lines(E,lapply(E/252,g,log(1+.1),-2,-1.5,r),col='red')
lines(E,lapply(E/252,g,log(1+.15),-2,-1.5,r),lty=2,col='red')
grid()
plot(E,lapply(E/252,g,log(1+.1),-3,-1.5,0),xlab=expression('252m'[1]),ylab='s',main=expression('L'[0]~'= -1.5,  L'~'= -3'),type='l')
lines(E,lapply(E/252,g,log(1+.15),-3,-1.5,0),lty=2)
lines(E,lapply(E/252,g,log(1+.1),-3,-1.5,r),col='red')
lines(E,lapply(E/252,g,log(1+.15),-3,-1.5,r),lty=2,col='red')
grid()

h<-function(E,y0,L0,r){
  t1<-unlist(lapply(E/252,g,y0,-2,L0,r))
  t2<-unlist(lapply(E/252,g,y0,-3,L0,r))
  w1<-which(t1>t2)
  w2<-which(t1<=t2)
  lines(E[w1],t1[w1],col='green')
  lines(E[w2],t2[w2])
}

E<-seq(-.4,-.01,.001)
dev.new()
par(mfrow=c(1,1))
plot(NULL,xlab=expression('252m'[1]),ylab='s', main="-2x vs -3x Leveraged ETF",
     xlim=c(min(E),max(E)), ylim=c(0,.0225))
for(i in seq(-1,-1.5,-.1)){
  h(E,log(1+.15),i,r)
}
grid()

##############
#1>L>1/(2-.2)  log(L^(-1)-1)=log(1+x_0) -> L=1/(2+x_0), x_0<0
#lower bound
############## 
g<-function(m1,y0,L,L0,r){
  y<-seq(y0+.001,abs(y0),.001)
  return(max(unlist(lapply(y,f,m1,y0,L,L0,r)),na.rm=T))
}
E<-seq(.01,.2,.001)
dev.new()
par(mfrow=c(3,2))
plot(NULL,xlab=expression('252m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.2)'),
     xlim=c(min(E),max(E)), ylim=c(0,.06))
clr<-c('green','red')
Ls<-c(.56,.99) 
for(i in 1:2){
  lines(E,lapply(E/252,g,log(1-.2),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.56','L=.99'))
grid()
plot(NULL,xlab=expression('52m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.25)'),
     xlim=c(min(E),max(E)), ylim=c(0,.125))
Ls<-c(.58,.99)
for(i in 1:2){
  lines(E,lapply(E/52,g,log(1-.25),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.58','L=.99'))
grid()
plot(NULL,xlab=expression('12m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.3)'),
     xlim=c(min(E),max(E)), ylim=c(0,.25))
Ls<-c(.59,.99)
for(i in 1:2){
  lines(E,lapply(E/12,g,log(1-.3),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.59','L=.99'))
grid()
plot(NULL,xlab=expression('4m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.35)'),
     xlim=c(min(E),max(E)), ylim=c(0,.45))
Ls<-c(.61,.99)
for(i in 1:2){
  lines(E,lapply(E/4,g,log(1-.35),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.61','L=.99'))
grid()
plot(NULL,xlab=expression('2m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.4)'),
     xlim=c(min(E),max(E)), ylim=c(0,.6))
Ls<-c(.63,.99)
for(i in 1:2){
  lines(E,lapply(E/2,g,log(1-.4),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.63','L=.99'))
grid()
plot(NULL,xlab=expression('m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.45)'),
     xlim=c(min(E),max(E)), ylim=c(0,.8))
Ls<-c(.65,.99)
for(i in 1:2){
  lines(E,lapply(E,g,log(1-.45),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.65','L=.99'))
grid()


############################
#0<L<.5
#lower bound
############################
g<-function(m1,y0,L,L0,r){
  y<-seq(-1*y0,y0-.001,.001)
  return(max(unlist(lapply(y,f,m1,y0,L,L0,r)),na.rm=T))
}
E<-seq(.01,.2,.001)
dev.new()
par(mfrow=c(3,2))
plot(NULL,xlab=expression('252m'[1]),ylab='s', main=expression('y'[1]~'= log(1+.15)'),
     xlim=c(min(E),max(E)), ylim=c(0,.08))
clr<-c('red','green')
Ls<-c(.3,.46)
for(i in 1:2){
  lines(E,lapply(E/252,g,log(1+.15),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.3','L=.46'))
grid()
plot(NULL,xlab=expression('52m'[1]),ylab='s', main=expression('y'[1]~'= log(1+.2)'),
     xlim=c(min(E),max(E)), ylim=c(0,.17))
Ls<-c(.3,.45)
for(i in 1:2){
  lines(E,lapply(E/52,g,log(1+.2),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.3','L=.45'))
grid()
plot(NULL,xlab=expression('12m'[1]),ylab='s', main=expression('y'[1]~'= log(1+.25)'),
     xlim=c(min(E),max(E)), ylim=c(0,.35))
Ls<-c(.3,.44)
for(i in 1:2){
  lines(E,lapply(E/12,g,log(1+.25),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.3','L=.44'))
grid()
plot(NULL,xlab=expression('4m'[1]),ylab='s', main=expression('y'[1]~'= log(1+.3)'),
     xlim=c(min(E),max(E)), ylim=c(0,.6))
Ls<-c(.3,.43)
for(i in 1:2){
  lines(E,lapply(E/4,g,log(1+.3),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.3','L=.43'))
grid()
plot(NULL,xlab=expression('2m'[1]),ylab='s', main=expression('y'[1]~'= log(1+.35)'),
     xlim=c(min(E),max(E)), ylim=c(0,.85))
Ls<-c(.3,.42)
for(i in 1:2){
  lines(E,lapply(E/2,g,log(1+.35),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.3','L=.42'))
grid()
plot(NULL,xlab=expression('m'[1]),ylab='s', main=expression('y'[1]~'= log(1+.4)'),
     xlim=c(min(E),max(E)), ylim=c(0,1.2))
Ls<-c(.3,.41)
for(i in 1:2){
  lines(E,lapply(E,g,log(1+.4),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.3','L=.41'))
grid()


##############
#1>L>1/(2-.2)  log(L^(-1)-1)=log(1+x_0) -> L=1/(2+x_0), x_0<0
#upper bound
############## 
g<-function(m1,y0,L,L0,r){
  y<-seq(y0+.001,abs(y0),.001)
  return(max(unlist(lapply(y,f,m1,y0,L,L0,r)),na.rm=T))
}
gg<-function(m1,y0,L,L0,r){
  y<-seq(-1*y0,y0-.001,.001)
  return(max(unlist(lapply(y,f,m1,y0,L,L0,r)),na.rm=T))
}
E<-seq(.01,.2,.01)
dev.new()
par(mfrow=c(3,2))
plot(NULL,xlab=expression('252m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.2),  y'[1]~'= log(1+.15)'),
     xlim=c(min(E),max(E)), ylim=c(0,.06))
Ls<-seq(.56,.99,.01) 
Y<-rep(10,length(E))
for(i in Ls){
  tmp<-unlist(lapply(E/252,g,log(1-.2),i,1,0))
  Y<-(tmp+Y-abs(tmp-Y))/2
}
lines(E,Y,col='green')
Ls<-seq(.01,.46,.01)
Y<-rep(10,length(E))
for(i in Ls){
  tmp<-unlist(lapply(E/252,gg,log(1+.15),i,1,0))
  Y<-(tmp+Y-abs(tmp-Y))/2
}
lines(E,Y,col='red')
grid()
legend(x='bottomright',lty=rep(1,2),col=c('red','green'),legend=c(expression('L'<=.46),expression('L'>=.56)))

plot(NULL,xlab=expression('52m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.25),  y'[1]~'= log(1+.2)'),
     xlim=c(min(E),max(E)), ylim=c(0,.13))
Ls<-seq(.58,.99,.01) 
Y<-rep(10,length(E))
for(i in Ls){
  tmp<-unlist(lapply(E/52,g,log(1-.25),i,1,0))
  Y<-(tmp+Y-abs(tmp-Y))/2
}
lines(E,Y,col='green')
Ls<-seq(.01,.45,.01)
Y<-rep(10,length(E))
for(i in Ls){
  tmp<-unlist(lapply(E/52,gg,log(1+.2),i,1,0))
  Y<-(tmp+Y-abs(tmp-Y))/2
}
lines(E,Y,col='red')
grid()
legend(x='bottomright',lty=rep(1,2),col=c('red','green'),legend=c(expression('L'<=.45),expression('L'>=.58)))

plot(NULL,xlab=expression('12m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.3),  y'[1]~'= log(1+.25)'),
     xlim=c(min(E),max(E)), ylim=c(0,.3))
Ls<-seq(.59,.99,.01) 
Y<-rep(10,length(E))
for(i in Ls){
  tmp<-unlist(lapply(E/12,g,log(1-.3),i,1,0))
  Y<-(tmp+Y-abs(tmp-Y))/2
}
lines(E,Y,col='green')
Ls<-seq(.01,.44,.01)
Y<-rep(10,length(E))
for(i in Ls){
  tmp<-unlist(lapply(E/12,gg,log(1+.25),i,1,0))
  Y<-(tmp+Y-abs(tmp-Y))/2
}
lines(E,Y,col='red')
grid()
legend(x='bottomright',lty=rep(1,2),col=c('red','green'),legend=c(expression('L'<=.44),expression('L'>=.59)))

plot(NULL,xlab=expression('4m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.35),  y'[1]~'= log(1+.3)'),
     xlim=c(min(E),max(E)), ylim=c(0,.5))
Ls<-seq(.61,.99,.01) 
Y<-rep(10,length(E))
for(i in Ls){
  tmp<-unlist(lapply(E/4,g,log(1-.35),i,1,0))
  Y<-(tmp+Y-abs(tmp-Y))/2
}
lines(E,Y,col='green')
Ls<-seq(.01,.43,.01)
Y<-rep(10,length(E))
for(i in Ls){
  tmp<-unlist(lapply(E/4,gg,log(1+.3),i,1,0))
  Y<-(tmp+Y-abs(tmp-Y))/2
}
lines(E,Y,col='red')
grid()
legend(x='bottomright',lty=rep(1,2),col=c('red','green'),legend=c(expression('L'<=.43),expression('L'>=.61)))

plot(NULL,xlab=expression('2m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.4),  y'[1]~'= log(1+.35)'),
     xlim=c(min(E),max(E)), ylim=c(0,.7))
Ls<-seq(.63,.99,.01) 
Y<-rep(10,length(E))
for(i in Ls){
  tmp<-unlist(lapply(E/2,g,log(1-.4),i,1,0))
  Y<-(tmp+Y-abs(tmp-Y))/2
}
lines(E,Y,col='green')
Ls<-seq(.01,.42,.01)
Y<-rep(10,length(E))
for(i in Ls){
  tmp<-unlist(lapply(E/2,gg,log(1+.35),i,1,0))
  Y<-(tmp+Y-abs(tmp-Y))/2
}
lines(E,Y,col='red')
grid()
legend(x='bottomright',lty=rep(1,2),col=c('red','green'),legend=c(expression('L'<=.42),expression('L'>=.63)))

plot(NULL,xlab=expression('m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.45),  y'[1]~'= log(1+.4)'),
     xlim=c(min(E),max(E)), ylim=c(0,1))
Ls<-seq(.65,.99,.01) 
Y<-rep(10,length(E))
for(i in Ls){
  tmp<-unlist(lapply(E,g,log(1-.45),i,1,0))
  Y<-(tmp+Y-abs(tmp-Y))/2
}
lines(E,Y,col='green')
Ls<-seq(.01,.41,.01)
Y<-rep(10,length(E))
for(i in Ls){
  tmp<-unlist(lapply(E,gg,log(1+.4),i,1,0))
  Y<-(tmp+Y-abs(tmp-Y))/2
}
lines(E,Y,col='red')
grid()
legend(x='bottomright',lty=rep(1,2),col=c('red','green'),legend=c(expression('L'<=.41),expression('L'>=.65)))



############################
#0<L<.5
#upper bound
############################
g<-function(m1,y0,L,L0,r){
  y<-seq(-1*y0,y0-.001,.001)
  return(max(unlist(lapply(y,f,m1,y0,L,L0,r)),na.rm=T))
}
E<-seq(.01,.2,.001)
dev.new()
par(mfrow=c(3,2))
plot(NULL,xlab=expression('252m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.2),  y'[1]~'= log(1+.15)'),
     xlim=c(min(E),max(E)), ylim=c(0,.08))
clr<-c('red','green')
Ls<-c(.4,.47)
for(i in 1:2){
  lines(E,lapply(E/252,g,log(1-.2),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.4','L=.47'))
grid()
plot(NULL,xlab=expression('52m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.25),  y'[1]~'= log(1+.175)'),
     xlim=c(min(E),max(E)), ylim=c(0,.15))
Ls<-c(.4,.46)
for(i in 1:2){
  lines(E,lapply(E/52,g,log(1-.25),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.4','L=.46'))
grid()
plot(NULL,xlab=expression('12m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.3),  y'[1]~'= log(1+.2)'),
     xlim=c(min(E),max(E)), ylim=c(0,.35))
Ls<-c(.4,.46)
for(i in 1:2){
  lines(E,lapply(E/12,g,log(1-.3),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.4','L=.46'))
grid()
plot(NULL,xlab=expression('4m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.35),  y'[1]~'= log(1+.225)'),
     xlim=c(min(E),max(E)), ylim=c(0,.6))
Ls<-c(.4,.45)
for(i in 1:2){
  lines(E,lapply(E/4,g,log(1-.35),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.4','L=.45'))
grid()
plot(NULL,xlab=expression('2m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.4),  y'[1]~'= log(1+.25)'),
     xlim=c(min(E),max(E)), ylim=c(0,.8))
Ls<-c(.4,.45)
for(i in 1:2){
  lines(E,lapply(E/2,g,log(1-.4),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.4','L=.45'))
grid()
plot(NULL,xlab=expression('m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.45),  y'[1]~'= log(1+.275)'),
     xlim=c(min(E),max(E)), ylim=c(0,1.1))
Ls<-c(.4,.44)
for(i in 1:2){
  lines(E,lapply(E,g,log(1-.45),Ls[i],1,0),col=clr[i])
}
legend(x='bottomright',lty=rep(1,2),col=clr,legend=c('L=.4','L=.44'))
grid()





##################
#last figure
##################

f<-function(y,m1,y0,L,L0,r){
  d<-L+(1-L)*exp(r0)
  a<-(1/(y-y0))*(log((d+L*(exp(y0)-1))/(d+L*(exp(y)-1)))/(y-y0)+L*exp(y)/(d+L*(exp(y)-1)))
  b<-L*exp(y)/(d+L*(exp(y)-1))-2*a*y
  c<-log(d+L*(exp(y0)-1))-a*y0^2-b*y0-log(1+r/252)
  s<-sqrt(((L0-b)*m1-c)/a-m1^2)
  return(s)
}


##############
#1>L>1/(2-.2)  log(L^(-1)-1)=log(1+x_0) -> L=1/(2+x_0), x_0<0
#upper bound
############## 
g<-function(m1,y0,L,L0,r){
  y<-seq(y0+.001,abs(y0),.001)
  return(max(unlist(lapply(y,f,m1,y0,L,L0,r)),na.rm=T))
}
gg<-function(m1,y0,L,L0,r){
  y<-seq(-1*y0,y0-.001,.001)
  return(max(unlist(lapply(y,f,m1,y0,L,L0,r)),na.rm=T))
}
E<-seq(0,.1,.0001)
dev.new()
par(mfrow=c(1,1))

plot(NULL,xlab=expression('m'[1]),ylab='s', main=expression('y'[0]~'= log(1-.45),  L'>=.7),
     xlim=c(min(E),max(E)), ylim=c(0,.42))
for(r0 in c(0,.01,.02,.03,.04,.05)){
  Ls<-seq(.7,.99,.01) 
  Y<-rep(10,length(E))
  for(i in Ls){
    tmp<-unlist(lapply(E,g,log(1-.45),i,1,0))
    Y<-(tmp+Y-abs(tmp-Y))/2
  }
  lines(E,Y)
}
grid()