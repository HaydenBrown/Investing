######################################################
#The following code uses S&P and 1YTR as the asset classes.
#The code must be modified to consider an alternative pair 
#of asset classes. 
######################################################
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

#illustrates 149 years of annual real returns
par(mar=c(2,3,2,2))
par(mfrow=c(2,1))
L<-nrow(x)
d<-x[1:(L-1),c('Year','Dividend','Index','CPI','1YTR')]
d$Inflation<-x$CPI[2:L]/x$CPI[1:(L-1)]
s<-((x$Index[2:L]+x$Dividend[1:(L-1)])/x$Index[1:(L-1)])/d$Inflation-1
b<-(1+d$`1YTR`/100)/d$Inflation-1
plot(1871:2019,s,main='S&P real return',xlab='Year',pch=20)
lines(1871:2019,s)
plot(1871:2019,b,main='1YTR real return',xlab='Year',pch=20)
lines(1871:2019,b)
par(mar=c(5.1,4.1,4.1,2.1))

#Split real returns at 1951
s1<-s[d$Year<1951]
b1<-b[d$Year<1951]
s2<-s[d$Year>1950]
b2<-b[d$Year>1950]

###########################################
#Autoregression of real returns 
###########################################
#test for autoregression, lag 2, Ljung-Box
LB<-function(i,x,type){
  temp<-Box.test(x=x,lag=i,type=type)
  return(temp$p.value)
}
LB(2,x=s1,type="Ljung-Box")
LB(2,x=b1,type="Ljung-Box")
LB(2,x=s2,type="Ljung-Box")
LB(2,x=b2,type="Ljung-Box")

#models
ar2b1<-lm(b1[3:length(b1)]~b1[2:(length(b1)-1)]+b1[1:(length(b1)-2)])
ar2b2<-lm(b2[3:length(b2)]~b2[2:(length(b2)-1)]+b2[1:(length(b2)-2)])
ar1b1<-lm(b1[2:length(b1)]~b1[1:(length(b1)-1)])
ar1b2<-lm(b2[2:length(b2)]~b2[1:(length(b2)-1)])
ar0s1<-lm(s1~1)
ar0s2<-lm(s2~1)

#residuals
rs1<-ar0s1$residuals
rs2<-ar0s2$residuals
rb1<-ar1b1$residuals
rb2<-ar1b2$residuals

###################################################
#Normality of real returns
###################################################
#Normality test on residuals
ks.test(rs1,'pnorm',0,sd(rs1))
ks.test(rs2,'pnorm',0,sd(rs2))
ks.test(rb1,'pnorm',0,sd(rb1))
ks.test(rb2,'pnorm',0,sd(rb2))

#normality plots for S&P residuals, subdivided time period
par(mfrow=c(1,2))
qqnorm(rs1,col='blue',main=paste0('Pre-1951 S&P QQ plot'),xlab='N(0,1) quantiles',ylab='Residual quantiles')  
qqline(rs1)   
grid() 
qqnorm(rs2,col='blue',main=paste0('Post-1951 S&P QQ plot'),xlab='N(0,1) quantiles',ylab='Residual quantiles')  
qqline(rs2)   
grid()

#normality plots for 1YTR residuals, subdivided time period
par(mfrow=c(1,2))
qqnorm(rb1,col='blue',main=paste0('Pre-1951 1YTR QQ plot'),xlab='N(0,1) quantiles',ylab='Residual quantiles')  
qqline(rb1)   
grid() 
qqnorm(rb2,col='blue',main=paste0('Post-1951 1YTR QQ plot'),xlab='N(0,1) quantiles',ylab='Residual quantiles')  
qqline(rb2)   
grid()

#means and variances for each time period
x1<-cbind(rs1[-1],rb1)
Mu1<-c(0,0)
Sigma1<-cov(x1)
x2<-cbind(rs2[-1],rb2)
Mu2<-c(0,0)
Sigma2<-cov(x2)

##################################################
#kernel density estimate of real returns
##################################################
#kernel density estimate
par(mfrow=c(1,2))
fhat1<-kde(x1)
fhat2<-kde(x2)
plot(fhat1,display="slice",main='Residual kde (pre-1951)',xlab='S&P',ylab='1YTR',ylim=c(-0.3,0.3),xlim=c(-0.5,0.5),col='green')
points(x1,cex=0.5,pch=1,col='green')
plot(fhat2,display="slice",main='Residual kde (post-1951)',xlab='S&P',ylab='1YTR',ylim=c(-0.3,0.3),xlim=c(-0.5,0.5),col='red')
points(x2,cex=0.5,pch=1,col='red')

#builds function to sample from kde and plots samples
bivSample<-function(n,fhat){
  densities<-as.vector(fhat$estimate)/sum(fhat$estimate)
  temp<-cumsum(densities) #used to help turn uniform samples into bivariate samples from a given kernel density
  numCol<-ncol(fhat$estimate)
  u<-runif(n)
  for(i in 1:n){
    place<-head(which(temp>u[i]),1)
    r<-trunc(place/numCol) #row of sample
    c<-place-r*numCol #column of sample
    if(i==1) y<-c(fhat$eval.points[[1]][c],fhat$eval.points[[2]][r])
    else y<-rbind(y,c(fhat$eval.points[[1]][c],fhat$eval.points[[2]][r]))
  }
  return(y)
}

############################################
#Real return simulation function
#Needed for DCA and VA simulation
#Accounts for the autoregression in 1YTR
############################################
#ith row indicates ith year
realReturns<-function(n,check,t){
  if(check=='normal'){
    if(t=='pre'){
      Intercept<-matrix(rep(c(ar0s1$coefficients[1],ar1b1$coefficients[1]),n),ncol=2,byrow=T)
      temp<-Intercept+mvrnorm(n,Mu1,Sigma1)
      for(i in 2:n){
        temp[i,2]<-temp[i,2]+temp[(i-1),2]*ar1b1$coefficients[2]
      }
    }
    if(t=='post'){
      Intercept<-matrix(rep(c(ar0s2$coefficients[1],ar1b2$coefficients[1]),n),ncol=2,byrow=T)
      temp<-Intercept+mvrnorm(n,Mu2,Sigma2)
      for(i in 2:n){
        temp[i,2]<-temp[i,2]+temp[(i-1),2]*ar1b2$coefficients[2]
      }
    }
  }
  if(check=='kde'){
    if(t=='pre'){
      Intercept<-matrix(rep(c(ar0s1$coefficients[1],ar1b1$coefficients[1]),n),ncol=2,byrow=T)
      temp<-Intercept+bivSample(n,fhat1)
      for(i in 2:n){
        temp[i,2]<-temp[i,2]+temp[(i-1),2]*ar1b1$coefficients[2]
      }
    }
    if(t=='post'){
      Intercept<-matrix(rep(c(ar0s2$coefficients[1],ar1b2$coefficients[1]),n),ncol=2,byrow=T)
      temp<-Intercept+bivSample(n,fhat2)
      for(i in 2:n){
        temp[i,2]<-temp[i,2]+temp[(i-1),2]*ar1b2$coefficients[2]
      }
    }
  }
  return(temp+1)
}

###############################################
#DCA simulation 
set.seed(2)
###############################################
#change realReturns parameters as desired
M<-3000 #number of simulations
proportions<-seq(0,1,by=.1)
years<-c(2,6,12,20,30,42)
results<-list()
for(N in years){
  for(k in 1:M){
    temp2<-c()
    #first column stocks, second column bonds
    xSim<-realReturns(N,'normal','pre')
    X<-cbind(cumprod(rev(xSim[,1])),cumprod(rev(xSim[,2])))
    for(p in proportions){
      RR<-(p*sum(X[,1])+(1-p)*sum(X[,2])-N)/N
      temp2<-c(temp2,RR)
    }
    if(k==1) temp1<-temp2 else temp1<-rbind(temp1,temp2)
  }
  results[[N]]<-temp1
}
#in results, columns are for different percentages of stocks: 0% to 100% in 10% increments
#in results, rows are for different samples of returns
#results[[i]] indicates that the investment is maintained for i years

#holds other results from previous runs with different distributions
#so multiple results can be plotted at once
r1<-results
r2<-results
r3<-results
r4<-results

########################################
#VA simulation 
set.seed(2)
########################################
#change realReturns parameters as desired
M<-3000 #number of simulations
rate<-seq(0.02,.12,by=.005)
years<-c(2,6,12,20,30,42)
results<-list() #holds RR
investments<-list() #holds annual investments
for(N in years){
  for(k in 1:M){
    temp2<-c() #holds RR
    Temp2<-c() #holds A_k
    #first column stocks, second column bonds
    xSim<-realReturns(N,'kde','pre')
    for(r in rate){
      TI<-0 #total investment
      VoBI<-0 #value of bond investment
      VoSI<-0 #value of stock investment
      for(i in 1:N){
        Target<-(1+r)^(i-1)
        VoBI<-VoBI+VoSI-Target
        Temp2<-c(Temp2,VoBI)
        if(VoBI<0){
          TI<-TI-VoBI
          VoBI<-0
        }
        VoBI<-VoBI*xSim[i,2]
        VoSI<-Target*xSim[i,1]
      }
      RR<-(VoBI+VoSI)/TI-1
      temp2<-c(temp2,RR)
    }
    if(k==1) temp1<-temp2 else temp1<-rbind(temp1,temp2)
    if(k==1) Temp1<-Temp2 else Temp1<-rbind(Temp1,Temp2)
  }
  results[[N]]<-temp1
  investments[[N]]<-Temp1
}

#holds other results from previous runs with different distributions
#so multiple results can be plotted at once
r1<-results
r2<-results
r3<-results
r4<-results

#holds other investments from previous runs with different distributions
#so multiple investments can be plotted at once
i1<-investments
i2<-investments
i3<-investments
i4<-investments

####################################################
#DCA and VA empirical quantile visualizations
#Change plot labels and bounds as necessary
#xaxis<-proportions #for DCA
xaxis<-rate #for VA
####################################################
#plot 95% CI
par(mfrow=c(2,3))
for(N in years){
  avg<-colMeans(results[[N]])
  rDen<-apply(results[[N]],2,density)
  quants<-lapply(rDen,quantile.density,probs=c(0.025,0.975))
  quants<-matrix(unlist(quants),ncol=2,byrow = T) 
  plot(xaxis,avg,main=paste('Invest for',N,'years'),ylim=c(min(quants[,1]),max(quants[,2])),ylab='DCRR',xlab='Proportion invested in S&P')
  arrows(xaxis,quants[,1],xaxis,quants[,2],length=0.01,angle=90,code=3)
}

#plot quantile z for multiple results
z<-.975
par(mfrow=c(2,3))
for(N in years){
  q1<-apply(r1[[N]],2,quantile,z)
  q2<-apply(r2[[N]],2,quantile,z)
  q3<-apply(r3[[N]],2,quantile,z)
  q4<-apply(r4[[N]],2,quantile,z)
  plot(xaxis,q1,main=paste('Invest for',N,'years'),
       ylim=c(min(c(q1,q2,q3,q4)),max(c(q1,q2,q3,q4))),
       ylab='VARR',xlab='Rate',col='green',pch=1)
  points(xaxis,q2,col='red',pch=1)
  points(xaxis,q3,col='green',pch=3)
  points(xaxis,q4,col='red',pch=3)
}

#plot expectation for multiple results
par(mfrow=c(2,3))
for(N in years){
  q1<-apply(r1[[N]],2,mean)
  q2<-apply(r2[[N]],2,mean)
  q3<-apply(r3[[N]],2,mean)
  q4<-apply(r4[[N]],2,mean)
  plot(xaxis,q1,main=paste('Invest for',N,'years'),
       ylim=c(min(c(q1,q2,q3,q4)),max(c(q1,q2,q3,q4))),
       ylab='E[VARR]',xlab='Rate',col='green',pch=1)
  points(xaxis,q2,col='red',pch=1)
  points(xaxis,q3,col='green',pch=3)
  points(xaxis,q4,col='red',pch=3)
}

#plot standard deviation for multiple results
par(mfrow=c(2,3))
for(N in years){
  q1<-apply(r1[[N]],2,sd)
  q2<-apply(r2[[N]],2,sd)
  q3<-apply(r3[[N]],2,sd)
  q4<-apply(r4[[N]],2,sd)
  plot(xaxis,q1,main=paste('Invest for',N,'years'),
       ylim=c(min(c(q1,q2,q3,q4)),max(c(q1,q2,q3,q4))),
       ylab='sd(VARR)',xlab='Rate',col='green',pch=1)
  points(xaxis,q2,col='red',pch=1)
  points(xaxis,q3,col='green',pch=3)
  points(xaxis,q4,col='red',pch=3)
}

#plot Sharpe ratio for multiple results
par(mfrow=c(2,3))
for(N in years){
  q1<-apply(r1[[N]],2,mean)/apply(r1[[N]],2,sd)
  q2<-apply(r2[[N]],2,mean)/apply(r2[[N]],2,sd)
  q3<-apply(r3[[N]],2,mean)/apply(r3[[N]],2,sd)
  q4<-apply(r4[[N]],2,mean)/apply(r4[[N]],2,sd)
  plot(xaxis,q1,main=paste('Invest for',N,'years'),
       ylim=c(min(c(q1,q2,q3,q4)),max(c(q1,q2,q3,q4))),
       ylab='Sharpe ratio',xlab='Rate',col='green',pch=1)
  points(xaxis,q2,col='red',pch=1)
  points(xaxis,q3,col='green',pch=3)
  points(xaxis,q4,col='red',pch=3)
}

#plot annual investments
#only for va
h<-function(temp){
  temp[temp>0]<-0
  temp<-apply(temp,1,cumsum)
  return(-1*t(temp))
}
maxY<-20 #has to be in years
par(mfrow=c(2,3))
#annual investment
for(N in seq(1,21,by=4)){
  cols<-(1+(N-1)*maxY):(N*maxY)
  #lower quantiles
  lq1<-apply(i1[[maxY]][,cols],2,quantile,.025)
  lq2<-apply(i2[[maxY]][,cols],2,quantile,.025)
  lq3<-apply(i3[[maxY]][,cols],2,quantile,.025)
  lq4<-apply(i4[[maxY]][,cols],2,quantile,.025)
  #upper quantiles
  uq1<-apply(i1[[maxY]][,cols],2,quantile,.975)
  uq2<-apply(i2[[maxY]][,cols],2,quantile,.975)
  uq3<-apply(i3[[maxY]][,cols],2,quantile,.975)
  uq4<-apply(i4[[maxY]][,cols],2,quantile,.975)
  #medians
  mq1<-apply(i1[[maxY]][,cols],2,quantile,.5)
  mq2<-apply(i2[[maxY]][,cols],2,quantile,.5)
  mq3<-apply(i3[[maxY]][,cols],2,quantile,.5)
  mq4<-apply(i4[[maxY]][,cols],2,quantile,.5)
  plot(1:maxY,mq1,main=paste('Rate =',rate[N]),
       ylim=c(min(c(lq1,lq2,lq3,lq4)),max(c(uq1,uq2,uq3,uq4))),
       ylab='Annual investment',xlab='Year',col='green',pch=1)
  points(1:maxY,mq2,col='red',pch=1)
  points(1:maxY,mq3,col='green',pch=3)
  points(1:maxY,mq4,col='red',pch=3)
  
  points(1:maxY,lq1,col='green',pch=1)
  points(1:maxY,lq2,col='red',pch=1)
  points(1:maxY,lq3,col='green',pch=3)
  points(1:maxY,lq4,col='red',pch=3)
  
  points(1:maxY,uq1,col='green',pch=1)
  points(1:maxY,uq2,col='red',pch=1)
  points(1:maxY,uq3,col='green',pch=3)
  points(1:maxY,uq4,col='red',pch=3)
}

#total investment
for(N in seq(1,21,by=4)){
  cols<-(1+(N-1)*maxY):(N*maxY)
  #lower quantiles
  lq1<-apply(h(i1[[maxY]][,cols]),2,quantile,.025)
  lq2<-apply(h(i2[[maxY]][,cols]),2,quantile,.025)
  lq3<-apply(h(i3[[maxY]][,cols]),2,quantile,.025)
  lq4<-apply(h(i4[[maxY]][,cols]),2,quantile,.025)
  #upper quantiles
  uq1<-apply(h(i1[[maxY]][,cols]),2,quantile,.975)
  uq2<-apply(h(i2[[maxY]][,cols]),2,quantile,.975)
  uq3<-apply(h(i3[[maxY]][,cols]),2,quantile,.975)
  uq4<-apply(h(i4[[maxY]][,cols]),2,quantile,.975)
  #medians
  mq1<-apply(h(i1[[maxY]][,cols]),2,quantile,.5)
  mq2<-apply(h(i2[[maxY]][,cols]),2,quantile,.5)
  mq3<-apply(h(i3[[maxY]][,cols]),2,quantile,.5)
  mq4<-apply(h(i4[[maxY]][,cols]),2,quantile,.5)
  plot(1:maxY,mq1,main=paste('Rate =',rate[N]),
       ylim=c(min(c(lq1,lq2,lq3,lq4)),max(c(uq1,uq2,uq3,uq4))),
       ylab='Total investment',xlab='Year',col='green',pch=1)
  points(1:maxY,mq2,col='red',pch=1)
  points(1:maxY,mq3,col='green',pch=3)
  points(1:maxY,mq4,col='red',pch=3)
  
  points(1:maxY,lq1,col='green',pch=1)
  points(1:maxY,lq2,col='red',pch=1)
  points(1:maxY,lq3,col='green',pch=3)
  points(1:maxY,lq4,col='red',pch=3)
  
  points(1:maxY,uq1,col='green',pch=1)
  points(1:maxY,uq2,col='red',pch=1)
  points(1:maxY,uq3,col='green',pch=3)
  points(1:maxY,uq4,col='red',pch=3)
}

######################################################
#DCA theoretical visualizations
######################################################
#DCRR expected value
ExpDCRR<-function(k,p,ExpDC1,ExpDC2){
  (p*ExpDC1[k]+(1-p)*ExpDC2[k]-k)/k
}
#DCRR variance
VarDCRR<-function(k,p,VarDC1,VarDC2,CovDC12){
  (p^2*VarDC1[k]+(1-p)^2*VarDC2[k]+2*p*(1-p)*CovDC12[k])/(k^2)
}
#DCRR variance argmin
pVarDCRR<-function(n,VarDC1,VarDC2,CovDC12){
  (VarDC2[n]-CovDC12[n])/(VarDC1[n]+VarDC2[n]-2*CovDC12[n])
}
#DCRR optimal sharpe arg
pSharp<-function(n,ExpDC1,ExpDC2,VarDC1,VarDC2,CovDC12){
  -1*(n*(CovDC12[n]-VarDC2[n])-ExpDC2[n]*CovDC12[n]+ExpDC1[n]*VarDC2[n])/(VarDC1[n]*(n-ExpDC2[n])+n*(VarDC2[n]-2*CovDC12[n])+CovDC12[n]*(ExpDC1[n]+ExpDC2[n])-ExpDC1[n]*VarDC2[n])
}
#optional test to verify argmax or argmin of optimal sharpe arg
minmaxpSharp<-function(n,ExpDC1,ExpDC2,VarDC1,VarDC2,CovDC12){
  2*(n*(VarDC1[n]+VarDC2[n]-2*CovDC12[n])+ExpDC1[n]*(CovDC12[n]-VarDC2[n])+ExpDC2[n]*(CovDC12[n]-VarDC1[n]))/(n^3)
}

#changge Mu and Sigma as necessary
Mu<-mean(cbind(s1,b1))
Sigma<-cov(cbind(s1,b1))
ExpDC1<-c(Mu[1])
ExpDC2<-c(Mu[2])
VarDC1<-c(Sigma[1,1])
VarDC2<-c(Sigma[2,2])
CovDC12<-c(Sigma[1,2])
for(k in 2:60){
  ExpDC1<-c(ExpDC1,Mu[1]*(1+tail(ExpDC1,1)))
  ExpDC2<-c(ExpDC2,Mu[2]*(1+tail(ExpDC2,1)))
  VarDC1<-c(VarDC1,(Sigma[1,1]+Mu[1]^2)*(tail(VarDC1,1)+(tail(ExpDC1,1)/Mu[1])^2)-tail(ExpDC1,1)^2)
  VarDC2<-c(VarDC2,(Sigma[2,2]+Mu[2]^2)*(tail(VarDC2,1)+(tail(ExpDC2,1)/Mu[2])^2)-tail(ExpDC2,1)^2)
  CovDC12<-c(CovDC12,Sigma[1,2]*(tail(ExpDC1,1)/Mu1[1]+tail(ExpDC2,1)/Mu[2]-1)+tail(CovDC12,1))
}

#stores second set of results so both can be plotted
sExpDC1<-ExpDC1
sExpDC2<-ExpDC2
sVarDC1<-VarDC1
sVarDC2<-VarDC2
sCovDC12<-CovDC12

#plot expectation 1 set of results
par(mfrow=c(2,3))
for(N in c(2,6,12,20,30,42)){
  xaxis<-seq(0,1,by=.01)
  Y<-ExpDCRR(N,xaxis,ExpDC1,ExpDC2)
  plot(xaxis,Y,main=paste('Invest for',N,'years'),ylab='E[DCRR]',xlab='Proportion invested in S&P',type='l')
}

#plot expectation 2 sets of results
par(mfrow=c(2,3))
for(N in c(2,6,12,20,30,42)){
  xaxis<-seq(0,1,by=.01)
  Y1<-ExpDCRR(N,xaxis,sExpDC1,sExpDC2)
  Y2<-ExpDCRR(N,xaxis,ExpDC1,ExpDC2)
  plot(xaxis,Y1,main=paste('Invest for',N,'years'),ylim=c(min(Y1,Y2),max(Y1,Y2)),ylab='E[DCRR]',xlab='Proportion invested in S&P',type='l',col='green')
  lines(xaxis,Y2,col='red')
}

#plot standard deviation 1 set of results
par(mfrow=c(2,3))
for(N in c(2,6,12,20,30,42)){
  xaxis<-seq(0,1,by=.01)
  Y<-VarDCRR(N,xaxis,VarDC1,VarDC2,CovDC12)
  plot(xaxis,Y,main=paste('Invest for',N,'years'),ylab='Var(DCRR)',xlab='Proportion invested in S&P',type='l')
}

#plot standard deviation 2 sets of results
par(mfrow=c(2,3))
for(N in c(2,6,12,20,30,42)){
  xaxis<-seq(0,1,by=.01)
  Y1<-sqrt(VarDCRR(N,xaxis,sVarDC1,sVarDC2,sCovDC12))
  Y2<-sqrt(VarDCRR(N,xaxis,VarDC1,VarDC2,CovDC12))
  plot(xaxis,Y1,main=paste('Invest for',N,'years'),ylab='sd(DCRR)',ylim=c(min(Y1,Y2),max(Y1,Y2)),xlab='Proportion invested in S&P',type='l',col='green')
  lines(xaxis,Y2,col='red')
}

#plot variance argmin 1 set of results
par(mfrow=c(1,1))
xaxis<-2:60
ps<-lapply(xaxis,pVarDCRR,VarDC1,VarDC2,CovDC12)
plot(xaxis,ps,main=paste('Argmin variance of DCRR'),ylab='p',xlab='Years')

#plot variance argmin 2 sets of results
par(mfrow=c(1,1))
xaxis<-2:60
Y1<-lapply(xaxis,pVarDCRR,sVarDC1,sVarDC2,sCovDC12)
Y1<-unlist(Y1)
Y2<-lapply(xaxis,pVarDCRR,VarDC1,VarDC2,CovDC12)
Y2<-unlist(Y2)
plot(xaxis,Y1,main='Argmin variance of DCRR',ylim=c(min(Y1,Y2),max(Y1,Y2)),ylab='p',xlab='Years',col='green')
points(xaxis,Y2,col='red')

#plot Sharpe ratio 1 set of results
par(mfrow=c(2,3))
for(N in c(2,6,12,20,30,42)){
  xaxis<-seq(0,1,by=.01)
  Y<-ExpDCRR(N,xaxis,ExpDC1,ExpDC2)/sqrt(VarDCRR(N,xaxis,VarDC1,VarDC2,CovDC12))
  plot(xaxis,Y,main=paste('Invest for',N,'years'),ylab='Sharpe ratio',xlab='Proportion invested in S&P',type='l')
}

#plot Sharpe ratio 2 sets of results
par(mfrow=c(2,3))
for(N in c(2,6,12,20,30,42)){
  xaxis<-seq(0,1,by=.01)
  Y1<-ExpDCRR(N,xaxis,sExpDC1,sExpDC2)/sqrt(VarDCRR(N,xaxis,sVarDC1,sVarDC2,sCovDC12))
  Y2<-ExpDCRR(N,xaxis,ExpDC1,ExpDC2)/sqrt(VarDCRR(N,xaxis,VarDC1,VarDC2,CovDC12))
  plot(xaxis,Y1,main=paste('Invest for',N,'years'),ylab='Sharpe ratio',ylim=c(min(Y1,Y2),max(Y1,Y2)),xlab='Proportion invested in S&P',type='l',col='green')
  lines(xaxis,Y2,col='red')
}

#plot optimal Sharpe ratio 1 set of results
par(mfrow=c(1,1))
xaxis<-2:60
ps<-lapply(xaxis,pSharp,ExpDC1,ExpDC2,VarDC1,VarDC2,CovDC12)
plot(xaxis,ps,main=paste('Argmax Sharpe ratio of DCRR'),ylab='p',xlab='Years')

#plot optimal Sharpe ratio 2 sets of results
par(mfrow=c(1,1))
xaxis<-2:60
Y1<-lapply(xaxis,pSharp,sExpDC1,sExpDC2,sVarDC1,sVarDC2,sCovDC12)
Y1<-unlist(Y1)
Y2<-lapply(xaxis,pSharp,ExpDC1,ExpDC2,VarDC1,VarDC2,CovDC12)
Y2<-unlist(Y2)
plot(xaxis,Y1,main='Argmax Sharpe ratio of DCRR',ylim=c(min(Y1,Y2),max(Y1,Y2)),ylab='p',xlab='Years',col='green')
points(xaxis,Y2,col='red')

#plot max Sharpe ratio 2 sets of results
par(mfrow=c(1,1))
xaxis<-2:60
Y1<-lapply(xaxis,pSharp,sExpDC1,sExpDC2,sVarDC1,sVarDC2,sCovDC12)
Y1<-unlist(Y1)
Y2<-lapply(xaxis,pSharp,ExpDC1,ExpDC2,VarDC1,VarDC2,CovDC12)
Y2<-unlist(Y2)
Y1<-ExpDCRR(xaxis,Y1,sExpDC1,sExpDC2)/sqrt(VarDCRR(xaxis,Y1,sVarDC1,sVarDC2,sCovDC12))
Y2<-ExpDCRR(xaxis,Y2,ExpDC1,ExpDC2)/sqrt(VarDCRR(xaxis,Y2,VarDC1,VarDC2,CovDC12))
plot(xaxis,Y1,main='Max Sharpe ratio of DCRR',ylim=c(min(Y1,Y2),max(Y1,Y2)),ylab='Sharpe ratio',xlab='Years',col='green')
points(xaxis,Y2,col='red')
