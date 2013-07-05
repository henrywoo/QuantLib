library(TTR)
library(quantmod)
library(fPortfolio)

Q2.1.1=function(m,graphnum){
  y=readSeries("2012-13.FE5101.A2.data.csv",header=TRUE,sep=',',skip=3)
  price=y$PX_LAST
  #if(graphnum==2){par(mfrow=c(1,2),oma=c(0,0,0,0), mar=c(5.5,2,1,0.1))}
  r_compound = ts(diff(log(y))[-1])
  sdev=r_compound^2
  variance=SMA(sdev,n=m)
  sigma=sqrt(variance)
  if(graphnum==2){
    plot(sqrt(sdev),type='l',col='peachpuff2',sub='abs(deviation)',xlab=paste('window size=',m),ylab='')
  }
  plot(sigma,type='l',col='black',sub='volatility',xlab=paste('window size=',m),ylab='')
  #par(las=0)
}
#1. when window size=1, variance is the same as square of return of that day
Q2.1.1(1,2)
#2. As window size increased, the curve becomes more and more smooth
Q2.1.1(15,1)
Q2.1.1(60,1)


BondPrice = function(r,alpha,coupon,n,par){
  P0=(1+r)^(1-alpha)*((coupon/r)*(1-1/(1+r)^n)+par/(1+r)^n)
}
