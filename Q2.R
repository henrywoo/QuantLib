#
library(TTR)
library(qcc)
library(quantmod)
library(fGarch)
library(rugarch)

library(fPortfolio)
library(PerformanceAnalytics)
library(timeSeries)

Return.calculate <-
  function(prices, method = c("discrete","log"))
  { # @ author Peter Carl
    
    #  Calculate returns from a price stream
    
    # Required inputs
    
    # Prices: data object containing ordered price observations
    # method: "simple", "compound"
    
    # FUNCTION:
    
    method = method[1]
    pr = checkData(prices, method = "xts")
    
    if(method=="simple" || method=='discrete'){
      #Returns = pr/pr[-nrow(pr), ] - 1
      Returns = pr/xts:::lagts.xts(pr) - 1
      xtsAttributes(Returns) <- list(ret_type="discrete")
    }
    if(method=="compound" || method=='log') {
      Returns = diff(log(pr))
      xtsAttributes(Returns) <- list(ret_type="log")
    }
    
    reclass(Returns,match.to=pr)
  }

#' @rdname Return.calculate
#' @export 
CalculateReturns <- 
  function(prices, method = c("discrete","log"))
  { # @ author Peter Carl
    Return.calculate(prices=prices, method=method)
  }

#
#x=xts(read.csv("2012-13.FE5101.A2.data.csv",header=T,skip=3))
#
# 1.


Q2.1=function(m){
  y=readSeries("2012-13.FE5101.A2.data.csv",header=TRUE,sep=',',skip=3)
  price=y$PX_LAST
  par(mfrow=c(1,2),oma=c(0,0,0,0), mar=c(5.5,2,1,0.1))
  ##plot(price,type='l',col='red',sub='price',xlab='',ylab='' )
  #square=function(x){x^2}
  #r_simple   = Return.calculate(y,method='simple')
  #r_compound = Return.calculate(y,method='compound')
  r_compound = ts(diff(log(y))[-1])
  ##plot(r_compound,type='l',col='blue',sub='return',xlab='',ylab='')
  sdev=r_compound^2
  ##plot(sdev,type='l',col='peachpuff2',sub='squared deviation',xlab='',ylab='')
  #s=fapply(r,start(r),end(r),square)
  variance=SMA(sdev,n=m)
  plot(sqrt(sdev),type='l',col='peachpuff2',sub='abs(deviation)',xlab=paste('window size=',m),ylab='')
  plot(sqrt(variance),type='l',col='black',sub='volatility',xlab=paste('window size=',m),ylab='')
  #s = r_compound^2
  #unweighted_daily_variance=sum(s$PX_LAST,na.rm=T)/length(s)
  par(las=0)
}
Q2.1(20)

Q2.1.1=function(m){
  y=readSeries("2012-13.FE5101.A2.data.csv",header=TRUE,sep=',',skip=3)
  price=y$PX_LAST
  par(mfrow=c(1,2),oma=c(0,0,0,0), mar=c(5.5,2,1,0.1))
  r_compound = ts(diff(log(y))[-1])
  sdev=r_compound^2
  variance=SMA(sdev,n=m)
  plot(sqrt(sdev),type='l',col='peachpuff2',sub='abs(deviation)',xlab=paste('window size=',m),ylab='')
  plot(sqrt(variance),type='l',col='black',sub='volatility',xlab=paste('window size=',m),ylab='')
  par(las=0)
}
#1. when window size=1, variance is the same as square of return of that day
Q2.1.1(1)
#2. As window size increased, the curve becomes more and more smooth
Q2.1.1(15)
Q2.1.1(30)
Q2.1.1(60)




# 2.
library(rugarch)
lambda=0.94



