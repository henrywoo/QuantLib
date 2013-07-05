#EWMA Model
library(TTR)
library(quantmod)
library(fPortfolio)

Q2.2=function(){
  y=readSeries("2012-13.FE5101.A2.data.csv",header=TRUE,sep=',',skip=3)
  price=y$PX_LAST
  r_compound = ts(diff(log(y))[-1])
  sdev=r_compound^2
  variance2<-HoltWinters(sdev,alpha=.94,gamma=F, beta=F)
  paste(head(sqrt(variance2$x),n=5))
  plot(variance2,type='l',col='black',sub='volatility',xlab='',ylab='')
}

Q2.2()


