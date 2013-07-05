#EWMA Model
#library(TTR)
#library(quantmod)
#library(fPortfolio)
library(fGarch)


par(mfrow=c(2,2))
y=read.csv("2012-13.FE5101.A2.data.csv",header=F,sep=',')
x=y$V2
plot(x,type='l')
acf(x)
acf(x^2)

#y=read.csv("Q2.4.csv",header=F,sep=',')