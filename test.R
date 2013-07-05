system("python 1.py")

pdf=matrix(c(1/3,1/3,1/3,10,-3,-1),nrow=3)

pdf=matrix(c(1/3,1/3,1/3,10,-3,-1),nrow=3,byrow=F,dimnames=list(c('case1','case2','case3'),c('freq','val')))
mean_=sum((pdf[,2])*pdf[,1])
variance=sum((pdf[,2]-mean_)^2*pdf[,1])

pdf=matrix(c(122/429,22/39,5/33,10,-3,-1),nrow=3,byrow=F,dimnames=list(c('case1','case2','case3'),c('freq','val')))
mean_=sum((pdf[,2])*pdf[,1])
variance=sum((pdf[,2]-mean_)^2*pdf[,1])
