library(fPortfolio)
library(RODBC)


#insert into Database

ReadRSK=function (f){
  # read barra risk file into a text array
  #con = file("P:\\WQ\\barra\\HK\\HKE11205.RSK", "r")
  con = file(f, "r")
  line=readLines(con,n=1)
  as.Date(line,"%d%b%y") # %b -> MAY
  cat("Date:",line)
  txt = c()
  while( length(line) != 0 ) {
    #print(line)
    line=readLines(con,n=1)
    line=gsub(" +\"","\"",line)
    line=gsub(",$",'',line)
    txt =c(txt,line)
  }
  close(con)
  
  #read all the file and get the historical return
  #TODO
  
  
  #substr(),nchar(), grep(), regexpr(), sub(), gsub()
  
  #colnames=c("BARRID","SEDOL","HKSE_ID","NAME","HBTA","BETA","SRISK%","TRISK%","SIZE",
  #           "YIELD","BTOP","ETOP","LIQ","SUCCESS","VIM","SAP5","REDCHIP","HANGSENG",
  #           "INDNAME","IND","PRICE","CAPITALIZATION","YLD%","ESTU","XXX");
  #b=read.csv("E:\\WQ\\barra\\HK\\HKE11205.RSK",skip=2,header=FALSE,sep=",",quote="\"",col.names=colnames);
  
  b=read.csv(header=T,sep=",",quote="\"",text=txt);
  #b1 = as.double(b[ b$HKSE_ID=='101', ]['BETA'])
  
  #summary(b$BETA)
  #b[b$BETA==-0.041,]
  #b[b$YIELD==2.972,]
  
  ys = b[ , ]['YIELD']
  for (i in 1:nrow(ys)){
    if (ys$YIELD[i]>0){
      name=as.character(b$NAME[i])
      hkid=as.character(b$HKSE_ID[i])
      beta=as.character(b$BETA[i])
      yild=as.character(b$YIELD[i])
      ind=as.character(b$IND[i])
      print(name)
      print(beta)
      print(hkid)
      print(yild)
      print(ind)
    }
  }
  
}



for (i in dir("P:\\WQ\\barra\\HK",pattern="*.RSK")){
  print (i)
  fname= paste("P:\\WQ\\barra\\HK\\",i,sep='')
  ReadRSK(fname)
  break;
}




EF= function(rho_){
  rho=rho_ # correlation
  
  v1=0.398 # volatility
  v2=0.426
  
  r1=0.59 # yield
  r2=0.81
  
  n=1000
  
  tmp=sort(abs(rnorm(n)))
  ww=tmp/max(tmp+0.01)
  remove(tmp)
  
  eport=function(w1,r1,r2){w1*r1+(1-w1)*r2;}
  vport=function(w1,v1,v2,rho){w1^2*v1+(1-w1)^2*v2+2*w1*(1-w1)*rho*sqrt(v1*v2);}
  
  #trisk=array()
  #expectedreturn=array()
  #for (i in 1:n-1){trisk=append(trisk,sqrt(vport(ww[i],v1,v2,rho)))}
  #for (i in 1:n-1){expectedreturn=append(expectedreturn,eport(ww[i],r1,r2))}
  
  trisk=c()
  expectedreturn=c()
  for (i in 1:n-1){trisk=c(trisk,sqrt(vport(ww[i],v1,v2,rho)))}
  for (i in 1:n-1){expectedreturn=c(expectedreturn,eport(ww[i],r1,r2))}
  
  plot(trisk, expectedreturn, col='brown',
       xlab='total risk[sigma]',ylab='expected return',
       main='efficient frontiner')
  
  min(trisk)
}