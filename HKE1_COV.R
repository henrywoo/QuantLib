#read var-cov matrix

#31MAY2012  Annualized Variance/Covariance in units of %-squared. 

#Risk Exposure 1 – Size SIZE F6.3 80-85
#Risk Exposure 2 – Yield YIELD F6.3 87-92
#Risk Exposure 3 – Book to Price BTOP F6.3 94-99
#Risk Exposure 4 – Earnings to Price ETOP F6.3 101-106
#Risk Exposure 5 – Liquidity LIQ F6.3 108-113
#Risk Exposure 6 – Success SUCCESS F6.3 115-120
#Risk Exposure 7 – Volatility VIM F6.3 122-127
#Risk Exposure 8 – S&P500 Return Sensitivity SAP5 F6.3 129-134
#Risk Exposure 9 – Redchip REDCHIP F6.3 136-141
#Risk Exposure 10 – Hang Seng Index Membership HANGSENG F6.3 143-148
#1 FINANCE Finance and Banking
#2 PROPERT Properties
#3 DIVERSE Diversified Holding Companies
#4 UTILITY Utilities
#5 CHINESE Chinese Enterprises
#6 TRANSPOR Transportation and Storage
#7 CONSUMER Consumer Goods and Services
#8 ELECT Electronics and Electrical Appliances
#9 TEXTILES Textiles
#10 INDUST Industrial
#11 HOTELS Hotel, Leisure, and Food
#12 MEDIA Media and Telecommunication
#13 CONSTRUC Construction

con = file("P:\\WQ\\barra\\HK\\HKE11205.COV", "r")
line=readLines(con,n=1)
txt = c()
while( length(line) != 0 ) {
  #print(line)
  line=readLines(con,n=1)
  line=gsub(" +\"","\"",line)
  line=gsub(",$",'',line)
  txt =c(txt,line)
}
close(con)
print(txt)

b=read.csv(header=T,sep=",",quote="\"",text=txt,skip=1)