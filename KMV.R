#KVM

# s -short term liabilities
# l- long term liabilities
"kmv.default_value" = function(s,l){
  if (l/s < 1.5){
    s+0.5*l
  }else{
    s+(0.7-(0.3*s/l))*l
  }
}

# r - expected asset return
# d - default threshold
# sigma - std/vol of expected asset return
"kmv.dd" = function(r,d,sigma){
  return((r-d)/sigma)
}

"kmv.dd2" = function(V,d,ROA,sigma,t){
  (log(V)-log(d)+(ROA-(sigma^2)/2)*t)/(sigma*sqrt(t))
}

# probability of default
"kmv.pd" = function(dd){
  pnorm(-dd)
}

dv=kmv.default_value(1000,1400)
print(dv)
r=6000
sigma=0.2*r
dd=kmv.dd(r,dv,sigma)
print(dd)