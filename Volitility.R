#
#volatility
#

forwardVol = function(vol1, t1, vol2, t2){
  assert(t2<t1)
  sqrt((vol2^2*t2-vol1^2*t1)/(t2-t1))
}

print(forwardVol(0.12,3/12,0.13,0.5))

print(forwardVol(0.165,1/12,0.178,2/12))

strike_call = function(spot,tenor,rawdelta,vol){
  return(spot*(exp(-qnorm(rawdelta)*vol*sqrt(tenor)+vol^2*theta*tenor/2)));
}

strike_put = function(spot,tenor,rawdelta,vol){
  return(spot*(exp(-qnorm(rawdelta)*vol*sqrt(tenor)+vol^2*theta*tenor/2)));
}

print(strikeforvolanddelta(100,0.509589,0.25,0.21,1,1));