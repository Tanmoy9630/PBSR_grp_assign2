### q4_part-B (LAPLACE DISTRIBUTION)

rm(list=ls())
library(MASS)
plot(Insurance$Holders,Insurance$Claims
     ,xlab =
       'Holders'
     ,ylab=
       'Claims'
     ,pch=
       20
)
grid()
attach(Insurance)
data=Insurance
data=data.frame(cbind(Claims,Holders))



### negative log liklihood

library(jmuOutlier)
ngll2=function(theta,data){
  b0=theta[1]
  b1=theta[2]
  sig=(theta[3])
  l=0
  n=nrow(data)
  
  for(i in 1:n){
    l=l+log(dlaplace(data[,1][i]-b0-b1*data[,2][i],0,sig))
    
  }
  return(-l)
}
theta_initial=c(9,0.1,50)
ngll2(theta_initial,data)
fit33=optim(theta_initial,ngll2,data=data)
fit33
n=nrow(data)
BIC3=log(n)*(3)-2*(-1*fit33$value)
BIC3
plot(Claims~Holders)
abline(a=fit33$par[1],b=fit33$par[2])
