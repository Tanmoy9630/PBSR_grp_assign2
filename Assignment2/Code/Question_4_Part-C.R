## Part C

library(SciViews)
library(MASS)
library(jmuOutlier)
Holders=Insurance$Holders
Claims=Insurance$Claims
data=data.frame(cbind(Claims,Holders))
data = data[-61,]
n=length(Holders)-1

Negloglike=function(data,theta)
{
  l=0
  for(i in 1:n)
  {
    l=l+log(dlnorm(data[,1][i] , theta[1]+theta[2]*log(data[,2][i]), theta[3]))
  }
  return(-l)
}

theta_initial=c(8,0.1,50)

fit=optim(theta_initial,Negloglike,data=data)
fit


plot(log(Claims)~log(Holders))
abline(a=fit$par[1],b=fit$par[2], col ="red")

BIC=ln(n)*(length(fit$par))+2*fit$value
#BIC value is:
BIC 
