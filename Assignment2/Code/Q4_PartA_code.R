##Question 4
library(SciViews)
library(MASS)
Holders=Insurance$Holders
Claims=Insurance$Claims
n=length(Holders)
## Part A
Negloglike=function(data,theta)
{
  l=0
  for(i in 1:n)
  {
    l=l+dnorm(data[,1][i],mean = theta[1]+(theta[2]*data[,2][i]),sd=exp(theta[3]),log = TRUE)
  }
  return(-l)
}
theta_initial=c(5,1,50)
data=data.frame(cbind(Claims,Holders))
fit=optim(theta_initial,Negloglike,data=data)
fit
BIC=ln(n)*(length(fit$par))+2*fit$value ## calculating BIC with the formula
BIC
plot(Claims~Holders)
abline(a=fit$par[1],b=fit$par[2])

