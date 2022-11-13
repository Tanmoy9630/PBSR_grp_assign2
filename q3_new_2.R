### question no. :3

rm(list=ls())

attach(faithful)
data=faithful
waiting=sort(waiting)



############# gamma + normal model :


NegLogLikeMix=function(theta,data){
  shape=(theta[1]) 
  scale=(theta[2])  ### when we will use optim() it will search for sigma in entire real line but sigma always > 0, to take only +ve values
  ### exp() is one-one function
  mu=theta[3]
  sigma2=exp(theta[4])
  p=exp(theta[5])/(1+exp(theta[5]))
  n=length(data)
  l=0
  for(i in 1:n){
    l=l+log(p*dgamma(data[i],shape,scale)+(1-p)*dnorm(data[i],mu,sigma2))
  }  
  return(-l)
}

theta_initial=c(74,.7,80,6,0.5)

fit=optim(theta_initial,NegLogLikeMix,control=list(maxit=10000),data =data$waiting)
fit


theta_hat1=fit$par
shape_hat=(theta_hat1[1])
scale_hat=(theta_hat1[2])  
mu2_hat=theta_hat1[3]
sigma2_hat=exp(theta_hat1[4])
p_hat_1=exp(theta_hat1[5])/(1+exp(theta_hat1[5]))

shape_hat
scale_hat
mu2_hat
sigma2_hat
p_hat_1
d_1st=(p_hat_1*dgamma(waiting,shape_hat,scale_hat))+((1-p_hat_1)*dnorm(waiting,mu2_hat,sigma2_hat))
l1=NegLogLikeMix(theta_hat1,waiting)
l1
#plot(d_1st)

hist(waiting,probability = T)
lines(waiting,d_1st,lwd=2,col="red")

###### gamma + gamma model :

LogLikeMix2=function(theta,data){
  shape1=(theta[1]) 
  scale1=(theta[2])  
  shape2=(theta[3])
  scale2=(theta[4])
  p=(theta[5])
  n=length(data)
  l=0
  for(i in 1:n){
    l=l+log(p*dgamma(data[i],shape1,scale1)+(1-p)*dgamma(data[i],shape2,scale2))
  }  
  return(-l)
}
theta_initial=c(74,.7,88,.9,0.5)
LogLikeMix2(theta_initial,waiting)

fit1=optim(theta_initial,LogLikeMix2,control=list(maxit=10000),data =waiting)
fit1

theta_hat2=fit1$par
shape_hat1=theta_hat2[1]
scale_hat1=(theta_hat2[2])  ### when we will use optim() it will search for sigma in entire real line but sigma always > 0, to take only +ve values
### exp() is one-one function
shape_hat2=theta_hat2[3]
scale_hat2=(theta_hat2[4])
p_hat_2=theta_hat2[5]

shape_hat1
scale_hat1
shape_hat2
scale_hat2
p_hat_2
d_2nd=p_hat_2*dgamma(waiting,shape_hat1,scale_hat1)+(1-p_hat_2)*dgamma(waiting,shape_hat2,scale_hat2)

lines(waiting,d_2nd,lwd=2,col='blue')
l2=LogLikeMix2(theta_hat2,waiting)
l2
#############################################################
#### log normal +log normal model
ll3=function(data,theta)
{
  meu1=(theta[1]) 
  sig1=exp(theta[2])  
  meu2=(theta[3])
  sig2=exp(theta[4])
  p=exp(theta[5])/(1+exp(theta[5]))
  n=length(data)
  l=0
  for(i in 1:n){
    l=l+log(p*dlnorm(data[i],meu1,sig1)+(1-p)*dlnorm(data[i],meu2,sig2))
  }  
  return(-l)
}

theta_initial=c(3.9,0.015,4.4,0.012,0.5)
ll3(waiting,theta_initial)

fit3=optim(theta_initial,ll3,control=list(maxit=10000),data =waiting)
fit3



theta_hat3=fit3$par
mu1_hat1=theta_hat3[1]
sig_hat1=exp(theta_hat3[2])  ### when we will use optim() it will search for sigma in entire real line but sigma always > 0, to take only +ve values
### exp() is one-one function
mu2_hat2=theta_hat3[3]
sig_hat2=exp(theta_hat3[4])
p_hat_3=exp(theta_hat3[5])/(1+exp(theta_hat3[5]))

mu1_hat1
sig_hat1
mu2_hat2
sig_hat2
p_hat_3
d_3rd=p_hat_3*dlnorm(waiting,mu1_hat1,sig_hat1)+(1-p_hat_3)*dlnorm(waiting,mu2_hat2,sig_hat2)

lines(waiting,d_3rd,lwd=2,col='yellow')
l3=ll3(waiting,theta_hat3)
l3
### for aic fitting ###########

### aic= (2*number_of_parameter)-loglikelihhod_function(theta_hat,data)

aic1=2*5-l1
aic2=2*5-l2
aic3=2*5-l3
aic1
aic2
aic3
aic_table= data.frame (
  models = c("gamma+normal", "gamma+gamma", "lognormal+lognormal"),
  AIC = c(aic1,aic2,aic3))

aic_table


### parameters of ln + ln model :

mu1_hat1
sig_hat1
mu2_hat2
sig_hat2
p_hat_3
#### p() calc :

dmix=function(x,theta){
  mu1=theta[1]
  sigma1=theta[2]
  mu2=theta[3]
  sigma2=theta[4]
  p=theta[5]
  f=p*dlnorm(x,mu1,sigma1)+(1-p)*dlnorm(x,mu2,sigma2)
  return(f)
}  
#P(60 < waiting < 70)
P=integrate(dmix,60,70,c(mu1_hat1,sig_hat1,mu2_hat2,sig_hat2,p_hat_3))  ##p(70<waiting<100) 
P

