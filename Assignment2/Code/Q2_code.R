#Question2
Question2<-function(n,sim_size,alpha,sigma)
{
  LogLikeFunc=function(data,para)
  {
    l =sum(dgamma(data,shape = para[1],scale = para[2],log=T))
    return(-l)
  }
  MyMLE=function(data){
    s=var(data)/mean(data)
    a=mean(data)/s
    initial=c(a,s)
    fit=optim(initial,LogLikeFunc,data=data)
    return(log(fit$par[1]))
  }
  v=c()
  for(i in 1:sim_size){
    x=rgamma(n=n,shape = alpha,scale=sigma)
    v=append(v,MyMLE(x),after = length(v))
  }
  hist(v,col="purple")
  abline(v=log(1.5),col="red")
  q=quantile(v,probs = c(2.5,97.5)/100)
  r=as.data.frame(q)
  d=r$q[2]-r$q[1]
  print('gap between 2.5 and 97.5 percentile points')
  d
}
Question2(20,1000,1.5,2.2)
Question2(40,1000,1.5,2.2)
Question2(100,1000,1.5,2.2)

