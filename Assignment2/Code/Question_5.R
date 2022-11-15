library(quantmod)

getSymbols('TCS.NS')

getSymbols('^NSEI')

TCS_rt = diff(log(TCS.NS$TCS.NS.Adjusted))
Nifty_rt = diff(log(NSEI$NSEI.Adjusted))
retrn = cbind.xts(TCS_rt,Nifty_rt)
retrn = na.omit(data.frame(retrn))

beta = cov(retrn$NSEI.Adjusted,retrn$TCS.NS.Adjusted)/var(retrn$NSEI.Adjusted)

alpha = mean(retrn$TCS.NS.Adjusted) - mean(retrn$NSEI.Adjusted)*beta

sigma = sqrt(var(retrn$TCS.NS.Adjusted) - cov(retrn$NSEI.Adjusted,retrn$TCS.NS.Adjusted)*beta)

linear_model = lm(TCS.NS.Adjusted ~ NSEI.Adjusted, data=retrn)
summary(linear_model)