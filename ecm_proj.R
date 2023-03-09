library(quantmod)
library('urca')
library("dplyr")

macro<-c("WM2NS","M2REAL")
start<-"2010-01-01"
end<-"2023-03-08"

SPX<- getSymbols("^GSPC",auto.assign = FALSE, from = start, to = end)[,6]
getSymbols(macro,src='FRED', from = start, to = end)

#data
df<-na.omit(data.frame(cbind(SPX,M2REAL)))
plot(df$M2REAL, df$GSPC.Adjusted, main = "RealM2 and SP500", xlab ='M2 Real', ylab = "SP500", pch = 16, col = 'blue')

#df.temp<-tail(df,5)
#plot(df.temp$WM2NS, df.temp$TB3MS)

#cointegration testing
jotest=ca.jo(df, type="eigen", K=2, ecdet="none", spec="transitory")
summary(jotest)


#check stationary and plot
sp<-as.matrix(df)%*%as.matrix(jotest@V[,1])
plot(sp, type = 'l', main = 'Residuals',xlab='time',ylab='resid')
summary(ur.df(sp,'drift',lags= 1))

#ols
reg<-lm(df$GSPC.Adjusted~ df$M2REAL)
summary(reg)
z = reg$residuals
plot(z, type = 'l', main = 'residuals')
summary(ur.df(z,'none',lags= 1))

#add z
df$z<-z



#ecm data prep
ecm_prep<-function(df, l){
  #var1, var2, residual
  z.1<-lag(df[,3], k = 1)[2:nrow(df)]
  delta.v1<- diff(df[,1], lag = 1)
  delta.v2<- diff(df[,2], lag = 1)
  delta.v1.1<-lag(delta.v1,k = l)
  delta.v2.1<-lag(delta.v2,k = l)
  out<-cbind(df[2:nrow(df),],z.1, delta.v1, delta.v2, delta.v1.1, delta.v2.1)
  return(na.omit(out[2:nrow(df),]))
}

ecm_data<-ecm_prep(df, 5)
colnames(ecm_data)<-c('SP500','M2','Z','Z.1_lag','SP500_delta','REALM2_delta','SP500_delta_1lag','REALM2_delta_1lag')

#ecm model 1
ecm1<-lm(SP500_delta ~ SP500_delta_1lag + REALM2_delta_1lag + Z.1_lag, data = ecm_data)
summary(ecm1)
acf(ecm1$residuals)
shapiro.test(ecm1$residuals)

#ecm model 2
ecm2<-lm(REALM2_delta ~  SP500_delta_1lag + REALM2_delta_1lag + Z.1_lag, data = ecm_data)
summary(ecm2)
acf(ecm2$residuals)
shapiro.test(ecm2$residuals)


