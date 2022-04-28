########################################################################
# STAT4005 Final Exam
#
# Please fill in your information
#
# Name: Huang Zihao
# Student ID: 1155124405
#
########################################################################

# Q3a
data = read.table("Q3.txt", header=T)
d2 <- ts(data[3])

# Q3b
Box.test(d2,lag=12,type='Ljung') 
# conclusion:
# We reject the null hypothesis.
# Thus, the first 12 lags of autocorrelations 
# have non-zero value at the 5% level.

# Q3c
library(tseries)
adf.test(d2)
#lag order is 7

# Q3d
library(forecast)
m1=auto.arima(d2,ic=c("aic"))
m1
# MA(1): Xt = Zt +0.2191*Zt-1

# Q3e
p1 = predict(m1,12)
p1
right = p1$pred+p1$se*1.96
left = p1$pred-p1$se*1.96
ts.plot(cbind(p1$pred,right,left))

########################################################################

# Q4a
data4 = read.table("Q4.txt", header=T)
d4 <- ts(data4[2])
d4_l = log(d4+1)
d4_l


# Q4b
par(mfrow=c(2,1))
acf(d4_l)
acf(d4_l^2)
# Log return and squared log return are stationary time series
# Reason: The figure shows that the autocorrelation are close to 0

# Q4c
len = length(d4_l)
fit = lm(d4_l[3:len]~d4_l[1:(len-2)])
Rs = summary(fit)$r.squared
pvalue= 1-pchisq(len*Rs,1)
pvalue<0.05
# False 


# Q4d
d5 = d4_l^2
pacf(d5)
fit2 = garch(d5,order=c(0,3))
summary(fit2)






