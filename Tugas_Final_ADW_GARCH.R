#Pemodelan data empiris
library(quantmod)
library(forecast)
library(TTR)
library(graphics)
library(TSA)
library(tseries)
library(fGarch)

# import data
aces <- read.csv("D:/Tugas GARCH/1-ACES.csv",header = T)
head(aces)

# #####################################################################
# Eksplor Data Saham
# #####################################################################
# ploting saham ACES
ts.plot(aces$ACES, col="blue", main="Time Series Plot", ylab="Close Price ACES")
adf.test(aces$ACES) #p-value = 0.4053 #tidak stasioner

# menghitung harga return
races <- data.frame(diff(log(aces$ACES), differences = 1))
head(races)

# ploting return ACES
adf.test(races) #p-value = 0.01 #stationary
ts.plot(races, col="blue", main="Time Series Plot (Return)", ylab="Return ACES")

# #####################################################################
# Menentukan Fungsi Mean
# #####################################################################
# identifikasi
par(mfrow=c(2,1))
acf(races) ## cuts off pada lag ke-1
pacf(races) ## cuts off pada lag ke-2
# ingat identifikasi model >> model AR(2) atau MA(1)

# pendugaan parameter model 
# AR(2)
arima(races, order = c(2,0,0), method = "ML") #aic = -629.66

# MA(1)
arima(races, order = c(0,0,1), method = "ML") #aic = -632.35

# sisaan model MA(1)
sisa <- residuals(arima(races, order = c(0,0,1), method = "ML"))
plot(sisa)
shapiro.test(sisa) 
tsdiag(arima(races, order = c(0,0,1), method = "ML"),
       gof=16, omit.initiatif=F) #L-Jung Box #H0: antar et no autokor 

# cek autokorelasi sisaan MA(1)
source("D:\\Tugas GARCH\\Note Sintaks\\lampiran.R")
acfStat(sisa,36) # memenuhi no auto sisaan

# #####################################################################
# Menguji keberadaan ARCH/GARCH pada Sisaan
# #####################################################################
# membuat plot autokorelasi dari komponen residual kuadrat
par(mfrow=c(2,1))
acf(sisa^2)
pacf(sisa^2)

source("D:\\Tugas GARCH\\Note Sintaks\\lampiran.R")
acfStat(sisa^2,36)
# korelasi sisaan^2 signfikan hingga lag yg cukup panjang
# H1 : terdapat efek ARCH/GARCH

# #####################################################################
# Menentukan Fungsi Varians
# #####################################################################
## GARCH(1,1)
fit <- garchFit(formula = ~garch(1,1), data = races, trace = F,
                algorithm = "lbfgsb+nm")
summary(fit) 

## GARCH(1,1) >> tanpa mu
fitm <- garchFit(formula = ~garch(1,1), data = races, trace = F,
                algorithm = "lbfgsb+nm", include.mean = F)
summary(fitm) 

## MA(1)- GARCH(1,1)
fit1 <- garchFit(formula = ~arma(0,1)+garch(1,1), data = races, trace = F,
                 algorithm = "lbfgsb+nm")
summary(fit1) 

## MA(1)- GARCH(1,1) >> tanpa mu
fit2 <- garchFit(formula = ~arma(0,1)+garch(1,1), data = races, trace = F,
                 algorithm = "lbfgsb+nm", include.mean = F)
summary(fit2) 

graphics.off()
plot(fit2)
# 13:   QQ-Plot of Standardized Residuals   

plot(density(residuals(fit2))) #residual bersifat heavy-tail 

# #####################################################################
# Peramalan dengan Model Terbaik
# #####################################################################
# Model terbaik adalah MA(1)-GARCH(1,1) >> fit2

graphics.off()
predict(fit2, n.ahead=5, plot=T) #default SK 95%
