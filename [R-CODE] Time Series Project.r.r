### Proyek Akhir Runtun Waktu ###

library(TSA)
library(forecast)
library(tseries)
library(normtest)

### Import Data ###
library(readxl)
dataweather <- read_excel("[DATA] Proyek UAS Runtun Waktu.xlsx", 
                          col_types = c("numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "text"))                                                                                         
View(dataweather)
head(dataweather)
tail(dataweather)

### Buat Time Series Objek ###
#Akan digunakan data temperature
datatemp <- ts(dataweather$temperature, frequency = 1, start = 1)
datatemp

### Statistika Deskriptif ###
hist(datatemp)

summary(datatemp)

library(psych)
describe(datatemp)

var(datatemp)

### Uji Stasioneritas ###
adf.test(datatemp)
#karena data sudah stasioner, maka tidak perlu differencing (d=0)

plot(datatemp, 
     main = "Temperature Cuaca Rata-rata dari 1 Oktober 2019 pukul 00.00 sampai 21 Oktober 2019 pk. 18.00 ", 
     xlab = "lag",
     ylab = "Temperature",
     col = "red")

#Differencing (jika data tidak stasioner)
diftemp<-diff(datatemp,differences = 1)
plot(diftemp, type = "l", main="Temperature Cuaca Rata-rata Setelah Differencing", 
     col="red")
adf.test(diftemp)

#Untuk melihat plot data temp, acf, dan pacf
tsdisplay(datatemp)

#Untuk melihat ACF
ACF <- acf(datatemp)
plot(ACF, main = "ACF Data Rata-rata Temperature Cuaca")

#Untuk melihat PACF
PACF <- pacf(datatemp)
plot(PACF, main = "Partial ACF Data Rata-rata Temperature Cuaca")

#untuk melihat EACF
EACF <- eacf(datatemp)

### Spesifikasi Model ###
#Karena tidak dilakukan differencing maka d=0, sehingga model arima(p,0,q)
model1<-Arima(datatemp, order=c(2,0,1))
model2<-Arima(datatemp, order=c(2,0,2))
model3<-Arima(datatemp, order=c(2,0,3))
model4<-Arima(datatemp, order=c(3,0,2))
model5<-Arima(datatemp, order=c(3,0,3))
cbind(model1, model2, model3, model4, model5)

#Untuk mengeluarkan model terbaik
auto.arima(datatemp, trace=TRUE)
#model 2 yang terbaik

### Estimasi Parameter (model 2) ###
fit <- Arima(datatemp, order=c(2,0,2), include.constant = TRUE)
fit

### Diagnosis Model ###
##Analisa Residual
#Uji Independensi Residual (Ljung Box Test)
checkresiduals(fit)

#Plot 1: digunakan untuk melihat Uji Stasioneritas
#Plot 2 (ACF): melihat apakah residual dalam setiap lag memiliki autokorelasi atau tidak
#Plot 3: Histogram untuk residual, untuk melihat distribusi Normal

#Uji Normalitas Residual (Jarque Bera Test)
jb.norm.test(datatemp,nrepl = 2000)

#Uji Stasioneritas Residual (Augmented Dickey Fuller Test)
adf.test(residuals(fit))

##Overfitting
overfit1<-Arima(datatemp,order = c(2,0,3),include.constant =TRUE)
overfit2<-Arima(datatemp,order = c(3,0,2),include.constant =TRUE)
cbind(fit,overfit1,overfit2)
overfit1
overfit2

#Menghitung t-tabel
alpha=0.05
qt(c(alpha/2, 1-(alpha/2)), df=498)

### Forecasting ###
#Cross Validation
test <- window(datatemp, start=c(495))
test
train <- window(datatemp, end=c(494))
train

fit2 <- Arima(train, order = c(2,0,2), include.constant = TRUE)
fit2
forecast2 <- forecast(fit2, 5)
forecast2
cbind(test, forecast2)

plot(forecast2, 
     fcol="blue", 
     main = "Peramalan ARIMA(2,0,2) melalui DataTraining - Testing", 
     xlab="Periode", 
     ylab="Temperature")
lines(seq(240,244),
      test[1:5],
      col="red",
      lwd=2)
legend(x=c(0,100),
       y=c(4000,5000), 
       col=c("blue", "red"),
       legend = c("Peramalan Nilai Testing","Nilai Aktual"), 
       lwd=2, 
       cex=1)

#Forecasting untuk 5 periode ke depan
forecast <- forecast(fit, h=5)
forecast
plot(forecast, 
     fcol="blue", 
     main = "Peramalan ARIMA(2,0,2) 5 periode ke depan", 
     xlab="Periode", 
     ylab="Temperature")
legend(x=c(0,100),
       y=c(4000,4600), 
       col="blue",
       legend = c("Hasil Peramalan"), 
       lwd=2, 
       cex=1)