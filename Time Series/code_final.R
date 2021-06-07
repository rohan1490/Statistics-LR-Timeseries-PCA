library(fpp2)
library(tseries)
library(astsa)
data <- read.csv("D:/Study/Statistics/TABA/Time Series/Electricity.csv",header = TRUE, sep = ",")
head(data)

data <- data[,-1]
str(data)

summary(data)

#train <- data[1:140]
#test <- data[141:144]


#Convert to timeseries
timeseries <- ts(data,frequency=12,start=c(2008,1),end =c(2019,12) )
train <- window(timeseries,start=c(2008,1),end=c(2019,8))
test <- window(timeseries,start=c(2019,9),end=c(2019,12))


par(mfrow = c(1,1))
tsplot(timeseries, ylab="Denmark - Supply of Electricity (Gigawatt-hour)", lwd=2, col=rgb(.9,  0, .7, .5))			  
tsplot(train, ylab="Denmark - Supply of Electricity (Gigawatt-hour)", lwd=2, col=rgb(.9,  0, .7, .5))			  
tsplot(test, ylab="Denmark - Supply of Electricity (Gigawatt-hour)", lwd=2, col=rgb(.9,  0, .7, .5))			  
plot(timeseries, type='o', col=4)

#Decompose timeseries
plot(decompose(train, type = "additive"))

#Decompose using stl function
decomposel <- stl(timeseries, s.window = "periodic")
plot(decomposel)


#Seasonal Plot
library(ggplot2)
ggseasonplot(timeseries, year.labels = TRUE, year.labels.left = TRUE)+ylab("Denmark - Supply of Electricity (Gigawatt-hour)")+ggtitle("Seasonal Plot")

#Mean of plots each month
ggsubseriesplot(timeseries)+ylab("Denmark - Supply of Electricity (Gigawatt-hour)")+ggtitle("Seasonal subseries Plot")


snaivemodel <- snaive(train, h=4)
str(snaivemodel)

autoplot(timeseries, series = 'Original Timeseries') + 
  autolayer(snaivemodel$fitted, series = 'Fitted Seasonal Naive model')+
  autolayer(snaivemodel, series = 'Seasonal Naive Forecast', PI= FALSE)+
  xlab("Year")+
  ylab("Denmark - Supply of Electricity (Gigawatt-hour)")+
  theme_minimal()+
  theme(legend.position = c(.85, .9))+
  scale_color_manual(values = c("chartreuse3","coral3","turquoise4"))



checkresiduals(snaivemodel)
accuracy(snaivemodel,test)
a1 <- accuracy(snaivemodel,test)
str(a1)
a1[1,2]
summary(snaivemodel)


#Holt-winter model

#fitholt1 <- hw(train, seasonal = "multiplicative", h=4)
#fitholt1
#accuracy(fitholt1,test)
#plot(fitholt1)

fitholt <- hw(train, seasonal = "additive", h=4)

summary(fitholt)
a2 <- accuracy(fitholt,test)
checkresiduals(fitholt)
plot(fitholt)
mean(fitholt$residuals)

autoplot(timeseries, series = 'Original Timeseries') + 
  autolayer(fitholt$fitted, series = 'Fitted Holt-Winter model')+
  autolayer(fitholt, series = 'Holt-Winter Forecast', PI= FALSE)+
  xlab("Year")+
  ylab("Denmark - Supply of Electricity (Gigawatt-hour)")+
  theme_minimal()+
  theme(legend.position = c(.85, .9))+
  scale_color_manual(values = c("chartreuse3","turquoise4","coral3"))



fitets <- ets(train, model = "ZZZ")
fitets
str(fitets)
checkresiduals(fitets)
mean(fitets$residuals)

accuracy(fitets)
summary(fitets)
fitets$method
fitetsforecast <- forecast(fitets, h=4)

a3 <-accuracy(fitestforecast,test)


autoplot(timeseries, series = 'Original Timeseries') + 
  autolayer(fitets$fitted, series = 'Fitted ets() model')+
  autolayer(fitestforecast, series = 'ets() Forecast', PI= FALSE)+
  xlab("Year")+
  ylab("Denmark - Supply of Electricity (Gigawatt-hour)")+
  theme_minimal()+
  theme(legend.position = c(.85, .9))+
  scale_color_manual(values = c("turquoise4","chartreuse3","coral3"))

#ARIMA modelling

ndiffs(train)
nsdiffs(train)
adf.test(train)

diff <- diff(diff(train),12)
mean(diff)
autoplot(diff)
ndiffs(diff)
nsdiffs(diff)

par(mfrow = c(1,2))
Acf(diff)
Pacf(diff)

adf.test(diff)
pp.test(diff)




automodel <- auto.arima(train, stepwise = FALSE, approximation = FALSE)
str(automodel)
checkresiduals(automodel)
qqnorm(automodel$residuals)
qqline(automodel$residuals)
Box.test(automodel$residuals, type="Ljung-Box")
summary(automodel)
mean(automodel$residuals)

fcast1 <- forecast(automodel, h=4)
a4 <- accuracy(fcast1,test)

autoplot(timeseries, series = 'Original Timeseries') + 
  autolayer(fitets$fitted, series = 'Fitted ets() model')+
  autolayer(fitestforecast, series = 'ets() Forecast', PI= FALSE)+
  xlab("Year")+
  ylab("Denmark - Supply of Electricity (Gigawatt-hour)")+
  theme_minimal()+
  theme(legend.position = c(.85, .9))+
  scale_color_manual(values = c("turquoise4","chartreuse3","coral3"))


ar1 <-Arima(train, order = c(0,1,2), seasonal = list(order=c(0,1,1), period = 12))
ar1

fcast2 <- forecast(ar1, h=4)
fcast2
a5 <- accuracy(fcast2,test)
a5[1,2]
a5[2,2]

checkresiduals(ar1)
qqnorm(ar1$residuals)
qqline(ar1$residuals)
Box.test(ar1$residuals, type="Ljung-Box")
summary(ar1)

autoplot(timeseries, series = 'Original Timeseries') + 
  autolayer(ar1$fitted, series = 'Fitted ARIMA(0,1,2)(0,1,1)[12] model')+
  autolayer(fcast2, series = 'ARIMA(0,1,2)(0,1,1)[12] Forecast', PI= FALSE)+
  xlab("Year")+
  ylab("Denmark - Supply of Electricity (Gigawatt-hour)")+
  theme_minimal()+
  theme(legend.position = c(.85, .9))+
  scale_color_manual(values = c("turquoise4","chartreuse3","coral3"))


str(automodel)
#Function for RMSE
RMSE <- function(error) { sqrt(mean(error^2)) }
round(RMSE(automodel$aicc))



ic <- data.frame(Model = c("ARIMA(012)(011)[12]", "Auto-ARIMA", "Ets Model","Holt-Winter","Seasonal Naive"),
                 AICc = c(round(ar1$aicc,2), round(automodel$aicc,2), round(fitets$aicc,2),round(fitholt$model$aicc,2),"-"),
                 RMSE_train = c(round(a5[1,2],2), round(a4[1,2],2), round(a3[1,2],2),round(a2[1,2],2),round(a1[1,2],2)),
                 RMSE_test = c(round(a5[2,2],2), round(a4[2,2],2), round(a3[2,2],2),round(a2[2,2],2),round(a1[2,2],2)),
                          stringsAsFactors = FALSE)
ic

print(table(ic, caption = "Information criteria results",
             label = "tab:ic_models"))

