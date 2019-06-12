#Bitcoin Forecast

library(readxl)
setwd("~/Desktop/R Friday /class 1/")
BPrice <- read_excel("BPrice.xlsx", sheet = "MergerData")

ts.plot(BPrice$Bitcoin) 
ts.plot(BPrice$SP500)
ts.plot(BPrice$Dexuseu)
ts.plot(BPrice$Gold)
ts.plot(BPrice$WTI)

model<- lm(Bitcoin~SP500+Dexuseu+Gold+WTI,data=BPrice)
summary(model) 

kpss.test(BPrice$Bitcoin,null="Level")
kpss.test(BPrice$Bitcoin,null="Trend")
kpss.test(diff(BPrice$Bitcoin),null="Level")
kpss.test(diff(BPrice$Bitcoin),null="Trend")$p.value# employment series is TREND stationary after first-differencing, 0.1, so only one diff 

kpss.test(BPrice$WTI,null="Level")
kpss.test(BPrice$WTI,null="Trend")
kpss.test(diff(BPrice$WTI),null="Level")
kpss.test(diff(BPrice$WTI),null="Trend")#WTI is trend stationary

kpss.test(BPrice$Dexuseu,null="Level")
kpss.test(BPrice$Dexuseu,null="Trend")
kpss.test(diff(BPrice$Dexuseu),null="Level")#Dexuseu is level stationary

kpss.test(BPrice$Gold,null="Level")
kpss.test(BPrice$Gold,null="Trend")
kpss.test(diff(BPrice$Gold),null="Level")#GOLDAMGBD228NLBM is level stationary

kpss.test(BPrice$SP500,null="Level")
kpss.test(BPrice$SP500,null="Trend")
kpss.test(diff(BPrice$SP500),null="Level")#SP500 is level stationary

model1 <- lm(diff(Bitcoin)~diff(WTI)+diff(Dexuseu)+diff(Gold)+diff(SP500),data=BPrice)
summary(model1)

log(BPrice$Bitcoin)[2:nrow(Bitcoin)]-log(BPrice$Bitcoin)[1:nrow(Bitcoin)-1]
diff(BPrice$Bitcoin)

ts.plot(diff(BPrice$Bitcoin))
ts.plot(diff(BPrice$Bitcoin))

#remove all data before 2017
library(readxl)
setwd("~/Users/dixie/Desktop/School/UTD/Fall2018/R Parker /Practice")
BPrice2017 <- read_excel("BPrice2017.xlsx", sheet = "MergerData")
View(BPrice2017)
newmodel<- lm(Bitcoin~SP500+Dexuseu+Gold+WTI,data=BPrice2017)
summary(newmodel) 

ts.plot(BPrice2017$Bitcoin) 
ts.plot(BPrice2017$SP500)
ts.plot(BPrice2017$Dexuseu)
ts.plot(BPrice2017$Gold)
ts.plot(BPrice2017$WTI)

kpss.test(BPrice2017$Bitcoin)
acf(diff(BPrice2017$Bitcoin))
ts.plot(diff(BPrice2017$Bitcoin))
pacf(diff(BPrice2017$Bitcoin)) 
arima(diff(BPrice2017$Bitcoin))
model_bitcoin <- arima(log(BPrice2017$Bitcoin),c(9,1,11))
model_bitcoin
AIC(model_bitcoin)

maxa <- 13
outp <- matrix(0L,(maxa+1)^2,3)

ndx <- 1

for(i in 0:maxa){
  print(paste(as.character(round(i*(maxa+1)/(maxa+1)^2*100,digits=2)),'%...',sep=''))
  for(j in 0:maxa) {
    tryCatch({aic<-Bitcoin%>%arima(c(i,1,j))%>%AIC},error=function(err){aic<-9999.99})
    outp[ndx,1:3] <- c(i,j,aic)
    ndx <- ndx + 1
  }
} 

rm(model_bitcoin)
model_bitcoin <- stats::arima(BPrice2017$Bitcoin,c(9,1,11))
steps <- 30
future <- forecast(model_bitcoin,h=steps)
plot(future)
predict_m1 <- predict(model_bitcoin,n.ahead=30)
predict_m1

library(TSA)
periodogram(BPrice2017$Bitcoin)
periodogram(diff(BPrice2017$Bitcoin))
periodogram(diff(log(BPrice2017$Bitcoin)))
diffBitCoin <- diff(BPrice2017$Bitcoin)
BPrice2017 <- BPrice2017[1:2]
Week_days <- weekdays(BPrice2017$Date)
BPrice2017$Week_Days <- weekdays(BPrice2017$Date)
View(BPrice2017)

class(BPrice2017$Date)

class(BPrice2017$Week_Days)
Weekdays <- BPrice2017$Week_Days
BPrice2017<- dummy_cols(BPrice2017)
class(BPrice2017)
BPrice2017<-BPrice2017[-4]
Mon <- BPrice2017$Week_Days_Monday
Tue <- BPrice2017$Week_Days_Tuesday
Wed <- BPrice2017$Week_Days_Wednesday
Thurs <- BPrice2017$Week_Days_Thursday
Fri <- BPrice2017$Week_Days_Friday
new_Bmodel <- lm(diff(BPrice2017$Bitcoin)~Mon[2:417]+Tue[2:417]+Wed[2:417]+Thurs[2:417]+Fri[2:417],data=BPrice2017)
summary(new_Bmodel) 
resid_Bitcoinmodel <- residuals(new_Bmodel)
periodogram(resid_Bitcoinmodel)


library(vars)
diff_jp <- function(x){
  n <- nrow(x)
  return(x[2:n,]-x[1:n-1,])
}

x <- library(vars)

?VAR
diff_jp <- function(x){
  n <- nrow(x)
  return(x[2:n,]-x[1:n-1,])
}

x <- BPrice2017 %>% dplyr::select(Bitcoin,SP500,Gold,Dexuseu,WTI) %>% diff_jp
VAR(x,p=1,type="both") %>% AIC
VAR(x,p=2,type="both") %>% AIC
VAR(x,p=3,type="both") %>% AIC
VAR(x,p=4,type="both") %>% AIC
VAR(x,p=4,type="both") %>% AIC
VAR(x,p=10,type="both") %>% AIC
model7 <- VAR(x,p=2,type="both")
summary(model7)
grangertest(Bitcoinmodel)

steps <- 30
predict_m2 <- predict(model7,n.ahead=30)
predict_m2
x1 <- BPrice2017 %>% dplyr::select(Bitcoin)%>% diff_jp

steps <- 30
future <- forecast(x1,h=steps)
plot(future)
predict_m1 <- predict(model_bitcoin,n.ahead=30)
predict_m1

#Q1:Use a naïve regression to find spurious correlations to the bitcoin price in the data set
#A1:Bitcoin=-41790-57.1WTI+22850Dexuseu-2.831Gold +10.92SP500; All variables are significant and this is spurious as the data is not stationarized.
#Q2:Use the KPSS test to find how many differences each series takes to become stationary.
#A2:We took the difference once to convert all series to stationary data
#Q3:After taking differences, regress the bitcoin price on the other series. What relationships do you find now?
#A3:Now we see that none of the variables are significant so there is no relationship.
#A4:We now have 418 rows in the filtered data and the data is plotted
#Q5:Fit various arima models to the bitcoin price. Which model fits best using the AIC?
#A5:Arima model fits best at p=6,I=1 and q=6 as the AIC is lowest 
#A6:Forecast the next 30 days of the bitcoin price and plot the forecast.
#A6:plotted
#Q7:Plot the periodogram of the data. Do you see any seasonality in the data?
#Q7:there is seasonality
#Q8:Fit a model where you regress the stationarity-transformed price on dummy variables for the different days of the week. Obtain the residuals from the model. Plot the periodogram of these residuals. Has the periodogram changed greatly? Do you think this transformation helps us to capture any seasonality in the data?
#A8:There is no difference in periodogram. Yes, So some Seasonality can be captured
#Q9:Using the AIC, select a VAR model which best captures the relationships between our 5 variables. What Granger causality relationships do you see between our prices?
#A9:Var model with p=2 best fits the model. And we dont see any granger causality
#Q10:Forecast the next 30 days of the prices using the VAR model. Compare your forecasts to one from the ARIMA model.
#A10:The plot looks very similar