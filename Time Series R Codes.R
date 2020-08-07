#############################################
# Dataset is about sales from 1987 to 1993
# We name it FANCY.df
#############################################
FANCY.df <- read.delim("download.txt", header = FALSE)

str(FANCY.df)
class(FANCY.df)


# converting the dataframe to time-series.
FANCY.ts= ts(FANCY.df, frequency = 12, start = c(1987,1), end = c(1993,12))
str(FANCY.ts)
FANCY.ts
### Initial Observations
#1. Every year there is a gradual increase in the sales from the month of January to December. The sales peak at December for every year.
#2.	Drop in sales is obeserved in April and August from their previous months in majority of the years.
#3.	Overall there is an increase in the sales from 1987 to 1993.
#4. Sudden drop in sales is observed in January (every new year).



###############################################################
# Prelimanary Analysis
##############################################################
### Q1a
#plotting
plot(FANCY.ts)
# We can see trend by putting the best fit line
abline(reg=lm(FANCY.ts~time(FANCY.ts)))

## or we can use a better Time plot
library(fpp2)

autoplot(FANCY.ts) + 
  ggtitle("Time plot for the Monthly sales of a souvenir shop ") +
  ylab("Number of sales")
## Observations
# We can observe that there is an increasing trend in sales over the years. 
# Hence the dominant component in our time-series is Trend 

######## Check Presence of seasonality by decomposing data
# Decomposing the time-series into components
# We go for the multiplicative decompostion because it is observed that the variation of the data increases with increasing year.
decompose(FANCY.ts, type = "multiplicative")

plot(decompose(FANCY.ts, type = "multiplicative"))
# From the decomposed seasonal plot it can be seen that the variation in the time-series data repeats itself every year
# The seasonality can be better understood when we over lap the seasonality curves for each year.
# before we we plot the seasonality plot, we make the trend stationary using diff
Diff_Fancy.ts=diff(FANCY.ts)
plot(Diff_Fancy.ts)
# and now plot the data after making the trend stationary to check seasonality.
ggseasonplot(Diff_Fancy.ts)
# seasonality is clear in the ablove plot where there is a big drop in January every year  and big rise in the months of november and january.
#
#
#################################################################

# Hence our series Fancy.ts has trend and seasonality. 

##################################################################

### Q1b
# Checking stationarity
# 1st check visually
# Already done above.
# from the plots above it can be  observed that the Fancy dataset time-series exhibits non-stationarity because 
# of the non-constant mean (increasing best fit line) and non-constant variance (increasing from left to right). 
#
# 2nd stationarity check using KPSS test
library(tseries)
kpss.test(FANCY.ts)
# the p-value from kpss test is 0.01 which is less than 0.05.
# Hence the series is not stationary.
kpss.test(diff(log(FANCY.ts)))

#### Q2
# splitting, removing last 2 years(24months) as testing data
library(TSstudio)

Split_Fancy.ts=ts_split(FANCY.ts,sample.out = 24)

training_fancy=Split_Fancy.ts$train
testing_fancy= Split_Fancy.ts$test

str(training_fancy)
str(testing_fancy)

# Find AR & MA values
acf(diff(log(training_fancy))) # q = 2

pacf(diff(log(training_fancy))) # p = 2

arimaMod <- arima(log(training_fancy),c(2,1,2), seasonal = list(order = c(2,1,2), period =12))

summary(arimaMod)
arima.Pred <- predict(arimaMod,n.ahead = 2*12)
arima.Pred
arima.Predf <- 2.718^arima.Pred$pred
arima.Predf
arima.Predf %>% autoplot()

Vec_fancy<- cbind(testing_fancy,arima.Predf)
Vec_fancy
ts.plot(Vec_fancy, col=c("blue", "red"), main="Quarterly Income: Actual vs Forecast")

accuracy(arima.Predf, testing_fancy)

checkresiduals(arimaMod)

##Multiplicative Holts Winter Model
plot(training_fancy)
HW_model_multi <- HoltWinters(training_fancy, seasonal = "multiplicative")
plot(HW_model_multi)
holts.fr.mul <- forecast(HW_model_multi,2*12)
holts.fr.mul
plot_forecast(holts.fr.mul)

Box.test(holts.fr.mul$residuals, type = "Ljung-Box")

accuracy(holts.fr.mul,testing_fancy)


## To predict the values for the next 5 year with the created ARIMA model.

arima.Pred5 <- predict(arimaMod,n.ahead = 7*12)
arima.Pred5f <- 2.718^arima.Pred5$pred
arima.Pred5f

ts.plot(arima.Pred5f, training_fancy, col=c("blue", "red"), main="Quarterly Income: Actual vs Forecast")

## To predict the values for the next 5 years with the created HW model.

holts.fr.mul5 <- forecast(HW_model_multi,7*12)
holts.fr.mul5
plot_forecast(holts.fr.mul5)


