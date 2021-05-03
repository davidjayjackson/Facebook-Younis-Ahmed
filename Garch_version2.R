library(tseries)
library(rugarch)
library(FinTS)
library(xts) # Added XTS Time Series Library
rm(list=ls())
price <- get.hist.quote(instrument = "^BSESN",start = "2000-01-01", end = "2021-03-31", quote = "AdjClose")
price <- na.omit(price$Adjusted)
## Use TS function to convert to time series
price_ts <- as.xts(price)
summary(price_ts)
### Plot Time series by using plot.xts finction for xts
plot.xts(price_ts,main=" Time Series Plot of S&P BSE SENSEX (^BSESN)",ylab="Adjusted Volume",xlab="Year") 


## Run This by itself and create bselogret Time Series
bselogret <- diff(log(price_ts))
bselogret <- na.omit(bselogret)
summary(bselogret)

plot.xts(bselogret, main = "Plot of bselogret Times Series")

ArchTest(bselogret)
garach_fit <- garch(bselogret,grad="numerical",trace=FALSE)

## Plot of Residuals

plot(garach_fit1$fitted.values)

## This line overwrites the  bselogret from previous command
bselogret1 <- diff(log(price_ts))[-1]
bselogret1 <- na.omit(bselogret1)
summary(bselogret1)

plot.xts(bselogret1, main = "Plot of bselogret1 Times Series")

ArchTest(bselogret)
garach_fit1 <- garch(bselogret1,grad="numerical",trace=FALSE)

## Plot of Residuals

plot(garach_fit1$fitted.values)


