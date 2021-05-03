library(tseries)
library(rugarch)
library(FinTS)
rm(list=ls())
price <- get.hist.quote(instrument = "^BSESN",start = "2000-01-01", end = "2021-03-31", quote = "AdjClose")
price <- na.omit(price$Adjusted)
price_ts <- ts(price)
summary(price_ts)
### Plot Time series
plot(price_ts,main=" Time Series Plot of S&P BSE SENSEX (^BSESN)",ylab="Adjusted Volume",xlab="Year") 
grid() 

bselogret <-diff(log(price_ts))
summary(bselogret)
plot(bselogret)



bselogret <- diff(log(price_ts))[-1]
bselogret <- na.omit(bselogret)
# bselogret <- subset(bselogret,Adjusted >=0)
summary(bselogret)

ArchTest(bselogret)
garch(bselogret,grad="numerical",trace=FALSE)

bselogret_fit <- garch(bselogret)

## Using CSV to Data Frame

df <- read.csv("./price.csv")
summary(df)

price_fit <- garch(bselogret,grad="numerical",trace=FALSE)
