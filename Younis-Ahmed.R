library(tseries)
library(rugarch)
library(FinTS)
rm(list=ls())
price <- get.hist.quote(instrument = "^BSESN",start = "2000-01-01", end = "2021-03-31", quote = "AdjClose")
price <- na.omit(price)
summary(price)
### Plot Time series
plot(price,main=" Time Series Plot of S&P BSE SENSEX (^BSESN)",ylab="Adjusted Volume",xlab="Year") 
grid() 

bselogret <-diff(log(price))
summary(bselogret$Adjusted)
plot(bselogret)



bselogret=diff(log(price))[-1]
bselogret <- na.omit(bselogret)
bselogret <- subset(bselogret,Adjusted >=0)
summary(bselogret)

ArchTest(bselogret)
garch(bselogret,grad="numerical",trace=FALSE)

bselogret_fit <- garch(bselogret)
