# creates box-whisker plots
# data is sorted into 15 minute intervals
# this is then further sorted into larger intervals at the end (using round())
# currently chron_weekly must be run before this script

library(chron)
library(ggplot2)

quarterHour <- seq(from = 0, by = 15*60/(24*60*60), to = 1) # sequence that keeps track of every 15 minutes
m = length(quarterHour)

Hour <- seq(0, 0, nrow(EVBB_processed))
EVBB <- cbind(EVBB_processed, Hour)
EVBB1 <- head(EVBB, n = 1)
#tmp <-  EVBB1


for (j in 1:m){
  tmp <- subset(EVBB, EVBB$timeAsNumeric >= quarterHour[j] & EVBB$timeAsNumeric <= quarterHour[j+1])
  tmp$Hour <- quarterHour[j]
  EVBB1 <- rbind(EVBB1, tmp)
}

EVBB1 <- EVBB1[-1,]  # remove the duplicate first row
EVBB1$Hour <- EVBB1$Hour * 24 # change the time back into 24 hour

#EVBB$Hour <- floor(EVBB$Hour)  # Changes 1/4 hour data into 1 hourly
EVBB1$Hour <- 2 * round(EVBB1$Hour/2)  # Changes 1/4 hour data into 2 hourly to tidy up the boxplot somewhat

ggplot(data = EVBB1, mapping = aes(group = Hour, y = charge_power_kw)) + geom_boxplot() + ylim(0, 3) + 
  labs(x = "Time", y = "Average charge power (kW)")


# boxplot(charge_power_kw~Hour, data = EVBB, main = "EV charging power", ylab = "kW", xlab = "Hours past midnight")

#hist(x = EVBB$charge_power_kw, xlab = "Charge power", main = "Histogram of different charge power frequencies")



#if (nrow(tmp) == 0){
#  tmp[1,7] <- 0
#}