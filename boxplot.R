# creates box-whisker plots
# data is sorted into 15 minute intervals
# this is then further sorted into larger intervals at the end (using round())

library(chron)
library(ggplot2)


# chronologize data (weekly)

EVBB_processed <- read.csv("~/EVBB/data/EVBB_processed.csv", stringsAsFactors = F) 

EVBB_processed$time <- chron(times=EVBB_processed$time) # converting from character to times
EVBB_processed$timeAsNumeric <- as.numeric(EVBB_processed$time) # converting from times to numeric

EVBB_processed$day_of_week <- factor(EVBB_processed$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", 
                                                                            "Thursday", "Friday", "Saturday", 
                                                                            "Sunday")) # setting day of the week as ordered factor
EVBB_processed <- with(EVBB_processed, EVBB_processed[order(day_of_week, time),]) # orders the dataframe according to days of the week


quarterHour <- seq(from = 0, by = 15*60/(24*60*60), to = 1) # sequence that keeps track of every 15 minutes
m = length(quarterHour)

zeroHour <- seq(0, 0, nrow(EVBB_processed))
EVBB <- cbind(EVBB_processed, zeroHour)
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
  labs(x = "Time", y = "Charge power (kW)")


# boxplot(charge_power_kw~Hour, data = EVBB, main = "EV charging power", ylab = "kW", xlab = "Hours past midnight")

#hist(x = EVBB$charge_power_kw, xlab = "Charge power", main = "Histogram of different charge power frequencies")



#if (nrow(tmp) == 0){
#  tmp[1,7] <- 0
#}