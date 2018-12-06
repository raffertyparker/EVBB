# This plots daily occurrences of fast charging
# currently not good for seperating out by day
# THIS NEEDS WORK eg divide total charge by number of cars

EVBB_tmp <- read.csv("~/EVBB/data/EVBB_processed1.csv")
EVBB_fastcharge <- EVBB_tmp[EVBB_tmp$charge_power_kw >= 7, ]
EVBB_fastcharge$location <- factor(EVBB_fastcharge$location)
EVBB_fastcharge$id <- factor(EVBB_fastcharge$id)

#pie(table(EVBB_fastcharge$location), main = "Location of fast charging")

# The pie chart demonstrates that there are often times during
# which fastcharge is occurring while the car is supposedly at home.

quarterHour <- seq(from = 0, by = 15*60/(24*60*60), to = 1) # sequence that keeps track of every 15 minutes

fastChargeTimes <- data.frame(day="", time=0, charge_power=0, ave_power =0)
# quarterHourData$day <- as.character(day)

days <- levels(EVBB_fastcharge$day_of_week)
n <- length(levels(EVBB_fastcharge$day_of_week))
m <- length(quarterHour)


for (i in 1:n){
  tmp <- EVBB_fastcharge[EVBB_fastcharge$day_of_week == days[i],] # subsets the dataframe according to the day
  for (j in 1:(m)){
    tmp1 <- tmp[tmp$time_as_numeric >= quarterHour[j] & tmp$time_as_numeric <= quarterHour[j+1],] # subset the dataframe according to 15 min time intervals
    if (nrow(tmp1) == 0){
      tmp1[1, 7] <- 0
    }
    tmp2 <- sum(tmp1$charge_power_kw) # total charge in each 15 minute segment
    tmp3 <- tmp2/nrow(tmp1) # average charge in each 15 minute segment
    tmp4 <- data.frame(day=days[i], time=quarterHour[j], charge_power=tmp2, ave_power=tmp3)
    fastChargeTimes <- rbind(fastChargeTimes, tmp4)
  }
}

fastChargeTimes = fastChargeTimes[-1,]  # remove the empty forst row created by the previous loop
fastChargeTimes$time <- fastChargeTimes$time * 24 # change the time back into 24 hour


fastChargeTimes$day <- factor(fastChargeTimes$day, 
                               levels = c("Monday", "Tuesday", "Wednesday", 
                                          "Thursday", "Friday", "Saturday", 
                                          "Sunday")) # setting day of the week as ordered factor
fastCharge <- with(fastChargeTimes, fastChargeTimes[order(day, time),])


#for (i in 1:n){
#  plot(y = fastChargeTimes$ave_power[fastChargeTimes$day == days[i]], x = fastChargeTimes$time[fastChargeTimes$day == days[i]], main = days[i], type = 'l', xlab = "Time", ylab = "Average fast-charge power")
#} 

ggplot(data = fastCharge, aes(x = time, y = ave_power)) + facet_wrap(~day) + 
  geom_area(alpha=0.1) + 
  xlab("Time of day (hours)") + 
  ylab("Charging power (kW)")

ggsave("plots/fast_charging_times.png")



# Plot - separarte panel per car, fill will charging power





