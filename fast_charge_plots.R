# This plots daily occurrences of fast charging

quarterHour <- seq(from = 0, by = 15*60/(24*60*60), to = 1) # sequence that keeps track of every 15 minutes

fastChargeTimes <- data.frame(day="", time=0, charge_power=0, ave_power =0)
#quarterHourData$day <- as.character(day)

days <- levels(EVBB_fastcharge$day_of_week)
n <- length(levels(EVBB_fastcharge$day_of_week))
m <- length(quarterHour)


for (i in 1:n){
  tmp <- EVBB_fastcharge[EVBB_fastcharge$day_of_week == days[i],] # subsets the dataframe according to the day
  for (j in 1:(m)){
    tmp1 <- tmp[tmp$timeAsNumeric >= quarterHour[j] & tmp$timeAsNumeric <= quarterHour[j+1],] # subset the dataframe according to 15 min time intervals
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


for (i in 1:n){
  plot(y = fastChargeTimes$ave_power[fastChargeTimes$day == days[i]], x = fastChargeTimes$time[fastChargeTimes$day == days[i]], main = days[i], type = 'l', xlab = "Time", ylab = "Average fast-charge power")
} 


