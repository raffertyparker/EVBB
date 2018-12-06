# This plots EV charging, differentiating between fast and slow charges

evbb <- read.csv("~/EVBB/data/EVBB_processed.csv", stringsAsFactors = F) 
evbb$time <- chron(times=evbb$time) # converting from character to times
evbb$time_as_numeric <- as.numeric(evbb$time) # converting from times to numeric
evbb$day_of_week <- factor(evbb$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", 
                                                                            "Thursday", "Friday", "Saturday", 
                                                                            "Sunday")) # setting day of the week as ordered factor
evbb <- with(evbb, evbb[order(day_of_week, time),]) # orders the dataframe according to days of the week

weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
#Use `%in%` and `weekdays` to create a logical vector
#convert to `factor` and specify the `levels/labels`
evbb$weekday <- factor((evbb$day_of_week %in% weekdays1), 
                   levels = c(FALSE, TRUE), labels = c('Weekend', 'Weekday')) 

evbb$charging_rate <- factor((evbb$charge_power_kw >= 7),
                    levels = c(FALSE, TRUE), labels = c('slow', 'fast'))


ggplot(data = evbb, aes(x = time, y = charge_power_kw, colour = charging_rate)) + facet_wrap(~day_of_week) + 
  geom_area(alpha=0.1) + 
  xlab("Time of day (hours)") + 
  ylab("Charging power (kW)")

ggsave("plots/fast_slow_charging_by_day.png")

ggplot(data = evbb, aes(x = time, y = charge_power_kw, colour = charging_rate)) + facet_wrap(~weekday) + 
  geom_area(alpha=0.1) + 
  xlab("Time of day (hours)") + 
  ylab("charging power (kW)")

ggsave("plots/fast_slow_charging_weekend_weekday.png")