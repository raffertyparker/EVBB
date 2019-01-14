# Loads data from Otago HCS
library(readr)

# Will only work if you have mounted the HCS as a volume (on Mac) or drive. This may require the VPN if you are off campus.

# Adjust pathname for your platform

# this works on a Mac:
dPath <- "/Volumes/hum-csafe/Research Projects/GREEN Grid/Self_contained_Projects/2018_evChargingRafferty/data/"

# use the same method to load from Dropbox - just change dPath, readr will still work provided it can access the file.

# The data file:
dFile <- paste0(dPath, "2019_12_10/EVBB_processed1.csv")

df <- readr::read_csv(dFile)

# quick look
head(df)

# Note that readr loaded it into a tbl - which has fancy printing

table(df$day_of_week) # number of obs per day
table(df$month) # number of obs per month

# extract hour
library(lubridate)
df$hour <- lubridate::hour(df$time)
table(df$hour) # number of observations per hour

# set correct order for days of the week
levels(as.factor(df$day_of_week))

df$day_of_week <- ordered(df$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                         "Friday", "Saturday", "Sunday"))

library(data.table) # we like data.table

dt <- data.table::as.data.table(df)

# simple table of averages by day of week
dt[, .(nObs = .N,
       meanPower = mean(charge_power_kw),
       medianPower = median(charge_power_kw),
       minPower = min(charge_power_kw),
       maxPower = max(charge_power_kw)),
   keyby = .(day_of_week)]

summaryDT <- dt[, .(nObs = .N,
                    meanPower = mean(charge_power_kw)),
                keyby = .(hour, day_of_week, month, id)
                ]

library(ggplot2)
ggplot(summaryDT, aes(x = day_of_week, y = hour, fill = nObs)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(id ~ month)

ggplot(summaryDT, aes(x = day_of_week, y = hour, fill = meanPower)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_grid(id ~ month)

