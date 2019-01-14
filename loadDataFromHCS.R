# Loads data from Otago HCS
library(readr)

# Will only work if you have mounted the HCS as a volume (on Mac) or drive. This may require the VPN if you are off campus.

# Adjust pathname for your platform

# this works on a Mac:
dPath <- "/Volumes/hum-csafe/Research Projects/GREEN Grid/Self_contained_Projects/2018_evChargingRafferty/data/"

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
