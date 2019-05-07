# to see if we can 'fill in' the timestamps that don't exist due to the ftf black box
# not collecting data when the car is powered off (not charging/not in use)

library(readr)
library(data.table)
library(lubridate)

# load the data we have

# data file to use
file <- "EVBB_processed_all_v1.0_20180125.csv"
# for Mac
user <- Sys.info()[[7]]
if(user == "ben"){
  dPath <- "/Volumes/hum-csafe/Research Projects/GREEN Grid/externalData/flipTheFleet/safe/testData/2019_01_25/"
  dFile <- paste0(dPath, file, ".zip") # use zipped data
  if(!file.exists(dFile)) {
    # we probably don't have the HCS mounted so switch to local
    dPath <- "~/Data/NZ_GREENGrid/ftf/"
    dFile <- paste0(dPath, file, ".zip")
  }
} else {
  # for Xubuntu:
  dPath <- "/run/user/1001/gvfs/smb-share:server=storage.hcs-p01.otago.ac.nz,share=hum-csafe,user=student%5Cparra358/Research Projects/GREEN Grid/externalData/flipTheFleet/safe/testData/2019_01_25/"
  dFile <- paste0(dPath, "EVBB_processed_all_v1.0_20180125.csv")
}

print(paste0("Using ", dFile))
rawDF <- readr::read_csv(dFile) # creates a tidyverse tibble https://www.tidyverse.org/articles/2018/01/tibble-1-4-1/
# remove location var as it is NA but may lead to readers/ftf members thinking we know their location when we don't
rawDF$location <- NULL

# remove the dayid as we don't need it
rawDF$dayid <- NULL

# tbh the month & day_of_week vars aren't needed either as we have the date...

rawDT <- data.table::as.data.table(rawDF) # so we can do data.table stuff

rawDT <- rawDT[, r_dateTime := lubridate::as_datetime(paste0(date, time))]

# make a plot of the number of EVs seen per day/hour

plotDT <- rawDT[, .(nEVs = uniqueN(id),
                    nObs = .N), keyby = .(time15m = hms::trunc_hms(time, 15*60),
                                                   date = lubridate::date(r_dateTime))]
plotDT <- plotDT[, nObsPerEV := nObs/nEVs]


ggplot2::ggplot(plotDT, aes(x = date, y = time15m, alpha = nEVs)) +
  geom_tile()

ggplot2::ggplot(plotDT, aes(x = date, y = time15m, alpha = nObs)) +
  geom_tile()

ggplot2::ggplot(plotDT, aes(x = date, y = time15m, alpha = nObsPerEV)) +
  geom_tile()

# make a list of vehicles with start & end dates

vehicleIDsDT <- rawDT[, .(nObs = .N,
                          startTime = min(r_dateTime),
                          endTime = max(r_dateTime)), keyby = .(id)]

vehicleIDsDT <- vehicleIDsDT[, nDays := as.Date(endTime) - as.Date(startTime)]

head(vehicleIDsDT[order(nObs)])

# notice that some ids have very few observations
allTimesDT <- data.table::data.table() # data bucket
IdList <- vehicleIDsDT[, id] # get the list of ids
n <- 1
for(hh in IdList){
  # yes we could probably lapply this
  message("Imputing times for vehicle ", n , " of ", length(idList))
  from <- vehicleIDsDT[id == hh, startTime] # first obs
  to <- vehicleIDsDT[id == hh, endTime] # last obs
  imputedTimes <- seq(from, to , "1 min") # sequence of 1 minute times between from & to
  tempDT <- data.table::as.data.table(imputedTimes) # make a data.table
  tempDT <- tempDT[, id := hh] # add id back
  data.table::setnames(tempDT, "x", "r_dateTime") # set name of time var
  allTimesDT <- rbind(allTimesDT, tempDT)
  n <- n + 1
}
# seq(lubridate::as_datetime("2019-04-01 01:00:00"), lubridate::as_datetime("2019-04-10 01:00:00"), "10 mins")

# truncate minute observations back to the start minute so they can be matched
allTimesDT <- allTimesDT[, r_dateTimeImputed := lubridate::floor_date(r_dateTime, unit = "minutes", 1)]
setkey(allTimesDT, id, r_dateTimeImputed)
rawDT <- rawDT[, r_dateTimeImputed := lubridate::floor_date(r_dateTime, unit = "minutes", 1)]
setkey(rawDT, id, r_dateTimeImputed)

imputedDT <- rawDT[allTimesDT]
summary(imputedDT)
summary(allTimesDT)
