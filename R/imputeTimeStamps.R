# to see if we can 'fill in' the timestamps that don't exist due to the ftf black box
# not collecting data when the car is powered off (not charging/not in use)

library(readr)
library(data.table)
library(ggplot2)
library(here)
library(lubridate)

# load the data we have

# data file to use
#fileName <- "EVBB_processed_all_v1.0_20180125"
fileName <- "EVBB_processed_all_v2.0_20190604"

pPath <- paste0(here::here(), "/plots/")

# for Mac
user <- Sys.info()[[7]]
if(user == "ben"){
    dPath <- "~/Data/NZ_FlipTheFleet/processed/"
    dFile <- paste0(dPath, fileName, ".csv.gz")
    oPath <- "~/Data/NZ_FlipTheFleet/processed/"
} else {
  # for Xubuntu:
  dPath <- "/run/user/1001/gvfs/smb-share:server=storage.hcs-p01.otago.ac.nz,share=hum-csafe,user=student%5Cparra358/Research Projects/GREEN Grid/externalData/flipTheFleet/safe/testData/2019_01_25/"
  dFile <- paste0(dPath, "EVBB_processed_all_v1.0_20180125.csv")
}

print(paste0("Using ", dFile))
rawDF <- readr::read_csv(dFile) # creates a tidyverse tibble https://www.tidyverse.org/articles/2018/01/tibble-1-4-1/

rawDT <- data.table::as.data.table(rawDF) # so we can do data.table stuff

# make a list of vehicles with start & end dates

vehicleIDsDT <- rawDT[, .(nObs = .N,
                          startTime = min(r_dateTime),
                          endTime = max(r_dateTime)), keyby = .(id,dvID)]

vehicleIDsDT <- vehicleIDsDT[, nDays := as.Date(endTime) - as.Date(startTime)]

head(vehicleIDsDT[order(nObs)])

# notice that some ids have very few observations
allTimesDT <- data.table::data.table() # data bucket
IdList <- vehicleIDsDT[, id] # get the list of ids
n <- 1
for(hh in IdList){
  # yes we could probably lapply this
  message("Imputing times for vehicle ", n , " of ", length(IdList))
  from <- vehicleIDsDT[id == hh, startTime] # first obs
  to <- vehicleIDsDT[id == hh, endTime] # last obs
  imputedTimes <- seq(from, to , "1 min") # sequence of 1 minute times between from & to
  tempDT <- data.table::as.data.table(imputedTimes) # make a data.table
  tempDT <- tempDT[, id := hh] # add id back
  tempDT <- tempDT[, dvID := vehicleIDsDT[id == hh, dvID]] # add id back
  data.table::setnames(tempDT, "x", "r_dateTime") # set name of time var
  allTimesDT <- rbind(allTimesDT, tempDT)
  n <- n + 1
}
# seq(lubridate::as_datetime("2019-04-01 01:00:00"), lubridate::as_datetime("2019-04-10 01:00:00"), "10 mins")

# truncate minute observations back to the start minute so they can be matched
allTimesDT[, r_dateTimeImputed := lubridate::floor_date(r_dateTime, unit = "minutes", 1)]
setkey(allTimesDT, id, dvID, r_dateTimeImputed) # need id & dvID to show up on all rows
rawDT[, r_dateTimeImputed := lubridate::floor_date(r_dateTime, unit = "minutes", 1)]
setkey(rawDT, id, dvID, r_dateTimeImputed) # need id & dvID to show up on all rows

message("Summary of allTimesDT: ")
summary(allTimesDT)

message("Summary of rawDT: ")
head(rawDT)
summary(rawDT)

# link imputed times to observed data ----
imputedDT <- rawDT[allTimesDT]

# set charge power to 0 where not observed
imputedDT[, charge_power_kw_imputed := ifelse(is.na(charge_power_kw),
                                              0, 
                                              charge_power_kw)]
# remove vars we don't need to save space
colsToDelete <- c("i.r_dateTime")
imputedDT[, (colsToDelete) := NULL]

message("Summary of imputedDT: ")
head(imputedDT)
summary(imputedDT)

oFile <- paste0(oPath, fileName, "_expanded.csv")
data.table::fwrite(imputedDT, oFile)
dkUtils::gzipIt(oFile)

# data tests ----
imputedDT[, date_hour := lubridate::floor_date(r_dateTimeImputed, unit = "hour")]
plotDT <- imputedDT[,.(nObs = .N), keyby = .(date_hour, dvID)]
# this should be a flat line of 60
plotDT <- imputedDT[, .(nObs = .N), by = .(date_hour, dvID)]
p <- ggplot2::ggplot(plotDT, aes(x = date_hour, y = nObs, colour = dvID)) +
  geom_line() +
  facet_wrap(. ~ dvID) + 
  theme(legend.position = "none") +
  geom_hline(aes(yintercept = 60, colour = "red")) +
  labs(caption = paste0("Data: ", fileName,
                        "\nWould expect 60 per hour (reference line)"))

ggplot2::ggsave(paste0(pPath, fileName, "_expanded_nObsPerEVPerHour.png"))

setkey(imputedDT, id, r_dateTimeImputed) # crucial
imputedDT[, dateTimeDiff := r_dateTimeImputed - shift(r_dateTimeImputed, type = "lag"), by = id]

imputedDT[ , .(nObs = .N,
               mean = mean(dateTimeDiff, na.rm = TRUE),
               min = min(dateTimeDiff, na.rm = TRUE),
               max = max(dateTimeDiff, na.rm = TRUE)), keyby = dvID]

message("Done!")