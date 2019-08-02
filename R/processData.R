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
fileName <- "EVBB_processed_all_v2.0_20190604" # <- works with v2 

pPath <- paste0(here::here(), "/plots/")

# for Mac
user <- Sys.info()[[7]]
if(user == "ben"){
    # use local
    dPath <- "~/Data/NZ_FlipTheFleet/raw/"
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

# create dateTime ----
rawDT <- rawDT[, r_dateTime := lubridate::as_datetime(paste0(date, time))]

# Rename vehicle ids to something more user-friendly ----
rawDT[,id := factor(id, ordered = TRUE)]
levSeq <- seq(1:length(levels(rawDT$id)))
levSeqChar <- as.character(levSeq)
rawDT[, dvID := factor(id,
                         labels = levSeqChar)]
rawDT[, dvID := paste("Vehicle", dvID, sep = " ")]

# data plots ----
plotDT <- rawDT[, .(nEVs = uniqueN(id),
                    nObs = .N), keyby = .(date = lubridate::date(r_dateTime))]

ggplot2::ggplot(plotDT, aes(x = date, y = nEVs)) +
  geom_line() +
  labs(caption = paste0("Data: ", fileName))
ggsave(paste0(pPath, fileName, "_nEVsLine.png"))

plotDT <- rawDT[, .(nEVs = uniqueN(id),
                    nObs = .N), keyby = .(time15m = hms::trunc_hms(time, 15*60),
                                                   date = lubridate::date(r_dateTime))]
plotDT <- plotDT[, nObsPerEV := nObs/nEVs]

p <- ggplot2::ggplot(plotDT, aes(x = date, y = time15m, alpha = nEVs)) +
  geom_tile() +
  labs(caption = paste0("Data: ", fileName))
ggplot2::ggsave(paste0(pPath, fileName, "_nEVsTile.png"))

p <- ggplot2::ggplot(plotDT, aes(x = date, y = time15m, alpha = nObs)) +
  geom_tile() +
  labs(caption = paste0("Data: ", fileName))
ggplot2::ggsave(paste0(pPath, fileName, "_nObsTile.png"))

p <- ggplot2::ggplot(plotDT, aes(x = date, y = time15m, alpha = nObsPerEV)) +
  geom_tile() +
  labs(caption = paste0("Data: ", fileName))
ggplot2::ggsave(paste0(pPath, fileName, "_nObsPerEV.png"))

# removes variables we don't need ----
colsToDelete <- c("dayid", "location","date", "month", "day_of_week", "time", "fractime")

rawDT[, (colsToDelete) := NULL]

message("Test data")
head(rawDT)

# save data ----
oFile <- paste0(oPath, fileName, ".csv")
data.table::fwrite(rawDT, oFile)
dkUtils::gzipIt(oFile)

# data tests ----
rawDT[, date_hour := lubridate::floor_date(r_dateTime, unit = "hour")]
plotDT <- rawDT[, .(nObs = .N), by = .(date_hour, dvID)]
p <- ggplot2::ggplot(plotDT, aes(x = date_hour, y = nObs, colour = dvID)) +
  geom_line() +
  facet_wrap(. ~ dvID) + 
  theme(legend.position = "none") +
  geom_hline(aes(yintercept = 60, colour = "red")) +
  labs(caption = paste0("Data: ", fileName,
                        "\nWould expect 0-60 per hour (reference line)"))

ggplot2::ggsave(paste0(pPath, fileName, "_nObsPerEVPerHour.png"))

message("Done!")