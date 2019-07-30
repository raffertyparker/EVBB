# Version 2 of EVBB report

# Major changes:
# - uses drake to create & process data
# - parameterises input data so can use expanded synthetic observations file (or any other file of same format)

# Packages needed ----
# use require so it fails if package missing
require(data.table)
require(drake)
require(hms) # for hh:mm:ss if we need it
require(lubridate) # for date & time manip

# library(ggjoy)
# library(dplyr) # for filter
# library(forcats) # used to reverse days of week in joy plots


# Parameters ----

# > Data file to use ----
#dataFile <- "EVBB_processed_all_v1.0_20180125.csv" # latest
dataFile <- "EVBB_processed_all_v1.0_20180125_expanded.csv" # expanded

# > for Mac ----
user <- Sys.info()[[7]]
if(user == "ben"){
  dPath <- "/Volumes/hum-csafe/Research Projects/GREEN Grid/externalData/flipTheFleet/safe/testData/2019_01_25/"
  dFile <- paste0(dPath, dataFile, ".zip") # use zipped data
  if(!file.exists(dFile)) {
    # we probably don't have the HCS mounted so switch to local
    dPath <- "~/Data/NZ_FlipTheFleet/"
    dFile <- paste0(dPath, dataFile, ".gz")
  }
} else {
  # > for Xubuntu ----
  dPath <- "/run/user/1001/gvfs/smb-share:server=storage.hcs-p01.otago.ac.nz,share=hum-csafe,user=student%5Cparra358/Research Projects/GREEN Grid/externalData/flipTheFleet/safe/testData/2019_01_25/"
  dFile <- paste0(dPath, "EVBB_processed_all_v1.0_20180125.csv")
}

# > colours ----
peaksAlpha <- 0.1
peaksCol <- "#0072B2" # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette

# > defn of peak ----
amPeakStart <- hms::as.hms("07:00:00")
amPeakEnd <- hms::as.hms("09:00:00")
pmPeakStart <- hms::as.hms("17:00:00") # see https://www.electrickiwi.co.nz/hour-of-power
pmPeakEnd <- hms::as.hms("21:00:00") # see https://www.electrickiwi.co.nz/hour-of-power

# Functions ----

getData <- function(dFile){
  print(paste0("Using ", dFile))
  rawDF <- readr::read_csv(dFile) # creates a tidyverse tibble https://www.tidyverse.org/articles/2018/01/tibble-1-4-1/
  # remove location var as it is NA but may lead to readers/ftf members thinking we know their location when we don't
  rawDF$location <- NULL
  
  # remove the dayid & fractime as we don't need them
  rawDF$dayid <- NULL
  rawDF$fractime <- NULL
  
  # convert to data.table as much faster
  rawDT <- data.table::as.data.table(rawDF) # so we can do data.table stuff
  # Combine date and time columns into POSIXct datetime ----
  # NB if this is the imputed file this will not include the imputed dateTimes
  rawDT[, dateTime := lubridate::as_datetime(paste0(date, time))]
  
  # over-write dateTime where imputed file used ----
  # should fail gracefully
  try(rawDT[, dateTime := r_dateTimeImputed])
  
  # over-write charge_power_kw where imputed file used ----
  rawDT[, charge_power_kw_obs := charge_power_kw]
  rawDT[, charge_power_kw := ifelse(is.na(charge_power_kw),
                                    0,
                                    charge_power_kw_obs)]
  
  # df$dateTime <- lubridate::as_datetime(paste0(df$date, df$time))
  
  # set correct order for days of the week ----
  rawDT <- rawDT[, day_of_week := ordered(day_of_week, 
                                          levels=c("Monday", "Tuesday", "Wednesday",
                                                   "Thursday", "Friday", "Saturday", "Sunday"))]
  # set charge type ----
  rawDT <- rawDT[, chargeType := ifelse(charge_power_kw == 0, "Not charging", NA)]
  rawDT <- rawDT[, chargeType := ifelse(charge_power_kw > 0, "Standard charging", chargeType)]
  rawDT <- rawDT[, chargeType := ifelse(charge_power_kw > 7, "Fast charging", chargeType)]
  
  # Rename vehicle ids to something more user-friendly ----
  rawDT <- rawDT[,dvID := factor(id, ordered = TRUE)]
  levSeq <- seq(1:length(levels(rawDT$dvID)))
  levSeqChar <- as.character(levSeq)
  rawDT <- rawDT[, dvID := factor(dvID,
                                  labels = levSeqChar)]
  rawDT <- rawDT[, dvID := paste("Vehicle", dvID, sep = " ")]
  
  rawDT <- setnames(rawDT, c("state_of_charge_percent"), c("SoC_percent"))
  
  rawDT <- rawDT[, qHour := hms::trunc_hms(time, 15*60)] # truncate to previous 15 min
  #df$qHour <- format(as.POSIXct(hms::trunc_hms(df$time, 15*60)), "%H:%M")
  
  # Month as ordered factor ----
  rawDT <- rawDT[, month := factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                                     "Jun", "Jul", "Aug", "Sep", "Oct",
                                                     "Nov", "Dec"))]
  
  
  # Create factor for weekdays/weekends ----
  rawDT <- rawDT[, weekdays := "Weekdays"]
  rawDT <- rawDT[, weekdays := ifelse(day_of_week == "Saturday" |
                                        day_of_week == "Sunday", "Weekends", weekdays)]
  # weekdays1 <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  # df$weekday <- factor((df$day_of_week %in% weekdays1), 
  #                    levels = c(TRUE, FALSE), labels = c('Weekday', 'Weekend'), ordered = TRUE)
  return(rawDT)
}

makeCleanData <- function(rawData){
  # Clean data ----
  # https://www.rdocumentation.org/packages/data.table/versions/1.12.2/topics/copy
  cleanDT <- data.table::copy(drake::readd(rawData)) # need to do this otherwise all the following cleaning acts by reference on rawDT
  # removal of silly state of charge percentage values ----
  #df$SoC_percent[df$SoC_percent > 100] <- NA
  cleanDT[SoC_percent > 100, SoC_percent := NA]
  #df$SoC_percent[df$SoC_percent < 0] <- NA
  cleanDT[SoC_percent < 0, SoC_percent := NA]
  
  # > removal of odd charge_power_kw values ----
  # "...charging stations are being developed with capacities of 120kW in New Zealand"
  # (Concept Consulting report)
  #df$charge_power_kw[df$charge_power_kw > 120] <- NA
  cleanDT[charge_power_kw > 120, charge_power_kw := NA]
  
  # > remove vehicles with all-zero charging values ----
  # also removes those with very few observations
  summaryDT <- cleanDT[, .(mean = mean(charge_power_kw, na.rm = TRUE), 
                           sd = sd(charge_power_kw, na.rm = TRUE), 
                           nObs = .N), keyby = .(dvID)]
  includeDT <- summaryDT[mean != 0, .(dvID)] # include where mean kw > 0 - just keep id variable (not the summary stats as well)
  setkey(includeDT, dvID)
  setkey(cleanDT, dvID)
  
  cleanDT <- cleanDT[includeDT]
  
  # set key (& order) to id & dateTime ----
  # crucial for various calculations
  setkey(cleanDT, id, dateTime)
  
  # calculate time diff from 1 obs to the next ----
  cleanDT <- cleanDT[, dateTimeDiff := dateTime - shift(dateTime), by = id]
  
  # locate in peak/not peak ----
  cleanDT <- cleanDT[, peakPeriod := "Not peak"]
  cleanDT <- cleanDT[, startTime := hms::as.hms(dateTime)]
  cleanDT <- cleanDT[, peakPeriod := ifelse(startTime >= amPeakStart & 
                                              startTime <= amPeakEnd,
                                            "Morning peak",
                                            peakPeriod)]
  cleanDT <- cleanDT[, peakPeriod := ifelse(startTime >= pmPeakStart & 
                                              startTime <= pmPeakEnd,
                                            "Evening peak",
                                            peakPeriod)]
  
  # Create charge flag ----
  cleanDT <- cleanDT[, chargeFlag := "Not classified (what is this??)"]
  
  cleanDT <- cleanDT[, chargeFlag := ifelse(charge_power_kw == 0, "Not charging (0 kW)", chargeFlag)]
  
  cleanDT <- cleanDT[, chargeFlag := ifelse(shift(charge_power_kw, type = "lag") == 0 & # previous = 0
                                              charge_power_kw > 0 & # this one = charging
                                              shift(charge_power_kw, type = "lead") == 0,# next one is 0
                                            "Single charge observation", chargeFlag), by = id] 
  
  # test method 1: use 120 second threshold ----     
  clean1DT <- cleanDT[, chargeFlag := ifelse(shift(charge_power_kw, type = "lag") == 0 & # previous = 0
                                               charge_power_kw > 0 & # this one = charging
                                               shift(charge_power_kw, type = "lead") > 0 & # next one also charging
                                               dateTimeDiff < 120, #  and within 2 minutes
                                             "First charge obs in a seq", chargeFlag), by = id] 
  
  clean1DT <- clean1DT[, chargeFlag := ifelse(shift(charge_power_kw, type = "lag") > 0 &  # previous = charging and
                                                shift(dateTimeDiff, type = "lag") < 120 & # within 2 minutes
                                                charge_power_kw > 0 & # this one = charging
                                                shift(charge_power_kw, type = "lead") > 0,  # next one also charging
                                              "Charging in a seq", chargeFlag), by = id] 
  
  clean1DT <- clean1DT[, chargeFlag := ifelse(shift(charge_power_kw, type = "lag") > 0 & # previous = charging
                                                shift(dateTimeDiff, type = "lag") < 120 & # within 2 minutes
                                                charge_power_kw > 0 & # this one = charging
                                                shift(charge_power_kw, type = "lead") == 0, # next one not charging
                                              "Last charge in a seq", chargeFlag), by = id] 
  
  sequenceMethod1_T <- table(clean1DT$chargeFlag, clean1DT$chargeType, useNA = "always")
  
  #cleanDT$chargeFlag <- ordered(cleanDT$chargeFlag, levels=c("first", "charging", "last"))
  
  # test method 2: no 120 second threshold ----     
  clean2DT <- cleanDT[, chargeFlag := ifelse(shift(charge_power_kw, type = "lag") == 0 & # previous = 0
                                               charge_power_kw > 0 & # this one = charging
                                               shift(charge_power_kw, type = "lead") > 0 , # next one also charging
                                             #dateTimeDiff < 120, #  and within 2 minutes
                                             "First charge obs in a seq", chargeFlag), by = id] 
  
  clean2DT <- clean2DT[, chargeFlag := ifelse(shift(charge_power_kw, type = "lag") > 0 &  # previous = charging and
                                                #shift(dateTimeDiff, type = "lag") < 120 & # within 2 minutes
                                                charge_power_kw > 0 & # this one = charging
                                                shift(charge_power_kw, type = "lead") > 0,  # next one also charging
                                              "Charging in a seq", chargeFlag), by = id] 
  
  clean2DT <- clean2DT[, chargeFlag := ifelse(shift(charge_power_kw, type = "lag") > 0 & # previous = charging
                                                #shift(dateTimeDiff, type = "lag") < 120 & # within 2 minutes
                                                charge_power_kw > 0 & # this one = charging
                                                shift(charge_power_kw, type = "lead") == 0, # next one not charging
                                              "Last charge in a seq", chargeFlag), by = id] 
  
  sequenceMethod2_T <- table(clean2DT$chargeFlag, clean2DT$chargeType, useNA = "always")
  
  # applying the 120 second rule reduces the number categorised as it will not know what to do with:
  # charge -> gap of > 120 secs -> charge <gap of > 120 secs -> charge
  # for now we therefore do not use the 120 second rule (see annex #chargeFlagTest for reporting)
  
 # use no threshold data
  
  return(clean2DT)
}

# Make the plan ----
plan <- drake::drake_plan(
  rawData = getData(dFile),
  cleanData = makeCleanData(rawData), # uses drake::readd(rawData) within function
  report = rmarkdown::render(
     knitr_in("EVBB_reportContent.Rmd"),
     params = list(title = "Analysis of electric vehicle usage patterns in New Zealand",
                subtitle = paste0("Analysis of ", dataFile),
                dFile = dFile),
     output_file = file_out(paste0("EVBB_report_",
                                   dataFile, ".html")),
     quiet = TRUE
  )
)


# test it ----
plan

config <- drake_config(plan)
vis_drake_graph(config)

# do it ----
make(plan)