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
#dataFile <- "EVBB_processed_all_v1.0_20180125" # previous
#dataFile <- "EVBB_processed_all_v1.0_20180125_expanded" # expanded
#dataFile <- "EVBB_processed_all_v1.0_2019060" # latest
dataFile <- "EVBB_processed_all_v2.0_20190604_expanded" # expanded

# > for Mac ----
user <- Sys.info()[[7]]
if(user == "ben"){
  dPath <- "/Volumes/hum-csafe/Research Projects/GREEN Grid/externalData/flipTheFleet/safe/testData/2019_01_25/"
  dFile <- paste0(dPath, dataFile, ".zip") # use zipped data
  if(!file.exists(dFile)) {
    # we probably don't have the HCS mounted so switch to local
    dPath <- "~/Data/NZ_FlipTheFleet/"
    dFile <- paste0(dPath, dataFile, ".csv.gz")
  }
} else {
  # > for Xubuntu ----
  dPath <- "/run/user/1001/gvfs/smb-share:server=storage.hcs-p01.otago.ac.nz,share=hum-csafe,user=student%5Cparra358/Research Projects/GREEN Grid/externalData/flipTheFleet/safe/testData/2019_01_25/"
  dFile <- paste0(dPath, dataFile, ".csv")
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
  
  # remove the vars don't need to save memory
  rawDF$dayid <- NULL
  rawDF$fractime <- NULL
  rawDF$month <- NULL
  rawDF$day_of_week <- NULL
  
  # convert to data.table as much faster
  rawDT <- data.table::as.data.table(rawDF) # so we can do data.table stuff
  print(head(rawDT))
  
  return(rawDT)
}

makeCleanData <- function(rawData){
  # Clean data ----
  # https://www.rdocumentation.org/packages/data.table/versions/1.12.2/topics/copy
  cleanDT <- data.table::copy(drake::readd(rawData)) # need to do this otherwise all the following cleaning acts by reference on rawDT
  
  # Combine date and time columns into POSIXct datetime ----
  # NB if this is the imputed file it will include the imputed dateTimes
  # so this step is slightly redundant
  cleanDT[, dateTime := lubridate::as_datetime(paste0(date, time))]
  
  # over-write dateTime where imputed file used ----
  # should fail gracefully if variable absent (non-imputed file)
  try(cleanDT[, dateTime := r_dateTimeImputed])
  
  # df$dateTime <- lubridate::as_datetime(paste0(df$date, df$time))
  
  # set correct order for days of the week ----
  cleanDT[, day_of_week := ordered(day_of_week, 
                                          levels=c("Monday", "Tuesday", "Wednesday",
                                                   "Thursday", "Friday", "Saturday", "Sunday"))]
  # over-write charge_power_kw where imputed file used ----
  cleanDT[, charge_power_kw_obs := charge_power_kw]
  cleanDT[, charge_power_kw := ifelse(is.na(charge_power_kw),
                                      0,
                                      charge_power_kw_obs)]
  
  # > removal of odd charge_power_kw values ----
  # "...charging stations are being developed with capacities of 120kW in New Zealand"
  # (Concept Consulting report)
  #df$charge_power_kw[df$charge_power_kw > 120] <- NA
  cleanDT[charge_power_kw > 120, charge_power_kw := NA]
  
  # set charge type based on chare values ----
  cleanDT[, chargeType := ifelse(charge_power_kw == 0, "Not charging", NA)]
  cleanDT[, chargeType := ifelse(charge_power_kw > 0, "Standard charging", chargeType)]
  cleanDT[, chargeType := ifelse(charge_power_kw > 7, "Fast charging", chargeType)]
  
  # set charge flag ----
  cleanDT[, chargeFlag := "Not classified (what is this??)"]
  cleanDT[, chargeFlag := ifelse(charge_power_kw == 0, "Not charging (0 kW)", chargeFlag)]
  cleanDT[, chargeFlag := ifelse(shift(charge_power_kw, type = "lag") == 0 & # previous = 0
                                          charge_power_kw > 0 & # this one = charging
                                          shift(charge_power_kw, type = "lead") == 0,# next one is 0
                                        "Single charge observation", chargeFlag), by = id] 
  
  
  
  cleanDT <- cleanDT[, qHour := hms::trunc_hms(time, 15*60)] # truncate to previous 15 min
  #df$qHour <- format(as.POSIXct(hms::trunc_hms(df$time, 15*60)), "%H:%M")
  
  # Month as ordered factor ----
  cleanDT[, month := factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                                     "Jun", "Jul", "Aug", "Sep", "Oct",
                                                     "Nov", "Dec"))]
  
  # Create factor for weekdays/weekends ----
  cleanDT[, weekdays := "Weekdays"]
  cleanDT[, weekdays := ifelse(day_of_week == "Saturday" |
                                        day_of_week == "Sunday", "Weekends", weekdays)]
  # weekdays1 <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  # df$weekday <- factor((df$day_of_week %in% weekdays1), 
  #                    levels = c(TRUE, FALSE), labels = c('Weekday', 'Weekend'), ordered = TRUE)
  
  # remove impossible state of charge percentage values ----
  # reduce name first
  setnames(cleanDT, "state_of_charge_percent", "SoC_percent")
  cleanDT[SoC_percent > 100, SoC_percent := NA]
  cleanDT[SoC_percent < 0, SoC_percent := NA]
  
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
  cleanDT[, dateTimeDiff := dateTime - shift(dateTime), by = id]
  
  # locate in peak/not peak ----
  cleanDT[, peakPeriod := "Not peak"]
  cleanDT[, startTime := hms::as.hms(dateTime)]
  cleanDT[, peakPeriod := ifelse(startTime >= amPeakStart & 
                                              startTime <= amPeakEnd,
                                            "Morning peak",
                                            peakPeriod)]
  cleanDT[, peakPeriod := ifelse(startTime >= pmPeakStart & 
                                              startTime <= pmPeakEnd,
                                            "Evening peak",
                                            peakPeriod)]
  return(cleanDT)
}

# Make the plan ----
plan <- drake::drake_plan(
  rawData = getData(dFile),
  cleanData = makeCleanData(rawData), # uses drake::readd(rawData) within function
  # report = rmarkdown::render(
  #    knitr_in("EVBB_reportContent.Rmd"),
  #    params = list(title = "Analysis of electric vehicle usage patterns in New Zealand",
  #               subtitle = paste0("Analysis of ", dataFile),
  #               dFile = dFile),
  #    output_file = file_out(paste0("EVBB_report_",
  #                                  dataFile, ".html")),
  #    quiet = FALSE
  )
)


# test it ----
plan

config <- drake_config(plan)
vis_drake_graph(config)

# do it ----
make(plan)