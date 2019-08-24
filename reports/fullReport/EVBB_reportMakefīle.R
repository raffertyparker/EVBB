# Calls EVBB report with specific data version

# Major changes:
# - parameterises input data so can easily switch

# Packages needed ----
# use require so it fails if package missing
require(bookdown)
require(data.table)
require(here)
require(rmarkdown)

# Parameters ----

# > Data file to use ----
#dataFile <- "EVBB_processed_all_v1.0_20180125" # previous
dataFile <- "EVBB_processed_all_v2.0_20190604" # latest

# > Data location ----
sysname <- Sys.info()[[1]]
nodename <- Sys.info()[[4]]
user <- Sys.info()[[7]]

# > for Raff on Xubuntu ----
dPath <- "/run/user/1001/gvfs/smb-share:server=storage.hcs-p01.otago.ac.nz,share=hum-csafe,user=student%5Cparra358/Research Projects/GREEN Grid/externalData/flipTheFleet/safe/testData/2019_01_25/"
dFile <- paste0(dPath, dataFile, ".csv")

if(user == "dataknut" & nodename == "gridcrawler"){
  # for CS RStudio server
  # use local
  dPath <- path.expand("/home/dataknut/greenGridData/Self_contained_Projects/2018_evChargingRafferty/data/processed/")
  dFile <- paste0(dPath, dataFile, ".csv.gz")
}
if(user == "ben"){
 # for Mac
 # use local
  dPath <- path.expand("~/Data/NZ_FlipTheFleet/processed/")
  dFile <- paste0(dPath, dataFile, ".csv.gz")
}

# > colours ----
peaksAlpha <- 0.1
peaksCol <- "#0072B2" # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette

# > defn of peak ----
amPeakStart <- hms::as.hms("07:00:00")
amPeakEnd <- hms::as.hms("09:00:00")
pmPeakStart <- hms::as.hms("17:00:00") # see https://www.electrickiwi.co.nz/hour-of-power
pmPeakEnd <- hms::as.hms("21:00:00") # see https://www.electrickiwi.co.nz/hour-of-power

rmd <- paste0(here::here(), "/reports/fullReport/EVBB_report.Rmd")
outF <- paste0(here::here(), "/docs/EVBB_report_", dataFile ,'.html') # for easier github pages management

# load the EV data ----
loadData <- function(dFile){
  print(paste0("Using ", dFile)) # <- dFile
  rawDF <- readr::read_csv(dFile) # creates a tidyverse tibble https://www.tidyverse.org/articles/2018/01/tibble-1-4-1/
  
  # convert to data.table as much faster
  dt <- data.table::as.data.table(rawDF) # so we can do data.table stuff
  # dt <- data.table::fread(dFile) # this loads data but assumes r_dateTmime is UTC which it isn't
  # could use lubridate::force_tz() to fix but readr is about as quick
  return(dt)
}

rawDT <- loadData(dFile)

# load the EA gen data ----
genPath <- paste0(here::here(), "/data/")
getGenFileList <- function(dPath){
  all.files <- list.files(path = dPath, pattern = ".csv")
  dt <- as.data.table(all.files)
  dt[, fullPath := paste0(dPath, all.files)]
  return(dt)
}

getGenData <- function(files){
  # https://stackoverflow.com/questions/21156271/fast-reading-and-combining-several-files-using-data-table-with-fread
  # this is where we need drake
  # and probably more memory
  message("Loading ", nrow(files), " files")
  l <- lapply(files$fullPath, fread)
  dt <- rbindlist(l)
  setkey(dt, rDateTime)
  file.remove("temp.csv") # side effect
  return(dt)
}

genFiles <- getGenFileList(genPath)
genDataDT <- getGenData(genFiles)

# fix dates & times ----
genDataDT <- genDataDT[!is.na(rTime)] # drop the TP49 & TP50
genDataDT[, rDateTime := lubridate::as_datetime(rDateTime)]
genDataDT[, rDateTime := lubridate::force_tz(rDateTime, tzone = "Pacific/Auckland")]
genDataDT[, date := lubridate::date(rDateTime)]
genDataDT[, month := lubridate::month(rDateTime)]
genDataDT[, day_of_week := lubridate::wday(rDateTime, label = TRUE)]
genDataDT[, hms := hms::as.hms(rDateTime)] # set to middle of half-hour
genDataDT[, halfHour := hms::trunc_hms(hms, 30*60)] # truncate to previous half-hour

# Create factor for weekdays/weekends ----
genDataDT[, weekdays := "Weekdays"]
genDataDT[, weekdays := ifelse(day_of_week == "Sat" |
                               day_of_week == "Sun", "Weekends", weekdays)]

# run report
doReport <- function(){
  message("Running ", rmd, " and saving as ", outF) 
  rmarkdown::render(input = rmd,
                    output_file=outF)
}

#doReport()



