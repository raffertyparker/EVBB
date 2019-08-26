# gets wholesale EA elec gen data
library(data.table)
library(here)
library(curl)

# parameters ----
nDays <- 30

# functions ----
stackedDemandProfilePlot <- function(dt) {
  #nHalfHours <- uniqueN(dt$rDate) * 48
  plotDT <- dt[, .(meanDailyKWh = sum(kWh)/nDays), keyby = .(rTime, Fuel_Code)]
  
  p <- ggplot(plotDT, aes(x = rTime, y = meanDailyKWh/1000000, fill = Fuel_Code)) +
    geom_area(position = "stack") +
    labs(x = "Time of Day",
         y = "Mean GWh per half hour",
         caption = "Source: NZ Electricity Authority generation data for June (winter) 2018")
  return(p)
}


getData <- function(f){
  # f <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/201808_Generation_MD.csv"
  req <- curl::curl_fetch_disk(f, "temp.csv") # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
  if(req$status_code != 404){ #https://cran.r-project.org/web/packages/curl/vignettes/intro.html#exception_handling
    df <- readr::read_csv(req$content)
    print("File downloaded successfully")
    dt <- cleanEA(df) # clean up to a dt
    return(dt)
  } else {
    print(paste0("File download failed (Error = ", req$status_code, ") - does it exist at that location?"))
  }
}

cleanEA <- function(df){
  # takes a df, cleans & returns a dt
  dt <- data.table::as.data.table(df) # make dt
  dt <- reshapeEAGenDT(dt) # make long
  dt <- setEAGenTimePeriod(dt) # set time periods to something intelligible as rTime
  dt <- dt[, rDate := as.Date(Trading_date)] # fix the dates so R knows what they are
  dt <- dt[, rDateTime := lubridate::ymd_hms(paste0(rDate, rTime))] # set full dateTime
  return(dt)
}

reshapeEAGenDT <- function(dt){
  # reshape the data as it comes in a rather unhelpful form
  reshapedDT <- melt(dt,
                     id.vars=c("Site_Code","POC_Code","Nwk_Code", "Gen_Code", "Fuel_Code", "Tech_Code","Trading_date"),
                     variable.name = "Time_Period", # converts TP1-48/49/50 <- beware of these ref DST!
                     value.name = "kWh" # energy - see https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/
  )
  return(reshapedDT)
}

setEAGenTimePeriod <- function(dt){
  # convert the given time periods (TP1 -> TP48, 49. 50) to hh:mm
  dt <- dt[, c("t","tp") := tstrsplit(Time_Period, "P")]
  dt <- dt[, mins := ifelse(as.numeric(tp)%%2 == 0, "45", "15")] # set to q past/to (mid points)
  dt <- dt[, hours := floor((as.numeric(tp)+1)/2) - 1]
  dt <- dt[, strTime := paste0(hours, ":", mins, ":00")]
  dt <- dt[, rTime := hms::as.hms(strTime)]
  # head(dt)
  dt <- dt[, c("t","tp","mins","hours","strTime") := NULL]  #remove these now we're happy
  return(dt)
}

# NZ Electricity Authority generation data
months <- c("201801", "201802","201803","201804","201805", "201806", "201807","201808","201809","201810","201811","201812")
#months <- c("201806","201807")
rDataLoc <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/"

refreshWholesaleData <- function(){
  for(m in months){
    print(paste0("Getting, processing and cleaning ", m))
    rDataLoc <- paste0("https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/", m, "_Generation_MD.csv")
    dt <- getData(rDataLoc)
    data.table::fwrite(dt, file = paste0(here::here(), "/data/EA_", m, "_Generation_MD.csv"))
  } 
}

refreshWholesaleData()
