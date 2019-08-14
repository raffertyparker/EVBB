# Calls EVBB report with specific data version

# Major changes:
# - parameterises input data so can easily switch

# Packages needed ----
# use require so it fails if package missing
require(bookdown)
require(data.table)
require(drake) # adds data pre-load to speed iterative analysis
require(rmarkdown)

# Parameters ----

# > Data file to use ----
#dataFile <- "EVBB_processed_all_v1.0_20180125" # previous
dataFile <- "EVBB_processed_all_v2.0_20190604" # latest

# > for Mac ----
user <- Sys.info()[[7]]
if(user == "ben"){
 # use local
  dPath <- "~/Data/NZ_FlipTheFleet/processed/"
  dFile <- paste0(dPath, dataFile, ".csv.gz")
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

title <- "Analysis of a sample Flip The Fleet members' electric vehicle usage patterns in New Zealand"
subtitle <- paste0("Statistical report using ", dataFile)

rmd <- paste0(here::here(), "/reports/fullReport/EVBB_report.Rmd")
outF <- paste0(here::here(), "/reports/fullReport/EVBB_report_",dataFile,'.html')

message("Running ", rmd, " and saving as ", outF) 
rmarkdown::render(rmd,
                  output_format = "html_document2",
                  params = list(title = title, subtitle = subtitle, dataFile = dataFile),
                  output_file=outF)


# Make the plan ----
plan <- drake::drake_plan(
  rawData = getData(dFile),
  cleanData = makeCleanData(rawData), # uses drake::readd(rawData) within function
  report = rmarkdown::render(knitr_in(rmd),
                    params = list(title = title, subtitle = subtitle, dataFile = dataFile),
                    output_file=file_out(outF)),
  quiet = FALSE
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

