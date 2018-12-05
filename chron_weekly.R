# chronologize data (weekly)

library(chron)

EVBB_processed <- read.csv("~/EVBB/data/EVBB_processed.csv", stringsAsFactors = F) 

EVBB_processed$time <- chron(times=EVBB_processed$time) # converting from character to times
EVBB_processed$timeAsNumeric <- as.numeric(EVBB_processed$time) # converting from times to numeric

EVBB_processed$day_of_week <- factor(EVBB_processed$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", 
                                                                            "Thursday", "Friday", "Saturday", 
                                                                            "Sunday")) # setting day of the week as ordered factor
EVBB_processed <- with(EVBB_processed, EVBB_processed[order(day_of_week, time),]) # orders the dataframe according to days of the week
