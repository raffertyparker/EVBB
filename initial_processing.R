# This processes the data further, giving useful factors and 
# chronologizing into weekly averages

library(chron)

# change filepath to read processed version of data
EVBB_tmp <- read.csv("~/EVBB/data/EVBB_processed.csv", stringsAsFactors = F)

EVBB_tmp$time <- chron(times=EVBB_tmp$time) # converting from character to times
EVBB_tmp$time_as_numeric <- as.numeric(EVBB_tmp$time) # converting from times to numeric
EVBB_tmp$day_of_week <- factor(EVBB_tmp$day_of_week, ordered = TRUE,
                                     levels = c("Monday", "Tuesday", "Wednesday", 
                                                "Thursday", "Friday", "Saturday", 
                                                  "Sunday")) # setting day of the week as ordered factor
EVBB_tmp1 <- with(EVBB_tmp, EVBB_tmp[order(day_of_week, time),]) # orders the dataframe according to days of the week

write.csv(EVBB_tmp1, file = "~/EVBB/data/EVBB_processed1.csv")
