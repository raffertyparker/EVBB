#This seperates out the fast-charges so we can see when/where they occur

EVBB_fastcharge <- EVBB_processed[EVBB_processed$charge_power_kw >= 5, ]

EVBB_fastcharge$location <- factor(EVBB_fastcharge$location)

pie(table(EVBB_fastcharge$location), main = "Location of fast charging")




# The pie chart demonstrates that there are often times during
#which fastcharge is occurring while the car is supposedly at home.