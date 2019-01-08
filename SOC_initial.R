# This is a cursory look at the new EV data with state of charge

library(readr)
df <- read_csv("~/EVBB/data/EVBB_processed_founders.csv")
View(df)



library(ggplot2)
library(readr)
library(knitr)
library(lubridate)
library(hms)
library(plyr)
library(dplyr)

# colour blind palettes for charts
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
# with grey
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# with black
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

iFile <- "data/EVBB_processed.csv" # set this here



t <- summary(df)


df$day_of_week <- factor(df$day_of_week, ordered = TRUE,
                         levels = c("Monday", "Tuesday", "Wednesday","Thursday",
                                    "Friday", "Saturday", "Sunday")) 
df$month <- factor(df$month, ordered = TRUE, levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                                        "Jun", "Jul", "Aug", "Sep", "Oct",
                                                        "Nov", "Dec"))

weekdays1 <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
df$weekday <- factor((df$day_of_week %in% weekdays1), 
                     levels = c(TRUE, FALSE), labels = c('Weekday', 'Weekend'), ordered = TRUE) 

df$charging_rate <- factor((df$charge_power_kw >= 7),
                           levels = c(TRUE, FALSE), labels = c('Fast', 'Slow'),
                           ordered = TRUE)


df$halfHour <- format(as.POSIXct(hms::trunc_hms(df$time, 30*60)), "%H:%M") # <- code to half hours, removing seconds


df$id <- factor(df$id, ordered = TRUE)
levSeq <- seq(1:length(levels(df$id)))
levSeqChar <- as.character(levSeq)

df$id <- factor(df$id,
                labels = levSeqChar)

df$id <- as.character(df$id)

df$id <- paste("Vehicle", df$id, sep = " ")

df$state_of_charge_percent[df$state_of_charge_percent > 100] <- NA
df$state_of_charge_percent[df$state_of_charge_percent < 0] <- NA


#######################
p <- ggplot2::ggplot(df, aes(x = charge_power_kw)) +
  guides(colour = guide_legend(title = "Vehicle:")) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=cbPalette) + # use colour-blind friendly palette
  geom_density() # <- make the plot in an object first

p + labs(x = "Power (kW)") + facet_grid(id ~ .) +
  annotate("rect", xmin = 0, xmax = 7, ymin = 0, ymax = 1.5,
           alpha = .1, fill="yellow") +
  annotate("rect", xmin = 7, xmax = 50, ymin = 0, ymax = 1.5,
           alpha = .1, fill="blue") +
  annotate("text",label="Slow", x=3.5, y=1.25, angle=0) +
  annotate("text",label="Fast", x=(50-7)/2 + 7, y=1.25, angle=0)

################################



# charging density plot by vehicle

p <- ggplot2::ggplot(df, aes(x = halfHour, group = halfHour, y = charge_power_kw)) +
  guides(colour = guide_legend(title = "Vehicle:")) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) +
  scale_colour_manual(values=cbPalette) + # use colour-blind friendly palette
  geom_boxplot() # <- make the plot in an object first



# State of charge by weekend/weekday

p <- ggplot2::ggplot(df, aes(x = halfHour, group = halfHour, y = state_of_charge_percent)) +
  guides(colour = guide_legend(title = "Vehicle:")) +
  scale_colour_manual(values=cbPalette) + # use colour-blind friendly palette
  geom_boxplot() +
  stat_summary(aes(group = weekday), fun.y=mean, geom="line", colour = "red")

p + labs(x = "Time of Day", y = "State of charge (%)") + facet_grid(~weekday) +  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) 

 