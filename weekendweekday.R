# This shows differences in charging patterns between the weekend and weekdays.
# Currently not accurate because charge_power_kw = 0 are not included
# thus data doesn't include times where the car is not charging

p <- ggplot2::ggplot(df, aes(x = time, y = charge_power_kw, group = halfHour)) +
  density(alpha = 0.5) # <- make the plot in an object first

p + labs(y = "Power (kW)", x = "Time of day") + facet_grid(~weekday)  +
  stat_summary(aes(group = weekday), fun.y=mean, geom="line", colour="green") 


# Density plot of charge times according to weekday/charging rate
# Probably not useful or interesting but looks kinda cool
ggplot(df, aes(x = halfHour, fill = charging_rate)) +
  theme_bw() +
  facet_wrap(~weekday) +
  geom_density(alpha = 0.5) +
  labs(y = "Power (kW)", x = "Time of day")


# As previous but bar plot instead of density plot
# Looks shoddy but more informative
ggplot(df, aes(x = halfHour, y = charge_power_kw, fill = charging_rate)) +
  geom_bar(stat = "summary", fun.y = mean) +
  theme_bw() +
  facet_wrap(~weekday) +
  labs(y = "Power (kW)", x = "Time of day", fill = "Charging rate")


## The plot below displays error message 
# "Error: geom_density requires the following missing aesthetics: y"
# despite y being clearly defined. Ask Ben about this.
ggplot(df, aes(x = charge_power_kw, fill = charging_rate)) +
  geom_density() +
  facet_wrap(~weekday) +
  labs(y = "Power (kW)", x = "Time of day", fill = "Charging rate")


# The abomination below pillages Daniel's code and makes it shit
ggplot(df, aes(x = time, y = charge_power_kw, group = id)) +
  geom_line(alpha=0.1, position = "identity") +
  scale_x_continuous(breaks = seq(0,24,by=4)) +
  coord_cartesian(xlim = c(0,24),ylim=c(0,3.5)) +
  xlab("Time of day (hour)")+
  ylab("Charging power (kW)")+
  stat_summary(aes(group = halfHour), fun.y = mean, geom = "line", colour = "green") +
  annotate("rect", xmin = 6.5, xmax = 9.5, ymin = 0, ymax = 3.5,
           alpha = .1, fill="#E69F00") +
  annotate("rect", xmin = 17, xmax = 21, ymin = 0, ymax = 3.5,
           alpha = .1, fill="#E69F00") +
  annotate("text",label="Morning Peak",x=8,y=2.5, angle=90, colour="#E69F00") +
  annotate("text",label="Evening Peak",x=19,y=2.5, angle=90, colour="#E69F00") +
  facet_wrap(~ weekday)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

p <- ggplot2::ggplot(df, aes(x = halfHour, group = halfHour, y = charge_power_kw)) +
  geom_boxplot()


# 
df$halfHour <- factor(as.character(df$halfHour), ordered = TRUE)

list_of_times <- levels(df$halfHour)

# NOTE it is important this plot uses facet_wrap(~weekday), otherwise the values given are twice as large as they should be
# This actually isn't great, like the other plots in this script it doesn't take into consideration the zeros, and
# Block above gives the same plot with less steps.
# Note the same plot was constructed above in a far more succinct manner.
require(plyr)
hhmeans <- ddply(df,c("halfHour","charging_rate", "weekday"), summarise, 
                 mean = mean(charge_power_kw))

ggplot(hhmeans[order(hhmeans$charging_rate), ], aes(x = halfHour, y = mean, fill = charging_rate)) +
  theme_bw() +
  facet_wrap(~weekday) +
  geom_bar(stat = "identity") +
  labs(y = "Power (kW)", x = "Time of day", fill = "Charging Rate")
  

