#---------------------------------
# Title: Lollipop Chart of Time Frame for Mental Health Reporting 
# Author: Hayley Pippin
# Last updated: November 26, 2022
# Description: Generates lollipop chart of the number of states that require 
#              reporting by a specific time frame.
# Inputs: n/a
# Outputs: Lollipop chart of mental health time frame for reporting.
#---------------------------------

## ==== set up ==== ##
library(ggplot2)

## ==== generate data ==== ##

# define factor of time frames (need to specify levels for chart ordering)
timeline <-  factor(c("Immediately or Promptly", "Within 48-72 Hours", "Within 5-7 Days",
                    "Within 25-30 Days", "Timely Manner", "Unspecified"), 
                    levels = c("Immediately or Promptly", "Within 48-72 Hours", "Within 5-7 Days",
                                "Within 25-30 Days", "Timely Manner", "Unspecified"))

# create data frame with time frames and number of states that require that time frame
data <- data.frame(
  timeline = timeline,
  num_states = c(7, 7, 7, 4, 1, 24)
)

## ==== generate lollipop plot ==== ##

lolliplot <- ggplot(data, aes(x=timeline, y=num_states)) +
  geom_segment( aes(xend=timeline, y=0, yend=num_states), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) + # change size and opacity of points here
  scale_x_discrete(limits = levels(data$timeline)) + # set order of events on x-axis
  theme_light() +
  coord_flip() + # make horizontal
  labs(x = "Timeframe to Report Mental Health Events", y = "Number of States") + 
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
)



