#---------------------------------
# Title: Bar Chart of Mental Health Events Reporting 
# Author: Hayley Pippin
# Last updated: November 26, 2022
# Description: Generates bar chart of state mental health reporting type.
# Inputs: n/a
# Outputs: Bar chart of mental health events that must be reported to NICS
#---------------------------------

## ==== set up ==== ##

# import libraries
library(ggplot2)

## ==== generate data ==== ##

# create list of mental health event types
event = c("Involuntarily Committed as Inpatients", "Individuals Ordered to Involuntary Outpatient Treatment",
          "Court Adjudicates Individual No Longer Competent to Manage Affairs, May Include Guardian Appointment",
          "Voluntary Hospitalization, May be in Lieu of Court Order or Certification of Mental Incompetence",
          "Voluntary Outpatient Treatment for Mental Health or Substance Abuse", "Substance Abuse Treatment",
          "Court Determines Individual Mentally Incompentent to Stand Trial",
          "Judgment that Individual is Not Guilty by Reason of Insanity",
          "Emergency Hold for Psychiatric Evaluation Lasting up to 72 Hours",
          "Non-legal and Non-Medical Stakeholder Observations that Activate Mandatory Reporting"
          )

# create list of number of states that require reporting for corresponding event
num_states = c(40, 18, 13, 5, 1, 13, 23, 23, 3, 3)

# create data frame
data = data.frame(
  event = as.factor(event),
  num_states = num_states
)

## ==== generate bar chart ==== ##

barplot = ggplot(data, aes(x = event, y = num_states, fill = event)) +
                   geom_bar(stat = "identity") +
                   scale_x_discrete(limits = levels(data$event)) +
                   scale_fill_hue(c = 40) +
                   theme_minimal() +
                   theme(legend.position = "none")





