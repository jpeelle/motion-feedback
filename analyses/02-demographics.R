
library(tidyverse)


# read in demographic data
dfp <- read.csv("../data/participants.csv")

# make sure these subjects aren't NA
dfp <- dfp[!is.na(dfp$subject_number),]

# summarize each age group and feedback/nofeedback
  dfp %>%
  group_by(age_group, feedback) %>%
  summarize(N = n(),
            minage = min(age),
            maxage = max(age),
            meanage = mean(age),
            SDage = sd(age))
  