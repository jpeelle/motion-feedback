
library(tidyverse)


# read in demographic data
dfp <- read.csv("../data/participants.csv")


# summarize each age group and feedback/nofeedback
  dfp %>%
  group_by(age_group, FIRMM) %>%
  summarize(N = n(),
            minage = min(age),
            maxage = max(age),
            meanage = mean(age),
            SDage = sd(age))