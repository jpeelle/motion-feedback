
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

  # A tibble: 4 × 7
  # Groups:   age_group [2]
  # age_group feedback     N minage maxage meanage SDage
  # <chr>        <int> <int>  <int>  <int>   <dbl> <dbl>
  #   1 older            0     5     65     79    70.4  5.22
  # 2 older            1    25     66     81    71.3  3.81
  # 3 young            0    17     20     30    23.3  3.06
  # 4 young            1    31     19     29    22.4  2.64
  # > 
    
  
  
  dfp %>% count(age_group, feedback, sex)
  # age_group feedback sex  n
  # 1     older        0   F  5
  # 2     older        1   F 17
  # 3     older        1   M  8
  # 4     young        0   F 13
  # 5     young        0   M  4
  # 6     young        1   F 28
  # 7     young        1   M  3