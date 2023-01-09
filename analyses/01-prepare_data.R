
library(tidyverse)

#---- read data, filter, add relevant info ----

df <- read.csv("../data/motion_data.csv")


# remove rows where motion is 0 (first frame every run)
df <- df[df$x!=0,]



# add scan counter for subject (scan) and per run (runscan)
df <- df %>% group_by(subject_number) %>% mutate(frame = row_number(subject_number))
df <- df %>% group_by(subject_number, run) %>% mutate(runframe = row_number(run))

dfframes <- df %>% 
  group_by(subject_number) %>%
  summarize(max_frames = max(frame))

# min(dfframes$max_frames)
# [1] 763
# > max(dfframes$max_frames)
# [1] 846
# > mean(dfframes$max_frames)
# [1] 799.0513
# > median(dfframes$max_frames)
# [1] 794


#---- get information from participants file to motion file ----

# read in demographic data and add age, sex, age group to main df
dfp <- read.csv("../data/participants.csv")

dfj <- left_join(df, dfp, 
                 by = c("subject_number", "feedback"),
              keep = FALSE)



#---- write ----

write.table(dfj, file = "data_filtered.csv", sep=",", row.names = FALSE)


#---- read tSNR data ----

dftSNR <- read.csv("../data/tSNR_data.csv")

dftSNRj <-  left_join(dftSNR, dfp, 
                      by = c("subject_number"),
                      keep = FALSE)

write.table(dftSNRj, file = "tSNR_data_filtered.csv", sep=",", row.names = FALSE)



