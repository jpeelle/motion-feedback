
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(lme4)
library(lmerTest)


#---- read data, filter, add relevant info ----

df <- read.csv("../data/motion_data.csv")


# remove rows where motion is 0 (first frame every run)

df <- df[df$x!=0,]



# add scan counter
df <- df %>% group_by(subject_number) %>% mutate(scan = row_number(subject_number))


# read in demographic data and add age, sex, age group to main df
dfp <- read.csv("../data/participants.csv")


#---- get information from participants file to motion file ----

dfj <- left_join(df, dfp, 
                 by = c("subject_number", "FIRMM"),
              keep = FALSE)


# remove where we don't have demographic info
dfj <- dfj[!is.na(dfj$age_group),]

# write

write.table(dfj, file = "data_filtered.csv", sep=",", row.names = FALSE)



#---- summarize by run/subject ----

dfrun <- summarize(group_by(df, subject_number, run),
                 FIRMM=mean(FIRMM),
                 mean=mean(FD),
                 SD=sd(FD))




