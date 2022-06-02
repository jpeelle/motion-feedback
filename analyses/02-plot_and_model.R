
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
#df_demo <- read.csv("../data/participants.csv")



#---- summarize by session ----

dfrun <- summarize(group_by(df, subject_number, run),
                 FIRMM=mean(FIRMM),
                 mean=mean(FD),
                 SD=sd(FD))


dfsubject <- summarize(group_by(df, subject_number),
                   FIRMM=mean(FIRMM),
                   meanFD=mean(FD),
                   medianFD=median(FD),
                   SDFD=sd(FD))


write.table(dfsubject, file = "data_by_subject.csv", sep =",", row.names = FALSE)


# 
# ggbarplot(dfleft, x = "condition", y = "activity",
#                 ylim = c(-5, 12),
#                 add = c("mean_se", "jitter"),
#                 color = "agegroup.x",
#                 fill = "agegroup.x", alpha = 0.1,
#                 palette = c("#339933", "#FF9933"),
#                 position = position_dodge(0.8))
#    

#---- plot FD for 1-2 subjects ----

df40 <- df[df$subject_number=="PL00040",]





ggplot(data = df40, aes(x=scan, y=FD)) +
         geom_line()



#---- plot FD for FIRMM vs no FIRMM ----

ggbarplot(dfsubject, x = "FIRMM", y = "meanFD",
          add = c("mean_se", "jitter"),
          position = position_dodge(0.8))


#---- plot FD over time ----


# # left
# 
# dfall <- dfal[dfal$ear=="L",]
# 
# gdl <- dfall %>%
#   group_by(agegroup, frequency) %>%
#   summarize(threshold = mean(threshold))
# 
# ggplot(data = dfall, aes(x = frequency, y = threshold, group = subj, color = agegroup)) + 
#   geom_line(alpha = 0.5, size = 0.3) +
#   geom_line(data = gdl, aes(x = frequency, y = threshold, group = agegroup), size = 2) +
#   theme_classic() + 
#   labs(title = "Left", x = "Frequency (Hz)", y = "Threshold (dB HL)") +
#   scale_x_continuous(trans="log2") + scale_x_continuous(breaks=c(500, 2000, 4000, 8000)) +
#   scale_y_reverse() + 
#   color_palette(palette = c("#FF9933", "#339933")) +
#   ylim(85, -10) +
#   theme(legend.position = "none")  


dfmean <- df %>%
  group_by(FIRMM, scan) %>%
  summarize(meanFD = mean(FD))


# hack for now to get rid of weird big number of scans
dfmean <- dfmean[dfmean$scan<800,]


ggplot(data = dfmean, aes(x = scan, y = meanFD, group = FIRMM, color = FIRMM)) +
  geom_line()


# todo:

# - remove gridlines, add indicators for each run, plot single subjects for supplemental





#---- model ----


# m1 <- lmer(FD ~ FIRMM * scan +
#              (1 | scan) +
#              (1 | run),
#            data = df, )
# 
# summary(m1)

# add age group






