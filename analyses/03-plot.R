
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggridges)

feedbackColor = "green"
nofeedbackColor = "gray40"


#---- read data ----

df <- read.csv("data_filtered.csv")
dfp <- read.csv("../data/participants.csv")

df$feedback <- as.factor(df$feedback)



dfsubject <- group_by(df, subject_number) %>%
  summarize(meanFD=mean(FD),
            medianFD=median(FD),
            SDFD=sd(FD),
            meanDVARS=mean(DVARS),
            medianDVARS=median(DVARS),
            SDDVARS=sd(DVARS))

dfp <- dfp[!is.na(dfp$subject_number),]

dfsubject <- left_join(dfp, dfsubject, 
                       by = "subject_number",
                       keep = FALSE)

# get rid of NAs (people in dfp but not df) - not needed if data complete
# dfsubject <- dfsubject[!is.na(dfsubject$meanFD),]

dfsubject$feedback <- as.factor(dfsubject$feedback)

dfsubject$age_group <- factor(dfsubject$age_group, levels = c("young", "older"))




#---- how many scans per subject? this varies... ----

#TODO





#---- plot FD over time for 2 subjects ----

df16 <- df[df$subject_number=="sub-16",]  # sub-16 is our first nofeedback young adult
df22 <- df[df$subject_number=="sub-22",] # sub-22 is our first feedback young adult

p1 <- ggplot() +
  geom_line(data = df16, aes(x=frame, y=FD), color = nofeedbackColor) +
  geom_line(data = df22, aes(x=frame, y=FD), color = feedbackColor) +
  theme_classic() +
  xlab("Frame") +
  #xlim(0,800) +
  NULL




#---- plot mean FD over time for all subjects (grouped by FIRMM) ----

dfmean <- df %>%
  group_by(feedback, frame) %>%
  summarize(meanFD = mean(FD), meanDVARS = mean(DVARS))

dfmean$feedback <- as.factor(dfmean$feedback)



p2 <- ggplot(data = dfmean, aes(x = frame, y = meanFD, group = feedback, color = feedback)) +
  geom_line() +
  scale_colour_discrete(type = c(nofeedbackColor, feedbackColor)) + 
  xlim(0, 800) + 
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Frame") +
  ylab("Mean FD")



#---- plot distribution of all FD values for FIRMM vs. no-FIRMM ----

ggplot(df, aes(x = FD, color = feedback)) +
  geom_density() + 
  theme_classic()


#---- plot mean FD for FIRMM vs no FIRMM, summarized (one point per subject) ----

p3 <- ggbarplot(dfsubject, x = "age_group", y = "meanFD",
          add = c("mean_se", "jitter"),
          error.plot = "upper_errorbar",
          color = "feedback",
          fill = "feedback", alpha = 0.1,
          palette = c(nofeedbackColor, feedbackColor),
          position = position_dodge(0.8),
          xlab = "Age Group",
          ylab = "Mean FD")
  

#---- combine these three plots (p1, p2, p3) into a single plot ----

p12 <- ggarrange(p1, p2,
          ncol = 1)

ggarrange(p12, p3,
          ncol = 2)          

ggsave("figures/FDcomparison.png", width = 6, height = 4, units = "in", dpi = 300)
ggsave("figures/FDcomparison.pdf", width = 6, height = 4, units = "in", dpi = 300)


#---- plot by run ----

dfrun <- df %>%
  group_by(feedback, run, subject_number) %>%
  summarize(meanFD = mean(FD))

dfrun$feedback <- as.factor(dfrun$feedback)


ggbarplot(dfrun, x = "run", y = "meanFD",
          add = c("mean_se", "jitter"),
          error.plot = "upper_errorbar",
          color = "feedback",
          fill = "feedback", alpha = 0.1,
          palette = c(nofeedbackColor, feedbackColor),
          position = position_dodge(0.8),
          xlab = "Run",
          ylab = "Mean FD")

ggsave("figures/FD_by_run.png", width = 6, height = 4, units = "in", dpi = 300)
ggsave("figures/FD_by_run.pdf", width = 6, height = 4, units = "in", dpi = 300)



#---- ridge plots of individual subjects ----

# TODO: order by FIRMM/no FIRMM...still in progress
# dff <- df %>% group_by(FIRMM)
# dff %>% arrange(subject_number, .by_group = TRUE)
# ggplot(dff, aes(x = FD, y = subject_number, fill = FIRMM, height = stat(density))) +
#   geom_density_ridges2(stat = "density") +
#   xlim(c(0,1)) + 
#   stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
#   theme_classic()





# Now repeat the same plots as for FD, but with DVARS as the outcome



#---- plot DVARS over time for 2 subjects ----


p5 <- ggplot() +
  geom_line(data = df16, aes(x=frame, y=DVARS), color = nofeedbackColor) +
  geom_line(data = df22, aes(x=frame, y=DVARS), color = feedbackColor) +
  theme_classic() +
  xlab("Frame")



#---- plot mean DVARS over time for all subjects (grouped by feedback) ----


p6 <- ggplot(data = dfmean, aes(x = frame, y = meanDVARS, group = feedback, color = feedback)) +
  geom_line() +
  scale_colour_discrete(type = c(nofeedbackColor, feedbackColor)) + 
  #xlim(0, 800) + 
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Frame") +
  ylab("Mean DVARS")



#---- plot distribution of all DVARS values for FIRMM vs. no-FIRMM ----

ggplot(df, aes(x = DVARS, color = feedback)) +
  geom_density() + 
  theme_classic()


#---- plot mean DVARS for FIRMM vs no FIRMM, summarized (one point per subject) ----

p7 <- ggbarplot(dfsubject, x = "age_group", y = "meanDVARS",
                add = c("mean_se", "jitter"),
                error.plot = "upper_errorbar",
                color = "feedback",
                fill = "feedback", alpha = 0.1,
                palette = c(nofeedbackColor, feedbackColor),
                position = position_dodge(0.8),
                xlab = "Age Group",
                ylab = "Mean DVARS")


#---- combine these three plots (p1, p2, p3) into a single plot ----

p56 <- ggarrange(p5, p6,
                 ncol = 1)

ggarrange(p56, p7,
          ncol = 2)          

ggsave("figures/DVARScomparison.png", width = 6, height = 4, units = "in", dpi = 300)
ggsave("figures/DVARScomparison.pdf", width = 6, height = 4, units = "in", dpi = 300)


#---- plot DVARS by run ----

dfrun <- df %>%
  group_by(feedback, run, subject_number) %>%
  summarize(meanDVARS = mean(DVARS))

dfrun$feedback <- as.factor(dfrun$feedback)


ggbarplot(dfrun, x = "run", y = "meanDVARS",
          add = c("mean_se", "jitter"),
          error.plot = "upper_errorbar",
          color = "feedback",
          fill = "feedback", alpha = 0.1,
          palette = c(nofeedbackColor, feedbackColor),
          position = position_dodge(0.8),
          xlab = "Run",
          ylab = "Mean DVARS")

ggsave("figures/DVARS_by_run.png", width = 6, height = 4, units = "in", dpi = 300)
ggsave("figures/DVARS_by_run.pdf", width = 6, height = 4, units = "in", dpi = 300)


#---- tSNR ----

#df <- read.csv("data_filtered.csv")

dftSNR <- read.csv("tSNR_data_filtered.csv")


dftSNRsubject <- group_by(dftSNR, subject_number) %>%
  summarize(meantSNR=mean(mean_tSNR),
  mediantSNR=mean(median_tSNR))

dftSNRsubject$feedback <- as.factor(dftSNRsubject$feedback)

dftSNRsubject <- left_join(dfp, dftSNRsubject, 
          by = "subject_number",
          keep = FALSE)

ggbarplot(dftSNRsubject, x = "age_group", y = "meantSNR",
          add = c("mean_se", "jitter"),
          error.plot = "upper_errorbar",
          color = "feedback",
          fill = "feedback", alpha = 0.1,
          palette = c(nofeedbackColor, feedbackColor),
          position = position_dodge(0.8),
          xlab = "Feedback",
          ylab = "Whole-brain mean tSNR")

ggsave("figures/mean_tSNR.png", width = 6, height = 4, units = "in", dpi = 300)
ggsave("figures/mean_tSNR.pdf", width = 6, height = 4, units = "in", dpi = 300)

