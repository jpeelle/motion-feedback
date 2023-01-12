
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
          ncol = 2,
          labels = "auto")          

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



#---- plot distribution of all DVARS values for FIRMM vs. no-FIRMM, and histograms ----

pdens <- ggplot(df, aes(x = FD, color = feedback)) +
  geom_density() + 
  xlim(0, 2) +
  scale_color_manual(values = c(nofeedbackColor, feedbackColor)) +
  theme_classic()


hbreaks <- c(0, .2, .4, .6, .8, 1, 1.2, 1.4, 1.6, 1.8, 2)

x <- df$FD[df$feedback==0]
x <- x[x<=2]

histnofeedback <- hist(x,
                        breaks = hbreaks)


x <- df$FD[df$feedback==1]
x <- x[x<=2]

histfeedback <- hist(x,
                      breaks = hbreaks)


# Histogram for feedback and no feedback
hf <- histfeedback$density / histnofeedback$density

hnf <- histnofeedback$density / histnofeedback$density

# Put into data frame - one mnore break than count, make sure all have the same length
fdbreak <- hbreaks[1:length(hbreaks)-1]
hdf <- data.frame(fdbreak, hf, hnf)

hdf <- pivot_longer(hdf, cols = c("hf"), names_to = "feedback", values_to = "density")
hdf$feedback <- as.factor(hdf$feedback)

pprop <- ggplot(data = hdf, aes(x = fdbreak,
                       y = density)) +
  geom_bar(stat = "identity", fill = feedbackColor) +
  xlab("FD") +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2)) +
  ylab("Proportion relative to no feedback") +
  scale_fill_manual(values=feedbackColor) +
  geom_hline(yintercept=1, linetype="dashed", color = nofeedbackColor) +
  theme_classic() 

pdensprop <- ggarrange(pdens, pprop, 
                       ncol = 2,
                       labels = "auto")


ggsave("figures/FDdensity.png", width = 6, height = 4, units = "in", dpi = 300)
ggsave("figures/FDdensity.pdf", width = 6, height = 4, units = "in", dpi = 300)





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
          ncol = 2,
          labels = "auto")          

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

dftSNR <- read.csv("tSNR_data_filtered.csv")


dftSNRsubject <- group_by(dftSNR, subject_number) %>%
  summarize(meantSNR=mean(mean_tSNR),
  mediantSNR=mean(median_tSNR))
  

dftSNRsubject <- left_join(dfp, dftSNRsubject, 
          by = "subject_number",
          keep = FALSE)

dftSNRsubject$feedback <- as.factor(dftSNRsubject$feedback)

dftSNRsubject$age_group <- factor(dftSNRsubject$age_group, levels = c("young", "older"))

ggbarplot(dftSNRsubject, x = "age_group", y = "meantSNR",
          add = c("mean_se", "jitter"),
          error.plot = "upper_errorbar",
          color = "feedback",
          fill = "feedback", alpha = 0.1,
          palette = c(nofeedbackColor, feedbackColor),
          position = position_dodge(0.8),
          xlab = "Age Group",
          ylab = "Whole-brain mean tSNR")

ggsave("figures/mean_tSNR.png", width = 3, height = 4.5, units = "in", dpi = 300)
ggsave("figures/mean_tSNR.pdf", width = 3, height = 4.5, units = "in", dpi = 300)


#---- accuracy ----

ggbarplot(dfsubject, x = "age_group", y = "accuracy",
          add = c("mean_se", "jitter"),
          error.plot = "upper_errorbar",
          color = "feedback",
          fill = "feedback", alpha = 0.1,
          palette = c(nofeedbackColor, feedbackColor),
          position = position_dodge(0.8),
          xlab = "Age Group",
          ylab = "Accuracy")


ggsave("figures/accuracy.png", width = 3, height = 4.5, units = "in", dpi = 300)
ggsave("figures/accuracy.pdf", width = 3, height = 4.5, units = "in", dpi = 300)

