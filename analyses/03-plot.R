
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggridges)

feedbackColor = "green"
nofeedbackColor = "gray40"


#---- read data ----

df <- read.csv("data_filtered.csv")
dfp <- read.csv("../data/participants.csv")


dfsubject <- group_by(df, subject_number) %>%
  summarize(meanFD=mean(FD),
            medianFD=median(FD),
            SDFD=sd(FD))

dfp <- dfp[!is.na(dfp$subject_number),]

dfsubject <- left_join(dfp, dfsubject, 
                       by = "subject_number",
                       keep = FALSE)

# get rid of NAs (people in dfp but not df)
dfsubject <- dfsubject[!is.na(dfsubject$meanFD),]

dfsubject$FIRMM <- as.factor(dfsubject$FIRMM)



#---- how many scans per subject? this varies... ----

#TODO





#---- plot FD over time for 2 subjects ----

df16 <- df[df$subject_number=="sub-16",]  # sub-16 is our first nofeedback young adult
df22 <- df[df$subject_number=="sub-22",] # sub-22 is our first feedback young adult

p1 <- ggplot() +
  geom_line(data = df16, aes(x=scan, y=FD), color = nofeedbackColor) +
  geom_line(data = df22, aes(x=scan, y=FD), color = feedbackColor) +
  theme_classic() +
  xlab("Frame") +
  xlim(0,800)




#---- plot mean FD over time for all subjects (grouped by FIRMM) ----

dfmean <- df %>%
  group_by(FIRMM, scan) %>%
  summarize(meanFD = mean(FD))

dfmean$FIRMM <- as.factor(dfmean$FIRMM)

# hack for now to get rid of weird big number of scans
dfmean <- dfmean[dfmean$scan<800,]


p2 <- ggplot(data = dfmean, aes(x = scan, y = meanFD, group = FIRMM, color = FIRMM)) +
  geom_line() +
  scale_colour_discrete(type = c(nofeedbackColor, feedbackColor)) + 
  xlim(0, 800) + 
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Frame") +
  ylab("Mean FD")


#---- plot mean FD for FIRMM vs no FIRMM, summarized (one point per subject) ----

p3 <- ggbarplot(dfsubject, x = "age_group", y = "meanFD",
          add = c("mean_se", "jitter"),
          error.plot = "upper_errorbar",
          color = "FIRMM",
          fill = "FIRMM", alpha = 0.1,
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
  group_by(FIRMM, run, subject_number) %>%
  summarize(meanFD = mean(FD))

dfrun$FIRMM <- as.factor(dfrun$FIRMM)


ggbarplot(dfrun, x = "run", y = "meanFD",
          add = c("mean_se", "jitter"),
          error.plot = "upper_errorbar",
          color = "FIRMM",
          fill = "FIRMM", alpha = 0.1,
          palette = c(nofeedbackColor, feedbackColor),
          position = position_dodge(0.8),
          xlab = "Run",
          ylab = "Mean FD")

ggsave("figures/FD_by_run.png", width = 6, height = 4, units = "in", dpi = 300)
ggsave("figures/FD_by_run.pdf", width = 6, height = 4, units = "in", dpi = 300)



#---- ridge plots of individual subjects ----

# TODO: order by FIRMM/no FIRMM...still in progress
ggplot(dff, aes(x = FD, y = subject_number, fill = FIRMM, height = stat(density))) +
  geom_density_ridges2(stat = "density") +
  xlim(c(0,1)) + 
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)
theme_classic()

