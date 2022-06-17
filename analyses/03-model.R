
# 03-model.R
# See whether motion (framewise displacement; FD) is affected by feedback
# (FIRMM: 0 or 1). Account for temporal autocorrelation in the timeseries data.


library(nlme)

df <- read.csv("data_filtered.csv")


#---- model ----

# NB need to specify autocorrelation within each run (not subject) since those
# are the time series data

# hack for now since we are still sorting out demographic info
df <- df [!is.na(df$age_group),]

df$run <- as.factor(df$run)


# model 1: no temporal autocorrelation
m1 <- lme(FD ~ 1 + run + age_group * FIRMM,
          random = ~1 | scan,
          data = df,
          method = "ML",
          correlation = NULL)


# model 2: temporal autocorrelation. This works but it's by scan. I think we
# want to look at this within subject. (Ideally, within each run of each
# subject, because those are the time series...)
m2 <- lme(FD ~ 1 + run + age_group * FIRMM,
          random = ~1 | scan,
          data = df,
          method = "ML",
          correlation = corAR1(.99, ~ 1 | scan, FALSE))


# Things that did not work:
# - having a corAR1 of 1 (has to be under 1, but not sure what it should be
# - having a grouping argument to corAR1 that did not match the random effects


