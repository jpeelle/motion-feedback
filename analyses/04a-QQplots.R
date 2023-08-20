
# 04-model.R
# See whether motion (framewise displacement; FD) is affected by feedback
# (feedback: 0 or 1). Account for temporal autocorrelation in the timeseries data.

library(tidyverse)
library(nlme)
library(LambertW)


df <- read.csv("data_filtered.csv")


#---- transform FD and DVARS ----
# Because of the skew in FD and DVARS distributions (which is apparent looking
# at Q-Q plots of model resituals), we transformed both using the LambertW
# package before modeling.

df <- df %>%
  mutate(FDgaussianized = Gaussianize(data = df$FD,
                                      type = "h",
                                      method = "IGMM"))

df <- df %>%
  mutate(DVARSgaussianized = Gaussianize(data = df$DVARS,
                                      type = "h",
                                      method = "IGMM"))


#---- model FD: no transform ----

df$run <- as.factor(df$run)


# model 1: no temporal autocorrelation
m1raw <- lme(FD ~ 1 + frame + age_group * feedback,
          random = ~ frame | subject_number,
          data = df,
          method = "ML",
          correlation = NULL)

# Prepare for including autocorrelation - estimate autocorrelation

# Make simple model with just the time variable (frame)
simple_model <- lm(FD ~ frame, data = df)

# View the lagged correlations and save as a df
acf_df <- acf(residuals(simple_model))

# Get the lag1 correlation
autocorrelation_estimate <- acf_df$acf[2]


# model 2: temporal autocorrelation. 

m2raw <- lme(FD ~ 1 + frame + age_group * feedback,
                random = ~ frame | subject_number,
                data = df,
                method = "ML",
                correlation = corAR1(value = autocorrelation_estimate, form = ~ frame | subject_number, fixed = FALSE))

r <- resid(m2raw)
qqnorm(r)
qqline(r)


# Do an additional model by run (but ignoring autocorrelations because it's a bit too complex)

m3raw <- lme(FD ~ 1 + run + age_group * feedback,
          random = ~ run | subject_number,
          data = df,
          method = "REML",   # failed to converge with ML
          correlation = NULL)

r <- resid(m3raw)
qqnorm(r)
qqline(r)



#---- model FD: transform ----


# model 1: no temporal autocorrelation
m1 <- lme(FDgaussianized ~ 1 + frame + age_group * feedback,
          random = ~ frame | subject_number,
          data = df,
          method = "ML",
          correlation = NULL)

# Prepare for including autocorrelation - estimate autocorrelation

# Make simple model with just the time variable (frame)
simple_model <- lm(FDgaussianized ~ frame, data = df)

# View the lagged correlations and save as a df
acf_df <- acf(residuals(simple_model))

# Get the lag1 correlation
autocorrelation_estimate <- acf_df$acf[2]


# model 2: temporal autocorrelation. 

m2 <- lme(FDgaussianized ~ 1 + frame + age_group * feedback,
          random = ~ frame | subject_number,
          data = df,
          method = "ML",
          correlation = corAR1(value = autocorrelation_estimate, form = ~ frame | subject_number, fixed = FALSE))

r <- resid(m2)
qqnorm(r)
qqline(r)



# Do an additional model by run (but ignoring autocorrelations because it's a bit too complex)

m3 <- lme(FDgaussianized ~ 1 + run + age_group * feedback,
          random = ~ run | subject_number,
          data = df,
          method = "ML",
          correlation = NULL)

r <- resid(m3)
qqnorm(r)
qqline(r)


#---- model DVARS: no transform ----

# model 4: no temporal autocorrelation
m4raw <- lme(DVARS ~ 1 + frame + age_group * feedback,
          random = ~ frame | subject_number,
          data = df,
          method = "ML",
          correlation = NULL)



# Prepare for including autocorrelation - estimate autocorrelation

# Make simple model with just the time variable (frame)
simple_model <- lm(DVARS ~ frame, data = df)

# View the lagged correlations and save as a df
acf_df <- acf(residuals(simple_model))

# Get the lag1 correlation
autocorrelation_estimate <- acf_df$acf[2]


# model 5: temporal autocorrelation. 

m5raw <- lme(DVARS ~ 1 + frame + age_group * feedback,
          random = ~ frame | subject_number,
          data = df,
          method = "ML",
          correlation = corAR1(value = autocorrelation_estimate, form = ~ frame | subject_number, fixed = FALSE))


r <- resid(m5raw)
qqnorm(r)
qqline(r)


# Do an additional model by run (but ignoring autocorrelations because it's a bit too complex)

m6raw <- lme(DVARS ~ 1 + run + age_group * feedback,
          random = ~ run | subject_number,
          data = df,
          method = "ML",
          correlation = NULL)

r <- resid(m6raw)
qqnorm(r)
qqline(r)





#---- model DVARS: transform ----

# model 4: no temporal autocorrelation
m4 <- lme(DVARSgaussianized ~ 1 + frame + age_group * feedback,
             random = ~ frame | subject_number,
             data = df,
             method = "ML",
             correlation = NULL)



# Prepare for including autocorrelation - estimate autocorrelation

# Make simple model with just the time variable (frame)
simple_model <- lm(DVARSgaussianized ~ frame, data = df)

# View the lagged correlations and save as a df
acf_df <- acf(residuals(simple_model))

# Get the lag1 correlation
autocorrelation_estimate <- acf_df$acf[2]


# model 5: temporal autocorrelation. 

m5 <- lme(DVARSgaussianized ~ 1 + frame + age_group * feedback,
             random = ~ frame | subject_number,
             data = df,
             method = "ML",
             correlation = corAR1(value = autocorrelation_estimate, form = ~ frame | subject_number, fixed = FALSE))


r <- resid(m5)
qqnorm(r)
qqline(r)


# Do an additional model by run (but ignoring autocorrelations because it's a bit too complex)

m6 <- lme(DVARSgaussianized ~ 1 + run + age_group * feedback,
             random = ~ run | subject_number,
             data = df,
             method = "ML",
             correlation = NULL)

r <- resid(m6)
qqnorm(r)
qqline(r)
