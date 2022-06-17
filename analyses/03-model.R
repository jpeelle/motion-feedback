
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




# > summary(m1)
# Linear mixed-effects model fit by maximum likelihood
# Data: df 
# AIC      BIC    logLik
# 1652.154 1723.983 -818.0771
# 
# Random effects:
#   Formula: ~1 | scan
# (Intercept) Residual
# StdDev: 0.003857588 0.245981
# 
# Correlation Structure: AR(1)
# Formula: ~1 | scan 
# Parameter estimate(s):
#   Phi 
# 0.07276733 
# Fixed effects:  FD ~ 1 + run + age_group * FIRMM 
# Value   Std.Error    DF   t-value p-value
# (Intercept)           0.4387454 0.005505968 57042  79.68544       0
# run                   0.0065413 0.000644880 57042  10.14338       0
# age_groupyoung       -0.1468675 0.005502667 57042 -26.69023       0
# FIRMM                -0.1092800 0.005257614 57042 -20.78509       0
# age_groupyoung:FIRMM  0.0321257 0.005922003 57042   5.42480       0
# Correlation: 
#   (Intr) run    ag_grp FIRMM 
# run                  -0.406                     
# age_groupyoung       -0.835  0.000              
# FIRMM                -0.857 -0.001  0.859       
# age_groupyoung:FIRMM  0.761  0.001 -0.908 -0.888
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -1.7857985 -0.5702735 -0.2240873  0.2909514 31.1279496 
# 
# Number of Observations: 58604
# Number of Groups: 1558 


# > summary(m1)
# Linear mixed-effects model fit by maximum likelihood
# Data: df 
# AIC      BIC    logLik
# 1652.154 1723.983 -818.0771
# 
# Random effects:
#   Formula: ~1 | scan
# (Intercept) Residual
# StdDev: 0.003857588 0.245981
# 
# Correlation Structure: AR(1)
# Formula: ~1 | scan 
# Parameter estimate(s):
#   Phi 
# 0.07276733 
# Fixed effects:  FD ~ 1 + run + age_group * FIRMM 
# Value   Std.Error    DF   t-value p-value
# (Intercept)           0.4387454 0.005505968 57042  79.68544       0
# run                   0.0065413 0.000644880 57042  10.14338       0
# age_groupyoung       -0.1468675 0.005502667 57042 -26.69023       0
# FIRMM                -0.1092800 0.005257614 57042 -20.78509       0
# age_groupyoung:FIRMM  0.0321257 0.005922003 57042   5.42480       0
# Correlation: 
#   (Intr) run    ag_grp FIRMM 
# run                  -0.406                     
# age_groupyoung       -0.835  0.000              
# FIRMM                -0.857 -0.001  0.859       
# age_groupyoung:FIRMM  0.761  0.001 -0.908 -0.888
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -1.7857985 -0.5702735 -0.2240873  0.2909514 31.1279496 
# 
# Number of Observations: 58604
# Number of Groups: 1558 
