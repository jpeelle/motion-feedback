
# 03-model.R
# See whether motion (framewise displacement; FD) is affected by feedback
# (feedback: 0 or 1). Account for temporal autocorrelation in the timeseries data.


library(nlme)

df <- read.csv("data_filtered.csv")


#---- model FD ----

df$run <- as.factor(df$run)


# model 1: no temporal autocorrelation
m1 <- lme(FD ~ 1 + frame + age_group * feedback,
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

m2 <- lme(FD ~ 1 + frame + age_group * feedback,
                random = ~ frame | subject_number,
                data = df,
                method = "ML",
                correlation = corAR1(value = autocorrelation_estimate, form = ~ frame | subject_number, fixed = FALSE))

# > summary(m2)
# Linear mixed-effects model fit by maximum likelihood
# Data: df 
# AIC       BIC   logLik
# -19641.14 -19550.81 9830.569
# 
# Random effects:
#   Formula: ~frame | subject_number
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev       Corr  
# (Intercept) 0.0842839781 (Intr)
# frame       0.0001368689 -0.277
# Residual    0.2240814684       
# 
# Correlation Structure: AR(1)
# Formula: ~frame | subject_number 
# Parameter estimate(s):
#   Phi 
# 0.3973766 
# Fixed effects:  FD ~ 1 + frame + age_group * feedback 
# Value  Std.Error    DF   t-value p-value
# (Intercept)              0.4408685 0.03710688 61779 11.881047  0.0000
# frame                    0.0000431 0.00001661 61779  2.595558  0.0094
# age_groupyoung          -0.1590555 0.04204294    74 -3.783168  0.0003
# feedback                -0.1115340 0.04048551    74 -2.754912  0.0074
# age_groupyoung:feedback  0.0433081 0.04755107    74  0.910769  0.3654
# Correlation: 
#   (Intr) frame  ag_grp fedbck
# frame                   -0.090                     
# age_groupyoung          -0.876  0.000              
# feedback                -0.909  0.000  0.802       
# age_groupyoung:feedback  0.774  0.000 -0.884 -0.851
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -3.3574280 -0.5082019 -0.1604105  0.3018268 33.1946968 
# 
# Number of Observations: 61858
# Number of Groups: 78 


# Do an additional model by run (but ignoring autocorrelations because it's a bit too complex)

m3 <- lme(FD ~ 1 + run + age_group * feedback,
          random = ~ run | subject_number,
          data = df,
          method = "ML",
          correlation = NULL)




#---- model DVARS ----

df$run <- as.factor(df$run)


# model 4: no temporal autocorrelation
m4 <- lme(DVARS ~ 1 + frame + age_group * feedback,
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

m5 <- lme(DVARS ~ 1 + frame + age_group * feedback,
          random = ~ frame | subject_number,
          data = df,
          method = "ML",
          correlation = corAR1(value = autocorrelation_estimate, form = ~ frame | subject_number, fixed = FALSE))


# > summary(m5)
# Linear mixed-effects model fit by maximum likelihood
# Data: df 
# AIC      BIC    logLik
# 658179 658269.3 -329079.5
# 
# Random effects:
#   Formula: ~frame | subject_number
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev      Corr  
# (Intercept) 32.17666679 (Intr)
# frame        0.04100516 -0.456
# Residual    55.38226626       
# 
# Correlation Structure: AR(1)
# Formula: ~frame | subject_number 
# Parameter estimate(s):
#   Phi 
# 0.4578282 
# Fixed effects:  DVARS ~ 1 + frame + age_group * feedback 
# Value Std.Error    DF   t-value p-value
# (Intercept)             244.41916 13.012741 61779 18.783064  0.0000
# frame                     0.00611  0.004908 61779  1.244024  0.2135
# age_groupyoung          -31.76389 14.664594    74 -2.166026  0.0335
# feedback                -24.40944 14.121376    74 -1.728546  0.0881
# age_groupyoung:feedback   2.21901 16.585846    74  0.133790  0.8939
# Correlation: 
#   (Intr) frame  ag_grp fedbck
# frame                   -0.136                     
# age_groupyoung          -0.871  0.000              
# feedback                -0.904  0.000  0.802       
# age_groupyoung:feedback  0.770  0.000 -0.884 -0.851
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -3.6565131 -0.5347612 -0.1496879  0.3213161 19.1732902 
# 
# Number of Observations: 61858
# Number of Groups: 78 




# Do an additional model by run (but ignoring autocorrelations because it's a bit too complex)

m6 <- lme(DVARS ~ 1 + run + age_group * feedback,
          random = ~ run | subject_number,
          data = df,
          method = "ML",
          correlation = NULL)
