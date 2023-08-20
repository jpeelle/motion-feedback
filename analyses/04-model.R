
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


#---- average reduction in FD ----

# (not modeling, just how much did FD go down?)

dfsubject <- df %>% group_by(subject_number) %>%
  summarize(meanFD = mean(FD),
            medianFD = median(FD))

dfp <- read.csv("../data/participants.csv")
dfsubject <- left_join(dfp, dfsubject, 
                       by = "subject_number",
                       keep = FALSE)

dfsubject %>% group_by(feedback) %>% summarize(meanFD = mean(meanFD))

# > dfsubject %>% group_by(feedback) %>% summarize(meanFD = mean(meanFD))
# # A tibble: 2 × 2
# feedback meanFD
# <int>  <dbl>
#   1        0  0.347
# 2        1  0.282


#---- model FD ----

df$run <- as.factor(df$run)


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

# > summary(m2)
# Linear mixed-effects model fit by maximum likelihood
# Data: df 
# AIC       BIC   logLik
# -95733.32 -95642.99 47876.66
# 
# Random effects:
#   Formula: ~frame | subject_number
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev       Corr  
# (Intercept) 0.0487452009 (Intr)
# frame       0.0000726026 -0.189
# Residual    0.1208251030       
# 
# Correlation Structure: AR(1)
# Formula: ~frame | subject_number 
# Parameter estimate(s):
#   Phi 
# 0.3920536 
# Fixed effects:  FDgaussianized ~ 1 + frame + age_group * feedback 
# Value   Std.Error    DF   t-value p-value
# (Intercept)              0.3572638 0.021880945 61778 16.327621  0.0000
# frame                    0.0000273 0.000008824 61778  3.088461  0.0020
# age_groupyoung          -0.1033003 0.024837814    74 -4.158994  0.0001
# feedback                -0.0534164 0.023917879    74 -2.233326  0.0286
# age_groupyoung:feedback  0.0184424 0.028091970    74  0.656502  0.5135
# Correlation: 
#   (Intr) frame  ag_grp fedbck
# frame                   -0.066                     
# age_groupyoung          -0.877  0.000              
# feedback                -0.911  0.000  0.802       
# age_groupyoung:feedback  0.776  0.000 -0.884 -0.851
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -3.2333315 -0.7142755 -0.1814473  0.6278241  5.2229397 
# 
# Number of Observations: 61857
# Number of Groups: 78 




# Do an additional model by run (but ignoring autocorrelations because it's a bit too complex)

m3 <- lme(FDgaussianized ~ 1 + run + age_group * feedback,
          random = ~ run | subject_number,
          data = df,
          method = "ML",
          correlation = NULL)

# > summary(m3)
# Linear mixed-effects model fit by maximum likelihood
# Data: df 
# AIC       BIC   logLik
# -86572.7 -86292.69 43317.35
# 
# Random effects:
#   Formula: ~run | subject_number
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev     Corr                              
# (Intercept) 0.04696067 (Intr) run2   run3   run4   run5  
# run2        0.03945384 -0.125                            
# run3        0.04399800 -0.208  0.728                     
# run4        0.04576182 -0.039  0.622  0.697              
# run5        0.05388313 -0.209  0.691  0.828  0.771       
# run6        0.05263918 -0.228  0.643  0.750  0.670  0.859
# Residual    0.11902123                                   
# 
# Fixed effects:  FDgaussianized ~ 1 + run + age_group * feedback 
# Value   Std.Error    DF   t-value p-value
# (Intercept)              0.3555292 0.020471649 61774 17.366906  0.0000
# run2                     0.0111638 0.004760753 61774  2.344960  0.0190
# run3                     0.0189044 0.005247816 61774  3.602345  0.0003
# run4                     0.0229300 0.005437600 61774  4.216932  0.0000
# run5                     0.0206506 0.006320480 61774  3.267252  0.0011
# run6                     0.0179240 0.006190537 61774  2.895389  0.0038
# age_groupyoung          -0.1040961 0.023205030    74 -4.485930  0.0000
# feedback                -0.0605921 0.022347092    74 -2.711407  0.0083
# age_groupyoung:feedback  0.0261700 0.026245629    74  0.997117  0.3220
# Correlation: 
#   (Intr) run2   run3   run4   run5   run6   ag_grp fedbck
# run2                    -0.044                                                 
# run3                    -0.064  0.702                                          
# run4                    -0.022  0.608  0.678                                   
# run5                    -0.063  0.671  0.800  0.748                            
# run6                    -0.068  0.627  0.727  0.655  0.833                     
# age_groupyoung          -0.876  0.000  0.000  0.000  0.000  0.000              
# feedback                -0.910  0.000  0.000  0.000  0.000  0.000  0.802       
# age_groupyoung:feedback  0.774  0.000  0.000  0.000  0.000  0.000 -0.884 -0.851
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -3.2467639 -0.7048397 -0.1714973  0.6277209  5.4701817 
# 
# Number of Observations: 61857
# Number of Groups: 78 





#---- model DVARS ----


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


# > summary(m5)
# Linear mixed-effects model fit by maximum likelihood
# Data: df 
# AIC      BIC    logLik
# 594521.5 594611.8 -297250.7
# 
# Random effects:
#   Formula: ~frame | subject_number
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev      Corr  
# (Intercept) 22.38925881 (Intr)
# frame        0.02745623 -0.395
# Residual    33.18692603       
# 
# Correlation Structure: AR(1)
# Formula: ~frame | subject_number 
# Parameter estimate(s):
#   Phi 
# 0.4624653 
# Fixed effects:  DVARSgaussianized ~ 1 + frame + age_group * feedback 
# Value Std.Error    DF   t-value p-value
# (Intercept)             228.48139  9.306936 61778 24.549584  0.0000
# frame                     0.00521  0.003254 61778  1.602724  0.1090
# age_groupyoung          -24.05534 10.517516    74 -2.287169  0.0250
# feedback                -15.40664 10.127865    74 -1.521213  0.1325
# age_groupyoung:feedback   2.17760 11.895410    74  0.183062  0.8552
# Correlation: 
#   (Intr) frame  ag_grp fedbck
# frame                   -0.115                     
# age_groupyoung          -0.873  0.000              
# feedback                -0.907  0.000  0.802       
# age_groupyoung:feedback  0.772  0.000 -0.884 -0.851
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -3.8411906 -0.6749803 -0.1497273  0.5597513  5.4435466 
# 
# Number of Observations: 61857
# Number of Groups: 78 



# Do an additional model by run (but ignoring autocorrelations because it's a bit too complex)

m6 <- lme(DVARSgaussianized ~ 1 + run + age_group * feedback,
          random = ~ run | subject_number,
          data = df,
          method = "ML",
          correlation = NULL)


# > summary(m6)
# Linear mixed-effects model fit by maximum likelihood
# Data: df 
# AIC      BIC    logLik
# 607027.8 607307.8 -303482.9
# 
# Random effects:
#   Formula: ~run | subject_number
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev   Corr                              
# (Intercept) 21.90133 (Intr) run2   run3   run4   run5  
# run2        12.05616 -0.211                            
# run3        14.54155 -0.351  0.692                     
# run4        15.92971 -0.227  0.599  0.764              
# run5        21.52526 -0.370  0.591  0.758  0.711       
# run6        17.93931 -0.376  0.495  0.684  0.694  0.800
# Residual    32.32299                                   
# 
# Fixed effects:  DVARSgaussianized ~ 1 + run + age_group * feedback 
# Value Std.Error    DF   t-value p-value
# (Intercept)             227.31594  8.962557 61774 25.362843  0.0000
# run2                      2.05963  1.436412 61774  1.433874  0.1516
# run3                      3.98708  1.706406 61774  2.336538  0.0195
# run4                      5.40560  1.858504 61774  2.908578  0.0036
# run5                      4.34756  2.478246 61774  1.754289  0.0794
# run6                      2.73375  2.081469 61774  1.313377  0.1891
# age_groupyoung          -23.11906 10.120695    74 -2.284335  0.0252
# feedback                -16.27255  9.745900    74 -1.669682  0.0992
# age_groupyoung:feedback   2.64483 11.446654    74  0.231057  0.8179
# Correlation: 
#   (Intr) run2   run3   run4   run5   run6   ag_grp fedbck
# run2                    -0.063                                                 
# run3                    -0.100  0.675                                          
# run4                    -0.067  0.590  0.747                                   
# run5                    -0.105  0.580  0.743  0.700                            
# run6                    -0.107  0.492  0.672  0.683  0.787                     
# age_groupyoung          -0.873  0.000  0.000  0.000  0.000  0.000              
# feedback                -0.906  0.000  0.000  0.000  0.000  0.000  0.802       
# age_groupyoung:feedback  0.772  0.000  0.000  0.000  0.000  0.000 -0.884 -0.851
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -3.5909175 -0.6629991 -0.1422863  0.5601828  5.4620115 
# 
# Number of Observations: 61857
# Number of Groups: 78 
# 


#---- tSNR ----

dftSNR <- read.csv("tSNR_data_filtered.csv")

m8 <- lme(mean_tSNR ~ 1 + run + age_group * feedback,
          random = ~ run | subject_number,
          data = dftSNR,
          method = "ML",
          correlation = NULL)

# > summary(m8)
# Linear mixed-effects model fit by maximum likelihood
# Data: dftSNR 
# AIC      BIC    logLik
# 2562.807 2600.143 -1272.403
# 
# Random effects:
#   Formula: ~run | subject_number
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev    Corr  
# (Intercept) 5.1821216 (Intr)
# run         0.7560091 -0.257
# Residual    2.6148811       
# 
# Fixed effects:  mean_tSNR ~ 1 + run + age_group * feedback 
# Value Std.Error  DF   t-value p-value
# (Intercept)             35.62189 2.3494343 389 15.161899  0.0000
# run                     -0.01283 0.1116692 389 -0.114888  0.9086
# age_groupyoung           5.61719 2.6542796  74  2.116276  0.0377
# feedback                 4.51521 2.5559386  74  1.766556  0.0814
# age_groupyoung:feedback -0.11883 3.0020112  74 -0.039584  0.9685
# Correlation: 
#   (Intr) run    ag_grp fedbck
# run                     -0.117                     
# age_groupyoung          -0.873  0.000              
# feedback                -0.907  0.000  0.802       
# age_groupyoung:feedback  0.772  0.000 -0.884 -0.851
# 
# Standardized Within-Group Residuals:
#   Min          Q1         Med          Q3         Max 
# -5.11176386 -0.37735370  0.00962262  0.46065257  2.94656859 
# 
# Number of Observations: 468
# Number of Groups: 78 


#---- accuracy ----

m9 <- lm(accuracy ~ 1 + age_group * feedback,
          data = dfsubject)

summary(m9)

# 
# > summary(m9)
# 
# Call:
#   lm(formula = accuracy ~ 1 + age_group * feedback, data = dfsubject)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.40721 -0.02096  0.00909  0.04570  0.12195 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.861746   0.022468  38.354   <2e-16 ***
#   age_groupolder           -0.082580   0.047130  -1.752   0.0839 .  
# feedback1                 0.005049   0.027958   0.181   0.8572    
# age_groupolder:feedback1 -0.072837   0.053305  -1.366   0.1759    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.09264 on 74 degrees of freedom
# Multiple R-squared:  0.3825,	Adjusted R-squared:  0.3575 
# F-statistic: 15.28 on 3 and 74 DF,  p-value: 7.84e-08
