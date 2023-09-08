
# 
# See whether motion (framewise displacement; FD) is affected by feedback
# (feedback: 0 or 1). Account for temporal autocorrelation in the timeseries data.

library(tidyverse)
library(nlme)
library(LambertW)

df <- read.csv("data_filtered.csv")





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

r <- resid(m2)
qqnorm(r)
qqline(r)

df <- df %>%
  mutate(FDlog = log(FD))


# Make simple model with just the time variable (frame)
simple_model <- lm(FDlog ~ frame, data = df)

# View the lagged correlations and save as a df
acf_df <- acf(residuals(simple_model))

# Get the lag1 correlation
autocorrelation_estimate <- acf_df$acf[2]


m2log <- lme(FDlog ~ 1 + frame + age_group * feedback,
          random = ~ frame | subject_number,
          data = df,
          method = "ML",
          correlation = corAR1(value = autocorrelation_estimate, form = ~ frame | subject_number, fixed = FALSE))



r2 <- resid(m2log)
qqnorm(r2)
qqline(r2)




# > summary(m2)
# Linear mixed-effects model fit by maximum likelihood
# Data: df 
# AIC       BIC   logLik
# -14874.85 -14784.52 7447.423
# 
# Random effects:
#   Formula: ~frame | subject_number
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev       Corr  
# (Intercept) 0.0846033390 (Intr)
# frame       0.0001416329 -0.243
# Residual    0.2317320258       
# 
# Correlation Structure: AR(1)
# Formula: ~frame | subject_number 
# Parameter estimate(s):
#   Phi 
# 0.3866899 
# Fixed effects:  FD ~ 1 + frame + age_group * feedback 
# Value  Std.Error    DF   t-value p-value
# (Intercept)              0.4504154 0.03766628 61778 11.958053  0.0000
# frame                    0.0000442 0.00001716 61778  2.572848  0.0101
# age_groupyoung          -0.1659770 0.04270876    74 -3.886252  0.0002
# feedback                -0.1193043 0.04112688    74 -2.900883  0.0049
# age_groupyoung:feedback  0.0507396 0.04830427    74  1.050417  0.2969
# Correlation: 
#   (Intr) frame  ag_grp fedbck
# frame                   -0.081                     
# age_groupyoung          -0.876  0.000              
# feedback                -0.910  0.000  0.802       
# age_groupyoung:feedback  0.775  0.000 -0.884 -0.851
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -3.5430896 -0.5015948 -0.1618787  0.2942134 29.0260803 
# 
# Number of Observations: 61857
# Number of Groups: 78 



df <- df %>%
  mutate(FDgaussianizedhh = Gaussianize(data = df$FD,
                                      type = "hh",
                                      method = "IGMM"))

df <- df %>%
  mutate(FDgaussianizedh = Gaussianize(data = df$FD,
                                        type = "h",
                                        method = "IGMM"))


# Make simple model with just the time variable (frame)
simple_model <- lm(FDgaussianizedhh ~ frame, data = df)

# View the lagged correlations and save as a df
acf_df <- acf(residuals(simple_model))

# Get the lag1 correlation
autocorrelation_estimate <- acf_df$acf[2]

m2gauss <- lme(FDgaussianizedhh ~ 1 + frame + age_group * feedback,
             random = ~ frame | subject_number,
             data = df,
             method = "ML",
             correlation = corAR1(value = autocorrelation_estimate, form = ~ frame | subject_number, fixed = FALSE))
         
         
r3 <- resid(m2gauss)
qqnorm(r3)
qqline(r3)
         
         
# Tukey transform: fails since vector is longer than 5000
# df <- df %>%
#   mutate(FDtukey = transformTukey(df$FD,
#                                   statistic = 2,
#                                   plotit = TRUE))



         
         
# Do an additional model by run (but ignoring autocorrelations because it's a bit too complex)

m3 <- lme(FD ~ 1 + run + age_group * feedback,
          random = ~ run | subject_number,
          data = df,
          method = "REML",
          correlation = NULL)


m3a <- lme(FD ~ 1 + run * age_group * feedback,
          random = ~ run | subject_number,
          data = df,
          method = "ML",
          correlation = NULL)

# m3 failed to converge with method = "ML"

# > summary(m3)
# Linear mixed-effects model fit by REML
# Data: df 
# AIC       BIC   logLik
# -6179.275 -5899.269 3120.637
# 
# Random effects:
#   Formula: ~run | subject_number
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev     Corr                              
# (Intercept) 0.08278666 (Intr) run2   run3   run4   run5  
# run2        0.09479313 -0.134                            
# run3        0.08375247 -0.317  0.765                     
# run4        0.08873574 -0.171  0.552  0.720              
# run5        0.11464313 -0.258  0.747  0.854  0.745       
# run6        0.10375735 -0.318  0.697  0.794  0.706  0.878
# Residual    0.22783828                                   
# 
# Fixed effects:  FD ~ 1 + run + age_group * feedback 
# Value  Std.Error    DF   t-value p-value
# (Intercept)              0.4524204 0.03491125 61774 12.959158  0.0000
# run2                     0.0198209 0.01118544 61774  1.772030  0.0764
# run3                     0.0294646 0.00999432 61774  2.948129  0.0032
# run4                     0.0358940 0.01053085 61774  3.408465  0.0007
# run5                     0.0352148 0.01335915 61774  2.636004  0.0084
# run6                     0.0278481 0.01217601 61774  2.287125  0.0222
# age_groupyoung          -0.1769315 0.03945612    74 -4.484260  0.0000
# feedback                -0.1344319 0.03799784    74 -3.537883  0.0007
# age_groupyoung:feedback  0.0724903 0.04462674    74  1.624370  0.1085
# Correlation: 
#   (Intr) run2   run3   run4   run5   run6   ag_grp fedbck
# run2                    -0.047                                                 
# run3                    -0.095  0.740                                          
# run4                    -0.057  0.547  0.698                                   
# run5                    -0.078  0.729  0.824  0.726                            
# run6                    -0.094  0.682  0.768  0.689  0.853                     
# age_groupyoung          -0.873  0.000  0.000  0.000  0.000  0.000              
# feedback                -0.907  0.000  0.000  0.000  0.000  0.000  0.802       
# age_groupyoung:feedback  0.772  0.000  0.000  0.000  0.000  0.000 -0.884 -0.851
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -3.9093458 -0.4939251 -0.1547667  0.3020147 29.6137869 
# 
# Number of Observations: 61857
# Number of Groups: 78 
# > 





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
# 658169.3 658259.6 -329074.6
# 
# Random effects:
#   Formula: ~frame | subject_number
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev      Corr  
# (Intercept) 32.17557679 (Intr)
# frame        0.04100525 -0.456
# Residual    55.38268593       
# 
# Correlation Structure: AR(1)
# Formula: ~frame | subject_number 
# Parameter estimate(s):
#   Phi 
# 0.4578286 
# Fixed effects:  DVARS ~ 1 + frame + age_group * feedback 
# Value Std.Error    DF   t-value p-value
# (Intercept)             244.41952 13.012674 61778 18.783188  0.0000
# frame                     0.00611  0.004908 61778  1.244030  0.2135
# age_groupyoung          -31.76489 14.664553    74 -2.166100  0.0335
# feedback                -24.40635 14.121338    74 -1.728331  0.0881
# age_groupyoung:feedback   2.21691 16.585800    74  0.133663  0.8940
# Correlation: 
#   (Intr) frame  ag_grp fedbck
# frame                   -0.136                     
# age_groupyoung          -0.871  0.000              
# feedback                -0.904  0.000  0.802       
# age_groupyoung:feedback  0.770  0.000 -0.884 -0.851
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -3.6564918 -0.5347602 -0.1496751  0.3213417 19.1731510 
# 
# Number of Observations: 61857
# Number of Groups: 78 



# Do an additional model by run (but ignoring autocorrelations because it's a bit too complex)

m6 <- lme(DVARS ~ 1 + run + age_group * feedback,
          random = ~ run | subject_number,
          data = df,
          method = "ML",
          correlation = NULL)



# summary(m6)
# Linear mixed-effects model fit by maximum likelihood
# Data: df 
# AIC      BIC    logLik
# 670464.3 670744.3 -335201.1
# 
# Random effects:
#   Formula: ~run | subject_number
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev   Corr                              
# (Intercept) 30.84138 (Intr) run2   run3   run4   run5  
# run2        22.20677 -0.190                            
# run3        22.88193 -0.377  0.674                     
# run4        24.41178 -0.246  0.485  0.724              
# run5        32.62056 -0.416  0.580  0.750  0.666       
# run6        27.06392 -0.454  0.512  0.693  0.695  0.837
# Residual    54.00454                                   
# 
# Fixed effects:  DVARS ~ 1 + run + age_group * feedback 
# Value Std.Error    DF   t-value p-value
# (Intercept)             243.49427 12.192275 61774 19.971192  0.0000
# run2                      3.08907  2.623021 61774  1.177675  0.2389
# run3                      5.56690  2.696872 61774  2.064205  0.0390
# run4                      7.33254  2.863635 61774  2.560571  0.0105
# run5                      5.65629  3.768866 61774  1.500794  0.1334
# run6                      2.94432  3.157047 61774  0.932620  0.3510
# age_groupyoung          -32.41583 13.721953    74 -2.362334  0.0208
# feedback                -27.06506 13.213980    74 -2.048214  0.0441
# age_groupyoung:feedback   6.10355 15.519814    74  0.393274  0.6952
# Correlation: 
#   (Intr) run2   run3   run4   run5   run6   ag_grp
# run2                    -0.061                                          
# run3                    -0.112  0.660                                   
# run4                    -0.076  0.486  0.708                            
# run5                    -0.123  0.573  0.733  0.656                     
# run6                    -0.133  0.510  0.679  0.682  0.819              
# age_groupyoung          -0.870  0.000  0.000  0.000  0.000  0.000       
# feedback                -0.903  0.000  0.000  0.000  0.000  0.000  0.802
# age_groupyoung:feedback  0.769  0.000  0.000  0.000  0.000  0.000 -0.884
# fedbck
# run2                          
# run3                          
# run4                          
# run5                          
# run6                          
# age_groupyoung                
# feedback                      
# age_groupyoung:feedback -0.851
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -3.5936520 -0.5249495 -0.1419796  0.3263341 19.8939736 
# 
# Number of Observations: 61857
# Number of Groups: 78 



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
