#Model assumptions
#packages
library(readr)
library(dplyr)
library(ggpubr)
library(car)
library(nortest)
library(moments)
library(olsrr)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

#----------------------------------------------------------------------------------
#data
survey2023_INSPQ <- read_csv("data/survey2023 INSPQ.csv")

#----------------------------------------------------------------------------------
#data clean up
survey2023_INSPQ <- survey2023_INSPQ %>%
  filter(Sex != "Unknown")

survey2023_INSPQ <- survey2023_INSPQ %>%
  filter(Industry != "Unknown")

#----------------------------------------------------------------------------------
#shapiro-wilks test only if 50 observations or less in general

#Kolmogorov-Smirnov if > 50 observations

ols_test_normality(model1)
#----------------------------------------------------------------------------------
#other tests look good though visually

hist(survey2023_INSPQ$RiskScore, col='steelblue')

#calculate skewness
skewness(survey2023_INSPQ$RiskScore)

#calculate kurtosis
kurtosis(survey2023_INSPQ$RiskScore)

#Jarque-Bera Normality Test
jarque.test(survey2023_INSPQ$RiskScore) #normal distribution?

ggdensity(survey2023_INSPQ$RiskScore, 
          main = "",
          xlab = "Risk Score")

ggqqplot(survey2023_INSPQ$RiskScore)

qqPlot(survey2023_INSPQ$RiskScore)


# Perform Anderson-Darling test on residuals
ad.test(survey2023_INSPQ$RiskScore)

#----------------------------------------------------------------------------------
#residuals
plot(model1)
hist(model1$residuals, main = "Residual Histogram")
boxplot(model1$residuals, main="Residual Box Plot")
#----------------------------------------------------------------------------------
#visuals

boxplot(RiskScore~workhours, data=survey2023_INSPQ, notch=FALSE, ylim=c(0,20),
        main="", xlab="Work Hours per Week", ylab = "Behaviour risk score")

boxplot(RiskScore~rechours, data=survey2023_INSPQ, notch=FALSE, ylim=c(0,20),
        main="", xlab="Recreational Hours per Week", ylab = "Behaviour risk score")


boxplot(RiskScore~days, data=survey2023_INSPQ, notch=FALSE, ylim=c(0,20),
        main="", xlab="Recreational Hours per Week", ylab = "")


