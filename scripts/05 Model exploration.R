#Model testing
#load packages
library(readr)
library(dplyr)
library(lme4)
library(car)
library(tidyverse)
library(caret)

#----------------------------------------------------------------------------------
#data
survey2023_INSPQ <- read_csv("data/survey2023 INSPQ.csv")

#----------------------------------------------------------------------------------
#data clean up
survey2023_INSPQ <- survey2023_INSPQ %>%
  filter(Sex != "Unknown")

#----------------------------------------------------------------------------------

boxplot(RiskScore~workhours, data=survey2023_INSPQ, notch=FALSE, ylim=c(0,25),
        main="", xlab="Work Hours per Week", ylab = "Behaviour risk score")

boxplot(RiskScore~rechours, data=survey2023_INSPQ, notch=FALSE, ylim=c(0,25),
        main="", xlab="Recreational Hours per Week", ylab = "Behaviour risk score")

boxplot(RiskScore~INSPQriskOFF, data=survey2023_INSPQ, notch=FALSE, ylim=c(0,25),
        main="", xlab="INSPQ risk Score", ylab = "Behaviour risk score")


