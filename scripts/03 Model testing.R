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
#factor independent variables
sexf <- factor(survey2023_INSPQ$Sex)
agef <- factor(survey2023_INSPQ$Age)
eduf <- factor(survey2023_INSPQ$Education)
indf <- factor(survey2023_INSPQ$Industry)
years <- factor(survey2023_INSPQ$years)
days <- factor(survey2023_INSPQ$days)
workhours <- factor(survey2023_INSPQ$workhours)
rechours <- factor(survey2023_INSPQ$rechours)

sexf <- as.numeric(sexf)
agef <- as.numeric(agef)
eduf <- as.numeric(eduf)
indf <- as.numeric(indf)
yearsf <- as.numeric(years)
daysf <- as.numeric(days)
workhoursf <- as.numeric(workhours)
rechoursf <- as.numeric(rechours)

#----------------------------------------------------------------------------------
#create model(s)
model1 <- lm(RiskScore ~ sexf + agef + eduf + indf + yearsf + daysf + workhoursf + rechoursf, data = survey2023_INSPQ)
summary(model1)

#including region and INSPQ score? OUT or ON region? Both? Factor?

#----------------------------------------------------------------------------------
#spatial autocorrelation - did this affect how region is included in model?


#----------------------------------------------------------------------------------
# Make predictions
predictions <- model1 %>% predict(survey2023_INSPQ)

# Model performance
data.frame(
  RMSE = RMSE(predictions, survey2023_INSPQ$RiskScore),
  R2 = R2(predictions, survey2023_INSPQ$RiskScore)
)
car::vif(model1)

#test VIF - all VIF scores < 2
#----------------------------------------------------------------------------------
#AIC or Maximum likelihood to consider "strongest" model; include interactions

