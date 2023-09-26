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

survey2023_INSPQ <- na.omit(survey2023_INSPQ)

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
model1 <- lm(RiskScore ~ sexf + agef + eduf + indf + yearsf + daysf + workhoursf + rechoursf + RegionOFF, data = survey2023_INSPQ)
summary(model1)
#survey2023_INSPQ$RegionOFF and survey2023_INSPQ$INSPQriskOFF are correlated based on VIF


#including region and INSPQ score: OUT region

#----------------------------------------------------------------------------------
#spatial autocorrelation - did this affect how region is included in model?
#no significant spatial autocorrelation but autocorrelation did show that INSPQ level and behaviour risk score are correlated


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
#AIC to consider "strongest" model; include interactions

#AIC for RiskScore
model.null = lm(RiskScore ~ 1,
                data=survey2023_INSPQ)

model.full = lm(RiskScore ~ 
                  sexf + agef + eduf + indf + daysf + workhoursf + rechoursf + RegionOFF + INSPQriskOFF + sexf*agef + sexf*eduf + sexf*indf + sexf*daysf + sexf*workhoursf + sexf*rechoursf + sexf*RegionOFF + sexf*INSPQriskOFF + agef*eduf + agef*indf + agef*daysf + agef*workhoursf + agef*rechoursf + agef*RegionOFF + agef*INSPQriskOFF + eduf*indf + eduf*daysf + eduf*workhoursf + eduf*rechoursf + eduf*RegionOFF + eduf*INSPQriskOFF+ indf*daysf + indf*workhoursf + indf*rechoursf + indf*RegionOFF + indf*INSPQriskOFF + daysf*workhoursf + daysf*rechoursf + daysf*RegionOFF + daysf*INSPQriskOFF + workhoursf*rechoursf + workhoursf*RegionOFF + workhoursf* INSPQriskOFF + rechoursf*RegionOFF + rechoursf*INSPQriskOFF + RegionOFF*INSPQriskOFF,                
                data=survey2023_INSPQ)

step(model.null,
     scope = list(upper=model.full),
     direction="forward",
     data=survey2023_INSPQ)  


#----------------------------------------------------------------------------------
#top models
#RiskScore ~ INSPQriskOFF + workhoursf + rechoursf
#                          DF.    SS.   RSS.    AIC
#<none>                                 1844.6 403.48
#+ eduf                     1    19.474 1825.1 403.75
#+ agef                     1     8.287 1836.3 404.75
#+ workhoursf:INSPQriskOFF  1     6.774 1837.8 404.88
#+ workhoursf:rechoursf     1     3.065 1841.5 405.21
#+ indf                     1     2.635 1842.0 405.25
#+ daysf                    1     0.173 1844.4 405.47
#+ sexf                     1     0.123 1844.5 405.47
#+ rechoursf:INSPQriskOFF 

model <- glm(RiskScore ~ INSPQriskOFF + workhoursf + rechoursf +eduf +agef + workhoursf:INSPQriskOFF +indf + daysf + sexf + rechoursf:INSPQriskOFF , data = survey2023_INSPQ)
summary(model)

#----------------------------------------------------------------------------------
#wait a minute now, technically risk score and INSPQriskOFF are spatially correlated?
#try without INSPQriskOFF

#AIC to consider "strongest" model; include interactions

#AIC for RiskScore
model.null = glm(RiskScore ~ 1,
                data=survey2023_INSPQ)

model.full = glm(RiskScore ~ 
                  sexf + agef + eduf + indf + daysf + workhoursf + rechoursf + RegionOFF + sexf*agef + sexf*eduf + sexf*indf + sexf*daysf + sexf*workhoursf + sexf*rechoursf + sexf*RegionOFF + agef*eduf + agef*indf + agef*daysf + agef*workhoursf + agef*rechoursf + agef*RegionOFF + eduf*indf + eduf*daysf + eduf*workhoursf + eduf*rechoursf + eduf*RegionOFF + indf*daysf + indf*workhoursf + indf*rechoursf + indf*RegionOFF + daysf*workhoursf + daysf*rechoursf + daysf*RegionOFF + workhoursf*rechoursf + workhoursf*RegionOFF + rechoursf*RegionOFF,                
                data=survey2023_INSPQ)

step(model.null,
     scope = list(upper=model.full),
     direction="forward",
     data=survey2023_INSPQ) 

#----------------------------------------------------------------------------------
#top models
#RiskScore ~ rechoursf + workhoursf + eduf
#                          DF.    SS.   RSS.    AIC
#<none>                              1901.0 408.39
#+ agef                  1    12.182 1888.8 409.34
#+ eduf:rechoursf        1     9.184 1891.8 409.60
#+ workhoursf:rechoursf  1     5.253 1895.7 409.94
#+ indf                  1     4.218 1896.8 410.03
#+ daysf                 1     0.901 1900.1 410.31
#+ sexf                  1     0.599 1900.4 410.34
#+ eduf:workhoursf       1     0.174 1900.8 410.37

model <- glm(RiskScore ~ workhoursf + rechoursf +eduf, data = survey2023_INSPQ)
model <- glm(RiskScore ~ workhoursf + rechoursf +eduf +agef + eduf:rechoursf + workhoursf:rechoursf +indf + daysf + sexf + eduf:workhoursf, data = survey2023_INSPQ)
summary(model)
