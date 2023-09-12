


library(dplyr)

survey2023_INSPQ <- survey2023_INSPQ %>%
  filter(Sex != "Unknown")


DOR <- DOR %>%
  filter(Sex != "Unknown")


# normality
#p > 0.05 = normal
shapiro.test(DOR$RiskScore) #not normal, with or without unknown industry 
shapiro.test(DOR$RiskScore) # not normal


library(lme4)
library(lmerTest)
library(car)
#unknown idustry included
model1 <-lmer(OUTRS ~ Sex + factage + factedu + factyears + facthours + factrec + (1|Industry) + (1|out1region), data = DOR, REML = FALSE)
model1 <-lmer(OUTRS ~ Sex + factage + factedu + factyears + facthours + factrec + (1|Industry) + (1|out1region), data = DOR)

model2 <-lm(OUTRS ~ Sex + factage + factedu + factyears + factrec + facthours, data = DOR)
summary(model1)
summary(model2)
Anova(model1)



model3 <-lmer(OUTRS ~ Sex + factage + factedu + factyears + factrec + facthours + (1|Industry) + (1|out1region) + (1|ID), data = DOR)
model3 <-lm(OUTRS ~ Sex + factage + factedu + factyears + factrec + facthours, data = DOR1)


summary(model3)
summary(model2)
summary(model3)
Anova(model3)

#unknown industry excluded, have to remove predictor because sample size small
model2 <-lmer(OUTRS ~ Sex + factrec + facthours + factedu + factage + factyears + (1|Industry), data = DORI)
model2 <-lm(OUTRS ~ Sex + factrec + facthours + factedu + factage + factyears, data = DORI)

summary(model1)
summary(model2)
Anova(model1)


#VIF
#requires packages in R
library(tidyverse)
library(caret)
library(dplyr)

# Make predictions
predictions <- model1 %>% predict(DOR)

# Model performance
data.frame(
  RMSE = RMSE(predictions, DOR$OUTRS),
  R2 = R2(predictions, DOR$OUTRS)
)
car::vif(model1)

cor(DOR$factyears, DOR$factedu)

boxplot(OUTRS~workyears, data=DOR, notch=FALSE, ylim=c(0,50),
        main="", xlab="Years in Job Position", ylab = "Perceived risk score")

boxplot(OUTRS~rechours, data=DOR, notch=FALSE, ylim=c(0,50),
        main="", xlab="Recreational Hours per Week", ylab = "Perceived risk score")


