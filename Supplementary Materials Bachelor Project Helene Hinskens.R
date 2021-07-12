#loading libraries and loading data
library(tidyverse)
library(magrittr)
library(lattice)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(cowplot)
library(sjPlot)
library(mctest)
library(broom)
require(MASS)
library(questionr)
library(car)
library(Hmisc)

survey <- read.csv("20210507_draft_data.csv", header=TRUE, sep=";")


#regression analysis for Background Characteristics
#Creating new dataset with variables, performing logistic regression, builing a full- and stepmodel 
#and showing the results
data1 <- survey[,c("donated", "Grade", "Sex", "ETH01", "ETH03", "EduLevel", "MPT01W14", "AgeW1")]

fit1 <- data1 %$% glm(donated ~ AgeW1 + Grade + Sex + EduLevel + ETH01 + ETH03 + MPT01W14,
                      family = binomial(link="logit"))
summary(fit1)

#creating a full model and step model
full.model1 <- glm(donated ~ AgeW1 + Grade + Sex + EduLevel + ETH01 + ETH03 + MPT01W14,
                   data = na.omit(data1), family = binomial(link="logit"))
step.model1 <- step(full.model1, direction = "both", 
                    trace = FALSE)
summary(step.model1)
#releveling the variable sex so girl and boy are starting points
data1$Sex <- as.factor(data1$Sex)
data1$Sex = relevel(data1$Sex , ref="Jongen")

#another full- and stepmodel with the right direction of sex
full.model1 <- glm(donated ~ AgeW1 + Grade + Sex + EduLevel + ETH01 + ETH03 + MPT01W14,
                   data = na.omit(data1), family = binomial(link="logit"))
step.model1 <- step(full.model1, direction = "both", 
                    trace = FALSE)
summary(step.model1)

summary(full.model1)

#logistic regression for socio-emotional well-being. 
#Creating new dataset with variables, performing logistic regression, builing a full- and stepmodel 
#and showing the results

data2 <- survey[,c("AWB01W14", "CWB01W14", "PAF", "NAF","SEEW14", "LONW14", "donated" )]
#logistic regression
fit2 <- data2 %$% glm(donated ~ AWB01W14 + CWB01W14 + PAF + NAF + SEEW14 + LONW14,
                      family = binomial(link="logit"))
#view results
summary(fit2)

#another full model and creating a stepmodel
full.model2 <- glm(donated ~ AWB01W14 + CWB01W14 + PAF + NAF + SEEW14 + LONW14,
                   data = na.omit(data2), family = binomial(link="logit"))
step.model2 <- step(full.model2, direction = "both", 
                    trace = FALSE)
#view stepmodel
summary(step.model2)



#logistic regression for self-regulation. 
#Creating new dataset

data3<- survey[,c("SRT14", "donated")]
#builing logit model
fit3 <- data3 %$% glm(donated ~ SRT14,
                      family = binomial(link="logit"))
#viewing results
summary(fit3)


#logistic regression analysis for parental context
#new dataset
data4 <- survey[,c("PRW14", "PMKW14", "ADSW14", "donated" )]

#logistic regression
fit4 <- data4 %$% glm(donated ~ PRW14 + PMKW14 + ADSW14,
                      family = binomial(link="logit"))
#view full model
summary(fit4)

#build stepmodel
full.model4 <- glm(donated ~ PRW14 + PMKW14 + ADSW14,
                   data = na.omit(data4), family = binomial(link="logit"))
step.model4 <- step(full.model4, direction = "both", 
                    trace = FALSE)
#view stepmodel
summary(step.model4)



#logistic regression for peer pressure
#new dataset
data5 <- survey[,c("IFI01W1", "IFI02W1", "SCPW1", "donated" )]
#performing regressio
fit5 <- data5 %$% glm(donated ~ IFI01W1 + IFI02W1 + SCPW1,
                      family = binomial(link="logit"))
#viewing regression
summary(fit5)

#building step model
full.model5 <- glm(donated ~ IFI01W1 + IFI02W1 + SCPW1,
                   data = na.omit(data5), family = binomial(link="logit"))
step.model5 <- step(full.model5, direction = "both", 
                    trace = FALSE)
#results stepmodel
summary(step.model5)


#logitistic regression for commitment to study
#new dataset
data6 <- survey[,c("StateW16", "NCompletedE1", "NCompletedE2", "donated" )]
#logit model
fit6 <- data6 %$% glm(donated ~ StateW16 + NCompletedE1 + NCompletedE2,
                      family = binomial(link="logit"))
#view regression results
summary(fit6)


#build stepmodel
full.model6 <- glm(donated ~ StateW16 + NCompletedE1 + NCompletedE2,
                   data = na.omit(data6), family = binomial(link="logit"))
step.model6 <- step(full.model6, direction = "both", 
                    trace = FALSE)
#view stepmodel results
summary(step.model6)


#logistic regression for social media use
#new dataset
data7 <- survey[,c("MPSW1", "INPF1W1", "INAF1W1", "INPF2W1", "INAF2W1", "donated" )]
#build regression model
fit7 <- data7 %$% glm(donated ~ MPSW1 + INPF1W1 + INAF1W1 + INPF2W1 + INAF2W1,
                      family = binomial(link="logit"))
#view regression results
summary(fit7)


#build stepmodel
full.model7 <- glm(donated ~ MPSW1 + INPF1W1 + INAF1W1, INPF2W1, INAF2W1,
                   data = na.omit(data7), family = binomial(link="logit"))
step.model7 <- step(full.model7, direction = "both", 
                    trace = FALSE)
#view stepmodel results
summary(step.model7)



#creating dataset with final variables
datafinal <- survey[,c("donated", "Grade", "AgeW1", "SEEW14", "ADSW14", "SCPW1", "IFI01W1", "NCompletedE2", "MPSW1", "INAF1W1")]
#creating a final logistic regression model 
fitfinal <- datafinal %$% glm(donated ~ Grade + AgeW1 + SEEW14 + ADSW14 + SCPW1 + IFI01W1 + NCompletedE2 + MPSW1 + INAF1W1,
                              family = binomial(link="logit"))
summary(fitfinal)

# calculating the odds ratios and the 95% confidence interval for the final variables
odds.ratio(fitfinal)

numdata <- datafinal[,c("donated", "AgeW1", "SEEW14", "ADSW14", "SCPW1", "MPSW1")]
numdata <- na.omit(datafinal)
numfit <- numdata %$% glm(donated ~ AgeW1 + SEEW14 + ADSW14 + SCPW1 + MPSW1,
                          family = binomial(link="logit"))

#ASSUMPTIONS

#checking the assumption for linearity of the logit
probabilities <- predict(numfit, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

# Select only numeric predictors
mydata <- numdata %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
#plotting the data
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


#checking Cook's Distance
plot(fitfinal, which = 4, id.n = 3)

#checking for any influencial cases
# Extract model results
model.data <- augment(numfit) %>% 
  mutate(index = 1:n()) 

model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = donated), alpha = .5) +
  theme_bw()

#another check for influencial cases
as.data.frame(do.call(rbind, datafinal))

datafinal %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_boxplot() 


# filter potential influencial data 
model.data %>% 
  filter(abs(.std.resid) > 3)

#checking for multicollinearity
fitfinal %>% mc.plot(vif = 10, ev = 0.01)


