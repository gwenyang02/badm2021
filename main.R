#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()
set.seed(42) # Set a seed to ensure repeatable random samples

#install.packages("tidyverse")
# install.packages("car")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("nnet")
# install.packages("randomForest")
# install.packages("effects")
# install.packages("data.table")
# install.packages("corrplot")

library("tidyverse")
library("car")
library("rpart")
library("rpart.plot")
library("nnet")
library("randomForest")
library("effects")
library(data.table)
library("corrplot")
require(PerformanceAnalytics)
source('BCA_functions_source_file.R')

usage  <- read_csv("USAGE_DATASET.CSV")
survey <- read_csv("SURVEY_DATASET.CSV")
fsa    <- read_csv("FSA_DATASET.CSV")
variable.summary(survey)
#missing
#perc_weight           character 5.154062     
#bmi_class             character 5.154062
variable.summary(fsa)
#no other missing variables

#change column name
colnames(fsa)[2] <- "clinic_fsa"

#combine whole dataset
combined1 <- usage %>% merge(survey, by = "pt_id") #%>% merge(fsa, by = "clinic_fsa")
#combined2 <- full_join(combined1, fsa, by = "clinic_fsa")
combined2 <- left_join(combined1, fsa, by = "clinic_fsa")

na <- variable.summary(combined2)

#additions:
health <- combined2 

#factors
health$power_us <- as.factor(health$power_us)
health$income <- as.factor(health$income)
health$age <- as.factor(health$age)
health$edu <- as.factor(health$edu)
health$perc_health <- as.factor(health$perc_health)
health$power_us <- as.factor(health$power_us)
#health$perc_weight <- as.factor(health$perc_weight)
#health$bmi_class <- as.factor(health$bmi_class)
health$repstrain <- as.factor(health$repstrain)
health$injstatus <- as.factor(health$injstatus)
health$physactivityindicator <- as.factor(health$physactivityindicator)
health$perc_mentalHealth <- as.factor(health$perc_mentalHealth)
health$perc_lifstress <- as.factor(health$perc_lifstress)
health$perc_workstress <- as.factor(health$perc_workstress)
health$care_language <- as.factor(health$care_language) # only 2 levels - change to binary?

#NA - nonresponse may be indicative of something
health$perc_weight <- health$perc_weight %>% replace_na("Non Reporting")
health$bmi_class <- health$bmi_class %>% replace_na("Non Reporting")
health$perc_weight <- as.factor(health$perc_weight)
health$bmi_class <- as.factor(health$bmi_class)
variable.summary(health)

#binning
#health$pop_level <- binVariable(health$pop_fsa, bins = 6, method= "intervals", labels = c("low", "low-med", "med", "med-high", "high", "very high"))
#health$fsa_age_level <- binVariable(health$median_age_fsa, bins = 9, method= "intervals", labels = FALSE)
#health$fsa_inc_level <- binVariable(health$median_income_fsa, bins = 6, method= "intervals", labels = c("low", "low-med", "med", "med-high", "high", "very high"))
#health$dcost_level <- binVariable(health$median_age_fsa, bins = 6, method= "intervals", labels = c("low", "low-med", "med", "med-high", "high", "very high"))

#remove non-useful measures
health$...1.x <- NULL
health$...1.y <- NULL
health$...1 <- NULL

#scatterplots - can't do with factor
#scatterplot(power_us ~ spending, data = filter(health, Sample == "Estimation"))

#num scale
health$perc_mentalHealth_num <- as.numeric(recode_factor(health$perc_mentalHealth, "Excellet" = 5, "Very Good" = 4, "Good" = 3, "Fair" = 2, "Poor" = 1))
health$perc_health_num <- as.numeric(recode_factor(health$perc_health, "Excellet" = 5, "Very Good" = 4, "Good" = 3, "Fair" = 2, "Poor" = 1))
health$perc_lifstress_num <- as.numeric(recode_factor(health$perc_lifstress, "Extreme" = 5, "Quite" = 4, "A bit" = 3, "Not Very" = 2, "None" = 1))
health$perc_workstress_num <- as.numeric(recode_factor(health$perc_workstress, "Extreme" = 5, "Quite" = 4, "A bit" = 3, "Not Very" = 2, "None" = 1))
#binary
health$care_language_bin <- ifelse(health$care_language == "English", 0, 1)


#start of testing
r.train   <- filter(health,Sample == "Estimation")
r.test    <- filter(health,Sample == "Validation")
r.holdout <- filter(health,Sample =="Holdout" )

#paste(names(health))

#removed pt_id, clinic_fsa, freq, perc_(health, mental health, lifstress, workstress), clinic_fsa (is significant though)
model1.log <- glm(power_us ~ clinic_id  + income  + age  +  
                    edu +  perc_weight  +  bmi_class  +  arthritis  +  highBP  +     diabetes  + 
                    stroke + repstrain + injstatus + physactivityindicator + gave_birth_last5 +      
                    othercare  +  spending  +  pop_fsa  +    median_age_fsa  + 
                    hhold_fsa  +  median_income_fsa + hhold_work_health + avg_spend_health + avg_dcost  +  avg_insur_prem + tot_spend_toba_alco +
                    perc_mentalHealth_num + perc_health_num + perc_lifstress_num +   
                    perc_workstress_num + care_language_bin, data = r.train, family = binomial(logit))
summary(model1.log)

#interactions
#correlations - arthritis, BP, diabetes, stroke
chart.Correlation(health[,c(8,13,14,15)], histogram=TRUE, pch=19)
#all significant
health$arthBP <- health$arthritis*health$highBP
health$arthdiab <- health$arthritis*health$diabetes
health$arthstroke <- health$arthritis*health$stroke
health$BPdiab <- health$highBP*health$diabetes
health$BPstroke <- health$highBP*health$stroke
health$diabstroke <- health$diabetes*health$stroke

chart.Correlation(health[,c(8,27,29,30,31,32,33,34,35)], histogram=TRUE, pch=19)

# interaction terms
#lifestress, workstress, mental health?
chart.Correlation(health[,c(40,41,42,43)], histogram=TRUE, pch=19)
health$perc_mentalxhealth <- health$perc_health_num*health$perc_mentalHealth_num
health$perc_overallstress <- health$perc_lifstress_num*health$perc_workstress_num

# median age, median income, insurance premium, tobacco/alcohol spending
chart.Correlation(health[,c(28,30,32,34,35)], histogram=TRUE, pch=19)
health$age_inc_fsa <- health$median_age_fsa*health$median_income_fsa
health$age_spend_fsa <- health$median_age_fsa*health$avg_spend_health
health$age_insur_fsa <- health$median_age_fsa*health$avg_insur_prem
health$age_tobacco_fsa <- health$median_age_fsa*health$tot_spend_toba_alco
health$inc_spend_fsa <- health$median_income_fsa*health$avg_spend_health
health$inc_insur_fsa <- health$median_income_fsa*health$avg_insur_prem
health$inc_tobacco_fsa <- health$median_income_fsa*health$tot_spend_toba_alco
health$spend_insur_fsa <- health$avg_spend_health*health$avg_insur_prem
health$spend_tobacco_fsa <- health$avg_spend_health*health$tot_spend_toba_alco
health$insur_tobacco_fsa <- health$avg_insur_prem*health$tot_spend_toba_alco

#created variables
health <- mutate(health, spend_per_inc = health$spending / health$median_income_fsa)
health <- mutate(health, high_inj = ifelse(as.numeric(health$injstatus) >= 2, 1, 0))
health <- mutate(health, old_pain = health$age*as.factor(health$high_inj))

#repstrain and injstatus do not provide complete enough data
health$repstrain <- NULL
health$injstatus <- NULL

# creating more accurate location data
health$clinic_id_fac <- as.factor(health$clinic_id)
summary(health$clinic_fsa_num)
health$clinic_fsa_num <- as.numeric(str_sub(health$clinic_fsa, 2,2)) 

r.train   <- filter(health,Sample == "Estimation")
r.test    <- filter(health,Sample == "Validation")
r.holdout <- filter(health,Sample =="Holdout" )


#### additional modeling - add interaction terms ####
model1.log.interaction <- glm(power_us ~ clinic_id  + income  + age  +  
                                edu +  perc_weight  +  bmi_class  +  arthritis  +  highBP  +  diabetes  + 
                                stroke + repstrain + injstatus + physactivityindicator + gave_birth_last5 +      
                                othercare  +  spending  +  pop_fsa  +    median_age_fsa  + 
                                hhold_fsa  +  median_income_fsa + hhold_work_health + avg_spend_health + avg_dcost  +  avg_insur_prem + tot_spend_toba_alco +
                                perc_mentalHealth_num + perc_health_num + perc_lifstress_num +   
                                perc_workstress_num + care_language_bin + arthBP + arthdiab + arthstroke + BPdiab + BPstroke + diabstroke + perc_mentalxhealth+
                                perc_overallstress + age_inc_fsa + age_spend_fsa + age_insur_fsa + age_tobacco_fsa + inc_spend_fsa + inc_insur_fsa + inc_tobacco_fsa +
                                spend_insur_fsa + spend_tobacco_fsa + insur_tobacco_fsa, 
                              data = r.train, family = binomial(logit))
summary(model1.log.interaction)

#stepwise
model1.step <- step(model1.log.interaction,direction="both")

summary(model1.step)
model1.step$coefficients
age_75 <- model1.step$coefficients[9]
under <- model1.step$coefficients[12]
mod_act <- model1.step$coefficients[13]
give_birth_5 <- model1.step$coefficients[16]

#tree
model2.rpart <- rpart(formula = power_us ~ income + age + perc_weight + physactivityindicator + gave_birth_last5 + othercare + pop_fsa
                      + hhold_work_health + avg_spend_health + avg_dcost + perc_mentalHealth_num + age_inc_fsa + age_insur_fsa + inc_insur_fsa + spend_insur_fsa,
                      data = r.train,
                      cp = 0.0001, #set to 0.0001 to check 
                      model = TRUE)

plotcp(model2.rpart)
rpart.plot(model2.rpart,type=1,extra=1,fallen.leaves = FALSE,uniform=TRUE, yes.text="true",no.text="false",cex=0.6,digits=2)
#rpart.plot(model2.rpart, type = 0, extra = 1,
#           fallen.leaves = TRUE, uniform = FALSE) # Without uniform spacing
printcp(model2.rpart)


#forest

model3.forest <- randomForest(formula = power_us ~ income + age + perc_weight + physactivityindicator + gave_birth_last5 + othercare + pop_fsa
                              + hhold_work_health + avg_spend_health + avg_dcost + perc_mentalHealth_num + age_inc_fsa + age_insur_fsa + inc_insur_fsa + spend_insur_fsa,
                              data = r.train,
                              mtry=4, ntree= 500,
                              importance = TRUE)
model3.forest

importance(model3.forest,type = 2)
varImpPlot(model3.forest,type = 2, main = "Importance Plot")

model4.net <- Nnet(formula = power_us ~ income + age + perc_weight + physactivityindicator + gave_birth_last5 + othercare + pop_fsa
                   + hhold_work_health + avg_spend_health + avg_dcost + perc_mentalHealth_num + age_inc_fsa + age_insur_fsa + inc_insur_fsa + spend_insur_fsa,
                   data = r.train,
                   decay = 0.10, # decay parameter
                   size = 2)
model4.net$value
summary(model4.net)

summary(r.train$power_us)


#go with logistic stepwise
plot(allEffects(model1.step), type="response")


#specific high effect
plot(effect("age",model1.step), type = "response")
plot(effect("physactivityindicator",model1.step)) # look at barplot, maybe relevel
data <- summary(health$physactivityindicator)
barplot(data)
plot(effect("gave_birth_last5",model1.step), type = "response")
plot(effect("age_inc_fsa",model1.step), type = "response")



health$power_us.model1.step <- rawProbScore(model = "model1.step",
                                            data = health,
                                            targLevel = "1")

health$power_us.model1.log.interaction <- rawProbScore(model = "model1.log.interaction",
                                            data = health,
                                            targLevel = "1")

Submission.model1.step <- health[health$Sample == "Holdout",c("pt_id","power_us.model1.step")]

submission.model1.int <- health[health$Sample == "Holdout", c("pt_id","power_us.model1.log.interaction")]


names(submission.model1.int) <- c("pt_id", "score")

#change to team name
write.csv(submission.model1.int,"team408.csv")

#need different measure of fit for presentation - hit rates, AUC - find something?

#hit-rate matrix
HRmodel1.step <- table(filter(health,Sample == "Estimation")$power_us, model1.step$fitted.values > 0.5)
# Print the results
n <- 4
colors <- c('blue')
legendlables <- c("LinCCS","LogCCS","MixedCCS","MixedCCS2")
HRmodel1.step
#    FALSE TRUE
#  0  1884   19
#  1   504  267

