#assignment 1 part 1

#load the data
data_part_1 = read.csv("https://tinyurl.com/yxm5rd89")

#add packages
library(tidyverse)
library(psych)

#explore the data
summary(data_part_1)

view(data_part_1)

str(data_part_1)

levels(data_part_1)

#the structure of sex was assessed separately, as it is categorical

str(data_part_1$sex)

#as the research question is about experienced pain (quantitative scale, 0-10), the State Anxiety Inventory - T (quantitative, 20 - 80), 
#The Pain Catastrophing Scale (quantitative, 0 - 52), The mindful attention scale (quantative, 1 - 6)
#Cortisol levels: blood and saliva.

#After exploring the data, I was able to identify 2 errors in the data. Participant 88 reported having experienced pain
# equal to 55, yet the maximum pain score was 10. Similarly, participant 34 reported STAI-T of 4.2, while the minumum score is 20.
#These participants were excluded from the data. This decision was made, because there is no certainty of what participants meant to report. 

data_part_1_cleaned <- data_part_1[!(data_part_1$ID=="ID_88" | data_part_1$ID=="ID_34"),]
data_part_1_cleaned
view(data_part_1_cleaned)

#focused exploration of each variable of interest

# histograms
data_part_1_cleaned %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram(bins = 50)	

data_part_1_cleaned %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram(bins = 50)	

data_part_1_cleaned %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram(bins = 50)	

data_part_1_cleaned %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram( bins = 50)	

data_part_1_cleaned %>% 	
  ggplot() +	
  aes(x = mindfulness) +	
  geom_histogram( bins = 50)	

data_part_1_cleaned %>% 	
  ggplot() +	
  aes(x = cortisol_saliva) +	
  geom_histogram(bins = 50)	

data_part_1_cleaned %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram( bins = 50)	


# scatterplots	

data_part_1_cleaned %>% 	
  ggplot() +	
  aes(x = pain, y = STAI_trait) +	
  geom_point()	

data_part_1_cleaned %>% 	
  ggplot() +	
  aes(x = pain, y = age) +	
  geom_point()	

data_part_1_cleaned %>% 	
  ggplot() +	
  aes(x = pain, y = pain_cat) +	
  geom_point()	

data_part_1_cleaned %>% 	
  ggplot() +	
  aes(x = pain, y = mindfulness ) +	
  geom_point()	

data_part_1_cleaned %>% 	
  ggplot() +	
  aes(x = pain, y = cortisol_saliva) +	
  geom_point()	

data_part_1_cleaned %>% 	
  ggplot() +	
  aes(x = pain, y = cortisol_serum ) +	
  geom_point()	

#coeftable 

coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}

#building model 1


model1 <- lm(pain ~ age + sex, data = data_part_1_cleaned)
model1
summary(model1)

confint(model1)
lm.beta(model1)
standardCoefs(model1)
coef_table(model1)


#identifying extreme outliers

model1 %>% 	
  plot(which = 4)	

model1 %>% 	
  plot(which = 5)	

#as observations 8, 23 and 47 are flagged (yet don't exceed the cook's distance of < 1), they are evaluated

data_part_1_cleaned %>% 	
  slice(c(8, 23, 47))	

#no extreme outliers


#assessing the assumptions of linear regression

#normality of the residuals

# QQ plot	
model1 %>% 	
  plot(which = 2)	
#not violated

#linearity

model1 %>% 	
  residualPlots()	
#line is curved, but it's not sgnificant. not violated.

#homoscedacity

model1 %>% 	
  ncvTest() 
#insignificant, thus not violated.

#no multicollinaearity

model1 %>% 	
  vif()	
#not violated


#building model 2

model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = data_part_1_cleaned)
model2
summary(model2)
confint(model2)
lm.beta(model2)
standardCoefs(model2)
coef_table(model2)

#initial notes: the r^2 is remarkably higher for model 2: 0.518 vs 0.074!


#identifying extreme outliers

model2 %>% 	
  plot(which = 4)	

model2 %>% 	
  plot(which = 5)	

#observations 47, 74 and 86 are flagged, yet are not considered to be extreme outliers.These observations are evaluated. 
data_part_1_cleaned %>% 	
  slice(c(47, 74, 86))	
#seems fine

#assessing the assumptions of linear regression

#normality of the residuals

# QQ plot	
model2 %>% 	
  plot(which = 2)	
#no violation

#linearity

model2 %>% 	
  residualPlots()	
#no violation

#homoscedacity

model2 %>% 	
  ncvTest() 
#no violation

#no multicollinaearity

model2 %>% 	
  vif()	
# cortisol_saliva and cortisol_serum have a VIF of over 3. As both variables measure the same construct, it is logical that they are correlated. 
#I will remove cortisol_saliva, as cortisol extracted from blood is considered to be a more reliable method.
#therefore, i will build a new model

model2_final <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +  cortisol_serum, data = data_part_1_cleaned)
model2_final
summary(model2_final)
confint(model2_final)
lm.beta(model2_final)
standardCoefs(model2_final)
coef_table(model2_final)


#Rechecking the multicollinearity
model2_final %>% 	
  vif()	
#not violated.

#I will recheck the other assumptions just to be sure
# QQ plot	
model2_final %>% 	
  plot(which = 2)	
#no violation

#linearity

model2_final %>% 	
  residualPlots()	
#no violation

#homoscedacity

model2_final %>% 	
  ncvTest() 
#no violation




#comparing the two models
#using ANOVA
anova(model1, model2_final)	
#significant difference! 

#and AIC
AIC(model1)	
AIC(model2_final)
#aic for model 2 is smaller and < 2, thus it is better.



#assignment 1 part 2

#the same data is used as the assignment 1. This dataset is called data_part_1_cleaned.

#first, i will build the model

model3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +  cortisol_serum + weight + IQ + household_income, data = data_part_1_cleaned)
summary(model3)

#first I will identify extreme outliers

model3 %>% 	
  plot(which = 4)	

model3 %>% 	
  plot(which = 5)	

#observations 47, 85 and 86 are flagged, yet are not considered to be extreme outliers.These observations are evaluated. 
data_part_1_cleaned %>% 	
  slice(c(47, 85, 86))
#seems fine

#Secondly, I will run model diagnostics
#assessing the assumptions of linear regression

#normality of the residuals

# QQ plot	
model3 %>% 	
  plot(which = 2)	
#no violation

#linearity

model3 %>% 	
  residualPlots()	
#no violation

#homoscedacity

model3 %>% 	
  ncvTest() 
#no violation

#multicollinaearity

model3 %>% 	
  vif()	
#no violation.

#Next, I will execute the backwards regression.

model_backward <- step(model3, direction = "backward")
summary(model_backward)
confint(model_backward)
lm.beta(model_backward)
standardCoefs(model_backward)
coef_table(model_backward)


#model doagnostics for backwards model

#normality of the residuals
# QQ plot	
model_backward %>% 	
  plot(which = 2)	
#not violated

#linearity

model_backward %>% 	
  residualPlots()	
#line is curved, but it's not sgnificant. not violated.

#homoscedacity

model_backward %>% 	
  ncvTest() 
#insignificant, thus not violated.

#no multicollinaearity

model_backward %>% 	
  vif()	
#not violated

#here is the theory based model again

model_theory <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +  cortisol_serum, data = data_part_1_cleaned)

#Comparing the original model chosen, and the new model

anova(model_theory, model_backward)
AIC(model_theory)
AIC(model_backward)

#anova is not significant! no real difference?
#the differences between thr r^2 are very small anyway (0.5073 for backwards regression vs. 0.518 for original model)


#loading the data for part 2
data_part_2 = read.csv("https://tinyurl.com/87v6emky")
summary(data_part_2)

#calculating predicted values for the dataset 2 based on the regression model trained on dataset 1

pred_test_theory <- predict(model_theory, data_part_2)
pred_test_backward <- predict(model_backward, data_part_2)


# calculating the sum of squared residuals

RSS_test = (sum((data_part_2[, "pain"] - pred_test_theory)^2))
RSS_test_back = (sum((data_part_2[, "pain"] - pred_test_backward)^2))
RSS_test
#243.8192
RSS_test_back
#249.5637

#assignment 1 part 3

 
library(cAIC4) 
library(r2glmm) 
library(lme4) 
library(lmerTest)
library(MuMIn)

#customfunction for standardised beta coefficients


stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}


#loading data
dataset3 = read.csv("https://tinyurl.com/b385chpu")
dataset4 = read.csv("https://tinyurl.com/4f8thztv")

summary(dataset3)
view(dataset3)
describe(dataset3)
levels(dataset3$sex)


#Participant 2 was excluded, as household income was -7884.

dataset3_cleaned <- dataset3[!(dataset3$ID=="ID_2"),]
dataset3_cleaned
view(dataset3_cleaned)

#participant 25 reported their gender as "woman". this was changed to female

dataset3_cleaned_final <- dataset3_cleaned %>%
  mutate(sex = replace(sex, sex == "woman", "female"))

view((dataset3_cleaned_final))
str(dataset3_cleaned_final)

#as hospital is character it is mutated to be a factor to better fit the analysis
dataset3_cleaned_final %>% mutate(hospital = factor(hospital))
describe(dataset3_cleaned_final)
summary(dataset3_cleaned_final)


#random intercept mixed model for dataset 3 using the predictors from part 1

model_random_intercept = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +  cortisol_serum + (1 | hospital), data = dataset3_cleaned_final)
summary(model_random_intercept)
confint(model_random_intercept)




# final model from part 1
model_theory <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +  cortisol_serum, data = data_part_1_cleaned)

# variance explained by the fixed effect predictors using marginal R^2
# and the variance explained by the fixed and random effect terms combined using conditional R2

# marginal R2
# If the 95% CI contains 0, it means that the fixed effect term(s) do not explain a significant portion of the variation of the
# outcome
r2beta(model_random_intercept, method = "nsj", data = dataset3)

# marginal and conditional R2
# conditional R tells the variance explained by the fixed and random effect terms combined 

r.squaredGLMM(model_random_intercept)



#examining dataset 4 for errors
summary(dataset4)
describe(dataset4)
view(dataset4)
#no errors

datset4_prediction <-  predict(model_random_intercept, dataset4, allow.new.levels = TRUE)

#computing variance explained by the model using dataset 4
RSS = sum((dataset4$pain - datset4_prediction)^2)
RSS

model_mean <- lm(pain ~ 1, data = dataset4)
model_mean
TSS=sum((dataset4$pain - predict(model_mean))^2)
TSS

R<-1-(RSS/TSS)
R

#building a new linear model using only the most influental predictor from the previous model. 
#so cortisol, since the r^2m was .13

model_random_intercept_slope = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital),
                                    data = dataset3_cleaned_final)
model_random_intercept_slope
#singular fit? thus, random slope not useful

#visualising the graphs for all hospitals

dataset3_cleaned_final%>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 1) +		
  geom_line(color='blue', aes(y=predict(model_random_intercept_slope), x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)






