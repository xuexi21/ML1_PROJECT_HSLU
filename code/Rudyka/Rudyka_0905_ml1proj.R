library(ggplot2)
library(car) #for vif
library(multcomp) # for ghlv 

# load with factors/categories
ins_cat <- read.csv2("ins.csv", sep = ',',
                     stringsAsFactors = TRUE)

# load as is 
ins <- read.csv2("ins.csv", sep = ',')

str(ins)
head(ins)

# changing types 
# new column! claim_num 
ins$claim_num <- as.numeric(ins$claim)
ins$smoker <- as.factor(ins$smoker)
ins$sex <- as.factor(ins$sex)
ins$bmi <- as.numeric(ins$bmi)
#new column hereditary_diseases_facto
ins$hereditary_diseases_factor <- as.factor(ins$hereditary_diseases)
ins$diabetes <- as.factor(ins$diabetes)
ins$regular_ex <- as.factor(ins$regular_ex)
#new column job_title_factor
ins$job_title_factor <- as.factor(ins$job_title)

#unique(ins$hereditary_diseases_factor)

#todo check on NA

# graph investigation
# pairs with numeric variables
pairs(claim_num ~ age+weight+bmi+bloodpressure ,
      data = ins,
      pch = ".",
      upper.panel = panel.smooth)

# first plots

ggplot(ins, 
       mapping = aes(x = age,
                          y = claim_num,
                          color = smoker)) +
  geom_point() +
  geom_smooth()


ggplot(ins, 
       mapping = aes(x = age,
                     y = claim_num,
                     color = smoker)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(. ~ smoker) 


# Linear Regression 

# First attempt, with age, bmi and 0 and 1 factors

lm_age_bmi_smoker_regex_diabetes_ex <-  lm(claim_num ~ age + bmi + regular_ex + smoker + diabetes, data = ins )

summary(lm_age_bmi_smoker_regex_diabetes_ex)

# we have quite high R squared, 71% percentages of observations could be explained by this model
#toask isit really a good model? how to assess it
#todo divide ds to train and test and assess performance 

plot(x = ins$bmi, y = ins$claim_num, col = ins$smoker)

lines(lm_age_bmi_smoker_regex_diabetes_ex$fitted.values) # not correct


plot(x = ins$age, y = ins$claim_num, col = ins$smoker)
lines(ins$age, lm_age_bmi_smoker_regex_diabetes_ex$fitted.values) # not correct

abline(lm_age_bmi_smoker_regex_diabetes_ex, col = "red") # maybe not correct 




#drop 
drop1(lm_age_bmi_smoker_regex_diabetes_ex, test = 'F')

# p-value small => all variiables seem to be significant 


# Checking for collinearity 

vif(lm_age_bmi_smoker_regex_diabetes_ex)
# no large numbers therefore there are not explicit collinearity 

# Another LM with almost all the variables:  

lm2 <- update(lm_age_bmi_smoker_regex_diabetes_ex, .~. + weight + bloodpressure + job_title_factor + hereditary_diseases_factor)

summary(lm2)
# it has slightly better r-squared about 76%, but is it better? 
# not all of categories are significant, lets check with drop1: 
drop1(lm2, test = 'F')

# seems that everything is significant and nothing to remove 

# if we compare two models with anova 

anova(lm_age_bmi_smoker_regex_diabetes_ex, lm2)
# anova analysis shows strong evidences of that second model is differ from first one

#side note: which hereditary disease play role? 

boxplot(claim_num ~ hereditary_diseases_factor, data = ins)

# does the claim differ depending on disease at all. Tukey test for that 

test.tukey <- glht(lm2,
                      linfct = mcp(hereditary_diseases_factor = "Tukey"))

summary(test.tukey)



vif(lm2)

# as we can see there are no numbers larger than 10, but there are some with above 5, 
# so we are not sure if there are no collinearity issues 

#todo remove job title and remove or change to 0/1 hereditary_diseases_factor
#toask should we remove variables with numbers from 5 to 10? 
#toask how to remove it if only for some factors this is the case


# gam 

# gam basic variables 

gam1 <- gam(claim_num ~ s(age) + s(bmi) +s(weight) 
               +s(bloodpressure), data = ins)

summary(gam1)

plot(gam1, residuals = T, page = 1, shade = T)

# the gam with quasipoisson

gamq <- gam(claim_num ~ s(age) + s(bmi) +s(weight) +s(bloodpressure), 
            data = ins, family = 'quasipoisson')

summary(gamq)

plot(gamq, residuals = T, page = 1, shade = T) # seems something is not correct


#how to obtain factors as a complain, example (not relevant code!)
bliss$conc.asFactor <- cut(bliss$conc, breaks = c(-Inf,0,2,4),
                           labels = c("no insecticide", "low conc", "high conc"))
levels(bliss$conc.asFactor)

# GLM

glm_pois_age_bmi_smoker_regex_diabetes_ex <- glm(claim_num ~ age + bmi + regular_ex + smoker + diabetes, data = ins, 
                                                 family = 'poisson')

summary(glm_pois_age_bmi_smoker_regex_diabetes_ex)

#here is some problems with overdispersion, so let's try another quasipoisson family

glm_qpois_age_bmi_smoker_regex_diabetes_ex <- glm(claim_num ~ age + bmi + regular_ex + smoker + diabetes, data = ins, 
                                                  family = 'quasipoisson')
 
summary(glm_qpois_age_bmi_smoker_regex_diabetes_ex)
# 
# plot(x = ins$bmi, y = ins$claim_num, col = ins$smoker)
# lines(lm_age_bmi_smoker_regex_diabetes_ex$fitted.values)
# 
# plot(x = ins$age, y = ins$claim_num, col = ins$smoker)
# lines(lm_age_bmi_smoker_regex_diabetes_ex$fitted.values)
# 
# lm_age_bmi_smoker_regex_diabetes_ex$fitted.values

# below are just notes: 

library(ggcorrplot)
x <- cor(na.omit(ins[,c('claim_num', 'weight', 'age', 'bmi', "bloodpressure")]), method = "pearson")
ggcorrplot(x, colors = c("red","white","blue"),
           title = "Correlation Matrix")

