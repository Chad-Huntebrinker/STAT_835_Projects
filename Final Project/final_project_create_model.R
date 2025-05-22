#Chad Huntebrinker
#Final Project
library(readxl)
library(VGAM)

#First, load the dataset
data_set <- read_excel("Heart_Disease_data.xlsx")
colSums(is.na(data_set))

#Data was pretty spread out, so we needed to break them down into 3 categories instead of 5.
data_set$num3 <- cut(data_set$num,
                     breaks = c(-1, 0, 2, 4),
                     labels = c(0, 1, 2),
                     ordered_result = TRUE)


#Variables that will be included due to the CDC: trestbps, chol, age, sex

#Add the baseline predictors
vglm_model_1 <- vglm(num3 ~ trestbps + chol + age + sex, 
                     family = cumulative(parallel = TRUE), data = data_set)
summary(vglm_model_1)

#Needed to remove chol because the p-value
vglm_model_2 <- vglm(num3 ~ trestbps + age + sex, 
                     family = cumulative(parallel = TRUE), data = data_set)
summary(vglm_model_2)
lrtest(vglm_model_1, vglm_model_2)

#Try adding the other variables

model_cp <- vglm(num3 ~ trestbps + age + sex + cp, 
                 family = cumulative(parallel = TRUE), 
                 data = data_set)

model_fbs <- vglm(num3 ~ trestbps + age + sex + fbs, 
                       family = cumulative(parallel = TRUE), 
                       data = data_set)

model_restecg <- vglm(num3 ~ trestbps + age + sex + restecg, 
                       family = cumulative(parallel = TRUE), 
                       data = data_set)

model_thalach <- vglm(num3 ~ trestbps + age + sex + thalach, 
                      family = cumulative(parallel = TRUE), 
                      data = data_set)

model_exang <- vglm(num3 ~ trestbps + age + sex + exang, 
                    family = cumulative(parallel = TRUE), 
                    data = data_set)

model_oldpeak <- vglm(num3 ~ trestbps + age + sex + oldpeak, 
                      family = cumulative(parallel = TRUE), 
                      data = data_set)

model_slope <- vglm(num3 ~ trestbps + age + sex + slope, 
                      family = cumulative(parallel = TRUE), 
                      data = data_set)

model_ca <- vglm(num3 ~ trestbps + age + sex + ca, 
                 family = cumulative(parallel = TRUE), 
                 data = data_set)

model_thal <- vglm(num3 ~ trestbps + age + sex + thal, 
                      family = cumulative(parallel = TRUE), 
                      data = data_set)

summary(model_cp)
summary(model_fbs) # No good
summary(model_restecg) # No good
summary(model_slope)
summary(model_ca) # No good
summary(model_exang)
summary(model_oldpeak)
summary(model_thalach)
summary(model_thal) # No good

#Model with fbs, restecg, ca, and thal are no good but the other ones seem like good predictors.
#Check the AICs of all of them
AIC(vglm_model_1)
AIC(vglm_model_2)

AIC(model_cp)
AIC(model_slope) #Doesn't have a huge improvement
AIC(model_exang)
AIC(model_oldpeak)
AIC(model_thalach)
#Slope doesn't have a huge drop in AIC, so we're on the fence with it.
#For the sake of keeping the model simple, we'll keep it off the model.

#But the rest of the models improve when we add the new predictor variables.
#So we will try adding them in.

vglm_model_3 <- vglm(num3 ~ trestbps + age + sex + cp + exang + oldpeak + thalach,
                     family = cumulative(parallel = TRUE),
                     data = data_set)
summary(vglm_model_3)
#The p-value for age was greater than 0.05, so let's try removing it from the model.


vglm_model_4 <- vglm(num3 ~ trestbps + sex + cp + exang + oldpeak + thalach,
                     family = cumulative(parallel = TRUE),
                     data = data_set)
summary(vglm_model_4)

AIC(vglm_model_3)
AIC(vglm_model_4)

#The AICs are basically the same, but model 4 (without age) is simpler so we'll keep that

#Next, we'll test interaction between predictors in model_4.
#Specifically, the sex of the individual with it:
vglm_model_interaction <- vglm(num3 ~ sex * trestbps + sex * cp + sex * exang + sex * oldpeak + sex * thalach,
                               family = cumulative(parallel = TRUE),
                               data = data_set)

summary(vglm_model_interaction)
AIC(vglm_model_interaction)
#The AIC is basically the same and the p-values disqualify them. So no interaction between sex and the other
#variables.

#Next we'll move onto how well the model fits and explains the data.