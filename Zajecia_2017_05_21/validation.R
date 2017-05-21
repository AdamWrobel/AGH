
# load packages
library(ggplot2)
library(purrr)
library(dplyr)

# read code provide on previous lecture
source('creditrisk.R')

# read some functions
source('functions.R')

head(trainingData)

varNames <- c('LIMIT_BAL','AGE',
              'BILL_AMT1','BILL_AMT2','BILL_AMT3',
              'BILL_AMT4','BILL_AMT5','BILL_AMT6',
              'PAY_AMT1','PAY_AMT2','PAY_AMT3',
              'PAY_AMT4','PAY_AMT5','PAY_AMT6')

# check used transformation
walk(varNames[c(1:3,9)],
     variableTransformBins_2,
     plot_on = 1)

test <- lapply(varNames,
               variableTransformBins_2, return_full = 1)
test[[1]] %>% head

test[[1]] %>% ggplot() + 
  geom_density(aes(crit.x, group = bin, fill = factor(bin)), adjust = 3)


# multicolinearity 
trainingData %>% select(LIMIT_BAL,contains("BILL_AMT")) %>% cor %>% round(3)
trainingData %>% select(LIMIT_BAL,contains("PAY_AMT")) %>% cor %>% round(3)
trainingData %>% select(LIMIT_BAL,PAY_2,PAY_3,PAY_4,PAY_5,PAY_6) %>% cor %>% round(3)



# check if model is sensitive
compare_result(alpha_input = 0.5)
compare_result(nbin = 5)
compare_result(link_function = 'probit')


# build alternative model
fullModel_alt <- step(fullModel, trace = F)
summary(fullModel_alt)

test_model_input[,"predicted_PD"] <- predict(fullModel,trainingData_transformed,type="response")
test_model_input[,"predicted_PD_alt"] <- predict(fullModel_alt,trainingData_transformed,type="response")

ggplot(test_model_input, aes(x = predicted_PD_alt, y = predicted_PD)) + 
    geom_point() + geom_smooth()

ggplot(test_model_input, aes(predicted_PD_alt-predicted_PD)) + 
    geom_density(fill = 'chartreuse2')

# check categorical variable
trainingData_transformed$SEX %>% table
trainingData_transformed$EDUCATION %>% table
trainingData_transformed$MARRIAGE %>% table
trainingData_transformed$DEFAULT_PAYMENT_NEXT_MONTH %>% table
trainingData_transformed$PAY_0 %>% table

validation_data <- trainingData_transformed %>% 
  mutate(EDUCATION_gr = ifelse(EDUCATION %in% c(0,4,5,6),'other',EDUCATION))

validation_data %>% group_by(EDUCATION) %>% summarize(default_rate = sum(DEFAULT_PAYMENT_NEXT_MONTH)/n())
validation_data %>% group_by(EDUCATION_gr) %>% summarize(default_rate = sum(DEFAULT_PAYMENT_NEXT_MONTH)/n())

validation_data <- validation_data %>% 
  mutate(EDUCATION_gr = ifelse(EDUCATION_gr %in% c(2,3),'high school/university',EDUCATION_gr),
         EDUCATION_gr = ifelse(EDUCATION_gr %in% c(1),'graduate school',EDUCATION_gr))

validation_data %>% group_by(EDUCATION_gr) %>% summarize(default_rate = sum(DEFAULT_PAYMENT_NEXT_MONTH)/n())
validation_data$EDUCATION_gr %>% table

validation_data <- validation_data %>% 
  mutate(MARRIAGE_gr = ifelse(MARRIAGE %in% c(0,3),'other',MARRIAGE),
         MARRIAGE_gr = ifelse(MARRIAGE == 2,'married',MARRIAGE_gr),
         MARRIAGE_gr = ifelse(MARRIAGE == 1,'single',MARRIAGE_gr))

validation_data %>% group_by(MARRIAGE_gr) %>% summarize(default_rate = sum(DEFAULT_PAYMENT_NEXT_MONTH)/n())
validation_data$MARRIAGE_gr %>% table

validation_data <- validation_data %>% 
  mutate(MARRIAGE_gr = ifelse(MARRIAGE_gr %in% c('other','single'),'single & other',MARRIAGE_gr))

validation_data %>% group_by(MARRIAGE_gr) %>% summarize(default_rate = sum(DEFAULT_PAYMENT_NEXT_MONTH)/n())
validation_data$MARRIAGE_gr %>% table

ggplot(validation_data, aes(predicted_PD)) + 
  geom_histogram(aes(y =..density.., fill = factor(SEX)), alpha = 0.5, position = position_identity()) +
  facet_wrap(~EDUCATION_gr)

# estimate the full logit model
fullModel2 <-glm(DEFAULT_PAYMENT_NEXT_MONTH ~ 1 + LIMIT_BAL + SEX + EDUCATION_gr + MARRIAGE_gr +
                   AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 +
                   BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 +
                   PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,
                 family=binomial(link="logit"), data=validation_data)

# model summary
summary(fullModel2)
fullModel3 <- step(fullModel2, trace = F); summary(fullModel3)
