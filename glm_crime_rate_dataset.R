library(Amelia)
library(pROC)
library(reshape2)
library(ROCR)
library(caret)
library(rsample)
library(ggplot2)
library(ggcorrplot)
library(mice)
library(LaF)
library(BaylorEdPsych)
library(MissMech)
library(VIM)
library(dplyr)
library(mltools)

library(glmnet)

# Load original data
original_data <- read.csv(file="/Users/admin/Documents/Queens_Masters_Courses/CISC873/Assignment_GLM/Dataset\ 2/873groupAset2crime/Imputed_Train.csv", header=TRUE, sep=",")
single_col_data <- read.csv(file="/Users/admin/Documents/Queens_Masters_Courses/CISC873/Assignment_GLM/Dataset\ 2/873groupAset2crime/percentiles_singlecol_imputed_train.csv", header=TRUE, sep=",")
multi_col_bin_class_data <- read.csv(file="/Users/admin/Documents/Queens_Masters_Courses/CISC873/Assignment_GLM/Dataset\ 2/873groupAset2crime/percentiles_onehot_imputed_train.csv", header=TRUE, sep=",")

plot(original_data$Percent..BEDROOMS...4.bedrooms,original_data$Robbery_rate)
#### Model for Class 1 and 4 ####
# Filter data
new_data <- as.data.frame(subset(single_col_data, single_col_data$robbery == 1 
                                 | single_col_data$robbery == 4))
new_data <- new_data[,-c(3:6)]
new_data$City <- NULL
new_data$robbery <- as.factor(as.numeric(as.factor(new_data$robbery)) - 1)
new_data$Year <- as.factor(new_data$Year - 1)

# Random sampling 80/20 train test split
sample_size <- floor(0.80 * nrow(new_data))
set.seed(123)
train_index <- sample(seq_len(nrow(new_data)), size = sample_size)
train_data <- new_data[train_index,]
test_data <- new_data[-train_index,]

# Train Logistic Regression (GLM) model
glm_fit <- glm(train_data$robbery~.,data=train_data,family=binomial((link="logit")))

# Prediction against test data
predicted <- predict(glm_fit, test_data, type = "response")
test_data$pred <- ifelse(predict(glm_fit, test_data, type = "response") > 0.5,1,0)

# Performance metrics
pred_table <- table(test_data$pred,test_data$robbery)
df_cm <- confusionMatrix(data = as.factor(test_data$pred), reference = test_data$robbery)
roc_obj <- roc(test_data$robbery, predicted)
auc(roc_obj)

# Important Features
anova(glm_fit, test = 'Chisq')

#### Model for Class 2 and 3 ####
new_data <- single_col_data[,c("robbery","Percent_VEHICLESAVAILABLE_2VehiclesAvailable",
                               "Percent_SEXANDAGE_Female"                                       ,
                               "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___15_",
                               "Percent_VEHICLESAVAILABLE_3OrMoreVehiclesAvailable"             ,
                               "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___10_",
                               "PercentRACE_White"                                              ,
                               "PercentRACE_BlackOrAfricanAmerican"                             ,
                               "Widowed_PercentOfPopulation15YearsAndOver"                      ,
                               "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___200",
                               "Percent_HOUSINGTENURE_Renter_occupied"                          ,
                               "PercentOfAgeGroup18To24WithHighSchoolGraduate_includesEquivalen",
                               "Percent_BEDROOMS_3Bedrooms"                                     ,
                               "Percent_OCCUPATION_ServiceOccupations"                          ,
                               "Percent_GROSSRENT_LessThan_200"                                 ,
                               "Percent_INDUSTRY_EducationalServices_AndHealthCareAndSocialAssi",
                               "Percent_PERCENTAGEOFFAMILIESANDPEOPLEWHOSEINCOMEINTHEPAST12MONT",
                               "PercentOfAgeGroupAbove25YrsWithHighSchoolGraduate_includesEquiv")]
# Filter data

new_data <- as.data.frame(subset(new_data, new_data$robbery == 2 | new_data$robbery == 3))
new_data[c(2:18)] <- lapply(new_data[c(2:18)], function(x) c(scale(x)))
new_data$City <- NULL
new_data$robbery <- as.factor(as.numeric(as.factor(new_data$robbery)) - 1)
new_data$Year <- as.factor(new_data$Year - 1)
#new_data <- new_data[,-c(3:6)]

# Random sampling 80/20 train test split
sample_size <- floor(0.80 * nrow(new_data))
set.seed(123)
train_index <- sample(seq_len(nrow(new_data)), size = sample_size)
train_data <- new_data[train_index,]
test_data <- new_data[-train_index,]

# Train Logistic Regression (GLM) model
glm_fit <- glm(train_data$robbery~.,data=train_data,family=binomial((link="logit")))

# Prediction against test data
predicted <- predict(glm_fit, test_data, type = "response")
test_data$pred <- ifelse(predict(glm_fit, test_data, type = "response") > 0.5,1,0)

# Performance metrics
pred_table <- table(test_data$pred,test_data$robbery)
df_cm <- confusionMatrix(data = as.factor(test_data$pred), reference = test_data$robbery)
roc_obj <- roc(test_data$robbery, predicted)
auc(roc_obj)

# Important features
anova(glm_fit, test = 'Chisq')


#### Model for Class 1 and 2 vs 3 and 4 ####
# Filter data
new_data <- new_df
new_data$robbery <- ifelse(single_col_data$robbery == 1 | single_col_data$robbery == 2,1,0)
new_data <- new_data[,-c(3:6)]
new_data$City <- NULL
new_data$robbery <- as.factor(new_data$robbery)
new_data$Year <- as.factor(new_data$Year - 1)

# Random sampling 80/20 train test split
sample_size <- floor(0.80 * nrow(new_data))
set.seed(123)
train_index <- sample(seq_len(nrow(new_data)), size = sample_size)
train_data <- new_data[train_index,]
test_data <- new_data[-train_index,]

# Train Logistic Regression (GLM) model
glm_fit <- glm(train_data$robbery~.,data=train_data,family=binomial((link="logit")))

# Prediction against test data
predicted <- predict(glm_fit, test_data, type = "response")
test_data$pred <- ifelse(predict(glm_fit, test_data, type = "response") > 0.5,1,0)

# Performance metrics
pred_table <- table(test_data$pred,test_data$robbery)
df_cm <- confusionMatrix(data = as.factor(test_data$pred), reference = test_data$robbery)
roc_obj <- roc(test_data$robbery, predicted)
auc(roc_obj)

# Important features
anova(glm_fit, test = 'Chisq')

####### Model with important features #######
# Filter data
new_df <- single_col_data[,c("robbery","percentage_Total__3_personHousehold_",                           
                               "PercentOfHoudseholdWithChildrenUnder18_AGEOFOWNCHILDREN_Under6Y",
                               "Percent_SEXANDAGE_Female",                                       
                               "Percent_GROSSRENT__750To_999",                                   
                               "Percent_UNITSINSTRUCTURE_MobileHome",                            
                               "Percent_INDUSTRY_Manufacturing",                                 
                               "Percent_INDUSTRY_RetailTrade",                                   
                               "Percent_INDUSTRY_Arts_Entertainment_AndRecreation_AndAccommodat",
                               "Percent_SEXANDAGE_25To34Years",                                  
                               "PercentOfHoudseholdWithChildrenUnder18_AGEOFOWNCHILDREN_6To17Ye",
                               "CivilianEmployedPopulation16YearsAndOver_PrivateWageAndSalaryWo",
                               "Percent_GROSSRENT__1_000To_1_499",                               
                               "Percent_GROSSRENT__500To_749",                                   
                               "Estimate_HOUSINGOCCUPANCY_RentalVacancyRate",                    
                               "Percent_OCCUPATION_SalesAndOfficeOccupations",                   
                               "Percent_SEXANDAGE_Male",                                         
                               "CivilianEmployedPopulation16YearsAndOver_Self_employedWorkersIn",
                               "Percent_BEDROOMS_3Bedrooms",                                     
                               "Percent_BEDROOMS_2Bedrooms",                                     
                               "NeverMarried_PercentOfPopulation15YearsAndOver","Year")]

new_df <- single_col_data[,c("robbery","Percent_VEHICLESAVAILABLE_2VehiclesAvailable"                   ,"PercentRACE_BlackOrAfricanAmerican"                             ,"Percent_VEHICLESAVAILABLE_NoVehiclesAvailable"                  ,"PercentRACE_White"                                              ,"NowMarried_exceptSeparated__PercentOfPopulation15YearsAndOver"  ,"TotalLaw_enforcement_employees"                                 ,"Law_enforcement_TotalOfficersEmloyee"                           ,"PercentOfAgeGroupAbove25YrsWithBachelor_sDegree"                ,"Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___10_","Population"                                                     ,"percentage_Total__1_personHousehold_"                           ,"Percent_BEDROOMS_4Bedrooms"                                     ,"Estimate_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS__Med","Percent_VEHICLESAVAILABLE_1VehicleAvailable"                    ,"PercentOfAgeGroupAbove25YrsWith9thTo12thGrade_NoDiploma"        ,"Percent_PERCENTAGEOFFAMILIESANDPEOPLEWHOSEINCOMEINTHEPAST12MONT","Percent_HOUSINGOCCUPANCY_VacantHousingUnits"                    ,"Percent_HOUSINGOCCUPANCY_OccupiedHousingUnits"                  ,"Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___100")]
new_df <- single_col_data[,c("robbery","Percent_VEHICLESAVAILABLE_NoVehiclesAvailable"                  ,
                             "PercentRACE_BlackOrAfricanAmerican"                             ,
                             "PercentRACE_White"                                              ,
                             "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___10_",
                             "Percent_BEDROOMS_4Bedrooms"                                     ,
                             "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS__Less",
                             "Percent_HOUSINGTENURE_Renter_occupied"                          ,
                             "PercentOfAgeGroupAbove25YrsWith9thTo12thGrade_NoDiploma"        ,
                             "Percent_VEHICLESAVAILABLE_3OrMoreVehiclesAvailable"             ,
                             "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___15_",
                             "TotalCiviliansEmployee"                                         ,
                             "Percent_HOUSINGOCCUPANCY_VacantHousingUnits"                    ,
                             "Separated_PercentOfPopulation15YearsAndOver"                    ,
                             "PercentOfAgeGroupAbove25YrsWithHighSchoolGraduate_includesEquiv",
                             "PercentOfAgeGroupAbove25YrsWithLessThan9thGrade"                ,
                             "Percent_BEDROOMS_5OrMoreBedrooms"                               ,
                             "Percent_OCCUPATION_ServiceOccupations")]
# SO FAR THE WINNER 0.955 auc
new_df <- single_col_data[,c("robbery","Percent_VEHICLESAVAILABLE_NoVehiclesAvailable"                  ,
                             "PercentRACE_BlackOrAfricanAmerican"                             ,
                             "PercentRACE_White"                                              ,
                             "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___10_",
                             "Percent_BEDROOMS_4Bedrooms"                                     ,
                             "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS__Less",
                             "Percent_HOUSINGTENURE_Renter_occupied"                          ,
                             "PercentOfAgeGroupAbove25YrsWith9thTo12thGrade_NoDiploma"        ,
                             "Percent_VEHICLESAVAILABLE_3OrMoreVehiclesAvailable"             ,
                             "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___15_")]

# Scale columns except target variable
new_df[c(2:11)] <- lapply(new_df[c(2:11)], function(x) c(scale(x)))

new_df <- as.data.frame(subset(new_df, new_df$robbery == 1 | new_df$robbery == 4))
new_df$robbery <- as.factor(as.numeric(as.factor(new_df$robbery)) - 1)
new_df$Year <- as.factor(new_df$Year - 1)

# Sampling 80/20 train test split
sample_size <- floor(0.80 * nrow(new_df))
set.seed(123)
train_index <- sample(seq_len(nrow(new_df)), size = sample_size)
train_data <- new_df[train_index,]
test_data <- new_df[-train_index,]

# Train Logistic Regression (GLM) model
glm_fit <- glm(train_data$robbery~.,data=train_data,family=binomial((link="logit")))

# Prediction against test data
predicted <- predict(glm_fit, test_data, type = "response")
test_data$pred <- ifelse(predict(glm_fit, test_data, type = "response") > 0.5,1,0)

# Performance metrics
pred_table <- table(test_data$pred,test_data$robbery)
df_cm <- confusionMatrix(data = as.factor(test_data$pred), reference = test_data$robbery)

tp <- df_cm$table[1,1]
fp <- df_cm$table[1,2]
fn <- df_cm$table[2,1]
precision <- tp / (tp + fp)
precision
recall <- tp/(tp+fn)
recall
f_measure <- 2*((precision*recall)/(precision+recall))
f_measure
mcc(test_data$robbery, as.factor(test_data$pred))

roc_obj <- roc(test_data$robbery, predicted)
auc(roc_obj)

# Important features
anova(glm_fit, test = 'Chisq')

# Model with Ridge logistic regression
# Filter data
new_df <- single_col_data[,c("robbery","percentage_Total__3_personHousehold_",                           
                             "PercentOfHoudseholdWithChildrenUnder18_AGEOFOWNCHILDREN_Under6Y",
                             "Percent_SEXANDAGE_Female",                                       
                             "Percent_GROSSRENT__750To_999",                                   
                             "Percent_UNITSINSTRUCTURE_MobileHome",                            
                             "Percent_INDUSTRY_Manufacturing",                                 
                             "Percent_INDUSTRY_RetailTrade",                                   
                             "Percent_INDUSTRY_Arts_Entertainment_AndRecreation_AndAccommodat",
                             "Percent_SEXANDAGE_25To34Years",                                  
                             "PercentOfHoudseholdWithChildrenUnder18_AGEOFOWNCHILDREN_6To17Ye",
                             "CivilianEmployedPopulation16YearsAndOver_PrivateWageAndSalaryWo",
                             "Percent_GROSSRENT__1_000To_1_499",                               
                             "Percent_GROSSRENT__500To_749",                                   
                             "Estimate_HOUSINGOCCUPANCY_RentalVacancyRate",                    
                             "Percent_OCCUPATION_SalesAndOfficeOccupations",                   
                             "Percent_SEXANDAGE_Male",                                         
                             "CivilianEmployedPopulation16YearsAndOver_Self_employedWorkersIn",
                             "Percent_BEDROOMS_3Bedrooms",                                     
                             "Percent_BEDROOMS_2Bedrooms",                                     
                             "NeverMarried_PercentOfPopulation15YearsAndOver","Year")]

new_df <- single_col_data[,c("Percent_VEHICLESAVAILABLE_2VehiclesAvailable"                   ,"PercentRACE_BlackOrAfricanAmerican"                             ,"Percent_VEHICLESAVAILABLE_NoVehiclesAvailable"                  ,"PercentRACE_White"                                              ,"NowMarried_exceptSeparated__PercentOfPopulation15YearsAndOver"  ,"TotalLaw_enforcement_employees"                                 ,"Law_enforcement_TotalOfficersEmloyee"                           ,"PercentOfAgeGroupAbove25YrsWithBachelor_sDegree"                ,"Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___10_","Population"                                                     ,"percentage_Total__1_personHousehold_"                           ,"Percent_BEDROOMS_4Bedrooms"                                     ,"Estimate_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS__Med","Percent_VEHICLESAVAILABLE_1VehicleAvailable"                    ,"PercentOfAgeGroupAbove25YrsWith9thTo12thGrade_NoDiploma"        ,"Percent_PERCENTAGEOFFAMILIESANDPEOPLEWHOSEINCOMEINTHEPAST12MONT","Percent_HOUSINGOCCUPANCY_VacantHousingUnits"                    ,"Percent_HOUSINGOCCUPANCY_OccupiedHousingUnits"                  ,"Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___100")]

new_df <- scale(single_col_data[,c("robbery","Percent_VEHICLESAVAILABLE_NoVehiclesAvailable"                  ,
                             "PercentRACE_BlackOrAfricanAmerican"                             ,
                             "PercentRACE_White"                                              ,
                             "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___10_",
                             "Percent_BEDROOMS_4Bedrooms"                                     ,
                             "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS__Less",
                             "Percent_HOUSINGTENURE_Renter_occupied"                          ,
                             "PercentOfAgeGroupAbove25YrsWith9thTo12thGrade_NoDiploma"        ,
                             "Percent_VEHICLESAVAILABLE_3OrMoreVehiclesAvailable"             ,
                             "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___15_",
                             "TotalCiviliansEmployee"                                         ,
                             "Percent_HOUSINGOCCUPANCY_VacantHousingUnits"                    ,
                             "Separated_PercentOfPopulation15YearsAndOver"                    ,
                             "PercentOfAgeGroupAbove25YrsWithHighSchoolGraduate_includesEquiv",
                             "PercentOfAgeGroupAbove25YrsWithLessThan9thGrade"                ,
                             "Percent_BEDROOMS_5OrMoreBedrooms"                               ,
                             "Percent_OCCUPATION_ServiceOccupations")])
new_df <- as.data.frame(subset(new_df, new_df$robbery == 1 | new_df$robbery == 4))
new_df$robbery <- as.factor(as.numeric(as.factor(new_df$robbery)) - 1)
new_df$Year <- as.factor(new_df$Year - 1)

# Random sampling 80/20 train test split
sample_size <- floor(0.80 * nrow(new_df))
set.seed(123)
train_index <- sample(seq_len(nrow(new_df)), size = sample_size)
train_data <- new_df[train_index,]
test_data <- new_df[-train_index,]

# Build ridge logistic regression model
y_train_std <- as.factor(train_data[,1])
x_train_std <- data.matrix(train_data[,2:22])
cv_train_std <- cv.glmnet(x_train_std, y_train_std, type.measure="class", nfolds=10, family="binomial")

# Fit model with ridge penality
std_ridge_logit <- glmnet(x_train_std, y_train_std, family = "binomial", alpha=0)

# Prediction with training set
lambda <- cv_train_std$lambda.min
SRL_pred_train <- predict(std_ridge_logit, x_train_std, type="class", s = lambda)

# Report mean error rate (fraction of incorrect labels)
conf_matrix_train <- table(y_train_std, SRL_pred_train)
conf_matrix_train
error_rate_train <- (10+15)/201.0
error_rate_train # 0.12

# Load test data and separate ind/dep variables
y_test_std <- as.factor(test_data[,1])
x_test_std <- data.matrix(test_data[,2:22])

# Prediction with test data
SRL_pred_test <- predict(std_ridge_logit, x_test_std, type = "class", s= lambda)
conf_matrix_test <- table(y_test_std, SRL_pred_test)
conf_matrix_test

error_rate_test <- (8 + 2) / 51.0
error_rate_test #0.20

# Log-transformed data
logged_train_data <- log(train_data[,-c(1,22)])
logged_train_data$Percent_UNITSINSTRUCTURE_MobileHome <- NULL
logged_train_data$Estimate_HOUSINGOCCUPANCY_RentalVacancyRate <- NULL
logged_train_data$Year <- train_data$Year
logged_train_data$robbery <- train_data$robbery

y_train_log <- as.factor(logged_train_data[,20])
x_train_log <- data.matrix(logged_train_data[,1:19])

# Cross-validation
cv_train_log <- cv.glmnet(x_train_log, y_train_log, type.measure = "class", nfolds = 10,
                          family = "binomial")

lambda_log <- cv_train_log$lambda.min
lambda_log

# Fit the model
log_ridge_logit <- glmnet(x_train_log, y_train_log, family = "binomial", alpha = 0)

# Predicting with the log training set
logRL_pred_train <- predict(log_ridge_logit, x_train_log, type = "class", s = lambda_log)
log_conf_matrix_train <- table(y_train_log, logRL_pred_train)
log_conf_matrix_train

# Error rate
log_error_train <- (12 + 18) / 201.0
log_error_train # 0.15

# Logged test data
logged_test_data <- log(test_data[,-c(1,22)])
logged_test_data$Percent_UNITSINSTRUCTURE_MobileHome <- NULL
logged_test_data$Estimate_HOUSINGOCCUPANCY_RentalVacancyRate <- NULL
logged_test_data$Year <- test_data$Year
logged_test_data$robbery <- test_data$robbery

y_test_log <- as.factor(logged_test_data[,20])
x_test_log <- data.matrix(logged_test_data[,1:19])

# Predicting with LOG test set
logRL_pred_test <- predict(log_ridge_logit, x_test_log, type = "class", s = lambda)
log_conf_matrix_test <- table(y_test_log, logRL_pred_test)
log_conf_matrix_test

# Error rate
log_error_test <- (9 + 6) / 51.0
log_error_test #0.29

##################################

# Year 1 and Year 2 Data Separation
data <- single_col_data[,c("robbery","Year","Percent_VEHICLESAVAILABLE_NoVehiclesAvailable"                  ,
                                     "PercentRACE_BlackOrAfricanAmerican"                             ,
                                     "PercentRACE_White"                                              ,
                                     "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___10_",
                                     "Percent_BEDROOMS_4Bedrooms"                                     ,
                                     "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS__Less",
                                     "Percent_HOUSINGTENURE_Renter_occupied"                          ,
                                     "PercentOfAgeGroupAbove25YrsWith9thTo12thGrade_NoDiploma"        ,
                                     "Percent_VEHICLESAVAILABLE_3OrMoreVehiclesAvailable"             ,
                                     "Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___15_",
                                     "TotalCiviliansEmployee"                                         ,
                                     "Percent_HOUSINGOCCUPANCY_VacantHousingUnits"                    ,
                                     "Separated_PercentOfPopulation15YearsAndOver"                    ,
                                     "PercentOfAgeGroupAbove25YrsWithHighSchoolGraduate_includesEquiv",
                                     "PercentOfAgeGroupAbove25YrsWithLessThan9thGrade"                ,
                                     "Percent_BEDROOMS_5OrMoreBedrooms"                               ,
                                     "Percent_OCCUPATION_ServiceOccupations")]

df_year1 <- as.data.frame(subset(data, Year == 1))



df_year2 <- as.data.frame(subset(data, Year == 2))

# Filter data Year 1
df_year1 <- as.data.frame(subset(df_year1, df_year1$robbery == 1 
                                 | df_year1$robbery == 4))

df_year2 <- as.data.frame(subset(df_year2, df_year2$robbery == 1 
                                 | df_year2$robbery == 4))

df_year2 <- as.data.frame(subset(df_year1, df_year1$robbery == 2 | df_year1$robbery == 3))
df_year2 <- as.data.frame(subset(df_year2, df_year2$robbery == 2 | df_year2$robbery == 3))


df_year1$robbery <- ifelse(df_year1$robbery == 1 | df_year1$robbery == 2,1,0)
df_year2$robbery <- ifelse(df_year2$robbery == 1 | df_year2$robbery == 2,1,0)

#Year 1
#df_year1 <- df_year1[,-c(3:6)]
df_year1$City <- NULL
df_year1$Year <- NULL
df_year1$robbery <- as.factor(as.numeric(as.factor(df_year1$robbery)) - 1)
df_year1[c(2:18)] <- lapply(df_year1[c(2:18)], function(x) c(scale(x)))
#Year 2
df_year2$City <- NULL
df_year2$Year <- NULL
df_year2$robbery <- as.factor(as.numeric(as.factor(df_year2$robbery)) - 1)
df_year2[c(2:18)] <- lapply(df_year2[c(2:18)], function(x) c(scale(x)))

#df_year1$Year <- as.factor(new_data$Year - 1)

# Random sampling 80/20 train test split
sample_size <- floor(0.80 * nrow(df_year2))
set.seed(123)
train_index <- sample(seq_len(nrow(df_year2)), size = sample_size)
train_data <- df_year2[train_index,]
test_data <- df_year2[-train_index,]

# Train Logistic Regression (GLM) model
glm_fit <- glm(train_data$robbery~.,data=train_data,family=binomial((link="logit")))

# Prediction against test data
predicted <- predict(glm_fit, test_data, type = "response")
test_data$pred <- ifelse(predict(glm_fit, test_data, type = "response") > 0.5,1,0)

# Performance metrics
pred_table <- table(test_data$pred,test_data$robbery)
df_cm <- confusionMatrix(data = as.factor(test_data$pred), reference = test_data$robbery)
roc_obj <- roc(test_data$robbery, predicted)
auc(roc_obj)


# Random sampling 80/20 train test split
sample_size <- floor(0.80 * nrow(df_year1))
set.seed(123)
train_index <- sample(seq_len(nrow(df_year1)), size = sample_size)
train_data <- df_year1[train_index,]
test_data <- df_year1[-train_index,]

# Train Logistic Regression (GLM) model
glm_fit <- glm(train_data$robbery~.,data=train_data,family=binomial((link="logit")))

# Prediction against test data
predicted <- predict(glm_fit, test_data, type = "response")
test_data$pred <- ifelse(predict(glm_fit, test_data, type = "response") > 0.5,1,0)

# Performance metrics
pred_table <- table(test_data$pred,test_data$robbery)
df_cm <- confusionMatrix(data = as.factor(test_data$pred), reference = test_data$robbery)
roc_obj <- roc(test_data$robbery, predicted)
auc(roc_obj)
anova(glm_fit,test="Chisq")
