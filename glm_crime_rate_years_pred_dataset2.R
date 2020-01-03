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
single_col_data <- read.csv(file="/Users/admin/Documents/Queens_Masters_Courses/CISC873/Assignment_GLM/Dataset\ 2/873groupAset2crime/percentiles_singlecol_imputed_train.csv", header=TRUE, sep=",")


# Year 1 and Year 2 Data Separation
data <- single_col_data[,c("robbery","Year","Percent_VEHICLESAVAILABLE_NoVehiclesAvailable",
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

df_year1 <- as.data.frame(subset(df_year1, df_year1$robbery == 2 | df_year1$robbery == 3))
df_year2 <- as.data.frame(subset(df_year2, df_year2$robbery == 2 | df_year2$robbery == 3))


df_year1$robbery <- ifelse(df_year1$robbery == 1 | df_year1$robbery == 2,1,0)
df_year2$robbery <- ifelse(df_year2$robbery == 1 | df_year2$robbery == 2,1,0)

#Year 1
#df_year1 <- df_year1[,-c(3:6)]
df_year1$City <- NULL
df_year1$Year <- NULL
df_year1$robbery <- as.factor(as.numeric(as.factor(df_year1$robbery)) - 1)
df_year1[c(2:ncol(df_year1))] <- lapply(df_year1[c(2:ncol(df_year1))], function(x) c(scale(x)))
#Year 2
df_year2$City <- NULL
df_year2$Year <- NULL
df_year2$robbery <- as.factor(as.numeric(as.factor(df_year2$robbery)) - 1)
df_year2[c(2:ncol(df_year2))] <- lapply(df_year2[c(2:ncol(df_year2))], function(x) c(scale(x)))

#df_year1$Year <- as.factor(new_data$Year - 1)
## Year 1
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

## Year 2
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
anova(glm_fit,test="Chisq")

plot(single_col_data$Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___10_,single_col_data$robbery, line='l')
cor(single_col_data$Percent_INCOMEANDBENEFITS_IN2012INFLATION_ADJUSTEDDOLLARS___10_,single_col_data$robbery)
