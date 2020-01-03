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
#Read TRAIN and TESTCSV into R
train_crime_rate_data <- read.csv(file="/Users/admin/Documents/Queens_Masters_Courses/CISC873/Assignment_GLM/Dataset\ 2/873groupAset2crime/CA_Crime_Rate_Train.csv", header=TRUE, sep=",")
#test_crime_rate_data <- read.csv(file="/Users/admin/Documents/Queens_Masters_Courses/CISC873/Assignment_GLM/Dataset\ 2/873groupAset2crime/CA_Crime_Rate_Test.csv", header=TRUE, sep=",")

############## Looking for NAs in rows for target variables ###########
# Robbery rate
counter <- 1
for(i in c(1:length(train_crime_rate_data$Robbery_rate))) {
  
  if(is.na(train_crime_rate_data$Robbery_rate[i]) == TRUE){
    print(counter)
    print(train_crime_rate_data$Robbery_rate[i])
  }
  counter <- counter + 1
}

# Property crime rate
counter <- 1
for(i in c(1:length(train_crime_rate_data$Property_crime_rate))) {

  if(is.na(train_crime_rate_data$Property_crime_rate[i]) == TRUE){
    print(counter)
    print(train_crime_rate_data$Property_crime_rate[i])
  }
  counter <- counter + 1
}

# Burglary rate
counter <- 1
for(i in c(1:length(train_crime_rate_data$Burglary_rate))) {
  
  if(is.na(train_crime_rate_data$Burglary_rate[i]) == TRUE){
    print(counter)
    print(train_crime_rate_data$Burglary_rate[i])
  }
  counter <- counter + 1
}

# Larceny crime rate
counter <- 1
for(i in c(1:length(train_crime_rate_data$Larceny_theft_rate))) {
  
  if(is.na(train_crime_rate_data$Larceny_theft_rate[i]) == TRUE){
    print(counter)
    print(train_crime_rate_data$Larceny_theft_rate[i])
  }
  counter <- counter + 1
}

# Motor_vehicle rate
counter <- 1
for(i in c(1:length(train_crime_rate_data$Motor_vehicle_theft_rate))) {
  
  if(is.na(train_crime_rate_data$Motor_vehicle_theft_rate[i]) == TRUE){
    print(counter)
    print(train_crime_rate_data$Motor_vehicle_theft_rate[i])
  }
  counter <- counter + 1
}

# Remove row 442 because NA
train_crime_rate_data <- train_crime_rate_data[-c(442),]

#Count how many missing days
max(colSums(is.na(train_crime_rate_data)))

#Remove city ID
train_crime_rate_data$City <- NULL
train_crime_rate_data$Percent.RACE....Native.Hawaiian.and.Other.Pacific.Islander <- NULL
sum(is.na(train_crime_rate_data$Percent.RACE....Native.Hawaiian.and.Other.Pacific.Islander))

#Change integer features to numeric
lapply(train_crime_rate_data,class)
train_crime_rate_data <- train_crime_rate_data %>%
  mutate (
    Population = as.numeric(Population),
    Total.law_enforcement_employees = as.numeric(Total.law_enforcement_employees),
    Law_enforcement_Total.officers.emloyee = as.numeric(Total.law_enforcement_employees),
    Total.civilians.employee = as.numeric(Total.civilians.employee),
    Estimate..INCOME.AND.BENEFITS..IN.2012.INFLATION.ADJUSTED.DOLLARS....Per.capita.income..dollars. = as.numeric(Estimate..INCOME.AND.BENEFITS..IN.2012.INFLATION.ADJUSTED.DOLLARS....Per.capita.income..dollars.),
    Estimate..INCOME.AND.BENEFITS..IN.2012.INFLATION.ADJUSTED.DOLLARS....Mean.family.income..dollars. = as.numeric(Estimate..INCOME.AND.BENEFITS..IN.2012.INFLATION.ADJUSTED.DOLLARS....Mean.family.income..dollars.),
    Estimate..INCOME.AND.BENEFITS..IN.2012.INFLATION.ADJUSTED.DOLLARS....Median.family.income..dollars. = as.numeric(Estimate..INCOME.AND.BENEFITS..IN.2012.INFLATION.ADJUSTED.DOLLARS....Median.family.income..dollars.),
    Year = as.numeric(Year)
  )

test_crime_rate_data <- test_crime_rate_data %>%
  mutate (
    Population = as.numeric(Population),
    Total.law_enforcement_employees = as.numeric(Total.law_enforcement_employees),
    Law_enforcement_Total.officers.emloyee = as.numeric(Total.law_enforcement_employees),
    Total.civilians.employee = as.numeric(Total.civilians.employee),
    Estimate..INCOME.AND.BENEFITS..IN.2012.INFLATION.ADJUSTED.DOLLARS....Per.capita.income..dollars. = as.numeric(Estimate..INCOME.AND.BENEFITS..IN.2012.INFLATION.ADJUSTED.DOLLARS....Per.capita.income..dollars.),
    Estimate..INCOME.AND.BENEFITS..IN.2012.INFLATION.ADJUSTED.DOLLARS....Mean.family.income..dollars. = as.numeric(Estimate..INCOME.AND.BENEFITS..IN.2012.INFLATION.ADJUSTED.DOLLARS....Mean.family.income..dollars.),
    Estimate..INCOME.AND.BENEFITS..IN.2012.INFLATION.ADJUSTED.DOLLARS....Median.family.income..dollars. = as.numeric(Estimate..INCOME.AND.BENEFITS..IN.2012.INFLATION.ADJUSTED.DOLLARS....Median.family.income..dollars.),
    Year = as.numeric(Year)
  )

########Check for missing completely at random (MCAR) ##########
pMiss <- function(train_crime_rate_data){sum(is.na(train_crime_rate_data))/length(train_crime_rate_data)*100}
apply(train_crime_rate_data,2,pMiss)
length(subset(apply(train_crime_rate_data,1,pMiss),apply(train_crime_rate_data,1,pMiss) > 10))


##### IMPUTATION using CART #########
init = mice(train_crime_rate_data, maxit=0)
meth = init$method
predM = init$predictorMatrix

#If missing more than 10% data, then we impute. We use CART (Classification and regression trees) as the imputation method, since it is not stochastic 
#and the default imputation methods involve linear regression, where X matrix cannot be inverted.
#The mice package implements a method to deal with missing data. The package creates multiple
#imputations (replacement values) for multivariate missing data 
imputed_data = mice(train_crime_rate_data, m = 5, method='cart', predictorMatrix=predM, seed=103)
original_imputed <- complete(imputed_data)
#Create new dataset 
imputed <- complete(imputed_data)
#Check for missing values
sapply(imputed, function(x) sum(is.na(x)))
#Write to csv
write.csv(imputed,'/Users/admin/Documents/Queens_Masters_Courses/CISC873/Assignment_GLM/Dataset\ 2/imputed_train_crime_rate_data.csv')

# Look at new data frame's structure
str(imputed)

# Remove dependent variables from dataframe except Robbery
imputed$Property_crime_rate <- NULL
imputed$Burglary_rate <- NULL
imputed$Larceny_theft_rate <- NULL
imputed$Motor_vehicle_theft_rate <- NULL

# Random sampling 80/20 train test split
sample_size <- floor(0.80 * nrow(imputed))
set.seed(123)
train_index <- sample(seq_len(nrow(imputed)), size = sample_size)
train_data <- imputed[train_index,]
test_data <- imputed[-train_index,]


for(i in c(1:length(original_imputed))) {
  for(j in c(1:length(original_imputed))) {
    
  }
}

# Train x model
lm_robbery_fit <- lm(train_data$Robbery_rate~.,train_data)

#Evaluate fit of multiple regression model
coefficients(lm_robbery_fit)
confint(lm_robbery_fit, level = 0.95)
fitted(lm_robbery_fit)
residual(lm_robbery_fit)
View(anova(lm_robbery_fit))
vcov(lm_robbery_fit)
influence(lm_robbery_fit)
summary(lm_robbery_fit)$r.squared

corr_data <- original_imputed[c(26,27,28,)]
corr_of_data_1 <- cor(corr_data)
ggcorrplot(corr_of_data_1, method='circle')  

plot()
original_data$

layout(matrix(c(1,2,3,4),2, 2))
plot(lm_robbery_fit)
new_data <- train_data[c(1,2,3,4,5,6)]
ggcorrplot(cor(new_data),method='circle')
#corrplot(cor(new_data), method= 'circle')
###### Population vs total enfocement employees
cor(train_data$Population,train_data$Total.law_enforcement_employees)

plot(log(train_data$Population),log(train_data$Total.law_enforcement_employees),main="Population vs Total Law Enforcement Employees",xlab="",ylab="",axes=FALSE, pch=2,lwd=2,col="forestgreen")
magaxis(side=1,xlab = "Population",tcl=0.8,cex.lab=1.4)
magaxis(side=2,ylab = "Total Law Enforcement Employees",tcl=0.8,cex.lab=1.4)
abline(lm(log(train_data$Total.law_enforcement_employees)~log(train_data$Population)),lwd=3,col="red")
box(lty = 'solid',col = 'black',lwd = 4)


library(corrplot)
corrplot(cor(new_df), method="circle")
# Predict with test data
predictions = predict.lm(lm_robbery_fit, test_data)


#plot predictor variable vs predicted
plot(test_data$Total.civilians.employee,test_data$Robbery_rate, xlab="Predictor variable",ylab="Predicted variable");
points(predictions, test_data$Robbery_rate, col="blue")
#glm_robbery <- glm(train_data$Robbery_rate~., family=gaussian((link="identity")), data=train_data)
summary(lm_robbery_fit)$coefficients
summary(lm_robbery_fit)$r.squared
# R-squared 0.783 achieved


#
# Test x model
prediction <- predict(glm_robbery, test_data)

# Calculate Mean Absolute Error
mae <- function(err)
{
  mean(abs(error))
}

plot(imput., type='l')

glm_property <- glm(train_data$Property_crime_rate~., family=gaussian((link="identity")), data=imputed_robbery_rate)
glm_burglary <- glm(train_data$Burglary_rate~., family=gaussian((link="identity")), data=imputed_robbery_rate)
glm_fit4 <- glm(train_data$Lar~., family=gaussian((link="identity")), data=imputed_robbery_rate)
glm_fit5 <- glm(imputed_robbery_rate$Robbery_rate~., family=gaussian((link="identity")), data=imputed_robbery_rate)



#Number of multiple imputations (n)
#If missing more than 10% data, then we impute. We use CART (Classification and regression trees) as the imputation method, since it is not stochastic 
#and the default imputation methods involve linear regression, where X matrix cannot be inverted.
#The mice package implements a method to deal with missing data. The package creates multiple
#imputations (replacement values) for multivariate missing data 
#imputed_Data <- mice(train_crime_rate_data, m=5, maxit = 50, method = 'cart', seed = 500)

# Correlation analysis
corr <- cor(imputed)
ggcorrplot(corr)

# Create 5 models

# Robbery
fit2 <- glm(train_x$Churn ~ .,data=train_x,family=binomial((link="logit")))


glm_fit <- glm(imputed_robbery_rate$Robbery_rate~., family=gaussian((link="identity")), data=imputed_robbery_rate)

predict(glm_fit, test_crime_rate_data, interval="prediction", na.action=na.pass)
pdata <- predict(glm_fit, newdata = test_crime_rate_data, type="response")

new_df <- as.data.frame(imputed[,c(2,3,4,5,6)],xlab="Robbery Rate")
hist(new_df$Robbery_rate)
corr <- cor(new_df)

ggcorrplot(corr)

prediction=predict(glm_fit,type=c("response"))

robbery_rate_anova <- anova(glm_fit, test='Chisq')
View(robbery_rate_anova)

ggcorrplot(corr, method = "circle")
#churn_data$prediction=prediction
roc_obj <- roc(test_crime_rate_data, prediction)
auc(roc_obj)
roc_obj <- roc(test_x, pdata)
auc(roc_obj)
#Create confusion matrix
df_cm <- confusionMatrix(data = as.factor(as.numeric(pdata>0.5) + 1), reference = as.factor(as.numeric(test_x$Churn)))




