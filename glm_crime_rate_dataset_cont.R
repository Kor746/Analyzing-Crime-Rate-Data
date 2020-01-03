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

motor_data <- read.csv(file="/Users/admin/Documents/Queens_Masters_Courses/CISC873/Assignment_GLM/Dataset\ 2/gopi_data/motor.csv", header=TRUE, sep=",")
burglary_data <- read.csv(file="/Users/admin/Documents/Queens_Masters_Courses/CISC873/Assignment_GLM/Dataset\ 2/gopi_data/burglary.csv", header=TRUE, sep=",")
larceny_data <- read.csv(file="/Users/admin/Documents/Queens_Masters_Courses/CISC873/Assignment_GLM/Dataset\ 2/gopi_data/larceny.csv", header=TRUE, sep=",")
property_data <- read.csv(file="/Users/admin/Documents/Queens_Masters_Courses/CISC873/Assignment_GLM/Dataset\ 2/gopi_data/property.csv", header=TRUE, sep=",")
robbery_data <- read.csv(file="/Users/admin/Documents/Queens_Masters_Courses/CISC873/Assignment_GLM/Dataset\ 2/gopi_data/robbery.csv", header=TRUE, sep=",")

#### Data ####
current_data <- larceny_data
df_crime <- as.data.frame(subset(current_data, current_data$larceny_cutoffs == 1
                                 | current_data$larceny_cutoffs == 2))

# Filter
df_crime$larceny_cutoffs <- as.factor(as.numeric(as.factor(df_crime$larceny_cutoffs)) - 1)
# Scaling
df_crime[c(2:ncol(df_crime))] <- lapply(df_crime[c(2:ncol(df_crime))], function(x) c(scale(x)))

# Random sampling 80/20 train test split
sample_size <- floor(0.80 * nrow(df_crime))
set.seed(123)
train_index <- sample(seq_len(nrow(df_crime)), size = sample_size)
train_data <- df_crime[train_index,]
test_data <- df_crime[-train_index,]

# Train Logistic Regression (GLM) model
glm_fit <- glm(train_data$larceny_cutoffs~.,data=train_data,family=binomial((link="logit")))

# Prediction against test data
predicted <- predict(glm_fit, test_data, type = "response")
test_data$pred <- ifelse(predict(glm_fit, test_data, type = "response") > 0.5,1,0)

# Performance metrics
pred_table <- table(test_data$pred,test_data$larceny_cutoffs)
df_cm <- confusionMatrix(data = as.factor(test_data$pred), reference = test_data$larceny_cutoffs)
roc_obj <- roc(test_data$larceny_cutoffs, predicted)
auc(roc_obj)

anova(glm_fit, test = 'Chisq')

