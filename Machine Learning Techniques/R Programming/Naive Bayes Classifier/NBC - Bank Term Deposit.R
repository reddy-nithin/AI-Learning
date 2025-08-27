#Solution to Assisted classwork on Classification Models (NBC)

#@author Nithineshwar Songala @date 03/18/2025

#set the working directory
setwd("/users/nithinreddy/R predictive")

#read the dataset_bank_term_deposit dataset from the working directory
dataset <- read.csv("dataset_bank_term_deposit.csv")

View(dataset)

#Split the dataset into training and validation sample
set.seed(1000)
proportion <- 0.80
indexes_train <- sample(nrow(dataset), proportion*nrow(dataset))
head(indexes_train,10)

data_train <- dataset[indexes_train, ]
data_test <- dataset[-indexes_train, ]


# -------------- NAVIE BAYES MODEL ----------------- #

install.packages("naivebayes")
library(naivebayes)

#Build an NBC model on training dataset using 6 predictors:
#age, duration, marital status, housing, previous and job
nbc_model_1 <- naive_bayes(y ~ age + duration + housing + marital + previous + job, data = data_train)

#prior (a-priori) / table (likelihoods)
nbc_model_1$prior
nbc_model_1$table$age
nbc_model_1$table$marital

#Making predictions using nbc_model_1 
data_test$class_predicted_1 <- predict(nbc_model_1, data_test[ , c("age", 
                                                                   "duration",
                                                                   "housing",
                                                                   "marital",
                                                                   "previous",
                                                                   "job")])
(confusion_matrix_1 <- table(data_test$y, data_test$class_predicted_1))
(predictive_accuracy <- sum(diag(confusion_matrix_1))/sum(confusion_matrix_1))


#Buidling nbc_model_2 with predictions: age, housing, marital status, previous calls, and job status
nbc_model_2 <- naive_bayes(y ~ age + housing + marital + previous + job, data = data_train)

#Making predictions using nbc_model_2 
data_test$class_predicted_2 <- predict(nbc_model_2, data_test[ , c("age", 
                                                                   "housing",
                                                                   "marital",
                                                                   "previous",
                                                                   "job")])
(confusion_matrix_2 <- table(data_test$y, data_test$class_predicted_2))
(predictive_accuracy <- sum(diag(confusion_matrix_2))/sum(confusion_matrix_2))

#prior (a-priori) / table (likelihoods)
nbc_model_2$prior
nbc_model_2$table$age
nbc_model_2$table$marital

#--------------- New Train Test Split --------------------

# Creating new train test split
set.seed(1000)
proportion_2 <- 0.75
indexes_train_2 <- sample(nrow(dataset), proportion_2*nrow(dataset))
head(indexes_train_2,10)

data_train_2 <- dataset[indexes_train_2, ]
data_test_2 <- dataset[-indexes_train_2, ]

# Building an NBC with new dataset
nbc_model_3 <- naive_bayes(y ~ age + duration + housing + marital + previous + job, data = data_train_2)

#Making predictions using nbc_model_3 
data_test_2$class_predicted_3 <- predict(nbc_model_3, data_test_2[ , c("age", 
                                                                   "duration",
                                                                   "housing",
                                                                   "marital",
                                                                   "previous",
                                                                   "job")])

(confusion_matrix_3 <- table(data_test_2$y, data_test_2$class_predicted_3))
(predictive_accuracy <- sum(diag(confusion_matrix_3))/sum(confusion_matrix_3))


nbc_model_3$table


