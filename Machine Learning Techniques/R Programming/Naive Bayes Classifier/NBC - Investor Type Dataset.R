#Demo Replication Navie Bayes Classifier

#@author Nithineshwar Songala @date 03/18/2025

#set the working directory
setwd("/users/nithinreddy/R predictive")

#Read Dataset
dataset <- read.csv("dataset_investor_type.csv")
View(dataset)

# Split the dataset into two datasets: Training and Validation

data_train <- dataset[1:900, ]
data_test <- dataset[901:1000, ]
View(data_train)
View(data_test)


# ----------------- NAIVE BAYES CLASSIFIER ------------------ #
library(naivebayes)

#Build an NBC using education and age as predictors
nbc_model_1 <- naive_bayes(investor_type ~ educ_group + age, data = data_train)

#examine a priori probabilities and likelihoods
nbc_model_1$prior
nbc_model_1$tables$educ_group
nbc_model_1$tables$age

#Examine the distribution of ages within the three classes
hist(data_train$age[which(data_train$investor_type == "active_advisory")], 50,
     xlab = "age", main = "age distribution within active_advisory class")
hist(data_train$age[which(data_train$investor_type == "active_self")], 50,
     xlab = "age", main = "age distribution within active_self class")
hist(data_train$age[which(data_train$investor_type == "passive")], 50,
     xlab = "age", main = "age distribution within passive class")



# NBC model 1 as an equation
#P(C_K|educ_group, age) = [P(C_k) = {P(educ_group|C_k) = P(age|C_k)}]/P(educ_group, age)

#P(C_1|educ_group, age) = [P(C_1) = {P(educ_group|C_1) = P(age|C_1)}]/P(educ_group, age)
#P(C_2|educ_group, age) = [P(C_2) = {P(educ_group|C_2) = P(age|C_2)}]/P(educ_group, age)
#P(C_3|educ_group, age) = [P(C_3) = {P(educ_group|C_3) = P(age|C_3)}]/P(educ_group, age)

# Making predictions using NBC model 1 for validation sample
data_test$class_predicted_1 <- predict(nbc_model_1, data_test[ ,c("educ_group", "age")])

#get posterior probabilities from nbc_model_1
post_probabilities <- predict(nbc_model_1, data_test[ ,c("educ_group", "age")], type = "prob")

data_test$C1_post_prob_1 <- post_probabilities[ , "active_advisory"]
data_test$C1_post_prob_1 <- post_probabilities[ , "active_self"]
data_test$C1_post_prob_1 <- post_probabilities[ , "passive"]



#build NBC using all available information as predictors
nbc_model_2 <- naive_bayes(investor_type ~ income_group + networth_group + educ_group + age + homeowner + num_children + multiBrandUser, data = data_train)

# Making predictions using NBC model 2 for validation sample
data_test$class_predicted_2 <- predict(nbc_model_2, data_test[ , c("income_group",
                                                                   "networth_group",
                                                                   "educ_group",
                                                                   "age",
                                                                   "homeowner",
                                                                   "num_children",
                                                                   "multiBrandUser")])

#get posterior probabilities from nbc_model_2
post_probabilities_2 <- predict(nbc_model_2, data_test[ ,c("income_group",
                                                           "networth_group",
                                                           "educ_group",
                                                           "age",
                                                           "homeowner",
                                                           "num_children",
                                                           "multiBrandUser")], type = "prob")

data_test$C1_post_prob_2 <- post_probabilities_2[ , "active_advisory"]
data_test$C1_post_prob_2 <- post_probabilities_2[ , "active_self"]
data_test$C1_post_prob_2 <- post_probabilities_2[ , "passive"]

#Compute predictive accuracy of NBC_model_2
(confusion_matrix_2 <- table(data_test$investor_type, data_test$class_predicted_2))
(predictive_accuracy_2 <- sum(diag(confusion_matrix_2))/sum(confusion_matrix_2))



#build NBC using all available information as predictors (include laplase smoothing options)
nbc_model_3 <- naive_bayes(investor_type ~ income_group + networth_group + educ_group + age + homeowner + num_children + multiBrandUser, laplace = 1, data = data_train)

# Making predictions using NBC model 3 for validation sample
data_test$class_predicted_3 <- predict(nbc_model_3, data_test[ , c("income_group",
                                                                   "networth_group",
                                                                   "educ_group",
                                                                   "age",
                                                                   "homeowner",
                                                                   "num_children",
                                                                   "multiBrandUser")])

#get posterior probabilities from nbc_model_3
post_probabilities_3 <- predict(nbc_model_3, data_test[ ,c("income_group",
                                                           "networth_group",
                                                           "educ_group",
                                                           "age",
                                                           "homeowner",
                                                           "num_children",
                                                           "multiBrandUser")], type = "prob")

data_test$C1_post_prob_3 <- post_probabilities_3[ , "active_advisory"]
data_test$C1_post_prob_3 <- post_probabilities_3[ , "active_self"]
data_test$C1_post_prob_3 <- post_probabilities_3[ , "passive"]

#Compute predictive accuracy of NBC_model_3
(confusion_matrix_3 <- table(data_test$investor_type, data_test$class_predicted_3))
(predictive_accuracy_3 <- sum(diag(confusion_matrix_3))/sum(confusion_matrix_3))



#build NBC using all available predictors expect age (include laplase smoothing options)
nbc_model_4 <- naive_bayes(investor_type ~ income_group + networth_group + educ_group + homeowner + num_children + multiBrandUser, laplace = 1, data = data_train)

# Making predictions using NBC model 4 for validation sample
data_test$class_predicted_4 <- predict(nbc_model_4, data_test[ , c("income_group",
                                                                   "networth_group",
                                                                   "educ_group",
                                                                   "homeowner",
                                                                   "num_children",
                                                                   "multiBrandUser")])

#Compute predictive accuracy of NBC_model_4
(confusion_matrix_4 <- table(data_test$investor_type, data_test$class_predicted_4))
(predictive_accuracy_4 <- sum(diag(confusion_matrix_4))/sum(confusion_matrix_4))



#build NBC using all available predictors expect multiBrandUser (include laplase smoothing options)
nbc_model_5 <- naive_bayes(investor_type ~ income_group + networth_group + educ_group + homeowner + num_children + age, laplace = 1, data = data_train)

# Making predictions using NBC model 5 for validation sample
data_test$class_predicted_5 <- predict(nbc_model_5, data_test[ , c("income_group",
                                                                   "networth_group",
                                                                   "educ_group",
                                                                   "homeowner",
                                                                   "num_children",
                                                                   "age")])

#Compute predictive accuracy of NBC_model_5
(confusion_matrix_5 <- table(data_test$investor_type, data_test$class_predicted_5))
(predictive_accuracy_5 <- sum(diag(confusion_matrix_5))/sum(confusion_matrix_5))

