#Bankrupty Prediction - Group Project
# @author J. Straub and Nithin Songala

#wd
setwd("/users/nithinreddy/R predictive")

# load dataset from external source about online store
dataset <- read.csv("dataset.csv")
nb_dataset <- read.csv("dataset.csv")
clean_dataset <- read.csv("Dataset_clean 1.csv")
nb_clean_dataset <- read.csv("Dataset_clean 1.csv")


#summarize
summary(dataset)
View(dataset)

summary(clean_dataset)
View(clean_dataset)

#----------------------------------- Logistic Regression -----------------------------#

#Logit Model -1 
logit_model_1 <- glm(Bankrupt. ~  ., 
                     family = binomial(link = "logit"), data = dataset)
summary(logit_model_1)

dataset$probability_predicted_1 <- predict(logit_model_1, dataset, type = "response")

dataset$choice_predicted_1 <- 0
dataset$choice_predicted_1[which(dataset$probability_predicted_1 > 0.5)] <- 1

(confusion_matrix_1 <- table(dataset$Bankrupt, dataset$choice_predicted_1))
(accuracy_1 <- sum(diag(confusion_matrix_1))/sum(confusion_matrix_1))

#------------------------------------------------------------------------------------------

#logit model 2: all of the 15 selected variables 
logit_model_2 <- glm(Bankrupt ~  ., family = binomial(link = "logit"), data = clean_dataset)
summary(logit_model_2)

#marginal/ multiplicative effects
(marginal_effects_2 <- coef(logit_model_2))
(multi_effects_2 <- exp(marginal_effects_2))
(multi_effects_percent_2 <- exp(multi_effects_2-1)*100)

#estimate logit model 
#except promotion type
cbind(marginal_effects_2,multi_effects_2,multi_effects_percent_2)

#making predictions
clean_dataset$probability_predicted_2 <- predict(logit_model_2, clean_dataset, type = "response")
clean_dataset$choice_predicted_2 <- 0
clean_dataset$choice_predicted_2[which(clean_dataset$probability_predicted_2 > 0.5)] <- 1

(confusion_matrix_2 <- table(clean_dataset$Bankrupt, clean_dataset$choice_predicted_2))
(accuracy_2 <- sum(diag(confusion_matrix_2))/sum(confusion_matrix_2))

#------------------------------------------------------------------------------------------

#Logit model 3 : 6 out of the 15 selected varaibles
logit_model_3 <- glm(Bankrupt ~  Tax_rate+Net_Value_Per_Share+Debt_ratio+Total_Asset_Turnover+Cash_to_Total_Assets+Total_income_to_Total_expense, 
                     family = binomial(link = "logit"), data = clean_dataset)
summary(logit_model_3)

#marginal/ multiplicative effects
(marginal_effects_3 <- coef(logit_model_3))
(multi_effects_3 <- exp(marginal_effects_3))
(multi_effects_percent_3 <- exp(multi_effects_3-1)*100)

#estimate logit model 
#except promotion type
cbind(marginal_effects_3,multi_effects_3,multi_effects_percent_3)

#making predictions
clean_dataset$probability_predicted_3 <- predict(logit_model_3, clean_dataset[ , c("Tax_rate",
                                                                        "Net_Value_Per_Share",
                                                                        "Debt_ratio",
                                                                        "Total_Asset_Turnover",
                                                                        "Cash_to_Total_Assets",
                                                                        "Total_income_to_Total_expense")],
                                            type = "response")

clean_dataset$choice_predicted_3 <- 0
clean_dataset$choice_predicted_3[which(clean_dataset$probability_predicted_3 > 0.5)] <- 1

(confusion_matrix_3 <- table(clean_dataset$Bankrupt, clean_dataset$choice_predicted_3))
(accuracy_3 <- sum(diag(confusion_matrix_3))/sum(confusion_matrix_3))


# ------------------------------- Navie Bayes --------------------------------#
library(naivebayes)
 
#Convert Bankrupt from int to facotr for NBC
nb_dataset$Bankrupt <- as.factor(nb_dataset$Bankrupt)
nb_clean_dataset$Bankrupt <- as.factor(nb_clean_dataset$Bankrupt)

#Split the dataset into training and validation sample
set.seed(1000)
proportion <- 0.70
indexes_train <- sample(nrow(nb_dataset), proportion*nrow(nb_dataset))
indexes_train_2 <- sample(nrow(nb_clean_dataset), proportion*nrow(nb_clean_dataset))

head(indexes_train,10)
data_train <- nb_dataset[indexes_train, ]
data_test <- nb_dataset[-indexes_train, ]

data_train_2 <- nb_clean_dataset[indexes_train_2, ]
data_test_2 <- nb_clean_dataset[-indexes_train_2, ]

#NBC Model 1: With all the predictors 
nbc_model_1 <- naive_bayes( Bankrupt ~ ., laplace = 1, data = data_train)

#examine a priori probabilities and likelihoods
nbc_model_1$prior
nbc_model_1$tables$Tax_rate
nbc_model_1$tables$Total_Asset_Turnover

# Making predictions using NBC model 1 for validation sample
data_test$class_predicted_1 <- predict(nbc_model_1, data_test)

#get posterior probabilities from nbc_model_1
post_probabilities_1 <- predict(nbc_model_1, data_test, type = "prob")
data_test$C1_post_prob_1 <- post_probabilities_1[ , "0"]
data_test$C1_post_prob_1 <- post_probabilities_1[ , "1"]

View(data_test)

#Compute predictive accuracy of NBC_model_1
(confusion_matrix_1 <- table(data_test$Bankrupt, data_test$class_predicted_1))
(predictive_accuracy_1 <- sum(diag(confusion_matrix_1))/sum(confusion_matrix_1))

#----------------------------------------------------------------------------------------

#NBC Model 2: With selected 15 predictors 
nbc_model_2 <- naive_bayes( Bankrupt ~ .,laplace = 1, data = data_train_2)

#examine a priori probabilities and likelihoods
nbc_model_2$prior
nbc_model_2$tables$Tax_rate
nbc_model_2$tables$Total_Asset_Turnover

# Making predictions using NBC model 2 for validation sample
data_test_2$class_predicted_2 <- predict(nbc_model_2, data_test_2)

#get posterior probabilities from nbc_model_2
post_probabilities_2 <- predict(nbc_model_2, data_test_2, type = "prob")
data_test_2$C2_post_prob_2 <- post_probabilities_2[ , "0"]
data_test_2$C2_post_prob_2 <- post_probabilities_2[ , "1"]

View(data_test_2)

#Compute predictive accuracy of NBC_model_2
(confusion_matrix_2 <- table(data_test_2$Bankrupt, data_test_2$class_predicted_2))
(predictive_accuracy_2 <- sum(diag(confusion_matrix_2))/sum(confusion_matrix_2))

#----------------------------------------------------------------------------------------

#NBC 3: with same 6 variables used in logit model
nbc_model_3 <- naive_bayes( Bankrupt ~ Tax_rate + 
                              Net_Value_Per_Share + 
                              Debt_ratio + 
                              Total_Asset_Turnover + 
                              Cash_to_Total_Assets + 
                              Total_income_to_Total_expense, 
                            laplace = 1, data = data_train_2)

#examine a priori probabilities and likelihoods
nbc_model_3$prior
nbc_model_3$tables$Tax_rate
nbc_model_3$tables$Total_Asset_Turnover

# Making predictions using NBC model 3 for validation sample
data_test_2$class_predicted_3 <- predict(nbc_model_3, data_test_2[ ,c("Tax_rate",
                                                                  "Net_Value_Per_Share",
                                                                  "Debt_ratio",
                                                                  "Total_Asset_Turnover",
                                                                  "Cash_to_Total_Assets",
                                                                  "Total_income_to_Total_expense")])

#get posterior probabilities from nbc_model_3
post_probabilities_3 <- predict(nbc_model_2, data_test_2[ ,c("Tax_rate",
                                                         "Net_Value_Per_Share",
                                                         "Debt_ratio",
                                                         "Total_Asset_Turnover",
                                                         "Cash_to_Total_Assets",
                                                         "Total_income_to_Total_expense")], type = "prob")
data_test_2$C3_post_prob_3 <- post_probabilities_3[ , "0"]
data_test$C3_post_prob_3 <- post_probabilities_3[ , "1"]

View(data_test)

#Compute predictive accuracy of NBC_model_3
(confusion_matrix_3 <- table(data_test_2$Bankrupt, data_test_2$class_predicted_3))
(predictive_accuracy_3 <- sum(diag(confusion_matrix_3))/sum(confusion_matrix_3))