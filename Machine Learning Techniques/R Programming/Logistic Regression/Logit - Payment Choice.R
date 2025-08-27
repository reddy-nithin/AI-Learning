#Solution to Assignment Discrete Choice Models

#@author Nithineshwar Songala @date 03/06/2025

#set the working directory
setwd("/users/nithinreddy/R predictive")

#read the volutary payment choice dataset from the working directory
dataset <- read.csv("dataset_payment_choice.csv")

View(dataset)

# Coerse data structure
dataset$genre <- as.factor(dataset$genre)
dataset$payment_choice <- as.factor(dataset$payment_choice)
dataset$appeal_type <- as.factor(dataset$appeal_type)
dataset$previous_payer <- as.factor(dataset$previous_payer)

summary(dataset)

#create indicators for all categorical variables
dataset$payment_yes_i <- 0
dataset$payment_yes_i[which(dataset$payment_choice == "yes")] <- 1

dataset$appeal_charity_i <- 0
dataset$appeal_charity_i[which(dataset$appeal_type == "charity")] <- 1

dataset$genre_action_i <- 0
dataset$genre_action_i[which(dataset$genre == "action")] <- 1

dataset$genre_simulation_i <- 0
dataset$genre_simulation_i[which(dataset$genre == "simulation")] <- 1

dataset$prev_payer_yes_i <- 0
dataset$prev_payer_yes_i[which(dataset$previous_payer == "yes")] <- 1

dataset$prev_payer_no_i <- 0
dataset$prev_payer_no_i[which(dataset$previous_payer == "no")] <- 1

# Question 1
table(dataset$payment_choice, useNA = "ifany")/nrow(dataset)

#Question 3: estimate choice as a function  developer rating, number of streams and game genre
logit_model_1 <- glm(payment_yes_i ~ dev_rating + num_streams + 
                       genre_simulation_i + genre_action_i, 
                     family = binomial(link = "logit"), 
                     data=dataset)

summary(logit_model_1)

#Marginal and Multiplicative effects for logit_model_1
(marginal_effects_1 <- coef(logit_model_1))
(multi_effects_1 <- exp(marginal_effects_1))
(multi_effects_percent_1 <- (multi_effects_1-1) * 100)

#Compute Predictive accuracy of logit-model_1
dataset$probability_predicted_1 <- predict(logit_model_1,
                                           dataset[, c("dev_rating",
                                                       "num_streams",
                                                       "genre_simulation_i",
                                                       "genre_action_i")],
                                           type = "response")
dataset$choice_predicted_1 <- 0
dataset$choice_predicted_1[which(dataset$probability_predicted_1 > 0.50)] <- 1

(confusion_matrix_1 <- table(dataset$payment_choice, 
                             dataset$choice_predicted_1))

(accuracy_1 <- sum(diag(confusion_matrix_1))/sum(confusion_matrix_1))

#Question 7: estimate choice as a function  developer rating, number of streams, game genre, previous payer status and payment appeal
logit_model_2 <- glm(payment_yes_i ~ dev_rating + num_streams + 
                       genre_simulation_i + genre_action_i +
                       prev_payer_no_i + prev_payer_yes_i +
                       appeal_charity_i,
                     family = binomial(link = "logit"), 
                     data=dataset)

summary(logit_model_2)

#Marginal and Multiplicative effects for logit_model_2
(marginal_effects_2 <- coef(logit_model_2))
(multi_effects_2 <- exp(marginal_effects_2))
(multi_effects_percent_2 <- (multi_effects_2-1) * 100)

#Compute Predictive accuracy of logit-model_2
dataset$probability_predicted_2 <- predict(logit_model_2,
                                           dataset[, c("dev_rating",
                                                       "num_streams",
                                                       "genre_simulation_i",
                                                       "genre_action_i",
                                                       "prev_payer_no_i",
                                                       "prev_payer_yes_i",
                                                       "appeal_charity_i")],
                                           type = "response")

dataset$choice_predicted_2 <- 0
dataset$choice_predicted_2[dataset$probability_predicted_2 > 0.50] <- 1

(confusion_matrix_2 <- table(dataset$payment_choice, 
                            dataset$choice_predicted_2))

(accuracy_2 <- sum(diag(confusion_matrix_2))/sum(confusion_matrix_2))


#Making a prediction for new customer outside of a sample
new_customer_1 <- data.frame(dev_rating = 2.5,
                             num_streams = 1000,
                             genre_action_i = 1,
                             genre_simulation_i = 0,
                             appeal_charity_i = 0,
                             prev_payer_no_i = 0,
                             prev_payer_yes_i = 0)

logit_model_3 <- glm(payment_yes_i ~ dev_rating + num_streams + 
                       genre_simulation_i + genre_action_i +
                       prev_payer_no_i + prev_payer_yes_i +
                       appeal_charity_i,
                     family = binomial(link = "logit"), 
                     data=dataset)

summary(logit_model_3)

predict(logit_model_3, new_customer_1, type = "response")


#Making another prediction for new customer outside of a sample
# Changing the data points to check with multiple options

#Base Case (without any additional conditions):
new_customer_2 <- data.frame(dev_rating = mean(dataset$dev_rating),
                             num_streams = mean(dataset$num_streams),
                             genre_action_i = 1,
                             genre_simulation_i = 0,
                             appeal_charity_i = 0,
                             prev_payer_no_i = 0,
                             prev_payer_yes_i = 0)

predict(logit_model_3, new_customer_2, type = "response")

# Case 2: First-time payer scenario (prev_payer_no_i = 1)
new_customer_3 <- data.frame(dev_rating = mean(dataset$dev_rating),
                             num_streams = mean(dataset$num_streams),
                             genre_action_i = 1,
                             genre_simulation_i = 0,
                             appeal_charity_i = 0,
                             prev_payer_no_i = 1,
                             prev_payer_yes_i = 0)

predict(logit_model_3, new_customer_3, type = "response")

#Case 3: Charity appeal scenario (appeal_charity_i = 1)
new_customer_4 <- data.frame(dev_rating = mean(dataset$dev_rating),
                             num_streams = mean(dataset$num_streams),
                             genre_action_i = 1,
                             genre_simulation_i = 0,
                             appeal_charity_i = 1,
                             prev_payer_no_i = 0,
                             prev_payer_yes_i = 0)

predict(logit_model_3, new_customer_3, type = "response")