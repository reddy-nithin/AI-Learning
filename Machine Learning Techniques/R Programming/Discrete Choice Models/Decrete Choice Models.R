# Demonstartion of discrete choice models
#
# @author Nithineshwar Songala @date 02/21/2025

#set the working directory
setwd("/Users/lavanyadhaipully/Desktop/Nithin Assign")

#load the dataset
dataset <- read.csv("dataset_online_store.csv")
View(dataset)

#summary of the dataset
summary(dataset)
table(dataset$isMultiBrandUser)
table(dataset$promotiontype)

#display a table of proportionsfor categorical type in the dataset
(frequency_repeat_buyer <- table(dataset$isRepeatBuyer))
(proportion_repeat_buyer <- frequency_repeat_buyer/nrow(dataset))
(proportion_repeat_buyer <- frequency_repeat_buyer/length(dataset$isRepeatBuyer))

#Bar Chart of frequency/proportion table
barplot(frequency_repeat_buyer)
barplot(proportion_repeat_buyer)
barplot(proportion_repeat_buyer, xlab = "Repeat Purchase Choice Proportion", 
        ylab = "Proportion", col = c("blue", "red"))
barplot(proportion_repeat_buyer, xlab = "Repeat Purchase Choice Proportion", 
        ylab = "Proportion", col = c("blue", "red"), horiz = TRUE)


#create indicator variables for all two category variables in dataset
dataset$isRepeatBuyer_yes_i <- 0
dataset$isRepeatBuyer_yes_i[which(dataset$isRepeatBuyer == "yes")] <- 1

dataset$isMultiBrandUser_yes_i <- 0
dataset$isMultiBrandUser_yes_i[which(dataset$isMultiBrandUser == "yes")] <- 1

dataset$hasUserFriend_yes_i <- 0
dataset$hasUserFriend_yes_i[which(dataset$hasUserFriend == "yes")] <- 1

#estimate a logit model of repeat purchase as a function of isMultiBrandUser 
#log(p/(1-p)) = b0 + b1 * isMultiBrnadUser_yes_i

logit_model_1 <- glm(isRepeatBuyer_yes_i ~ isMultiBrandUser_yes_i,
                     family = binomial(link = "logit"), data = dataset)
summary(logit_model_1)
#log(p/(1-p)) = 1.11 - 1.07 * isMultiBrnadUser_yes_i

#marginal and multiplicative effects
marginal_effects_1 <- coef(logit_model_1) #doesn't print
(marginal_effects_1 <- coef(logit_model_1)) # to print use()
(multi_effects_1 <- exp(marginal_effects_1))
(multi_effects_percent_1 <- (multi_effects_1 - 1)*100)

#estimate a logit model where repeat purchase is a function of all variables
# except promotion type
logit_model_2 <- glm(isRepeatBuyer_yes_i ~ isMultiBrandUser_yes_i + 
                       hasUserFriend_yes_i + cookingRating + recipeRating,
                     family = binomial(link = "logit"), data = dataset)
summary(logit_model_2)

#marginal and multiplicative effects
(marginal_effects_2 <- coef(logit_model_2))
(multi_effects_2 <- exp(marginal_effects_2))
(multi_effects_percent_2 <- (multi_effects_2 - 1)*100)
cbind(marginal_effects_2, multi_effects_2, multi_effects_percent_2)

#making prediction to compute predictive accuracy
dataset$probability_predicted_2 <- predict(logit_model_2, dataset[ , c("isMultiBrandUser_yes_i",
                                                    "hasUserFriend_yes_i", "cookingRating",
                                                    "recipeRating")], type = "response")
dataset$choice_predicted_2 <- 0
dataset$choice_predicted_2[which(dataset$probability_predicted_2 > 0.5)] <- 1
(confusion_matrix_2 <- table(dataset$isRepeatBuyer, dataset$choice_predicted_2))


(accuracy_2 <-sum(diag(confusion_matrix_2))/sum(confusion_matrix_2))


#create Indicators for promotion type
dataset$promotion_free_i <-0
dataset$promotion_free_i[which(dataset$promotionType == "free")] <- 1

dataset$promotion_discount_i <-0
dataset$promotion_discount_i[which(dataset$promotionType == "discount")] <- 1

#estimate a logit model where repeat purchase is a function of all variables
# including promotion type
logit_model_3 <- glm(isRepeatBuyer_yes_i ~ isMultiBrandUser_yes_i + 
                       hasUserFriend_yes_i + cookingRating + recipeRating + 
                       promotion_free_i + promotion_discount_i, 
                     family = binomial(link = "logit"), data = dataset)
summary(logit_model_3)

#marginal and multiplicative effects for logit_model_3
(marginal_effects_3 <- coef(logit_model_3))
(multi_effects_3 <- exp(marginal_effects_3))
(multi_effects_percent_3 <- (multi_effects_3 - 1)*100)
cbind(marginal_effects_3, multi_effects_3, multi_effects_percent_3)

#making prediction to compute predictive accuracy for logit_model_3
dataset$probability_predicted_3 <- predict(logit_model_3, dataset[ , c("isMultiBrandUser_yes_i",
                                                                       "hasUserFriend_yes_i", "cookingRating",
                                                                       "recipeRating", "promotion_free_i",
                                                                       "promotion_discount_i")], type = "response")
dataset$choice_predicted_3 <- 0
dataset$choice_predicted_3[which(dataset$probability_predicted_3 > 0.5)] <- 1
(confusion_matrix_3 <- table(dataset$isRepeatBuyer, dataset$choice_predicted_3))

(accuracy_3 <-sum(diag(confusion_matrix_3))/sum(confusion_matrix_3))

# making a prediction for a new customer ( multibrand user, has no user friend, 
# received a discount type promotion)
new_customer <- data.frame(isMultiBrandUser_yes_i =1, hasUserFriend_yes_i =0,
                           cookingRating = mean(dataset$cookingRating),
                           recipeRating = mean(dataset$recipeRating),
                           promotion_free_i = 0, promotion_discount_i = 1)

predict(logit_model_3, new_customer, type = "response")

dataset[1:10, c("isRepeatBuyer", "probability_predicted_2", "choice_predicted_2",
                "probability_predicted_3", "choice_predicted_3")]




