library(readr)
library(mice)
library(caTools)
library(caret)
library(ROCR)
set.seed(144)

loans <- read_csv("loans.csv")

# What proportion of the loans in the dataset were not paid in full?
table(loans$not.fully.paid)["1"] / nrow(loans)

# Which of the following variables has at least one missing observation? 
names(which(sapply(loans, function(x) any(is.na(x)))))
# [1] "log.annual.inc"    "days.with.cr.line" "revol.util"       
# [4] "inq.last.6mths"    "delinq.2yrs"       "pub.rec" 



# Which of the following is the best reason to fill in the missing values 
# for these variables instead of removing observations with missing data? 

# We want to be able to predict risk for all borrowers, 
# instead of just the ones with all data reported.


loans_imputed <- read_csv("loans_imputed.csv")
split <- sample.split(loans_imputed$not.fully.paid, 0.7)
loans_train <- loans_imputed[split, ]
loans_test <- loans_imputed[!split, ]


logistic_model <- glm(not.fully.paid~ credit.policy + purpose + int.rate + installment + 
      log.annual.inc + dti + fico + days.with.cr.line + revol.bal + 
     revol.util + inq.last.6mths + delinq.2yrs + pub.rec, family=binomial(link="logit"), data=loans_train)


# What is the value of Logit(A) - Logit(B)?
-(logistic_model$coefficients[["fico"]]) * 10

# What is the value of O(A)/O(B)?
exp(-(logistic_model$coefficients[["fico"]]) * 10)

# Compute the confusion matrix using a threshold of 0.5.
loans_test$prob <- predict(logistic_model, loans_test, type="response")
loans_test$preds <- (loans_test$prob > 0.5)
loans_test$reference <- (loans_test$not.fully.paid == 1)
conf_matrix <- confusionMatrix(factor(loans_test$preds), factor(loans_test$reference))


# What is the accuracy of the logistic regression model? 
total <- sum(conf_matrix$table)
correctos <- sum(diag(conf_matrix$table))
correctos / total

# What is the accuracy of the baseline model?
falsos <- sum(conf_matrix$table[,1])
falsos/total
# 2403 predictions are correct (accuracy 2403/2873=0.8364)
# while 2413 predictions would be correct in the baseline model 
# of guessing every loan would be paid back in full 
# (accuracy 2413/2873=0.8399).

pred.obj <- prediction(loans_test$preds, loans_test$reference)

auc.perf = performance(pred.obj, measure = "auc")
auc.perf@y.values
perf <- performance(pred.obj, "tpr", "fpr")
plot(perf)
