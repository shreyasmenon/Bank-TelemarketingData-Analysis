#Biased
#libraries
library(caret)
library(caTools)
library(class)
library(e1071)
library(ggplot2)
library(gplots)
library(InformationValue)
library(randomForest)
library(ROCR)

#-------------------------------------------------------------------------------------------------------------------------------------

#Reading the dataset
bank_data <- read.csv('C:/Users/Dell1/Desktop/Multivariate Bank Marketing Dataset/bank-final-clean.csv',header = TRUE)
View(bank_data)

#-------------------------------------------------------------------------------------------------------------------------------------

#Splitting the data set
split <- sample.split(bank_data$y,SplitRatio = 0.75)
train <- subset(bank_data,split == TRUE)
test <- subset(bank_data,split == FALSE)

#-------------------------------------------------------------------------------------------------------------------------------------

#Variable Selection
new_reg <- step(glm(formula = y~.,family = binomial, data = train))

#-------------------------------------------------------------------------------------------------------------------------------------

#Logistic Regression
lreg <- glm(formula = y ~ age + contact + duration + campaign + previous + admin. + 
                blue.collar + entrepreneur + housemaid + management + self.employed + 
                services + student + technician + unemployed + single + apr + 
                aug + jul + jun + mar + may + nov + oct + failure + nonexistent + 
                basic + professional.course + university.degree,family = binomial, data = train)

#testing the data set (LR)
predicted <- predict(lreg, type = "response", newdata = test)
prob_pred = ifelse(predicted > 0.5, 'yes','no')
table(test$y,prob_pred)

#ROC Curve
pred <- prediction(predicted, test$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col=2, lwd = 3, main = "ROC Curve")
abline(0,1)
#Area Under the Curve (AUC)
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]

#-------------------------------------------------------------------------------------------------------------------------------------

#Logistic Regression without duration variable
lreg_1 <- glm(formula = y ~ age + contact + campaign + previous + blue.collar + 
                entrepreneur + management + retired + self.employed + services + 
                student + technician + divorced + married + apr + aug + jul + 
                jun + mar + may + nov + fri + mon + thu + failure + nonexistent + 
                basic + high.school + professional.course,family = binomial, data = train)

predict_1 <- predict(lreg_1, type = "response", newdata = test)
prob_pre = ifelse(predict_1 > 0.5, 'yes','no')
table(test$y,prob_pre)

#ROC Curve
pred_1 <- prediction(predict_1, test$y)
perf_1 <- performance(pred_1, measure = "tpr", x.measure = "fpr")
plot(perf_1, col=2, lwd = 3, main = "ROC Curve")
abline(0,1)
#Area Under the Curve(AUC)
auc_ROCR <- performance(pred_1, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]

#-------------------------------------------------------------------------------------------------------------------------------------

#Random Forest
classifier_rf = randomForest(x = train[-6],y = as.factor(train$y) ,data = train, ntree = 20 , importance = TRUE)
y_pred <- predict(classifier_rf, newdata = test[-6])
table(test$y,y_pred)
plot(classifier_rf)

#-------------------------------------------------------------------------------------------------------------------------------------

#KNN
knn_classifier <- knn(train=train[,-6],test = test[,-6], cl = train[,6], k =5 )
table(knn_classifier,test$y)

#-------------------------------------------------------------------------------------------------------------------------------------

#Calculating teh accuracies of KNN, RF and LR
model_accuracy <- c(89.01,90.51,90.7)
models_labels <- c('KNN','RF','LR')

accracy <- data.frame(Location =models_labels, Count = model_accuracy)
ggplot(
  accracy
  , aes(y = Count
        , x = Location)) +
  geom_col() + xlab("Models") + ylab("Accuracy") + ggtitle("Comparison of Accuracy amongst different models")

#-------------------------------------------------------------------------------------------------------------------------------------
