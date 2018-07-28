#Unbiased
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
#For unbiased data 
#Taking random samples from dataset with 4640 yes and no
bank_no <- subset(bank_data, bank_data$y == 0)
bank_yes <- subset(bank_data, bank_data$y == 1)
bank_no <- bank_no[sample(nrow(bank_no)),]
bank_no <- bank_no[1:4640,]
df_bank_data <- rbind(bank_no, bank_yes)

#-------------------------------------------------------------------------------------------------------------------------------------
#calculating the number of yes and no values in the predictor variable
table(df_bank_data$y)

#Splitting the data set in 75:25
split <- sample.split(df_bank_data$y,SplitRatio = 0.75)
train <- subset(df_bank_data,split == TRUE)
test <- subset(df_bank_data,split == FALSE)

#-------------------------------------------------------------------------------------------------------------------------------------
#Variable Selection and Logistic regression
new_reg <- step(glm(formula = y~.,family = binomial, data = train))
#Training the Logistic Regression model with duration variable
lreg <- glm(formula = y ~ contact + duration + campaign + previous + blue.collar + 
              management + retired + self.employed + services + student + 
              technician + divorced + married + single + apr + aug + jul + 
              jun + mar + may + nov + oct + mon + thu + failure + nonexistent + 
              basic + illiterate + university.degree,family = binomial, data = train)

#testing the data set (LR)
predicted <- predict(lreg, type = "response", newdata = test)
prob_pred = ifelse(predicted > 0.4, 'yes','no')
table(test$y,prob_pred)

#ROC Curve
pred <- prediction(predicted, test$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col=2, lwd = 3, main = "ROC Curve")
abline(0,1)
#AUC
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]

#-------------------------------------------------------------------------------------------------------------------------------------

#Logistic Regression Without duration variable
lreg1 <- glm(formula = y ~ contact + campaign + previous + blue.collar + 
               management + retired + self.employed + services + student + 
               technician + divorced + married + single + apr + aug + jul + 
               jun + mar + may + nov + oct + mon + thu + failure + nonexistent + 
               basic + illiterate + university.degree,family = binomial, data = train[-3])
predicted_1 <- predict(lreg1, type = "response", newdata = test)
prob_pred_1 = ifelse(predicted_1 > 0.5, 'yes','no')
table(test$y,prob_pred_1)
#ROC Curve
pred_1 <- prediction(predicted_1, test$y)
perf_1 <- performance(pred_1, measure = "tpr", x.measure = "fpr")
plot(perf_1, col=2, lwd = 3, main = "ROC Curve")
abline(0,1)
#AUC
auc_ROCR <- performance(pred_1, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]

#-------------------------------------------------------------------------------------------------------------------------------------
#Random Forest
classifier_rf = randomForest(x = train[-6],y = as.factor(train$y) ,data = train, ntree = 20 , importance = TRUE)
y_pred <- predict(classifier_rf, newdata = test[-6])
table(test$y,y_pred)
plot(classifier_rf)
varImpPlot(classifier_rf)

#ROC Curve
pred <- prediction(y_pred, test$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col=2, lwd = 3, main = "ROC Curve")
abline(0,1)
#Area Under the Curve
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]

# Random forest with AIC run values to reduce variables
rf_c = randomForest(x = train[-6][-7][-9][-10][-17][-21][-22][-23][-24]
                    [-27][-34][-35][-38][-39][-42][-44][-46],y = as.factor(train$y) ,data = train, ntree = 20 , importance = TRUE)
pred <- predict(rf_c, newdata = test[-6])
table(test$y,pred)
plot(rf_c)

#-------------------------------------------------------------------------------------------------------------------------------------
#KNN
knn_classifier <- knn(train=train[,-6],test = test[,-6], cl = train[,6], k =5 )
table(knn_classifier,test$y)

#KNN with AIC run values
knn_c <- knn(train=train[,-6][,-7][,-9][,-10][,-17][,-21][,-22][,-23][,-24]
             [,-27][,-34][,-35][,-38][,-39][,-42][,-44][,-46],test = test[,-6][,-7][,-9][,-10][,-17][,-21][,-22][,-23][,-24]
             [,-27][,-34][,-35][,-38][,-39][,-42][,-44][,-46], cl = train[,6], k =5 )
table(knn_c,test$y)

#-------------------------------------------------------------------------------------------------------------------------------------

#calculating model accuracies for KNN, RF and LR
model_accuracy <- c(76.16,85.94,83.83)
models_labels <- c('KNN','RF','LR')


accracy <- data.frame(Location =models_labels, Count = model_accuracy)
ggplot(
  accracy
  , aes(y = Count
        , x = Location)) +
  geom_col() + xlab("AIC Values") + ylab("Number of variables") + ggtitle("Number of Variables Vs AIC Values")