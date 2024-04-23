library(readr)
library(tidyverse)
library(caret)
library(e1071)
library(dplyr)
library(caret)
library(e1071)
library(partykit)
library(ROCR)
library(rpart.plot)
library(randomForest)
library(C50)
library(plotrix)
library(dlookr)
library(ggplot2)
library(readr)
library(gridExtra)
library(grid)

#Read data
data <- read_csv("C:/Users/ABU TAREQ RONY/OneDrive/Desktop/New Poject Autism/Dataset/Toddler Autism dataset July 2018.csv")
#Descriptive Statistic
summary(data)
str(data)

#Remove Case No as it has no contribution
data <- data[,-1]
head(data)
summary(data)
library(summarytools)
freq(data$used_app_before)
#Check null values
colSums(is.na(data))

#Correlation of numeric data
#data = as.numeric(data)
require(corrplot)# correlation package
cordata.2 <- cor(data[,c(1,2,3,4,5,6,7,8,9,10,11)])
# Correlation plot and values in a matrix graph 
#A10 and Age_Mons are not really correlated with the score
corrplot(cordata.2,method = "number",type="upper",tl.col ="black")



chi1 <- chisq.test(data$Sex,data$Class)
chi1
chi2 <- chisq.test(data$Ethnicity,data$Class)
chi2
chi3 <- chisq.test(data$Jaundice,data$Class)
chi3
chi4 <- chisq.test(data$Family_mem_with_ASD,data$Class)
chi4
chi5 <- chisq.test(data$Relation,data$Class)
chi5 
chi6 <- chisq.test(data$used_app_before,data$Class) 
chi6

##ANOVA test
ANOVA1 <-lm(age ~ Class, data = data)
ANOVA1
anova(ANOVA1)


ANOVA2 <-lm(Qchat_score ~ Class, data = data)
ANOVA2
anova(ANOVA2)

ANOVA3 <-lm(A1 ~ Class, data = data)
ANOVA3
anova(ANOVA3)

ANOVA4 <-lm(A2 ~ Class, data = data)
ANOVA4
anova(ANOVA4)


ANOVA5 <-lm(A3 ~ Class, data = data)
ANOVA5
anova(ANOVA5)

ANOVA6 <-lm(A4 ~ Class, data = data)
ANOVA6
anova(ANOVA6)

ANOVA7 <-lm(A5 ~ Class, data = data)
ANOVA7
anova(ANOVA7)

ANOVA8 <-lm(A6 ~ Class, data = data)
ANOVA8
anova(ANOVA8)


ANOVA9 <-lm(A7 ~ Class, data = data)
ANOVA9
anova(ANOVA9)


ANOVA10 <-lm(A8 ~ Class, data = data)
ANOVA10
anova(ANOVA10)


ANOVA11 <-lm(A9 ~ Class, data = data)
ANOVA11
anova(ANOVA11)


ANOVA12 <-lm(A10 ~ Class, data = data)
ANOVA12
anova(ANOVA12)

#Exploratory Data Analysis
x<-filter(data,Class=="Yes")
p1<-ggplot(data,aes(Sex))+geom_bar(aes(fill=Class)) 
p2<-ggplot(data,aes(Ethnicity))+geom_bar(aes(fill=Class))
p3<-ggplot(data,aes(Jaundice))+geom_bar(aes(fill=Class))
p4<-ggplot(data,aes(Relation))+geom_bar(aes(fill=Class))
p5<-ggplot(data,aes(Family_mem_with_ASD))+geom_bar(aes(fill=Class))
p6<-ggplot(data,aes(used_app_before))+geom_bar(aes(fill=Class))
grid.arrange(p1,p2,p3,p4,p5,p6, ncol=1, top=textGrob("Exploratory Data Analysis of Categorical Variables", gp=gpar(fontsize=12, font = 2)))




Q1<-ggplot(data,aes(A1))+geom_bar(aes(fill=Class)) 
Q2<-ggplot(data,aes(A2))+geom_bar(aes(fill=Class)) 
Q3<-ggplot(data,aes(A3))+geom_bar(aes(fill=Class)) 
Q4<-ggplot(data,aes(A4))+geom_bar(aes(fill=Class)) 
Q5<-ggplot(data,aes(A5))+geom_bar(aes(fill=Class)) 
Q6<-ggplot(data,aes(A6))+geom_bar(aes(fill=Class)) 
Q7<-ggplot(data,aes(A7))+geom_bar(aes(fill=Class)) 
Q8<-ggplot(data,aes(A8))+geom_bar(aes(fill=Class)) 
Q9<-ggplot(data,aes(A9))+geom_bar(aes(fill=Class)) 
Q10<-ggplot(data,aes(A10))+geom_bar(aes(fill=Class)) 

grid.arrange(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10, ncol=2, top=textGrob("Exploratory Data Analysis of Question 1-10", gp=gpar(fontsize=12, font = 2)))

#Pie Chart of Class Attribute




tab_sub <- table(data$Class)
pct <- round(tab_sub/sum(tab_sub)*100)
lbls <- c("Yes", "No")
lbls <- paste(lbls, pct) # add percents to labels 
#lbls <- paste(pct,"%",sep="") # ad % to labels
pie3D(tab_sub,labels=lbls,explode=0.5,
      main="Pie Chart of Dependent Variable(Class)", col = hcl.colors(length(tab_sub), "Spectral"),
      border = "white",shade = 0.5, labelcol = "green",
      labelcex = 2)


#Outlier remove from data
data <- read_csv("C:/Users/ABU TAREQ RONY/OneDrive/Desktop/New Poject Autism/Dataset/Toddler Autism dataset July 2018.csv")
boxplot(data$age,main="Age With Outliers",col="deepskyblue3", plot=T)$out
outliers <- boxplot(data$age, plot=T)$out#Do aagain from this line
x<-data #https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/
data<- x[-which(x$age %in% outliers),]
No_Outlier<-boxplot(data$age,main="Age Without Outliers",col="deepskyblue3", plot=T)


data <- read_csv("C:/Users/ABU TAREQ RONY/OneDrive/Desktop/New Poject Autism/Dataset/Newdata.csv")

##Modelling
data <- read_csv("C:/Users/ABU TAREQ RONY/OneDrive/Desktop/New Poject Autism/Dataset/Toddler Autism dataset July 2018.csv")
data$Class <- as.factor(data$Class)
inTrain <- createDataPartition(data$Class, p = 0.8, list = FALSE)
autism_train <- data[inTrain, ]
autism_test <- data[-inTrain, ]



#Logistics Regression
#Logistics Regression
data$Class <- as.factor(data$Class)
model_1 <- glm(Class~A1+A2+A3+A4+A5+A6+A7+A8+Sex+Jaundice, data = data, family = binomial())
summary(model_1)
exp(coef(model_1))


exp(cbind(OR = coef(model_1), confint(model_1)))




log_imp <- varImp(model_1, scale = FALSE, competes = FALSE)
log_imp

pred_log <- predict(model_1, newdata = autism_test)
pred_log
confusionMatrix(pred_log, autism_test$Class)

#K nearest Neighbours
knn <- train(Class ~., data = autism_train,
             method = "knn",
             preProcess = c("center", "scale"),
             tuneLength = 10)
knn
knn_pred <- predict(knn, newdata = autism_test)
confusionMatrix(knn_pred, autism_test$Class)


#Random Forest
mtryGrid <- data.frame(mtry = floor(seq(10, ncol(autism_train), length = 10)))
rf <- train(Class ~., data = autism_train,
            method = "rf",
            tuneGrid = mtryGrid,
            ntree = 200,
            importance = TRUE)
rf

rf_imp <- varImp(rf, scale = FALSE, competes = FALSE)
rf_imp

rf_pred <- predict(rf, newdata = autism_test)
confusionMatrix(rf_pred, autism_test$Class)


# Naive Bayes Model
model_naive <- naiveBayes(formula = Class ~ ., data = data, laplace = 1)
test <- autism_test %>%  select(-Class)
naive_pred <- predict(object = model_naive, newdata = autism_test)

confusionMatrix(data = naive_pred, reference = autism_test$Class, positive = "Yes")

autism_test$pred <- predict(object = model_naive, newdata = autism_test, type = "raw")
autism_test$actual <- ifelse(test = autism_test$Class == "Yes", 1, 0)
# objek prediction
roc_pred <- prediction(predictions = autism_test$pred[,1], labels = autism_test$actual)

# ROC curve
plot(performance(prediction.obj = roc_pred, measure = "tpr", x.measure = "fpr"))
abline(0,1,lty = 8)



#Decision Tree
model_c50<-C5.0(Class ~ ., data=data)
pred_train<-predict(model_c50,data)
confusionMatrix(as.factor(data$Class),as.factor(pred_train))





data <- read_csv("C:/Users/ABU TAREQ RONY/OneDrive/Desktop/New Poject Autism/Dataset/Newdata.csv")

##Modelling
#data <- read_csv("C:/Users/ABU TAREQ RONY/OneDrive/Desktop/New Poject Autism/Dataset/Toddler Autism dataset July 2018.csv")
data$Class <- as.factor(data$Class)
inTrain <- createDataPartition(data$Class, p = 0.8, list = FALSE)
autism_train <- data[inTrain, ]
autism_test <- data[-inTrain, ]

data <- read_csv("C:/Users/ABU TAREQ RONY/OneDrive/Desktop/New Poject Autism/Dataset/Toddler Autism dataset July 2018.csv")
data <- read_csv("C:/Users/ABU TAREQ RONY/OneDrive/Desktop/New Poject Autism/Dataset/data.csv")
#data <- scale(data)
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE)
head(data)
#Sex+Ethnicity+Jaundice+
log_fit <- train(Class ~A1+A2+A3+A4+A5+A6+A7+A8+age, data = data,
                 method = "glm",
                 trControl = ctrl)
summary(log_fit)
exp(coef(log_fit))
model<-glm(Class ~A1+A2+A3+A4+A5+A6+A7+A8+Sex+Ethnicity+Jaundice+age, data = data)
summary(model)
exp(coef(model))
