data <- read_csv("C:/Users/ABU TAREQ RONY/OneDrive/Desktop/New Poject Autism/Dataset/data.csv")
head(data)
data$Class <- as.factor(data$Class)
inTrain <- createDataPartition(data$Class, p = 0.8, list = FALSE)
autism_train <- data[inTrain, ]
autism_test <- data[-inTrain, ]
model_1 <- glm(Class~A1+A2+A3+A4+A5+A6+A7+A8+age+Sex+Jaundice, data = autism_train, family = binomial())
summary(model_1)
exp(coef(model_1))

exp(1.7731 )


pred_log <- predict(model_1, newdata = autism_test)
confusionMatrix(pred_log, autism_test$Class)


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
varImp(model_naive)





# First table
CYL_table = table(data$Class)

# Table calculating percentages
CYL_tableProb = CYL_table/nrow(data)*100
pie1 = pie(CYL_table, 
           labels = CYL_table, 
           radius = 0.8)
pieLabels = paste(unique(sort(data$Class)),
                  "Autism",
                  "\n",
                  "n=",
                  sort(CYL_table), 
                  "\n", 
                  round(CYL_tableProb,1),
                  "%")

# par(mar) to change margins.
par(mar=c(0.1, 0.1, 0.1, 0.1))

pie(CYL_table,
    labels = pieLabels,
    radius = 0.6,
    border = "yellow",
    lty = 1,
    cex=0.9,
    font = 3)

pie(CYL_table,
    labels = pieLabels,
    radius = 0.6,
    col = terrain.colors(3),
    border = "blue",
    lty = 1,
    cex=0.8,
    font = 3, ,main="Pie Chart of Autism Class")

