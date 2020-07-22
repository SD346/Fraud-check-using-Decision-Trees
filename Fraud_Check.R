Fraud_check <- read.csv(choose.files())
View(Fraud_check)
#As per the condition, we need to consider individuals who have taxable_income <= 30000 as "Risky" and others are "Good"
#Hence we have to make Taxable_Income as categorical.
Cat_Taxable.Income <- cut(Fraud_check$Taxable.Income, breaks = c(0,30000,100000),labels = c("Risky","Good"))
Fraud_check1 <- cbind(Fraud_check[,-3],Cat_Taxable.Income)
Fraud_check1 <- Fraud_check1[,c(6,1:5)] #Arranging the Cat_Taxable.Income column
View(Fraud_check1)

#Splitting the data into 80% traindata and 20% testdata
part <- sample(nrow(Fraud_check1),nrow(Fraud_check1)*0.8,replace = FALSE)
train <- Fraud_check1[part,]
test <- Fraud_check1[-part,]

library(C50)
#Building model with traindata
model <- C5.0(train[,-1],train$Cat_Taxable.Income)
plot(model)
windows()
#Checking accuracy with traindata
pred <- predict(model,train)
mean(pred==train$Cat_Taxable.Income)#80.42% accuracy
library(caret)
confusionMatrix(train$Cat_Taxable.Income,pred)

#Computing Prediction model accuracy with testdata
pred1 <- predict(model,test)
mean(pred1==test$Cat_Taxable.Income )#75% accuracy
confusionMatrix(test$Cat_Taxable.Income,pred1)
