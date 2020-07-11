library(randomForest)
library(MASS)
library(caret)
# USe the set.seed function so that we get same results each time 
set.seed(123)


CompanyData <- read.csv(file.choose())
hist(CompanyData$Sales, main = "Sales of Companydata",xlim = c(0,20),
     breaks=c(seq(10,20,30)), col = c("blue","red", "green","violet"))
highsales = ifelse(CompanyData$Sales<9, "No", "Yes")  # if greater than 8 then high sales else Low
CD = data.frame(CompanyData[2:11], highsales)
str(CD)
table(CD$highsales)
# Data Partition
set.seed(123)
ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))
train <- CD[ind==1,]
test  <- CD[ind==2,]
set.seed(213)
rf <- randomForest(as.factor(highsales)~., data=train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting
attributes(rf)
# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1)
head(train$highsales)
# looks like the first six predicted value and original value matches.

confusionMatrix(as.factor(pred1),as.factor( train$highsales))   # 100 % accuracy on training data 
pred2 <- predict(rf, test)
confusionMatrix(as.factor(pred2), as.factor(test$highsales))
plot(rf)
tune <- tuneRF(train[,-11], as.factor(train[,11]), stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
rf1 <- randomForest(as.factor(highsales)~., data=train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
rf1
pred1 <- predict(rf1, train)
confusionMatrix(as.factor(pred1), as.factor(train$highsales))  # 100 % accuracy on training data 
pred2 <- predict(rf1, test)
confusionMatrix(as.factor(pred2), as.factor(test$highsales)) # 84.35 % accuracy on test data 
hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")
varImpPlot(rf1)
varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
# Quantitative values 
importance(rf1)

varUsed(rf)
# Partial Dependence Plot 
partialPlot(rf1, train, Price, "Yes")

getTree(rf, 1, labelVar = TRUE)
MDSplot(rf1, CD$highsales)

