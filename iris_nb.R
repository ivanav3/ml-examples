data(iris)

data <- iris
iris <- NULL

str(data)
summary(data)

apply(data,2,function(x) sum(is.na(x)))
apply(data,2,function(x) sum(x=="",na.rm=T))
apply(data,2,function(x) sum(x==" ",na.rm=T))
apply(data,2,function(x) sum(x=="-",na.rm=T))


apply(data[,-ncol(data)],2,function(x)shapiro.test(x))

# None of the values is normally distributed, that's why we discretize them.
library(bnlearn)

data[,-ncol(data)] <- discretize(data[,-ncol(data)],method="quantile",breaks=c(5,5,5,5))
summary(data)

library(caret)

set.seed(33)
indices <- createDataPartition(data$Species, p = 0.8, list = FALSE)
train.data <- data[indices,]
test.data <- data[-indices,]

library(e1071)
nb1 <- naiveBayes(Species ~ ., data = train.data)

nb1.pred <- predict(nb1, newdata = test.data)

confusion_matrix <- table(actual=test.data$Species,predicted=nb1.pred)
confusion_matrix

evaluation_metrics <- function(cm){
  TP <- cm[1,1]
  FP <- c(cm[2,1],cm[3,1])
  
  TN <- c(cm[2,2],cm[3,3])
  FN <- c(cm[1,2],cm[3,2],cm[1,3],cm[2,3])
  
  acc <- sum(diag(cm)) / sum(cm)
  precision <- TP / (TP + sum(FP))
  recall <- TP / (TP + sum(FN))
  F1 <- 2*precision*recall / (precision + recall)
  
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}

evaluation_metrics(confusion_matrix)
