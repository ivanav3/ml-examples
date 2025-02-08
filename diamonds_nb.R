library(ggplot2)

data(diamonds)
data <- diamonds
diamonds <- NULL

str(data)
summary(data)

data$expensive <- ifelse(data$price>quantile(data$price,0.75),"Yes","No")
data$price <- NULL
data$expensive <- as.factor(data$expensive)

apply(data,2,function(x) sum(is.na(x)))
apply(data,2,function(x) sum(x=="",na.rm=T))
apply(data,2,function(x) sum(x==" ",na.rm=T))
apply(data,2,function(x) sum(x=="-",na.rm=T))

apply(head(data[,-c(2,3,4,10)],5000),2,function(x)shapiro.test(x))


data[,-c(2,3,4,10)] <- discretize(data[,-c(2,3,4,10)],method="quantile",breaks=c(5,5,5,5,5,5))

library(caret)

set.seed(33)
indices <- createDataPartition(data$expensive, p = 0.8, list = FALSE)
train.data <- data[indices,]
test.data <- data[-indices,]

library(e1071)
nb1 <- naiveBayes(expensive ~ ., data = train.data)

nb1.pred <- predict(nb1, newdata = test.data)

confusion_matrix <- table(actual=test.data$expensive,predicted=nb1.pred)
confusion_matrix

compute.eval.metrics <- function(cm) {
  
  TP <- cm[2,2] 
  TN <- cm[1,1] 
  FP <- cm[1,2] 
  FN <- cm[2,1]
  
  acc <- (TP+TN) / sum(cm)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}

nb1.eval <- compute.eval.metrics(confusion_matrix)
nb1.eval

nb2.pred.prob <- predict(nb1,test.data,type="raw")
nb2.pred.prob

summary(nb2.pred.prob[,2])

library(pROC)
nb2.roc <-roc(
  response=as.numeric(test.data$expensive),
  predictor=nb2.pred.prob[,2],
  levels=c(2,1))

nb2.coords <-  coords(nb2.roc, 
                      ret = c("spec", "sens", "thr"),
                      x = "local maximas", transpose = FALSE)

plot.roc(nb2.roc,
         print.thres = T,
         print.thres.best.method = "youden")

nb2.coords

prob.thr <- 0.249

nb2.pred2 <- ifelse(test = nb2.pred.prob[,2] >= prob.thr, 
                    yes = "Yes", 
                    no = "No")
nb2.pred2 <- as.factor(nb2.pred2)

nb2.cm2 <- table(actual = test.data$expensive, predicted = nb2.pred2)
nb2.cm2

nb2.eval2 <- compute.eval.metrics(nb2.cm2)
nb2.eval2