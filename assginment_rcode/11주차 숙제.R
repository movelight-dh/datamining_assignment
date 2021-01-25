setwd("C:/rdata")
bank.df <- read.csv("UniversalBank.csv")
str(bank.df)

###############create dummy variable
bank.df$Education <- factor(bank.df$Education)
edu.df <- as.data.frame(model.matrix(~ 0 + Education, data=bank.df))
edu.df
bank.df <- cbind(bank.df[, -c(1, 5, 8)], edu.df[,])

###############data partition
set.seed(111)
train.index <- sample(row.names(bank.df), dim(bank.df)[1]*0.6)
valid.index <- setdiff(row.names(bank.df), train.index)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[valid.index, ]

###############creat new data
new.df <- data.frame(Age = 40, Experience = 10, Income = 84,
                     Family = 2, CCAvg = 2, Mortgage = 0, Securities.Account = 0,
                     CD.Account = 0, Online = 1, CreditCard = 1,
                     Education1 = 0, Education2 = 1, Education3 = 0)

###############normination
library(caret)
train.norm.df <- train.df
valid.norm.df <- valid.df
bank.norm.df <- bank.df
new.norm.df <- new.df

norm.values <- preProcess(train.df[,-7], method = c("center", "scale"))
train.norm.df[, -7] <- predict(norm.values, train.df[, -7])
valid.norm.df[, -7] <- predict(norm.values, valid.df[, -7])
bank.norm.df[, -7] <- predict(norm.values, bank.df[, -7])
new.norm.df <- predict(norm.values, new.df)

###############knn(k=1)
library(FNN)
nn <- knn(train=train.norm.df[ , -7], test=new.norm.df, cl=train.norm.df[ , 7], k=1, prob=TRUE)
nn
row.names(train.df)[attr(nn, "nn.index")]

###############find optimal k
accuracy.df <- data.frame(k=seq(1,20,1), accuracy=rep(0,20))
for(i in 1:20){
  knn.pred <- knn(train=train.norm.df[ , -7], test=valid.norm.df[ , -7], 
                  cl=train.norm.df[, 7], k=i)
  accuracy.df[i,2]<- confusionMatrix(knn.pred, as.factor(valid.norm.df[, 7]))$overall[1]
}
accuracy.df

###############confusion matrix(k=3, valid)
knn.pred <- knn(train = train.norm.df[, -7], test=valid.norm.df[, -7],
                cl = train.norm.df[, 7], k=3)

conf <- confusionMatrix(knn.pred, as.factor(valid.norm.df[, 7]), positive = '1')
conf

###############knn(k=3, train=bank.norm.df)
library(FNN)
knn.pred.new <- knn(train=bank.norm.df[ , -7], test=new.norm.df, cl=bank.norm.df[ , 7], k=3, prob=TRUE)
knn.pred.new
row.names(bank.df)[attr(knn.pred.new, "nn.index")]

###############data partition & norm
set.seed(111)
train.index <- sample(row.names(bank.df), 0.5*dim(bank.df)[1])
valid.index <- sample(setdiff(row.names(bank.df), train.index), 0.3*dim(bank.df)[1])
test.index <- sample(setdiff(row.names(bank.df), c(train.index,valid.index)))

train.df <- bank.df[train.index, ]
valid.df <- bank.df[valid.index, ]
test.df <- bank.df[test.index, ]

train.norm.df <- train.df
valid.norm.df <- valid.df
test.norm.df <- test.df
new.norm.df <- new.df

train.norm.df[, -7] <- predict(norm.values, train.df[, -7])
valid.norm.df[, -7] <- predict(norm.values, valid.df[, -7])
test.norm.df[, -7] <- predict(norm.values, test.df[, -7])
new.norm.df <- predict(norm.values, new.df)

#train confusion matrix
knn.pred <- knn(train=train.norm.df[ , -7], test=train.norm.df[ , -7], 
                cl=train.norm.df[, 7], k=3)

conf.train <-confusionMatrix(knn.pred, as.factor(train.norm.df[, 7]), positive = '1')
conf.train

# valid confusion matrix
knn.pred <- knn(train=train.norm.df[ , -7], test=valid.norm.df[ , -7], 
                cl=train.norm.df[, 7], k=3)

conf.valid <-confusionMatrix(knn.pred, as.factor(valid.norm.df[, 7]), positive = '1')
conf.valid

# test confusion matirx
knn.pred <- knn(train=train.norm.df[ , -7], test=test.norm.df[ , -7], 
                cl=train.norm.df[, 7], k=3)

conf.test <- confusionMatrix(knn.pred, as.factor(test.norm.df[, 7]), positive = '1')
conf.test
