setwd("c:/rdata")
BHouse.df <- read.csv("BostonHousing.csv")

cor(BHouse.df[ , c("INDUS", "NOX", "TAX")])

cor(BHouse.df[ , -14])

over0.75<-(cor(BHouse.df[ , -14]) > 0.75 & cor(BHouse.df[ , -14]) < -0.75)
over0.75

##############################################
#partition data
set.seed(1)

train.index <- sample(c(1:506), dim(BHouse.df)[1]*0.6)
train.df <- BHouse.df[train.index, c(-5,-9,-14)]
valid.df <- BHouse.df[-train.index, c(-5,-9,-14)]
str(train.df)

############################################exhaustive search
library(leaps)

search <- regsubsets(MEDV ~ ., data = train.df, nbest = 1,
                     nvmax = dim(train.df)[2], method = "exhaustive")
sum <- summary(search)
sum$which
sum$rsq
sum$adjr2

#############################################first
selected.var <- c(-5,-9,-10,-14)
set.seed(1)
train.index <- sample(c(1:506), dim(BHouse.df)[1]*0.6)
train.df <- BHouse.df[train.index, selected.var]
valid.df <- BHouse.df[-train.index, selected.var]

house.lm.first <- lm(MEDV~., data=train.df)
options(scipen = 999)
summary(house.lm.first)

#############################################second
selected.var <- c(-5,-7,-9,-10,-14)
set.seed(1)
train.index <- sample(c(1:506), dim(BHouse.df)[1]*0.6)
train.df <- BHouse.df[train.index, selected.var]
valid.df <- BHouse.df[-train.index, selected.var]

house.lm.second <- lm(MEDV~., data=train.df)
options(scipen = 999)
summary(house.lm.second)

#############################################third
selected.var <- c(-1,-5,-7,-9,-10,-14)
set.seed(1)
train.index <- sample(c(1:506), dim(BHouse.df)[1]*0.6)
train.df <- BHouse.df[train.index, selected.var]
valid.df <- BHouse.df[-train.index, selected.var]

house.lm.third <- lm(MEDV~., data=train.df)
options(scipen = 999)
summary(house.lm.third)

#############################################ME, RMSR
library(forecast)
selected.var <- c(-5,-9,-10,-14)
set.seed(1)
train.index <- sample(c(1:506), dim(BHouse.df)[1]*0.6)
train.df <- BHouse.df[train.index, selected.var]
valid.df <- BHouse.df[-train.index, selected.var]

house.lm.first.pred <- predict(house.lm.first, valid.df)
accuracy(house.lm.first.pred, valid.df$MEDV)

selected.var <- c(-5,-7,-9,-10,-14)
set.seed(1)
train.index <- sample(c(1:506), dim(BHouse.df)[1]*0.6)
train.df <- BHouse.df[train.index, selected.var]
valid.df <- BHouse.df[-train.index, selected.var]

house.lm.second.pred <- predict(house.lm.second, valid.df)
accuracy(house.lm.second.pred, valid.df$MEDV)

selected.var <- c(-1,-5,-7,-9,-10,-14)
set.seed(1)
train.index <- sample(c(1:506), dim(BHouse.df)[1]*0.6)
train.df <- BHouse.df[train.index, selected.var]
valid.df <- BHouse.df[-train.index, selected.var]

house.lm.third.pred <- predict(house.lm.third, valid.df)
accuracy(house.lm.third.pred, valid.df$MEDV)

#gain library####################
library(gains)

############Lift chart(first) 작성
selected.var <- c(-5,-9,-10,-14)
set.seed(1)
train.index <- sample(c(1:506), dim(BHouse.df)[1]*0.6)
train.df <- BHouse.df[train.index, selected.var]
valid.df <- BHouse.df[-train.index, selected.var]

gain <- gains(valid.df$MEDV[!is.na(house.lm.first.pred)], house.lm.first.pred[!is.na(house.lm.first.pred)])
gain

options(scipen = 999)
price <- valid.df$MEDV[!is.na(valid.df$MEDV)]
plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs),
     xlab="#cases", ylab="Cummulative Price", main="Lift chart(fisrt)", type="l")
lines(c(0,sum(price))~c(0,dim(valid.df)[1]),col="RED",lty=2)

############Lift chart(second) 작성
selected.var <- c(-5,-7,-9,-10,-14)
set.seed(1)
train.index <- sample(c(1:506), dim(BHouse.df)[1]*0.6)
train.df <- BHouse.df[train.index, selected.var]
valid.df <- BHouse.df[-train.index, selected.var]

gain <- gains(valid.df$MEDV[!is.na(house.lm.second.pred)], house.lm.second.pred[!is.na(house.lm.second.pred)])
gain

options(scipen = 999)
price <- valid.df$MEDV[!is.na(valid.df$MEDV)]
plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs),
     xlab="#cases", ylab="Cummulative Price", main="Lift chart(second)", type="l")
lines(c(0,sum(price))~c(0,dim(valid.df)[1]),col="RED",lty=2)

###########3#Lift chart(third) 작성
selected.var <- c(-1,-5,-7,-9,-10,-14)
set.seed(1)
train.index <- sample(c(1:506), dim(BHouse.df)[1]*0.6)
train.df <- BHouse.df[train.index, selected.var]
valid.df <- BHouse.df[-train.index, selected.var]

gain <- gains(valid.df$MEDV[!is.na(house.lm.third.pred)], house.lm.third.pred[!is.na(house.lm.third.pred)])
gain

options(scipen = 999)
price <- valid.df$MEDV[!is.na(valid.df$MEDV)]
plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs),
     xlab="#cases", ylab="Cummulative Price", main="Lift chart(third)", type="l")
lines(c(0,sum(price))~c(0,dim(valid.df)[1]),col="RED",lty=2)