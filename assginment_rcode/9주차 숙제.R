setwd("c:/rdata")
BHouse.df <- read.csv("BostonHousing.csv")

#select variables for regression
selected.var <- c(1, 4, 6, 13)

#partition data
set.seed(1)
dim(BHouse.df)
train.index <- sample(c(1:506), dim(BHouse.df)[1]*0.6)
#train.index <- sample(rownames(BHouse.df), dim(BHouse.df)[1]*0.6)

train.df <- BHouse.df[train.index, selected.var]
valid.df <- BHouse.df[-train.index, selected.var]

#linear regression
str(train.df)
house.lm <- lm(MEDV~ ., data=train.df)
#lm(MEDV ~ 0 + CRIM + CHAS + RM, data = train.df)=>절편을 만들지말고 변수를 선택해서 lm을 만듬
str(house.lm)

options(scipen=999)
summary(house.lm)

library(forecast)
house.lm.pred <- predict(house.lm, valid.df)
accuracy(house.lm.pred, valid.df$MEDV)
accuracy(house.lm$fitted.values, train.df$MEDV)
