setwd("c:/rdata")
housing.df <- read.csv("WestRoxbury.csv", header = TRUE)

set.seed(1)
#훈련데이터#
train.rows <- sample(row.names(housing.df), dim(housing.df)[1]*0.5)

#검증데이터#
valid.rows <- sample(setdiff(rownames(housing.df), train.rows), dim(housing.df)[1]*0.3)

#평가데이터#
test.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))

#create the three data frame#
train.data <- housing.df[train.rows, ]
valid.data <- housing.df[valid.rows, ]
test.data <- housing.df[test.rows, ]

#linear model#
reg <- lm(TOTAL.VALUE ~ .-TAX, data = housing.df, subset = train.rows)
pred.valid <- predict(reg, newdata = valid.data)
pred.test <- predict(reg, newdata = test.data)

library(forecast)
accuracy(reg$fitted.values, train.data$TOTAL.VALUE)
accuracy(pred.valid, valid.data$TOTAL.VALUE)
accuracy(pred.test, test.data$TOTAL.VALUE)
