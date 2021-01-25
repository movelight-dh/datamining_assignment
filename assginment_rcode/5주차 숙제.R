setwd("c:/Rdata")
Homework.df <- read.csv("Homework.csv", header=TRUE)
library(caret)
library(e1071)

confusionMatrix(as.factor(ifelse(Homework.df$Probability > 0.25, "1", "0")), #예측값
                as.factor(Homework.df$actual), positive = "1") #실제값

confusionMatrix(as.factor(ifelse(Homework.df$Probability > 0.5, "1", "0")), #예측값
                as.factor(Homework.df$actual), positive = "1") #실제값

confusionMatrix(as.factor(ifelse(Homework.df$Probability > 0.75, "1", "0")), #예측값
                as.factor(Homework.df$actual), positive = "1") #실제값

library(gains)
gain <- gains(Homework.df$actual, Homework.df$Probability)
gain
barplot(gain$mean.resp/mean(Homework.df$actual), names.arg=gain$cume.obs,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

library(arules)
Cos.df <- read.csv("Cosmetics.csv", header=TRUE)
incid.Cos.df <- ifelse(Cos.df > 0, 1, 0)
incid.Cos.mat <- as.matrix(incid.Cos.df[ , -1])
incid.Cos.trans <- as(incid.Cos.mat, "transactions")
rules <- apriori(incid.Cos.trans, parameter = list(target = "rules"))
inspect(head(sort(rules, by="lift"), n=3))
