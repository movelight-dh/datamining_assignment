setwd("c:/rdata")
accidents.df <- read.csv("accidents.csv")

accidents.df$INJURY <- ifelse(accidents.df$MAX_SEV_IR>0, "yes", "no")

for(i in c(1:dim(accidents.df)[2])){
  accidents.df[,i] <-as.factor(accidents.df[,i])
}

prop.table(table(accidents.df$INJURY))

table(accidents.df[1:12, c("INJURY", "WEATHER_R", "TRAF_CON_R")])

head(accidents.df[, c("INJURY", "WEATHER_R", "TRAF_CON_R")], 12)

library(e1071)
accidents.nb <- naiveBayes(INJURY ~ WEATHER_R + TRAF_CON_R , data=accidents.df[1:12, c('INJURY', 'WEATHER_R', 'TRAF_CON_R')])
accidents.nb

pred.class <- predict(accidents.nb, newdata=accidents.df[1:12, c('INJURY', 'WEATHER_R', 'TRAF_CON_R')])
pred.class

library(caret)
confusionMatrix(pred.class, accidents.df[1:12, 'INJURY'], positive = 'yes')

##################################
selected.var  <- c(25,1,2,8,15,16,17,19)
train.index <- sample(c(1:dim(accidents.df)[1]), dim(accidents.df)[1]*0.6)
train.df <-accidents.df[ train.index, selected.var]
valid.df <-accidents.df[ - train.index, selected.var]

accidents.nb <- naiveBayes(INJURY ~ ., data=train.df)
pred.class <- predict(accidents.nb, newdata = valid.df)
pred.class

confusionMatrix(pred.class, valid.df$INJURY, positive = 'yes')
