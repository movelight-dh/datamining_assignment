setwd("C:/rdata")
rating <- read.csv("c:/rdata/courserating.csv")
rating[,1]
row.names(rating) <- rating[,1] 
row.names(rating)

m <- as.matrix(rating[, -1])
mm <- m[c(1,4,15),]
library(recommenderlab)
r <- as(m, "realRatingMatrix")
rr <- as(mm, "realRatingMatrix")
?Recommender

UB.Rec.pea <- Recommender(r, "UBCF", parameter="pearson")
pred <- predict(UB.Rec.pea, r, type="ratings")
pmmm <- as(pred, "matrix")
pmmm

similarity(rr, method = "cosine")

UB.Rec.cos <- Recommender(r, "UBCF")
pred <- predict(UB.Rec.cos, r, type="ratings")
cmmm <- as(pred, "matrix")
cmmm

IB.Rec <- Recommender(r, "IBCF")
ipred <- predict(IB.Rec, r, n=5, type = "topNList")
immm <- as(ipred, "matrix")
iimmm <- as(ipred, "list")
immm
iimmm
