setwd("c:/rdata")
pharma.df <- read.csv("Pharmaceuticals.csv")

####################
row.names(pharma.df) <- pharma.df[,1]
str(pharma.df)

####################
pharma.df.norm <- sapply(pharma.df[,3:11], scale)
pharma.df.norm
row.names(pharma.df.norm) <- row.names(pharma.df)

#############################
pkgs <- c("factoextra",  "NbClust")
install.packages("factoextra")
install.packages("NbClust")
library(factoextra)
library(NbClust)
fviz_nbclust(pharma.df.norm, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

#############################
km <- kmeans(pharma.df.norm, 4)

#############################
centers <- aggregate( . ~ km$cluster, data = pharma.df[,3:11], FUN = mean)
centers

#############################
table(pharma.df$Median_Recommendation, km$cluster)
table(pharma.df$Location, km$cluster)
table(pharma.df$Exchange, km$cluster)
