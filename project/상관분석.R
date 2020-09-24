data <- read.csv(file="C:/Project/동합치기_합본.csv", header=T)
data
names(data) <- c("place","y","wifi","card","company")
x1 <- data[,-1]
x1
x <- x1[,1:4]
head(x)

install.packages("cluster")
library(cluster)
dist <- dist(x, method="euclidean")
hc <- hclust(dist)
plot(hc)
rect.hclust(hc, k=3, border = "red")

ghc <- cutree(hc,k=3)
ghc
x$ghc <- ghc
table(x$ghc)
g1 <- subset(x, ghc==1)
summary(g1[1:4])
g2 <- subset(x, ghc==2)
summary(g2[1:4])
g3 <- subset(x,ghc==3)
summary(g3[1:4])

install.packages("ggplot2")
library(ggplot2)
t <- sample(1:nrow(x), 19)
test <- x[t,]
dim(test)
test

mydia <- test[c("y","wifi","card","company")]
head(mydia)

result <- hclust(dist(mydia), method="ave")
result
plot(result, hang=-1)

result2 <- kmeans(mydia, 3)
names(result2)
result2$cluster

mydia$cluster <- result2$cluster
head(mydia)

cor(mydia[,], method="pearson")
plot(mydia[,])

install.packages("mclust")
library(mclust)
install.packages("corrgram")
library(corrgram)
corrgram(mydia[,1:4], upper.panel = panel.conf)
#corrgram(mydia[,-5], lower.panel = panel.conf)

plot(mydia$company, mydia$y, col=mydia$cluster)
points(result2$centers[,c("company", "y")], col=c(1,2,3), pch =23, cex=3)

