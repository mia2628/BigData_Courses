data <- read.csv(file="C:/Project/동합치기_합본.csv", header=T)
data
names(data) <- c("place","y","wifi","card","company","population","group")
x <- data[,-1]
x <- x[,-6]
head(x)
y = data$group
kc <- kmeans(x, 2)
kc
table(y, kc$cluster)
plot(x[c("company", "y")], col=kc$cluster)
points(kc$centers[,c("company", "y")], col=1:3, pch=23, cex=3)
