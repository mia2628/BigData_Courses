data <- read.csv(file="C:/Project/최종.CSV", header=T)
data
names(data) <- c("place","y","wifi","card","calculate")
data1 <- data[, 2:5]
head(data1) #정규화가 됨
#군집갯수 결정
install.packages("NbClust") #NbClust 를 사용 해서 k 값을 얻을수 있다.
library(NbClust)
nc <-NbClust(data1,min.nc = 2,max.nc = 10,method = 'kmeans')
nc
plot(table(nc$Best.nc[1,]))

#k-means모델
data1.k <- kmeans(data1, centers = 3, iter.max =100 )  # centers = 3(best number) iter.max =(최대 반복수)
data1.k

#confusion matrix
table(data1$y,data1.k$cluster)

data1
#시각화
#k-means 결과
plot(data1[-5],col=data1.k$cluster)
text(data1$speed[10] + 8, data1$dist[10], labels="", pos=4)
#실제값
plot(data1[-5],col=data1$y)

par(mfrow=c(1,1)) #그래프 1개만 보기 혹은 2개 보기
#k-means 결과
plot(data1[c("wifi","y")],col=data1.k$cluster, pch=20,cex=4,  xlab="와이파이접속자수", cex.lab = 2, ylab="주차수요량")
#실제값
plot(data1[c("wifi","y")],col=data1$y)

library(MASS)
legend(x = 3500, y = 50,
pch = c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
)