#가중평균
weighted_mean1 <- (200*0.09 + 1000*0.21)/(200+1000)
weighted_mean1

alcohol <- data.frame(volume=c(200,1000), alcohol_rate=c(0.09,0.21))
weighted_mean2 <- weighted.mean(alcohol$alcohol_rate, alcohol$volume)
weighted_mean2


#변이추정 - 데이터집합의 변이를 계산
x <- c(1,2,3,3,5,6,7,9)
mean(x) #평균
median(x) #짝수일때 두값의 평균
max(x) #최대값
min(x) #최소값
summary(x) #요약
var(x) #분산
sd(x) #표준편차
IQR(x) #사분위범위
mad(x) # 중위절대편차
quantile(x, p=c(.05, .25, .5, .75, .95))

state <- read.csv("c:/r/state.csv")
mean(state[["Population"]])
mean(state[["Population"]], trim=0.1) # 절사평균, 각 끝의 10% 제외외
median(state[["Population"]]) #중간값값
weighted.mean(state[["Murder.Rate"]], w=state[["Population"]]) #가중평균

#데이터 분포 탐색
#R 함수 이용한 백분위수 생성 예
quantile(state[["Population"]], p=c(.05,.25,.5,.75,.95)) #백분위수

#R 함수 이용한 상자그림 생성 예
boxplot(state[["Population"]]/1000000, ylab="Population (millions)")

##도수분포표
#데이터준비
breaks <- seq(from=min(state[["Population"]]), to=max(state[["Population"]]), length=11)
pop_freq <- cut(state[["Population"]], breaks=breaks, right = TRUE, include.lowest = TRUE)
table(pop_freq)

#히스토그램은 도수분포표를 시각화하는 방법 - R의 hist()함수를 이용한 히스토그램 생성 예
hist(state[["Population"]], breaks=breaks)

x<-c(1,2,3,3,5,6,7,9)
breaks1 <- seq(from=min(x), to=max(x), length=5)
pop_freq1 <- cut(x, breaks=breaks1, right = TRUE, include.lowest = TRUE)
#도수분포표 생성
table(pop_freq1)
#히스토그램
hist(x, breaks=breaks1)


##상관관계
#R의 cor()함수로 상관 행렬 생성 예
sp500_px <- read.csv("c:/r/sp500_0.csv")
sp500_sym <- read.csv("c:/r/sp500_sym.csv", stringsAsFactors = FALSE)

telecom <- sp500_px[, sp500_sym[sp500_sym$sector=="telecommunications_services", "symbol"]]
telecom <- telecom[row.names(telecom)>"2012-07-01", ]
#상관행렬
telecom_cor <- cor(telecom)
telecom_cor

#상관행렬 - R의 corrplot 패키지의 corrlpot()함수를 이용한 시각화
install.packages("corrplot")
library(corrplot)
corrplot(cor(telecom), method="circle")

#R의 plot()함수를 이용하여 산점도 그래프 생성 예
telecom <- sp500_px[, sp500_sym[sp500_sym$sector=="telecommunications_sercices", 'symbol']]
telecom <- telecom[row.names(telecom)>"2012-07-01", ]
#산점도 그래프
plot(telecom$T, telecom$VZ, xlab="T", ylab="VZ")
cor(telecom$T, telecom$VZ)
