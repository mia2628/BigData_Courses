#======================p113
# 평균, 중앙값, 분산, 표준편차
score <- c(85,90,93,86,82)
mean(score) #평균값
median(score) # 중앙값
var(score)  # 분산
sd(score)   # 표준편차

#======================p114
#데이터프레임 : 평균, 편차, 표준 편차
name <- c('주시현','최경우','이은주','허민성','홍미나')
height <- c(168,176,167,174,169)
weight <- c(52,68,47,82,51)

df <- data.frame(이름=name,키=height,몸무게=weight)

attach(df)

mean(키)
median(키)
mean(몸무게)
sd(키)
sd(몸무게)


mean(df$키)
median(df$키)
mean(df$몸무게)
sd(df$키)
sd(df$몸무게)


#======================p117
#정규분포
height <- rnorm(n=1000000,mean=168,sd=7) # 백만건, 평균 168, 표준편차 7
hist(height,breaks=10,probability=T)  #breaks는 막대개수,probability는 상대도수(F : y축값 실제값) 
hist(height,breaks=100,probability=T)  #breaks는 막대개수,probability는 상대도수 
hist(height,breaks=1000,probability=T)  #breaks는 막대개수,probability는 상대도수 


#======================p121
#t검정
score1 <-read.csv("c:/r/tdata.csv", header=T, stringsAsFactors=T)
score1

# 정규분포를 따르는지 확인
shapiro.test(score1$성적)
# p-value가 0.05보다 높은 0.9이다. 
# 따라서 정규분포를 따르고, 이 데이터에 대해 t-test를 할 수 있다


result <- t.test(score1$성적,alternative=c("greater"),mu=75)

result


#======================p123
score1 <-read.csv("c:/r/tdata2.csv", header=T, stringsAsFactors=T)
score1

# 정규분포를 따르는지 확인
shapiro.test(score1$성적)
# p-value가 0.05보다 높은 0.8344이다. 
# 따라서 정규분포를 따르고, 이 데이터에 대해 t-test를 할 수 있다


result <- t.test(score1$성적,alternative=c("greater"),mu=75)
result

# 신약효과 ========p125
x<-c(15,10,13,7,9,8,21,9,14,8) 
y<-c(15,14,12,8,14,7,16,10,15,12) 
shapiro.test(x) 
shapiro.test(y) 

t.test(x,y) 


#======================p126
# 대응표본일 경우( 전 후 관계 )
x<-c(52,60,63,43,46,56,62,50) 
y<-c(58,62,62,48,50,55,68,57)  
shapiro.test(x) 
shapiro.test(y) 
t.test(x,y,paired = TRUE) 






#======================p128
# 분산분석(oneway.test)
x <- c(1.09, 2.12, 2.92, 4.06, 4.90)
y <- c(1,2,3,4,5)
z <- c(1.10, 1.96, 12.98, 4.09, 4.92)
shapiro.test(x) 
shapiro.test(y) 
shapiro.test(z) 

mydata <-c(x,y,z)  #벡터형으로 자료를 생성함
mydata
group <-c(rep(1,5), rep(2,5), rep(3,5))  #처음 5개를 1로, 다음 5개를 2, 다음 5개를 3
group
oneway.test(mydata~group, var=T)  # mydata를 3개로 그룹지어서 평균이 같은지 검증


#======================p129
# 분산분석(oneway.test)
x <- c(1.09, 2.12, 2.92, 4)
x1<-c(23,27,24,25,29,30,26) 
x2<-c(35,32,38,36,32,33,34) 
x3<-c(36,41,38,39,40,38,39) 
x4<-c(32,30,37,34,35,34,32)
shapiro.test(x1) 
shapiro.test(x2) 
shapiro.test(x3) 
shapiro.test(x4) 

mydata <-c(x1,x2,x3,x4)
group <-c(rep(1,7), rep(2,7), rep(3,7),rep(4,7))
oneway.test(mydata~group, var=T) 


#======================p132
# 빈도분석
fruits1 <- read.csv("c:/r/love_fruits.csv",header=T)
head(fruits1)

prop.table(table(fruits1$선호과일)) 
round(prop.table(table(fruits1$선호과일))*100,2)

#======================p133
# 건수와 비율을 하나의 테이블로 완성
table(fruits1$선호과일)
count <- c(table(fruits1$선호과일))
pct <- c(round(prop.table(table(fruits1$선호과일))*100,2))
love_fruits <- data.frame(건수=count,비율=pct)
love_fruits


#======================p134
# 바 차트
barplot(love_fruits$건수, names.arg=c("바나나","복숭아","사과","체리","포도"),
        ylim=c(0,15), col=rainbow(5))


#======================p139
#상관분석
x <- c(3,5,8,11,13)
y <- c(1,2,3,4,5)
cor(x,y)
#상관분석 
cor.test(x,y)


#======================p144
# 회귀분석
x <- c(110,120,130,140,150)
y <- c(100,105,128,115,142)
plot(x,y,pch=20,col="red")
?pch

# pch 점의 모양 ( 1~25)
plot(x,y,pch=0,col="red")
plot(x,y,pch=1,col="red")
plot(x,y,pch=2,col="red")

line <- lm(y~x)
line

abline(line, col="blue")

#======================p146
#단순회귀분석
score <- read.csv("c:/r/score.txt",header=T,sep=",")
attach(score)
score

lm1 <- lm(성적~IQ)
lm1 

y <- -5.292 + 0.671 * 125  # IQ가 125 일 경우 예상 점수 계산하기
y

plot(IQ,성적,pch=20,col="red")
abline(lm1,col="blue")

#======================p147
#단순회귀분석 : 병원직원들의 친절도
x<-c(15,10,13,7,9,8,21,9,14,8) 
y<-c(15,14,12,8,14,7,16,10,15,12) 
out=lm(y~x)
summary(out)


#======================p148
# 다중회귀분석
lm3 <- lm(성적~IQ+다니는학원수+게임하는시간+TV시청시간)
lm3

y = 23.299 + (0.468*130) + (0.718 * 3) - (0.839*2) - (1.385*1)
y

summary(lm3)

#======================p152
# 다중회귀분석 : 인맥관리
x1<-c(100,90,98,79,81,69,80,77,68,54)
x2<-c(5,4,5,3,4,3,2,3,2,1)
x3<-c(5,3,4,3,4,3,2,3,2,1) 
x4<-c(5,3,3,2,3,3,4,3,2,1) 
mydata <-data.frame(y=x1, s1=x2, s2=x3, s3=x4)
mydata 

model <-lm(y~., data=mydata)
summary(model)

#======================p153
# 다중회귀분석 : 고객충성도
x1<-c(100,90,98,79,81,69,80,77,68,74) 
x2<-c(5,4,4,3,4,3,4,3,2,3) 
x3<-c(4,3,3,2,3,2,3,3,2,3) 
x4<-c(5,3,3,2,3,3,4,3,2,3) 
mydata <-data.frame(y=x1, s1=x2, s2=x3, s3=x4)
mydata 

model <-lm(y~., data=mydata)
summary(model)



# 패키지 설치
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("descr")
install.packages("klaR")

library(dplyr)
library(ggplot2)
library(corrplot)
library(descr) 
library(klaR)

##########################################
####### chapter 2
##########################################

#======================= p172

state <- read.csv("C:/r/state.csv")

# population 평균, 절사평균, 중간값 
mean(state[["Population"]])
mean(state[["Population"]], trim=0.1)
median(state[["Population"]])
weighted.mean(state[["Murder.Rate"]], w=state[["Population"]])

# Murder.Rate 평균, 절사평균, 중간값
mean(state[["Murder.Rate"]])
mean(state[["Murder.Rate"]], trim=0.1) # 0.1은 각 끝에서 10% 제외
median(state[["Murder.Rate"]])
weighted.mean(state[["Murder.Rate"]], w=state[["Population"]])

#======================= p174
# 추가 : 가중평균
# 알코올 도수 9% 와인 200ml
# 알코올 도수 21% 소주 1000ml
# 와소 폭타주의 평균 알코올 도수는?

# weighted mean
weighted_mean1 <- (200*0.09 + 1000*0.21)/(200+1000)
weighted_mean1

# weighted mean
alcohol <- data.frame(volume=c(200, 1000), alcohol_rate=c(0.09, 0.21))
weighted_mean2 <- weighted.mean(alcohol$alcohol_rate, alcohol$volume)
weighted_mean2


# 데이터집합의 변이 ======================= p177
x <-c(1,2,3,3,5,6,7,9)
mean(x)
median(x) #짝수일 때 두값의 평균
max(x)
min(x)
summary(x)

var(x)  #분산
sd(x)   # 표준 편차
IQR(x)  # 사분위범위
mad(x)  # 중위절대편차
quantile(x, p=c(.05, .25, .5, .75, .95))


#======================= p178
# quantile 백분위수 생성
quantile(state[["Population"]], p=c(.05, .25, .5, .75, .95)) 
quantile(x, p=c(.05, .25, .5, .75, .95))

summary(state[["Population"]])

boxplot(state[["Population"]]/1000000, ylab="Population (millions)")
boxplot(x)


#======================= p181
# 데이터 준비
breaks <- seq(from=min(state[["Population"]]), to=max(state[["Population"]]), length=11)
pop_freq <- cut(state[["Population"]], breaks=breaks, right=TRUE, include.lowest = TRUE)
state['PopFreq'] <- pop_freq

#도수분포표 생성
table(pop_freq)

# 히스토그램 생성
hist(state[["Population"]], breaks=breaks) 


# 히스토그램 추가 ======================= p183
x <-c(1,2,3,3,5,6,7,9)
breaks1 <- seq(from=min(x), to=max(x), length=5)
pop_freq1 <- cut(x, breaks=breaks1, right=TRUE, include.lowest = TRUE)
# right = TRUE 옵션 : a < x <= b
# include.lowest = TRUE :구성요소 값이 최소값과 같아도 변환

#도수분포표 생성
table(pop_freq1)

# 히스토그램 
hist(x, breaks=breaks1) 

#======================= p184
### 막대그래프
dfw <- read.csv("c:/r/dfw_airline.csv")
barplot(dfw)
barplot(as.matrix(dfw))

#  cex.axis : y축 글자 크기, cex.names : x축 글자 크기
barplot(as.matrix(dfw)/6, cex.axis = 0.8, cex.names = 0.7)

#  ylim : y축 최소값, 최대값 지정
barplot(as.matrix(dfw)/6, ylim=c(0,20000), cex.axis = 0.8, cex.names = 0.7)


#======================= p188
### chapter 2.7
#데이터 준비
sp500_px <- read.csv("c:/r/sp500_0.csv")
sp500_sym <- read.csv("c:/r/sp500_sym.csv", stringsAsFactors = FALSE)


telecom <- sp500_px[, sp500_sym[sp500_sym$sector=="telecommunications_services", 'symbol']]
telecom <- telecom[row.names(telecom)>"2012-07-01", ]

#상관행렬
telecom_cor <- cor(telecom)
telecom_cor

#======================= p189
# corrplot 패키지의 corrplot() 함수를 이용한 상관 행렬 시각화  
library(corrplot)
corrplot(cor(telecom))

# method = c("circle", "square", "ellipse", "number", "shade", "color", "pie") 
corrplot(cor(telecom), method="circle")
corrplot(cor(telecom), method = "square")
corrplot(cor(telecom), method = "ellipse")
corrplot(cor(telecom), method = "number")
corrplot(cor(telecom), method = "shade")
corrplot(cor(telecom), method = "color")
corrplot(cor(telecom), method = "pie")

#======================= p191
plot(telecom$T, telecom$VZ, xlab="T", ylab="VZ")

# 상관계수 제시
# 관계 : <= 0.1 관계없음  <= 0.4 : 약한관계
#        <= 0.7 중간관계  <= 1.0 : 강한 관계    
cor(telecom$T, telecom$VZ)

# 해석 : 상관계수는 약 0.681 으로 두 값 사이에 중간 정도의 상관 관계가 있고,
# AT&T(T) 주가가 상승하면 버라이즌(VZ) 주식도 상승하는 경향이 있음을 알 수 있음


##########################################
####### chapter 3 
##########################################

#======================= p199
### chapter3.3
#데이터 준비
loans_income <- read.csv("c:/r/loans_income.csv")[,1]

# 단순랜덤표본
samp_data <- data.frame(income=sample(loans_income, 1000), 
                        type='data_dist')
str(samp_data)

# 추가 : tapply 연습
# tapply : 요인(factor)의 수준(level)별로 특정 벡터에 함수 명령어를 동시에 적용
# taply(함수사용할 수치, 인수의 index, 적용함수)

samp <- data.frame(name=factor(c("kim","choi","choi","lee","kim","lee")),
                   score=c(50,30,10,50,20,30))

tapply(samp$score, samp$name, length)
tapply(samp$score, samp$name, mean)

#======================= p200
# 5개 표본씩 평균
samp_mean_05 <- data.frame(
  income = tapply(sample(loans_income, 1000*5), 
                  rep(1:1000, rep(5, 1000)), FUN=mean),
  type = 'mean_of_5')

str(samp_mean_05)

# 20개 표본씩 평균
samp_mean_20 <- data.frame(
  income = tapply(sample(loans_income, 1000*20), 
                  rep(1:1000, rep(20, 1000)), FUN=mean),
  type = 'mean_of_20')

# 데이터프레임 rbind, 타입을 factor 변환 
income <- rbind(samp_data, samp_mean_05, samp_mean_20)
str(income)
income$type = factor(income$type, 
                     levels=c('data_dist', 'mean_of_5', 'mean_of_20'),
                     labels=c('Data', 'Mean of 5', 'Mean of 20'))

#======================= p201
# 갯수 확인
table(income$type)

# plot the histograms
library(ggplot2)

# ggplot(데이터, aes(x = x축변수, y = y축변수))
# facet_grid(type ~ .) : 집단간 비교를 위한 면 분할

ggplot(income, aes(x=income)) +
  geom_histogram(bins=40) +              # 막대개수
  facet_grid(type ~ .)                   # 가로분할

# 옵션 수정 bins=400,  facet_grid(. ~ type) 
ggplot(income, aes(x=income)) +
  geom_histogram(bins=400) +             # 막대개수
  facet_grid(. ~ type)                   # 세로분할


#======================= p204
### chapter3.4

# boot패키지의 boot() 함수를 이용한 부트스트랩 
library(boot)
loans_income <- read.csv("c:/r/loans_income.csv")[,1]

summary(loans_income)

stat_fun <- function(x, idx) median(x[idx])
boot_obj <- boot(loans_income, R = 1000, statistic=stat_fun) # 부트스트랩

boot_obj

#############################
## 문제 풀이
#############################
# p156 정답

#문제1. 
x1<-c(65,87,73,79,81,69,80,77,68,74) 
x2<-c(75,69,83,81,72,79,90,88,76,82) 
x3<-c(59,78,67,62,83,76,55,75,49,68) 
x4<-c(94,89,80,88,90,85,79,93,88,85) 
shapiro.test(x1) 
shapiro.test(x2) 
shapiro.test(x3) 
shapiro.test(x4)
mydata <-c(x1,x2,x3,x4)
group <-c(rep(1,10), rep(2,10), rep(3,10),rep(4,10))
oneway.test(mydata~group, var=T) 

# p-value = 2e-05  
# 유의확률이 p=2e-05<0.05므로 귀무가설 기각
# 교육방법의 효과가 동일하다고 볼 수 없다.

#문제 2.
x<-c(52,60,63,43,46,56,62,50) 
y<-c(58,62,62,48,50,55,68,57)  
shapiro.test(x) 
shapiro.test(y) 

t.test(x,y,paired = TRUE) 

# p-value = 0.02  
# 유의확률이 p=0.02<0.05이므로 귀무가설을 기각하여, 국어성적 향상에 교육이 효과적이다.


# 문제 3.
pre<-c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3) 
post <-c(14.0,8.8,11.2,14.2,11.8,6.4,9.8,11.3,9.3,13.6) 
shapiro.test(pre)
shapiro.test(post)

t.test(pre,post, paired = TRUE)

summary(pre)
summary(post)

#p-value = 0.009 
# 유의수준보다 작아서 대립가설 채택
# 성형전 10.63에서 성형후 11.04로 전체적으로 0.41 증가하였다.

#문제 4.
x<-c(15,10,13,7,9,8,21,9,14,8) 
y<-c(15,14,12,8,14,7,16,10,15,12) 
shapiro.test(x) 
shapiro.test(y) 

t.test(x,y) 

# p-value = 0.6  
# 유의확률이 p=0.6>0.05므로 귀무가설을 기각하지 못하여, 신약의 효과가 있다고 주장할 수 없다.

# 문제 5.
x1<-c(23,27,24,25,29,30,26) 
x2<-c(35,32,38,36,32,33,34) 
x3<-c(36,41,38,39,40,38,39) 
x4<-c(32,30,37,34,35,34,32)
shapiro.test(x1) 
shapiro.test(x2) 
shapiro.test(x3) 
shapiro.test(x4) 

mydata <-c(x1,x2,x3,x4)
group <-c(rep(1,7), rep(2,7), rep(3,7),rep(4,7))
oneway.test(mydata~group, var=T) 
#p-value = 3e-09  
#유의확률이 p=3e-09<0.05므로 귀무가설 기각
#제품이 4가지 온도에 따라 강도의 차이가 있다고 볼 수 있다

# 문제 6.
x<-c(15,10,13,7,9,8,21,9,14,8) 
y<-c(15,14,12,8,14,7,16,10,15,12) 
out=lm(y~x)
summary(out)

# 회귀식 : Y=6.359+ 0.52x

# p-value = 0.0158  
# 유의확률이 p=0.0158<0.05므로 귀무가설을 기각, 친절도는 만족도에 유의한 영향을 미친다. 47% 신뢰도를 가진다.



#문제 7.
x1<-c(100,90,98,79,81,69,80,77,68,54)
x2<-c(5,4,5,3,4,3,2,3,2,1)
x3<-c(5,3,4,3,4,3,2,3,2,1) 
x4<-c(5,3,3,2,3,3,4,3,2,1) 
mydata <-data.frame(y=x1, s1=x2, s2=x3, s3=x4)
mydata 

model <-lm(y~., data=mydata)
summary(model)
# 회귀식 : Y=s1+s2+s3
#결과해석  유의확률 0.00102로 회귀식은 유의
# 88%의 신뢰도를 가진다.
# 세부항목중 인성과 성격이 유의한 영향을 미친다.

#문제 8.
x1<-c(100,90,98,79,81,69,80,77,68,74) 
x2<-c(5,4,4,3,4,3,4,3,2,3) 
x3<-c(4,3,3,2,3,2,3,3,2,3) 
x4<-c(5,3,3,2,3,3,4,3,2,3) 
mydata <-data.frame(y=x1, s1=x2, s2=x3, s3=x4)
mydata 

model <-lm(y~., data=mydata)
summary(model)
# 회귀식 : Y=s1+s2+s3
# 결과해석  유의확률 0.01178로 회귀식은 유의하다. 
# 73%의 신뢰도를 가진다.
# 세부항목중 가격만이 유의한 영향을 미친다.

#문제 9.
a <- c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)
shapiro.test(a)  
t.test(a, alternative=c("greater"), mu=55)


# 유의수준 0.05로 판단할 경우, p-value가 0.05를 넘은 0.4046 이다
#그러므로 귀무가설을 채택한다
#귀무가설 : 안올랐다 (모집단의 평균이 55점이다)     
# 그러므로 성적이 오르지 않았다는 것을 알 수 있다.




##########################################
####### chapter 4 
##########################################

#======================= p219

# 세 종류 데이터  
data1 <- c(30, -5, 55, -30, -20, 45)
data2 <- c(12, 13, 12, 13, 12, 13)
data3 <- c(30, -5, 55, -30, -20, 45, 30, -5, 55, -30, -20, 45)

# t-검정
t.test(data1, alternative = c("greater"))
# t-검정
t.test(data2, alternative = c("greater"))
# t-검정
t.test(data3, alternative = c("greater"))


#======================= p222

# A 페이지에 사용자가 머무르는 시간이 B 페이지보다 유의미하게 작은 지 검정   

session_times <- read.csv("c:/r/web_page_data.csv")
head(session_times)

t.test(Time ~ Page, data=session_times, alternative='less' )
#  Time 종속변수 , Page 설명변수,  데이터는 session_times
# 해석 : 유의수준 0.05보다 크므로 귀무가설 채택

#======================= p229
### chapter 4.7 분산분석
four_sessions  <- read.csv("c:/r/four_sessions.csv")
summary(aov(Time ~ Page, data=four_sessions))

oneway.test(four_sessions$Time ~ four_sessions$Page, var=T) 


#======================= p232
### chapter 4.9 카이제곱검정
click_rate <-  read.csv("c:/r/click_rates.csv")
clicks <- matrix(click_rate$Rate, nrow=3, ncol=2, byrow=TRUE)

head(clicks)
chisq.test(clicks, simulate.p.value=TRUE)


# S======================= 참고 p28-44

#======================= p239
# 카이제곱검정 - 적합도 검정 
obs <- c(20, 40, 40) 
obs.probs <- c(2/10, 3/10, 5/10)
(g.fit <- chisq.test(obs, p=obs.probs))

# 가설설정
# 귀무가설 H0 : 관측값의 도수와 가정한 이론 도수(기대 관측도수)가 동일하다
# 대립가설 H1 : 적어도 하나의 범주 (혹은 계급)의 도수가 가정한 이론 도수(기대 관측도수)와 다르다


# 분석결과 
# p-value가 0.06948, 유의수준 𝜶 0.05 보다 크므로 귀무가설 H0를채택
# "멘델이 주장한 콩의 잡종비율 이론적 분포는 적합하다"고 판단할 수 있다

#======================= p243
# 카이제곱검정 - 독립성 검정

raw_data <- c(7, 13, 9, 12, 13, 21, 10, 19, 11, 18, 12, 13)
data_mtx <- matrix(raw_data, byrow=TRUE, nrow=3)
data_mtx

dimnames(data_mtx) <- list("Class" = c("Class1", "Class2", "Class3"), 
                           "Score" = c("ScoreA", "ScoreB", "ScoreC", "ScoreF"))
data_mtx

#  addmargins 행과열의 합계
addmargins(data_mtx)                       

# 비례분포 : prop.table()
addmargins(prop.table(data_mtx)) 

# bar plot 
barplot(t(data_mtx), beside=TRUE, legend=TRUE, 
        ylim=c(0, 30), 
        ylab="Observed frequencies in sample", 
        main="Frequency of math score by class")

#======================= p245
# chisquared test : chisq.test()
(i.fit <- chisq.test(data_mtx))

# 가설
# 귀무가설 H0 : 두 변수 X와 Y는 서로 독립이다 (관련성이 없다)
# 대립가설 H1 : 두 변수 X와 Y는 서로 독립이 아니다 (관련성이 있다)
# 

# 분석결과
# p-value가 0.9667, 유의수준 𝜶 0.05 보다 크므로 귀무가설 H0를 채택
# "학급과 빅데이터 분석 성적 간에는 서로 관련성이 없다. 
# 즉 독립적이다."고 판단할 수 있다.

#======================= p249
# 카이제곱검정 - 동질성 검정
raw_data <- c(50, 30, 20, 50, 80, 70)
data_mtx <- matrix(raw_data, byrow=TRUE, nrow=2)
data_mtx

dimnames(data_mtx) <- list("성별" = c("남학생", "여학생"), 
                           "DS교과목" = c("통계", "머신러닝", "딥러닝"))
data_mtx

# marginal distribution : addmargins() 
addmargins(data_mtx)

# proportional distribution : prop.table() 
addmargins(prop.table(data_mtx))

#======================= p250
# bar plot : barplot()
barplot(t(data_mtx), beside=TRUE, legend=TRUE, 
        ylim=c(0, 120), 
        ylab="Observed frequencies in sample", 
        main="데이터 사이언스 교과목 선호 조사 결과")

#======================= p251
# chisquared test : chisq.test() 
(h.fit <- chisq.test(data_mtx))
1-pchisq(q=c(19.318), df=2, lower.tail=TRUE)

# 가설
# 귀무가설 H0 : 남학생/여학생별 선호하는 교과목이 동일하다
# 대립가설 H1 : 남학생/여학생별 선호하는 교과목이 동일하지 않다

# 분석결과
# p-value가 6.384e-05, 유의수준 𝜶=0.05 보다 작기 때문에
# 귀무가설 𝑯𝟎를 기각하고 대립가설 𝑯𝟏을 채택하여
# ‘남학생/여학생별 선호하는 교과목이 동일하지 않다’고 판단할 수 있다

# E=======================  참고 p28-44


##########################################
### chapter 5 
##########################################

#======================= p256

### chapter 5.1 단순선형회귀

lung <- read.csv("c:/r/LungDisease.csv")

# 면진에 대한 노출 연수와 폐할량 산전도 그래프  
head(lung)
plot(lung$Exposure, lung$PEFR, xlab="Exposure", ylab="PEFR")

#======================= p257
# 단순선형회귀모델 
model <- lm(PEFR ~ Exposure, data=lung)
model

# 이 회귀 모델의 절편(b0)은 424.583, 기울기(b1)는 -4.185 이다. 
# 즉, 다음과 같은 회귀모델로 피팅되었음
# PEFR = 424.583 + -4.185 x Exposure  

# 문제 : plot 위에 회귀선을 나타내시오





#======================= p258

# ======추가 회귀분석, 회귀선, 잔차연결선
# cars 데이터로 단순선형회귀
head(cars)
attach(cars)

# 1 plot
plot(dist~speed, data = cars)

# 2 lm() ~ abline()
m3 <- lm(dist~speed, cars)
abline(m3, col = "red")

#predict함수를 사용하여 예측
yhat <- predict(m3)

#잘 예측 되었는지 비교하기 위해 오른쪽에 실제 값을 붙여주겠습니다
cbind( dist, yhat )

join <- function(i)
  lines( c(speed[i], speed[i]), c(dist[i], yhat[i]), col="green")
sapply(1:50, join)

# ============================================

#======================= p259
# plot를 그리고, 회귀선과 잔차선을 나타내시오
model <- lm(PEFR ~ Exposure, data=lung)
plot(lung$Exposure, lung$PEFR, xlab="Exposure", ylab="PEFR", pch=20, col="red")
abline(model,col="blue")

attach(lung)
head(lung)
str(lung)

# predict함수를 사용하여 예측
yhat <- predict(model)
head(yhat)
# 오른쪽에 실제 값을 붙여주기
cbind( Exposure, yhat )

# 직선연결 : lines(x,y)
join <- function(i)
  lines( c(Exposure[i], Exposure[i]), c(PEFR[i], yhat[i]), col="green")
sapply(1:122, join)

#======================================================

#======================= p262
model <- lm(PEFR ~ Exposure, data=lung)
model

# 예측값과 잔차
fitted <- predict(model) # 예측값
fitted
head(fitted)
resid <- residuals(model) # 잔차
resid
head(resid)



# S======================= 참고 p16-41

install.packages(c("car", "modelr", "scatterplot3d"))

# 대학생 92명의 키와 몸무게 데이터 읽기 
std90 <- read.table("c:/r/student90.csv", sep = ",", 
                    stringsAsFactors = FALSE, 
                    header = TRUE, 
                    na.strings = "")
nrow(std90)
head(std90)

# 단순 선형 회귀 모델 생성 
(m <- lm(weight_kg ~ height_cm, data = std90))


# 통계량 확인 
summary(m)


# 대학생 90명 데이터의 1~4번째 적합(예측)된 값 확인하기 
# fitted(model) 혹은 회귀식 적용 ==> 결과동일

fitted(m)[1:4]

# 학생의 몸무게(kg) = 32.66 + 0.224 * 학생의 키(cm) 
((32.6604144) + (0.2246605) * (std90$height_cm[1:4]))

# 키와 몸림무게의 이상값 그리기
# https://thebook.io/006723/ch08/02/06/ 참고
# which 1~6  가능. which = 4 는 cooks.distance
# 쿡의 거리는 회귀 직선의 모양(기울기나 절편 등)에 크게 영향을 끼치는 점들을 찾는 방법

plot(m, which = 4)

# 이상값 진단 
x_cooks.d <- cooks.distance(m)
x_cooks.d[1:4]

NROW(x_cooks.d)

x_cooks.d[which(x_cooks.d>qf(0.5, df1 = 2, df2 = 88))]

# 이상값 진단
# R의 car::outlierTest(model) 함수 이용하여 
# 본페로니(Bonferroni) p가 0.05 보다 작은 경우 이상치인 것으로 판단한다

library("car")
outlierTest(m)

# 분석결과
# 본페로니 p(=0.73) > 0.05, 이상치가 검출되지 않음을 알 수 있다

# 대학생 90명 데이터의 1~4번째 잔차 구하기 : residuals(model) 
residuals(m)[1:4]

# 실제 데이터 값 = 적합된 값 + 잔차 
# 대학생 90명 데이터의 1 ~ 4번째 실제 몸무게 
std90$weight_kg[1:4]

# 실제 데이터 값 = 적합 된 값 + 잔차
fitted(m)[1:4] + residuals(m)[1:4]
#   1  2   3  4 
# 98 77 70 90 


# Q-Q plot : 잔차의 정규성 확인
qqnorm(residuals(m))  #정규분포의 QQ plot을 그리는 함수
qqline(residuals(m))  #정규분포의 QQ plot에서 1Q와 3Q를 지나는 선을 그리느 함수

# 동일한 결과 나옴
plot(m, which = 2)

# 샤피로 윌크 검정: 일변수 자료에 대해 수치적으로 정규성을 검정하는 기법
shapiro.test(residuals(m))

# 가설
# 귀무 가설 𝑯𝟎: 잔차가 정규분포를 따른다.
# 대립 가설 𝑯𝟏: 잔차가 정규분포를 따르지 않는다

# 샤피로 윌크 검정 결과
# p-value(=0.2189) > 0.05 이므로 귀무가설을 기각할 수 없다

# 회귀 계수의 95% 신뢰구간 구하기 : confint(model)
confint(m, level = 0.95)
#                  2.5 %      97.5 %
# (Intercept) 4.68512548  60.6357032
# height_cm   0.05911794   0.3902031


# 신뢰구간 구하기 (predict interval = "confidence" 신뢰구간)
m_conf <- predict(m, level = 0.95, interval = "confidence")
head(m_conf)

# 키와 몸무게 산포도, 추정된 평균 몸무게, 신뢰구간 
plot(weight_kg~height_cm, data=std90)
lwr <- m_conf[,2]   
upr <- m_conf[,3]   
sx <- sort(std90$height_cm, index.return=TRUE)
abline(coef(m), lwd=2)                      # 회귀계수
lines(sx$x, lwr[sx$ix], col="blue", lty=2)  # 아래쪽
lines(sx$x, upr[sx$ix], col="blue", lty=2)  # 윗쪽


# 키와 몸무게의 예측구간 (predict interval = "predict" 예측구간) 
m_pred <- predict(m, level = 0.95, interval = "predict")
head(m_pred)

# 키와 몸무게 산포도, 예측구간 
p_lwr <- m_pred[,2]   
p_upr <- m_pred[,3]   
lines(std90$height_cm, p_lwr, col="red", lty=2) # 아래쪽
lines(std90$height_cm, p_upr, col="red", lty=2) # 윗쪽


# 잔체 제곱 합 구하기 : deviance(model)
deviance(m)

# 새로운 학생의 키가 175cm 라면, 예상되는 몸무게 구하기
predict(m, newdata = data.frame(height_cm=175), interval = "confidence")

# 예측결과, 새로운 학생의 몸무게는 약 72 kg인 것으로 예측된다.

# 모델 평가 
summary(m)

# 분석결과
# 회귀식 : 몸무게(kg) = 32.66 + 0.225 * 학생의 키(cm)

#분산분석
anova(m)

#단순회귀분석
lm(m)

#분산분석 및 모델간의 비교

#몸무게(kg)~키(cm) 생성된 회귀 모델
(m_a <- lm(weight_kg ~ height_cm, data = std90))

#키(cm)를 제거하고,몸무게(kg)를 상수값으로 예측하는 경우
(m_b <- lm(weight_kg ~ 1, data = std90))

# 두 모델 비교 결과 
anova(m_a, m_b)

#실행결과 : 두 모델 간에는 유의한 차이가 있다고 결론을 내림
#(즉, 키(cm)열이 유의미한 설명 변수임을 뜻함)

#RMSE, MAE를 이용한 모델간의 비교
library(modelr)

# 오차를 제곱하여 n으로 나눈 값이 평균 제곱오차(MSE)를 다시 제곱근 시킨 지표
rmse(m_a, std90) # root-mean-squared-error(평균제곱근오차)
rmse(m_b, std90) # root-mean-squared-error(평균제곱근오차)

#  오차의 절대값을 씌우고 데이터 수로 나눈 것
mae(m_a, std90) # mean absolute error(평균절대오차)
mae(m_b, std90) # mean absolute error(평균절대오차)


# 실행결과
# m_a 모델의 RMSE 값과 MAE 값이 작게 나와 더 우수하다고 할 수 있다

# E======================= 참고 p16-41


#======================= p266

### chapter 4.2 다중선형회귀
# 킹카운티 주택 정보 예제 
house <- read.csv(("c:/r/house_sales.csv"), sep='\t')
head(house)
head(house[, c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms", 
               "Bedrooms", "BldgGrade")])

# 다중선형회귀모델 
house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade,  
               data=house, na.action=na.omit) # na.action=na.omit 결측값 삭제
house_lm

#======================= p270
summary(house_lm)

#======================= p274
# stepAIC() 함수를 이용한 단계적 회귀 예 
house_full <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                   Bedrooms + BldgGrade + PropertyType + NbrLivingUnits + 
                   SqFtFinBasement + YrBuilt + YrRenovated + NewConstruction,
                 data=house, na.action=na.omit)

# MASS 패키지에 있는 stepAIC() 함수를 이용해서 변수선택 방법을 지정
library(MASS)

# direction : backward는 후진, forward는 전진, both는 단계선택

step_lm <- stepAIC(house_full, direction="both")
step_lm



### chapter 5.4 
# "다수의 유형을 갖는 범주형 변수들" 
# 너무 많은 종류의 범주형 변수는 원-핫인코딩으로 데이터를 변형하면 
# 많은 양의 이진 더미를 생성할 수 있음. 
# 이 경우, 유형을 일부를 통합하는 것이 좋을 지, 
# 그대로 유지하는 것이 좋을 지 결정해야 함.

#======================= p283
### chapter 5.6 교란변수
# 중요한 변수인 ZipGroup 을 포함시키지 않았을 때 
lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
     Bedrooms + BldgGrade , data=house, na.action=na.omit)

#======================= p284

# house 데이터의 zipcode는 우편번호를 나타내는 범주형변수입니다.
# zipcode의 경우  원-핫인코딩으로 변형 시 너무 많은 이진 더미를 생성할 수 있읍니다. 
# 아래와 같이 우편번호를 5개의 그룹으로 통합 ZipGroup 변수에 저장합니다. 

library(dplyr)
zip_groups <- house %>%
  mutate(resid = residuals(house_lm)) %>%   # 새로운 열 만들기
  group_by(ZipCode) %>%
  summarize(med_resid = median(resid),
            cnt = n()) %>%
  # sort the zip codes by the median residual
  arrange(med_resid) %>%
  mutate(cum_cnt = cumsum(cnt),
         ZipGroup = factor(ntile(cum_cnt, 5)))

house <- house %>%
  left_join(dplyr::select(zip_groups, ZipCode, ZipGroup), by='ZipCode')

house$ZipGroup




#======================= p285

# 중요한 변수인 ZipGroup 을 포함시켰을 때  
lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
     Bedrooms + BldgGrade + PropertyType + ZipGroup, data=house, na.action=na.omit)


#======================= p287
### chapter 5.7 상호작용효과
fit = lm  (AdjSalePrice ~  SqFtTotLiving * ZipGroup + SqFtLot + 
             Bathrooms + Bedrooms +  BldgGrade + PropertyType,
           data=house, na.action=na.omit)

summary(fit)


#===== p289
data(mtcars)
head(mtcars)
fit_02=lm(formula=mpg~hp*wt, data=mtcars)
summary(fit_02)

# 회귀식 결과
# mpg=49.8-0.12*hp-8.22*wt+0.027*hp*wt

#상호작용을 그래프로 작성
plot(mpg~hp, data=mtcars, main="Interaction of hp:wt")

# curve(함수식, add=TRUE) 기존그래프에 겹쳐 그림
# wt에 2.2를 대입하면 
# 49.8-0.12*hp-8.22*wt(2.2)+0.027*hp*wt(2.2)=31.71-0.06*X

curve(31.71-0.06*x, add=TRUE)               # wt가 2.2
curve(23.49-0.03*x, add=TRUE, lty=2, col=2) # wt가 3.2
curve(15.28-0.003*x, add=TRUE, lty=3, col=3) # wt가 4.2

# lty(line type) 선의 모양(1~6)
legend("topright", c("2.2", "3.2", "4.2"), title="wt", lty=1:3, col=1:3)

# 차의 중량이 늘어날수록 마력과 연비의 관계가 약해짐을 알 수 있다

#======================= p290
# S======================= 참고 p42-47

# 다중 선형 회귀 : trees data
# trees 데이터에는 벗나무 31개 각각에 대해 나무의 지름(Girth), 
# 나무의 키(Height), 목재의 부피(Volume) 3개의 숫자형 변수로 구성
# 지름(Girth), 나무의 키(Height)를 설명변수, 부피(Volume)를 반응 변수

str(trees)

summary(trees)

# trees 데이터의 분포  
library(scatterplot3d)
scatterplot3d(trees$Girth, trees$Height, trees$Volume)


# 다중 선형 회귀 모델 생설하기 
m <- lm(Volume ~ Girth + Height, data = trees)
m

# 회귀식 도출
# 𝐕𝐨𝐥𝐮𝐦𝐞 = −𝟓𝟕. 𝟗𝟖𝟕𝟕 + 𝟒. 𝟕𝟎𝟖𝟐 ∗ 𝑮𝒊𝒓𝒕𝒉 + 𝟎. 𝟑𝟑𝟗𝟑 ∗ 𝑯𝒆𝒊𝒈𝒉𝒕 + 𝛆𝐢


# trees 데이터와 회귀 모델을 중첩하여 시각화 
s <- scatterplot3d(trees$Girth, trees$Height, trees$Volume, 
                   pch = 20, type = 'h', angle = 55)
s$plane3d(m)

# 벗나무 세 그루의 지름과 키를 측정하여 부피를 예상하기  
(n.data <- data.frame(Girth=c(8.5, 13.0, 19.0), Height=c(72, 86, 85)))
(n.y <- predict(m, newdata = n.data))

# 회귀식에 적용한 결과 ==> predict와 동일
-57.9877 + 4.7082*8.5 + 0.3393*72
-57.9877 + 4.7082*13.0 + 0.3393*86
-57.9877 + 4.7082*19.0 + 0.3393*85

# 벗나무 세 그루의 지름과 키를 측정하여 부피를 예상한 결과 시각화   
s <- scatterplot3d(c(8.5, 13.0, 19.0), c(72, 86, 85), n.y, pch = 20, type = 'h', 
                   color = 'red', angle = 55)
s$plane3d(m)

# E======================= 참고 p41-47

##########################################################


