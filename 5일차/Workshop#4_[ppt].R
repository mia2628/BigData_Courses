#단순회귀모형실습
#대학생 90명의 키와 몸무게 데이터를 이용해, 회귀 모델을 생성하고,
#회귀 계수, 회귀 계수의 신뢰구간, 잔차, 잔차 제곱의 합,
#새로운 학생 키로 몸무게 예측, 모델 평가 해보기


#1.데이터셋 읽어오기
student1 <- read.csv("c:/r/student90.csv", header = T)
student1
x <- c(student1$height_cm) #키
y <- c(student1$weight_kg) #몸무게


#2.회귀모델 생성
lm1 <- lm(y~x)
#3.회귀 계수 구하기
lm1 # >> y = 32.6604 + 0.2247x


#4_1. 회귀 계수 값 검증하기
student2 <- data.frame(키=student1$height_cm, 몸무게=student1$weight_kg)
head(student2)
#키가 198이고 몸무게가 98인 사람의 값을 회귀 계수에 넣어서 검증해보기
y1 <- 32.6604 + 0.2247 * 198 # 98, 77.151
y2 <- 32.6604 + 0.2247 * 170 # 77, 70.8594


#4_2.회귀 계수 값 검증하기
attach(student2)
student2
m3 <- lm(y~x, data=student2)
summary(m3) # pvalue:0.008385 < 0.05로 귀무가설 기각, 대립가설 채택
            # 이 회귀식은 유의하다 >> 학생키로 몸무게 예측이 가능하다.

#그래프 표시
plot(y~x, data=student2)
abline(m3, col = "blue")
yhat <- predict(m3)
cbind(y, yhat)
join <- function(i)
  lines(c(x[i], x[i]), c(y[i], yhat[i]), col="red")
sapply(1:90, join)


#5.잔차 구하기
resid <- residuals(m3)
resid


#6.잔차 제곱합 구하기
deviance(m3)


#7.회귀 계수 신뢰 구간 구하기
confint(m3, level=0.95)


#8.

#9.모델 평가하기
summary(m3)
