##단순선형회귀
lung <- read.csv("c:/r/LungDisease.csv")
lung
plot(lung$Exposure, lung$PEFR, xlab="Exposure", ylab="PERF") #면진에 대한 노출 연수와 폐활량 산전도 그래프
model <- lm(PEFR~Exposure, data=lung) #선형 회귀 모델 생성
model
#실행결과: 이 회귀 모델의 절편(b0)는 424.583, 기울기(b1)는 -4.185dlek.
#          즉, 다음과 같은 회귀모델로 피팅되었음
#          PEFR = 424.583 + -4.185*Exposure

#회귀분석, 회귀선, 잔차연결선
head(cars) # cars는 단순한 내장 데이터세트 #dataset 알아보려면 'data()'
attach(cars)

#1 plot
plot(dist~speed, data=cars)
#2 lm() ~ abline()
m3 <- lm(dist~speed, cars)
m3
abline(m3, col ="red") # 직선을 그래프에 추가 >> 그래프가 얼마나 일치하는가 보여주는 선
yhat <- predict(m3) # predict함수를 사용하여 예측
yhat
cbind(dist, yhat) # 잘 예측 되었는지 비교하기 위해 오른쪽에 실제값을 붙여줌

join <- function(i)
  lines(c(speed[i], speed[i]), c(dist[i], yhat[i]), col="green")
#lines( x좌표 , y좌표 )인데, x좌표는 변동이 없어서 같은 점을 두개써주고, y좌표가 실제 값이기 때문에 두개의 점을 잡아서 선을 그리는 것임.
z <- sapply(1:50, join) #return 값이 없어서 null 값 출력

#잔차(residual) : 실제 값과 예측 값의 차이를 오차(error) 또는 잔차(residual)라고 함
#plot을 그리고, 회귀선과 잔차선을 나타내시오
model <- lm(PEFR ~ Exposure, data = lung)
plot(lung$Exposure, lung$PEFR, xlab = "Exposure", ylab="PEFR", pch=20, col="red")
abline(model, col="blue")
attach(lung)
head(lung)
str(lung)
yhat <- predict(model) #predict 함수를 사용하여 예측
yhat
head(yhat)
cbind(Exposure, yhat) # 오른쪽에 실제 값을 붙여주기
join <- function(i)
  lines( c(Exposure[i], Exposure[i]), c(PEFR[i], yhat[i]), col="green") # 직선연결:lines(x,y)
sapply(1:122, join)

#회귀함수 학습 - predict()로 예측 값(적합값), residual()으로 잔차를 구할 수 있음
model <- lm(PEFR~Exposure, data=lung) # formula = 종속변수~독립변수
model
fitted <- predict(model) # 예측 값
head(fitted)
resid <- residuals(model) # 잔차 
head(resid)


##최소제곱회귀 : 제곱의 합이 최소가 되도록 값을 정하는 방법
