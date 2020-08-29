#단순회귀모형실습
#대학생 90명의 키와 몸무게 데이터를 이용해, 회귀 모델을 생성하고,
#회귀 계수, 회귀 계수의 신뢰구간, 잔차, 잔차 제곱의 합,
#새로운 학생 키로 몸무게 예측, 모델 평가 해보기


#1.데이터셋 읽어오기
student1 <- read.csv("c:/r/student90.csv", header = T, sep=",",
                     stringsAsFactors = FALSE,
                     na.strings = "")
student1
nrow(student1)
head(student1)
x <- c(student1$height_cm) #키
y <- c(student1$weight_kg) #몸무게


#2.회귀모델 생성
lm1 <- lm(y~x, data=student1)
lm1 # >> y(몸무게) = 32.6604 + 0.2247(키)


#3.회귀 계수 구하기
coef(lm1) # >> y = 32.6604 + 0.2247x



#4_1. 회귀 계수 값 검증하기
student2 <- data.frame(height_cm=student1$height_cm, weight_kg=student1$weight_kg)
head(student2)
#키가 198이고 몸무게가 98인 사람의 값을 회귀 계수에 넣어서 검증해보기
y1 <- 32.6604 + 0.2247 * 198 # 98, 77.151
y2 <- 32.6604 + 0.2247 * 170 # 77, 70.8594


#4_2.회귀 계수 값 검증하기
attach(student2)
student2
lm1 <- lm(y~x, data=student2)
summary(lm1) # pvalue:0.008385 < 0.05로 귀무가설 기각, 대립가설 채택
            # 이 회귀식은 유의하다 >> 학생키로 몸무게 예측이 가능하다.
            # m3 == lm1

#4_3.회귀 계수 값 검증하기(적합,예측값 구하기)
#1~4번째 적합(예측)값 확인하기:fitted(lm1)
fitted(lm1)[1:4]
#1~4번째 몸무게 계산 값 구하기
(32.6604 + (0.2246) * (student2$height_cm[1:4])) # >> 같은값 확인


#5.잔차 구하기
#이상값 진단 : 모형진단에서 주의해야 할 것은 이상값 탐색임.
#이상값: 선형관계 및 오차 범위를 벗어난 값
#그래프 표시
plot(lm1, which = 4)
x_cooks.d <- cooks.distance(lm1) # 이상값 진단
x_cooks.d[1:4]
NROW(x_cooks.d)
x_cooks.d[which(x_cooks.d>qf(0.5, df1=2, df2=88))]
install.packages("car") # car::outlierTest() 함수로 본페로니(Bonferroni) p가 0.05보다 작은 경우 이상치인 것으로 판단한다
library(car)
outlierTest(lm1) # 본페로니 p(=0.73) > 0.05 이므로 이상치가 검출되지 않음을 알수 있음

#잔차
resid <- residuals(lm1) #실제 데이터 값 = 적합된 값 + 잔차차
resid[1:4]
student2$weight_kg[1:4] #대학생 90명 데이터의 1~4번째 실제 몸무게
fitted(lm1)[1:4] + resid[1:4] #대학생 90명 데이터의 1~4번째 적합값 + 잔차 >> 같음

#잔차분석
qqnorm(resid(lm1))
qqline(resid(lm1)) #Q-Q plot도를 이용하여 '잔차의 정규성' 확인
shapiro.test(resid(lm1)) #샤피로 윌크 검정을 이용하여 '잔차의 정규성' 확인
#검정결과:pvalue:0.2189>0.05로 데이터가 정규분포를 따른다는 귀무가설 채택


#6.잔차 제곱합 구하기
deviance(m3)


#7.회귀 계수 신뢰 구간 구하기
confint(m3, level=0.95) #단순 선형 회귀에서 절편과 기울기는 정규분포를 따른다
                        #따라서 t분포를 사용한 95%의 신뢰구간을 confint()로 구함
lm1_conf <- predict(lm1, level = 0.95, interval = "confidence")
head(lm1_conf) # 기본적으로 유의수준은 95%, level을 이용하면 유의수준 99%까지 계산가능
#그래프 마무리(95%신뢰구간_파란점선과 함께 산포도 출력)
#추정된 평균 몸무게_실선
plot(weight_kg~height_cm, data = student2)
lwr <- lm1_conf[,2]
upr <- lm1_conf[,3]
sx <- sort(student2$height_cm, index.return=TRUE)
abline(coef(lm1), lwd=2)
lines(sx$x, lwr[sx$ix], col="blue", lty=2)
lines(sx$x, upr[sx$ix], col="blue", lty=2)


#8.새로운 학생 키로 몸무게 예측하기
#생성된 회귀 모델에서 예측구간 구하기
lm1_pred <- predict(lm1, level = 0.95, interval = "predict")
head(lm1_pred)
p_lwr <- lm1_pred[,2]
p_upr <- lm1_pred[,3]
lines(student2$height_cm, p_lwr, col="red", lty=2)
lines(student2$height_cm, p_upr, col="red", lty=2)

#예측하기
#새로운 학생의 키가 175cm일때의 예상되는 몸무게 구하기
predict(lm1, newdata = data.frame(x=175), interval = "confidence")
#예측결과:새로운 학생의 몸무게는 약 72kg인 것으로 예측 (하한, 상한값도 구함)


#9.모델 평가하기
summary(m3) #F통계량은 모델이 통계적으로 얼마나 의미가 있는지 알려줌
#귀무가설:계수(또는 절편)이 0이다.
#대립가설: 계수는 0이 아니다
#수정결정계수(Adjusted R-squred)는 모델이 데이터의 분산을 얼마나 설명하는지 알려줌

#회귀모델 평과 결과
#절편과 계수는 통계적으로 유의(절편과 계수의 pvalue < 0.05)
#추정값의 95% 신뢰구간에 0이 포함되어 있지 않다.
#그러나, 결정계수가 0.076으로 종속변수와 독립변수의 선형관계가 매우 낮다.

anova(lm1) # anova()로 F통계량 구하기
#대학생 90명의 키와 몸무게 데이터에서 몸무게(kg)~키(cm) 생성된 회귀모델,
#축소 모델인 몸무게(kg)~1의 두 모델 비교하기
#축소 모델은 원래 사용한 모델보다 설명 변수를 줄인 모델로 키(cm)를 제거하고, 
#몸무게(kg)를 상수값으로 예측하는 경우
lm1_a <- lm(weight_kg ~ height_cm, data=student1)
lm1_a

lm1_b <- lm(weight_kg ~ 1, data=student1)
lm1_b
#두 모델 비교 결과
anova(lm1_a, lm1_b)
#F통계량은 7.273으로 낮게 나타나고, pvalue=0.008
#결론:두 모델 간에는 유의한 차이가 있다(즉, 키(cm)열이 유의미한 설명 변수임을 뜻함)

#RMSE, MAE를 아용한 모델간의 비교
install.packages("Metrics")
library(Metrics)
rmse(lm1_a, student1)
mae(lm1_b, student1)
