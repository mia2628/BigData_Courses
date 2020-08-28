##다중선형회귀
#워싱턴 시애틀에 위치한 킹카운티 주택 정보 예제
#데이터 준비
house <- read.csv("c:/r/house_sales.csv", sep = '\t')
head(house[ , c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms", "Bedrooms", 
                "BldgGrade")])
#다중선형회귀 모델 생성
house_lm <- lm(AdjSalePrice~SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade, data = house, na.action = na.omit)
house_lm
#모델 평가
summary(house_lm) #추정한 계수들과 함께, 계수의 표준오차(SE)와 t통계량을 함께 보여줌
                  #tvalue는 높을수록 pvalue는 낮을수록 예측 변수는 유의미
                  #결정계수는 1에 가까울수록 설명력이 높음
                  #F 통계량은 높을 수록 pvalue는 낮을수록 모델 유의미


#R의 stepAIC()함수를 이용한 단계적 회귀
house_full <- lm(AdjSalePrice~SqFtTotLiving + SqFtLot +Bathrooms + Bedrooms + BldgGrade + PropertyType + NbrLivingUnits + SqFtFinBasement + YrBuilt + YrRenovated + NewConstruction, data=house, na.action=na.omit)
unique(house)
install.packages("MASS")
library(MASS)
step_lm <- stepAIC(house_full, direction="both") #각각의 예측변수를 제거(변수앞~표시) 했을 때 AIC 값이 출력
#다음 모델은 AIC 값이 최소가 되는 NbrLivingUnits를 삭제함.
house_full <- lm(AdjSalePrice~SqFtTotLiving + SqFtLot +Bathrooms + Bedrooms + BldgGrade + PropertyType + SqFtFinBasement + YrBuilt + YrRenovated + NewConstruction, data=house, na.action=na.omit)
step_lm <- stepAIC(house_full, direction="both") #각각의 예측변수를 제거(변수앞-표시)했을 때 AIC 값이 출력
#각각의 예측변수를 추가(변수앞+표시)했을때 AIC 값이 출력
#다음 모델은 AIC 값이 최소가 되는 NewConstruction 를 삭제함.
house_full <- lm(AdjSalePrice~SqFtTotLiving + SqFtLot +Bathrooms + Bedrooms + BldgGrade + PropertyType + SqFtFinBasement + YrBuilt + YrRenovated, data=house, na.action=na.omit)
step_lm <- stepAIC(house_full, direction="both")
#이하동문
house_full <- lm(AdjSalePrice~SqFtTotLiving + SqFtLot +Bathrooms + Bedrooms + BldgGrade + PropertyType + SqFtFinBasement + YrBuilt, data=house, na.action=na.omit)
step_lm <- stepAIC(house_full, direction="both")
#이하동문
house_full <- lm(AdjSalePrice~SqFtTotLiving +Bathrooms + Bedrooms + BldgGrade + PropertyType + SqFtFinBasement + YrBuilt, data=house, na.action=na.omit)
step_lm <- stepAIC(house_full, direction="both")
#이하동문
house_full <- lm(AdjSalePrice~SqFtTotLiving +Bathrooms + Bedrooms + BldgGrade + PropertyType + SqFtFinBasement + YrBuilt, data=house, na.action=na.omit) # >>최종모델


##교란변수:회귀방정식에 중요한 변수가 포함되지 못해서 생기는 누락의 문제
lm(AdjSalePrice~SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade, data=house, na.action=na.omit) #ZipGroup 변수를 제외하고 회귀 모델을 생성했을 때
#house 데이터의 zipcode는 우편번호를 나타내는 범주형 변수이다.
#zipcode의 경우 원-핫인코딩으로 변형 시 너무 많은 이진 더미를 생성할 수 있다.
#아래와 같이 우편번호를 5개의 그룹으로 통합 ZipGroup 변수에 저장한다.
install.packages("dplyr")
library(dplyr)
zip_groups <- house %>%
  mutate(resid = residuals(house_lm)) %>% #새로운 열 만들기
  group_by(ZipCode) %>%
  summarize(med_resid = median(resid), cnt =n()) %>% # sort the zip codes by the median residual
  arrange(med_resid) %>%
  mutate(cum_cnt = cumsum(cnt), ZipGroup = factor(ntile(cum_cnt, 5)))
house <- house%>%
  left_join(dplyr::select(zip_groups, ZipCode, ZipGroup), by = 'ZipCode')
house$ZipGroup         

#킹 카운티 관련 데이터의 경우 주택의 가격 예측 예
#ZipGroup 변수를 제외하고 회귀 모델을 생성했을 때와,
#ZipGroup 변수를 포함한 후 회귀모델을 비교한 예를 보자
lm(AdjSalePrice~SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade + PropertyType + ZipGroup, data=house, na.action=na.omit)
#ZipGroup 변수를 포함하고 회귀 모델을 생성했을때 SqFtLot, Bathrooms와 같은 회귀 계수가 양수.
#Bedrooms는 여전히 음수인데, 살기 좋은 지역은 욕실 수가 같은 주택의 경우 작은 침실이 여러개 있으면 오히려 집값이 떨어진다고 함.


##상호작용과 주효과 - 상호작용 항을 이용한 회귀모델 예
lm(AdjSalePrice~SqFtTotLiving * ZipGroup + SqFtLot + Bathrooms + Bedrooms + BldgGrade + PropertyType, data=house, na.action=na.omit)
#가격대가 비싼 지역의 주택의 크기가 1제곱 피트씩 늘어날 때 가장 낮은 지역에 비해 예상 매매가가 거의 3배 차이
#결론:가격대가 가장 낮은 ZipGroup에서 집에 대한 기울기는 제곱피트당 118달러로, 주효과 SqFtTotLiving에 해당하는 계수와 같음.
#     가장 비싼 ZipGroup에 대한 계수는 이 주효과 계수에 SqFtTotLiving:ZipGroup5의 경우를 더한 합, 즉 118+230 = 348 달러와 같다.

data(mtcars)
head(mtcars)
fit_02=lm(formula = mpg~hp*wt, data=mtcars)
summary(fit_02)

#회귀식 결과
#mpg=49.8-0.12*hp-8.22*wt+0.027*hp*wt
#상호작용을 그래프로 작성
plot(mpg~hp, data=mtcars, main="Interaction of hp:wt")

#curve(함수식, add=TRUE) 기존그래프에 겹쳐 그림
#wt에 2.2를 대입하면 49.8-0.12*hp-8.22*wt(2.2)+0.027*hp*wt(2,2)=31.71-0.06*X
curve(31.71-0.06*x, add=TRUE) #wt가 2.2
curve(23.49-0.03*x, add=TRUE, lty=2, col=3) #wt가 3.2
curve(15.28-0.003*x, add=TRUE, lty=3, col=3) #wt가 4.2
#lty(line type) 선의 모양(1~6)
legend("topright", c("2.2", "3.2", "4.2"), title="wt", lty=1:3, col=1:3)
#차의 중량이 늘어날수록 마력과 연비의 관계가 약해짐을 알 수 있다.


#R의 trees 데이터를 이용해 다중 선형 회귀 수행하기
#trees 데이터에는 벗나무 31개 각각에 대해 나무의 지름(Girth), 나무의 키(Height), 목재의 부피(Volume) 3개의 숫자형 변수로 구성되어 있음
str(trees)
summary(trees)
#scatterplot3d()함수로 trees 데이터의 분포를 확인
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(trees$Girth, trees$Height, trees$Volume)
#다중 선형 회귀 모델 생성하기
m <- lm(Volume ~ Girth + Height, data=trees)
m
#trees 데이터와 회귀 모델을 중첩하여 시각화하기
s <- scatterplot3d(trees$Girth, trees$Height, trees$Volume, pch=20, type='h', angle=55)
s$plane3d(m)

#벗나무 세 그루의 지름과 키를 측정하여 나무의 부피를 예상하기
(n.data <- data.frame(Girth=c(8.5,13.0,19.0), Height=c(72,86,85)))
(n.y <- predict(m, newdata = n.data))
#벗나무 세그루의 지름과 키를 측정하여 나무의 부피를 예상한 결과를 시각화
s <- scatterplot3d(c(8.5,13.0,19.0), c(72,86,85), n.y, pch=20, type='h',
                   color='red', angle=55)
s$plane3d(m)
