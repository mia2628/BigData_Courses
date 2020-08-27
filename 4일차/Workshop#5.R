##통계학에서의 표본 분포 - R을 이용해 데이터의 분포와 표본의 분포
loans_income <- read.csv("c:/r/loans_income.csv")[,1]
#단순랜덤표본
samp_data <- data.frame(income=sample(loans_income, 1000), type='data_dist')
str(samp_data)
#추가 : tapply 연습
#tapply : 요인(factor)의 수준(level)별로 특정 벡터에 함수 명령어를 동시에 적용
#tapply(함수사용할 수치, 인수의 index, 적용함수)

samp <- data.frame(name=factor(c("kim", "choi", "choi", "lee", "kim", "lee")), score=c(50,30,10,50,20,30))
tapply(samp$score, samp$name, length)
tapply(samp$score, samp$name, mean)


#데이터 준비
loans_income <- read.csv("c:/r/loans_income.csv")[,1]
#단순랜덤표본
samp_data <- data.frame(income=sample(loans_income, 1000), type='data_dist')
#5개 표본씩 평균
samp_mean_05 <- data.frame(income = tapply(sample(loans_income, 1000*5), rep(1:1000, rep(5, 1000)), FUN = mean), type = 'mean_of_5')
#20개 표본씩 평균
samp_mean_20 <- data.frame(income = tapply(sample(loans_income, 1000*20), rep(1:1000, rep(20, 1000)), FUN = mean), type = 'mean_of_20')
#데이터프레임 rbind, 타입을 factor 변환
income <- rbind(samp_data, samp_mean_05, samp_mean_20)
income$type = factor(income$type, levels=c('data_dist', 'mean_of_5', 'mean_of_20'), labels = c('Data', 'Mean of 5', 'Mean of 20'))
#갯수 확인
table(income$type)
#plot the histograms
install.packages("ggplot2")
library(ggplot2)
ggplot(income, aes(x=income)) + geom_histogram(bins=40) + facet_grid(type ~ .) # 가로분할
ggplot(income, aes(x=income)) + geom_histogram(bins = 400) + facet_grid(type ~ .) # 가로분할

#옵션 수정 bins=400, facet_grid(. ~ type)
ggplot(income, aes(x=income)) + geom_histogram(bins=400) + facet_grid(. ~ type) # 세로분할

##부트스트랩 : 현재 있는 표본에서 추가적으로 표본을 복원 추출하고 각 표본에 대한 통계량을 다시 계산하는것
install.packages("boot")
library(boot)
loans_income <- read.csv("c:/r/loans_income.csv")[ ,1]
stat_fun <- function(x, idx) median(x[idx])
boot_obj <- boot(loans_income, R=1000, statistic = stat_fun) #부트스트랩 
#stat_fun함수는 인덱스 idx로 지정된 표본의 중앙값을 계산
boot_obj
#결과 : 중간값의 원래 추정치는 62,000 달러
#       부트스트랩 분포의 추정치는 약 -68달러 편향(bias) 가 있고,
#       206 달러의 표준오차가 있는 것으로 나타남

