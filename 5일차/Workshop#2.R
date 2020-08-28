##카이제곱검정
click_rate <- read.csv("c:/r/click_rates.csv")
head(click_rate)
clicks <- matrix(click_rate$Rate, nrow=3, ncol=2, byrow=TRUE)
head(clicks) #뉴스 헤드라인 별 클릭 수 데이터
chisq.test(clicks, simulate.p.value = TRUE) # p-value:0.4758 >> 귀무가설 채택

#카이제곱검정 종류
#1. 적합도 검정 - 한개의 요인을 대상으로 함
#2. 독립성 검정, 3. 동질성 검정 - 두개의 요인을 대상으로 함

#1.적합도 검정
#유전학자 멘델 - 멘델유전자의 이론이 맞다고 할 수 있는가?
obs <- c(20,40,40)
obs.probs <- c(2/10, 3/10, 5/10) # 잡종비율 A:B:C = 2:3:5라고 주장한것을 값 저장
(g.fit <- chisq.test(obs, p=obs.probs)) # p-value:0.06948 >> 귀무가설(HO) 채택                              # 멘델이 주장한 콩의 잡종비율 이론적 분포는 적합하다.

#2.독립성 검정
raw_data <- c(7,13,9,12,13,21,10,19,11,18,12,13)
data_mtx <- matrix(raw_data, byrow=TRUE, nrow=3)
data_mtx
dimnames(data_mtx) <- list("Class" = c("Class1", "Class2", "Class3"), 
                           "Score" = c("ScoreA", "ScoreB", "ScoreC", "ScoreF"))
data_mtx
addmargins(data_mtx) #marginal distribution : addmargins() - 합계를 맨 마지막 열에 추가해줌
addmargins(prop.table(data_mtx))#propotional distribution : prop.table() 빈도 비례분포 테이블
#barplot((data_mtx), beside = TRUE, legend = TRUE,
#        ylim=c(0,30),
#        ylab="Observed frequencies in sample",
#        main="Frequency of math score by class")

barplot(t(data_mtx), beside = TRUE, legend = TRUE,
        ylim=c(0,30),
        ylab="Observed frequencies in sample",
        main="Frequency of math score by class") #위 그래프를 t를 써서 x축,y축 교대
(i.fit <- chisq.test(data_mtx)) # chisquared test:chisq.test()
#결론:pvalue가 0.9667 이므로 귀무가설(HO) 채택
#     >> 학급과 빅데이터 분석 성적간에는 서로 관령성이 없다

#3.동질성 검정
raw_data <- c(50,30,20,50,80,70)
data_mtx <- matrix(raw_data, byrow = TRUE, nrow = 2)
data_mtx
dimnames(data_mtx) <- list("성별" = c("남학생","여학생"),
                           "DS교과목" = c("통계", "머신러닝", "딥러닝"))
data_mtx
addmargins(data_mtx) # marginal distribution : addmargins() - sum추가
addmargins(prop.table(data_mtx)) # propotional distribution : prop.table() 빈도 비례분포 테이블 전환
#barplot(data_mtx, beside = TRUE, legend = TRUE,
#        ylim = c(0,120),
#        ylab = "Observed frequencies in sample",
#        main="데이터 사이언스 교과목 선호 조사 결과")
barplot(t(data_mtx), beside = TRUE, legend = TRUE,
        ylim = c(0,120),
        ylab = "Observed frequencies in sample",
        main="데이터 사이언스 교과목 선호 조사 결과")
(h.fit <- chisq.test(data_mtx)) # chisquared test:chisq.test()
#결론:pvalue:6.384e-05로 귀무가설 기각하고 대립가설(H1) 채택
#     남학생/여학생별 선호하는 데이터 사이언스 교과목이 동일하지 않다.
