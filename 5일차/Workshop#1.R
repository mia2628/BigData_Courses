#t검정
#세 종류 데이터
data1 <- c(30, -5, 55, -30, -20, 45)
data2 <- c(12,13,12,13,12,13)
data3 <- c(30,-5,55,-30,-20,45,30,-5,55,-30,-20,45)

#t-검정
t.test(data1, alternative = c("greater"), mu=0)
t.test(data2, alternative = c("greater"))
t.test(data3, alternative = c("greater"))
#mydata <- c(data1, data2,data3)
#t.test(mydata)

#t검정 예제(1/3)
session_times <- read.csv("c:/r/web_page_data.csv")
head(session_times)
t.test(Time~Page, data=session_times, alternative = 'less')
#표본을 분석했을때 A가 작긴 하지만 p-value가 0.05보다 크기 때문에 유의하지 않다.


#분산분석 - aov()를 이용한 분산분석 예
four_sessions <- read.csv("c:/r/four_sessions.csv")
four_sessions
#aov(Time~Page, data=four_sessions)
summary(aov(Time~Page, data=four_sessions)) #df는 자유도, Sum sq는 제곱합, Mean Sq는 평균제곱, F value는 F통계량
oneway.test(Time~Page, data=four_sessions, var =T)

