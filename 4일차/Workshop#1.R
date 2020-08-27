# 평균, 분산, 표준편차
score <- c(85,90,93,86,82)
score
mean(score) #평균값
median(score) #중앙값
var(score) #분산
sd(score) #표준편차

#데이터프레임:평균,편차, 표준편차
name <- c('주시현', '최경우', '이은주', '허민성', '홍미나')
height <- c(168,176, 167, 174, 169)
weight <- c(52, 68, 47, 82, 51)
df <- data.frame(이름=name, 키=weight, 몸무게=weight)
df
attach(df)
mean(height) #평균
median(height) #중앙값
mean(weight) #평균
sd(height) #표준편차
sd(weight) #표준편차

#정규분포
height <- rnorm(n=1000000, mean=168, sd=7)
#rnorm(개수,평균,표준편차):정규분포 난수 발생
hist(height, breaks=10, probability = T)
#hist:특정 데이터 빈도를 막대모양으로 보여줌
#breaks:막대수, probability:상대도수

height <- rnorm(n=1000000, mean=168, sd=7)
hist(height, breaks=1000, probability = T)

#t검정
score1 <- read.csv("C:/r/tdata.csv", header = T, stringsAsFactors = T)
score1
#정규분포를 따르는지 확인
shapiro.test(score1$성적) #p-value가 0.05보다 높은 0.8857이므로 정규분포를 따른다
result <- t.test(score1$성적,alternative=c("greater"), mu=75)
#mu는 모평균의 설정값, alternative는 모평균보다 크다,작다 또는 다르다.
#greater는 크다 less는 작다 ,two.sided는 다르다.
result #p-value는 0.05보다 크기 떄문에 정규분포를 따른다. 자유도는 9, 표본의 평균값 77.1 t값 0.7 - 귀무가설 채택

score1 <- read.csv("c:/r/tdata2.csv", header = T, stringsAsFactors = T)
score1
#정규분포를 따르는지 확인
shapiro.test(score1$성적) #p-value가 0.05보다 크기 때문에 정규분포를 따른다.

result <- t.test(score1$성적,alternative=c("greater"), mu=75)
result # 자유도는 9, p-value가 0.05보다 작기 때문에 귀무가설 기각, 대립가설 채택
       # 즉, 이번학기 전체 학생들의 평균은 75가 아니다 라는 추측을 할 수 있다.

#신약 t검정
x <- c(15,10,13,7,9,8,21,9,14,8)
y <- c(15,14,12,8,14,7,16,10,15,12)
shapiro.test(x) # p-value가 0.05보다 크기 때문에 정규분포를 따른다.
shapiro.test(y) # p-value가 0.05보다 크기 때문에 정규분포를 따른다.
t.test(x,y) # t.test의 p-value가 0.05보다 크기 때문에 귀무가설을 기각하지 못하여 신약의 효과가 있다고 주장x

#t검정 - 대응표본일 경우
x <- c(52,60,63,43,46,56,62,50)
y <- c(58,62,62,48,50,55,68,57)
# 정규분포를 따르는지 확인
shapiro.test(x) # p-value가 0.05보다 크기 때문에 정규분포를 따른다.
shapiro.test(y) # p-value가 0.05보다 크기 때문에 정규분포를 따른다.
t.test(x,y,paired = TRUE) # paired:대응입니다.
#결과해석: 유의확률이 0.0166<0.05 이기 때문에 귀무가설 기각, 대립가설 채택

#oneway.test - 3집단
x <- c(1.09, 2.12, 2.92, 4.06, 4.90)
y <- c(1,2,3,4,5)
z <- c(1.10,1.96,3.98,4.09,4.92)
# 정규분포를 따르는지 확인
shapiro.test(x)
shapiro.test(y)
shapiro.test(z)
mydata <-c(x,y,z) #벡터형으로 자료를 생성
mydata
group <- c(rep(1,5), rep(2,5), rep(3,5)) #처음 5개를 1로, 다음 5개를 2, 다음 5개를 3
group
oneway.test(mydata~group, var=T) #mydata를 3그룹지어서 평균이 같은지 검증
#결과해석: 유의확률이 0.9729>0.05 이기 때문에 귀무가설 채택 -> 3집단은 평균이 같다고 할 수 있다.

#분산분석 oneway.test-3집단
x1 <- c(23,27,24,25,29,30,26)
x2 <- c(35,32,38,36,32,33,34)
x3 <- c(36,41,38,39,40,38,39)
x4 <- c(32,30,37,34,35,34,32)
shapiro.test(x1)
shapiro.test(x2)
shapiro.test(x3)
shapiro.test(x4)
mydata <- c(x1,x2,x3,x4) #벡터형으로 자료를 생성
group <- c(rep(1,7), rep(2,7), rep(3, 7), rep(4, 7))
oneway.test(mydata~group, var = T)
#결과해석 : p-value가 2.581e-09<0.05 이기 때문에 귀무가설 기각 -> 제품이 4가지 온도에 따라 강도의 차이가 있다고 볼 수 있음