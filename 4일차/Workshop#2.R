##빈도분석
fruits1 <- read.csv("c:/r/love_fruits.csv", header = T)
head(fruits1)
prop.table(table(fruits1$선호과일)) # prop.table:빈도계산, table:중복부분 집계해줌
round(prop.table(table(fruits1$선호과일))*100, 2) # round:반올림 소수점

#건수와 비율을 하나의 테이블로 완성
table(fruits1$선호과일)
count <- c(table(fruits1$선호과일))
pct <- c(round(prop.table(table(fruits1$선호과일))*100, 2))
love_fruits <- data.frame(건수=count, 비율=pct)
love_fruits

barplot(love_fruits$건수, names.arg = c("바나나", "복숭아", "사과", "체리", "포도"), ylim=c(0,15), col = (rainbow(5))) #names.arg:막대이름, name:arg 생략가능, ylim:y축범위, col:막대색상

##상관분석 A사건이 B사건에 어떤 영향을 주는가?
x <- c(3,5,8,11,13)
y <- c(1,2,3,4,5)
cor(x,y)
cor.test(x,y)

##회귀분석
x <- c(110, 120, 130, 140, 150)
y <- c(100, 105, 128, 115, 142)
plot(x,y,pch=20, col="red")
line <- lm(y~x)
line # 기본값 : -4.20, b1:0.94 이므로 회귀식은 y = -4.20 + 0.94x
abline(line, col="blue")

#성적과 IQ간의 회귀식
score <- read.csv("c:/r/score.txt", header = T, sep = ",")
attach(score) # 필드 고정
lm1 <- lm(성적~IQ)
lm1 # 따라서 회귀식은 y = -5.2918 + 0.6714x
y <- -5.292 + 0.671 * 125 # IQ가 125일 경우 예상 점수 계산
y # 78.583

#병원 친절도가 병원만족도에 미치는 영향
x <- c(15,10,13,7,9,8,21,9,14,8)
y <- c(15,14,12,8,14,7,16,10,15,12)
out=lm(y~x)
out
summary(out) #결과 보여줌
#p-value는 0.1585<0.05 이기 때문에 귀무가설 기각
#회귀식은 y = 6.3592 + 0.5211x
#47% 정도의 신뢰도를 가진다.

lm3 <- lm(성적~IQ+다니는학원수+게임하는시간+TV시청시간)
lm3 # 회귀식은 y = 23.29 + 0.46x1 + 0.7179x2 - 0.8390x3 - 1.3854x4
#IQ가 130인 사람이 학원을 3개 다니고 게임을 2시간 하고 tv를 1시간 볼 경우 예상되는 성적은?
y = 23.299 + (0.46 * 130) +(0.7179 * 3) - (0.8390 * 2) - (1.3854 * 1)
y # 82점

summary(lm3) # 92%의 신뢰도를 가진다


#개인요인이 인맥관리에 영향을 미치는가?
x1 <- c(100,90,98,79,81,69,80,77,68,54)
x2 <- c(5,4,5,3,4,3,2,3,2,1)
x3 <- c(5,3,4,3,4,3,2,3,2,1)
x4 <- c(5,3,3,2,3,3,4,3,2,1)
mydata <- data.frame(y=x1, s1=x2, s2=x3, s3=x4)
mydata
model <- lm(y~.,data=mydata) #.:모든 독립변수를 넣어줌
model
summary(model)
#회귀식: y = 44.844 + 13.837x1 - 8.22x2 + 5.22x3
#약 88%의 신뢰도를 가진다
#p-value는 0.001<0.05 이기 때문에 회귀식은 유의 -> 유의한 영향을 미친다 (귀무가설 기각, 대립가설 채택)

#서비스요인이 고객충성도에 유의항 영향을 미치는가?
x1 <- c(100,90,98,79,81,69,80,77,68,73)
x2 <- c(5,4,4,3,4,3,4,3,2,3)
x3 <- c(4,3,3,2,3,2,3,3,2,3)
x4 <- c(5,3,3,2,3,3,4,3,2,3)
mydata <- data.frame(y=x1, s1=x2, s2=x3, s3=x4)
mydata
model <- lm(y~., data=mydata)
summary(model)
# 회귀식: 38.585 + 13.268x1 + 6.707x2 - 7.195x3
#72%의 신뢰도를 가진다
# 가격만이 유의한 영향을 미친다.