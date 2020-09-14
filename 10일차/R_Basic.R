## 실습 1 ###############

# 데이터형 
x<-5      #숫자형 
y<-1.0    #숫자형 
z<-"hello World"   #문자형 
b1<- TRUE     #논리형 
b2<- FALSE    #논리형 

#데이터형 확인 
class(x)
class(y)
class(z)
class(b1)
class(b2) 

## 실습 2 ###############

a1 <- 10/0  
a1  #양의 무한대
a2 <- -10/0
a2  #음의 무한대
a3 <- a1/a2
a3  # Not a Number 
a4 <- NULL  
a5 <- NA  # 결측치 


## 실습 3 ###############

x <- c(1,2,3)
x
x[1]
x[3] <- 30 #데이터 수정
x[4] <- 4  #데이터 추가 
x
  
y<-0
y
y[2]<-20
y

# 순열 생성 
x <- c(1:10)   
x 
z <- seq(from=1,to=10, by=2)
z
z <- seq(1,10, by=2)
z
z <- seq(0,1, by=0.1)
z
z <- seq(0,1, length.out = 11)
z
z <- rep( c(1:3) ,time=2) 
z
z <- rep( c(1:3) ,each=2) 
z


## 실습 4 ###############
# 벡터 연산
x <- c(2, 4, 6, 8, 10)
length(x) 		# 길이(크기) 
x[1] 		      # x의 1번 요소
x[1, 2, 3] 		# 요소 구할때 -> 에러
x[c(1, 2, 3)] 	# 요소 구할때 -> c함수
x[-c(1, 2, 3)] 	# 요소 제외
x[c(1:3)] 		# 1~3 요소 출력

# 벡터끼리 연산수행
x <- c(1, 2, 3, 4)
y <- c(5, 6, 7, 8)
z <- c(3, 4)
w <- c(5, 6, 7)
x+2 		# x 개별요소에 2를 각각 더함
x + y 		# x와 y 크기가 동일 -> 각요소 더함
x + z 		# x와 y 크기가 정수배 -> 작은쪽 순환 더함
x + w 		# x와 y 크기가 정수배 아님 -> 에러

# 벡터 연산에 유용한 함수
x=1:10
x
x >5 		# x 요소값이 5보다 큰가? 


## 문제 1 ###
#1.1  c2 변수에 ~10까지 2 간격 연속값을 저장  

#1.2  str1 변수에  "hi" "R World" 문자열 두개 저장  



## 실습 5 ###############

# matrix 함수로 2차원 배열 생성
x <- 1:12  # 벡터 생성  
x
m1<- matrix(x, nrow = 3)  # x데이터를 활용하여 행렬 생성
m1
m2<- matrix(x, nrow = 3, byrow = T) #행우선
m2

#벡터를 묶어 배열 생성
v1 <- c(1, 2, 3, 4)
v2 <- c(5, 6, 7, 8)
v3 <- c(9, 10, 11, 12)
cbind(v1, v2, v3) # 열 단위로 묶어 배열 생성
rbind(v1, v2, v3) # 행 단위로 묶어 배열 생성 


## 실습 6 ###############
x <- 1:5  

# array 함수로 2차원 배열 생성 
a1 <- array(x, c(2, 4))  # x데이터를 활용하여 2행 4열 배열 생성 
a1

# array 함수로 3차원 배열 생성 
a1 <- array(x, c(2, 4, 2))  # x데이터를 활용하여 2행 4열 배열 생성 
a1

#행과 열 이름 설정
dimnamex = list(c("1st", "2nd"), c("1st", "2nd", "3rd", "4th"))  
a1 <- array(x, c(2, 4), dimnames = dimnamex)
a1


## 실습 7 ###############

#데이터프레임
name <- c("하나", "두나", "세나")
age <- c(22, 20, 25)
gender <- factor(c("M", "F", "M"))
blood <- factor(c("A", "O", "B"))
patients <- data.frame(name, age, gender, blood)
patients
 

# 데이터 프레임 요소에 접근 : $, [,]
patients
patients$name # name 속성값 출력
patients[1, ] # 1행 값
patients[, 2] # 2열 값
patients[3, 1] # 3행 1열 값
patients[patients$name=="하나", ] #하나에 대한 정보 추출
patients[patients$name=="하나", c("name", "age")] #하나의 이름과 나이 추출


## 실습 8 ###############

head(cars)
head(cars,3)
tail(cars)
tail(cars,3)
class(cars)
dim(cars)

## 실습 9 ###############
str(cars)
mean(cars$speed)
mean(cars$dist)
median(cars$speed)
median(cars$dist)
summary(cars) 


 
## 실습 10 ###############
# 01 파일 읽고 쓰기 #
# read.table()함수는 header=F 가 디폴트
students <- read.table("C:/r/students.txt", header = T, fileEncoding  = "utf-8") 
students 

class(students) 


## 실습 11 ###############

# 파일을 읽을 때 문장을 문자(character 데이터형)으로 인식하도록 설정하려면 
# stringsAsFactors = F  
students <- read.table("C:/r/students.txt", header = T, stringsAsFactors = F, fileEncoding  = "utf-8")  
str(students)

# 파일을 읽을 때 문장을 요인(factor 데이터형)으로 인식하도록 설정하려면 
# stringsAsFactors = T 
students <- read.table("C:/r/students.txt", header = T, stringsAsFactors = T, fileEncoding  = "utf-8")
str(students)
  

## 실습 12 ###############

#read.csv 함수: CSV(Comma-Separated Values) 파일을 읽을 때 사용
# 첫 행이 header이므로 header 옵션을 지정할 필요가 없음. 
# read.csv()함수는 header=T가 디폴트
students <- read.csv("C:/r/students.csv", fileEncoding = "utf-8")  
students

# 읽은 파일의 구조 확인
str(students) 
  

## 실습 13 ###############

# 파일을 읽을 때 문장을 요인(factor 데이터형)으로 인식하도록 설정하려면 
# stringsAsFactors = T 
# 파일을 읽을 때 문장을 문자(character 데이터형)으로 인식하도록 설정하려면 
# stringsAsFactors = F   
students <- read.csv("C:/r/students.csv", stringsAsFactors = FALSE, fileEncoding = "utf-8") 
str(students)

#?write.table
# 문장에 큰따옴표가 표시됨.
write.table(students, file = "C:/r/output.txt") 

# 문장에 큰따옴표되지 않음.
write.table(students, file = "C:/r/output.txt", quote = F) 


## 문제 2 ###
#--------------------------------------------------------
#  productId     productName     price 
#  10             smTV           1000000
#  20             lgTv           900000 
#  30             sonyTv         500000
#  40             pTv            700000

#2.1 df1 변수에 위와 같은 데이터를 갖는 데이터 프레임을 생성하세요. 

#2.2 위 데이터 프레임에서 가격 평균을 출력하세요.  

#2.3 위 데이터 프레임에서 가격 최대값을 출력하세요. 

#2.4 위 데이터 프레임을 product.csv 파일로 저장하세요. 
 

## 실습 14 ############### 

# 데이터 정제 예제 1 : 결측값 처리 # 
# is.na 함수를 이용해 결측값 처리하기

# airquality 데이터 셋의 R에서 기본으로 제공되는 데이터 셋.
# New York의 대기/공기에 대한 질을 측정한 데이터 셋
# 전체 153개 관측치와 6개의 변수로 구성됨.
head(airquality)

str(airquality)	# airquality 데이터의 구조를 살펴봄.


## 실습 15 ###############

# airquality 데이터에서 NA는 TRUE, 아니면 FALSE. 
is.na(airquality) 	
table(is.na(airquality))	# NA가 총 44개 있음.
table(is.na(airquality$Temp))	# Temp에는 NA가 없음을 확인함.
table(is.na(airquality$Ozone))	# Ozone에는 NA가 37개 발견됨.


## 실습 16 ###############

mean(airquality$Temp)		# NA가 없는 Temp는 평균이 구해짐.
mean(airquality$Ozone)		# NA가 있는 Ozone은 평균이 NA로 나옴.

# 함수 속성인 na.rm을 이용해 결측값 처리하기
mean(airquality$Ozone, na.rm = T)


# Ozone 속성에서 NA가 없는 값만 추출함.
air_narm <- airquality[!is.na(airquality$Ozone), ]  
mean(air_narm$Ozone)	# 결측값 제거후 mean 함수 동작

# na.omit 함수를 이용해 결측값 처리하기
air_narm1 <- na.omit(airquality) 
mean(air_narm1$Ozone)


## 실습 17 ############### 

# 이상값이 포함된 환자 데이터
patients <- data.frame(name = c("하나", "두나", "세나", "네나", "다나"), 
                       age = c(22, 20, 25, 30, 27), 
                       gender=factor(c("M", "F", "M", "K", "F")), 
                       blood = factor(c("A", "O", "B", "AB", "C")))
patients

# 성별에서 이상값 제거
patients_outrm <- patients[patients$gender=="M"|patients$gender=="F", ]
patients_outrm	


# 성별과 혈액형에서 이상값 제거
patients_outrm1 <- patients[(patients$gender == "M"|patients$gender == "F") 
                            & (patients$blood == "A"
                               |patients$blood == "B"
                               |patients$blood == "O"
                               |patients$blood == "AB"), ]
patients_outrm1	 


## 실습 18 ############### 

#성별은 남자는 1, 여자는 2로 표시, 혈액형은 A, B, O, AB형을 각각 1, 2, 3, 4로 표현
# 이상값이 포함된 환자 데이터
patients <- data.frame(name = c("하나", "두나", "세나", "네나", "다나"), 
                       age = c(22, 20, 25, 30, 27), 
                       gender = c(1, 2, 1, 3, 2), 
                       blood = c(1, 3, 2, 4, 5))
patients	

# 성별에 있는 이상값을 결측값으로 변경
patients$gender <- ifelse((patients$gender<1|patients$gender>2), 
                          NA, patients$gender)
patients	

# 혈액형에 있는 이상값도 결측값으로 변경
patients$blood <- ifelse((patients$blood<1|patients$blood>4), 
                         NA, patients$blood)
patients

# 결측값을 모두 제거
patients[!is.na(patients$gender)&!is.na(patients$blood), ]


## 실습 19 ############### 

#boxplot을 활용하여 정상값과 이상값을 구분
boxplot(airquality$Ozone)    # Ozone 대한 boxplot
boxplot(airquality$Ozone)$stats   # Ozone의 boxplot 통계값 계산
 

## 실습 20 ############### 

#boxplot을 활용하여 정상값과 이상값을 구분
boxplot(airquality[, c(1:4)])    # Ozone, Solar.R, Wind, Temp에 대한 boxplot
 
air <- airquality                # 임시 저장 변수로 airquality 데이터 복사
table(is.na(air$Ozone))          # Ozone의 현재 NA 개수 확인

# 이상값을 NA로 변경
air$Ozone <- ifelse(air$Ozone<1|air$Ozone>122, NA, air$Ozone) 
table(is.na(air$Ozone)) # 이상값 처리 후 NA 개수 확인(2개 증가)

# NA 제거
air_narm <- air[!is.na(air$Ozone), ] 
mean(air_narm$Ozone) # 이상값 두 개 제거로 is.na 결과보다 값이 줄어듦


## 실습 21 ###############  

x<- 5
if (x %% 2 == 0 ) {
  print('x는 짝수') 
}else {
  print('x는 홀수')
}

for (i in 1:5) {
  print(i)
}

## 실습 22 ###############  
for (i in 1:5) {
  if (i %% 2 == 0 ) {
    print('i는 짝수') 
  }else {
    print('i는 홀수')
  }
}
