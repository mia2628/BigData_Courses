#dataframe제작
name <- c("허윤성", "김준혁", "Scott", "Wilthon", "Ja-Hira", "Srethm")
age <- c(25, 24, 21, 22, 32, 28)
sex <- c("M", "M", "M", "F", "F", "F")
nation <- c("Korea", "Korea", "USA", "UK", "South-Africa", "Mongol")
major <- c("Engineering", "physics", "biology", "Mechanic", "Computing", "Archimetic")

df <- data.frame(name,age,sex,nation,major)
df



#1부터 100까지 합
hap <- 0
for(i in 1:100) {
  hap <- hap + i
}
print(hap)

#1부터 100까지 홀수 합
hap <- 0
for(i in 1:100) {
  if(i %% 2 == 1) {
    hap = hap + i
  }
}
print(hap)

#1부터 100까지 짝수 합
hap <- 0
for(i in 1:100) {
  if(i %% 2 == 0) {
    hap = hap + i
  }
}
print(hap)

#1부터 100까지 3의 배수의 합
hap <- 0
for(i in 1:100) {
  if(i %% 3 == 0) {
    hap <- hap + i
  }
}
print(hap)

#2부터 9까지의 구구단 제작
for(i in 2:9) {
  print(paste(i,"단"))
  for(j in 1:9) {
    print(paste(i, "*", j, "=" , i*j ))
  }
}

#문제2.1부터 100까지의 수 중에서 3의 배수이면서 4의 배수는 아닌 수의 합을 구하라.
hap <- 0
for(i in 1:100) {
  if(i %% 3 == 0 & i %% 4 != 0) {
    hap <- hap + i
  }
}
print(hap)

#문제3. x와 n을 입력하면 1부터 n까지의 수 중에서 x의 배수 합을 구해주는 사용자 정의 함수를 만들어라.
hap <- 0 
Multiple <- function(x,n) {
  for(i in 1:n) {
    if(i %% x == 0) {
      hap <- hap + i
    }
  }
  return(hap)
}

Multiple(3,100)


#gapminder 실습
install.packages("gapminder")
install.packages("dplyr")
library(gapminder)
library(dplyr)
glimpse(gapminder)
gapminder[]
gapminder[1:15, ]
gapminder[ , c("continent", "lifeExp")]
gapminder[ , c("country", "pop")]
unique(gapminder$country)

gapminder[gapminder$country == "Korea, Rep.", ]
gapminder[gapminder$country == "Korea, Rep.", "pop"]
gapminder[gapminder$country == "Korea, Rep." & gapminder$year > 1990, ]
gapminder[gapminder$country == "Korea, Rep." & gapminder$year > 1990, c("lifeExp", "pop")]

#dplyr
select(gapminder, country, lifeExp, pop) #속성(열) 추출
filter(gapminder, country == "Croatia") #샘플(행) 추출

summarise(gapminder, pop_avg = mean(pop))
summarise(group_by(gapminder, continent, country), pop_avg = mean(pop))

summarise(group_by(gapminder, continent, country), pop_avg = mean(pop))
#gapminder %>%
#  group_by(gapminder, continent, country) %>%
#  summarise(pop_avg = mean(pop))

temp1 <- filter(gapminder, country == "Croatia")
temp2 <- select(temp1, country, year, lifeExp)
temp3 <- apply(temp2[ , c("lifeExp")], 2, mean)
temp3

gapminder %>%
  filter(country == "Croatia") %>%
  select(country, year, lifeExp) %>%
  summarise(lifeExp_avg = mean(lifeExp))

##데이터 가공 : 방대한 데이터 요약
#데이터셋 읽어오기
avocado <- read.csv("C:/r/avocado.csv", header = TRUE, sep =",")
str(avocado)

#총 판매량과 평균가격 속성을 지역에 따라 구분하여 각각 요약
#dplyr 라이브러리의 group_by의 summarise 함수 사용
(x_avg <- avocado %>%
    group_by(region) %>%
    summarize(V_avg = mean(Total.Volume), P_avg = mean(AveragePrice)))

#지역별 특징을 연도별로 다시 세분화
(x_avg <- avocado %>%
  group_by(region, year) %>%
  summarize(V_avg = mean(Total.Volume), P_avg = mean(AveragePrice)))

#유기농 여부(type)를 기준으로 한 번 더 세분화
(x_avg <- avocado %>%
    group_by(region, year, type) %>%
    summarize(V_avg = mean(Total.Volume), P_avg = mean(AveragePrice)))

#시각화 : 총판매량을 연도에 따라 관찰
install.packages("ggplot2")
library(ggplot2)
(x_avg %>%
    filter(region != "Totalus") %>%
    ggplot(aes(year, V_avg, ol = type)) + geom_line() + facet_wrap(~region))

#데이터 정렬과 검색 : arrange 함수를 이용하여 총판매량 기준으로 순위, 최대값 등
arrange(x_avg, desc(V_avg))

#avocado 판매 정보를 월별 평균으로 요약
install.packages("lubridate")
library(lubridate)
(x_avg <- avocado %>%
    group_by(region, year, month(Date), type) %>%
    summarize(V_avg = mean(Total.Volume), P_avg = mean(AveragePrice)))


##winedata
wine <- read.table("C:/r/wine.data.txt", header = TRUE, sep =",")
head(wine)
n <- readLines("C:/r/wine.name.txt")
n
names(wine)[2:14] <- substr(n, 4, nchar(n))
names(wine)

#데이터셋 분할
train_set <- sample_frac(wine, 0.6)
str(train_set)


##전기 생산량
#데이터 불러오기 및 구조 변경
elec_gen <- read.csv("C:/r/electricity_generation_per_person.csv", header = TRUE, sep = ",")
names(elec_gen)
#년도 앞의 x 제거
names(elec_gen) <- substr(names(elec_gen), 2, nchar(names(elec_gen)))
names(elec_gen)

##전기 사용량
#데이터 불러오기
elec_use <- read.csv("C:/r/electricity_use_per_person.csv", header = TRUE, sep = ",")
names(elec_use)[2:56] <- substr(names(elec_use)[2:56], 2, nchar(names(elec_use)[2:56]))
names(elec_use)[2:56]

#두개의 데이터 프레임을 병합
install.packages("tidyr")
library(tidyr)
elec_gen_df <- gather(elec_gen, -ountry, key = "year", value = "ElectricityGeneration")
#데이터프레임에 이름 재설정
names(elec_gen_df) <- c("country", "year", "ElectricityGeneration")
elec_use_df <- gather(elec_use, -country, key = "year", value = "Electricityuse")
elec_gen_use <- merge(elec_gen_df, elec_use_df)
elec_gen_use
