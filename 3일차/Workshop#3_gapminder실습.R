#gapminder 라이브러리
#세계 각국의 기대수명 lifeExp, 1인당 국내총생산 gdpPercap
#인구pop 데이터 등을 집계해 놓은 gapminder 데이터 셋의 일부

install.packages("gapminder")
install.packages("dplyr")
library(gapminder)
library(dplyr)
glimpse(gapminder)
gapminder[1:15,]
gapminder[, c("country", "lifeExp")]
gapminder[, c("country", "lifeExp", "year")]

gapminder[gapminder$country == "Croatia", ] #Croatia만 추출
gapminder[gapminder$country == "Croatia", "pop"] #Croatia, 인구만 추출
gapminder[gapminder$country == "Croatia" & gapminder$year > 1990, c("lifeExp", "pop")] #Croatia의 1990년도 이후의 기대 수명과 인구 추출

#dplyr 라이브러리를 이용한 데이터 가공
#select 함수 이용
select(gapminder, country, year, lifeExp) #속성(열)추출
filter(gapminder, country == "Croatia") #샘플(행) 추출

#summarise 함수, 그룹별 통계 지표를 한번에 산출
#groub_by 함수, 데이터 프레임의 factor형 속성을 활용해 그룹으로 분류 가능
summarise(gapminder, pop_avg = mean(pop))
summarise(group_by(gapminder, continent, country),pop_avg = mean(pop))

#%>%(파이프라인) 연산자 : 일련의 가공작업을 연결, 앞의 명령어를 처리한 결과를
#다음 명령어로 전달하는 역할
summarise(group_by(gapminder, continent,country), pop_avg = mean(pop))
gapminder %>%
  group_by(continent, country) %>%
  summarise(pop_avg = mean(pop))    #윗 코드와 결과가 동일하게 출력됨 

#한 명령어로 처리한 결과를 다시 다른명령어로 처리시
temp1 <- filter(gapminder, country == "Croatia")
temp2 <- select(temp1, country, year, lifeExp)
temp3 <- apply(temp2[ , c("lifeExp")], 2, mean)
temp3  # 복습필요

#%>% 연산자를 이용하면
gapminder %>%
  filter(country == "Croatia") %>%
  select(country, year, lifeExp) %>%
  summarise(lifeExp_avg = mean(lifeExp))




unique(gapminder$country) #country 중복값 제거
#문제 1. 우리나라의 1990년도 이후의 기대 수명과 인구 추출
gapminder[gapminder$country == "Korea, Rep." & gapminder$year > 1990, c("lifeExp", "pop")]
#문제 2. 북한의 1990년도 이후의 기대 수명과 인구 추출
gapminder[gapminder$country == "Korea, Dem. Rep." & gapminder$year > 1990, c("lifeExp", "pop")]