name <- c("김영민", "김철수", "이수민", "김나영", "지수진")
sex <- c("남", "남", "여", "여", "여")
nation <- c("korea", "korea", "korea", "korea", "korea")
major <- c("컴퓨터공학과", "에너지공학과", "전산통계학과", "화학공학과", "패션의류학과")
age <- c(21, 24, 23, 24, 25)
df <- data.frame(name, sex, nation, major, age)
df
df
subset(df, age > 23)
subset(df, major == "컴퓨터공학과")
subset(df, nation == "korea" & age > 24, select = -c(major))
