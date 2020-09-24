data <- read.csv(file="C:/Project/최종.csv", header=T)
data
names(data) <- c("place","y","wifi","card","company")
data
x <- data[,-1]
x
head(x)
shapiro.test(x$y)
shapiro.test(x$wifi)
shapiro.test(x$card)
shapiro.test(x$company)

model <- lm(y ~  wifi + card  + company  , data=x)
summary(model)

plot(model)
return

