data <- read.csv(file="C:/Project/종합.CSV", header=T)
data
data1 <- data[, 2:7]
data1
data1 <- data1[-6,]
data1
x = cor(data1)
round(x,2)
install.packages("corrplot")
library(corrplot)
corrplot(x)
corrplot(x,method="number")

names(data1) <- c("wifi","cars","company","population","generation","house")
is.data.frame(pa)
head(data1)
model1 = lm(formula =pa ~ cars + wifi + company + population + generation, data=data1)
summary(model1)

win.graph()
par(mfrow = c(1,2))
plot(pa ~ cars + wifi + company + population + generation + house, data=data1)
Return
abline(data, col="red")

