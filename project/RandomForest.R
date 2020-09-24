install.packages("randomForest")
library(randomForest)
data <- read.csv(file="C:/Project/최종.csv", header=T)
data
names(data) <- c("place","y","wifi","card","company")
data1 <- data[,-5]
data1 <- data1[,-1]
model <- randomForest(y~., data=data1)
model

model2 = randomForest(formula = y~., data=data1, ntree=300, mtry=4, na.action = na.omit)
model2

model3 = randomForest(y~., data=data1, importance=T, na.action=na.omit)
varImpPlot(model3)

ntree <- c(400,500,600)
mtry <- c(2:4)
param <- data.frame(n=ntree, m=mtry)
param

for(i in param$n) { 
  cat('ntree =', i, '\n')
  for(j in param$m){
    cat('mtry =', j, '\n')
    model_park <- randomForest(y~., data= data1, ntree=i, mtry=j, na.action=na.omit)
    print(model_park)
  }
}

