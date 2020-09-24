data = read.csv(file="C:/Project/종합.CSV", header=T)
data
data1 <- data[, 2:9]
data1
names(data1) <- c("wifi","cars","company","population","generation","house","park","y")
data1
y = data1$y
X = data1[, 1:7]
feature_names = data.feature_names[2:]

from sklearn.tree import DecisionTreeClassifier

tree1 = DecisionTreeClassifier(criterion='entropy', max_depth=1, random_state=0).fit(X, y)