library(e1071)
plot(iris)

View(iris)

plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species)
plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species)

s<-sample(150,100)
col<-c("Petal.Length", "Petal.Width", "Species")
iris.train <- iris[s,col]
iris.test <- iris[-s,col]

svmfit <- svm(Species~., data = iris.train, kernel = "linear", cost = 1, scale = FALSE)
print(svmfit)

plot(svmfit, iris.train[, col])

tuned <- tune(svm, Species ~., data = iris.train, kernel = "linear", ranges = list(cost=c(0.001,0.01,.1,1,10,100)))
# Will show the optimal cost parameter
summary(tuned)

p<- predict(svmfit, iris.test[, col],type = "class")
plot(p)

table(p, iris.test[,3])
mean(p== iris.test[,3])

