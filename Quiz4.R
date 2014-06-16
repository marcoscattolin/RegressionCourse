#QUESTION1
library(MASS)
data <- shuttle
data$useNum <- (1-unclass(data$use))+1
fit <- glm(data=data, useNum~wind,family="binomial")
exp(fit$coefficients[1])/(exp(fit$coefficients[1]+fit$coefficients[2]))

#QUESTION2
fit <-glm(data=data, useNum~wind+magn,family="binomial")
exp(fit$coefficients[1])/(exp(fit$coefficients[1]+fit$coefficients[2]))

#QUESTION3
glm(data=data, use~wind,family="binomial")
glm(data=data, I(1-(as.numeric(data$use)-1))~wind,family="binomial")


#QUESTION4
data <- InsectSprays
fit <- glm(data=data, count~spray-1+offset(log(10)),family="poisson")
exp(fit$coefficients[1])/exp(fit$coefficients[2])


#QUESTION5




#QUESTION6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
knots <- 0
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
fit <- lm(y ~ xMat - 1)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)
fit$coefficients[3]+fit$coefficients[2]
