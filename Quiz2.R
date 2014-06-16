colnames(mtcars)
attach(mtcars)


#Q3 confidence for the expected value (ie. the value of the regression line at that point)
y <- mpg
x <- wt

fit <- lm(y~x)
summary(fit)
confint(fit)
newdata <- data.frame(x = mean(x))
predict.lm(fit, newdata,interval="confidence",level=.95)




#Q5 confidence for interval for a predicted value
y <- mpg
x <- wt

fit <- lm(y~x)
summary(fit)
confint(fit)
newdata <- data.frame(x = 3)
predict.lm(fit, newdata,interval="prediction",level=.95)
fit$coefficients[1]


#Q6
y <- mpg
x <- wt/2

fit <- lm(y~x)
summary(fit)
confint(fit)

-10.689-qt(.975,30)*1.118




#Q9
#Q3
y <- mpg
x <- wt

fit <- lm(y~x)

yhat_den <- rep(mean(y),32)
yhat_num <- fit$fitted.values 


num <- sum((y-yhat_num)^2)
den <- sum((y-yhat_den)^2)
