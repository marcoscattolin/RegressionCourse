
plot(mtcars$wt,mtcars$mpg,col=mtcars$cyl,pch=19)

#QUESTION1
data <- mtcars
data$cyl <- as.factor(data$cyl)
boxplot(mtcars$mpg~mtcars$cyl)
plot(mtcars$wt,mtcars$mpg,col=mtcars$cyl,pch=19)
fit1 <- lm(data=data,mpg~cyl+wt-1)
summary(fit1)$coefficients[3]-summary(fit1)$coefficients[1]

#QUESTION2
data <- mtcars
data$cyl <- as.factor(data$cyl)
plot(mtcars$wt,mtcars$mpg,col=mtcars$cyl,pch=19)
adj <- lm(data=data,mpg~cyl+wt-1)
notAdj <- lm(data=data,mpg~cyl-1)

summary(adj)$coefficients[3]-summary(adj)$coefficients[1]
summary(notAdj)$coefficients[3]-summary(notAdj)$coefficients[1]  #difference is bigger when weight is disregarded


#QUESTION3
data <- mtcars
data$cyl <- as.factor(data$cyl)
plot(mtcars$wt,mtcars$mpg,col=mtcars$cyl,pch=19)
interaction <- lm(data=data,mpg~cyl+wt+(wt*cyl))
noInteraction <- lm(data=data,mpg~cyl+wt)

anova(noInteraction,interaction) #pvalue for model 2 is larger than 0.5, we should not include the interaction




#QUESTION4
data <- mtcars
data$cyl <- as.factor(data$cyl)
plot(mtcars$wt,mtcars$mpg,col=mtcars$cyl,pch=19)
fit <- lm(data=data,mpg~I(wt*.5)+factor(cyl))

##Question 5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
plot(x,y)
fit <- lm(y~x)
abline(fit$coefficients)
hatvalues(fit)



##Question 6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
plot(x,y)
fit <- lm(y~x)
abline(fit$coefficients)
hatvalues(fit)
dfbetas(fit)[which.max(hatvalues(fit)),]

