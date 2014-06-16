attach(mtcars)
txType <- as.factor(mtcars$am)
levels(txType) <- c("automatic","manual")

boxplot(mpg~txType,col="blue",main="MPG and type of transmission",
        xlab="Transmission type",ylab="MPG, Miles/(US) gallon")

pairs(mtcars,panel=panel.smooth,main="\"mtcars\" dataset, Numeric Variables")

#univariate model
fit <- lm(mpg~txType)
summary(fit)
plot(fit)
plot(hatvalues(fit))
confint(fit)








#2 TX and HP
fit <- lm(mpg~txType+hp)
summary(fit)
plot(fit)
plot(hatvalues(fit))
confint(fit)
plot(hp,mpg,col=txType,pch=19)
abline(fit$coefficients[1],fit$coefficients[3])
abline(fit$coefficients[1]+fit$coefficients[2],fit$coefficients[3],col="red")



#3 TX and carb
fit <- lm(mpg~txType+carb)
summary(fit)
plot(fit)
plot(hatvalues(fit))
confint(fit)
plot(carb,mpg,col=txType,pch=19)
abline(fit$coefficients[1],fit$coefficients[3])
abline(fit$coefficients[1]+fit$coefficients[2],fit$coefficients[3],col="red")





fit <- lm(mpg~txType+cyl)
summary(fit)


fit <- lm(mpg~txType+disp)
summary(fit)

fit <- lm(mpg~txType+drat)
summary(fit)

fit <- lm(mpg~txType+wt)
summary(fit)

fit <- lm(mpg~txType+qsec)  #depends inderectly on HP
summary(fit)


fit <- lm(mpg~txType+vs)  #depends and therefore on HP
summary(fit)


fit <- lm(mpg~txType+gear)
summary(fit)


pairs(mtcars[,c("mpg","qsec","vs","hp")])



fit <- lm(mpg~txType+hp+wt+carb)
summary(fit)
