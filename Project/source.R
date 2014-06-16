data <- mtcars


data$am <- as.factor(data$am)
levels(data$am) <- c("automatic","manual")

pairs(data,panel=panel.smooth,main="\"mtcars\" dataset, Numeric Variables")

boxplot(data$mpg~data$am,col="blue",main="MPG and type of transmission",
        xlab="Transmission type",ylab="MPG, Miles/(US) gallon")


#univariate model
fit <- lm(mpg~am,data)
summary(fit)
plot(fit)


#2vars model
columns <- colnames(data)
columns <- columns[!(columns %in% c("mpg","am"))]
formulas <- paste0("mpg~am+",columns)
models <- length(formulas)
multivariateFit <- lapply(1:models, function(x) lm(formula=formulas[x],data))
lapply(1:models, function(x) anova(fit,multivariateFit[[x]]))
pvalues <- sapply(1:models, function(x) anova(fit,multivariateFit[[x]])[2,6])
formulas[which.min(pvalues)]
min(pvalues)
summary(multivariateFit[[which.min(pvalues)]])
plot(multivariateFit[[which.min(pvalues)]])



#3 vars model
columns <- colnames(data)
columns <- columns[!(columns %in% c("mpg","am","hp"))]
formulas <- paste0("mpg~am+hp+",columns)
models <- length(formulas)
multivariateFit <- lapply(1:models, function(x) lm(formula=formulas[x],data))
lapply(1:models, function(x) anova(fit,multivariateFit[[x]]))
pvalues <- sapply(1:models, function(x) anova(fit,multivariateFit[[x]])[2,6])
formulas[which.min(pvalues)]
min(pvalues)
summary(multivariateFit[[which.min(pvalues)]])
plot(multivariateFit[[which.min(pvalues)]])


#quadrivariate model
columns <- colnames(data)
columns <- columns[!(columns %in% c("mpg","am","hp","wt"))]
formulas <- paste0("mpg~am+hp+wt+",columns)
models <- length(formulas)
multivariateFit <- lapply(1:models, function(x) lm(formula=formulas[x],data))
lapply(1:models, function(x) anova(fit,multivariateFit[[x]]))
pvalues <- sapply(1:models, function(x) anova(fit,multivariateFit[[x]])[2,6])
formulas[which.min(pvalues)]
min(pvalues)
summary(multivariateFit[[which.min(pvalues)]])
plot(multivariateFit[[which.min(pvalues)]])
