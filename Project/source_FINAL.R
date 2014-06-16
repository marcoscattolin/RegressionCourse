data <- mtcars
summary(data)

#convert variables
data$cyl <- as.factor(data$cyl)
data$vs <- as.factor(data$vs)
data$am <- as.factor(data$am)
levels(data$am) <- c("automatic","manual")
data$gear <- as.factor(data$gear)
data$carb <- as.factor(data$carb)
summary(data)



#step 1
vars <- "am"
outcome <- "mpg"
formula <- paste0(outcome,"~",paste0(vars,collapse="+"))
fit <- lm(formula,data)

#step 2 and 3
loop <-  TRUE
while(loop == TRUE){
        #generate all possible formulas excluding already used variables
        remainingVars <- setdiff(colnames(data),c(outcome,vars))
        newFormulas <- paste0(formula,"+",remainingVars)
        
        #generate new linear models
        numModels <- length(newFormulas)
        newFit <- lapply(1:numModels, function(x) lm(newFormulas[x],data))
        
        #compare new linear models with baseline
        anovaPvalues <- sapply(1:numModels, function(x) anova(fit,newFit[[x]])[2,6])
        candidateFit <- which.min(anovaPvalues)
        
        #update baseline model if a new better model has been found
        betterModel <- anovaPvalues[candidateFit] < 0.05
        amSignificant <- summary(newFit[[candidateFit]])$coefficients[[2,4]] < 0.05
        if (betterModel & amSignificant){
                formula <- newFormulas[candidateFit]
                fit <- newFit[[candidateFit]]
                vars <- c(vars,remainingVars[candidateFit])
        } else {loop <- FALSE}        
}


aa <- influence.measures(fit)
