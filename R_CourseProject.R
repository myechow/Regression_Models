
setwd('D:\\Graduate Courses\\Miscellaneous\\Data Science Specialization\\Regression Models')

library(ggplot2)
library(knitr)


data(mtcars)

help(mtcars)

str(mtcars)

am <- factor(mtcars$am, labels=c("Automatic","Manual"))  # (where, Automatic=0, Manual=1)

# taking other variables as factors

vs <- factor(mtcars$vs) 
gear <- factor(mtcars$gear)
carb <- factor(mtcars$carb)

head(mtcars)

summary(mtcars)

boxplot(mpg ~ am, data = mtcars, col = (c("orange","pink")), ylab = "Miles Per Gallon", xlab = "Transmission Type")

aggregate(mtcars$mpg,by=list(mtcars$am),FUN=mean)

t.test(mpg ~ am, mtcars)

#model with only am (Simple)
summary(lm(mpg~am, data=mtcars))

#model with all variables (Multivariable)
summary(lm(mpg ~ ., mtcars))

#Variance Analysis
summary(aov(mpg ~ ., data = mtcars))

#Model Selection
summary(step(lm(mpg ~ ., mtcars), direction = "backward", trace = FALSE))

# 95% confidence interval for am
f <- lm(mpg ~ ., mtcars)
step_f <-step(f, direction = "backward", trace = FALSE)
confint(step_f)['am', ]

# Residual Plot
par(mfrow = c(2,2))
plot(step(lm(mpg ~ ., mtcars), direction = "backward", trace = FALSE))
