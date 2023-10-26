---
title: "Course Project for Regression Models"
author: "Myesha C Iqbal"
date: "25 October, 2023"
output:
  pdf_document: default
  html_document: default
---

## Synopsis

#### Question: You work for Motor Trend, a magazine about the automobile industry. Looking at a data set of a collection of cars, they are interested in exploring the relationship between a set of variables and miles per gallon (MPG) (outcome). They are particularly interested in the following two questions:

##### 1. "Is an automatic or manual transmission better for MPG"

##### 2. "Quantify the MPG difference between automatic and manual transmissions"

### Data Processing

#### Setting up working directory
```{r}
setwd('D:\\Graduate Courses\\Miscellaneous\\Data Science Specialization\\Regression Models')
```

#### Display session information

```         
sessionInfo()
```

#### Loading libraries
```{r}         
library(ggplot2)               
library(knitr)
```

#### Loading the dataset and performing some basic exploratory data analyses

```{r}        
data(mtcars)

help(mtcars)

str(mtcars)
```

##### Converting categorical variables as factors
```{r}        
    am <- factor(mtcars$am, labels=c("Automatic","Manual"))  # (where, Automatic=0, Manual=1)

    vs <- factor(mtcars$vs) 
    
    gear <- factor(mtcars$gear)
    
    carb <- factor(mtcars$carb)
```

##### Conitnuing diagnosis of the dataset
```{r}
head(mtcars)

summary(mtcars)
```


###### Boxplot created for comparison visualization
```{r echo=FALSE}
boxplot(mpg ~ am, data = mtcars, col = (c("orange","pink")), ylab = "Miles Per Gallon", xlab = "Transmission Type")
```

#### Statistical Inference & Residual Model Analysis

###### Calculating mean of mpg for cars with Automatic and Manual transmission

```{r}         
aggregate(mtcars$mpg,by=list(mtcars$am),FUN=mean)

t.test(mpg ~ am, mtcars)
```

###### Model with only am (Simple)

```{r}
summary(lm(mpg~am, data=mtcars))
```

###### Model with all variables (Multivariable)

```{r}    
summary(lm(mpg ~ ., mtcars))
```

###### Variance Analysis

```{r}      
summary(aov(mpg ~ ., data = mtcars))
```

###### Model Selection

```{r}       
summary(step(lm(mpg ~ ., mtcars), direction = "backward", trace = FALSE))
```

###### 95% confidence interval for am

```{r}
f <- lm(mpg ~ ., mtcars)
step_f <-step(f, direction = "backward", trace = FALSE)
confint(step_f)['am', ]
```
###### The confidence interval does include 0 and the p-value is larger than the 0.05 threshold. Failed to reject Null Hypothesis.

######  Residual Plot
```{r echo=FALSE}
par(mfrow = c(2,2))
plot(step(lm(mpg ~ ., mtcars), direction = "backward", trace = FALSE))
```



### Conclusion

######  There is a significant difference in mpg for Automatic and Manual transmission. A manual transmission will have slightly more Miles/(US) gallons than an automatic transmission. 
######  The model selection process showed that weight(wt), horsepower(hp), & cylinders(cyl) are statistically significant covariates when determining mpg. Moreover, the multivariable regression model provided an R-squared value of over .83, suggesting that the multivariable model can explain 83% or more of the variance. 

