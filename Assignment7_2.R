# 1. Write a program to create barplots for all the categorical columns in mtcars.
library(readr)
library(ggplot2)
mtcars
str(mtcars)
library(dplyr)
mtcars1 <- mutate(mtcars,
                  cyl = as.factor(cyl),
                  disp = as.factor(disp),
                  vs = as.factor(vs),
                  am = as.factor(am),
                  gear = as.factor(gear),
                  carb = as.factor(carb))
str(mtcars1)

is.fact <- sapply(mtcars1, is.factor)
mtcars2 <- mtcars1[,is.fact]

str(mtcars2)
par(mfrow= c(2,3))

lapply(lapply(mtcars2[,1:6], table), barplot)

# 2. Create a scatterplot matrix by gear types in mtcars dataset.

install.packages("car")
library(car)
str(mtcars)
scatterplotMatrix(~mpg+drat+wt+qsec|gear, data=mtcars,main="Scatterplot matrix by gear types in mtcars")

# 3. Write a program to create a plot density by class variable.
par(mfrow = c(1,1))
x <- mtcars$mpg
h <- hist(x, breaks = 10, col = "pink",
          xlab = "MPG",
          main = "Density plot of mpg")
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd= sd(x))
yfit <- yfit*diff(h$mids[1:2]*length(x))
lines(xfit, yfit, col="Blue", lwd = 3)
