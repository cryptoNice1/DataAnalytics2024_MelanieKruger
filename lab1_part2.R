library(ggplot2)

#plotting a scatterplot
plot(mtcars$wt,mtcars$mpg)
ggplot(mtcars, aes(wt,mpg)) + geom_point()

#plotting a line with points
plot(pressure$temperature,pressure$pressure,type="l")
points(pressure$temperature,pressure$pressure)
ggplot(pressure, aes(temperature,pressure)) + geom_line() + geom_point()

#plotting a bar graph
barplot(BOD$demand, names.arg = BOD$Time)
ggplot(BOD, aes(demand)) + geom_bar() #hi

barplot(table(mtcars$cyl))
ggplot(mtcars,aes(factor(cyl))) + geom_bar()

#histogram
hist(mtcars$mpg,breaks=10)
ggplot(mtcars, aes(mpg)) + geom_histogram(binwidth=10)

#boxplots
plot(ToothGrowth$supp, ToothGrowth$len)
ggplot(ToothGrowth, aes(supp,len)) + geom_boxplot()

boxplot(len ~ supp, data = ToothGrowth)
ggplot(ToothGrowth,aes(supp,len)) + geom_boxplot()

boxplot(len ~ supp + dose, data = ToothGrowth)
ggplot(ToothGrowth, aes(interaction(supp, dose), y=len)) + geom_boxplot()
