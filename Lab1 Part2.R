# Lab1 Part2
EPI_data<-read.csv("/Users/wangyuyan/Documents/王语嫣/Lally/Data Analytics/EPI/2010EPI_data.csv",skip=1)
# Plot a Cumulative density function
plot(ecdf(EPI_data$EPI), do.points=FALSE, verticals=TRUE) 

# Plot a qq plot
help("qqnorm")
par(pty="s") 
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI)
# Plot a qq plot against the distribution
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE) # points are visible on the plot.
par(pty="s")
help("qqnorm") # read the RStudio documentation for qqnorm
help("qqplot") # read the RStudio documentation for qqplot
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI) # adding the line on the Q-Q plot
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

# Explore with DALY, WATER_H
plot(ecdf(EPI_data$DALY),do.points=TRUE,verticals = TRUE) 
qqnorm(EPI_data$DALY)
qqline(EPI_data$DALY)

plot(ecdf(EPI_data$WATER_H),do.points=TRUE,verticals = TRUE) 
qqnorm(EPI_data$WATER_H)
qqline(EPI_data$WATER_H)

# Comparing distributions
boxplot(EPI_data$EPI,EPI_data$DALY)

# InterCompare

boxplot(EPI_data$EPI,EPI_data$DALY,EPI_data$ENVHEALTH,EPI_data$ECOSYSTEM,EPI_data$AIR_H,EPI_data$WATER_H,EPI_data$AIR_EWATER_E,EPI_data$BIODIVERSITY)

# Multivariate Regression 
multivariate <-read.csv("/Users/wangyuyan/Documents/王语嫣/Lally/Data Analytics/multivariate.csv")
head(multivariate)
attach(multivariate)
help(lm)
mm <-lm(Homeowners~Immigrant)
mm # mm here is a R object. 
summary(mm)$coef # The output above shows the estimate of the regression beta coefficients (column Estimate) and 
# their significance levels (column Pr(>|t|).
# The intercept is 107494.898 and the coefficient of Immigrant variable is -6656.839.
# The estimated regression equation can be written as follow:
# Homeowners = 107494.898 + (-6656.839)*Immigrant 
# We can rewrite it as:
# Homeowners = 107494.898 - 6656.839*Immigrant.plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
# Using this formula, for each new value in Immigrant, you can predict the value for Homeowners.
# As an examle:
# For Immigrant value = 0, we will get: Homeowners = 107494.898 - 6656.839*0 = 107494.898
# for Immigrant value = 20, we will get: Homeowners = 107494.898 - 6656.839*20 = -25641.88
# Predictions can be easily made using the R function predict().
# In the following example, we predict Homeowners for two Immigrant values: 0 and 20.
# you can pass the 0 and 20 values as a concatenated list for Immigrants as follows:
newImmigrantdata <- data.frame(Immigrant = c(0,  20))
mm %>% predict(newImmigrantdata)

abline(mm)
abline(mm,col=3,lwd=3) # line color = green, line width = 3
attributes(mm)
mm$coefficients

# Creating Plots
# CHapter 2 --R graphics Cookbook
plot(mtcars$wt,mtcars$mpg)
install.packages("ggplot2")
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure,type="l")
points(pressure$temperature,pressure$pressure)

?qplot
lines(pressure$temperature,pressure$pressure/2,col="red")
points(pressure$temperature,pressure$pressure/2,col="blue")
library(ggplot2)
qplot(pressure$temperature,pressure$pressure,geom="line")
qplot(temperature,pressure, data= pressure, geom="line")
ggplot(pressure, aes(x=temperature,y=pressure))+geom_line()+geom_point()
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()

# Creating Bar graphs
barplot(BOD$demand,NAMES.ARG=BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) #generate a table of counts
qplot(mtcars$cyl) #cyl is continous here
qplot(factor(mtcars$cyl)) # treat cyl as discrete
# Bar graph of counts
qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()

# Creating Histograms
# View the distribution of one dimentional data with a histogram
hist(mtcars$mpg)
hist(mtcars$mpg,breaks = 10) #specify approximate number of bins with breaks
hist(mtcars$mpg, breaks=12)
hist(mtcars$mpg,breaks = 5)
qplot(mpg,data=mtcars,binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 5)

#Creating Box-plot
plot(ToothGrowth$supp, ToothGrowth$len) #using plot() function and pass it a factor of x-values and a vector of y-values.
# Formula Syntax
boxplot(len~supp, data=ToothGrowth) # if the tow vectors are in the same df, you can use the formula syntax.
#With this syntax you can combine two variables on the x-axis.
# put interaction of two variables on x-axis
boxplot(len~supp + dose, data=ToothGrowth)
#with ggplot2 you can get the same results above
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len,geom="boxplot")
# if the two vectors are in the same df, you can use the following
qplot(supp,len,data=ToothGrowth,geom = "boxplot")
#in ggplot2, the above is equal to
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
# Using three seperate vectors
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom="boxplot")
# You can write the same thing above, get the columns from the df
qplot(interaction(supp, dose),len, data=ToothGrowth,geom="boxplot")
# Using ggplot() to do the same thing
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()




library(dplyr)
library(nycflights13)
head(flights)
summary(flights)


# filter() function in dplyr allows us to select a subset of rows in a dataframe.
# it allows us to filter by conditions
filter(flights,month == 10, day == 4, carrier =='AA')
head(filter(flights, month == 10, day == 4, carrier == 'AA'))
# instead of using the dplyr, we can use the [ ] notation, it is long and messy :(
head(flights[flights$month == 10 & flights$day == 4 & flights$carrier == 'AA' , ]) # here I have to keep calling the dataframe name, and use the logical operators with '&' and combine them.

# slice() in dplyr
# slice() function  allows us to select rows by the position
slice(flights, 2:15) # selecting first 15 rows

# arrange() in dplyr
# arrange() function works similar to filter() function except that instead of filtering or selcting rows, it reorder the rows
arrange(flights,year,month,day, arr_time)
head(arrange(flights,year,month,day,arr_time))
# if I want to use the descending time instead of accending time, 
head(arrange(flights,year,month,day, desc(arr_time)))

# select() in dplyr
select(flights,carrier)
head(select(flights,carrier))
# We can add aditional columns easily 
head(select(flights, carrier, arr_time))
head(select(flights, carrier, arr_time, day))
head(rename(flights, airline.carrier = carrier))

# distinct() in dplyr
# distinct() function in dplyr helps us to select the distinct or unique values in a column.
distinct(select(flights, carrier))

# mutate() in dplyr
# in additing to selecting sets of existing columns in the dataframe, sometimes 
# we need to add new columns that are functions of existing columns in the dataframe.
# we can use the mutate() function to do that.
head(mutate(flights, MyNewColumn = arr_delay - dep_delay))
# If you only want to see the new column instead of calling the mutate, you can 
# use the transmute() fuction.
# The difference between the mutate() and transmute() is that mutate() function returns
# the entire dataframe along with the new column and the transmute() shows only the new column.
head(transmute(flights, MyNewColumn = arr_delay - dep_delay))

# summarise() in dplyr
# The summarize() allows us to summarize the data frame into a single row using another aggrigate function
summarise(flights, avg_air_time = mean(air_time, na.rm = TRUE)) # average airtime
summarise(flights, TotalFlightTime = sum(air_time, na.rm = TRUE)) # Total Flight Time

# sample_n() in dplyr
# sample_n() function allows us to pick random number of rows that we wish to choose:
sample_n(flights, 15) # random 15 rows. 
sample_n(flights, 71) # random 71 rows. 

# sample_frac() in dplyr
# if you wan to pick a percentage of rows, sample_frac() function allow us to do that,
# you need to assign the fraction, example: 30% = 0.3, similaly 10% = 0.1
sample_frac(flights,0.1) # sample with a 10% of rows from the total number of rows 
sample_frac(flights, 0.3) # sample with a 30% of rows from the total number of rows 
sample_n(flights, 30)
sample_frac(flights, 0.5)


