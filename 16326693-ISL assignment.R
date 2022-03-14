#1.a ISLR 2.4 Applied Problem 8
#setting up the directory
setwd("D:/R")
#reading the file from the directory
college <- read.csv("College.csv")
head(College)
#1.b Now you should see that the first data column is Private. Note that another column labeled row.names now appears before the
#Private column. However, this is not a data column but rather the name that R is giving to each row.
rownames (college) <- college[, 1]
View (college)
college <- college[, -1]
head(college)
View (college)
#1.c.i Use the summary() function to produce a numerical summary
#of the variables in the data set
summary(college)
#1.c.ii. Use the pairs() function to produce a scatterplot matrix of the first ten columns or variables of the data. 
#Recall that you can reference the first ten columns of a matrix A using A[,1:10].
college$Private <- as.factor(college$Private)
#pairs(college[, 1:10])
head(college)


#1.c.iii. Use the plot() function to produce side-by-side boxplots of Outstate versus Private.
plot(college$Private, college$Outstate, xlab = "Private", ylab = "Out-of-state tuition in dollars")


#iv. Create a new qualitative variable, called Elite, by binning the Top10perc variable. We are going to divide universities
#into two groups based on whether or not the proportion of students coming from the top 10 % of their high school classes exceeds 50 %.
Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(Elite)
plot(college$Elite, college$Outstate, xlab = "Elite University", ylab = "Tuition in $")
#1.c.v. Use the hist() function to produce some histograms with differing numbers of bins for a few of the quantitative variables. You may find the command par(mfrow = c(2, 2))
#useful: it will divide the print window into four regions so that four plots can be made simultaneously. Modifying the
#arguments to this function will divide the screen in other ways.
par(mfrow=c(2,2))
hist(college$Apps, xlab = "Applications Received", col="red",main = "")
hist(college$perc.alumni, col=2, xlab = "Perc of alumni who donate",main = "")
hist(college$S.F.Ratio, col=3, breaks=10, xlab = "Student/Faculty ratio",main = "")
hist(college$Expend, breaks=100, xlab = "Instructional expenditure per student", col="green",main = "")
#1.c.vi. Continue exploring the data, and provide a brief summary of what you discover.
summary(college$Apps)
summary(college$PhD)
row.names(college)[which.max(college$Top10perc)]
acceptance_rate <- college$Accept / college$Apps
row.names(college)[which.min(acceptance_rate)] 
plot(college$Outstate, college$Grad.Rate)

#2.a Which of the predictors are quantitative, and which are qualitative?
Auto <- read.csv("Auto.csv",na.strings="?")
Auto <- na.omit(Auto)
str(Auto)
#Except NAME and HORSEPOWER remaining are quantitative 


#2.b What is the range of each quantitative predictor? You can answer this using the range() function.
sapply(Auto[, -c(4, 9)], mean)
qualitative_columns <- which(names(Auto) %in% c("name", "origin", "origins"))
qualitative_columns
sapply(Auto[, -qualitative_columns], range)
#2.(c) What is the mean and standard deviation of each quantitative predictor?
sapply(Auto[, -qualitative_columns], mean)
sapply(Auto[, -qualitative_columns], sd)
#2.(d) Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each 
#predictor in the subset of the data that remains?
subset <- Auto[-c(10:85), -c(4,9)]
sapply(subset, range)
sapply(subset, mean)
sapply(subset, sd)
#2.e) Using the full data set, investigate the predictors graphically,using scatterplots or other tools of your choice.
#Create some plots highlighting the relationships among the predictors. Comment on your findings
#pairs(Auto)
pairs(Auto[, -qualitative_columns])
with(Auto, plot(mpg, weight))
with(Auto, plot(mpg, cylinders))
# for 20 observations
Auto.sample <- Auto[sample(1:nrow(Auto), 20), ]
# ordering them
Auto.sample <- Auto.sample[order(Auto.sample$mpg), ]
# plot by using "dotchart"
with(Auto.sample, dotchart(mpg, name, xlab = "mpg"))
with(Auto, plot(origin, mpg), ylab = "mpg")
#####Findings#####*
#With mpg we are seeing an inverse effect for horsepower, weight and displacement
#mpg has increased drastically over years and it has twice in a decade.
#European and US cars have lower mpg than Japanese cars.
#2.f) Suppose that we wish to predict gas mileage (mpg) on the basis of the other variables. Do your plots suggest that any of the
#other variables might be useful in predicting mpg? Justify your answer.
Auto$horsepower <- as.numeric(Auto$horsepower)
cor(Auto$weight, Auto$horsepower)
cor(Auto$weight, Auto$displacement)
cor(Auto$displacement, Auto$horsepower)
#For predictors we can use origin, cylinders, year and hoursepower
#We cannot use weight and displacement as they are correlated highly with each other and also with horsepower.


#3(a) To begin, load in the Boston data set. The Boston data set is part of the ISLR2 library.
#How many rows are in this data set? How many columns? What do the rows and columns represent?
library(MASS)
Boston$chas <- as.factor(Boston$chas)
nrow(Boston)
ncol(Boston)
#rows indicate the U.S Census Tracts in Boston area
#column indicate the measures of census 
#3.(b) Make some pairwise scatterplots of the predictors (columns) in this data set. Describe your findings.
par(mfrow = c(2, 2))
plot(Boston$nox, Boston$crim)
plot(Boston$rm, Boston$crim)
plot(Boston$age, Boston$crim)
plot(Boston$dis, Boston$crim)
pairs(Boston)
#The bottom two plots, age and dis, are mirror images of one another.


#3.(c) Are any of the predictors associated with per capita crime rate? If so, explain the relationship.
par(mfrow = c(2, 2))
plot(Boston$crim ~ Boston$zn, log = 'xy', col = 'green')
plot(Boston$crim ~ Boston$age, log = 'xy', col = 'green')
plot(Boston$crim ~ Boston$dis, log = 'xy', col = 'red')
plot(Boston$crim ~ Boston$lstat, log = 'xy', col = 'red')
#Based on the graph, we may conclude that the predictors have a relationship with crim.
#As the per capita crime rises, the number of units built before 1940 rises as well.
#For instance, the average distance between five Boston job centers increases while per capita crime falls.
#For lstat As the population grows, so does the per capita crime rate..

#3.d) Do any of the census tracts of Boston appear to have particularly high crime rates? Tax rates? 
#Pupil-teacher ratios? Comment on the range of each predictor.
#crime rate
hist(Boston$crim, breaks = 50)
nrow(Boston[Boston$crim > 20, ])
#tax rate
hist(Boston$tax, breaks = 50)
nrow(Boston[Boston$tax == 666, ])
#Pupil-teacher ratios
hist(Boston$ptratio, breaks = 50)
nrow(Boston[Boston$ptratio > 20, ])
#3.e) How many of the census tracts in this data set bound the Charles river?
nrow(Boston[Boston$chas == 1, ])
#3.f) What is the median pupil-teacher ratio among the towns in this data set?
median(Boston$ptratio)


#3.g) Which census tract of Boston has lowest median value of owneroccupied homes? What are the values of the other predictors
#for that census tract, and how do those values compare to the overall ranges for those predictors? Comment on your findings.
t(subset(Boston,medv==min(Boston$medv)))
row.names(Boston[min(Boston$medv), ])
range(Boston$tax)
Boston[min(Boston$medv), ]$tax
#From the data we can say that Boston is the least desirable place to live.


#3.h) In this data set, how many of the census tracts average more than seven rooms per dwelling? More than eight rooms per
#dwelling? Comment on the census tracts that average more than eight rooms per dwelling.
nrow(Boston[Boston$rm > 7, ])
nrow(Boston[Boston$rm > 8, ])


#4.a.i Is there a relationship between the predictor and the response?
library(ISLR)
library(MASS)
data("Auto")
head(Auto)
lm.fit<-lm(mpg~horsepower,data=Auto)
summary(lm.fit)
#Because the p value is 2e-16, we do have a relationship between predictor and responder.
#4.a.ii. How strong is the relationship between the predictor and the response?
#R^{2} value is equal to 61% of variable (horsepower) in mpg. Mean for mpg is 23.44 and RSE of lm.fit is 4.9 which shows the % error of 20.9%
#4.a.iii. Is the relationship between the predictor and the response positive or negative?
#There is a negative association between predictor and responder. The linear regression shows how much horsepower a car has..
#4.a.iv. What is the predicted mpg associated with a horsepower of 98? What are the associated 95 % confidence and prediction intervals?
predict(lm.fit,data.frame(horsepower=c(98)),interval="prediction")
predict(lm.fit,data.frame(horsepower=c(98)),interval="confidence")
#4.b Plot the response and the predictor. Use the abline() function to display the least squares regression line.
attach(Auto)
plot(horsepower,mpg)
abline(lm.fit,lwd=5,col="blue")
#4.c) Use the plot() function to produce diagnostic plots of the least squares regression fit. 
#Comment on any problems you see with the fit.
which.max(hatvalues(lm.fit))
par(mfrow = c(2,2))
plot(lm.fit)
#We have non-linearity for the data in the plot for residuals and fitted values.
#For leverage and standardized residuals we have outliers. 


#5.a) Produce a scatterplot matrix which includes all of the variables in the data set.
pairs(Auto)
#5.b) Compute the matrix of correlations between the variables using
#the function cor(). You will need to exclude the name variable, cor() which is qualitative.
Auto$name<-NULL
cor(Auto,method = c("pearson"))
#5.c.i. Is there a relationship between the predictors and the response?
lm.fit<-lm(mpg~.,data=Auto)
summary(lm.fit)
#The F-statistic value displays that we have a relationship between predictors and the response.
#5.c.ii. Which predictors appear to have a statistically significant relationship to the response?
#We have origin, weight, year, and displacement relationships. This may be seen in the p-values for predictors..
#5.c.iii. What does the coefficient for the year variable suggest?
#Except for mpg, all of the predictors are constant. Cars have a fuel efficiency of roughly 1 mpg per year.
#The effect of a rise for one year is equivalent to 0.75 in mpg, as shown by the coefficient of year variable..
#5.d) Use the plot() function to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit.
#Do the residual plots suggest any unusually large outliers? Does the leverage plot identify any observations with unusually high leverage?
which.max(hatvalues(lm.fit))
par(mfrow = c(2,2))
plot(lm.fit)
#We have a non-linear relationship between the response and predictors for the first graph.
#The Residuals are distributed normally and are skewed towards right for second one.
#For this model the error assumption is not true for constant variance for third graph.
#We do not have any leverage points for fourth graph. But this one stands out as potential leverage point.
#5.e) Use the * and : symbols to fit linear regression models with interaction effects. 
#Do any interactions appear to be statistically significant?
lm.fit = lm(mpg ~.-name+displacement:weight, data = Auto)
summary(lm.fit)
#For p-vales, the interaction between displacement and weight is statistically significant.
#However, there is no connection between displacement and cylinders in this case.
#5.f) Try a few different transformations of the variables, such as log(X), ???X, X2. Comment on your findings.
par(mfrow = c(2, 2))
plot(log(Auto$horsepower), Auto$mpg)
plot(sqrt(Auto$horsepower), Auto$mpg)
plot((Auto$horsepower)^2, Auto$mpg)
lm.fit = lm(mpg ~.-name+I((displacement)^2)+log(displacement)+displacement:weight, data = Auto)
summary(lm.fit)
#We get the most linear plot for log transformation. We are predicting all the things based on horsepower.

#6.a) Fit a multiple regression model to predict Sales using Price, Urban, and US.
library("ISLR")
?Carseats
head(Carseats)
str(Carseats)
lm.fit = lm(Sales ~ Price+Urban+US, data= Carseats)
summary(lm.fit)
#6.b) Provide an interpretation of each coefficient in the model. Be careful-some of the variables in the model are qualitative!
#The coefficient for PRICE variable can be said as the average of price raise of a dollar is in decrease of 54.4588492 units in the sales remaining predictors are fixed. 
#For URBAN variable the coefficient can be interpreted as average of unit sales in urban location are 21.9161508 units lesser than of rural location adn all others are fixied.
#For US variable can be interpreted as average of unit sales in US store is 1200.5726978 higher than that of non US stores and the remaining predictors are fixed.
#6.c) Write out the model in equation form, being careful to handle the qualitative variables properly.
#Sales=13.0434689+(???0.0544588)???Price+(???0.0219162)???Urban+(1.2005727)???US+??
#US=1 if the store is in the US and 0 if not, and with Urban=1 if the store is in an urban location and 0 if not.
#6.d) For which of the predictors can you reject the null hypothesis H0 : ??j = 0?
#For the PRICE and US variables we will reject null hypothesis.
#6.e) On the basis of your response to the previous question, fit a smaller model that only uses the predictors for which there is
#evidence of association with the outcome.
fits <- lm(Sales ~ Price + US, data = Carseats)
summary(fits)
#6.f) How well do the models in (a) and (e) fit the data?
#Nearly 23.9262888% of variability can be explained in this model. For smaller model the R2 is better than that of larger model.
#6.g) Using the model from (e), obtain 95 % confidence intervals for the coefficient(s).
confint(fits)
#6.h) Is there evidence of outliers or high leverage observations in the model from (e)?
par(mfrow = c(2, 2))
plot(fits)
