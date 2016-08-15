#Liz Metcalfe
#15/8/2016
#Information Retrieval exercise for City Science Corp
library(MASS)
library(car)

#change directory and then:
mydata = read.csv("emissions.csv")

#mydata = CO2

#variables present
summary(mydata)
library(Hmisc)
describe(mydata)

#Find index of value
mydata[which(mydata$extra_urban_metric == 97.9), ]

#take columns subset
omegasub = omega_without_outliers[, c("urban_metric", "extra_urban_metric", "combined_metric")]

#find mean of subetted groups by manufacturer type
aggregate(mydata$noise_level, list(mydata$manufacturer), mean)

# Find rows with "sunroof" in the strings description variable
irisSubset <- mydata[grep("sunroof", mydata$description), ]

#Add NA to all blank spaces
mydata$particulates_emissions[mydata$particulates_emissions==""]  <- NA 

# Put all in lower case using the command:
mydata$manufacturer = tolower(mydata$manufacturer)


# 2-Way Frequency Table 
attach(mydata)
mytable <- table(A,B) # A will be rows, B will be columns 
mytable # print table 

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages


#plot kernal density to look at distribution of data
plot(density(mydata$conc))

# correlation coeficient
cor(mydata$conc,mydata$uptake)
# covariance coeficient
cov(mydata$conc,mydata$uptake)
# Correlations with significance levels
library(Hmisc)
rcorr(mydata$conc, mydata$uptake, type="pearson") # type can be pearson or spearman


#t-test to test differences
# paired t-test
t.test(mydata$conc,mydata$uptake,paired=TRUE) # where y1 & y2 are numeric
# significantly different


#plot numeric values against each other to look of 
plot(mydata$conc,mydata$uptake)
abline(...)

#take subset and then explore further
mydata1=subset(mydata,mydata$Type=="Quebec")

# for i loop

for i in (   ){
....
}

#linear regression
model1 = lm(mydata$conc~mydata$uptake)
par(mfrow=c(2,2))
plot(model1)

#Anova
# One Way Anova (Completely Randomized Design)
#lower case letters are numeric variables and upper case letters are factors.
fit <- aov(y ~ A, data=mydataframe)

#generalised linear models
fit <- glm(F~x1+x2+x3,data=mydata,family=binomial())
summary(fit)

# compare models
fit1 <- lm(y ~ x1 + x2 + x3 + x4, data=mydata)
fit2 <- lm(y ~ x1 + x2)
anova(fit1, fit2)

# K-fold cross-validation
library(DAAG)
cv.lm(df=mydata, fit, m=3) # 3 fold cross-validation