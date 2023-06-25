##########################################################
#Created on Tue March 23, 2023
# Tutorials for statistics using R programming

# author: https://www.youtube.com/@easydatascience2508

###########################################################



# Tutorial 1. Descriptive statistics

# Descriptive statistics with summary()
myvars <- c("mpg", "hp", "wt")
summary(mtcars[myvars])

mtcars$carb<-as.factor(mtcars$carb)
summary(mtcars$carb)



# Descriptive statistics via sapply()
mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, 
           skew=skew, kurtosis=kurt))
}

myvars <- c("mpg", "hp", "wt")
sapply(mtcars[myvars], mystats)


# Descriptive statistics via describe() in the Hmisc package
library(Hmisc)
myvars <- c("mpg", "hp", "wt")
describe(mtcars[myvars])


# Descriptive statistics via stat.desc() in the pastecs package 
library(pastecs)
myvars <- c("mpg", "hp", "wt")
stat.desc(mtcars[myvars])



# Descriptive statistics via describe() in the psych package
library(psych)
myvars <- c("mpg", "hp", "wt")
describe(mtcars[myvars])



# Descriptive statistics by group using by()
dstats <- function(x)sapply(x, mystats)
myvars <- c("mpg", "hp", "wt")
by(mtcars[myvars], mtcars$am, dstats)




# Descriptive statistics for groups defined by multiple variables
dstats <- function(x)sapply(x, mystats, na.omit=TRUE)
myvars <- c("mpg", "hp", "wt")
by(mtcars[myvars], 
   list(Transmission=mtcars$am,
        Engine=mtcars$vs), 
   FUN=dstats)

# Section 7.1.4
# Summarizing data interactively with dplyr

library(dplyr)
library(carData)
Salaries %>%
  summarize(med = median(salary), 
            min = min(salary), 
            max = max(salary))

Salaries %>%
  group_by(rank, sex) %>%
  summarize(n = length(salary),
            med = median(salary), 
            min = min(salary), 
            max = max(salary))

Salaries %>%
  group_by(rank, sex) %>%
  select(yrs.service, yrs.since.phd) %>%
  summarize_all(mean)














# Tutorial 2. Frequency and contingency tables

library(vcd)
head(Arthritis)

# one way table
mytable <- table(Arthritis$Improved)
mytable                       # counts
prop.table(mytable)           # proportions
prop.table(mytable)*100       # percents

# two way table
mytable <- xtabs(~ Treatment+Improved, data=Arthritis)
mytable  # counts

margin.table(mytable, 1)    # total counts for Treatment 
prop.table(mytable, 1)      # row proportions (rows add to 1)

margin.table(mytable, 2)    # total counts for Improved
prop.table(mytable, 2)      # column proportions (columns add to 1)

prop.table(mytable)         # cell proportions (all cells add to 1)
addmargins(mytable)         # cell counts with row and column sums
addmargins(prop.table(mytable)) # cell proportions with row and column proportions

addmargins(prop.table(mytable, 1), 2) # row proportions with row sums
addmargins(prop.table(mytable, 2), 1) # column proportions with column sums

# Listing 7.8 Two-way table using CrossTable
library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)

# Listing 7.9 Three-way contingency table
mytable <- xtabs(~ Treatment+Sex+Improved, data=Arthritis) 
mytable          
margin.table(mytable, 1)  # totals for Treatment
margin.table(mytable, 2)  # totals for Sex
margin.table(mytable, 3)  # totals for Improved
margin.table(mytable, c(1, 3)) # totals for Treatment by Improved

# Treatment by Sex for each Level of Improved
ftable(mytable)
ftable(prop.table(mytable, c(1, 2))) # proportions sum to 1 over index omitted
ftable(addmargins(prop.table(mytable, c(1, 2)), 3)) 
ftable(addmargins(prop.table(mytable, c(1, 2)), 3)) * 100













### Tutorial 3. Bivariate Correlations

states<- state.x77[,1:6]
cov(states)     #covariance
cor(states)     #Pearson's correlation

cor(states, method="spearman")     #Spearman's correlation

cor(states, method="kendall")       #Kendall's correlation


#between one set of variables and another
x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[,c("Life Exp", "Murder")]
cor(x,y)


# Testing a correlation coefficient for significance
cor.test(states[,3], states[,5])

# Correlation matrix and tests of significance via corr.test()
library(psych)
corr.test(states, use="complete")















### Lecture 4. Random number generation

#For uniformly distributed (flat) random numbers, use runif(). 
#By default, its range is from 0 to 1.

runif(1)

# Get a vector of 4 numbers
runif(4)


# Get a vector of 3 numbers from 0 to 100
runif(3, min=0, max=100)


# Get 3 integers from 0 to 100
# Use max=101 because it will never actually equal 101
floor(runif(3, min=0, max=101))


# This will do the same thing
sample(1:100, 3, replace=TRUE)


# To generate integers WITHOUT replacement:
sample(1:100, 3, replace=FALSE)

#To generate numbers from a normal distribution, use rnorm(). 
#By default the mean is 0 and the standard deviation is 1.

rnorm(4)


# Use a different mean and standard deviation
rnorm(4, mean=50, sd=10)


# To check that the distribution looks right, make a histogram of the numbers
x <- rnorm(400, mean=50, sd=10)
hist(x)


# Create Matrix with Random Values in Range

#create matrix of 10 random values between 1 and 20
random_matrix <- matrix(runif(n=10, min=1, max=20), nrow=5)
random_matrix


#Create Matrix with Random Integers in Range

#create matrix of 10 random integers between 1 and 20
random_matrix <- matrix(round(runif(n=10, min=1, max=20), 0), nrow=5)

random_matrix


  
#set.seed() to make generation reproducible
set.seed(1)

#create matrix with 10 random numbers between 1 and 20
random_matrix <- matrix(runif(n=10, min=1, max=20), nrow=5)

#view matrix
random_matrix
















### 5. Binomial distribution

## Binomial distribution

#dbinom() Function is used to find probability at a particular 
#value for a data that follows binomial distribution i.e. it finds:
# Syntax:
# dbinom(k, n, p)

#Example: finds the probability at k=3 in total 10 trials
#with p =0.2
dbinom(3, size = 10, prob = 0.2)


#displays a dataset containing the binomial probability distribution 
# variable k from 0 to 10 of total 10 trials
probabilities <- dbinom(x = c(0:10), size = 10, prob = 0.2)

plot(0:10, probabilities, type = "l")


#pbinom() Function is used to find the cumulative probability of 
# a data following binomial distribution till a given value
#ie it finds P(X <= k)
# Syntax:
# pbinom(k, n, p)


#probability of x occur equal or less than 3 times among total 10
# trials, with probability 0.2 for each trial
pbinom(3, size = 10, prob = 0.2)


#cumulative probability of binomial distribution variable,
# from 0 to 10 of total 10 trials, with probability 0.2 for each trial
plot(0:10, pbinom(0:10, size = 10, prob = 0.2), type = "l")


#qbinom() Function is used to find the nth quantile, that is 
# if P(x <= k) is given, it finds k.
#Syntax:
# qbinom(P, n, p)


#find the value where cumulativa probability is 0.8791261
# in total 10 trials, with success probabilkty 0.2 for each trial
qbinom(0.8791261, size = 10, prob = 0.2)

#find the quantile values for each cumulative probability value
# from 0 to 1. 
x <- seq(0, 1, by = 0.1)
y <- qbinom(x, size = 10, prob = 0.2)
plot(x, y, type = 'l')



#rbinom() Function generates n random variables of a particular 
# binomial probability.
#Syntax:
# rbinom(n, N, p)

#generate 8 random variables of binomial distribution where
# n = 8, p = 0.2
rbinom(8, size = 10, prob = 0.2)


#generate 1000 random variables of binomial distribution where
# n = 10, p = 0.2
hist(rbinom(1000, size = 10, prob = 0.2))












### Lecture 6. Binomial,Hypergeometric, Negative Binomial, 
### Geometric distributions

## Binomial distribution

#dbinom() Function is used to find probability at a particular 
#value for a data that follows binomial distribution i.e. it finds:
# Syntax:
# dbinom(k, n, p)

#Example: finds the probability at k=3 in total 10 trials
#with p =0.2
dbinom(3, size = 10, prob = 0.2)


#displays a dataset containing the binomial probability distribution 
# variable k from 0 to 10 of total 10 trials
probabilities <- dbinom(x = c(0:10), size = 10, prob = 0.2)

plot(0:10, probabilities, type = "l")


#pbinom() Function is used to find the cumulative probability of 
# a data following binomial distribution till a given value
#ie it finds P(X <= k)
# Syntax:
# pbinom(k, n, p)


#probability of x occur equal or less than 3 times among total 10
# trials, with probability 0.2 for each trial
pbinom(3, size = 10, prob = 0.2)


#cumulative probability of binomial distribution variable,
# from 0 to 10 of total 10 trials, with probability 0.2 for each trial
plot(0:10, pbinom(0:10, size = 10, prob = 0.2), type = "l")


#qbinom() Function is used to find the nth quantile, that is 
# if P(x <= k) is given, it finds k.
#Syntax:
# qbinom(P, n, p)


#find the value where cumulativa probability is 0.8791261
# in total 10 trials, with success probabilkty 0.2 for each trial
qbinom(0.8791261, size = 10, prob = 0.2)

#find the quantile values for each cumulative probability value
# from 0 to 1. 
x <- seq(0, 1, by = 0.1)
y <- qbinom(x, size = 10, prob = 0.2)
plot(x, y, type = 'l')



#rbinom() Function generates n random variables of a particular 
# binomial probability.
#Syntax:
# rbinom(n, N, p)

#generate 8 random variables of binomial distribution where
# n = 10, p = 0.2
rbinom(10, size = 10, prob = 0.2)


#generate 1000 random variables of binomial distribution where
# n = 10, p = 0.2
hist(rbinom(1000, size = 10, prob = 0.2))




#Hypergeometric distribution
#density function: dhyper(x_dhyper, m, n, k)
# x: represents the number of white ball draw without replacement
# m: he number of white balls in the urn.
# n: the number of black balls in the urn.
# k: the number of balls drawn from the urn,


# Specify x-values for dhyper function
x_dhyper <- seq(0, 20, by = 1)
x_dhyper 

# Apply dhyper function
y_dhyper <- dhyper(x_dhyper, m = 45, n = 30, k = 20)   

# Plot dhyper values
plot(y_dhyper) 


## cumulative probability
# Specify x-values for phyper function
x_phyper <- seq(0, 20, by = 1)    
x_phyper
# Apply phyper function
y_phyper <- phyper(x_phyper, m = 40, n = 20, k = 30)  

# Plot phyper values
plot(y_phyper)  



#quantile

# Specify x-values for qhyper function
x_qhyper <- seq(0, 1, by = 0.02)        

# Apply qhyper function
y_qhyper <- qhyper(x_qhyper, m = 50, n = 20, k = 30)    

# Plot qhyper values
plot(y_qhyper)

#draw random number
set.seed(400)                                 
N <- 10000                                      

# Draw N hypergeometrically distributed values
y_rhyper <- rhyper(N, m = 50, n = 20, k = 30) 
y_rhyper         

# Plot of randomly drawn hyper density
hist(y_rhyper,                                          
     breaks = 50,
     main = "")





# Negative Binomial distribution
#density (probability function)
#Syntax: dnbinom(vec, size, prob)

#Parameters:
# vec: x failures prior to the rth success 
#size: target for number of successful trials,
#prob: probability of success in each trial


#An oil company has a p = 0.20 chance of striking oil when 
#drilling a well. What is the probability the company drills 
#x = 7 wells to strike oil r = 3 times?
  
r = 3
p = 0.20
n = 7 - r
# probability
dnbinom(x = n, size = r, prob = p)


# Vector of x-values
x <- seq(0, 10, by = 1)

# Calling dnbinom() Function
# probability of x times total failure before getting 3 timts success
y <- dnbinom(x, size = 3, prob = 0.5)
y



#Geometric distribution
# dgeom function to plot

# Specify x-values for dgeom function
x_dgeom <- seq(2, 10, by = 1)    

# Apply dgeom function 
#probability of x trials until getting a success
y_dgeom <- dgeom(x_dgeom, prob = 0.5)    

# Plot dgeom values 
plot(y_dgeom)  















### Lecture 7.Correlograms

# Load the required libraries

# Example 1. Correlograms using corrplot package
#install.packages('corrplot')
library(corrplot)

# Load the data
data(mtcars)

# Calculate the correlation matrix
cor_matrix = cor(mtcars)

# Create the correlogram
corrplot(cor_matrix, type = "upper", 
         method = "circle",              #other shapes:square,ellipse,,,
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 45)    #color of text label.
                                            #text label string rotation



#Example 2. Correlograms using ggstatsplot package

library(ggstatsplot)

# loading data
data(mtcars)
ggcorrmat(mtcars, 
          method = "pearson",     #other choices: spearman, kendall
          label = TRUE,
          cor.vars = c("mpg", "disp", "hp",
                       "drat", "wt", "qsec"),
          size = 2)



#Example 3. Correlograms using lares package

# loading the library
library(lares)

# loading the data
data(mtcars)
corr <- cor(mtcars, method = "spearman")
corrplot(corr, method = "square", 
         title = "Correlogram of mtcars data set",
         tl.col = "blue", 
         tl.srt = 45)



















### Lecture 8. Poisson distribution

#Consider a Random Variable X with Poisson distribution given as
#mean and variance are lambda in a specific period. 

# dpois()
#The function dpois() calculates the probability of a random variable 
# that is available within a certain range (discrete)
 
#  dpois(k, lambda) 

dpois(10, 10)
dpois(2, 15)

k_vec <- seq(1:20)
plot(dpois(k_vec,10))



# ppois()
# This function calculates the probability of a random variable 
# that will be equal to or less than a number.

#  ppois(q, lambda) 

ppois(10, 10)
ppois(2, 15)

k_vec <- seq(1:20)
plot(ppois(k_vec,10))



# qpois()
#This function is used for generating quantile of a given Poisson’s 
#distribution. In probability, quantiles are marked points that 
# divide the graph of a probability distribution into intervals
# (continuous ) which have equal probabilities.

#  qpois(q, lambda) 

qpois(0.2, 10)
qpois(0.7, 15)

q_vec <- seq(1,10)/10
plot(qpois(q_vec,10))




# rpois()
#This function is used for generating random numbers from a given 
# Poisson’s distribution.

#  rpois(n, lambda) 

rpois(20, 10)



























### Lecture 9. Uniform distribution
#Probability Density Function
# dunif(x, min,max)   WHERE x is a value or vector
# default value for min and max is 0 and 1


# generating a sequence of values
x <- seq(0, 10 , by = 0.2) 
print ("dunif value")

# calculating density function
dunif(x, min = 3, max = 8)


plot(dunif(x, min = 3, max = 8))


#Cumulative probability distribution
# punif(x,   min,   max)    WHERE x is a value or vector of values

x <- seq(0, 10 , by = 0.2) 

punif (x , min =3 , max = 8)

plot(punif (x , min =3 , max = 8))



# Quantile for a probability
# qunif() method is used to calculate the corresponding quantile 
# for any probability (p) for a given uniform distribution.
# qunif(p,  min, max)
# p represents the probability or vector of probabilities

print ("Quantile Function Value")

# calculating the quantile function value
q <- seq(0, 1 , by = 0.1) 

plot(qunif(q, min = 3, max = 8))


#random number generation
#  runif() is used to generate a sequence of random following 
# a uniform distribution.  
# runif(n, min, max)   WHERE n represents how many random number
#will be generated

print("Generate 10 random numbers of uniform distribution between 3 and 8")
runif(10, min=3, max=8) 





















### Lecture 10.Normal distribution

# dnorm(x, mean, sd) to calculate normal density
# Example:
  
# creating a sequence of values 
# between -10 to 10 with a step of 0.1
x = seq(-10, 10, by=0.1)

y = dnorm(x, mean(x), sd(x))


# Plot the graph.
plot(x, y)



#pnorm() function is used to calculate the cumulative normal probability

#Example:
  
# creating a sequence of values
# between -10 to 10 with a difference of 0.1
x <- seq(-10, 10, by=0.1)

y <- pnorm(x, mean = 2.5, sd = 2)


# Plot the graph.
plot(x, y)


# qnorm() function is the inverse of pnorm() function. It takes 
#the probability value and gives output which corresponds to 
# the probability value.

#Example:
  
# Create a sequence of probability values 
# incrementing by 0.02.
x <- seq(0, 1, by = 0.05)

y <- qnorm(x, mean=10, sd=2)

# Plot the graph.
plot(x, y)


# rnorm() function is used to generate a vector of random numbers
# which are normally distributed.

#Example:
  
# Create a vector of 1000 random numbers
# with mean=90 and sd=5
x <- rnorm(10000, mean=90, sd=5)

# Create the histogram with 50 bars
hist(x, breaks=50)










### Lecture 11. Exponential distribution

#dexp() Function returns the corresponding values of the 
# exponential density for an input vector of quantiles.

#Syntax:
  
# dexp(x_dexp, rate)
# rate here represents the mean number occurence of poisson events
#during a unit time period.

# Specify x-values
x_dexp <- seq(1, 10, by = 0.1) 

# Apply dexp() function               
y_dexp <- dexp(x_dexp, rate = 5)    

# Plot dexp values 
plot(y_dexp)



#pexp() function returns the corresponding values of the exponential 
# cumulative distribution function for an input vector of quantiles.

#Syntax:
  
# pexp(x_pexp, rate )

# Specify x-values
x_pexp <- seq(1, 10, by = 0.1)                                     

# Apply pexp() function
y_pexp <- pexp(x_pexp, rate = 5) 

# Plot values                  
plot(y_pexp)   



#qexp() function gives the possibility, we can use the qexp 
# function to return the corresponding values of the quantile function.

#Syntax:
  
#  qexp(x_qexp, rate)

# Specify x-values 
x_qexp <- seq(0, 1, by = 0.1)                     

# Apply qexp() function
y_qexp <- qexp(x_qexp, rate = 5)

# Plot values                   
plot(y_qexp)  



#rexp() function is used to simulate a set of random numbers 
# drawn from the exponential distribution.

#Syntax:
  
#  rexp(N, rate )
# Specify size         
N <- 100

# Draw exp distributed values
y_rexp <- rexp(N, rate = 5)

# Plot exp density  
hist(y_rexp, breaks = 50, main = "")


#Example 
# Suppose that a system contains a certain type of component whose 
# time, in years, to failure is given by T. The random variable T 
#is modeled nicely by the exponential distribution with mean time 
# to failure β = 5.  what is the probability that a component 
# is still functioning at the end of 8 years?
  
#Solution : The probability that a given component has failure
# within 8 years is given by  P(X < 8)

p_less_8 <- pexp(8,1/5)

#the probability that a component 
# is still functioning at the end of 8 years?
# equals 1 - P(X < 8)

1 - p_less_8 





















### Lecture 12. Gamma distribution

#dgamma() function is used to create gamma density plot which 
# Syntax:
# dgamma(x_dgamma, shape, scale) 
# scale default 1

# R program to plot gamma distribution

# Specify x-values for gamma function
x_dgamma <- seq(0, 5, by = 0.2)   

# Apply dgamma function
y_dgamma <- dgamma(x_dgamma, shape = 6, scale = 1/5) 

# Plot dgamma values
plot(y_dgamma)


#pgamma() function is used in cumulative distribution function 
# (CDF) of the gamma distribution.

#Syntax:
# pgamma(x_pgamma, shape, scale)


# Specify x-values for gamma function
x_pgamma <- seq(0, 10, by = 0.2)   

# Apply pgamma function
y_pgamma <- pgamma(x_pgamma, shape = 3) 

# Plot pgamma values
plot(y_pgamma)


#qgamma() Function
#It is known as gamma quantile function of the gamma distribution 
#is the inverse operation of pgamma()

#Syntax:
#  qgamma(x_qgamma, shape)

# Specify x-values for gamma function
x_qgamma <- seq(0, 1, by = 0.01)   

# Apply qgamma function
y_qgamma <- qgamma(x_qgamma, shape = 3) 

# Plot qgamma values
plot(y_qgamma)



#rgamma() Function is used for generating random number in gamma 
# distribution.

#Syntax:
#  rgamma(N, shape, scale)

# Specify sample size
N <- 1000  

# Draw N gamma distributed values
y_rgamma <- rgamma(N, shape = 2, scale=1/5) 

# Print values to RStudio console
y_rgamma 

# Plot of randomly drawn gamma density
hist(y_rgamma, breaks = 200, main = "")


#Example
#Suppose that telephone calls arriving at a particular switchboard 
# follow a Poisson process with an average of 5 calls coming per
# minute. What is the probability that within 1 minutes
# 2 calls have come in to the switchboard?
# Solution : The Poisson process applies, with time until 2 Poisson 
#events following a gamma distribution with β = 1/5 and α = 2. 
#Denote by X = 1 the time in minutes that transpires before 2 calls 
#come.The required probability is given by a gamma distribution
# with alpha = 2,beta = 1/5

alpha = 2
beta = 1/5
p_x_less_1 <- pgamma(1, shape= alpha,scale= beta)


p_x_less_1



















### Lecture 13. Shapiro–Wilk Test for normality

library("dplyr")

# Using the ToothGrowth package
# loading the data set
my_data <- ToothGrowth

str(my_data)

#shapiro.test of 'len' in ToothGrowth data
shapiro.test(my_data$len)


#From the output obtained we can assume normality. The p-value 
#is greater than 0.05. Hence, the distribution of the given data 
#is not different from normal distribution significantly.






















### Lecture 14. Chi-square distribution and Chi-square test in R

#dchisq gives the density function.
# dchisq(x, df)

df = 10
vec <- 1:20

print ("Density function values")

dchisq(vec, df = df)

plot(dchisq(vec, df = df))



#qchisq gives the quantile function.
#qchisq(p, df)

free = 5
qchisq(.75, df=free)



# pchisq gives the cumulative probability
# pchisq(q, df)
# defining degrees of freedom
df = 5

# calculating for the values in the interval [0,5]
print ("Calculating for the values [0,5]")
pchisq(5, df = df)




# rchisq(n, df) returns n random numbers from the chi-square distribution.

x <- rchisq(50000, df = 5)



hist(x, 
     freq = FALSE, 
     xlim = c(0,16), 
     ylim = c(0,0.2),
     breaks=100)





# Chi-Square Test

#The chi-square test of independence evaluates whether there 
#is an association between the categories of the two variables.

#We will take the survey data in the MASS library which represents 
# the data from a survey conducted on students.

# load the MASS package
library(MASS)        
print(str(survey))


#Our aim is to test the hypothesis whether the students smoking 
#habit is independent of their exercise level at .05 significance
# level.

# Create a data frame from the main data set.
stu_data = data.frame(survey$Smoke,survey$Exer)

# Create a contingency table with the needed variables.           
stu_data = table(survey$Smoke,survey$Exer) 

print(stu_data)

# applying chisq.test() function
print(chisq.test(stu_data))

#As the p-value 0.4828 is greater than the .05, we conclude 
#that the smoking habit is independent of the exercise level of 
#the student and hence there is a weak or no correlation between 
# the two variables.

































### Lecture 15. Beta distribution

# dbeta(xvalues,alpha,beta)

# Plot for Beta Density(1,1)  where we can observe the 
# uniform distribution between 0 and 1.

# Creating the Sequence
x_seq = seq(0, 1, by = 0.1)

# Plotting the beta density
plot(x_seq, dbeta(x_seq, 1,1), xlab="X",
     ylab = "Beta Density", type = "l",
     col = "Red")


#Plot for Beta Density(2,1) where we can observe linearly 
# increasing function

# Creating the Sequence
x_seq = seq(0,1, by=0.1)

# Case 2
plot(x_seq, dbeta(x_seq, 2,1), xlab="X",
     ylab = "Beta Density", type = "l",
     col = "Red")


# Plot for Beta Density(2,2) 
# Creating the Sequence
x_seq = seq(0,1, by=0.1)

# Case 3
plot(x_seq, dbeta(x_seq, 2,2), xlab = "X",
     ylab = "Beta Density", type = "l",
     col = "Red")



## Cumulative Distributive Functions
# pbeta(x, alpha, beta)

# The Beta Distribution
plr.data <- data.frame(
  player_avg <- c(seq(0, 1, by = 0.025)),
  stringsAsFactors = FALSE
)

# Print the data frame.           
print(plr.data)
print(plr.data$player_avg)


# Cummilative distribution function
by2<- pbeta(plr.data$player_avg, shape1 = 4, shape2 = 6)
par(mar = rep(2,4))
plot(by2)



# quantile distribution function, to generate x in terms of probility
#qbeta(prob, alpha, beta)
prob_data <- c(seq(0, 1, by = 0.025)) 

by3 <- qbeta(prob_data, shape1 = 4, shape2 = 6)
par(mar = rep(2,4))
plot(by3)



#random number generator, to generate n beta random numbers
#rbeta(n, alpha, beta) 
b4 <- rbeta(20, shape1 = 5, shape2 = 8)
b4
























### Lecture 16. Lognormal distribution

# dlnorm() function is used to compute the log normal value 
# of the probability density function

# dlnorm(x, meanlog = 0, sdlog = 1) 

# meanlog default 0, sdlog default 1

# Creating x-values for density
x <- seq(1, 10, by = 0.1)

# Calling dlnorm() function
y <- dlnorm(x)

# Plot a graph
plot(x, y)


# create another density of lognormal , with meanlog 5, sdlog 2

y <- dlnorm(x, 5,2)

# Plot a graph
plot(x, y)



# plnorm() function is used to compute the log normal value of 
# the cumulative probability density function. 

# plnorm(x, meanlog = 0, sdlog = 1) 

# meanlog default 0, sdlog default 1

# Creating x-values for density
x <- seq(1, 10, by = 0.1)

# Calling plnorm() function
y <- plnorm(x)
plot(x,y)

#using meanlog 5, sdlog 2
# Calling plnorm() function
y <- plnorm(x,5,2)
plot(x,y)


#qlnorm() function is used to compute the value of log normal 
#quantile values, given the cumulative probabilities
#it is the inverse operation of plnorm()

# qlnorm(probs, meanlog = 0, sdlog = 1) 

# meanlog default 0, sdlog default 1

# Creating x-values for density
probs <- seq(0, 1, by = 0.05)

# Calling qlnorm() function
x <- qlnorm(probs)
plot(probs,x)


#using meanlog 5, sdlog 2
x <- qlnorm(probs, 5, 2)
plot(probs,x)


# rlnorm() function is used to generate random log normal 
# variates

# rlnorm(n, meanlog = 0, sdlog = 1) 

# meanlog default 0, sdlog default 1

# Set sample size
N <- 18

# Calling rlnorm() Function
x <- rlnorm(N)
x

#using meanlog 5, sdlog 2
x <- rlnorm(N, 5, 2)
x 





