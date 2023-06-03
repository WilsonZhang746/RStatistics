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
# n = 10, p = 0.2
rbinom(10, size = 10, prob = 0.2)


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


