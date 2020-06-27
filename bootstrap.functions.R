
library(tidyverse)
library(ggplot2)
library(readxl)

#Give a 95% t-confidence interval for the mean amount of oil recovered from all wells in this area
oil <- read_excel("oil.xlsx")
head(oil)
str(oil)
summary(oil)

## calculate components of test statistic 
n<-dim(oil)[1]
xbar <- mean(oil$barrels)
xbar 
#[1] 48.24844


sd <- sd(oil$barrels)
sd
#[1] 40.23979


## critical value 
se<-sd/sqrt(n)
se
#[1] 5.029974

qt(0.95, df=63) #95% confidence interval, degrees of freedom n-1
#[1] 1.669402

##Confidence Interval for one sample 95 percentile
xbar+c(-1.96,1.96)*qt(0.95, df=63)*sd/sqrt(n)
#[1] 31.79022 64.70665


#one-sample t test using builtin t.test function 
t.test(oil$barrels)

# One Sample t-test
# data:  oil$barrels
# t = 9.5922, df = 63, p-value = 6.198e-14
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#  38.19684  58.30004
# sample estimates:
#  mean of x 
#  48.24844 

hist(oil$barrels)
plot(oil)

?samp

#One sample Bootstrap Confidence Interval for the mean(from packet)
#this function take in a data set and the number of simulations that we want to run

bootStrapCI1 <- function(oil, nsim){
  n<-length(oil)
  bootCI <- c()
  
  for(i in 1:nsim){
    bootSamp<-sample(1:n, n, replace=TRUE)
    thisXbar<-mean(oil[bootSamp])
    bootCI<-c(bootCI, thisXbar)
  }
  return(bootCI)
}

# Qantile Method 
#Quantile Method for bootstrap confidence interval for one mean
oilBootCI<-bootStrapCI1(oil$barrels, nsim=1000)
quantile(oilBootCI, c(0.05, 0.95))

# console
#  5%      95% 
#  40.76961 57.05344 



# Hybrid Method: t-clitical with Bootstrap standard error
se <-sd(oilBootCI)/sqrt(n)
se
#[1] 0.6231499

mean(oil$barrels)+c(-1.96,1.96)*qt(0.95, df=n-1)*se            

#[1] 46.20947 50.28740
# when you have large enough sample size, these two methods are both effective despite the lack of normality!

#To investigate water quality, the Columbus Dispatch took water samples at 16 Ohio State Park swimming areas in central Ohio. Those samples were taken to laboratories and tested for E.coli, which are bacteria that can cause serious gastrointestinal problems. If a 100-milliliter sample (about 3.3 ounces) of water contains more than 130 E. coli bacteria, it is considered unsafe. Here are summary statistics for the E. coli levels found by the laboratories:
# n=16
# x¯=56.28
# s=77.29

#### One sample bootstrap hypothesis test for the mean  
ecoli<-c(291.0, 190.4, 47.0, 86.0, 44.0, 18.9, 1.0, 50.0,
         10.9, 45.7, 28.5, 8.6, 9.6, 16.0, 34.0, 18.9)

# By hand
xbar<-mean(ecoli)
xbar #56.28125

sd<-sd(ecoli)
sd #77.28992

n<-length(ecoli) #16
n

se<-sd/sqrt(n)
se #19.32248

mu_0<-130 # One-sided upper alternative;  If a 100-milliliter sample (about 3.3 ounces) of water contains more than 130 E. coli bacteria, it is considered unsafe. 

test_stat<-(xbar-mu_0)/se
test_stat #-3.81518

#One sided CI 
LTCI<-(xbar-mu_0)*se   #lower tail confidence interval 
LTCI #[1] -1424.429


####student t distribution
pt(test_stat, df=n-1, lower.tail=F) 
# [1] 0.999155

hist(ecoli)
plot(ecoli)

#### Due to the strong skew and small sample size, create a bootstrap confidence interval for the mean.
# Bootstrap confidence interval for the mean
ecoliBootCI<-bootStrapCI1(ecoli, nsim=10000)
# Approximated sampling distribution
hist(ecoliBootCI)


# Quantile Method 
quantile(ecoliBootCI, c(0.05, 0.95))
##     5%      95% 
## 28.53656 89.37031 

sd(ecoliBootCI)
# [1] 18.57439

# Hybrid Method (T-critical with Bootstrap SE)
se<-sd(ecoliBootCI)/sqrt(n)
se # [1] 4.643597
mean(ecoli)+c(-1.96,1.96)*qt(0.95, df=n-1)*se

# [1] 40.32595 72.23655

#Are these data good evidence that on average the E. coli levels in these swimming areas were unsafe?
t.test(ecoli, mu=mu_0, alternative="greater") 
#mu	 mu_0=130
#a number indicating the true value of the mean (or difference in means if you are performing a two sample test).

#One Sample t-test
#data:  ecoli
#t = -3.8152, df = 15, p-value = 0.9992
#alternative hypothesis: true mean is greater than 130
#95 percent confidence interval:
 # 22.40797      Inf
#sample estimates:
#  mean of x 
#56.28125 

t.test(ecoli, alternative="greater") 
#p-value = 0.005357

t.test(ecoliBootCI)
#One Sample t-test

#data:  ecoliBootCI
#t = 303.6, df = 9999, p-value < 2.2e-16
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  56.02812 56.75631
#sample estimates:
#  mean of x 
#56.39222 

t.test(ecoli)
#One Sample t-test

#data:  ecoli
#t = 2.9127, df = 15, p-value = 0.01071
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  15.09636 97.46614
#sample estimates:
#  mean of x 
#56.28125 


#####Bootstrap hypothesis test 
bootStrapHyp1<-function(ecoli, mu0, nsim, alternative){
  testStat<-mean(ecoli)
  nullDat<-ecoli-mean(ecoli)+mu0
  n<-length(ecoli)

  bootNull<-c()
  
  for(i in 1:nsim){
    bootSamp<-sample(1:n, n, replace=TRUE)
    thisXbar<-mean(nullDat[bootSamp])
    bootNull<-c(bootNull, thisXbar)
  }
  
  if(alternative=="greater"){
    pval<-mean(bootNull>=testStat)
  }
  if(alternative=="less"){
    pval<-mean(bootNull<=testStat)
  }
  if(alternative=="not equal"){
    pval<-min(mean(bootNull>=testStat), mean(bootNull<=testStat))*2
  }
  return(pval)
}

#Perform a bootstrap hypothesis test to assess whether there is data good evidence that on average the E. coli levels in these swimming areas were unsafe.
ecoliBootHyp<-bootStrapHyp1(ecoli, mu0=130, 
                            nsim=10000, alternative = "greater")
ecoliBootHyp
#[1] 1 

ecoliBootHyp<-bootStrapHyp1(ecoli, mu0=130, 
                            nsim=10000, alternative = "less")
ecoliBootHyp
# [1] 0

#What if we want to make inference about the median??
  
  ## Bootstrap for median 
  nsim<-1000
bootMed<-c()
for(i in 1:nsim){
  bootSamp<-sample(1:n, n, replace=TRUE)
  thisMed<-quantile(ecoli[bootSamp], 0.5)
  bootMed<-c(bootMed, thisMed)
}

hist(bootMed)

quantile(bootMed, c(0.025, 0.975))
# 2.5% 97.5% 
#  16    47 

##What if we want to make inference about the standard deviation??
  
  ## Bootstrap for standard deviation 
  bootSd<-c()
for(i in 1:nsim){
  bootSamp<-sample(1:n, n, replace=TRUE)
  thisSd<-sd(ecoli[bootSamp])
  bootSd<-c(bootSd, thisSd)
}

hist(bootSd)

quantile(bootSd, c(0.025, 0.975))
#     2.5%     97.5% 
#   18.13907 111.30142 

# Tropical Flowers
# Different varieties of the tropical flower Heliconia are fertilized by different species of hummingbirds. 
# Over time, the lengths of the flowers and the forms of the hummingbirds’ beaks have evolved to match each other. 
# Data on the lengths in millimeters of two color varieties of the same species of flower on the island of Dominica can be found in the R script.

# H. caribaea RED
red<-c(42.90, 42.01, 41.93, 43.09, 41.47, 41.69, 39.78, 
       39.63, 42.18, 40.66, 37.87, 39.16, 37.40, 38.20,
       38.10, 37.97, 38.79, 38.23, 38.87, 37.78, 38.01)

# H. caribaea YELLOW
yellow<-c(36.78, 37.02, 36.52, 36.11, 36.03, 35.45, 38.13,
          37.1, 35.17, 36.82, 36.66, 35.68, 36.03, 34.57, 34.63)

qqnorm(yellow)
qqline(yellow)

qqnorm(red)
qqline(red)

##### A two sample t-test to compare the two means of the red and yellow Heliconia flowers
t.test(red, yellow)

#Welch Two Sample t-test

#data:  red and yellow
#t = 7.4254, df = 31.306, p-value = 2.171e-08
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  2.623335 4.609046
#sample estimates:
#  mean of x mean of y 
#39.79619  36.18000 


## Two sample bootstrap confidence interval for difference in means

# Function for bootstrap confidence interval
# for difference in two sample means
bootStrapCI<-function(red, yellow, nsim){
  
  n1<-length(red)
  n2<-length(yellow)
  
  bootCI2<-c()
  
  for(i in 1:nsim){
    bootSamp1<-sample(1:n1, n1, replace=TRUE)
    bootSamp2<-sample(1:n2, n2, replace=TRUE)
    thisXbar<-mean(red[bootSamp1])-mean(yellow[bootSamp2])
    bootCI2<-c(bootCI2, thisXbar)
  }
  
  return(bootCI2)
}

?sample

#Let’s compute the confidence intervals using the quantile and hybrid methods:
  
flowerBootCI<-bootStrapCI2(red, yellow, nsim=10000)


t.test(flowerBootCI)
#One Sample t-test

#data:  flowerBootCI
#t = 760.02, df = 9999, p-value < 2.2e-16
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  3.613696 3.632385
#sample estimates:
#  mean of x 
#3.62304 

# Quantile Method 
quantile(flowerBootCI, c(0.025, 0.975))
# 2.5%    97.5% 
# 2.702083 4.559831 


# Hybrid Method: Parametric critical value and bootstrap SE
bootSE<-sd(flowerBootCI)/sqrt(n)

(mean(red)-mean(yellow))+c(-1.96,1.96)*qt(0.95, df=14)*bootSE

#  3.204777 4.027604

#### Two Sample Boostrap Hypothesis Test
# Function for bootstrap hypothesis test 
# for difference in two sample means

bootStrapHyp2<-function(red, yellow, nsim, alternative){
  
  testStat<-mean(red)-mean(yellow)
  n1<-length(red)
  n2<-length(yellow)
  allDat<-c(red, yellow)

  bootNull<-c()
  
  for(i in 1:nsim){
    bootSamp<-sample(1:(n1+n2), (n1+n2), replace=TRUE)
    boot1<-bootSamp[1:n1]
    boot2<-bootSamp[(n1+1):(n1+n2)]
    thisXbar<-mean(allDat[boot1])-mean(allDat[boot2])
    bootNull<-c(bootNull, thisXbar)
  }
  
  if(alternative=="greater"){
    pval<-mean(bootNull>=testStat)
  }
  if(alternative=="less"){
    pval<-mean(bootNull<=testStat)
  }
  if(alternative=="not equal"){
    pval<-min(mean(bootNull>=testStat), mean(bootNull<=testStat))*2
  }
  return(pval)
}

# Applying this function to the red and yellow flowers
flowerBoot<-bootStrapHyp2(red, yellow, nsim=10000, alternative="not equal")
flowerBoot
# [1] 0

flowerBoot<-bootStrapHyp2(red, yellow, nsim=10000)
flowerBoot



##### Permutation Test for Difference in Means
##### Here is code to carrying out a permutation test.

# Note the difference between the bootstrap methods and the permutation methods.
# Function for permutation test 
# for difference in two sample means
permHyp2<-function(red, yellow, nsim, alternative){
  testStat<-mean(red)-mean(yellow)
  n1<-length(red)
  n2<-length(yellow)
  allDat<-c(red, yellow)
  
  permNull<-c()
  
  for(i in 1:nsim){
    permSamp<-sample(1:(n1+n2), n1, replace=FALSE)
    thisXbar<-mean(allDat[permSamp])-mean(allDat[-permSamp])
    permNull<-c(permNull, thisXbar)
  }
  
  if(alternative=="greater"){
    pval<-mean(permNull>=testStat)
  }
  if(alternative=="less"){
    pval<-mean(permNull<=testStat)
  }
  if(alternative=="not equal"){
    pval<-min(mean(permNull>=testStat), mean(permNull<=testStat))*2
  }
  return(pval)
}


#If we were to do all permutations for the Heliconia example that would be alot!
# Number of combinations needed for exact test

choose(36,21) #5567902560



flowerPerm<-permHyp2(red, yellow, nsim=100000, alternative="not equal")
flowerPerm # [1] 0

#Permutation method for t test 

permutationCI<-function(red, yellow, nsim){
  testStat<-mean(red)-mean(yellow)
  n1<-length(red)
  n2<-length(yellow)
  allDat<-c(red, yellow)
  
  permCI<-c()

  for(i in 1:nsim){
    permSamp<-sample(1:(n1+n2), n1, replace=FALSE)
    thisXbar<-mean(allDat[permSamp])-mean(allDat[-permSamp])
    permCI<-c(permCI, thisXbar)
  }

  return(permCI)

}

flowerpermt<-permutationCI(red, yellow, nsim = 10000)

quantile(flowerpermt, c(0.025, 0.975))
#   2.5%     97.5% 
#  -1.569038  1.591076 
t.test(flowerpermt)
#One Sample t-test

#data:  flowerpermt
#t = 0.64409, df = 9999, p-value = 0.5195
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  -0.01064783  0.02106970
#sample estimates:
#  mean of x 
#0.005210933 



















