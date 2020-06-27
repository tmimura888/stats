
library("tidyverse")
library("ggplot2")
library("dplyr")
library("magrittr")

#Basics of Logistic Regression 
# How was the data collected?
# 1, Choose a single email account
# 2, Save each email that comes in during a given time frame 
# 3, Create dummy variable for each text component of interest
# 4, Visually classify each as spam or no

email <- mutate(email, log_num_char = log(num_char))
email 
#linear
qplot(x = log_num_char, y = spam, data = email, 
      geom = "point", alpha = I(.1), ylab = "spam") +
  stat_smooth(method = "lm",
              se = FALSE)

# logistic
qplot(x = log_num_char, y = spam, data = email, 
      geom = "point", alpha = I(.1), ylab = "spam") +
  stat_smooth(method = "glm", method.args = list(family = "binomial"),
              se = FALSE)

m1 <- glm(spam ~ log(num_char), data = email, family = "binomial")
summary(m1)

#Interpreting Log. Reg.
#1, Each row of the summary output is still a H-test on that parameter being 0.
#2, A positive slope estimate indicates that there is a positive association.
#3, Each estimate is still conditional on the other variables held constant.

m2 <- glm(spam ~ log(num_char) + to_multiple + attach + dollar + inherit + 
            viagra, data = email, family = "binomial")
summary(m2)

#Comparing Models
#Confusion matrix

pred1 <- predict(m1, newdata = email, type = "response")

conf_mat1<-data.frame(spam=email$spam, predSpam=pred1>.5)%>%
  group_by(spam, predSpam)%>%
  summarise(n=n())

conf_mat1


pred2<-predict(m2, newdata = email, type = "response")

conf_mat2<-data.frame(spam=email$spam, predSpam=pred2>.5)%>%
  group_by(spam, predSpam)%>%
  summarise(n=n())

conf_mat2


#Test-train
#In the test-train paradigm, you balance descriptive power with predictive accuracy by separating your data set into:
  
  #Training set: used to fit your model
#Testing set: used to evaluate predictive accuracy
#Related to cross-validation…

#Dividing the data
set.seed(501)
train_indices <- sample(1:nrow(email), size = floor(nrow(email)/2))
train_data <- email %>%
  slice(train_indices)
test_data <- email %>%
  slice(-train_indices)

#Training
m1 <- glm(spam ~ log(num_char), data = train_data, family = "binomial")
m2 <- glm(spam ~ log(num_char) + to_multiple + attach + dollar + inherit + 
            viagra, data = train_data, family = "binomial")

#Testing
test1 <- predict(m1, newdata = test_data, type = "response")

test_mat1<-data.frame(spam=test_data$spam, predSpam=test1>.5)%>%
  group_by(spam, predSpam)%>%
  summarise(n=n())

test_mat1

test2<-predict(m2, newdata = test_data, type = "response")

test_mat2<-data.frame(spam=test_data$spam, predSpam=test2>.5)%>%
  group_by(spam, predSpam)%>%
  summarise(n=n())

test_mat2

#Extending the model
#A GLM consists of three things:
  
 # A linear predictor
#A distribution of the response
#A link function between the two
#MLR
#Normal distribution, identity link function

#Logisitic Regression
#Binomial distribution, logit link function

#Poisson Regression





#### Advanced  Logistic Regression Models 

install.packages("titanic", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(titanic)

# LINEAR MODEL
lm_t=lm(Survived ~ Fare, data=titanic_train)

ggplot(titanic_train, aes(x=Fare, y=Survived)) + geom_point() + 
  stat_smooth(method="lm", se=TRUE)


# LOGIT MODEL
logit_t=glm(formula=Survived ~ Pclass+Sex, family=binomial(link="logit"), data=titanic_train)

summary(logit_t) #see the regression results


ggplot(titanic_train, aes(x=Fare, y=Survived)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"(link="logit")), se=TRUE)

# PROBIT MODEL
probit_t=glm(formula=Survived ~ Fare, family=binomial(link="probit"), data=titanic_train)

summary(probit_t) # see the results

ggplot(titanic_train, aes(x=Fare, y=Survived)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"(link="probit")), se=TRUE)


# Part 2: Multinomial Logistic Regression

install.packages("nnet", repos = "http://cran.us.r-project.org")

install.packages("Ecdat", repos = "http://cran.us.r-project.org")

library(nnet)
library(Ecdat)

Y <- Yogurt
M <- multinom(choice ~., data = Y) #the model

summary(M)

exp(coef(M)) #log odds relative to baseline

head(fitted(M)) #probabilities of picking each category for each observation

Y$choice <- relevel(Y$choice, ref = "dannon") #set new baseline

# FULL MODEL
M <- multinom(choice ~., data = Y)

summary(M)

z <- summary(M)$coefficients/summary(M)$standard.errors #Wald test for p values
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

install.packages("stargazer")
library(stargazer) #a package for making the output into a nice table

# MODEL 1
M1 <- multinom(choice ~ price.yoplait + price.dannon + price.hiland + price.weight, data = Y) #price model

summary(M1)

z1 <- summary(M1)$coefficients/summary(M1)$standard.errors 
p1 <- (1 - pnorm(abs(z1), 0, 1)) * 2
p1

# MODEL 2
M2 <- multinom(choice ~ feat.yoplait + feat.dannon + feat.hiland + feat.weight, data = Y) #feature model

summary(M2)

z2 <- summary(M2)$coefficients/summary(M2)$standard.errors
p2 <- (1 - pnorm(abs(z2), 0, 1)) * 2
p2

# MODEL 3
M3 <- multinom(choice ~ feat.weight + price.yoplait + price.dannon + price.hiland, data = Y)

summary(M3)

z3 <- summary(M3)$coefficients/summary(M3)$standard.errors
p3 <- (1 - pnorm(abs(z3), 0, 1)) * 2
p3

stargazer(M3, type = "text", title = "M3")

#Model Section for MLB Salaries
#In this lesson we will be using the Hitters dataset from the ISLR package. These data reflect the salaries of baseball platers and various player metrics for the 1886 and 1987 seasons.

library(ISLR)
data("Hitters")
names(Hitters)

dim(Hitters)

#First we want to check for any missing data in your response, which is salary. Then we will remove the NAs. This process is called listwise deletion.

# How many data points are misisng for Salary?
sum(is.na(Hitters$Salary))

# We are going to have to remove the NAs
Hitters<-na.omit(Hitters)
dim(Hitters)

sum(is.na(Hitters$Salary))

#Part I: Classic Variable Section Approaches
#Forward Selection
#Backward Selection
#Mixed Selection
#Best Subset Selection
#In order to demonstrate how the algorithms work, we will first only consider a small subset of predictors.

#Candidate Predictors:
  
  #CRBI: Career runs batted in
#Hits: Number of hits in 1986
#Runs: Number of runs in 1986
#Forward Selection Algorithm
#Start with nothing
#Pick variable with lowest sum of squares residual (or p-value)
#Add variables until hit stopping rule
#Let’s do this with our three variables.

#STEP 1: Use simple linear regression to select one variable to enter the model.

# Forward Selection
mod1<-lm(Salary~CRBI, data=Hitters)
summary(mod1)

mod2<-lm(Salary~Hits, data=Hitters)
summary(mod2)

mod3<-lm(Salary~Runs, data=Hitters)
summary(mod3)

# First step: keep CRBI (smallest p-val)

#STEP 2: Look for a second variable to add to the model.
mod12<-lm(Salary~CRBI+Hits, data=Hitters)
summary(mod12)

mod13<-lm(Salary~CRBI+Runs, data=Hitters)
summary(mod13)

# Second step: keep Hits
#STEP 3: Look for a third variable, but ….OH NO! ITs not significant! So we will stop and only use the best model with two variables.

mod123<-lm(Salary~CRBI+Hits+Runs, data=Hitters)
summary(mod123)
# STOP! Because adding Hits is not significant


#Backward Selection Algorithm
#Start with saturated model
#Remove largest p-value
#Remove variables until hit stopping rule
#STEP 1: Start with a full model using all the variables. Take out any variables that are not significant.

# Backward
# Start with all vars and take away
mod123<-lm(Salary~CRBI+Hits+Runs, data=Hitters)
summary(mod123)
# First step: Remove Runs

#STEP 2: Continue taking out variables until the only variables that remain are significant. We can stop with a two variable model.

mod12<-lm(Salary~CRBI+Hits, data=Hitters)
summary(mod12)

# STOP! Now everything is significant 

#Mixed Selection
#Start with no variables
#Add variables with best fit
#Continue to add variables, but if p-values become larger as new variables are added remove that variable from the model
#Continue going back and forth until variable have sufficiently low p-value
#Ok, now for the shortcut!… We can use an R package to automate this
#The leaps package in R can used for variable selection. For some reason, there can be problems with installing this package, but if you specify the repository it will work! (See the example in my code)

install.packages("leaps", repo = 'https://mac.R-project.org')
library(leaps)

#This package will default to telling you the best models with one through eight variables included in the model. 
#However, if you want to know all of the different possibilities up to the full model with all the predictors, you can do that too using the nvmax argument.
#You must specify what type of selection you would like to do using the method argument.

# Forward 
regfit.fwd<-regsubsets(Salary~., data=Hitters,
                       method="forward")
summary(regfit.fwd)

# Backward 
regfit.bwd<-regsubsets(Salary~., data=Hitters, 
                       method="backward")
summary(regfit.bwd)



#Best Subset Selection Algorithm
#Fit all the models for the specified number of predictors
#Select the model that is best (p-val, R-squared adj, Cp, BIC, AIC,…)
# Best Subset 
regfit.full<-regsubsets(Salary~., data=Hitters)
summary(regfit.full)

regfit.full<-regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary<-summary(regfit.full)
reg.summary


#Part II: Model Comparison Metrics
#Now lets look at the variables that are contained in the regsubset object. We can learn a lot about different model comparison metrics.

#R-squared
#R-squared Adjusted
#BIC
#Mallows Cp
#Remember, the goal of these model comparison metrics is to find the “best” model that balances the model fit and simplicity/interpretability.

names(reg.summary)
head(reg.summary)
#R-squared
#Although the R-squared metric is useful in simple linear regression, it has some drawbacks for multiple linear regression. 
#R-squared monotonically increases as we add more and more variables into the model, which is not good because it does not balance for the increased complexity. 
#We want to maximize this value!

# R-squared (monotonically increasing)
reg.summary$rsq

rsq_df<-data.frame(rsq=reg.summary$rsq,
                   size=1:19)

ggplot(rsq_df, aes(x=size, y=rsq))+
  geom_point()+
  geom_line()+
  ggtitle("R-squared")

#R-squared Adjusted
#This is similar to R-squared but gives some penalty to the number of predictors, p. We want to maximize this value!

adjrsq_df<-data.frame(rsq=c(reg.summary$rsq,reg.summary$adjr2),
                      type=c(rep("rsq",19),rep("rsq_adj",19)),
                      size=rep(1:19,2))

ggplot(adjrsq_df, aes(x=size, y=rsq, color=type))+
  geom_point()+
  geom_line()+
  ggtitle("R-squared vs Adj R-squared")

#Mallows Cp and BIC
#Unlike the R-squared values, we want to minimize these metrics:

#Note that BIC gives a harsher penalty and thereby picks smaller models.
# Lets look at the other metrics
metric_df<-data.frame(metric=c(reg.summary$rsq,
                               reg.summary$adjr2,
                               reg.summary$cp, 
                               reg.summary$bic),
                      type=c(rep("rsq",19),
                             rep("rsq_adj",19),
                             rep("cp",19),
                             rep("bic",19)),
                      size=rep(1:19,4))

# Add vertical lines to show the best model
which.min(reg.summary$bic)

which.max(reg.summary$rsq)

which.max(reg.summary$adjr2)

vline.dat <- data.frame(type=levels(metric_df$type), vl=c(6, 10,19, 11 ))


ggplot(metric_df, aes(x=size, y=metric, color=type))+
  geom_point()+
  geom_line()+
  ggtitle("Model Comparison Metrics")+
  geom_vline(aes(xintercept=vl), data=vline.dat, lty=2) +
  facet_grid(type~., scales = "free_y")




####Critical Thinking: MLR Model Extensions
#Part I: Numeric Interactions
#In the past, we have looked at the Boston dataset to explore polynomial regression. We are going to use this dataset for Part I. You will need to load it into this lab session from library MASS.

library(MASS)
library(ISLR)

names(Boston)

head(Boston)

#The model we created in Week 6 looked at median value as a function of lower status and proportion of older homes. 
#When we fit a model with just the additive effects of lstat and age, these are known as the main effects.

modInt0<-lm(medv~lstat+age, data=Boston)
summary(modInt0)

#However, there could be a relationship between lstat and age. We will now explore the their interaction.

#A colon can be use to create a model with only that specified interaction An astrix can be used to create a model with all levels of interactions and main effects


#Exercise 1: Interaction
#First, create a model with only the lstat and age interaction.

modInt1<-lm(medv~lstat:age, data=Boston)
summary(modInt1)

#Exercise 2: Full Model
#Now create a model with all possible interactions between lstat and age.

modInt2<-lm(medv~lstat*age, data=Boston)
summary(modInt2)


#Part II: Multicollinearity
#Lets change datasets again! We’ll be using the Credit data set in the ISLR package. 
#This is another simulated dataset about 10000 credit card customers. 
#For this exercise we’re only interested in four variables: balance, age, limit, and rating.

data(Credit)
names(Credit)

creditTrim<-data.frame(balance=Credit$Balance, 
                       limit=Credit$Limit, 
                       rating=Credit$Rating, 
                       age=Credit$Age)

#Multicollinearity occurs when there is are linear relationships between explanatory variables. So lets looks at a pairs plot and also the correlation matrix.

pairs(creditTrim)

cor(creditTrim)


#This causes our variance to be overestimated thereby decreasing the respective test statistic and causing larger p-values.

#Consider the two models :
  
credMod1<-lm(balance~age+limit, creditTrim)
summary(credMod1)

credMod2<-lm(balance~rating+limit, creditTrim)
summary(credMod2)

#A way to quantify the impact of multicollinearity is to look at the VIF (variance inflation factor). 
#A VIF close to 1 would imply no correlation. The larger the VIF the larger the amount of multicollinearity.

install.packages("DAAG")
library(DAAG)

vif(lm(balance~age+rating+limit, creditTrim))

#KNN Exercise and Lab
#In this exercise we will explore how to implement the KNN algorithm for classification.

#Application #1: Stock Market Data
#his data set consists of percentage returns for the S&P 500 stock index over 1, 250 days, from the beginning of 2001 until the end of 2005. 
#For each date, we have recorded the percentage returns for each of the five previous trading days, Lag1 through Lag5. 
#We have also recorded Volume (the number of shares traded on the previous day, in billions), 
#Today (the percentage return on the date in question) and Direction (whether the market was Up or Down on this date).

library(ISLR)
names(Smarket)

attach(Smarket)

install.packages(class)
library(class)

#Seperate out the 2005 data, which was the most current in the dataset. 
#This is very true to how forecasting would be done in a business setting because we have the past data to forecast the future.

# YEAR 2005
train=(Year <2005)
Smarket.2005= Smarket [! train ,]
dim(Smarket.2005)

Direction.2005=Direction[!train]

# Creating a variable for if the data is from the training set or testing set
Smarket$Year05<-"No"
Smarket$Year05[which(Smarket$Year==2005)]<-"Yes"

#We only want to predict the stock market Direction based on Lag1 and Lag2. Lets create a plot to visualize this! 
# Remember we want to only consider the closest neighbors (this uses Euclidean distance!)

library(tidyverse)
ggplot(Smarket, aes(Lag1, Lag2, color=Direction, pch=Year05))+
  geom_jitter(alpha=0.5)+
  theme_bw()

train.X=cbind(Lag1 ,Lag2)[train ,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction = Direction[train]


#Implement the KNN algorithm!
  
  #Model for K=1
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction ,k=1)

#Confusion matrix
table(knn.pred,Direction.2005)
##         Direction.2005
## knn.pred Down Up
##     Down   43 58
##     Up     68 83


#Model for K=3
knn.pred=knn(train.X,test.X,train.Direction ,k=3)

#Confusion matrix
table(knn.pred,Direction.2005)


mean(knn.pred==Direction.2005)

detach(Smarket)


#Application #2: Predicting Insurance Sales
#Caravan data set, which is part of the ISLR library. 
#This data set includes 85 predictors that measure demographic characteristics for 5,822 individuals. 
#The response variable is Purchase, which indicates whether or not a given individual purchases a caravan insurance policy. 
#In this data set, only 6% of people purchased caravan insurance.

### Application: Caravan Insurance
library(ISLR)
data("Caravan")
dim(Caravan)


attach(Caravan)
summary(Purchase)

# Percent that purchased 
348/dim(Caravan)[1]

#Since the KNN algorithm uses a distance metric, its VERY important to standardize the variables first.
#For instance, consider the variables salary and age. 
#Those are in completely different scale! 
#The variability of salary is much more than age and the importance of the age variable could get lost!
#So first, we should start with standardizing the variables.

# KNN uses a distance metric 
# so standardizing the units is VERY important!
# consider salary and age

# take out 86th col b/c Purchase variable
standardized.X=scale(Caravan [,-86])

# unstandardized 
var(Caravan[ ,1])

var(Caravan[ ,2])

# standardized 
var(standardized.X[ ,1])

var(standardized.X[ ,2])

#Since we want to predict the insurance sales, let’s train and then test the model.

# Split data into test and train
test=1:1000
train.X=standardized.X[-test ,]
test.X=standardized.X[test ,]
train.Y=Purchase[-test]
test.Y=Purchase[test]

#Model for K=1
# K=1
set.seed (1)
knn.pred1=knn(train.X,test.X,train.Y,k=1)

#Error rate 
mean(test.Y!=knn.pred1)

mean(test.Y!="No")

# confusion matrix
table(knn.pred1, test.Y)

# Rate correct.. better than random guess
9/(68+9)


#Model for K=3
# K=3
knn.pred3=knn(train.X,test.X,train.Y,k=3)
table(knn.pred3, test.Y)

5/26
#Model for K=5
# K=5
knn.pred3=knn(train.X,test.X,train.Y,k=5)
table(knn.pred3, test.Y)

4/15










