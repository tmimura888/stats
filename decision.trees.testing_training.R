#Trees Based Models
#Regression Trees
#We will need several packages for today’s code:
  
  install.packages("ISLR")
  library(ISLR)

install.packages("tree")
library(tree)

install.packages("MASS")
library(MASS)
#Fitting a regression tree
# REGRESSION TREES  
set.seed(1)
train<-sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston<-tree(medv~., Boston, subset=train)
summary(tree.boston)

#Plotting a regression tree
# plot
plot(tree.boston)
text(tree.boston, pretty=0)




#Using cross-validation to select complexity
# cross-validation for complexity
cv.boston<-cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')

# favors the 7 node tree


#Pruning the tree
# pruning to 5 nodes
prune.boston<-prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

#Making predictions
# make predictions
yhat<-predict(tree.boston, newdata=Boston[-train,])
boston.test<-Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)

# mse
mean((yhat-boston.test)^2)

#Classification Trees
#We will be using the carseat data from the book and making a new variable to seperate high sales.

# CLASSIFICATION TREE
attach(Carseats)

# make new variable if Sales is greater than 8 
High<-ifelse(Sales<=8, "No", "Yes")

Carseats<-data.frame(Carseats, High)
attach(Carseats)

#Fitting a classification tree
# classification tree
tree.carseats<-tree(High~.-Sales, Carseats)
summary(tree.carseats)

#Plotting a classification tree
# plot
plot(tree.carseats)
text(tree.carseats, pretty=0)

#Displaying branches with text
# print the full tree with text
tree.carseats

#Testing and training
# test and train 
set.seed(2)
train<-sample(1:nrow(Carseats), 200)

carseats.test<-Carseats[-train]
dim(carseats.test)

high.test<-High[-train]
tree.carseats<-tree(High~.-Sales, Carseats, subset=train)
tree.pred.all<-predict(tree.carseats, newdata=Carseats, type="class")
length(tree.pred.all)

tree.pred<-tree.pred.all[-train]

# confusion matrix
cm<-table(tree.pred, high.test)
cm


# error
sum(diag(cm))/sum(cm)

#Cross-validation for pruning
# prune 
set.seed(3)
cv.carseats<-cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)

# size is number of terminal nodes
# dev is the error 
# k is the cost complexity parameter (alpha)
cv.carseats

# plot
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

# now prune based on desired complexity
par(mfrow=c(1,1))
prune.carseats<-prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

# prediction and confusion matrix
tree.pred2<-predict(prune.carseats, newdata = Carseats, type="class")
test.pred<-tree.pred2[-train]
cm<-table(test.pred, high.test)
cm

# error rate
sum(diag(cm))/sum(cm)
## [1] 0.775

#What if we do less pruning?
  # what if we do less pruning 
  prune.carseats<-prune.misclass(tree.carseats, best=15)
plot(prune.carseats)
text(prune.carseats, pretty=0)

# prediction and confusion matrix
tree.pred2<-predict(prune.carseats, newdata = Carseats, type="class")
test.pred<-tree.pred2[-train]
cm<-table(test.pred, high.test)
cm

# error rate
sum(diag(cm))/sum(cm)
detach(Carseats)

#Improvements on Trees: Bagging/Random Forests/Boosting
#Improvements on Trees
#Use the Boston data:
  
  install.packages("ISLR")
  library(ISLR)
library(MASS)
data(Boston)
Bagging
#The randomForest package can be used for bagging and boosting:
  
  # package to be used for random forests and bagging
  install.packages("randomForest")
  library(randomForest)
#Create the testing and training sets
set.seed(1)
train<-sample(1:dim(Boston)[1], floor(dim(Boston)[1]/2))
boston.test<-Boston[-train, "medv"]
#Fitting the BAG
# bagging --> m=p
# uses by default 500 trees
bag.boston<-randomForest(medv~., data=Boston, subset = train, 
                         mtry=13, importance=TRUE)
#Test Errror
# test error
yhat.bag<-predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)

mean((yhat.bag-boston.test)^2)

#Decrease Number of Trees
# decrease number of trees
# ntree = 25
bag.boston2<-randomForest(medv~., data=Boston, subset = train, 
                          mtry=13, ntree=25, importance=TRUE)

yhat.bag2<-predict(bag.boston2, newdata=Boston[-train,])
mean((yhat.bag2-boston.test)^2)

#Random Forest
# change number of predictors used in the random forest
# default is p/3
# mtry = 6
set.seed(1)
rf.boston<-randomForest(medv~., data=Boston, subset=train,
                        mtry=6, importance=TRUE)
yhat.rf<-predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

#Variable Importance
# variable importance
importance(rf.boston)

varImpPlot(rf.boston)



#Boosting
#You will need a new package:
  
  # BOOSTING
  install.packages("gbm")
  library(gbm)
#Fitting a Boosted Tree
set.seed(1)
boost.boston<-gbm(medv~., data=Boston[train, ], distribution="gaussian",
                  n.trees=5000, interaction.depth = 4)
#Variable Importance
# relative influence plot
summary(boost.boston)

#Relative Marginal Plots
# relative dependence plots
# marginal effect of the selected var
par(mfrow=c(1,2))
plot(boost.boston, i="rm")

plot(boost.boston, i="lstat")

#Test Error with Difference Lambda
# test error of boost
# (lambda) shrinkage = 0.001
yhat.boost<-predict(boost.boston, newdata=Boston[-train,],
                    n.trees = 5000)
mean((yhat.boost-boston.test)^2)

# change the learning parameter
# (lambda) shrinkage = 0.1
boost.boston2<-gbm(medv~., data=Boston[train, ], distribution="gaussian",
                   n.trees=5000, interaction.depth = 4, 
                   shrinkage=0.1, verbose = F)
yhat.boost2<-predict(boost.boston2, newdata=Boston[-train,],
                     n.trees = 5000)
mean((yhat.boost2-boston.test)^2)















