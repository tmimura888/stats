ggplot(Boston, aes(x=lstat, y=medv, color=as.factor(part)))+
  geom_point()


ggplot(Boston, aes(x=lstat, y=medv, color=as.factor(part)))+
  geom_point()+
  facet_grid(.~part)

# TRAIN DATAFRAME
traindf<-Boston%>%
  filter(part==0)

testdf<-Boston%>%
  filter(part==1)

# TRAIN MODEL
trainLM<-lm(medv~lstat, traindf)
anova(trainLM)

# TEST MSE
testFit<-predict(trainLM, testdf)
testRSS<-sum((testdf$medv-testFit)^2)
testRSS


testMSE<-mean((testdf$medv-testFit)^2)
testMSE

# TEST DIFFERENT POLY DEGREES
train<-sample(506, 354)

degreePoly<-10
polyMSE<-matrix(nrow=degreePoly, ncol=2)
colnames(polyMSE)<-c("degree", "MSE")
for(i in 1:degreePoly){ 
  polyMSE[i,1]<-i
  this.fit<-lm(medv~poly(lstat,i), data=Boston, subset=train)
  polyMSE[i,2]<-mean((Boston$medv-predict(this.fit, Boston))[-train]^2)
}

polyDF<-as.data.frame(polyMSE)
head(polyDF)

ggplot(data=polyDF, aes(x=degree, y=MSE))+
  geom_point()+
  geom_line()

# MULTIPLE RANDOM SPLITS
degreePoly<-10
splits<-5

splitMat<-matrix(nrow=degreePoly*splits, ncol=3)
colnames(splitMat)<-c("run","MSE", "degree")

for(i in 1:splits){
  a=(i-1)*degreePoly+1
  b=i*degreePoly
  
  set.seed(i*10)
  splitMat[a:b,1]<-i
  train<-sample(506, 354)
  
  for(j in 1:degreePoly){ 
    c=a+(j-1)
    
    this.fit<-lm(medv~poly(lstat,j), data=Boston, subset=train)
    splitMat[c,2]<-mean((Boston$medv-predict(this.fit, Boston))[-train]^2)
    splitMat[c,3]<-j
  }
}
splitDF<-as.data.frame(splitMat)
head(splitDF)

ggplot(data=splitDF, aes(x=degree, y=MSE, color=as.factor(run)))+
  geom_point()+
  geom_line()
