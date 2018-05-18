
##############################################################################################################################################
# Train & Test sets
##############################################################################################################################################

unique(MCPT$season)
train <- MCPT$season %in% c("2008/2009","2010/2011","2012/2013","2014/2015")
sum(train)
test <- MCPT$season %in% c("2009/2010","2011/2012","2013/2014","2015/2016")
sum(test)



##############################################################################################################################################
# Regression logistique standard (2 classes)
##############################################################################################################################################

# Model fit
mydata <-MCPT[,c(4,6:ncol(MCPT))]
logit2 <- glm(goal_diff_class2~., data=mydata,subset=train,family="binomial")
contrasts(mydata$goal_diff_class2)
logit2.sum <- summary(logit2)
logit2.sum$coefficients[,1:4]
sort(logit2.sum$coefficients[,3])

# Apprentissage
logit2.predict <- predict(logit2,type="response")
logit2.pred <-rep("1home_win", sum(train))
logit2.pred[logit2.predict >.5]<-"2home_draworloss"
table(logit2.pred,mydata$goal_diff_class2[train])
mean(logit2.pred==mydata$goal_diff_class2[train])

# Test
logit2.predict <- predict(logit2,newdata=mydata[test,],type="response")
logit2.pred <-rep("1home_win", sum(test))
logit2.pred[logit2.predict >.5]<- "2home_draworloss"
table(logit2.pred,mydata$goal_diff_class2[test])
mean(logit2.pred==mydata$goal_diff_class2[test])


##############################################################################################################################################
# Polynomial Ordered Logistic Regression : porl (3 classes)
##############################################################################################################################################

library(MASS)
mydata <-MCPT[,c(5:ncol(MCPT))]
logit3 <-polr(goal_diff_class3 ~ ., data=mydata,subset=train,Hess=TRUE)
contrasts(mydata$goal_diff_class3)
summary(logit3)
names(logit3)

# Apprentissage
logit3.pred <- predict(logit3,mydata[train,],type="class")
table(logit3.pred,mydata$goal_diff_class3[train])
mean(logit3.pred==mydata$goal_diff_class3[train])

# Test
logit3.pred <- predict(logit3,newdata=mydata[test,],type="class")
table(logit3.pred,mydata$goal_diff_class3[test])
mean(logit3.pred==mydata$goal_diff_class3[test])


##############################################################################################################################################
# Linear Discriminat Analysis (2 classes)
##############################################################################################################################################

# Model fit
library(MASS)
mydata <- MCPT[,c(4,6:ncol(MCPT))]
lda2 <- lda(goal_diff_class2~., data=mydata,subset=train)

# Apprentissage
lda2.predict <- predict(lda2,mydata)$class
table(lda2.predict[train],mydata$goal_diff_class2[train])
mean(lda2.predict[train]==mydata$goal_diff_class2[train])

# Test
table(lda2.predict[test],mydata$goal_diff_class2[test])
mean(lda2.predict[test]==mydata$goal_diff_class2[test])

##############################################################################################################################################
# Linear Discriminat Analysis (3 classes)
##############################################################################################################################################

# Model fit
library(MASS)
mydata <- MCPT[,c(5:ncol(MCPT))]
lda3 <- lda(goal_diff_class3~., data=mydata,subset=train)

# Apprentissage
lda3.predict <- predict(lda3,mydata)$class
table(lda3.predict[train],mydata$goal_diff_class3[train])
mean(lda3.predict[train]==mydata$goal_diff_class3[train])

# Test
table(lda3.predict[test],mydata$goal_diff_class3[test])
mean(lda3.predict[test]==mydata$goal_diff_class3[test])



##############################################################################################################################################
# Quadratic Discriminat Analysis (2 classes)
##############################################################################################################################################

# Model fit
library(MASS)
mydata <- MCPT[,c(4,6:ncol(MCPT))]
qda2 <- qda(goal_diff_class2~., data=mydata,subset=train)

# Apprentissage
qda2.predict <- predict(qda2,mydata)$class
table(qda2.predict[train],mydata$goal_diff_class2[train])
mean(qda2.predict[train]==mydata$goal_diff_class2[train])

# Test
table(qda2.predict[test],mydata$goal_diff_class2[test])
mean(qda2.predict[test]==mydata$goal_diff_class2[test])


##############################################################################################################################################
# Quadratic Discriminat Analysis (3 classes)
##############################################################################################################################################

# Model fit
library(MASS)
mydata <- MCPT[,c(5:ncol(MCPT))]
qda3 <- qda(goal_diff_class3~., data=mydata,subset=train)

# Apprentissage
qda3.predict <- predict(qda3,mydata)$class
table(qda3.predict[train],mydata$goal_diff_class3[train])
mean(qda3.predict[train]==mydata$goal_diff_class3[train])

# Test
table(qda3.predict[test],mydata$goal_diff_class3[test])
mean(qda3.predict[test]==mydata$goal_diff_class3[test])

##############################################################################################################################################
# K nearest neighbors (2 classes)
##############################################################################################################################################

# We tested different values for the parameter k for the train and test samples, here we report Hit rates
# k=3  : train HR =78% test HR =59%
# k=5  : train HR =73% test HR =60%
# k=10 : train HR =68% test HR =61%
# k=20 : train HR =66% test HR =62%
# k=30 : train HR =66% test HR =62%

library(class)
set.seed(1)

mydata <- MCPT[,c(4,6:ncol(MCPT))]
train.X <-mydata[train,-1]
test.X <-mydata[test,-1]
train.Y <-mydata[train,1]
test.Y <- mydata[test,1]

# Apprentissage
knn.predict = knn(train=train.X, test=train.X, cl=train.Y,k=30)
table(knn.predict,train.Y)
mean(knn.predict==train.Y)

# Test
knn.predict = knn(train=train.X, test=test.X, cl=train.Y,k=3)
table(knn.predict,test.Y)
mean(knn.predict==test.Y)

##############################################################################################################################################
# K nearest neighbors (3 classes)
##############################################################################################################################################

# We tested different values for the parameter k for the train and test samples, here we report Hit rates
# k=3  : train HR =% test HR =%
# k=5  : train HR =% test HR =%
# k=10 : train HR =% test HR =%
# k=20 : train HR =% test HR =%
# k=30 : train HR =% test HR =%

library(class)
set.seed(1)

mydata <- MCPT[,c(5:ncol(MCPT))]
train.X <-mydata[train,-1]
test.X <-mydata[test,-1]
train.Y <-mydata[train,1]
test.Y <- mydata[test,1]

# Apprentissage
knn.predict = knn(train=train.X, test=train.X, cl=train.Y,k=30)
table(knn.predict,train.Y)
mean(knn.predict==train.Y)

# Test
knn.predict = knn(train=train.X, test=test.X, cl=train.Y,k=3)
table(knn.predict,test.Y)
mean(knn.predict==test.Y)


##############################################################################################################################################
# Classification Tree(2 classes)
##############################################################################################################################################

library(tree)
mydata <- MCPT[,c(4,6:ncol(MCPT))]

# Apprentissage
tree.fit2 <-tree(goal_diff_class2~.,data=mydata,subset=train)
tree.predict <- predict(tree.fit2,mydata[train,],type="class")
summary(tree.fit2)
plot(tree.fit2)
text(tree.fit2,pretty=0)
table(tree.predict,mydata$goal_diff_class2[train])
mean(tree.predict==mydata$goal_diff_class2[train])

# Test
tree.predict <- predict(tree.fit2,mydata[test,],type="class")
table(tree.predict,mydata$goal_diff_class2[test])
mean(tree.predict==mydata$goal_diff_class2[test])


##############################################################################################################################################
# Classification Tree(3 classes)
##############################################################################################################################################

library(tree)
mydata <- MCPT[,c(5:ncol(MCPT))]

# Apprentissage
tree.fit3 <-tree(goal_diff_class3~.,data=mydata,subset=train)
tree.predict <- predict(tree.fit3,mydata[train,],type="class")
summary(tree.fit3)
plot(tree.fit3)
text(tree.fit3,pretty=0)
table(tree.predict,mydata$goal_diff_class3[train])
mean(tree.predict==mydata$goal_diff_class3[train])

# Test
tree.predict <- predict(tree.fit3,mydata[test,],type="class")
table(tree.predict,mydata$goal_diff_class3[test])
mean(tree.predict==mydata$goal_diff_class3[test])

##############################################################################################################################################
# bagging trees(2 classes) 
##############################################################################################################################################
# Bagging is simply a special case of random forest where mtry=p

library(randomForest)
mydata <- MCPT[,c(4,6:ncol(MCPT))]
set.seed(1)

# Apprentissage
Bag2 <-randomForest(goal_diff_class2~.,data=mydata,subset=train,mtry=100)
Bag2.predict <- predict(Bag2,data=mydata[train,],type="class")
table(Bag2.predict,mydata$goal_diff_class2[train])
mean(Bag2.predict==mydata$goal_diff_class2[train])

# Test
Bag2.predict <- predict(Bag2,newdata=mydata[test,],type="class")
table(Bag2.predict,mydata$goal_diff_class2[test])
mean(Bag2.predict==mydata$goal_diff_class2[test])


##############################################################################################################################################
# bagging trees(3 classes) 
##############################################################################################################################################
# Bagging is simply a special case of random forest where mtry=p

library(randomForest)
mydata <- MCPT[,c(5:ncol(MCPT))]
set.seed(1)

# Apprentissage
Bag3 <-randomForest(goal_diff_class3~.,data=mydata,subset=train,mtry=100)
Bag3.predict <- predict(Bag3,data=mydata[train,],type="class")
table(Bag3.predict,mydata$goal_diff_class3[train])
mean(Bag3.predict==mydata$goal_diff_class3[train])

# Test
Bag3.predict <- predict(Bag3,newdata=mydata[test,],type="class")
table(Bag3.predict,mydata$goal_diff_class3[test])
mean(Bag3.predict==mydata$goal_diff_class3[test])



##############################################################################################################################################
# Random Forest(2 classes)
##############################################################################################################################################
# parameter mtry = sqrt(p)=10

library(randomForest)
mydata <- MCPT[,c(4,6:ncol(MCPT))]
set.seed(1)

# Apprentissage
Forest2 <-randomForest(goal_diff_class2~.,data=mydata,subset=train,mtry=10)
Forest2.predict <- predict(Forest2,data=mydata[train,],type="class")
table(Forest2.predict,mydata$goal_diff_class2[train])
mean(Forest2.predict==mydata$goal_diff_class2[train])

# Test
Forest2.predict <- predict(Forest2,newdata=mydata[test,],type="class")
table(Forest2.predict,mydata$goal_diff_class2[test])
mean(Forest2.predict==mydata$goal_diff_class2[test])


##############################################################################################################################################
# Random Forest(3 classes)
##############################################################################################################################################
# parameter mtry = sqrt(p)=10

library(randomForest)
mydata <- MCPT[,c(5:ncol(MCPT))]
set.seed(1)

# Apprentissage
Forest3 <-randomForest(goal_diff_class3~.,data=mydata,subset=train,mtry=10)
Forest3.predict <- predict(Forest3,data=mydata[train,],type="class")
table(Forest3.predict,mydata$goal_diff_class3[train])
mean(Forest3.predict==mydata$goal_diff_class3[train])

# Test
Forest3.predict <- predict(Forest3,newdata=mydata[test,],type="class")
table(Forest3.predict,mydata$goal_diff_class3[test])
mean(Forest3.predict==mydata$goal_diff_class3[test])


##############################################################################################################################################
# SVM (2 classes)
##############################################################################################################################################

library(e1071)
mydata <- MCPT[,c(4,6:ncol(MCPT))]
set.seed(1)

# Apprentissage
SVM2 <-svm(goal_diff_class2~.,data=mydata,subset=train,kernel="linear",scale=TRUE,cost=1)
SVM2.predict <- predict(SVM2,data=mydata[train,],type="class")
table(SVM2.predict,mydata$goal_diff_class2[train])
mean(SVM2.predict==mydata$goal_diff_class2[train])

# Test
SVM2.predict <- predict(SVM2,newdata=mydata[test,],type="class")
table(SVM2.predict,mydata$goal_diff_class2[test])
mean(SVM2.predict==mydata$goal_diff_class2[test])


##############################################################################################################################################
# SVM (3 classes)
##############################################################################################################################################

library(e1071)
mydata <- MCPT[,c(5:ncol(MCPT))]
set.seed(1)

# Apprentissage
SVM3 <-svm(goal_diff_class3~.,data=mydata,subset=train,kernel="linear",scale=TRUE,cost=1)
SVM3.predict <- predict(SVM3,data=mydata[train,],type="class")
table(SVM3.predict,mydata$goal_diff_class3[train])
mean(SVM3.predict==mydata$goal_diff_class3[train])

# Test
SVM3.predict <- predict(SVM3,newdata=mydata[test,],type="class")
table(SVM3.predict,mydata$goal_diff_class3[test])
mean(SVM3.predict==mydata$goal_diff_class3[test])


##############################################################################################################################################
# Boosting : Adaboost (2 classes)
##############################################################################################################################################

library(adabag)
mydata <- MCPT[,c(4,6:ncol(MCPT))]
set.seed(1)

# Apprentissage
adaboost2 <-boosting(goal_diff_class2~.,data=mydata[train,],boos=TRUE,coeflearn='Breiman')
table(adaboost2$class,mydata$goal_diff_class2[train])
mean(adaboost2$class==mydata$goal_diff_class2[train])

# Test
adaboost2.predict <- predict(adaboost2,newdata=mydata[test,],type="class")
table(adaboost2.predict$class,mydata$goal_diff_class2[test])
mean(adaboost2.predict$class==mydata$goal_diff_class2[test])


##############################################################################################################################################
# Boosting : Adaboost (3 classes)
##############################################################################################################################################

library(adabag)
mydata <- MCPT[,c(5:ncol(MCPT))]
set.seed(1)

# Apprentissage
adaboost3 <-boosting(goal_diff_class3~.,data=mydata[train,],boos=TRUE,coeflearn='Breiman')
table(adaboost3$class,mydata$goal_diff_class3[train])
mean(adaboost3$class==mydata$goal_diff_class3[train])

# Test
adaboost3.predict <- predict(adaboost3,newdata=mydata[test,],type="class")
table(adaboost3.predict$class,mydata$goal_diff_class3[test])
mean(adaboost3.predict$class==mydata$goal_diff_class3[test])



































