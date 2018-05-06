
library(doParallel)
library(parallel)
detectCores()
cl <-makeCluster(3)
registerDoParallel(cl)


##############################################################################################################################################
# Train & Test sets
##############################################################################################################################################

unique(MCPT$season)
train <- MCPT$season %in% c("2008/2009","2009/2010","2010/2011","2011/2012","2012/2013","2013/2014")
sum(train)
test <- MCPT$season %in% c("2014/2015","2015/2016")
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
summary(mydata[train,1])

# Apprentissage
logit2.predprob <- predict(logit2,type="response")
logit2.predclass <-rep("1home_draworloss", sum(train))
logit2.predclass[logit2.predprob >0.5]<-"2home_win"
table(logit2.predclass,mydata$goal_diff_class2[train],dnn=c("prediction","reality"))
mean(logit2.predclass==mydata$goal_diff_class2[train])

# Test
logit2.predict <- predict(logit2,newdata=mydata[test,],type="response")
logit2.pred <-rep("1home_draworloss", sum(test))
logit2.pred[logit2.predict >.5]<- "2home_win"
table(logit2.pred,mydata$goal_diff_class2[test],dnn=c("prediction","reality"))
mean(logit2.pred==mydata$goal_diff_class2[test])

# ROC curve
library(pROC)
logit2.roc <- roc(mydata$goal_diff_class2[test], logit2.predict)
plot(logit2.roc,col="blue", xlim=c(1,0),lwd=2,main="Regression logistique (ROC)")
logit2.roc$auc


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
table(logit3.pred,mydata$goal_diff_class3[train],dnn=c("prediction","reality"))
mean(logit3.pred==mydata$goal_diff_class3[train])

# Test
logit3.pred <- predict(logit3,newdata=mydata[test,],type="class")
table(logit3.pred,mydata$goal_diff_class3[test],dnn=c("prediction","reality"))
mean(logit3.pred==mydata$goal_diff_class3[test])

##############################################################################################################################################
# Regression logistique avec regularisation LASSO (2 classes)
##############################################################################################################################################

library(glmnet)
set.seed(1)
mydata <-MCPT[,c(4,6:ncol(MCPT))]
x <- as.matrix(mydata[train,-1])
y <- mydata[train,1]

# Régression logistique avec régulariusation de type lasso(alpha=1). Le paramètre de régularisation est obtenu par validation croisée
cvglmnet2 <- cv.glmnet(x,y,family="binomial",alpha=1)
plot(cvglmnet2)
cvglmnet2$lambda.min
coef(cvglmnet2, s = "lambda.min")

# Apprentissage
cvglmnet2.predclass <- predict(cvglmnet2, newx = as.matrix(mydata[train,-1]), type = "class", s = "lambda.min")
cvglmnet2.predprob <- predict(cvglmnet2, newx = as.matrix(mydata[train,-1]), type = "response", s = "lambda.min")
table(cvglmnet2.predclass,mydata$goal_diff_class2[train],dnn=c("prediction","reality"))
mean(cvglmnet2.predclass==mydata$goal_diff_class2[train])

# Test
cvglmnet2.predclass <- predict(cvglmnet2, newx = as.matrix(mydata[test,-1]), type = "class", s = "lambda.min")
cvglmnet2.predprob <- predict(cvglmnet2, newx = as.matrix(mydata[test,-1]), type = "response", s = "lambda.min")
table(cvglmnet2.predclass,mydata$goal_diff_class2[test],dnn=c("prediction","reality"))
mean(cvglmnet2.predclass==mydata$goal_diff_class2[test])

# ROC curve
library(pROC)
cvglmnet2.roc <- roc(mydata$goal_diff_class2[test], as.vector(cvglmnet2.predprob))
plot(cvglmnet2.roc,col="blue", lwd=2, xlim=c(1,0),main="Regression logistique LASSO (ROC)")
cvglmnet2.roc$auc


##############################################################################################################################################
# Regression logistique avec regularisation LASSO (3 classes)
##############################################################################################################################################


library(glmnet)
set.seed(1)
mydata <-MCPT[,c(5:ncol(MCPT))]
x <- as.matrix(mydata[train,-1])
y <- mydata[train,1]

# Régression logistique avec régulariusation de type lasso(alpha=1). Le paramètre de régularisation est obtenu par validation croisée
cvglmnet3 <- cv.glmnet(x,y,family="multinomial",type.multinomial = "grouped",alpha=1)
plot(cvglmnet3)
cvglmnet3$lambda.min
coef(cvglmnet3, s = "lambda.min")

# Apprentissage
cvglmnet3.pred <- predict(cvglmnet3, newx = as.matrix(mydata[train,-1]), type = "class", s = "lambda.min")
table(cvglmnet3.pred,mydata$goal_diff_class3[train],dnn=c("prediction","reality"))
mean(cvglmnet3.pred==mydata$goal_diff_class3[train])

# Test
cvglmnet3.pred <- predict(cvglmnet3, newx = as.matrix(mydata[test,-1]), type = "class", s = "lambda.min")
table(cvglmnet3.pred,mydata$goal_diff_class3[test],dnn=c("prediction","reality"))
mean(cvglmnet3.pred==mydata$goal_diff_class3[test])

##############################################################################################################################################
# Linear Discriminat Analysis (2 classes)
##############################################################################################################################################

# Model fit
library(MASS)
mydata <- MCPT[,c(4,6:ncol(MCPT))]
lda2 <- lda(goal_diff_class2~., data=mydata,subset=train)

# Apprentissage
lda2.predclass <- predict(lda2,mydata[train,])$class
lda2.predprob <- predict(lda2,mydata[train,])$posterior
table(lda2.predclass,mydata$goal_diff_class2[train],dnn=c("prediction","reality"))
mean(lda2.predclass==mydata$goal_diff_class2[train])

# Test
lda2.predclass <- predict(lda2,mydata[test,])$class
lda2.predprob <- predict(lda2,mydata[test,])$posterior
table(lda2.predclass,mydata$goal_diff_class2[test],dnn=c("prediction","reality"))
mean(lda2.predclass==mydata$goal_diff_class2[test])

# # ROC curve
library(pROC)
lda2.roc <- roc(mydata$goal_diff_class2[test], as.vector(lda2.predprob[,1]))
plot(lda2.roc,col="blue", lwd=2, xlim=c(1,0),main="Linear Discriminant Analysis (ROC)")
lda2.roc$auc



##############################################################################################################################################
# Linear Discriminat Analysis (3 classes)
##############################################################################################################################################

# Model fit
library(MASS)
mydata <- MCPT[,c(5:ncol(MCPT))]
lda3 <- lda(goal_diff_class3~., data=mydata,subset=train)

# Apprentissage
lda3.predclass <- predict(lda3,mydata[train,])$class
lda3.predprob <- predict(lda3,mydata[train,])$posterior
table(lda3.predclass,mydata$goal_diff_class3[train])
mean(lda3.predclass==mydata$goal_diff_class3[train])

# Test
lda3.predclass <- predict(lda3,mydata[test,])$class
lda3.predprob <- predict(lda3,mydata[test,])$posterior
table(lda3.predclass,mydata$goal_diff_class3[test])
mean(lda3.predclass==mydata$goal_diff_class3[test])


##############################################################################################################################################
# Quadratic Discriminant Analysis (2 classes)
##############################################################################################################################################

# Model fit
library(MASS)
mydata <- MCPT[,c(4,6:ncol(MCPT))]
qda2 <- qda(goal_diff_class2~., data=mydata,subset=train)

# Apprentissage
qda2.predclass <- predict(qda2,mydata[train,])$class
qda2.predprob <- predict(qda2,mydata[train,])$posterior
table(qda2.predclass,mydata$goal_diff_class2[train],dnn=c("prediction","reality"))
mean(qda2.predclass==mydata$goal_diff_class2[train])

# Test
qda2.predclass <- predict(qda2,mydata[test,])$class
qda2.predprob <- predict(qda2,mydata[test,])$posterior
table(qda2.predclass,mydata$goal_diff_class2[test],dnn=c("prediction","reality"))
mean(qda2.predclass==mydata$goal_diff_class2[test])

# # ROC curve
library(pROC)
qda2.roc <- roc(mydata$goal_diff_class2[test], as.vector(qda2.predprob[,1]))
plot(qda2.roc,col="blue", lwd=2, xlim=c(1,0),main="Linear Discriminant Analysis (ROC)")
qda2.roc$auc


##############################################################################################################################################
# Quadratic Discriminant Analysis (3 classes)
##############################################################################################################################################

# Model fit
library(MASS)
mydata <- MCPT[,c(5:ncol(MCPT))]
qda3 <- qda(goal_diff_class3~., data=mydata,subset=train)

# Apprentissage
qda3.pred <- predict(qda3,mydata[train,])$class
qda3.predict <- predict(qda3,mydata[train,])$posterior
table(qda3.pred,mydata$goal_diff_class3[train])
mean(qda3.pred==mydata$goal_diff_class3[train])

# Test
qda3.pred <- predict(qda3,mydata[test,])$class
qda3.predict <- predict(qda3,mydata[test,])$posterior
table(qda3.pred,mydata$goal_diff_class3[test])
mean(qda3.pred==mydata$goal_diff_class3[test])



##############################################################################################################################################
# K nearest neighbors (2 classes)
##############################################################################################################################################

# We tested different values for the parameter k for the train and test samples, here we report Hit rates
# k=3   : train HR =78.2% test HR =59.6%
# k=5   : train HR =73.6% test HR =60.6%
# k=10  : train HR =69.5% test HR =61.9%
# k=20  : train HR =67.1% test HR =63.0%
# k=50  : train HR =66.0% test HR =63.9%
# k=100 : train HR =65.3% test HR =64.4%
# k=200 : train HR =65.0% test HR =65.7%


library(class)

# variables X standardisees pour eviter les problemes d'echelle
library(class)
mydata.Y <-  MCPT[,4]
mydata.X <-  scale(MCPT[,c(6:ncol(MCPT))])
train.X <- mydata.X[train,]
test.X <-  mydata.X[test,]
train.Y <- mydata.Y[train]
test.Y <-  mydata.Y[test]


# Apprentissage
set.seed(1)
knn2.predict = knn(train=train.X, test=train.X, cl=train.Y,k=100, prob=TRUE,use.all=TRUE)
table(knn2.predict,train.Y,dnn=c("prediction","reality"))
mean(knn2.predict==train.Y)

# Test
knn2.predict = knn(train=train.X, test=test.X, cl=train.Y,k=100, prob=TRUE,use.all=TRUE)
table(knn2.predict,test.Y,dnn=c("prediction","reality"))
mean(knn2.predict==test.Y)

# ROC curve
library(pROC)
knn2.roc <- roc(test.Y, attr(knn2.predict,"prob"))
plot(knn2.roc,col="blue", lwd=2, xlim=c(1,0),main = "K plus proches voisins (ROC)")
knn2.roc$auc


rm(mydata.Y, mydata.X,train.X, train.Y, test.X, test.Y)

##############################################################################################################################################
# K nearest neighbors (3 classes)
##############################################################################################################################################

# We tested different values for the parameter k for the train and test samples, here we report Hit rates
# k=3   : train HR =67.69% test HR =44.09%
# k=5   : train HR =61.95% test HR =45.71%
# k=10  : train HR =58.25% test HR =47.95%
# k=20  : train HR =55.52% test HR =48.64%
# k=50  : train HR =54.35% test HR =50.50%
# k=100 : train HR =53.98% test HR =51.49%
# k=200 : train HR =53.40% test HR =51.72%

library(class)

mydata.Y <-  MCPT[,5]
mydata.X <-  scale(MCPT[,c(6:ncol(MCPT))])
train.X <- mydata.X[train,]
test.X <-  mydata.X[test,]
train.Y <- mydata.Y[train]
test.Y <-  mydata.Y[test]

# Apprentissage
set.seed(1)
knn3.predict = knn(train=train.X, test=train.X, cl=train.Y,k=100, prob=TRUE,use.all=TRUE)
table(knn3.predict,train.Y)
mean(knn3.predict==train.Y)

# Test
knn3.predict = knn(train=train.X, test=test.X, cl=train.Y,k=100, prob=TRUE,use.all=TRUE)
table(knn3.predict,test.Y)
mean(knn3.predict==test.Y)

rm(mydata.Y, mydata.X, train.X, train.Y, test.X, test.Y)



##############################################################################################################################################
# Classification Tree(2 classes)
##############################################################################################################################################
# La variable 'B365H' a un impact dominant 

# Model fit
library(tree)
mydata <- MCPT[,c(4,6:ncol(MCPT))]
tree2.fit <-tree(goal_diff_class2~.,data=mydata,subset=train,split=c("deviance","gini"))
summary(tree2.fit)
plot(tree2.fit)
text(tree2.fit,pretty=0)

# Apprentissage
tree2.predclass <- predict(tree2.fit,mydata[train,],type="class")
tree2.predprob <- predict(tree2.fit,mydata[train,],type="vector")
table(tree2.predclass,mydata$goal_diff_class2[train],dnn=c("prediction","reality"))
mean(tree2.predclass==mydata$goal_diff_class2[train])

# Test
tree2.predclass <- predict(tree2.fit,mydata[test,],type="class")
tree2.predprob <- predict(tree2.fit,mydata[test,],type="vector")
table(tree2.predclass,mydata$goal_diff_class2[test],dnn=c("prediction","reality"))
mean(tree2.predclass==mydata$goal_diff_class2[test])

# ROC curve
library(pROC)
tree2.roc <- roc(mydata$goal_diff_class2[test], as.vector(tree2.predprob[,2]))
plot(tree2.roc,col="blue", lwd=2, xlim=c(1,0),main="Classification tree (ROC)")
tree2.roc$auc

##############################################################################################################################################
# Classification Tree(3 classes)
##############################################################################################################################################
# La variable 'B365A' a un impact dominant 

# Model fit
library(tree)
mydata <- MCPT[,c(5:ncol(MCPT))]
tree3.fit <-tree(goal_diff_class3~.,data=mydata,subset=train,split=c("deviance","gini"))
summary(tree3.fit)
plot(tree3.fit)
text(tree3.fit,pretty=0)

# Apprentissage
tree3.predclass <- predict(tree3.fit,mydata[train,],type="class")
tree3.predprob <- predict(tree3.fit,mydata[train,],type="vector")
table(tree3.predclass,mydata$goal_diff_class3[train],dnn=c("prediction","reality"))
mean(tree3.predclass==mydata$goal_diff_class3[train])

# Test
tree3.predclass <- predict(tree3.fit,mydata[test,],type="class")
tree3.predprob <- predict(tree3.fit,mydata[test,],type="vector")
table(tree3.predclass,mydata$goal_diff_class3[test],dnn=c("prediction","reality"))
mean(tree3.predclass==mydata$goal_diff_class3[test])



##############################################################################################################################################
# bagging trees(2 classes) 
##############################################################################################################################################
# Bagging is simply a special case of random forest where mtry=p

# Model fit
library(randomForest)
mydata <- MCPT[,c(4,6:ncol(MCPT))]
set.seed(1)
Bag2 <-randomForest(goal_diff_class2~.,data=mydata,subset=train,mtry=ncol(mydata)-1)
# Importance des variables de classification
Bag2$importance[order(Bag2$importance,decreasing=TRUE),]

# Apprentissage
Bag2.predclass <- predict(Bag2,data=mydata[train,],type="response")
Bag2.predprob <- predict(Bag2,data=mydata[train,],type="prob")
table(Bag2.predclass,mydata$goal_diff_class2[train],dnn=c("prediction","reality"))
mean(Bag2.predclass==mydata$goal_diff_class2[train])

# Test
Bag2.predclass <- predict(Bag2,newdata=mydata[test,],type="response")
Bag2.predprob <- predict(Bag2,newdata=mydata[test,],type="prob")
table(Bag2.predclass,mydata$goal_diff_class2[test],dnn=c("prediction","reality"))
mean(Bag2.predclass==mydata$goal_diff_class2[test])

# ROC curve
library(pROC)
Bag2.roc <- roc(mydata$goal_diff_class2[test], Bag2.predprob[,2])
plot(Bag2.roc,col="blue", lwd=2, xlim=c(1,0),main="Bagging of trees (ROC)")
Bag2.roc$auc


##############################################################################################################################################
# bagging trees(3 classes) 
##############################################################################################################################################
# Bagging is simply a special case of random forest where mtry=p

library(randomForest)
mydata <- MCPT[,c(5:ncol(MCPT))]
set.seed(1)
Bag3 <-randomForest(goal_diff_class3~.,data=mydata,subset=train,mtry=ncol(mydata)-1)
# Importance des variables de classification
Bag3$importance[order(Bag3$importance,decreasing=TRUE),]

# Apprentissage
Bag3.predclass <- predict(Bag3,data=mydata[train,],type="response")
Bag3.predprob <- predict(Bag3,data=mydata[train,],type="prob")
table(Bag3.predclass,mydata$goal_diff_class3[train],dnn=c("prediction","reality"))
mean(Bag3.predclass==mydata$goal_diff_class3[train])

# Test
Bag3.predclass <- predict(Bag3,newdata=mydata[test,],type="response")
Bag3.predprob <- predict(Bag3,newdata=mydata[test,],type="prob")
table(Bag3.predclass,mydata$goal_diff_class3[test],dnn=c("prediction","reality"))
mean(Bag3.predclass==mydata$goal_diff_class3[test])



##############################################################################################################################################
# Random Forest(2 classes)
##############################################################################################################################################
# parameter mtry = sqrt(p)=10

# Model fit
library(randomForest)
mydata <- MCPT[,c(4,6:ncol(MCPT))]
set.seed(1)
Forest2 <-randomForest(goal_diff_class2~.,data=mydata,subset=train,mtry=10)
# Importance des variables de classification
Forest2$importance[order(Forest2$importance,decreasing=TRUE),]

# Apprentissage
Forest2.predclass <- predict(Forest2,data=mydata[train,],type="response")
Forest2.predprob <- predict(Forest2,data=mydata[train,],type="prob")
table(Forest2.predclass,mydata$goal_diff_class2[train],dnn=c("prediction","reality"))
mean(Forest2.predclass==mydata$goal_diff_class2[train])

# Test
Forest2.predclass <- predict(Forest2,newdata=mydata[test,],type="response")
Forest2.predprob <- predict(Forest2,newdata=mydata[test,],type="prob")
table(Forest2.predclass,mydata$goal_diff_class2[test],dnn=c("prediction","reality"))
mean(Forest2.predclass==mydata$goal_diff_class2[test])

# ROC curve
library(pROC)
Forest2.roc <- roc(mydata$goal_diff_class2[test], Forest2.predprob[,2])
plot(Forest2.roc,col="blue", lwd=2, xlim=c(1,0),main="Random forest (ROC)")
Forest2.roc$auc

##############################################################################################################################################
# Random Forest(3 classes)
##############################################################################################################################################
# parameter mtry = sqrt(p)=10

# Model fit
library(randomForest)
mydata <- MCPT[,c(5:ncol(MCPT))]
set.seed(1)
Forest3 <-randomForest(goal_diff_class3~.,data=mydata,subset=train,mtry=10)
# Importance des variables de classification
Forest3$importance[order(Forest3$importance,decreasing=TRUE),]

# Apprentissage
Forest3.predclass<- predict(Forest3,data=mydata[train,],type="response")
Forest3.predprob<- predict(Forest3,data=mydata[train,],type="prob")
table(Forest3.predclass,mydata$goal_diff_class3[train],dnn=c("prediction","reality"))
mean(Forest3.predclass==mydata$goal_diff_class3[train])

# Test
Forest3.predclass <- predict(Forest3,newdata=mydata[test,],type="response")
Forest3.predprob <- predict(Forest3,newdata=mydata[test,],type="prob")
table(Forest3.predclass,mydata$goal_diff_class3[test],dnn=c("prediction","reality"))
mean(Forest3.predclass==mydata$goal_diff_class3[test])


##############################################################################################################################################
# SVM (2 classes)
##############################################################################################################################################


# Model fit
library(e1071)
mydata <- MCPT[,c(4,6:ncol(MCPT))] 
set.seed(1)
SVM2 <-svm(goal_diff_class2~.,data=mydata,subset=train,kernel="linear",scale=TRUE,cost=0.001,decision.values=TRUE)
summary(SVM2)

# Apprentissage
SVM2.pred <- predict(SVM2,newdata=mydata[train,],decision.values = TRUE)
table(SVM2.pred,mydata$goal_diff_class2[train],dnn=c("prediction","reality"))
mean(SVM2.pred==mydata$goal_diff_class2[train])

# Test
SVM2.pred <- predict(SVM2,newdata=mydata[test,],decision.values = TRUE)
table(SVM2.pred,mydata$goal_diff_class2[test],dnn=c("prediction","reality"))
mean(SVM2.pred==mydata$goal_diff_class2[test])

library(pROC)
SVM2.roc <- roc(mydata$goal_diff_class2[test], as.vector(attr(SVM2.pred,"decision.values")))
plot(SVM2.roc,col="blue", lwd=2, xlim=c(1,0),main="Support Vector Machine (ROC)")
SVM2.roc$auc


##############################################################################################################################################
# SVM (3 classes)
##############################################################################################################################################



library(e1071)
mydata <- MCPT[,c(5:ncol(MCPT))]
set.seed(1)
SVM3 <-svm(goal_diff_class3~.,data=mydata,subset=train,kernel="linear",scale=TRUE,cost=1,decision.values = TRUE)

# Apprentissage
SVM3.pred <- predict(SVM3,newdata=mydata[train,],decision.values = TRUE)
table(SVM3.pred,mydata$goal_diff_class3[train],dnn=c("prediction","reality"))
mean(SVM3.pred==mydata$goal_diff_class3[train])

# Test
SVM3.pred <- predict(SVM3,newdata=mydata[test,],decision.values = TRUE)
table(SVM3.pred,mydata$goal_diff_class3[test],dnn=c("prediction","reality"))
mean(SVM3.pred==mydata$goal_diff_class3[test])



##############################################################################################################################################
# Boosting : Adaboost (2 classes)
##############################################################################################################################################

library(adabag)
mydata <- MCPT[,c(4,6:ncol(MCPT))]
set.seed(1)
adaboost2 <-boosting(goal_diff_class2~.,data=mydata[train,],boos=TRUE,coeflearn='Breiman')

# Apprentissage

adaboost2.pred <- predict(adaboost2,newdata=mydata[train,])
table(adaboost2.pred$class,mydata$goal_diff_class2[train],dnn=c("prediction","reality"))
mean(adaboost2.pred$class==mydata$goal_diff_class2[train])

# Test
adaboost2.pred <- predict(adaboost2,newdata=mydata[test,])
table(adaboost2.pred$class,mydata$goal_diff_class2[test],dnn=c("prediction","reality"))
mean(adaboost2.pred$class==mydata$goal_diff_class2[test])

library(pROC)
adaboost2.roc <- roc(mydata$goal_diff_class2[test], adaboost2.pred$prob[,2])
plot(adaboost2.roc,col="blue", lwd=2, xlim=c(1,0),main="boosting of trees (ROC)")
adaboost2.roc$auc


##############################################################################################################################################
# Boosting : Adaboost (3 classes)
##############################################################################################################################################

library(adabag)
mydata <- MCPT[,c(5:ncol(MCPT))]
set.seed(1)
adaboost3 <-boosting(goal_diff_class3~.,data=mydata[train,],boos=TRUE,coeflearn='Breiman')

# Apprentissage
adaboost3.pred <- predict(adaboost3,newdata=mydata[train,])
table(adaboost3.pred$class,mydata$goal_diff_class3[train],dnn=c("prediction","reality"))
mean(adaboost3.pred$class==mydata$goal_diff_class3[train])

# Test
adaboost3.pred <- predict(adaboost3,newdata=mydata[test,])
table(adaboost3.pred$class,mydata$goal_diff_class3[test],dnn=c("prediction","reality"))
mean(adaboost3.pred$class==mydata$goal_diff_class3[test])

###############################################################################################################################################

stopCluster(cl)































