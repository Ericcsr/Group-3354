# Using random forest to do the analyse
library(tree)
library(randomForest)
library(ISLR)
library(MASS)
library(e1071)
library(neuralnet)
library(DMwR)
library(gbm)
library(lattice)
library(grid)
library(pROC)

On_stage_data2 <- read_csv("../On_stage_data2.csv")
On_stage_data2  = On_stage_data2[,c(-1,-69)]
# Sample division


attach(On_stage_data2)
#On_stage_data2 = On_stage_data2[,-1]
set.seed(2)
train1 = sample(which(On_stage_data2$cancer_true==T),0.8*length(which(On_stage_data2$cancer_true==T)),replace = F)
train2 = sample(which(On_stage_data2$cancer_true==F),0.8*length(which(On_stage_data2$cancer_true==F)),replace = F)
train  = rep(NA,length(train1)*0.8+length(train2)*0.8+100)
for (k in 1:length(train1)*0.8)
{
  train[k] = train1[k]
}
for( k in (length(train1)*0.8+1):(length(train1)*0.8+length(train2)*0.8))
{
  train[k] = train2[k-length(train1)*0.8]
}
train = na.omit(train)
# Create validation set
val = rep(NA,length(train1)*0.2+length(train2)*0.2+100)
counter = 1
for(k in train1)
{
  if (! k%in% train)
  {
    val[counter] = k
    counter = counter+1
  }
}
for (k in train2)
{
  if(! k%in% train)
  {
    val[counter] = k
    counter = counter+1
  }
}
val = na.omit(val)
# create test set
test = (1:length(On_stage_data2$ga001))[-train]
# Cross validate random forest
total_err.cv = rep(NA,120)
second_err.cv= rep(NA,120)
validation.set = cancer_true[val]
for (i in 1:120)
{
  rf.model = randomForest(as.factor(cancer_true)~.,data = On_stage_data2,subset = train, mtry = i, ntree = 1000)
  rf.model.pred = predict(rf.model,newdata = On_stage_data2[val,],type='class')
  t = table(rf.model.pred,validation.set)
  total_err.cv[i] = (t[2]+t[3])/(sum(t))
  second_err.cv[i]= t[3]/(t[4]+t[3])
} # Second error min 0.8

rf.second_err.min= which.min(second_err.cv)
rf.total_err = total_err.cv[rf.second_err.min] # For weighting
rf.model = randomForest(as.factor(cancer_true)~.,data=On_stage_data2,subset=train,mtry=rf.second_err.min,ntree= 1000)
rf.test = predict(rf.model,newdata=On_stage_data2[test,],type='class')
rf.table = table(rf.test,cancer_true[test])

auc1 = roc(as.numeric(On_stage_data2[test,]$cancer_true),as.numeric(rf.test))
par(c(2,1))
plot(auc1,ylim=c(0,1),print.thres=TRUE,main=paste('AUC Without SMOTE',round(auc1$auc[[1]],2)))
# Add the step of overstampling:
training_data = On_stage_data2[train,-121]
training_data = cbind(On_stage_data2[train,121],training_data)
colnames(training_data)[1] = "target"
training_data$target <- ifelse(training_data$target == FALSE,0,1)
training_data$target = as.factor(training_data$target)
training.data = SMOTE(target~.,data=training_data,perc.over = 500,perc.under = 1000)
rf.model = randomForest(target~.,data=training.data,mtry=rf.second_err.min,ntree = 1000)
test_data = On_stage_data2[test,-121]
test_data = cbind(On_stage_data2[test,121],test_data)
colnames(test_data)[1] = "target"
test_data$target = ifelse(test_data$target==FALSE,0,1)
test_data$target = as.factor(test_data$target)
rf.test2 = predict(rf.model,newdata=test_data,type ="class")
rf.table2 = table(rf.test2,On_stage_data2$cancer_true[test])
auc2 = roc(test_data$target,as.numeric(rf.test2))
plot(auc2,ylim=c(0,1),print.thres=TRUE,main=paste('AUC with SMOTE',round(auc2$auc[[1]],2)))
varImpPlot(rf.model)
# Use boosted tree
# Use validation set method to obtain best depth
boost.total_err  = rep(NA,4)
boost.second_err = rep(NA,4)
depth_vec = c(1,2,4,8)
cancer_true = ifelse(cancer_true==TRUE,1,0)
On_stage_data2 = On_stage_data2[,-121]
On_stage_data2 = cbind(On_stage_data2,cancer_true)
for (d in 1:length(depth_vec))
{
  boost.model = gbm(cancer_true~.,data=On_stage_data2[train,],distribution ="bernoulli", n.trees=5000,interaction.depth = depth_vec[d])
  boost.predict = predict(boost.model,newdata=On_stage_data2[val,],n.trees=5000)
  boost.predict = (boost.predict > -5) #Set threshold
  t_buffer = table(boost.predict,validation.set)
  boost.total_err[d] = (t_buffer[2]+t_buffer[3])/(sum(t_buffer))
  boost.second_err[d]= t_buffer[3]/(t_buffer[3]+t_buffer[4])
}

boost.second.min= which.min(boost.second_err)
boost.test_err = boost.total_err[boost.second.min] # For weighting
boost.model = gbm(cancer_true~.,data=On_stage_data2[train,],distribution = "bernoulli",n.trees=5000,interaction.depth = boost.second.min)
boost.pred  = predict(boost.model,newdata = On_stage_data2[test,],n.trees=5000)
boost.thre  = boost.pred > -5
boost.auc   = roc(On_stage_data2[test,]$cancer_true,as.numeric(boost.thre))
plot(boost.auc,ylim=c(0,1),print.thres=TRUE,main=paste('AUC of Boost',round(boost.auc$auc[[1]],2)))
# Use Logistic regression with preselected best features
both_feature = c("hc001","hc039_w3","hd005_w3","ge004","ge010_6","ca000_w3_2_1_","ca001_w3_2_1_","xsiblingnum",
                 "cc001_w3s1","cc012_w3_1_","i011","i020","i021","ea006_2_","ea006_4_","ea006_10_","xrtype",
                 "xrtype","fa006","Stock.investment","Systolic.2","hand.strength.test.right.2","knee.height","cancer_true")

# Standardlize data
logit.buffer = scale(On_stage_data2[train,names(On_stage_data2)%in%both_feature],scale=T,center = T)
logit.buffer = data.frame(logit.buffer)
logit.buffer[,22] = cancer_true[train]
logit.model = glm(cancer_true~.,data=logit.buffer,family=binomial)
logit.prob = predict(logit.model,scale(On_stage_data[val,names(On_stage_data2)%in%both_feature],center=T,scale=T), type="response")
logit.pred = logit.prob > 0.1 # According to the ground truth proposal from data.
logit.table = table(logit.pred,validation.set)
logit.test_err = (logit.table[2]+logit.table[3])/(sum(logit.table))

# Use PCA and logisitic regression Arguable.

# Use support vector machine.
cost_vec = c(0.001,0.01,0.1,1,5,10)
svm.second_err = rep(NA, length(cost_vec))
svm.total_err  = rep(NA, length(cost_vec))
for (cost in 1:length(cost_vec))
{
  svm.mod = svm(as.factor(cancer_true)~.,On_stage_data2[train,], type="C",cost=cost_vec[cost])
  svm.pred = predict(svm.mod,On_stage_data2[val,])
  svm.table = table(svm.pred,validation.set)
  svm.total_err[cost] = (svm.table[2]+svm.table[3])/sum(svm.table)
  svm.second_err[cost]= svm.table[3]/(svm.table[4]+svm.table[3])
}
svm.second_err.min = which.min(svm.second_err)
svm.test_err   = svm.total_err[svm.second_err.min]
svm.model = svm(as.factor(cancer_true)~.,On_stage_data2[train,], type='C', cost=cost_vec[svm.second_err.min])
svm.pred = predict(svm.model,On_stage_data2[test,])

# Using LDA
lda.model = lda(as.factor(cancer_true)~.,On_stage_data2[train,names(On_stage_data2)%in%both_feature],subset=train)
lda.pred  = predict(lda.model,On_stage_data2[val,names(On_stage_data2)%in%both_feature])
lda.table = table(lda.pred$class,validation.set)
lda.test_err = (lda.table[4]+lda.table[1])/sum(lda.table)
                
# QDA BUG
qda.model = qda(as.factor(cancer_true)~.,data=On_stage_data2[train,names(On_stage_data2)%in%both_feature],subset=train)
qda.pred  = predict(qda.model,On_stage_data2[val,names(On_stage_data2)%in%both_feature])
qda.table = table(qda.pred$class,validation.set)
qda.test_err = (qda.table[4]+qda.table[1])/sum(qda.table)

# Neural network
ann.dframe= scale(On_stage_data2[train,names(On_stage_data2)%in%both_feature],center=T,scale = T) # Try to standardlize data.
ann.model = neuralnet(cancer_true~.,data=On_stage_data2[train,names(On_stage_data2)%in%both_feature],hidden=c(25,25,15,5),linear.output = F)
ann.pred  = predict(ann.model,On_stage_data2[val,names(On_stage_data2)%in%both_feature],type='class')
ann.logit = ann.pred > 0.05
ann.table = table(ann.logit,validation.set)
ann.test_err = (ann.table[1]+ann.table[4])/sum(ann.table)

# Ensemble all learners
test_vote = rep(0,length(test))
ann.final = predict(ann.model,On_stage_data2[test,names(On_stage_data2)%in%both_feature])
ann.final = ann.final > 0.05

svm.final = predict(svm.model,On_stage_data2[test,names(On_stage_data2)%in%both_feature])

lda.final = predict(lda.model,On_stage_data2[test,names(On_stage_data2)%in%both_feature])

logit.final = predict(logit.model,scale(On_stage_data2[test,names(On_stage_data2)%in%both_feature],center=T,scale=T), type="response")
logit.final = logit.final > 0.25

boost.final = predict(boost.model,newdata=On_stage_data2[test,],n.trees=5000)
boost.final = boost.final > 20

rf.final = predict(rf.model,On_stage_data2[test,],type='class')

error_vector = c(ann.test_err,boost.test_err,rf.total_err,logit.test_err,svm.test_err,lda.test_err)
weight_vector = rep(0,6)
for (i in 1:6)
{
  weight_vector[i] = error_vector[i]/sum(error_vector)
}
test_vote = test_vote + weight_vector[1]*ann.final
test_vote = test_vote + weight_vector[2]*boost.final
test_vote = test_vote + weight_vector[3]*rf.final
test_vote = test_vote + weight_vector[4]*logit.final
test_vote = test_vote + weight_vector[5]*svm.final
test_vote = test_vote + weight_vector[6]*lda.final
# Threshold the test vote data
test_vote = test_vote > 0.5
