# Using random forest to do the analyse
library(tree)
library(randomForest)
library(ISLR)
library(MASS)
library(e1071)
library(neuralnet)

# Sample division
attach(On_stage_data)
On_stage_data = On_stage_data[,-1]
set.seed(2)
train1 = sample(which(On_stage_data$cancer_true==T),0.8*length(which(On_stage_data$cancer_true==T)),replace = F)
train2 = sample(which(On_stage_data$cancer_true==F),0.8*length(which(On_stage_data$cancer_true==F)),replace = F)
train  = rep(NA,length(train1)*0.75+length(train2)*0.75+100)
for (k in 1:length(train1)*0.75)
{
  train[k] = train1[k]
}
for( k in (length(train1)*0.75+1):(length(train1)*0.75+length(train2)*0.75))
{
  train[k] = train2[k-length(train1)*0.75]
}
train = na.omit(train)
# Create validation set
val = rep(NA,length(train1)*0.25+length(train2)*0.25+100)
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
test = (1:length(On_stage_data$ga001))[-train]
# Cross validate random forest
total_err.cv = rep(NA,122)
second_err.cv= rep(NA,122)
validation.set = cancer_true[val]
for (i in 1:122)
{
  rf.model = randomForest(as.factor(cancer_true)~.,data = On_stage_data,subset = train, mtry = i, ntree = 1000)
  rf.model.pred = predict(rf.model,newdata = On_stage_data[val,],type='class')
  t = table(rf.model.pred,validation.set)
  total_err.cv[i] = (t[2]+t[3])/(sum(t))
  second_err.cv[i]= t[4]/(t[4]+t[3])
}

rf.second_err.min= which.min(second_err.cv)
rf.total_err = total_err.cv[rf.second_err.min] # For weighting
rf.model = randomForest(as.factor(cancer_true)~.,data=On_stage_data,subset=train,mtry=second_err.min,ntree= 1000)
# Use boosted tree
library(gbm)
# Use validation set method to obtain best depth
boost.total_err  = rep(NA,4)
boost.second_err = rep(NA,4)
depth_vec = c(1,2,4,8)
for (d in 1:length(depth_vec))
{
  boost.model = gbm(cancer_true~.,data=On_stage_data[train,c(-119,-68)],distribution ="bernoulli", n.trees=5000,interaction.depth = depth_vec[d])
  boost.predict = predict(boost.model,newdata=On_stage_data[val,c(-68,-119)],n.trees=5000)
  boost.predict = (boost.predict > -20) #Set threshold
  t_buffer = table(boost.predict,validation.set)
  boost.total_err[d] = (t[2]+t[3])/(sum(t))
  boost.second_err[d]= t[4]/(t[3]+t[4])
}

boost.second.min= which.min(boost.second_err)
boost.test_err = boost.total_err[boost.second.min] # For weighting
boost.model = gbm(cancer_true~.,data=On_stage_data[train,c(-119,-68)],distribution = "bernoulli",n.trees=5000,interaction.depth = boost.second.min)

# Use Logistic regression with preselected best features
both_feature = c("hc001","hc039_w3","hd005_w3","ge004","ge010_6","ca000_w3_2_1_","ca001_w3_2_1_","xsiblingnum",
                 "cc001_w3s1","cc012_w3_1_","i011","i020","i021","ea006_2_","ea006_4_","ea006_10_","xrtype",
                 "xrtype","fa006","Stock.investment","Systolic.2","hand.strength.test.right.2","knee.height","cancer_true")


logit.model = glm(as.factor(cancer_true)~.,data=scale(On_stage_data[train,names(On_stage_data)%in%both_feature],scale=T,center=T),family=binomial)
logit.prob = predict(logit.model,scale(On_stage_data[val,names(On_stage_data)%in%both_feature],center=T,scale=T), type="response")
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
  svm.mod = svm(as.factor(cancer_true)~.,On_stage_data[train,-119], type="C",cost=cost_vec[cost])
  svm.pred = predict(svm.mod,On_stage_data[val,-119])
  svm.table = table(svm.pred,validation.set)
  svm.total_err[cost] = (svm.table[2]+svm.table[3])/sum(svm.table)
  svm.second_err[cost]= svm.table[4]/(svm.table[4]+svm.table[3])
}
svm.second_err.min = which.min(svm.second_err)
svm.test_err   = svm.total_err[svm.second_err.min]
svm.model = svm(as.factor(cancer_true)~.,On_stage_data[train,-119], type='C', cost=cost_vec[svm.second_err.min])

# Using LDA
lda.model = lda(as.factor(cancer_true)~.,On_stage_data[train,names(On_stage_data)%in%both_feature],subset=train)
lda.pred  = predict(lda.model,On_stage_data[val,names(On_stage_data)%in%both_feature])
lda.table = table(lda.pred$class,validation.set)
lda.test_err = (lda.table[4]+lda.table[1])/sum(lda.table)
                
# QDA BUG
qda.model = qda(as.factor(cancer_true)~.,data=On_stage_data[train,names(On_stage_data)%in%both_feature],subset=train)
qda.pred  = predict(qda.model,On_stage_data[val,names(On_stage_data)%in%both_feature])
qda.table = table(qda.pred$class,validation.set)
qda.test_err = (qda.table[4]+qda.table[1])/sum(qda.table)

# Neural network
ann.dframe= scale(On_stage_data[train,names(On_stage_data)%in%both_feature],center=T,scale = T) # Try to standardlize data.
ann.model = neuralnet(cancer_true~.,data=On_stage_data[train,names(On_stage_data)%in%both_feature],hidden=c(25,25,15,5),linear.output = F)
ann.pred  = predict(ann.model,On_stage_data[val,names(On_stage_data)%in%both_feature],type='class')
ann.logit = ann.pred > 0.05
ann.table = table(ann.logit,validation.set)
ann.test_err = (ann.table[1]+ann.table[4])/sum(ann.table)

# Ensemble all learners
test_vote = rep(0,length(test))
ann.final = predict(ann.model,On_stage_data[test,names(On_stage_data)%in%both_feature])
ann.final = ann.final > 0.05

svm.final = predict(svm.model,On_stage_data[test,names(On_stage_data)%in%both_feature])

lda.final = predict(lda.model,On_stage_data[test,names(On_stage_data)%in%both_feature])

logit.final = predict(logit.model,scale(On_stage_data[test,names(On_stage_data)%in%both_feature],center=T,scale=T), type="response")
logit.final = logit.final > 0.25

boost.final = predict(boost.model,newdata=On_stage_data[test,c(-68,-119)],n.trees=5000)
boost.final = boost.final > 20

rf.final = predict(rf.model,On_stage_data[test,],type='class')

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
