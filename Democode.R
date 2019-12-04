# Using random forest to do the analyse
library(tree)
library(randomForest)
library(ISLR)

# Sample division
attach(On_stage_data)
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
  total_err.cv[i] = (t[2]+t[3])/(t[1]+t[4])
  second_err.cv[i]= t[4]/(t[4]+t[3])
}
total_err.min = which.min(total_err.cv)
second_err.min= which.min(second_err.cv)
rf.model1 = randomForest(as.factor(cancer_true)~.,data=On_stage_data,subset=train,mtry=total_err.min,ntree = 1000)
rf.model2 = randomForest(as.factor(cancer_true)~.,data=On_stage_data,subset=train,mtry=second_err.min,ntree= 1000)
# Use boosted tree
library(gbm)
# Use validation set method to obtain best depth
boost.total_err  = rep(NA,4)
boost.second_err = rep(NA,4)
depth_vec = c(1,2,4,8)
boost
for (d in 1:length(depth_vec))
{
  boost.model = gbm(as.factor(cancer_true)~.,data=On_stage_data[train,],distribution ="Bernoulli", n.trees=5000,interaction.depth = depth_vec[d])
  boost.predict = predict(boost.model,newdata=On_stage_data[val,],ntrees=5000)
  boost.predict = (boost.predict > -20) #Set threshold
  t_buffer = table(boost.predict,validation.set)
  boost.total_err[d] = (t[2]+t[3])/(t[1]+t[4])
  boost.second_err[d]= t[4]/(t[3]+t[4])
}
boost.total.min = which.min(boost.total_err)
boost.second.min= which.min(boost.second_err)
boost.model1 = gbm(as.factor(cancer_true)~.,data=On_stage_data[train,],distribution = "Bernoulli",n.trees=5000,interaction.depth = boost.total.min)
boost.model2 = gbm(as.factor(cancer_true)~.,data=On_stage_data[train,],distribution = "Bernoulli",n.trees=5000,interaction.depth = boost.second.min)

# Use Logistic regression with preselected best features

# To be accomplished

# Use PCA and logisitic regression

pca.out = prcomp(On_stage_data[,-123],scale=TRUE)







