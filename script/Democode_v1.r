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
library(readr)

On_stage_data2 <- read_csv("On_stage_data2.csv")
On_stage_data2  = On_stage_data2[,c(-69)]
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
index = sample(length(train))
train = train[index]
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
rf.model = randomForest(as.numeric(cancer_true)~.,data=On_stage_data2,subset=train,mtry=rf.second_err.min,ntree= 1000)
rf.test = predict(rf.model,newdata=On_stage_data2[test,],type='class')
rf.thresh = rf.test > 0.026 # The true ratio in sample.
rf.table = table(rf.thresh,cancer_true[test])


auc1 = roc(as.numeric(On_stage_data2[test,]$cancer_true),as.numeric(rf.thresh))
par(c(2,1))
plot(auc1,ylim=c(0,1),print.thres=TRUE,main=paste('AUC Without SMOTE',round(auc1$auc[[1]],2)))
# Add the step of overstampling:
training_data = On_stage_data2[train,-121]
training_data = cbind(On_stage_data2[train,121],training_data)
colnames(training_data)[1] = "target"
training_data$target <- ifelse(training_data$target == FALSE,0,1)
training_data$target = as.factor(training_data$target)
training.data = SMOTE(target~.,data=training_data,perc.over = 500,perc.under = 1000)
rf.model = randomForest(as.numeric(target)~.,data=training.data,mtry=rf.second_err.min,ntree = 1000)
test_data = On_stage_data2[test,-121]
test_data = cbind(On_stage_data2[test,121],test_data)
colnames(test_data)[1] = "target"
test_data$target = ifelse(test_data$target==FALSE,0,1)
test_data$target = as.factor(test_data$target)
rf.test2 = predict(rf.model,newdata=test_data,type ="class")
rf.thresh2 = (rf.test2-1) > 0.026
rf.table2 = table(rf.thresh2,On_stage_data2$cancer_true[test])
auc2 = roc(On_stage_data2$cancer_true[test],as.numeric(rf.thresh2))
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
  boost.predict = (1/(1+exp(-boost.predict)) > 0.026) #Set threshold
  t_buffer = table(boost.predict,validation.set)
  boost.total_err[d] = (t_buffer[2]+t_buffer[3])/(sum(t_buffer))
  boost.second_err[d]= t_buffer[3]/(t_buffer[3]+t_buffer[4])
}

boost.second.min= which.min(boost.second_err)
boost.test_err = boost.total_err[boost.second.min] # For weighting
boost.model = gbm(cancer_true~.,data=On_stage_data2[train,],distribution = "bernoulli",n.trees=5000,interaction.depth = boost.second.min)
boost.pred  = predict(boost.model,newdata = On_stage_data2[test,],n.trees=5000)
boost.thre  = 1/(1+exp(-boost.pred)) > 0.026 
table(boost.thre,On_stage_data2$cancer_true[test])
boost.auc   = roc(On_stage_data2[test,]$cancer_true,as.numeric(boost.thre))
plot(boost.auc,ylim=c(0,1),print.thres=TRUE,main=paste('AUC of Boost',round(boost.auc$auc[[1]],2)))
# Use Logistic regression with preselected best features
# Separate code need to conduct stepwise feature selection
both_feature = c("xsiblingnum" ,"ca000_w3_2_1_" ,"fa001","cc012_w3_1_", 
                    "ge010_6","Stock.investment","ca001_w3_2_1_", 
                    "ge004","hd005_w3","i011","hc039_w3","Systolic.2", 
                    "hand.strength.test.right.2", "Water.cigarettes", "breath.test.1", 
                    "fa006","cg003_w2_1_","waist.circumference","cancer_true")
# Using PCA
#PCR
#dataset needed: Everything_noNA_scaled , Everything_noNA_noID
#Everything_noNA_noID=Everything_noNA_noID[-1]
#Everything_noNA_scaled=Everything_noNA_scaled[-1]
library(pls)
x=model.matrix(cancer_true~.,On_stage_data2)[,-1] # Except for the last label column
set.seed(2)
pcr.fit=pcr(cancer_true~., data=On_stage_data2,subset=train,validation="LOO")
validationplot(pcr.fit,val.type = "MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp = 60)
pcr.pred[which.max(pcr.pred)]
mean((pcr.pred-On_stage_data2[test])^2)
#natural incidence
# pcr.pred=1/(1+exp(-pcr.pred))
plot(pcr.pred)
pcr.result=pcr.pred>=0.026
table(pcr.result,On_stage_data2$cancer_true[test])

#PCR standarlization
pcr.scaled.dframe = scale(On_stage_data2,center = T,scale = T)
pcr.scaled.dframe = cbind(On_stage_data2$cancer_true,On_stage_data2[,-121])
colnames(pcr.scaled.dframe)[1]="cancer_true"
x2=model.matrix(cancer_true~.,pcr.scaled.dframe)[,-1]
pcr.fit2=pcr(cancer_true~., data=pcr.scaled.dframe,subset=train,validation="LOO")
validationplot(pcr.fit2,val.type = "MSEP")
pcr.pred2=predict(pcr.fit2,x2[test,],ncomp = 100)
pcr.pred2[which.max(pcr.pred2)]
mean((pcr.pred2-On_stage_data2$cancer_true[test])^2)
plot(pcr.pred2)
#natural incidence
# pcr.pred2=1/(1+exp(-pcr.pred2))
plot(pcr.pred2)
pcr.result2=pcr.pred2>=0.026
table(pcr.result2,On_stage_data2$cancer_true[test])

# Lasso + Logistic regression:
library(glmnet)
numeric_matrix <- model.matrix(cancer_true ~ ., data = On_stage_data2)[,-1]
# cancer_col <- as.numeric(unlist(cancer_col))
# test & train do respectively
train.x = numeric_matrix[train,]
train.y = as.numeric(On_stage_data2$cancer_true[train])
test.x = numeric_matrix[test,]
test.y = as.numeric(On_stage_data2$cancer_true[test])
lasso_cross_valid <- cv.glmnet(train.x, train.y, alpha = 1) # How the Alpha is set?
best_lambda <- lasso_cross_valid$lambda.min
lasso_fit <- glmnet(train.x, train.y, alpha = 1)
predict(lasso_fit, s = best_lambda, type = "coefficients")
pred <- predict(lasso_fit, s = best_lambda, newx = test.x)
lasso.result = pred > 0.026
table(lasso.result,On_stage_data2$cancer_true[test])

# Standardlize data
logit.buffer = scale(On_stage_data2[train,names(On_stage_data2)%in%both_feature],scale=T,center = T)
logit.buffer = data.frame(logit.buffer)
logit.buffer[,length(both_feature)] = cancer_true[train]
logit.model = glm(cancer_true~.,data=logit.buffer,family=binomial)
logit.prob = predict(logit.model,data.frame(scale(On_stage_data2[test,names(On_stage_data2)%in%both_feature],center=T,scale=T)), type="response")
logit.pred = logit.prob > 0.026 # According to the ground truth proposal from data.
logit.table = table(logit.pred,On_stage_data2$cancer_true[test])
logit.test_err = (logit.table[2]+logit.table[3])/(sum(logit.table))
logit.auc = roc(On_stage_data2$cancer_true[test],as.numeric(logit.pred))
plot(logit.auc,ylim=c(0,1),print.thres=TRUE,main=paste('AUC of Logistic Regression',round(logit.auc$auc[[1]],2)))

logit.train.prob = predict(logit.model,data.frame(scale(On_stage_data2[train,names(On_stage_data2)%in%both_feature],center=T,scale=T)),type="response")
logit.train.pred = logit.train.prob > 0.026
logit.train.table =table(logit.train.pred,On_stage_data2$cancer_true[train])
logit.train.auc = roc(On_stage_data2$cancer_true[train],as.numeric(logit.train.pred))
plot(logit.train.auc,ylim=c(0,1),print.thres=TRUE,main=paste('AUC of Logistic Regression (Training data)',round(logit.train.auc$auc[[1]],2)))
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
svm.linear.second_err = rep(NA, length(cost_vec))
svm.linear.val_err    = rep(NA, length(cost_vec)) 
for (cost in 1:length(cost_vec))
{
  svm.linear.mod = svm(as.factor(cancer_true)~.,On_stage_data2[train,],type="C",cost=cost_vec[cost],kernel = "linear")
  svm.linear.pred= predict(svm.linear.mod, On_stage_data2[val,])
  svm.linear.table = table(svm.linear.pred,validation.set)
  svm.linear.val_err[cost]   = (svm.linear.table[2]+svm.linear.table[3])/sum(svm.linear.table)
  svm.linear.second_err[cost]= (svm.linear.table[3])/(svm.linear.table[3]+svm.linear.table[4])
}


svm.second_err.min = which.min(svm.second_err)
svm.test_err   = svm.total_err[svm.second_err.min]
svm.model = svm(as.factor(cancer_true)~.,On_stage_data2[train,], type='C', cost=cost_vec[svm.second_err.min])
svm.pred = predict(svm.model,On_stage_data2[test,])
table(svm.pred,On_stage_data2$cancer_true[test])
svm.auc = roc(On_stage_data2$cancer_true[test],svm.pred)
plot(logit.train.auc,ylim=c(0,1),print.thres=TRUE,main=paste('AUC of Radial SVM',round(svm.auc$auc[[1]],2)))


svm.linear.second_err.min = which.min(svm.linear.second_err)
svm.linear.model = svm(as.factor(cancer_true)~.,On_stage_data2[train,], type='C', cost=cost_vec[svm.linear.second_err.min],kernel="linear")
svm.linear.pred = predict(svm.model,On_stage_data2[test,])
table(svm.linear.pred,On_stage_data2$cancer_true[test])
svm.linear.auc = roc(On_stage_data2$cancer_true[test],svm.linear.pred)
plot(logit.train.auc,ylim=c(0,1),print.thres=TRUE,main=paste('AUC of Linear SVM',round(svm.linear.auc$auc[[1]],2)))
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
ann.dframe = scale(On_stage_data2[train,names(On_stage_data2)%in%both_feature],center=T,scale = T) # Try to standardlize data.
ann.dframe = data.frame(ann.dframe)
ann.dframe$cancer_true = On_stage_data2$cancer_true[train]
#ann.dframe = cbind(ann.dframe[,19],ann.dframe[,-19])
#colnames(ann.dframe)[1] = "target"
#ann.dframe$target = ifelse(ann.dframe$target==TRUE,1,0)
#ann.dframe$target = as.factor(ann.dframe$target)
#ann.dframe = SMOTE(target~.,ann.dframe,perc.over = 500,perc.under = 1000)


ann.model = neuralnet(cancer_true~.,data=ann.dframe,hidden=c(100),linear.output = F)
ann.pred  = predict(ann.model,data.frame(scale(On_stage_data2[test,names(On_stage_data2)%in%both_feature],center=T,scale=T)),type='class')
ann.logit = ann.pred > 0.026
ann.table = table(ann.logit,On_stage_data2$cancer_true[test])
ann.test_err = (ann.table[1]+ann.table[4])/sum(ann.table)

ann.auc = roc(On_stage_data2$cancer_true[test],as.numeric(ann.pred))
plot(ann.auc,ylim=c(0,1),print.thres=TRUE,main=paste('AUC of Neural Net',round(ann.auc$auc[[1]],2)))

# Ensemble all learners TODO: Test the functionality when available
test_vote = rep(0,length(test))
ann.final = predict(ann.model,On_stage_data2[test,names(On_stage_data2)%in%both_feature])
ann.final = ann.final > 0.026

svm.final = predict(svm.model,On_stage_data2[test,names(On_stage_data2)%in%both_feature])

lda.final = predict(lda.model,On_stage_data2[test,names(On_stage_data2)%in%both_feature])

logit.final = predict(logit.model,scale(On_stage_data2[test,names(On_stage_data2)%in%both_feature],center=T,scale=T), type="response")
logit.final = logit.final > 0.026

boost.final = predict(boost.model,newdata=On_stage_data2[test,],n.trees=5000)
boost.final = 1/(1+exp(-boost.final))
boost.final = boost.final > 0.026

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


#----------seperate stepwise selection----------
library(readr)
library(leaps)
library(MASS)
library(glmnet)
library(magrittr)
set.seed(2)

df.scaled = pcr.scaled.dframe
#----- to be replaced with csr train:val:test = 6:2:2 split
#----- replacement done----------------------------

Y_train = as.matrix(df.scaled[train,1]) #cancer_true
X_train = as.matrix(df.scaled[train,-1]) #features
Y_test = as.matrix(df.scaled[test,1]) #cancer_true
X_test = as.matrix(df.scaled[test,-1]) #features

#full.model
full.model = glm(cancer_true~., data=df.scaled[train,], family =binomial)
summary(full.model)
pfit.full <- full.model %>% predict(df.scaled[train,], type = "response")
pred.full <- ifelse(pfit.full > 200/8000, "TRUE", "FALSE")
plot(Y_train, pfit.full)
table(pred.full,Y_train)

nothing = glm(cancer_true~1, data=df.scaled[train,], family=binomial)
summary(nothing)
pfit.nothing <- nothing %>% predict(df.scaled[train,], type = "response")
pred.nothing <- ifelse(pfit.nothing > 200/8000, "TRUE", "FALSE")
plot(Y_train, pfit.nothing)
table(pred.nothing,Y_train)

forwards <- step(nothing,scope=list(lower=formula(nothing),upper=formula(full.model)), direction="forward")
formula(forwards)
summary(forwards)

backwards <- step(full.model)
formula(backwards)
summary(backwards)

##-----to be replaced with new logistic regression
forward.lg <-glm(formula = cancer_true ~ xsiblingnum + ca001_w3_2_1_ + ge010_6 + 
                   cc012_w3_1_ + fa001 + ca024_w3_1_ + Community.activities + 
                   cg003_w2_1_ + ea006_4_ + i015 + breath.test.1 + hd005_w3 + 
                   ea006_10_ + ea006_6_ + ca013_2_ + Stock.investment + Water.cigarettes + 
                   i018 + Community.game + waist.circumference + fa006 + hand.strength.test.quality, 
                 family = binomial, data = df.scaled[train, ])

summary(forward.lg)
pfit <- predict(forward.lg, df.scaled[test,], type = "response")
plot(Y_test, pfit)
pred <- ifelse(pfit>2/80, 1, 0)
table(pred, Y_test)

library(pROC)
g <- roc(Y_test~pfit, plot=TRUE, xlab="Test_specificity", ylab="Test_sensitivity",print.auc=TRUE)
