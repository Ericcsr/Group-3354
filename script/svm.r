library(e1071)

df <- read.csv(file="Everything_noNA_noID.csv", header=TRUE, sep=",")

# remove unusable columns
df$ff012_3 <- NULL
df$fl020s6 <- NULL
df$which.knee <- NULL

# divde data into train and test
index <- sample(2,nrow(df),replace = TRUE,prob=c(0.7,0.3))
traindata <- df[index==1,]
testdata <- df[index==2,]

# set up svm model
svm_model <- svm(cancer_true~.,data=traindata, type="C")
svm_model

# predict on training data
svm_pred_1 <- predict(svm_model,traindata[,-121])
table_1 <- table(pred=svm_pred_1,true=traindata[,121])
table_1

accuracy_1 <- sum(diag(table_1))/sum(table_1)
accuracy_1

# predict on test data
svm_pred_2 <- predict(svm_model,testdata[,-121])
table_2 <- table(pred=svm_pred_2,true=testdata[,121])
table_2

accuracy_2 <- sum(diag(table_2))/sum(table_2)
accuracy_2

# generate subset
subdf <- select(df, hc039_w3, hd005_w3, hd003_004_w3, ge010_6, ca013_2_, xsiblingnum, cc001_w3s1, cc012_w3_1_, i011, ea006_1_, ea006_4_, ea006_10_, xrtype, fa006, ff012_1, fl022_1, Stock.investment, Systolic.2, breath.test.1, hand.strength.test.right.2, knee.height, waist.circumference, cancer_true)

# repeat the process with all columns
index <- sample(2,nrow(subdf),replace = TRUE,prob=c(0.7,0.3))
subtrain <- subdf[index==1,]
subtest <- subdf[index==2,]

svm_model_sub <- svm(cancer_true~.,data=subtrain, type="C")
svm_model_sub

svm_pred_1_sub <- predict(svm_model_sub,subtrain[,-23])
table_1_sub <- table(pred=svm_pred_1_sub,true=subtrain[,23])
table_1_sub

accuracy_1_sub <- sum(diag(table_1_sub))/sum(table_1_sub)
accuracy_1_sub

svm_pred_2_sub <- predict(svm_model_sub,subtest[,-23])
table_2_sub <- table(pred=svm_pred_2_sub,true=subtest[,23])
table_2_sub

accuracy_2_sub <- sum(diag(table_2_sub))/sum(table_2_sub)
accuracy_2_sub
