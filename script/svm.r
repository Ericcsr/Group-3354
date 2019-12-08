library(e1071)
library(dplyr)

dta <- read.csv(file="Everything_noNA_noID.csv", header=TRUE, sep=",")

df <- select(dta, xsiblingnum, ca000_w3_2_1_, fa001, cc012_w3_1_, ge010_6, Stock.investment, ca001_w3_2_1_, hc039_w3, hand.strength.test.right.2, Water.cigarettes, knee.height, ge004, hd005_w3, i011 , Systolic.2 , cg003_w2_1_, fa006, breath.test.1, waist.circumference,cancer_true)

df$X <- NULL
df$cancer_true = as.factor(df$cancer_true)

set.seed(2)
train=sample(nrow(df), round(0.7*nrow(df)))

# set error.fun for fine tune
mfn = function(real, pred){
	good = real[real==TRUE] == pred[real==TRUE]
	return(length(good[good==FALSE]))
}

# set tune control
tc = tune.control(cross=5, error.fun=mfn)

### polynomial kernel

# svmfit using best parameters that minimizes misclassification
svmfit=svm(cancer_true~., data=df[train,], kernel="polynomial", degree=2, cost=1)
summary(svmfit)

table(true=df[-train,"cancer_true"], pred=predict(svmfit, newdata=df[-train ,]))

# fine tune to minimize false negative
# to minimize misclassification, remove the parameter tunecontrol
tune.out=tune(svm, cancer_true~.,data=df[train,],kernel="polynomial",ranges=list(cost=c(5000, 10000, 20000), degree=c(2,3,4)), tunecontrol=tc)
summary(tune.out)

# confusion matrix of best model after tuning
table(true=df[-train,"cancer_true"], pred=predict(tune.out$best.model, newdata=df[-train ,]))


### radial kernel

# svmfit using best parameters that minimizes misclassification
svmfit=svm(cancer_true~., data=df[train,], kernel="radial", gamma=0.001, cost=1000)
summary(svmfit)

table(true=df[-train,"cancer_true"], pred=predict(svmfit, newdata=df[-train ,]))

# fine tune to minimize false negative
# to minimize misclassification, remove the parameter tunecontrol
tune.out=tune(svm, cancer_true~., data=df[train,], kernel="radial", ranges=list(cost=c(1e4, 5e4, 1e5), gamma=c(0.001, 0.005, 0.01)), tunecontrol=tc)
summary(tune.out)

# confusion matrix of best model after tuning
table(true=df[-train,"cancer_true"], pred=predict(tune.out$best.model, newdata=df[-train ,]))
