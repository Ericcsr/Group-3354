#PCR
x=model.matrix(cancer_true~.,Everything_noNA)[,-1]
set.seed(2)
train=sample(dim(Everything_noNA)[1],dim(Everything_noNA)[1]*0.7)
train.frame=Everything_noNA[train,]
set.seed(2)
pcr.fit=pcr(cancer_true~., data=Everything_noNA,subset=train,validation="CV")
validationplot(pcr.fit,val.type = "MSEP")
pcr.pred=predict(pcr.fit,x[-train,],ncomp = 60)
pcr.pred[which.max(pcr.pred)]
mean((pcr.pred-Everything_noNA$cancer_true[-train])^2)
plot(pcr.pred)
pcr.result=pcr.pred>0.09
table(pcr.result,Everything_noNA$cancer_true[-train])

#standarlization
x2=model.matrix(cancer_true~.,Everything_noNA_scaled)[,-1]
set.seed(2)
attach(Everything_noNA_scaled)
pcr.fit2=pcr(cancer_true~., data=Everything_noNA_scaled,subset=train,validation="LOO")
validationplot(pcr.fit2,val.type = "MSEP")
pcr.pred2=predict(pcr.fit2,x2[-train,],ncomp = 100)
pcr.pred2[which.max(pcr.pred2)]
mean((pcr.pred2-Everything_noNA_scaled$cancer_true[-train])^2)
plot(pcr.pred2)
pcr.result2=pcr.pred2>0.08
table(pcr.result2,Everything_noNA$cancer_true[-train])