library(haven)

# 1. Read Data
## HealthStatus.2015 <- read_dta("Health_Status_and_Functioning.dta")
IndInc.2015 <- read_dta("Individual_Income.dta")
HouInc.2015 <- read_dta("Household_Income.dta")

# 2. Process NA and empty columns
## only maintain the columns with less than half are NA...:
HouInc.2015.v1 <- Filter(function(x){ length(which(!is.na(x)))/length(x) > 0.5}, HouInc.2015)
IndInc.2015.v1 <- Filter(function(x){ length(which(!is.na(x)))/length(x) > 0.5}, IndInc.2015)
## only maintain the columns with less than half are empty cells:
HouInc.2015.v2 <- Filter(function(x){ length(which(x!=""))/length(x) > 0.5 }, HouInc.2015.v1)
IndInc.2015.v2 <- Filter(function(x){ length(which(x!=""))/length(x) > 0.5 }, IndInc.2015.v1)

# 3. Select usable features
## For Household dataset: HouInc.2015.v2:
### detect if most of row in columns are "No":
for (i in c(4,5,36:39,46:48,67)) {
  print (length(which(HouInc.2015.v2[i]==2))/length(which(HouInc.2015.v2[i]==1)))
} #so column 5,38,39,46-48 should be deleted (almost all are "No")
for (i in c(6,19,22,24,27,29:35,50,64)) {
  print (length(which(HouInc.2015.v2[i]==0))/length(which(HouInc.2015.v2[i]!=0)))
  print (sum(is.na(HouInc.2015.v2[i])))
} #19,31,34,35 should be deleted, as almost all 0; through 29 is almost all 0, but it has statidtical indication, so remained.
### delete columns: 7-9,42-44,49,51-54,59-63,65:67(meaningless), 5,19,31,34,35,38,39,46-48(almost all "No")
HouInc.2015.v3 <- HouInc.2015.v2[ -c(5,7:9,19,31,34,35,38,39,42:44,46:49,51:54,59:63,65:67) ]

## For Individual dataset: IndInc.2015.v2:
### detect if most of row in columns are "No":
for (i in c(8:16, 20)) {
  print (length(which(IndInc.2015.v2[i]==2))/length(which(IndInc.2015.v2[i]==1)))
} #so columns 8-15 should be deleted (almost all values are 2), 16 and 20 can be remained
### combine column 18 and 19 of IndInc.2015.v2
IndInc.2015.v2$hd003_004_w3 <- IndInc.2015.v2$hd003 + IndInc.2015.v2$hd004_w3
### delete columns: 5, 21, 22(meaningless), 8-15(almost all "No"), 18-19(combined to new column)
IndInc.2015.v3 <- IndInc.2015.v2[ -c(5,8:15, 18, 19, 21, 22) ]

# 4. change from 1/2 to 1/0:
two2zero <- function(df, index) {
  for (i in index) {
    g <- df[i]
    g[g==2] <- 0
    df[i] <- g
  }
  return(df)
}
HouInc.2015.v3 <- two2zero(HouInc.2015.v3, c(4,9,29))

IndInc.2015.v3 <- two2zero(IndInc.2015.v3, c(4,7,9))

# 5. Special process for Household dataset, individual in the household should be combined:
## but they are almost all NA, so no need to append to household dataset.
for (j in c(1,3,5,6)) {
  value <- 0
  for (i in c(1,2,3,4,5,6,7,8,9,10,11,12)) {
    cname <- paste("ga007_", toString(i), "_s",toString(j), sep="")
    cvalue <- HouInc.2015[c(cname)]
    cvalue[is.na(cvalue)] <- 0
    value <- value + cvalue/j
  }
  print (lapply(value, function(x){ length(which(x==0))/length(x)}))
}

# 6. Write to csv
write_csv(HouInc.2015.v3,"Household_and_individual_data/household2.csv")
write_csv(IndInc.2015.v3,"Household_and_individual_data/individual2.csv")

# 7. Combine the csv according to the household id
total <- merge(IndInc.2015.v3,HouInc.2015.v3,by="ID")
write_csv(total,"Household_and_individual_data/household_and_individual_merged.csv")

## the process to "Everything_new.csv"
library(readr)
Everything_new <- read_csv("Everything_new.csv")
every <- Everything_new[-c(1,7,15,16,18:26,28:30,34,35,37:46)]
every_simple <- every[-c(19,46,58)]
e_nona <- na.omit(every_simple)
write_csv(e_nona,"Sorted data/Everything_noNA.csv")


#-------------------12.2 update----forward feature selection--------------------
library(readr)
library(leaps)
library(MASS)
library(glmnet)
set.seed(2)
df<- read_csv("everthing_nona_purified.csv")

df1 <- as.data.frame(df[127])
for (i in c(5:126)) {
  df1 <- cbind(df1, normalized(as.matrix(df[i])))
}

train_id <- sample(nrow(df1), floor(nrow(df1)*0.7))
df_train <- df1[train_id,]
df_test <- df1[-train_id,]
Y = as.matrix(df_train[1])
X = as.matrix(df_train[-c(1,69,120)])

full.model <- lm(Y~X)
glm.try = glmnet(X,Y, family="binomial")
summary(glm.try)

pfit = predict(glm.try, X, s =0.001, type = "response")
pred <- ifelse(pfit > 0.3, "TRUE", "FALSE")
plot(Y, pfit)
table(Y, pred)

pfit = predict(glm.try, as.matrix(df_test[-c(1,69,120)]), s = 0.001, type = "response")
plot(df_test[1], pfit)

# stepwise selection
step.model <- stepAIC(glm.try, direction = "forward", trace = FALSE)
summary(step.model)
plot(x=Y, y=step.model$fitted.values)
mean((Y - step.model$fitted.values)^2)
###############################################################
# using direction = "both""backward""forward" result
# unscaled
both_feature = c("hc039_w3","hd005_w3","hd003_004_w3","ge010_6","ca013_2_","xsiblingnum",
                 "cc001_w3s1","cc012_w3_1_","i011","ea006_1_","ea006_4_",
                 "fa006","ff012_1","fl022_1","Stock.investment","Systolic.2","breath.test.1",
                 "hand.strength.test.right.2","knee.height","waist.circumference")
# scaled
both_feature = c("hc001","hc039_w3","hd005_w3","ge004","ge010_6","ca000_w3_2_1_","ca001_w3_2_1_","xsiblingnum",
                 "cc001_w3s1","cc012_w3_1_","i011","i020","i021","ea006_2_","ea006_4_","ea006_10_","xrtype",
                 "xrtype","fa006","Stock.investment","Systolic.2","hand.strength.test.right.2","knee.height")
both_feature_X = c("(Intercept)")
for (i in both_feature) {
  j <- paste("X",i,sep="")
  both_feature_X <- c(both_feature_X,j)
}
both_coef = c()
for (i in both_feature_X) {
  j <- step.model[["coefficients"]][[i]]
  both_coef <- c(both_coef,j)
}
both_feature_df <- cbind(rep(1,nrow(df_train)), df_train[both_feature])
both_pred = as.matrix(both_feature_df) %*% as.matrix(both_coef)
plot(x=Y, y=both_pred)
mean((Y - both_pred)^2)
# use logistic regression to see train error
both_df <- cbind(Y,df_train[both_feature])
glm.fits_both = glm(cancer_true~., data=both_df, family =binomial)
summary(glm.fits_both)
glm.prob = predict(glm.fits_both,both_df,type="response")
glm.pred <- ifelse(glm.prob>0.25, TRUE, FALSE)
table(glm.pred, Y)

# use logistic regression to see test error
Y_test = as.matrix(df_test["cancer_true"])
both_df <- cbind(Y_test,df_test[both_feature])
glm.fits_both = glm(cancer_true~., data=both_df, family =binomial)
summary(glm.fits_both)
glm.prob = predict(glm.fits_both,both_df,type="response")
glm.pred <- ifelse(glm.prob>0.25, TRUE, FALSE)
table(glm.pred, Y_test)

normalized<-function(y) {
  x<-y[!is.na(y)]
  x<-(x - min(x)) / (max(x) - min(x))
  y[!is.na(y)]<-x
  return(y)
}
#############################################################
# use logistic regression to see error
# 
# both_df <- cbind(Y,df[both_feature])
# 
# both_df2 <- as.data.frame(both_df[1])
# for (i in c(2:23)) {
#   both_df2 <- cbind(both_df2, normalized(both_df[i]))
# }
# 
# both_df2 <- scale(both_df, center = TRUE,scale = TRUE)
# 
# glm.fits_both = glm(cancer_true~., data=both_df2, family =binomial)
# summary(glm.fits_both)
# glm.prob = predict(glm.fits_both,both_df2,type="response")
# glm.pred <- ifelse(glm.prob>0.5, TRUE, FALSE)
# table(glm.pred, Y)

#regfit.full=regsubsets(x=as.matrix(df[-c(1:4,127)]), y=as.matrix(df[127]))
#regfit.fwd = regsubsets(y=as.matrix(df[127]), x=as.matrix(df[-c(1:4,127)]), nvmax=20, method="forward")
#summary (regfit.fwd)

#--------------12.7 update------------
library(readr)
library(leaps)
library(MASS)
library(glmnet)
library(magrittr)
set.seed(2)
df<- read_csv("everthing_nona_purified.csv")

df1 <- as.data.frame(df[127])
for (i in c(5:126)) {
  df1 <- cbind(df1, normalized(as.matrix(df[i])))
}

df1 <- df1[-c(43:53, 69, 120)]

train_id <- sample(nrow(df1), floor(nrow(df1)*0.7))
df_train <- df1[train_id,]
df_test <- df1[-train_id,]
Y_train = as.matrix(df_train[1])
X_train = as.matrix(df_train[-c(1)])
Y_test = as.matrix(df_test[1])
X_test = as.matrix(df_test[-c(1)])

glm.try = glm(cancer_true~., data=df_train, family =binomial)
summary(glm.try)
pfit <- glm.try %>% predict(df_test, type = "response")
pred <- ifelse(pfit > 0.2, "TRUE", "FALSE")
plot(Y_test, pfit)
table(Y, pred)

nothing = glm(cancer_true~1, data=df_train, family=binomial)
pfit <- glm.try %>% predict(df_train, type = "response")
plot(Y_train, pfit)
summary(nothing)

forwards <- step(nothing,scope=list(lower=formula(nothing),upper=formula(glm.try)), direction="forward")
formula(forwards)
summary(forwards)

forward.lg <- glm(cancer_true~xsiblingnum + ca000_w3_2_1_ + fa001 + cc012_w3_1_ + 
                    ge010_6 + Stock.investment + ca001_w3_2_1_ + knee.height + 
                    ge004 + hd005_w3 + i011 + hc039_w3 + Systolic.2 + 
                    hand.strength.test.right.2 + Water.cigarettes + breath.test.1 + 
                    fa006 + cg003_w2_1_ + waist.circumference, data=df_train, family=binomial)
summary(forward.lg)
pfit <- predict(forward.lg, df_test, type = "response")
plot(Y_test, pfit)
pred <- ifelse(pfit>2/80, 1, 0)
table(pred, Y_test)

backwards <- step(glm.try)

g <- roc(Y_test~pfit, plot=TRUE, xlab="Test_specificity", ylab="Test_sensitivity",print.auc=TRUE)






step.model <- glm.try
i=0
while (i<100) {
  i = i+1
  step.model <- stepAIC(step.model, direction="forward", trace = FALSE)
}

coef(step.model)
summary(step.model)

