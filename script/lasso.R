library(readr)

library(glmnet)

everthing_nona_purified <- read_csv("everthing_nona_purified.csv")

set.seed(1472)
test_row = sample(1:nrow(everthing_nona_purified),nrow(everthing_nona_purified)*3/10)


everthing_nona_purified$cancer_true[everthing_nona_purified$cancer_true=="TRUE"] <-1

cancer_col <- everthing_nona_purified["cancer_true"]

everthing_nona_purified <- everthing_nona_purified[-c(1:4)]

#everthing_nona_purified <- as.numeric(unlist(everthing_nona_purified))

numeric_matrix <- model.matrix(cancer_true ~ ., data = everthing_nona_purified)

cancer_col <- as.numeric(unlist(cancer_col))

# test & train do respectively

train.x = numeric_matrix[-test_row,]
train.y = cancer_col[-test_row]
test.x = numeric_matrix[test_row,]
test.y = cancer_col[test_row]

lasso_cross_valid <- cv.glmnet(train.x, train.y, alpha = 1)

best_lambda <- lasso_cross_valid$lambda.min

lasso_fit <- glmnet(train.x, train.y, alpha = 1)

predict(lasso_fit, s = best_lambda, type = "coefficients")

pred <- predict(lasso_fit, s = best_lambda, newx = test.x)
pred[pred < 0.025] <- 0
pred[pred >= 0.025] <- 1
table(test.y,pred)



