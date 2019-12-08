library(readr)

library(glmnet)

everthing_nona_purified <- read_csv("everthing_nona_purified.csv")

everthing_nona_purified$cancer_true[everthing_nona_purified$cancer_true=="TRUE"] <-1

cancer_col <- everthing_nona_purified["cancer_true"]

everthing_nona_purified <- everthing_nona_purified[-c(1:4)]

#everthing_nona_purified <- as.numeric(unlist(everthing_nona_purified))

numeric_matrix <- model.matrix(cancer_true ~ ., data = everthing_nona_purified)

cancer_col <- as.numeric(unlist(cancer_col))

lasso_cross_valid <- cv.glmnet(numeric_matrix, cancer_col, alpha = 1)

best_lambda <- lasso_cross_valid$lambda.min

lasso_fit <- glmnet(numeric_matrix, cancer_col, alpha = 1)

predict(lasso_fit, s = best_lambda, type = "coefficients")

