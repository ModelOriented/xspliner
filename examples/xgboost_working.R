library(xgboost)
library(xspliner)
library(breakDown)
HR <- HR_data

str(HR_data)
model_matrix_train <- model.matrix(left ~ . -1, HR)
data_train <- xgb.DMatrix(model_matrix_train, label = HR$left)
param <- list(max_depth = 2, objective = "binary:logistic")

HR_xgb_model <- xgb.train(param, data_train, nrounds = 50)
model <- xspline(HR_xgb_model, lhs = "left", response = "left", predictors = colnames(HR)[-7],
                 data = HR, form = "additive", family = "binomial", link = "logit",
                 bare = c("number_project", "time_spend_company", "Work_accident", "promotion_last_5years"),
                 xs_opts = list(effect = list(train = model_matrix_train)),
                 xf_opts = list(transition = list(alter = "never"))) # (todo) need to find out how predict for factors
