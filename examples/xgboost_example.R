library(breakDown)
library(xgboost)
library(xspliner)
model_matrix_train <- model.matrix(left ~ . -1, HR_data)
data_train <- xgb.DMatrix(model_matrix_train, label = HR_data$left)
param <- list(max_depth = 2, objective = "binary:logistic")

HR_xgb_model <- xgb.train(param, data_train, nrounds = 50)

model <- xspline(HR_xgb_model, "left", data = model_matrix_train)

partial(HR_xgb_model, "sales", train = model_matrix_train)
