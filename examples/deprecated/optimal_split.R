library("DALEX")
library("randomForest")
data <- breakDown::HR_data
# data$Work_accident <- factor(data$Work_accident,
#                              levels = c(0, 1),
#                              labels = c("no", "yes"))
HR_rf_model <- randomForest(factor(left)~., data = data, ntree = 100)
explainer_rf  <- explain(HR_rf_model, data = data,
                         predict_function = function(model, x)
                           predict(model, x, type = "prob")[,2])
expl_rf  <- variable_response(explainer_rf, variable = "sales", type = "pdp",
                              which.class = 2, prob = TRUE)
factorMerger::getOptimalPartitionDf(expl_rf, "GIC", 3)

oko <- expl_rf
plot(expl_rf)
# some helperf to get optimal split
factorMerger:::optimalNumberOfMerges(expl_rf)
oko <- factorMerger::getOptimalPartitionDf(expl_rf, "GIC", 2)
plot(expl_rf, showSplit = TRUE)

