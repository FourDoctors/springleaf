trainAndPredict.xgb <- function(
    training,
    label,
    testing,
    testid = test$ID,
    nrounds = 20,
    max.depth = 10,
    eta = 0.01
    ) {

    clf <- xgboost(data = training,
                   label = label,
                   nrounds = nrounds,
                   max.depth = max.depth,
                   eta = eta,
                   objective = "binary:logistic",
                   eval_metric = "auc")

    prediction.train <- predict(clf, data.matrix(training))
    prediction.test <- predict(clf, data.matrix(testing))

    cat("training AUC", roc.auc(prediction.train, label), "\n")

    subm <- data.frame( ID = testid, target = prediction.test)
    paramstring <- paste(
        paste('nrounds', nrounds, sep="_"),
        paste('maxDepth', max.depth, sep="_"),
        paste('eta', eta, sep="_"),
        sep = ".")
    filename <- paste("../data/predictions/",
                      paste("prediction_xgb", paramstring, sep="."),
                      sep="")
    write.csv(subm, file = paste(filename, "csv", sep="."), row.names=FALSE)
}
