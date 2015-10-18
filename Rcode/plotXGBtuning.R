plotXGBtuning <- function(df.tuning) {

    df.tuning.ml <- melt(df.tuning[, c("nrounds", "max.depth", "eta",
                                       "train.auc.mean", "test.auc.mean")],
                         id.vars = c("nrounds", "max.depth", "eta"))

    xyplot(value ~ nrounds | variable + as.factor(eta),
           groups = as.factor(max.depth),
           data=df.tuning.ml,
           type = "b",
           auto.key = TRUE,
           main = "Tuning curves for XGBOOST ")


}
