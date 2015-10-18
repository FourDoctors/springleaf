tunexgb <- function(
    training,
    label,
    grid = expand.grid(max.depth = c(6, 9),
        eta = c(0.01, 0.1, 1.0)),
    nrounds = 40,
    num.threads = 3) {

    foreach(n=1:nrow(grid), .combine = 'rbind') %do% {
        md <- grid$max.depth[n]
        eta <- grid$eta[n]
        print(paste("max.depth", md, "eta", eta))
        df.cv <- xgb.cv(param = list(max.depth = md,
                            eta = eta,
                            silent = 1,
                            nthread = num.threads,
                            objective = 'binary:logistic'),
                        data = xgb.DMatrix(data.matrix(training),
                            label = label),
                        nrounds = nrounds,
                        nfold = 5,
                        metrics = {'auc'} )
        df.cv$nrounds <- 1:nrounds
        df.cv$max.depth <- md
        df.cv$eta <- eta
        df.cv
    }
}





