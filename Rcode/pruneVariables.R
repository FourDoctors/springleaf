## ----loadLibsAndSource, include=FALSE------------------------------------

reqdpkgs <- c("lattice", "latticeExtra", "ggplot2", "reshape2",
              "plyr",  "lubridate", "Hmisc", "corrplot",
              "rpart", "rpart.plot", "rattle", "RWeka", "rattle",
              "caret", "gbm", "randomForest")
lapply(reqdpkgs, library, character.only=TRUE)



## ----zecn----------------------------------------------------------------
nzecnutransformed <- function(xs, ecs, name) {
    xs.nz <- as.numeric(xs != 0)
    xs.nz[is.na(xs.nz)] <- 1
    xs.ec <- as.numeric(xs %in% ecs)
    xs.ec[is.na(xs.ec)] <- 0
    xs.nu <- xs
    xs.nu[ xs %in% ecs] <- NA
    xs.nu[ xs == 0 ] <- NA
    df <- data.frame(nu=xs.nu, nz=xs.nz, ec=xs.ec)
    names(df) <- paste(name, names(df), sep=".")
    df
}


## ----threeclusters-------------------------------------------------------

preProcess.threeClusters <- function(thresh = 0.95,
                                     imputeMethod="medianImpute") {

    if ( !exists("cluents") ) load("../data/cluents.Rdata")
    if ( !exists("eclists") ) load("../data/eclists.Rdata")
    load("../data/train_labeled.Rdata")
    load("../data/test_labeled.Rdata")
    train.and.test <- rbind(train[, -1932], test) # target is at 1932
    N.train <- nrow(train)
    N.test <- nrow(test)

    preds.num <- setdiff(cluents$predictor, c("X", "ID"))
    var.preds.num <- sapply(preds.num, function(p) var(train[, p], na.rm=TRUE))
    names(var.preds.num) <- preds.num
    preds.num <- preds.num[ var.preds.num > 0]
    preds.num.X1 <- Filter(function(p) var.preds.num[p] > 0,
                           with(cluents, predictor[clufp == 1]))
    preds.num.X2 <- Filter(function(p) var.preds.num[p] > 0,
                           with(cluents, predictor[clufp == 2]))
    preds.num.X3 <- Filter(function(p) var.preds.num[p] > 0,
                           with(cluents, predictor[clufp == 3]))

    train.and.test.X1 <- train.and.test[, preds.num.X1]
    train.and.test.X2 <- train.and.test[, preds.num.X2]
    train.and.test.X3 <- train.and.test[, preds.num.X3]

    train.and.test.X1.nzecnu <- do.call(cbind,
                                        lapply(preds.num.X1,
                                               function(p) {
                                                   xs <- train.and.test[, p]
                                                   ecs <- eclists[[p]]
                                                   nzecnutransformed(xs, ecs, p)
                                               })
                                        )

    train.X1.nzecnu <- train.and.test.X1.nzecnu[1:N.train,]
    train.X2 <- train.and.test.X2[1:N.train,]
    train.X3 <- train.and.test.X3[1:N.train,]
    train.X1.nu <- train.X1.nzecnu[, grepl(pattern=".nu",
                                           x=names(train.X1.nzecnu), fixed=TRUE)]
    train.X1.ec <- train.X1.nzecnu[, grepl(pattern=".ec",
                                           x=names(train.X1.nzecnu), fixed=TRUE)]
    train.X1.nz <- train.X1.nzecnu[, grepl(pattern=".nz",
                                           x=names(train.X1.nzecnu), fixed=TRUE)]

    test.X1.nzecnu <- train.and.test.X1.nzecnu[N.train + (1:N.test), ]
    test.X2 <- train.and.test.X2[N.train + (1:N.test), ]
    test.X3 <- train.and.test.X3[N.train + (1:N.test), ]
    test.X1.nu <- test.X1.nzecnu[, grepl(pattern=".nu",
                                         x=names(test.X1.nzecnu), fixed=TRUE)]
    test.X1.ec <- test.X1.nzecnu[, grepl(pattern=".ec",
                                         x=names(test.X1.nzecnu), fixed=TRUE)]
    test.X1.nz <- test.X1.nzecnu[, grepl(pattern=".nz",
                                         x=names(test.X1.nzecnu), fixed=TRUE)]




    ## ----boxcox1-------------------------------------------------------------

    bcx.X1.nu <- lapply(names(train.X1.nu),
                        function(p) {
                            print(paste("box-cox", p))
                            BoxCoxTrans(train.X1.nu[, p], na.rm=TRUE)
                        })
    names(bcx.X1.nu) <- names(train.X1.nu)

    train.X1.nu.bcxd <- data.frame(lapply(names(train.X1.nu),
                                          function(p) predict(bcx.X1.nu[[p]],
                                                              train.X1.nu[,p])),
                                   stringsAsFactors=FALSE)
    names(train.X1.nu.bcxd) <- paste(names(train.X1.nu), "bc", sep=".")

    test.X1.nu.bcxd <- data.frame(lapply(names(test.X1.nu),
                                         function(p) predict(bcx.X1.nu[[p]],
                                                             test.X1.nu[,p])),
                                  stringsAsFactors=FALSE)
    names(test.X1.nu.bcxd) <- paste(names(test.X1.nu), "bc", sep=".")


    ## ----boxcox2-------------------------------------------------------------

    bcx.X2 <- lapply(names(train.X2),
                     function(p) {
                         print(paste("box-cox", p))
                         BoxCoxTrans(train.X2[, p], na.rm=TRUE)
                     })
    names(bcx.X2) <- names(train.X2)

    train.X2.bcxd <- data.frame(lapply(names(train.X2),
                                       function(p) predict(bcx.X2[[p]], train.X2[,p])),
                                stringsAsFactors=FALSE)
    names(train.X2.bcxd) <- paste(names(train.X2), "bc", sep=".")

    test.X2.bcxd <- data.frame(lapply(names(test.X2),
                                      function(p) predict(bcx.X2[[p]], test.X2[,p])),
                               stringsAsFactors=FALSE)
    names(test.X2.bcxd) <- paste(names(test.X2), "bc", sep=".")


    ## ----preprocess----------------------------------------------------------

    var.X1.nu.bcxd <- sapply(train.X1.nu.bcxd, function(xs) var(xs, na.rm=TRUE))
    nac.X1.nu.bcxd <- sapply(train.X1.nu.bcxd, function(xs) sum(is.na(xs)))

    train.X1.nu.bcxd.varnar <- train.X1.nu.bcxd[, var.X1.nu.bcxd > 1.e-5 &
                                                    nac.X1.nu.bcxd < N.train * 0.735]

    test.X1.nu.bcxd.varnar <- test.X1.nu.bcxd[, var.X1.nu.bcxd > 1.e-5 &
                                                  nac.X1.nu.bcxd < N.test * 0.735]

    var.X2.bcxd <- sapply(train.X2.bcxd, function(xs) var(xs, na.rm=TRUE))
    nac.X2.bcxd <- sapply(train.X2.bcxd, function(xs) sum(is.na(xs)))

    train.X2.bcxd.varnar <- train.X2.bcxd[, var.X2.bcxd > 1.e-5 &
                                              nac.X2.bcxd < N.train * 1]
    test.X2.bcxd.varnar <- test.X2.bcxd[, var.X2.bcxd > 1.e-5 &
                                            nac.X2.bcxd < N.test * 1]


    var.X3 <- sapply(train.X3, function(xs) var(xs, na.rm=TRUE))
    nac.X3 <- sapply(train.X3, function(xs) sum(is.na(xs)))

    train.X3.varnar <- train.X3[, var.X3 > 1.e-5 &
                                    nac.X3 < N.train * 1]
    test.X3.varnar <- test.X3[, var.X3 > 1.e-5 &
                                  nac.X3 < N.test * 1]

    var.X1.ec <- sapply(train.X1.ec, function(xs) var(xs, na.rm=TRUE))
    nac.X1.ec <- sapply(train.X1.ec, function(xs) sum(is.na(xs)))

    train.X1.ec.varnar <- train.X1.ec[ , var.X1.ec > 1.e-5 &
                                          nac.X1.ec < N.train * 1]
    test.X1.ec.varnar <- test.X1.ec[ , var.X1.ec > 1.e-5 &
                                        nac.X1.ec < N.test * 1]

    var.X1.nz <- sapply(train.X1.nz, function(xs) var(xs, na.rm = TRUE))
    nac.X1.nz <- sapply(train.X1.nz, function(xs) sum(is.na(xs)))

    train.X1.nz.varnar <- train.X1.nz[, var.X1.nz > 1.e-5 &
                                          nac.X1.nz < N.train * 1]
    test.X1.nz.varnar <- test.X1.nz[, var.X1.nz > 1.e-5 &
                                        nac.X1.nz < N.test * 1]

    pp.X1.nu <- preProcess(train.X1.nu.bcxd.varnar,
                           method=c("center", "scale", methodImpute, "pca"),
                           thresh = thresh)
    pp.X2 <- preProcess(train.X2.bcxd.varnar,
                        method=c("center", "scale", methodImpute, "pca"),
                        thresh = thresh)
    pp.X3 <- preProcess(train.X3.varnar,
                        method=c(methodImpute, "pca"),
                        thresh = thresh)
    pp.X1.ec <- preProcess(train.X1.ec.varnar,
                           method=c(methodImpute, "pca"),
                           thresh = thresh)
    pp.X1.nz <- preProcess(train.X1.nz.varnar,
                           method=c(methodImpute, "pca"),
                           thresh = thresh)

    pp.objs <- list(
        X1.nu=pp.X1.nu,
        X1.ec=pp.X1.ec,
        X1.nz=pp.X1.nz,
        X2=pp.X2,
        X3=pp.X3 )


    ## ----preprocesseddata----------------------------------------------------
    train.X1.nu.pp <- predict(pp.X1.nu, newdata=train.X1.nu.bcxd.varnar)
    names(train.X1.nu.pp) <- paste("X1.nu.pp", names(train.X1.nu.pp), sep=".")
    train.X1.ec.pp <- predict(pp.X1.ec, newdata=train.X1.ec.varnar)
    names(train.X1.ec.pp) <- paste("X1.ec.pp", names(train.X1.ec.pp), sep=".")
    train.X1.nz.pp <- predict(pp.X1.nz, newdata=train.X1.nz.varnar)
    names(train.X1.nz.pp) <- paste("X1.nz.pp", names(train.X1.nz.pp), sep=".")
    train.X2.pp <- predict(pp.X2, newdata=train.X2.bcxd.varnar)
    names(train.X2.pp) <- paste("X2.pp", names(train.X2.pp), sep=".")
    train.X3.pp <- predict(pp.X3, newdata=train.X3.varnar)
    names(train.X3.pp) <- paste("X3.pp", names(train.X3.pp), sep=".")

    test.X1.nu.pp <- predict(pp.X1.nu, newdata=test.X1.nu.bcxd.varnar)
    names(test.X1.nu.pp) <- paste("X1.nu.pp", names(test.X1.nu.pp), sep=".")
    test.X1.ec.pp <- predict(pp.X1.ec, newdata=test.X1.ec.varnar)
    names(test.X1.ec.pp) <- paste("X1.ec.pp", names(test.X1.ec.pp), sep=".")
    test.X1.nz.pp <- predict(pp.X1.nz, newdata=test.X1.nz.varnar)
    names(test.X1.nz.pp) <- paste("X1.nz.pp", names(test.X1.nz.pp), sep=".")
    test.X2.pp <- predict(pp.X2, newdata=test.X2.bcxd.varnar)
    names(test.X2.pp) <- paste("X2.pp", names(test.X2.pp), sep=".")
    test.X3.pp<- predict(pp.X3, newdata=test.X3.varnar)
    names(test.X3.pp) <- paste("X3.pp", names(test.X3.pp), sep=".")


    train.pp <- cbind(train.X1.nu.pp,
                      train.X1.nz.pp,
                      train.X1.ec.pp,
                      train.X2.pp,
                      train.X3.pp)
    test.pp <- cbind(test.X1.nu.pp,
                     test.X1.nz.pp,
                     test.X1.ec.pp,
                     test.X2.pp,
                     test.X3.pp)
    list(ppobject = pp.objs,
         train = train.pp,
         test = test.pp)

}






