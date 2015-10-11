## ----setup, include=FALSE------------------------------------------------

knitr::opts_chunk$set(cache=FALSE)
knitr::opts_chunk$set(echo=FALSE)
knitr::opts_chunk$set(include=TRUE)
knitr::opts_chunk$set(results="asis")
knitr::opts_chunk$set(fig.width=12)
knitr::opts_chunk$set(fig.height=14)
prjpath <- "~/work/learn/competitions/kaggle/springleaf/"
datapath <- paste(prjpath, "data/", sep="")
analpath <- paste(prjpath, "analysis/", sep="")
rcodepath <- paste(analpath, "Rcode/", sep="")
setwd(analpath)



## ----loadLibsAndSource, include=FALSE------------------------------------

reqdpkgs <- c("lattice", "latticeExtra", "ggplot2", "reshape2",
              "plyr",  "lubridate", "Hmisc", "corrplot",
              "rpart", "rpart.plot", "RWeka",
              "caret", "gbm", "randomForest")
lapply(reqdpkgs, library, character.only=TRUE)



## ----fitrpartnumnz-------------------------------------------------------

fit.rpart.num.nz <- rpart(target ~ .,
                          data = cbind(train.model.num.nz,
                              data.frame(target=train$target)),
                          control=rpart.control(cp=0.001, depth=100, minsplit=10)
                          )
print(roc.auc(predict(fit.rpart.num.nz), train$target))





## ----fitcat--------------------------------------------------------------

fit.rpart.non_num <- rpart(target ~ .,
                           data=cbind(train.model.non_num.treated,
                               data.frame(target=train$target)),
                           control=rpart.control(cp=0.001, depth=100, minsplit=10)
                           )
print(roc.auc(predict(fit.rpart.non_num), train$target))



## ----fitcatnz------------------------------------------------------------

fit.rpart.non_num.nz <- rpart(target ~ .,
                              data=cbind(train.model.non_num.treated,
                                  train.model.num.nz,
                                  data.frame(target=train$target)
                                  ),
                              control=rpart.control(cp=0.001, depth=100, minsplit=10)
                              )
print(roc.auc(predict(fit.rpart.non_num.nz), train$target))



## ----fitimpvars----------------------------------------------------------

vimp <- varImp(fit.rpart.non_num.nz)
vimp$variable <- row.names(vimp)
vimp <- vimp[order(vimp$Overall, decreasing=TRUE), ]
preds.non_num.nz.imp <- with(vimp, variable[Overall > 0])
print(paste("number of variables with position importance",
            length(preds.non_num.nz.imp))
      )

train.model.non_num.nz <- cbind(train.model.non_num.treated,
                                train.model.num.nz,)
control=rpart.control(cp=0.001, depth=100, minsplit=10)
fit.rpart.non_num.nz.imp <- rpart(target ~ .,
                                  data=cbind(
                                      train.model.non_num.nz[, preds.non_num.nz.imp],
                                      data.frame(target=train$target)
                                      ),
                                  control=control
                                  )

print(paste("when we use only the important variables we find an auc",
            roc.auc(predict(fit.rpart.non_num.nz.imp), train$target))
      )



## ----fitnonnumnzec-------------------------------------------------------

primp.nz <- preds.non_num.nz.imp[grepl(pattern='numeric',
                                        x = preds.non_num.nz.imp)
                                  ]
primp.non <- preds.non_num.nz.imp[!grepl(pattern='numeric',
                                        x = preds.non_num.nz.imp)
                                  ]
fit.rpart.non.nz.ec <- rpart(target ~ .,
                             data=cbind(train.model.num.nz[, primp.nz],
                                 train.model.non_num.treated[, primp.non],
                                 train.model.num.ec),
                             control=control
                             )



## ----varents-------------------------------------------------------------

var.entropies.X1 <- predictor.entropies(names(train.model.num.X1.ls),
                                        train.model.num.X1.ls, useNA=FALSE)
save(var.entropies.X1, file="../data/varEntropiesX1ls.Rdata")

var.entropies.X2 <- predictor.entropies(names(train.model.num.X2.ls),
                                        train.model.num.X2.ls, useNA=FALSE)
save(var.entropies.X2, file="../data/varEntropiesX2ls.Rdata")

var.entropies.X3.hc <- predictor.entropies(names(train.model.num.X3.cats.hc),
                                           train.model.num.X3.cats.hc, useNA=FALSE)
save(var.entropies.X3.hc, file="../data/varEntropiesX3hc.Rdata")

var.entropies.num.nz <- predictor.entropies(names(train.model.num.nz),
                                            train.model.num.nz, useNA=FALSE)
save(var.entropies.num.nz, file="../data/varEntropiesNumNZ.Rdata")

var.entropies.num.ec <- predictor.entropies(names(train.model.num.ec),
                                            train.model.num.ec, useNA=FALSE)
save(var.entropies.num.nz, file="../data/varEntropiesNumEC.Rdata")



var.entropies.non_num <- predictor.entropies(names(train.model.non_num.treated),
                                             train.model.non_num.treated, useNA=FALSE)
save(var.entropies.non_num, file="../data/varEntropiesNonNum.Rdata")



## ----impvarsfunc---------------------------------------------------------

importantVariables <- function(Xdata, Y,
                               control=rpart.control(depth=min(ncol(data), 100),
                                   minsplit=10,
                                   cp=0.001)
                               ) {
    fit <- rpart(target ~ .,
                 data=cbind(Xdata, data.frame(target=Y)),
                 control=control
                 )
    print(paste("fitted a model with auc",
                roc.auc(predict(fit), Y))
          )
    varimp <- varImp(fit)
    varimp$variable <- rownames(varimp)
    with(varimp, variable[Overall > 0])
}



## ----filterImpVars-------------------------------------------------------

emin <- 0.25
N <- nrow(train)
preds.fit.non_num <- importantVariables(
    train.model.non_num.treated[, with(var.entropies.non_num,
                                       variable[entropy > 0.25])],
    train$target
    )

preds.fit.num.X1 <- importantVariables(
    train.model.num.X1.ls[, with(var.entropies.X1,
                                 variable[entropy > 0.25 &
                                              nacount < N * 0.5]
                                 )
                         ],
    train$target
    )

preds.fit.num.X2 <- importantVariables(
    train.model.num.X2.ls[, with(var.entropies.X2,
                                 variable[entropy > 0.25 &
                                              nacount < N * 0.5]
                                 )
                          ],
    train$target
    )

preds.fit.num.X3.hc <- importantVariables(
    train.model.num.X3.cats.hc[, with(var.entropies.X3.hc,
                                 variable[entropy > 0.25 &
                                              nacount < N * 0.5]
                                 )
                          ],
    train$target
    )

preds.fit.num.ec <- importantVariables(
    train.model.num.ec[, with(var.entropies.num.ec,
                                 variable[entropy > 0.25 &
                                              nacount < N * 0.5]
                                 )
                          ],
    train$target
    )

preds.fit.num.nz <- importantVariables(
    train.model.num.nz[, with(var.entropies.num.nz,
                                 variable[entropy > 0.25 &
                                              nacount < N * 0.5]
                                 )
                          ],
    train$target
    )



## ----fullmodel-----------------------------------------------------------

preds.fit <- c(preds.fit.num.ec,
               preds.fit.num.nz,
               preds.fit.num.X1,
               preds.fit.num.X2,
               preds.fit.num.X3.hc,
               preds.fit.non_num
               )
train.fit <- cbind(train.model.non_num.treated,
                   train.model.num.treated)
train.fit <- train.fit[, preds.fit]

fit.rpart.all <- rpart(target ~ .,
                       data=cbind(train.fit,
                           data.frame(target=train$target)
                                  ),
                       control=control
                       )
save(fit.rpart.all, file="../data/fitRpartAll")



