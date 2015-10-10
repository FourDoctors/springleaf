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
              "caret", "gbm", "randomForest",
              "ROCR")
lapply(reqdpkgs, library, character.only=TRUE)

source(paste(rcodepath, "roc.R", sep=""))
source(paste(rcodepath, "correlations.R", sep=""))
source(paste(rcodepath, "numtoCat.R", sep=""))
source(paste(rcodepath, "discreteEncoded.R", sep=""))
source(paste(rcodepath, "huffman.r", sep=""))xs


## ----encodedNums---------------------------------------------------------

elosses <- c(1/8, 1/4, 1/2, 3/4)
fits.rpart.eloss.hc.num.X3 <- lapply(elosses,
                              function(max.loss) {
                                  data <- data.frame(
                                      lapply(train.model[, preds.num.X3.cats],
                                             function(xs) {
                                                 hxs <- huffman.encoded(xs,
                                                                        max.loss=max.loss)
                                                 print(paste("encoding for max-loss",
                                                             max.loss,
                                                             "entropy",
                                                             entropy(xs), "-->", entropy(hxs)
                                                             )
                                                       )
                                                 print(paste("encoding ",
                                                             "distinct",
                                                             length(unique(xs)), "-->",
                                                             length(unique(hxs))
                                                             )
                                                       )
                                                 print("----------------------------------")

                                                 hxs
                                             })
                                      )
                                  save(data, file=paste(
                                                 paste("../data/trainModelNumX3catsEloss",
                                                 max.loss, sep="-"), "Rdata", sep=".")
                                       )
                                  fit <- rpart(target ~ .,
                                               data = cbind(data,
                                                   data.frame(target=train$target)),
                                               control=rpart.control(cp=0.001,
                                                   depth=100, minsplit=10)
                                               )
                                  save(data, file=paste( paste("../data/fitNumX3catsEloss",
                                                 max.loss, sep="-"), "Rdata", sep=".")
                                       )
                                  fit
                              })

auc.elosses <- sapply(fits.rpart.eloss.hc.num.X3,
                      function(fit) roc.auc(predict(fit), train$target)
                      )


print("effect of lossy encoding of discrete numerical variables")
print(data.frame(
    entropy.loss = elosses,
    auc.rpart = auc.elosses
    ))



## ----hcmodeltrees--------------------------------------------------------

pdf(file="figures/treeEloss-0.125.pdf")
print(fancyRpartPlot(fits.rpart.eloss.hc.num.X3[[1]]))
dev.off()

pdf(file="figures/treeEloss-0.250.pdf")
print(fancyRpartPlot(fits.rpart.eloss.hc.num.X3[[2]]))
dev.off()

pdf(file="figures/treeEloss-0.500.pdf")
print(fancyRpartPlot(fits.rpart.eloss.hc.num.X3[[3]]))
dev.off()

pdf(file="figures/treeEloss-0.750.pdf")
print(fancyRpartPlot(fits.rpart.eloss.hc.num.X3[[4]]))
dev.off()



prp(fits.rpart.eloss.hc.num.X3[[1]], varlen=0)


