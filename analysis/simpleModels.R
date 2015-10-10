
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
              "rpart", "rpart.plot", "rattle", "RWeka",
              "caret", "gbm", "randomForest")
lapply(reqdpkgs, library, character.only=TRUE)



## ----loaddata------------------------------------------------------------

train <- load("../data/train_labeled.Rdata")
test <- load("../data/test_labeled.Rdata")



## ----predictorlists------------------------------------------------------

V <- ncol(train)
predictors <- names(train)[-V] # the last variable is the outcome, target
predictor_types <- sapply(predictors, function(p) class(train[, p]))
predictors_chr <- predictors[predictor_types == 'character']
predictors_num <- predictors[predictor_types == 'integer' |
                                 predictor_types == 'numeric']
predictors_log <- predictors[predictor_types == 'logical']



## ----baseline------------------------------------------------------------

baseline <- mean(train$target)
print( paste("Fraction of ones in the target", round(baseline,2)))



## ----baseline2-----------------------------------------------------------

print(paste("we have set a null model of target = 0 for any input"))
print(paste("this null model has a baseline error of ", baseline))



## ----loadscripts---------------------------------------------------------

source("../Rcode/errorCodes.R")
source("../Rcode/correlations.R")
source("../Rcode/numToCat.R")
source("../Rcode/huffman.r")
source("../Rcode/discreteEncoded.R")



## ----ovl-----------------------------------------------------------------

ovl_10_00001<- lapply(predictors,
                 function(var) {
                     print("outliers for ")
                     ol <- dpoutlyers(train[, var], offset=10, rarity=1e-5)
                     print(var)
                     print(ol)
                 }
                 )

names(ovl_10_00001) <- predictors



## ----removeec------------------------------------------------------------

train.noec <- train
test.noec <- test
for (p in names(ovl_10_00001)) {
         x <- train[,p]
         x[ !(x %in% ovl_10_00001[[p]]) ] <- NA
         train.noec[,p] <- x
         y <- test[,p]
         y[ !(y %in% ovl_10_00001[[p]]) ] <- NA
         test.noec[,p] <- y
     }



## ----cutsummaryplot------------------------------------------------------

n <- 3
hc.dp <- hclust(dist(scale(var.dpnz), method='euclidean'), method='ward')
hc.fp <- hclust(dist(scale(var.fpnz), method='euclidean'), method='ward')
clucut.dp <- cutree(hc.dp, k=n)
clucut.fp <- cutree(hc.fp, k=n)
apnd <- plotFPsummaries(summary.cluster.cut(clucut.dp,
                                            data=var.fpnormed)
                        )
apnf <- plotFPsummaries(summary.cluster.cut(clucut.fp,
                                            data=var.fpnormed)
                        )
print(apnd)
pdf("figures/cutsummaryDecimalPrintsK3_woZero.pdf")
print(apnd)
dev.off()
pdf("figures/cutsummaryFingerPrintsK3_woZero.pdf")
print(apnf)
dev.off()





## ----cluents-------------------------------------------------------------

pnums <- row.names(var.fpnz)
cluents <- data.frame(
    list(
        predictor = pnums,
        clufp = clucut.fp,
        cludp = clucut.dp,
        entropy.noz = var.fpnormed[pnums, 'ent.noz'],
        entropy = var.fpnormed[pnums, 'ent'],
        distinct = var.fpnormed[pnums, 'dis'],
        num.zeros = sapply(pnums, function(p) sum(train[,p] == 0, na.rm=TRUE)),
        num.ecs = sapply(pnums, function(p) {
                             xs <- train[,p]
                             sum( xs %in% ovl_10_00001[[p]], na.rm=TRUE)
                         })
        ),
    stringsAsFactors=FALSE
    )


cluents.ml <- melt(cluents,
                   id.vars=c("predictor", "clufp", "cludp")
                   )

histogram(~ value | as.factor(clufp),
          data=subset(cluents.ml, variable=='entropy.noz'),
          main='entropy no zeros')

histogram(~ value | as.factor(clufp),
          data=subset(cluents.ml, variable=='distinct'),
          main='distinct')

histogram(~ value | as.factor(clufp),
          data=subset(cluents.ml, variable=='num.zeros'),
          main='number of zeros')

histogram(~ value | as.factor(clufp),
          data=subset(cluents.ml, variable=='num.ecs'),
          main='number of error codes')



## ----wherezero-----------------------------------------------------------

not.zero <- function(xs, fill.na=TRUE) {
    ys <- xs != 0
    ys[is.na(ys)] <- fill.na
    ys
}



## ----copydata------------------------------------------------------------

train.model <- train
test.model <- test



## ----setnumdatatypes-----------------------------------------------------

elarge <- 4
max.loss <- 1
d <- dist(scale(var.fpnz), method='euclidean')
clu <- hclust(d, method="ward")
clucut <- cutree(clu, k=3)
preds.num <- setdiff(cluents$predictor, c("X", "ID"))
preds.num.X1 <- with(cluents, predictor[clufp == 1])
preds.num.X2 <- with(cluents, predictor[clufp == 2])
preds.num.X3 <- with(cluents, predictor[clufp == 3])

preds.num.X3.cats <- with(cluents,
                         predictor[clufp == 3 &
                                       entropy.noz <= elarge]
                         )
preds.num.X3.cons <- with(cluents,
                          predictor[clufp == 3 &
                                        entropy.noz > elarge]
                          )

preds.num.nz <- paste(preds.num, 'nz', sep='.')
train.model.num.nz <- data.frame(lapply(train[, preds.num],
                                          function(xs) not.zero(xs)
                                          ),
                                   stringsAsFactors=FALSE
                                   )
names(train.model.num.nz) <- paste(preds.num, 'nz', sep='.')


train.model.num.ec <- data.frame(lapply(preds.num,
                                        function(p) {
                                            xs <- train[, p]
                                            ecs <- ovl_10_00001[[p]]
                                            cxs <- as.character(xs)
                                            cxs[ !(xs %in% ecs)] <- "nec"
                                            print(paste("ec var", p,
                                                        "number ecs",
                                                        length(ecs))
                                                  )
                                            as.factor(cxs)
                                        }
                                        )
                                 )
names(train.model.num.ec) <- paste(preds.num, "ec", sep=".")

train.model.num.X1.ls <- data.frame(
    lapply(preds.num.X1,
           function(p) {
               xs <- train[, p]
               xs[xs %in% ovl_10_00001[[p]]] <- NA
               xs[ xs <= 0] <- NA
               log10(1 + xs)
           }),
    stringsAsFactors = FALSE
    )
names(train.model.num.X1.ls) <- paste(preds.num.X1, 'ls', sep='.')

train.model.num.X2.ls <- data.frame(
    lapply(preds.num.X2,
           function(p) {
               xs <- train[, p]
               xs[xs %in% ovl_10_00001[[p]]] <- NA
               xs[xs <= 0] <- NA
               log10(1 + xs)
           }),
    stringsAsFactors = FALSE
    )
names(train.model.num.X2.ls) <- paste(preds.num.X2, 'ls', sep='.')

train.model.num.X3.cons.ls <- data.frame(
    lapply(preds.num.X3.cons,
           function(p) {
               xs <- train[, p]
               xs[xs %in% ovl_10_00001[[p]]] <- NA
               xs[xs <= 0] <- NA
               log10(1 + xs)
           }),
    stringsAsFactors = FALSE
    )
names(train.model.num.X3.cons.ls) <- paste(preds.num.X3.cons, 'ls', sep='.')

max.loss <- 0.2
train.model.num.X3.cats.hc <- data.frame(
    lapply(train.model[, preds.num.X3.cats],
           function(xs) {
               hxs <- huffman.encoded(xs, max.loss=max.loss)
               print(paste("encoding ",
                           "entropy",
                           entropy(xs), "-->", entropy(hxs)
                           )
                     )
               print(paste("encoding ",
                           "distinct",
                           length(unique(xs)), "-->", length(unique(hxs))
                           )
                     )
               print("----------------------------------")

               hxs
           })
    )
names(train.model.num.X3.cats.hc) <- paste(preds.num.X3.cats,
                                                 'hc', sep=".")



## ----savenumvardata------------------------------------------------------

train.model.num.treated <- cbind(
    train.model.num.X1.ls,
    train.model.num.X2.ls,
    train.model.num.X3.cons.ls,
    train.model.num.X3.cats.hc,
    train.model.num.nz,
    train.model.num.ec
    )



## ----modeldataandpreds---------------------------------------------------

preds.model.dates <- predictors_chr[ grepl(x=predictors_chr, pattern='date')]
preds.model.cats <- c('hrq', 'cbns', 'abcdefu',
                      'shrug_first', 'shrug_second', 'shrug_third',
                      'oru_first', 'oru_second', 'oru_third',
                      'eye', 'status', 'medium', 'idused')
preds.model.jobs <- c('job_first', 'job_second')
preds.model.locs <- c('state_first', 'state_second', 'city')
preds.model.logs <- predictors_log

## set dates from strings

train.model.date <- data.frame(
    lapply(train.model[, preds.model.dates],
           function(xs) ymd(xs))
    )

train.model.date.year <- data.frame(
    lapply(train.model.date,
           function(xs) {
               ys <- as.character(year(xs))
               ys[is.na(ys)] <- "unknown"
               as.factor(ys)
           })
    )
names(train.model.date.year) <- paste(preds.model.dates,
                                      'year', sep='.')

train.model.date.month <- data.frame(
    lapply(train.model.date,
           function(xs) {
               ys <- as.character(month(xs))
               ys[is.na(ys)] <- "unknown"
               as.factor(ys)
           })
    )
names(train.model.date.month) <- paste(preds.model.dates,
                                      'month', sep='.')

train.model.date.week <- data.frame(
    lapply(train.model.date,
           function(xs) {
               ys <- as.character(week(xs))
               ys[is.na(ys)] <- "unknown"
               as.factor(ys)
               #week(xs)
           })
    )
names(train.model.date.week) <- paste(preds.model.dates,
                                      'week', sep='.')
train.model.date.mday <- data.frame(
    lapply(train.model.date,
           function(xs) {
               ys <- as.character(mday(xs))
               ys[is.na(ys)] <- "unknown"
               as.factor(ys)
               #mday(xs)
           })
    )
names(train.model.date.mday) <- paste(preds.model.dates,
                                     'mday', sep='.')

train.model.date.yday <- data.frame(
    lapply(train.model.date,
           function(xs) {
               ys <- as.character(yday(xs))
               ys[is.na(ys)] <- "unknown"
               as.factor(ys)
               #yday(xs)
           })
    )
names(train.model.date.yday) <- paste(preds.model.dates,
                                     'yday', sep='.')
train.model.date.wday <- data.frame(
    lapply(train.model.date,
           function(xs) {
               ys <- as.character(wday(xs))
               ys[is.na(ys)] <- "unknown"
               as.factor(ys)
           })
    )
names(train.model.date.wday) <- paste(preds.model.dates,
                                     'wday', sep='.')

train.model.cat <- data.frame(
    lapply(train[, preds.model.cats],
           function(xs) {
               xs[is.na(xs)] <- 'unknown'
               as.factor(xs)
           })
    )

train.model.log <- train[, preds.model.logs]
train.model.loc <- train[, preds.model.locs]
train.model.job <- train[, preds.model.jobs]

train.model.state <- data.frame(
    lapply(train[, c('state_first', 'state_second')],
           function(xs) as.factor(xs)
           )
    )

train.model.hasjob <- data.frame(
    hasjob = train$job_first != "-1" |
        train$job_second != "-1"
    )

train.model.non_num.treated <- cbind(
    train.model.date.year,
    train.model.date.month,
    train.model.date.week,
    train.model.date.mday,
    train.model.date.yday,
    train.model.date.wday,
    train.model.cat,
    train.model.log,
    train.model.state,
    train.model.hasjob
    )



## ----varfilters----------------------------------------------------------

filteredVariables <- function(data,
                              min.entropy=NULL,
                              max.na.fraction=NULL,
                              min.correlation=NULL,
                              min.kld=NULL,
                              target=NULL,
                              target.label0=0,
                              target.label1=1) {
    ##first filters will be on entropy and NAs
    predictors <- names(data)
    filter.entropy <- if(!is.null(min.entropy)) {
        sapply(predictors,
               function(p) entropy(data[, p]) >= min.entropy
               )
    } else {
        rep(TRUE, length(predictors))
    }
    filter.na <- if(!is.null(max.na.fraction)) {
        sapply(predictors,
               function(p) mean(is.na(data[, p])) <= max.na.fraction
               )
    } else {
        rep(TRUE, length(predictors))
    }
    filter.kld <- if(!is.null(min.kld)) {
        sapply(predictors,
               function(p) {
                   xs <- data[, p]
                   xs.0 <- xs[target==target.label0]
                   xs.1 <- xs[target==target.label1]
                   xs.0 <- xs.0[!is.na(xs.0)]
                   xs.1 <- xs.1[!is.na(xs.1)]
                   kld(xs.0, xs.1, asymm=FALSE) >= min.kld
               })
    } else {
        rep(TRUE, length(predictors))
    }
    filter.correlation <- rep(TRUE, length(predictors))

    predictors[filter.entropy &
                   filter.na &
                       filter.kld &
                           filter.correlation]
}

### Value in Zeros, and error-codes

 However, for the KLD we did
not remove these. In addition to removing them we should also look at
the statistics if we keep them, and remove all other values. What we
mean will be clearer when we discuss specific cases.

Consider a numerical variable which has many 0s. What is the
information in its value of 0? We can measure that if we compute the
distribution of the target over the subsets where the variable takes
the value of 0 or not.


## ----zeroornotnumeric----------------------------------------------------

N <- nrow(train)
q <- 0.2
pzs <- Filter(function(p) {
                  nz  <- sum(train[,p] == 0, na.rm=TRUE)/N
                  no <- sum(train[,p] != 0, na.rm=TRUE)/N
                  nz >= q & nz < 1 - q
              }, predictors_num
              )

prob.1.pzs <- do.call(rbind, lapply(pzs, function(p) {
                                        df <- train[, c('target', p)]
                                        names(df) <- c('target', 'varval')
                                        df$varval <- df$varval != 0
                                        df$varval[ is.na(df$varval) ] <- TRUE
                                        tdf <- table(df)
                                        print(tdf)
                                        t(t(tdf)/colSums(tdf))[2,]
                                    })
                      )
colnames(prob.1.pzs) <- c("V0", "V1")
prob.1.pzs <- as.data.frame(prob.1.pzs)
prob.1.pzs$variable <- paste(pzs, "nz", sep=".")
prob.1.pzs$odds.V1 <- with(prob.1.pzs, V1/(1-V1))
prob.1.pzs$odds.V0 <- with(prob.1.pzs, V0/(1-V0))
prob.1.pzs <- prob.1.pzs[ order(prob.1.pzs$odds.V0,
                                prob.1.pzs$odds.V1,
                                decreasing=TRUE),]




## ----dataset0ornot-------------------------------------------------------

train.znz <- train[, pzs] != 0
train.znz[ is.na(train.znz) ] <- TRUE
train.znz <- as.data.frame(train.znz)
names(train.znz) <- sapply(names(train.znz), function(p) paste(p, 'nz', sep="."))

train.znz$target <- train$target

fit.rpart.znz <- rpart(target ~ ., data=train.znz,
                       control=rpart.control(depth=100, cp=0.001, minsplit=10))


prd <- predict(fit.rpart.znz, newdata=train, method="response")
print("our training auc for the roc", roc.auc(prd, train$target)



## ----includeec-----------------------------------------------------------

q <- 0.1
pecs <- Filter(function(p) {
                   xs <- train[, p]
                   ecs <- ovl_10_00001[[p]]
                   sum(xs %in% ecs, na.rm=TRUE) >= q
               }, predictors_num
               )
train.ec <- data.frame(lapply(pecs,
                              function(p) {
                                  xs <- train[,p]
                                  ecs <- ovl_10_00001[[p]]
                                  isec <- xs %in% ecs
                                  xs[!isec] <- 0
                                  as.factor(xs)
                              })
                       )
names(train.ec ) <- paste(pecs, "ec", sep=".")



## ----treeWithEC----------------------------------------------------------

train.ec$target <- train$target
fit.rpart.ec <- rpart(target ~ ., data=train.ec,
                      control=rpart.control(cp=0.001, minsplit=10, depth=100)
                      )



## ----yeartovarents-------------------------------------------------------

var.entropies.year <- predictor.entropies(preds.model.dates.year,
                                           data=train.model)

var.entropies <- rbind(var.entropies, var.entropies.year[, names(var.entropies)])

kld.allvars <- kld.vars(predictors, data=train)
kld.years <- kld.vars(preds.model.dates.year, data=train.model)
kld.allvars <- rbind(kld.allvars, kld.years)
kld.allvars <- with(kld.allvars, kld.allvars[ order(kld/entropy, decreasing=TRUE), ])



## ----dropvars------------------------------------------------------------

for (p in preds.model.dates) {
    train.model[,p] <- as.factor(train.model[,p])
}

preds.model <- setdiff(
    unique(c(preds.model.num.cons, preds.model.num.cats,
             preds.model.logs, preds.model.cats,
             preds.model.dates.year)),
    c("X", "ID")
    )

keep.preds.model <- sapply(preds.model,
                           function(p){
                              k <- ( var.entropies[p, 'entropy'] > 0.1 &
                                        var.entropies[p, 'nacount'] < 0.5 * nrow(train.model) )
                              k
                           })


fit.rpart <- rpart(target ~ .,
                   data = train.model[, c(preds.model[keep.preds.model], "target")]
                   )



## ----highrelativekld-----------------------------------------------------

preds.model.1 <- with(kld.allvars, variable[ entropy > 0.5 &
                                              notna1 + notna0 > 1.4e5][1:200])


preds.model.2 <- Filter(function(p) kld.allvars[p, 'distinct'] > 1000 & kld.a,
                        preds.model.num
                        )

preds.model <- union(preds.model.1, preds.model.2)

fit.rpart.1 <- rpart(target ~ .,
                     data = train.model[, c(preds.model, "target")],
                     )



## ----highkld-------------------------------------------------------------

kld.allvars <- kld.allvars[order(kld.allvars$kld, decreasing=TRUE), ]
preds.model <- kld.allvars$variable[1:30]

fit.rpart.2 <- rpart(target ~ .,
                     data=train.model[, c(preds.model, "target")],
                     control=rpart.control(
                         minsplit=5,
                         cp=0.01,
                         maxdepth=10
                         )
                     )



## ----choosebynumberofdistinct--------------------------------------------

naentfilter <- function(data, notnanum = 1e5, e = 0.1){
    with(data, (notna1 + notna0 > notnanum) & entropy > e)
}
kldfilter <- function(data, k=0.01){
    with(data, kld > k)
}


preds.model.n <- with(kld.allvars,
                      variable[naentfilter(kld.allvars) &
                                   kldfilter(kld.allvars)]
                      )
preds.model.2 <- with(kld.allvars[preds.model.n, ],
                      variable[distinct == 2])
preds.model.3 <- with(kld.allvars[preds.model.n, ],
                      variable[distinct == 3])
preds.model.4 <- with(kld.allvars[preds.model.n, ],
                      variable[distinct == 4])
preds.model.5 <- with(kld.allvars[preds.model.n, ],
                      variable[distinct == 5])

preds.model <- c(preds.model.1, preds.model.2,
                 preds.model.3, preds.model.4,
                 preds.model.5)

fit.rpart <- rpart(target ~ .,
                   data = train.model[, c('target', preds.model)]
                   )

prp(fit.rpart, varlen=0)



## ----logisiticWith2ValVariables------------------------------------------

vals2numericVars <- with(num_vals_num_vars, label[num_vals == 2])
m.numeric2 <- glm(target ~ .,
                  data = train[, c(vals2numericVars, 'target')],
                  family='binomial'
                  )
tp.numeric2 <- predict(m.numeric2, newdata=train, type='response')
err.numeric2 <- sum( (tp.numeric2 > 0.5) != train$target, na.rm=TRUE)/nrow(train)

print("the logistic model with only numeric variables that take 2 values")
print(paste("training error :  ", round(err.numeric2,2)))
print(paste("compared to baseline: ", round(err.numeric2/baseline, 2)))


## ----logisiticWith3ValVariables------------------------------------------

vals3numericVars <- with(num_vals_num_vars, label[num_vals == 3])
m.numeric3 <- glm(target ~ .,
                  data = train.ecfixed[, c(vals3numericVars, 'target')],
                  family='binomial'
                  )
tp.numeric3 <- predict(m.numeric3, newdata=train.ecfixed, type='response')
err.numeric3 <- sum( (tp.numeric3 > 0.5) != train$target, na.rm=TRUE)/nrow(train)

print("the logistic model with only numeric variables that take 3 values")
print(paste("training error :  ", round(err.numeric3,2)))
print(paste("compared to baseline: ", round(err.numeric3/baseline, 2)))



## ----logisticWithNvalsVars-----------------------------------------------

for(n in 2:max(var.entropies$distinct_vals)) {
    predictors <- Filter(
        function(p) {
            var.entropies[p, 'entropy'] > 0.1 &
                var.entropies[p, 'nacount'] < 1000
        },
        with(var.entropies, variable[distinct_vals == n &
                                         (datatype == 'numeric' |
                                              datatype == 'integer')])
        )
    print(paste("to try predictors with ", n , "distinct values"))
    print(paste("total number of such predictors ", length(predictors)))
    if(length(predictors) > 0) {
        m <- glm(target ~ .,
                 data = train[, c(predictors, 'target')],
                 family='binomial'
                 )
        tp <- predict(m, newdata=train, type='response')
        err <- sum( (tp > 0.5) != train$target, na.rm=TRUE)/nrow(train)
        print(paste(" logistic model with only num vars that take ", n, " values"))
        print(paste("total number of variables considered", length(predictors)))
        print(paste("training error :  ", round(err,2)))
        print(paste("compared to baseline: ", round(err/baseline, 2)))
        print("|||||||||||||||||||||||||||||||||||||||")
    }
}



## ----cors1---------------------------------------------------------------

cors1 <- cor(train[, num_vals_num_vars$label[127:136]], use="pairwise.complete.obs")


