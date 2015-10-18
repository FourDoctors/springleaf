##we will use the raw data for xgboost
##prepare the data

##filter parameters
min.entropy <- 0.25
min.kld <- 0.05

preds.all <- Filter(function(p) !p %in% c('ID', 'X', 'target'), names(train))
train.and.test <- rbind(train[, preds_all], test[, preds_all])

N.train <- nrow(train)
N.test <- nrow(test)

preds.num <- preds.all[ grepl(pattern='numeric', x=preds.all)]

train.and.test.num <- train.and.test[, preds.num]
if(!exists("train.and.test.non_num.treated")) {
    load("../data/trainTest/trainTestNonNumTreated.Rdata")
}

train.non_num <-  train.and.test.non_num.treated[1:N.train, ]
train.num <- train.and.test.num[1:N.train,]
test.non_num <- train.and.test.non_num.treated[N.train + (1:N.test), ]
test.num <- train.and.test.num[N.train + (1:N.test),]

##the following is equivalent to setting all non-zero values to 1 (incl. NA)
train.num[is.na(train.num)] <- 1
test.num[is.na(test.num)] <- 1
train.num[train.num != 0] <- 1
test.num[test.num!= 0] <- 1


logvars <- grepl(pattern="logical", x=names(train.non_num))
train.logical <- data.frame(lapply(train.non_num[, logvars],
                                   function(xs) as.numeric(xs)))

train.categorical <- train.non_num[, !logvars]
train.categorical$hasjob <- as.numeric(train.categorical$hasjob)
test.logical <- data.frame(lapply(test.non_num[, logvars],
                                  function(xs) as.numeric(xs)))
test.categorical <- test.non_num[, !logvars]
test.categorical$hasjob <- as.numeric(test.categorical$hasjob)

train.xgb <- cbind(train.num,
                   train.categorical,
                   train.logical)
train.xgb <- data.frame(lapply(train.xgb,
                               function(xs) {
                                   if (class(xs) == 'factor') as.integer(xs)
                                   else xs
                               })
                        )
train.xgb[is.na(train.xgb)] <- -99999999999999

test.xgb <- cbind(test.num,
                  test.categorical,
                  test.logical)
test.xgb <- data.frame(lapply(test.xgb,
                              function(xs) {
                                  if (class(xs) == 'factor') as.integer(xs)
                                  else xs
                              })
                       )
test.xgb[is.na(test.xgb)] <- -99999999999999


##filter the columns

constant.filter <- sapply(train.xgb, function(xs) length(unique(xs)) > 1)

train.xgb <- train.xgb[, constant.filter]
test.xgb <- test.xgb[, constant.filter]

entropy.filter <- sapply(train.xgb, function(xs) entropy(xs) > min.entropy)

if (!exists("kld.xgbs.rawdata") ) {
    kld.xgbs <- sapply(names(train.xgb),
                       function(p) {
                           xs <- train.xgb[, p]
                           k <- kld(xs[train$target == 0],
                                    xs[train$target == 1],
                                    asymm = FALSE)
                           print(paste(p, k))
                       })
    save(kld.xgbs, file = "../data/kld.xgb.nz.non_num.Rdata")
}

kld.filter <- kld.xgbs.rawdata > min.kld

train.xgb <- train.xgb[, constant.filter & entropy.filter & kld.filter]
test.xgb <- test.xgb[, constant.filter & entropy.filter & kld.filter]
xgb.tuned <- tunexgb(train.xgb, train.label)

save(xgb.tuned,
     file = paste("../data/models",
         paste("xgb.tuned", "nz", "non_num",
               "ent", min.entropy, "kld", min.kld, sep="."),
         sep="/")
     )

