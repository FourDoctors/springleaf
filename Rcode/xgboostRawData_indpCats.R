##we will use the raw data for xgboost
##prepare the data
##and use independent categories for the factor variables


## source among other things

source("../Rcode/indpCats.R")

##filter parameters
min.entropy <- 0.25
min.kld <- 0.25

print("read train and test")
if(!exists("train")) load("../data/train_labeled.Rdata")
if(!exists("test")) load("../data/test_labeled.Rdata")

preds.all <- Filter(function(p) !p %in% c('ID', 'X', 'target'), names(train))
train.and.test <- rbind(train[, preds.all], test[, preds.all])

N.train <- nrow(train)
N.test <- nrow(test)

preds.num <- preds.all[ grepl(pattern='numeric', x=preds.all)]

train.and.test.num <- train.and.test[, preds.num]
if(!exists("train.and.test.non_num.treated")) {
    load("../data/trainTest/trainTestNonNumTreated.Rdata")
}

print("non num")
train.non_num <-  train.and.test.non_num.treated[1:N.train, ]
train.num <- train.and.test.num[1:N.train,]
test.non_num <- train.and.test.non_num.treated[N.train + (1:N.test), ]
test.num <- train.and.test.num[N.train + (1:N.test),]

logvars <- grepl(pattern="logical", x=names(train.non_num))
train.logical <- data.frame(lapply(train.non_num[, logvars],
                                   function(xs) as.numeric(xs)))

train.categorical <- train.non_num[, !logvars]
train.categorical$hasjob <- as.numeric(train.categorical$hasjob)
test.logical <- data.frame(lapply(test.non_num[, logvars],
                                  function(xs) as.numeric(xs)))
test.categorical <- test.non_num[, !logvars]
test.categorical$hasjob <- as.numeric(test.categorical$hasjob)


print("combine")
train.xgb <- cbind(train.num,
                   train.categorical,
                   train.logical)
##make independent categories for categorical variables

cat.vars <- sapply(train.xgb, function(xs) class(xs) == 'factor' | class(xs) == 'character')

train.xgb.cats <- train.xgb[, catvars]
train.xgb.num <- cbind(train.xgb[, !cat.vars])
train.xgb.cats <- as.data.frame(
    do.call(cbind,
            lapply(names(train.xgb.cats),
                   function(p) independentCategories(p, train.xgb.cats))
            )
    )
train.xgb <- cbind(train.xgb.num, train.xgb.cats)
train.xgb[is.na(train.xgb)] <- -99999999999999

test.xgb <- cbind(test.num,
                  test.categorical,
                  test.logical)
test.xgb.cats <- test.xgb[, catvars]
test.xgb.num <- cbind(test.xgb[, !cat.vars])
test.xgb.cats <- as.data.frame(
    do.call(cbind,
            lapply(names(test.xgb.cats),
                   function(p) independentCategories(p, test.xgb.cats))
            )
    )
test.xgb <- cbind(test.xgb.num, test.xgb.cats)
test.xgb[is.na(test.xgb)] <- -99999999999999


##filter the columns
print("filter entropy")
constant.filter <- sapply(train.xgb, function(xs) length(unique(xs)) > 1)

train.xgb <- train.xgb[, constant.filter]
test.xgb <- test.xgb[, constant.filter]


entropy.filter <- sapply(train.xgb, function(xs) entropy(xs) > min.entropy)

print("filter kld")
if (!exists("kld.xgbs.rawdata") ) {
    kld.xgbs <- foreach(p=names(train.xgb), .combine = 'c') %do% {
        xs <- train.xgb[, p]
        kld(xs[train$target == 0], xs[train$target == 1], asymm = FALSE)
    }
    save(kld.xgbs, file = "../data/kld.xgb.num_raw.non_num.Rdata")
}

kld.filter <- kld.xgbs > min.kld

train.xgb <- train.xgb[, entropy.filter & kld.filter]
test.xgb <- test.xgb[, entropy.filter & kld.filter]
xgb.tuned <- tunexgb(train.xgb, train$target)

save(xgb.tuned,
     file = paste("../data/models",
         paste("xgb.tuned", "num_raw", "non_num",
               "ent", min.entropy, "kld", min.kld, sep="."),
         sep="/")
     )

