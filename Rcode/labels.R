## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache=FALSE)
knitr::opts_chunk$set(echo=FALSE)
knitr::opts_chunk$set(include=TRUE)
knitr::opts_chunk$set(results="asis")
knitr::opts_chunk$set(fig.width=12)
knitr::opts_chunk$set(fig.height=14)
prjpath <- "~/work/learn/competitions/kaggle/springleaf/"
datapath <- paste(prjpath, "data/", sep="")
analpath <- paste(prjpath, "analyses/", sep="")
rcodepath <- paste(analpath, "Rcode/", sep="")
setwd(analpath)

## ----loadLibsAndSource, include=FALSE------------------------------------
reqdpkgs <- c("lattice", "latticeExtra", "ggplot2", "reshape2",
              "ggmap",	"plyr", "corrplot", "lubridate",
              "Hmisc", "corrplot", "rgdal", "sp")
lapply(reqdpkgs, library, character.only=TRUE)

## ----dataload------------------------------------------------------------
train <- read.csv(paste(datapath, 'train.csv', sep=""), stringsAsFactor=FALSE)
test <- read.csv(paste(datapath, 'test.csv', sep=""), stringsAsFactor=FALSE)

## ----predout-------------------------------------------------------------
predictors <- names(train[1:1933])
outcome <- names(train)[1934]

## ----allmissingcolumns---------------------------------------------------
N <- nrow(train)
all_missing <- sapply(predictors, function(p) sum(is.na(train[, p])) == N)
predictors <- predictors[!all_missing]
train <- train[!all_missing]
test <- test[!all_missing]

## ----factorvars----------------------------------------------------------
trstr <- capture.output(str(train, list.len=2000))
facvars <-  grepl(x=trstr, pattern='chr')

## ----datatypes-----------------------------------------------------------
predictor_types <- sapply(predictors, function(p) class(train[, p]))
print("Data types in the data frame train")
print(table(predictor_types))

## ----colByDataType-------------------------------------------------------
predictors_chr <- predictors[predictor_types == 'character']
predictors_num <- predictors[predictor_types == 'integer' | predictor_types == 'numeric']

## ----numdata-------------------------------------------------------------
train_num <- train[, predictors_num]
train_chr <- train[, predictors_chr]
test_num <- test[, predictors_num]
test_chr <- test[, predictors_chr]

## ----numdistinctchrvars--------------------------------------------------
num_chr_var_vals <- data.frame(
    predictor=predictors_chr,
    num_vals = sapply(predictors_chr, function(p) length(unique(train[,p])))
)
print(num_chr_var_vals)

## ----predictorscategorical-----------------------------------------------
maybe_cat <- sapply(
    predictors_chr,
    function(p) length(unique(train[,p])) < 11
)

categories_few <- lapply(
    predictors_chr[maybe_cat],
    function(p) unique(train[,p])
)
names(categories_few) <- predictors_chr[maybe_cat]
print(categories_few)

## ----manyvaluevars-------------------------------------------------------
categories_many <- lapply(
    predictors_chr[!maybe_cat],
    function(p) unique(train[,p])
)
names(categories_many) <- predictors_chr[!maybe_cat]

## ----datavars------------------------------------------------------------
data_months <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
                 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
is_date <- function(values){
    values <- values[values != '']
    n_months <- sum(sapply(data_months, function(p) {
                               sum(grepl(x=values,pattern=p)) }
                    ))
    n_months == length(values)
}

maybe_date <- sapply(
    predictors_chr,
    function(p) is_date(train[, p])
)
predictors_date <- predictors_chr[maybe_date]

parsed_date <- function(ds) {
    # ignore the time in the timestamp
    dmy(lapply(strsplit(x=ds, split=":"),
               function(s) s[1]))
}


## ----varlogical----------------------------------------------------------
is_logical <- function(values){
    uv <- unique(values)
    length(uv) <= 3 &
    (any(grepl(x=uv, pattern='true')) |
     any(grepl(x=uv, pattern='false')) )
}

maybe_logical <- sapply(
    predictors_chr,
    function(p) is_logical(train[, p])
)

predictors_logical <- predictors_chr[maybe_logical]


## ----varnotlognotdate----------------------------------------------------
predictors_nld <- predictors_chr[ !maybe_logical & !maybe_date]
uvs_nld <- lapply(predictors_nld, function(p) unique(train[, p]))
names(uvs_nld) <- predictors_nld
num_val_nld <- data.frame(
    predictor = predictors_nld,
    num_vals = sapply(predictors_nld, function(p) length(unique(train[, p])))
)
uvs_nld[ with(num_val_nld, predictor[num_vals < 11])]

## ----varlabels-----------------------------------------------------------
varlabels <- list()
n = 1
for(p in predictors_logical) {
    train[, p] <- as.logical(train[, p])
    test[, p] <- as.logical(test[,p])
    varlabels[p] <- paste('logical', n, sep='_')
    n = n + 1
}

n = 1
for(p in predictors_date){
    train[, p] <- parsed_date(train[, p])
    test[, p] <- parsed_date(test[,p])
    varlabels[p] <- paste('date', n, sep='_')
    n = n + 1
}

print('values of variable VAR_0001')
print(uvs_nld$VAR_0001)
varlabels$VAR_0001 <- 'hrq'

print('values of variable VAR_0005')
print(uvs_nld$VAR_0005)
varlabels$VAR_0005 <- 'cbns'

print('values of variable VAR_0044')
print(uvs_nld$VAR_0044)
varlabels$VAR_0044 <- 'isEmptyList'
train$VAR_0044 <- train$VAR_0044 == '[]'
test$VAR_0044 <- test$VAR_0044 == '[]'

print('values of variable VAR_0202')
print(uvs_nld$VAR_0202)
varlabels$VAR_0202 <- 'batchInquiry'
train$VAR_0202 <- train$VAR_0202 == 'BatchInquiry'
test$VAR_0202 <- test$VAR_0202 == 'BatchInquiry'

print('values of variable VAR_0216')
print(uvs_nld$VAR_0216)
varlabels$VAR_0216 <- 'ds'
train$VAR_0216 <- train$VAR_0216 == 'DS'
test$VAR_0216 <- test$VAR_0216 == 'DS'

print('values of variable VAR_0222')
print(uvs_nld$VAR_0222)
varlabels$VAR_0222 <- 'c6'
train$VAR_0222 <- train$VAR_0222 == 'C6'
test$VAR_0222 <- test$VAR_0222 == 'C6'

print('values of variable VAR_0283')
print(uvs_nld$VAR_0283)
varlabels$VAR_0283 <- 'shrug_first'
train$VAR_0283 <- as.factor(train$VAR_0283)
test$VAR_0283 <- as.factor(test$VAR_0283)

print('values of variable VAR_0305')
print(uvs_nld$VAR_0305)
varlabels$VAR_0305 <- 'shrug_second'
train$VAR_0305 <- as.factor(train$VAR_0305)
test$VAR_0305 <- as.factor(test$VAR_0305)

print('values of variable VAR_0325')
print(uvs_nld$VAR_0325)
varlabels$VAR_0325 <- 'shrug_third'
train$VAR_0325 <- as.factor(train$VAR_0325)
test$VAR_0325 <- as.factor(test$VAR_0325)

print('values of variable VAR_0352')
print(uvs_nld$VAR_0352)
varlabels$VAR_0352 <- 'oru_first'
train$VAR_0352 <- as.factor(train$VAR_0352)
test$VAR_0352 <- as.factor(test$VAR_0352)

print('values of variable VAR_0353')
print(uvs_nld$VAR_0353)
varlabels$VAR_0353 <- 'oru_second'
train$VAR_0353 <- as.factor(train$VAR_0353)
test$VAR_0353 <- as.factor(test$VAR_0353)

print('values of variable VAR_0354')
print(uvs_nld$VAR_0354)
varlabels$VAR_0354 <- 'oru_third'
train$VAR_0354 <- as.factor(train$VAR_0354)
test$VAR_0354 <- as.factor(test$VAR_0354)

print('values of variable VAR_0466')
print(uvs_nld$VAR_0466)
varlabels$VAR_0466 <- 'eye'
train$VAR_0466 <- as.factor(train$VAR_0466)
test$VAR_0466 <- as.factor(test$VAR_0466)

print('values of variable VAR_0467')
print(uvs_nld$VAR_0467)
varlabels$VAR_0467 <- 'status'
na0467 <- train$VAR_0467 == '-1' |  train$VAR_0467 == ''
train$VAR_0467[na0467] <- NA
train$VAR_0467 <- as.factor(train$VAR_0467)
test$VAR_0467 <- as.factor(test$VAR_0467)

print('values of variable VAR_1934')
print(uvs_nld$VAR_1934)
varlabels$VAR_1934 <- 'medium'
train$VAR_1934 <- as.factor(train$VAR_1934)
test$VAR_1934 <- as.factor(test$VAR_1934)

## ----manyvalvars---------------------------------------------------------
print(num_val_nld[ num_val_nld$num_vals > 10,])

print("some of the values for the variable VAR_0200")
print(head(unique(train$VAR_0200), n=20))
varlabels$VAR_0200 <- 'city'

print("values for the variable VAR_0214")
print(unique(train$VAR_0214))
varlabels$VAR_0214 <- 'idused'

print("some of the values for the variable VAR_0237")
print(head(unique(train$VAR_0237)))
varlabels$VAR_0237 <- 'state_first'

print("some of the values for the variable VAR_0274")
print(head(unique(train$VAR_0274)))
varlabels$VAR_0274 <- 'state_second'

print("some of the values for the variable VAR_0342")
print(head(unique(train$VAR_0342), n=10))
varlabels$VAR_0342 <- 'abcdefu'

print("some of the values for the variable VAR_0404")
print(head(unique(train$VAR_0404), n=10))
varlabels$VAR_0404 <- 'job_first'

print("some of the values for the variable VAR_0493")
print(head(unique(train$VAR_0493), n=10))
varlabels$VAR_0493 <- 'job_second'

## ----renamenum-----------------------------------------------------------

numvarlabels <- as.list(paste("numeric", 1:(length(predictors_num)-1), sep="_"))
names(numvarlabels) <- predictors_num[-1]

vl <- c(varlabels, numvarlabels)
vl$ID <- 'ID'
vl$target <- 'target'
vlc <- as.character(vl)
names(vlc) <- names(vl)
names(train) <- vlc[names(train)]
names(test) <- vlc[names(test)]

## ----relabeleddata-------------------------------------------------------
write.csv(train, file=paste(datapath, "train_labeled.csv", sep=""))
write.csv(test, file=paste(datapath, "test_labeled.csv", sep=""))

