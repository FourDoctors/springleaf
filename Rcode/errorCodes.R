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
train <- read.csv(paste(datapath, 'train_labeled.csv', sep=""),
                  stringsAsFactor=FALSE)
test <- read.csv(paste(datapath, 'test_labeled.csv', sep=""),
                 stringsAsFactor=FALSE)

## ----datatypes-----------------------------------------------------------
V <- ncol(train)
predictors <- names(train)[-V]
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

## ----numvals-------------------------------------------------------------

num_vals_num_vars <- data.frame(list(
    label = as.character(predictors_num),
    num_vals = sapply(predictors_num,
        function(p) length(unique(train[, p]))
    )),
    stringsAsFactors=FALSE
)

histogram(~ log10(num_vals), data=num_vals_num_vars)

## ----singlevalvar--------------------------------------------------------
print(num_vals_num_vars[num_vals_num_vars$num_vals == 1,])

print("the only value for numeric_792")
print(unique(train$numeric_792))

print("the only value for numeric_1373")
print(unique(train$numeric_1373))

predictors_num_dropped <- with(num_vals_num_vars, label[num_vals == 1])

print(num_vals_num_vars[num_vals_num_vars$num_vals == 2,])

## ----assumedvalsfew------------------------------------------------------
predictor.values.counts <- function(p, data=train){
    vals <- sort(unique(train[, p]), na.last=TRUE)
    tab <- data.frame(table(train[, p], useNA='ifany'))
    names(tab) <- c('value', 'count')
    tab$predictor <- p
    tab[, c("predictor", "value", "count")]
}

predictor.values <- function(vars, n, data=train) {
    mat <- cbind(vars,
                 do.call(rbind,
                         lapply(vars,
                                function(p) {
                                    sort(unique(train[, p]), na.last=TRUE)
                                })
                         )
                 )
    df <- data.frame(mat, stringsAsFactors=FALSE)
    names(df) <- c("variable", paste("value", 1:n, sep="."))
    df$entropy <- sapply(vars,
                         function(p) {
                             tab <- as.numeric(table(train[, p]))
                             prob <- tab/sum(tab)
                             -1 * sum(prob * log(prob))
                         })
    df$num.eff.vars <- exp(df$entropy)
    df
}
predictor.entropies <- function(vars, data=train, useNA='no') {
    df <- data.frame(
        list(
            variable = vars,
            entropy = sapply(vars, function(p) {
                                 tab <- as.numeric(table(train[,p]), useNA = useNA)
                                 p <- tab/sum(tab)
                                 - sum(p * log2(p))
                             }),
            median = sapply(vars, function(p) {
                                median(train[,p], na.rm=TRUE)
                            }),
            max = sapply(vars, function(p) {
                             max(train[,p], na.rm=TRUE)
                         }),
            min = sapply(vars, function(p) {
                             min(train[, p], na.rm=TRUE)
                         }),
            distinct_vals = sapply(vars, function(p) {
                                       length(unique(train[,p], na.rm=TRUE))
                                   }),
            nacount = sapply(vars, function(p) {
                                 sum(is.na(train[,p]))
                             })
            ),
        stringsAsFactors=FALSE
        )

    df$num_eff_vals <- 2**(df$entropy)
    df <- df[order(df$num_eff_vals),]
    df
}


## ----varenteffval--------------------------------------------------------

var.entropies <- predictor.entropies(num_vals_num_vars$label)

## ----threedistinctnum1---------------------------------------------------
three.vals <- predictor.values(
    with(num_vals_num_vars, label[num_vals==3]), 3
    )

three.vals.counts <- do.call(rbind,
                             lapply(three.vals$variable,
                                    predictor.values.counts)
                             )
print(three.vals.counts)

## ----threedistinctnum----------------------------------------------------
var.3val.cor <- cor(train[, with(var.entropies, variable[distinct_vals==3])], use='pairwise.complete.obs')
var.3val.cor[is.na(var.3val.cor)] <- 0
corrplot(var.3val.cor, method='color')

## ----fourdistinctnum2----------------------------------------------------
var.4val.cor <- cor(train[, with(var.entropies, variable[distinct_vals==4])], use='pairwise.complete.obs')
var.4val.cor[is.na(var.4val.cor)] <- 0
corrplot(var.4val.cor, method='color', tl.cex=0.5)

## ----outlyingvalues------------------------------------------------------
outlying.values <- function(var, data=train, max.outliers = 4){
    uv <- unique(train[,var])
    uv <- sort(uv[!is.na(uv)])
    uvneg <- uv[ uv < 0]
    if(length(uvneg) == 1) negovs <- uvneg
    else negovs <- c()
    uv <- uv[ uv >= 0]
    if (length(uv) == 0) uvneg
    else {
        lmd <- log10(median(uv) + 1)
        dep <- sapply(uv + 1, function(v) {
                          if( v <= 0 ) NA
                          else log10(v) - lmd
                      })
        lmx <- log10(max(uv) + 1)
        max_dep <- ceil(lmx) - ceil(lmd)
        ovs <- uv[dep >= ceil(max_dep)]
        if ((max_dep <= 0) | (length(ovs) > max.outliers)) negovs
        else c(negovs, ovs)
    }
 }


## ----outlyingvaluesevaluate----------------------------------------------
outlying.values.list <- lapply(num_vals_num_vars$label,
                               function(p) {
                                   print(paste("outliers for", p))
                                   outlying.values(p)
                               })
names(outlying.values.list) <- num_vals_num_vars$label

## ----markNAs-------------------------------------------------------------
train.ecfixed <- train
for(p in num_vals_num_vars$label) {
    ov <- outlying.values.list[[p]]
    x <- train[,p]
    x[x %in% ov] <- NA
    train.ecfixed[,p] <- x
}

write.csv(train.ecfixed, file=paste(datapath, 'train_ecfixed.csv', sep=""))

test.ecfixed <- test
for(p in num_vals_num_vars$label) {
    ob <- outlying.values.list[[p]]
    x <- test[,p]
    x[x %in% ov] <- NA
    test.ecfixed[,p] <- x
}

write.csv(test.ecfixed, file=paste(datapath, 'test_ecfixed.csv', sep=""))

## ----numnas--------------------------------------------------------------
nacount.rows <- rowSums(is.na(train.ecfixed))
nacount.cols <- data.frame(
    list(
        variable = names(train.ecfixed),
        count = sapply(names(train.ecfixed), function(p) sum(is.na(train.ecfixed[,p])))
        ),
    stringsAsFactors=FALSE
    )


predictors_allfilled <- with(nacount.cols, variable[count == 0])
predictors_num_allfilled <- intersect(predictors_allfilled, predictors_num)
var.entropies$nacount <- nacount.cols$count[var.entropies$variable]

## ----nonacolcor----------------------------------------------------------
nonacol.cor <- cor(train.ecfixed[, predictors_num_allfilled])

## ----vars192and193-------------------------------------------------------
print("correlation between numeric_192 and numeric_193")
print(with(train, cor(numeric_192, numeric_193, use="pairwise.complete.obs")))

## ----identicalvars-------------------------------------------------------
identical_vars <- list()
identical_vars$numeric_192 <- c('numeric_193')

