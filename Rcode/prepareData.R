

## ----setnumdatatypes-----------------------------------------------------
library(lubridate)
library(rpart)
library(rpart.plot)
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
print("test.model.num.nz")
test.model.num.nz <- data.frame(lapply(test[, preds.num],
                                          function(xs) not.zero(xs)
                                          ),
                                   stringsAsFactors=FALSE
                                   )
names(test.model.num.nz) <- paste(preds.num, 'nz', sep='.')
save(test.model.num.nz, file="../data/testModelNumNZ.Rdata")

print("test.model.num.ec")
test.model.num.ec <- data.frame(lapply(preds.num,
                                        function(p) {
                                            xs <- test[, p]
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
names(test.model.num.ec) <- paste(preds.num, "ec", sep=".")
save(test.model.num.ec, file="../data/testModelNumEC.Rdata")

print("test.model.X1.ls")
test.model.num.X1.ls <- data.frame(
    lapply(preds.num.X1,
           function(p) {
               xs <- test[, p]
               xs[xs %in% ovl_10_00001[[p]]] <- NA
               xs[ xs <= 0] <- NA
               log10(1 + xs)
           }),
    stringsAsFactors = FALSE
    )
names(test.model.num.X1.ls) <- paste(preds.num.X1, 'ls', sep='.')
save(test.model.num.X1.ls, file="../data/testModelNumX1ls.Rdata")

print("test.model.num.X2.ls")
test.model.num.X2.ls <- data.frame(
    lapply(preds.num.X2,
           function(p) {
               xs <- test[, p]
               xs[xs %in% ovl_10_00001[[p]]] <- NA
               xs[xs <= 0] <- NA
               log10(1 + xs)
           }),
    stringsAsFactors = FALSE
    )
names(test.model.num.X2.ls) <- paste(preds.num.X2, 'ls', sep='.')
save(test.model.num.X2.ls, file="../data/testModelNumX2ls.Rdata")

print("test.model.num.X3.cons.ls")
test.model.num.X3.cons.ls <- data.frame(
    lapply(preds.num.X3.cons,
           function(p) {
               xs <- test[, p]
               xs[xs %in% ovl_10_00001[[p]]] <- NA
               xs[xs <= 0] <- NA
               log10(1 + xs)
           }),
    stringsAsFactors = FALSE
    )
names(test.model.num.X3.cons.ls) <- paste(preds.num.X3.cons, 'ls', sep='.')
save(test.model.num.X3.cons.ls, file="../data/testModelNumX3ls.Rdata")

print("test.model.num.X3.cats.hc")
max.loss <- 0.2
test.model.num.X3.cats.hc <- data.frame(
    lapply(test.model[, preds.num.X3.cats],
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
names(test.model.num.X3.cats.hc) <- paste(preds.num.X3.cats,
                                                 'hc', sep=".")


## ----savenumvardata------------------------------------------------------

print("test.model.num.treated")
test.model.num.treated <- cbind(
    test.model.num.X1.ls,
    test.model.num.X2.ls,
    test.model.num.X3.cons.ls,
    test.model.num.X3.cats.hc,
    test.model.num.nz,
    test.model.num.ec
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

print("test.model.date")
test.model.date <- data.frame(
    lapply(test.model[, preds.model.dates],
           function(xs) ymd(xs))
    )

print("test.model.date.year")
test.model.date.year <- data.frame(
    lapply(test.model.date,
           function(xs) {
               ys <- as.character(year(xs))
               ys[is.na(ys)] <- "unknown"
               as.factor(ys)
           })
    )
names(test.model.date.year) <- paste(preds.model.dates,
                                      'year', sep='.')
save(test.model.date.year, file="../data/testModelDateYear.Rdata")

print("test.model.date.month")
test.model.date.month <- data.frame(
    lapply(test.model.date,
           function(xs) {
               ys <- as.character(month(xs))
               ys[is.na(ys)] <- "unknown"
               as.factor(ys)
           })
    )
names(test.model.date.month) <- paste(preds.model.dates,
                                      'month', sep='.')
save(test.model.date.month, file="../data/testModelDateMonth.Rdata")

print("test.model.date.week")
test.model.date.week <- data.frame(
    lapply(test.model.date,
           function(xs) {
               ys <- as.character(week(xs))
               ys[is.na(ys)] <- "unknown"
               as.factor(ys)
               #week(xs)
           })
    )
names(test.model.date.week) <- paste(preds.model.dates,
                                      'week', sep='.')
save(test.model.date.week, file="../data/testModelDateWeek.Rdata")

print("test.model.date.mday")
test.model.date.mday <- data.frame(
    lapply(test.model.date,
           function(xs) {
               ys <- as.character(mday(xs))
               ys[is.na(ys)] <- "unknown"
               as.factor(ys)
               #mday(xs)
           })
    )
names(test.model.date.mday) <- paste(preds.model.dates,
                                     'mday', sep='.')
save(test.model.date.mday, file="../data/testModelDateMday.Rdata")

print("test.model.date.yday")
test.model.date.yday <- data.frame(
    lapply(test.model.date,
           function(xs) {
               ys <- as.character(yday(xs))
               ys[is.na(ys)] <- "unknown"
               as.factor(ys)
               #yday(xs)
           })
    )
names(test.model.date.yday) <- paste(preds.model.dates,
                                     'yday', sep='.')
save(test.model.date.yday, file="../data/testModelDateYday.Rdata")

print("test.model.date.wday")
test.model.date.wday <- data.frame(
    lapply(test.model.date,
           function(xs) {
               ys <- as.character(wday(xs))
               ys[is.na(ys)] <- "unknown"
               as.factor(ys)
           })
    )
names(test.model.date.wday) <- paste(preds.model.dates,
                                     'wday', sep='.')
save(test.model.date.wday, file="../data/testModelDateWday.Rdata")

print("test.model.cat")
test.model.cat <- data.frame(
    lapply(test[, preds.model.cats],
           function(xs) {
               xs[is.na(xs)] <- 'unknown'
               as.factor(xs)
           })
    )

print("test.model.log")
test.model.log <- test[, preds.model.logs]
test.model.loc <- test[, preds.model.locs]
test.model.job <- test[, preds.model.jobs]

print("test.model.state")
test.model.state <- data.frame(
    lapply(test[, c('state_first', 'state_second')],
           function(xs) as.factor(xs)
           )
    )

print("test.model.hasjob")
test.model.hasjob <- data.frame(
    hasjob = test$job_first != "-1" |
        test$job_second != "-1"
    )

print("test.model.non_num")
test.model.non_num.treated <- cbind(
    test.model.date.year,
    test.model.date.month,
    test.model.date.week,
    test.model.date.mday,
    test.model.date.yday,
    test.model.date.wday,
    test.model.cat,
    test.model.log,
    test.model.state,
    test.model.hasjob
    )

save(test.model.non_num.treated, file="../data/testModelNonNumTreated.Rdata")
save(test.model.num.treated, file="../data/testModelNumTreated.Rdata")

