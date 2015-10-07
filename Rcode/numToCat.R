## R code to generate data structures and functions to reason about numerical variables.

## ----loadLibsAndSource, include=FALSE------------------------------------

reqdpkgs <- c("lattice", "latticeExtra", "ggplot2", "reshape2",
              "plyr",  "lubridate", "Hmisc", "corrplot",
              "rpart", "rpart.plot", "RWeka",
              "caret", "gbm", "randomForest")
lapply(reqdpkgs, library, character.only=TRUE)



##----variables we need for the code in this script to work may exist in the environment
if (! exists("ovl_10_001") ) {

    ovl_10_001<- lapply(predictors,
                        function(var) {
                            print("outliers for ")
                            ol <- dpoutlyers(train[, var], offset=10, rarity=0.001)
                            print(var)
                            print(ol)
                        }
                        )

    names(ovl_10_001) <- predictors
}

if (! exists("ovl_10_00001") ) {

    ovl_10_00001<- lapply(predictors,
                          function(var) {
                              print("outliers for ")
                              ol <- dpoutlyers(train[, var], offset=10, rarity=1e-5)
                              print(var)
                              print(ol)
                          }
                          )

    names(ovl_10_00001) <- predictors
}

## ----removeec------------------------------------------------------------

if ( (! exists("train.noec") ) & (! exists("test.noec") ) ) {

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
}

df.ec.replaced <- function(data, ec.lists, replacement=NA) {
    print(head(names(data)))
    vars <- names(data)
    df <- data.frame(
        lapply(vars,
               function(p) {
                   xs <- data[,p]
                   xs[ xs %in% ec.lists[[p]] ] <- replacement
                   xs
               }),
        stringsAsFactors=FALSE
        )
    names(df) <- vars
    df
}


## ----logscalehistos------------------------------------------------------

plotHistsForGivenEffVals <- function(nmin,
                                     nmax,
                                     data=train,
                                     ovl=ovl_10_00001,
                                     vents=var.entropies,
                                     logscale=TRUE) {
    vars <- with(vents,
                 variable[floor(num_eff_vals) >= nmin &
                              floor(num_eff_vals) <= nmax ]
                 )
    ml <- melt(df.ec.replaced(data[, vars], ovl))
    if(logscale) {
        ml <- subset(ml, value >= 0)
        ml$value <- log10(1 + ml$value)
    }
    histogram( ~ value | variable, data=ml)
}

plotHistsForGivenDistinctVals <- function(num.distinct=1,
                                          num.effective=NULL,
                                          from.index=1,
                                          num.plot=16,
                                          data=train,
                                          ec.lists=ovl_10_00001,
                                          vstats=var.entropies,
                                          varset=names(train),
                                          logscale=TRUE,
                                          breaks=NULL,
                                          zero.excluded=FALSE) {
    data <- data[, varset]
    vstats <- vstats[varset,]
    if (!is.null(num.effective)) {
        vstats <- vstats[order(vstats$num_eff_vals),]
    } else {
        vstats <- vstats[order(vstats$distinct_vals),]
    }
    vfilt <- with(vstats,
                  datatype %in% c('integer', 'numeric') &
                      entropy > 0 &
                          floor(distinct_vals) >= num.distinct
                  )
    if (!is.null(num.effective) ) {
        vfilt <- vfilt & vstats$num_eff_vals
    }

    vars <- vstats$variable[vfilt]

    to.index <- min(from.index + num.plot - 1, length(vars))
    if(to.index < from.index) {
        print("nothing to plot")
        return(NULL)
    } else {
        print(paste("plotting from index", from.index, "to", to.index))
    }
    vars <- vars[from.index:to.index]
    print(vars)
    tdf <- df.ec.replaced(data[, vars], ec.lists)
    print(names(tdf))
    if(zero.excluded) {
        tdf <- df.value.replaced(tdf, 0, NA)
    }

    varInfo <- do.call(rbind,
                       lapply(tdf,
                              function(xs) {
                                  xs <- xs[!is.na(xs)]
                                  c(distinct=length(unique(xs)),
                                    effective=num.effective.vals(xs),
                                    min=min(xs),
                                    med=median(xs),
                                    max=max(xs)
                                    )
                              })
        )
    print(varInfo)

    names(tdf) <- paste(vars, "dis",
                        with(vstats[vars,],
                             distinct_vals - (nacount > 0)),
                        sep="."
                        )

    mdf <- melt(tdf)
    if (logscale) mdf$value <- log10(1 + mdf$value)
    title <- "Histogram for numeric variables"
    title <- paste( if (logscale) "log scale" else "lin scale ", title)
    title <- paste( if (zero.excluded) " zero excluded" else "", title)
    histogram(~ value | variable,
              data=mdf, type="density",
              breaks=breaks,
              par.settings=list(
                  par.main.text=list(cex=1),
                  superpose.symbol=list(cex=0.5),
                  fontsize=list(text=8, points=4)
                  ),
              scales=list(par.sub.text=1),
              main=title
              )
}

## ----clustervarsummaries-------------------------------------------------
variable.summaries <- function(data, variables){
    var.summaries <- do.call(rbind,
                             lapply(variables,
                                    function(p){
                                        xs <- data[, p]
                                        nna <- sum(is.na(xs))/nrow(data)
                                        xs <- xs[!is.na(xs)]
                                        xs.r <- removedValues(xs, ovl_10_00001[[p]])
                                        xs.z <- removedValues(xs.r, 0)
                                        c(dis=log2(length(unique(xs))),
                                          nna=nna,
                                          min=log2(min(xs)),
                                          med=log2(median(xs)),
                                          max=log2(max(xs)),
                                          ent=entropy(xs),
                                          dis.noec=log2(length(unique(xs.r))),
                                          min.noec=log2(1 + min(xs.r)),
                                          med.noec=log2(1 + median(xs.r)),
                                          max.noec=log2(1 + max(xs.r)),
                                          ent.noec=entropy(xs.r),
                                          dis.noz=log2(length(unique(xs.z))),
                                          min.noz=log2(1 + min(xs.z)),
                                          med.noz=log2(1 + median(xs.z)),
                                          max.noz=log2(1 + max(xs.z)),
                                          ent.noz=entropy(xs.z)
                                          )
                                    })
                             )
    var.summaries[is.infinite(var.summaries)] <- NA
    row.names(var.summaries) <- variables
    var.summaries
}


## ----cutsummaries--------------------------------------------------------

summary.cluster.cut <- function(clucut, data=var.summaries) {
    do.call(cbind,
            lapply(unique(clucut),
                   function(c) {
                       colMeans(data[clucut == c,],
                                na.rm=TRUE)
                   })
            )
}


cutsummaries <- function(k, data=var.summaries) {
    d <- dist(scale(data), method='euclidean')
    clu <- hclust(d, method='ward')
    plot(clu, labels=FALSE)
    clucut <- cutree(clu, k=k)
    summary.cluster.cut(clucut, data)
}

plotFPsummaries <- function(csum, dmax=12, PLOT=TRUE, logscale=FALSE) {
    is.dpfield <- grepl(pattern = "dp", x=row.names(csum))
    csum.df <- data.frame(csum)
    csum.df$fpfield <- row.names(csum)
    csum.df <- subset(csum.df, !(fpfield %in% c("nna", "nec", "neg")))
    csum.df$fpclass <- c("ST", "DP")[1 + grepl(pattern="dp",
                                              x=as.character(csum.df$fpfield))]

    nonfpfields <-  with(csum.df, fpfield[!grepl(pattern="dp", fpfield)])
    dpfield.names <- c(paste("dp", 0:(dmax-1), sep="."))
    csum.df$fpfield <- factor(csum.df$fpfield,
                              levels=c(dpfield.names, nonfpfields))

    csum.df$fpclass <- as.factor(csum.df$fpclass)
    csum.df.ml <- melt(csum.df, id.vars=c("fpclass", "fpfield"))
    if (logscale) {
        csum.df.ml$value <- log10( csum.df.ml$value)
    }
    ap <- xyplot(value ~ fpfield | fpclass,
                 groups=variable,
                 data=csum.df.ml,
                 auto.key=TRUE,
                 type="b",
                 scales=list(x=list(rot=90), pch=25, relation="free"),
                 main="Fingerprints for clusters"
                 )
    if(PLOT) print(ap)
    ap
}



## ----varfingerprints-----------------------------------------------------

variable.dp.fingerprints <- function(data,
                                     variables,
                                     dmax=12,
                                     exclude.zero=FALSE,
                                     with.negatives=FALSE) {
    vfps <- do.call(rbind,
                    lapply(variables,
                           function(p) {
                               bins <- rep(0, dmax)
                               names(bins) = 0:(dmax-1)
                               xs <- data[, p]
                               xs <- xs[ xs >= 0]
                               xs <- xs[ !is.na(xs) ]
                               lxs <- table(floor(log10( 1 + xs) ))
                               bins[names(lxs)] <- as.numeric(lxs)
                               if(exclude.zero) bins[-1] else bins
                           })
                    )
    colnames(vfps) <- paste("dp", 0:(dmax-1), sep=".")
    if (with.negatives) {
        neg <- sapply(variables,
                      function(p) sum(data[, p] < 0, na.rm=TRUE)
                      )
        vfps <- cbind(vfps, neg)
    }
    row.names(vfps) <- variables
    vfpa
}
