##------------remove values------------

entropy.natural <- function(xs, exclude=NULL) {
    cxs <- as.numeric(table(xs, exclude=exclude))
    cxs <- cxs[ cxs > 0]
    pxs <- cxs/sum(cxs)
    - sum( pxs * log(pxs))
}

entropy.bits <- function(xs, exclude=NULL) {
    cxs <- as.numeric(table(xs, exclude=exclude))
    cxs <- cxs[ cxs > 0]
    pxs <- cxs/sum(cxs)
    - sum( pxs * log2(pxs))
}

entropy <- function(xs, exclude=NULL, scale=2, na.rm=FALSE) {
    if (scale==2) entropy.bits(xs, exclude)
    else entropy.natural(xs, exclude)/log(scale)
}


num.effective.vals <- function(xs, exclude=NULL) {
    exp(entropy.natural(xs, exclude))
}

remove.values <- function(xs, vs) xs[ ! xs %in% vs]

removedValues <- remove.values

df.value.replaced <- function(df, value, replacement=NA) {
    df[ df == value] <- replacement
    df
}

noec.df <- df.ec.replaced

predictor.values.counts <- function(p, data=train){
    vals <- sort(unique(data[, p]), na.last=TRUE)
    tab <- data.frame(table(data[, p], useNA='ifany'))
    names(tab) <- c('value', 'count')
    tab$predictor <- p
    tab[, c("predictor", "value", "count")]
}

predictor.values <- function(vars, n, data=train) {
    mat <- cbind(vars,
                 do.call(rbind,
                         lapply(vars,
                                function(p) {
                                    sort(unique(data[, p]), na.last=TRUE)
                                })
                         )
                 )
    df <- data.frame(mat, stringsAsFactors=FALSE)
    names(df) <- c("variable", paste("value", 1:n, sep="."))
    df$entropy <- sapply(vars,
                         function(p) {
                             tab <- as.numeric(table(data[, p]))
                             prob <- tab/sum(tab)
                             -1 * sum(prob * log(prob))
                         })
    df$num.eff.vars <- exp(df$entropy)
    df
}

predictor.entropies <- function(vars,
                                data=train,
                                ovl=ovl_10_00001,
                                useNA='no',
                                with.ec=FALSE) {
    vals.nonec <- function(p) {
        xs <- data[,p]
        if(!(p %in% names(ovl))) {
            p <- strsplit(p, split=".", fixed=TRUE)[[1]]
            if (p %in% names(ovl)) {
                vs <- ovl[[p]]
            } else {
                vs <- c()
            }
        } else {
            vs <- ovl[[p]]
        }
        xs[ !(xs %in% vs) ]
    }
    compute.statistic <- function(p, statistic.function, nonec=FALSE) {
        xs <- if (nonec) vals.nonec(p) else data[,p]
        if(class(xs) %in% c('numeric', 'integer')){
            statistic.function(xs, na.rm=TRUE)
        }
        else {NA}
    }
    df <- data.frame(
        list(
            variable = vars,
            entropy = sapply(vars, function(p) entropy(data[,p])),
            median = sapply(vars, function(p) compute.statistic(p, median)),
            max = sapply(vars, function(p) compute.statistic(p, max)),
            min = sapply(vars, function(p) compute.statistic(p, min)),

            distinct_vals = sapply(vars, function(p) {
                                       length(unique(data[,p], na.rm=TRUE))
                                   }),
            nacount = sapply(vars, function(p) {
                                 sum(is.na(data[,p]))
                             }),
            datatype = sapply(vars, function(p) class(data[, p]))
            ),
        stringsAsFactors=FALSE
        )
    if(with.ec) {
        df.ec <- data.frame(
            list(
                entropy.nonec = sapply(vars,
                    function(p) entropy(vals.nonec(p)),

                median.noec = sapply(vars,
                    function(p) compute.statistic(p, median, nonec=TRUE)),
                max.noec = sapply(vars,
                    function(p) compute.statistic(p, max, nonec=TRUE)),
                min.noec = sapply(vars,
                    function(p) compute.statistic(p, min, nonec=TRUE))
                ),
            stringsAsFactors = FALSE
                )
            )
        df <- cbind(df, df.ec)
    }
    df$num_eff_vals <- 2**(df$entropy)
    df <- df[order(df$num_eff_vals),]
    df
}




## ----kldfunc-------------------------------------------------------------

kld <- function(x, y, asymm = TRUE){
    if (!asymm) ( kld(x, y) + kld(y, x))/2
    else {
        vs <- union(unique(x), unique(y))
        cx = sapply(vs, function(v) sum(x==v))
        cy = sapply(vs, function(v) sum(y==v))
        px <- (cx + 1)/(sum(cx + 1))
        py <- (cy + 1)/sum(cy + 1)
        sum(px * log2(px/py))
    }
}



## ---------kld for vars--------------------------------------------------

kld.vars <- function(vars, data=train){
    t0 <- data$target == 0
    N <- length(vars)
    data.frame(
        list(
            variable = vars,
            kld = sapply(vars, function(p) {
                             x = data[, p][t0]
                             y = data[, p][!t0]
                             kld(x[!is.na(x)], y[!is.na(y)])
                         }),
            entropy = sapply(vars, function(p) {
                                 tab <- as.numeric(table(data[,p]),
                                                   useNA = 'no')
                                 p <- tab/sum(tab)
                                 - sum(p * log2(p))
                             }),
            min = sapply(vars, function(p) {
                             if (!is.numeric(data[,p])) NA
                             else {
                                 min(data[,p], na.rm=TRUE)
                             }
                         }),
            median = sapply(vars, function(p) {
                             if (!is.numeric(data[,p])) NA
                             else {
                                 median(data[,p], na.rm=TRUE)
                             }
                         }),
            max = sapply(vars, function(p) {
                             if (!is.numeric(data[,p])) NA
                             else {
                                 max(data[,p], na.rm=TRUE)
                             }
                         }),

            hasna  = sapply(vars, function(p) any(is.na(data[, p]))),
            notna1 = sapply(vars, function(p) sum(!is.na(data[,p][!t0]))),
            notna0 = sapply(vars, function(p) sum(!is.na(data[,p][t0]))),
            distinct = sapply(vars, function(p) length(unique(data[,p])))
            ),
        stringsAsFactors=FALSE
        )
}






## ----compareTwoVars------------------------------------------------------

comparison2vars <- function(u, v){
    xs = train[,u]
    ys = train[,v]
    muv = melt(train[, c(u,v)])
    list(
        correlation=cor(xs, ys, use='pairwise.complete.obs'),
        kld = kld(xs[!is.na(xs)], ys[!is.na(ys)], asymm=FALSE),
        hist.lin = histogram( ~ value | variable,
            data=muv,
            layout=c(1,2)),
        hist.log = histogram( ~ log10(value) |  variable,
            data=muv,
            layout=c(1,2))
        )
}

cumplot <- function(xs, logscale = FALSE) {
    xs <- sort( xs[!is.na(xs)])
    if (logscale) xs <- sort(log10(xs[ xs >= 0] + 1))
    n <- length(xs)
    p <- (1:n)/n
    xyplot(p ~ xs, type='l')
}


melt.noec <- function(data, ovl){
    vars <- names(data)
    data <- as.data.frame(do.call(cbind,
                               lapply(vars,
                                      function(v){
                                          x <- data[,v]
                                          x[ x %in% ovl[[v]]] <- NA
                                          x
                                      })
                               )
                          )
    names(data) <- vars
    melt(data)
    }




## ----plotvarsgiveneffvals------------------------------------------------
plotHistsForGivenEffVals <- function(nmin,
                                     nmax,
                                     data=train,
                                     ovl=ovl_10,
                                     vents=var.entropies,
                                     logscale=TRUE){
    vars <- with(vents, variable[floor(num_eff_vals) >= nmin &
                                     floor(num_eff_vals) < nmax])
    dt <- data[, vars]
    ml <- melt.noec(dt, ovl)
    ml <- subset(ml, !is.na(value))
    if(logscale) {
        ml <- subset(ml, value >= 0)
        ml$value <- log10(ml$value + 1)
    }
    histogram(~ value | variable, data=ml)
}

effective.vals <- function(xs) {
    xs <- xs[!is.na(xs)]
    cxs <- as.numeric(table(xs))
    pxs <- cxs/sum(cxs)
    e <- - sum( pxs * log2(pxs) )
    2**e
}

plotHistsForGivenDistinctVals <- function(numdistinct=1,
                                          numeffective=NULL,
                                          from.index=1,
                                          numplot=16,
                                          data=train,
                                          ovl=ovl_10_00001,
                                          vents=var.entropies,
                                          logscale=TRUE,
                                          breaks=NULL,
                                          zeroRemoved=FALSE) {
    if(!is.null(numeffective)) {
        vents <- vents[order(vents$num_eff_vals),]
    } else {
        vents <- vents[order(vents$distinct_vals),]
    }
    vars <- with(vents,
                 variable[entropy > 0 &
                              floor(distinct_vals) >= numdistinct &
                                  datatype %in% c('integer', 'numeric')
                          ]
                 )
    if (!is.null(numeffective)) {
        vars <- Filter(function(p) vents[p, 'num_eff_vals'] >= numeffective,
                       vars)
    }
    to.index <- min(from.index + numplot - 1, length(vars))
    print(paste('plots', from.index, "to" , to.index))
    if(to.index < from.index) {
        print("nothing to plot")
        return(NULL)
    }
    vars <- vars[from.index:to.index]
    tdf <- noec.df(data[, vars])
    if (zeroRemoved) {
        tdf <- data.frame(lapply(tdf,
                                 function(xs) {
                                     xs[xs==0] <- NA
                                     xs
                                 }),
                          stringsAsFactors=FALSE
                          )
    }
    print(do.call(rbind, lapply(tdf, function(xs) {
                                    c(min=min(xs, na.rm=TRUE),
                                      median=median(xs, na.rm=TRUE),
                                      max=max(xs, na.rm=TRUE),
                                      distinct=length(unique(xs[!is.na(xs)])),
                                      effective=effective.vals(xs)
                                      )
                                })
                  )
          )
    names(tdf) <- paste(vars, "dis",
                        with(vents[vars,],
                             distinct_vals - (nacount > 0)),
                        sep=".")
    mdf <- melt(tdf)
    if (logscale) mdf$value <- log10(1 + mdf$value)
    title <- "histogram for numeric variables"
    title <- paste( if (logscale) "log scale" else "lin scale ", title)
    title <- paste( if (zeroRemoved) "zero removed" else "", title)
    histogram( ~ value | variable,
              data=mdf, type='density',
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





viewVarEnt <- function(vents=var.entropies){
    vents[, c('entropy', 'distinct_vals', 'nacount')]
}



