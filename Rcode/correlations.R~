##------------remove values------------

removedValues <- function(xs, vs) xs[ ! xs %in% vs]

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

predictor.entropies <- function(vars, data=train, ovl=ovl_10_00001, useNA='no') {
    vals.nonec <- function(p) {
        xs <- data[,p]
        vs <- ovl[[p]]
        xs[ !(xs %in% vs) ]
    }

    df <- data.frame(
        list(
            variable = vars,
            entropy = sapply(vars, function(p) {
                                 tab <- as.numeric(table(data[,p]),
                                                   useNA = useNA)
                                 p <- tab/sum(tab)
                                 - sum(p * log2(p))
                             }),
            min = sapply(vars, function(p) min(data[, p], na.rm=TRUE)),
            median = sapply(vars, function(p) median(data[,p], na.rm=TRUE)),
            max = sapply(vars, function(p) max(data[,p], na.rm=TRUE)),

            min.nonec = sapply(vars, function(p) min(vals.nonec(p))),
            median.nonec = sapply(vars, function(p) median(vals.nonec(p))),
            max.nonec = sapply(vars, function(p) max(vals.nonec(p))),

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



viewVarEnt <- function(vents=var.entropies){
    vents[, c('entropy', 'distinct_vals', 'nacount')]
}



