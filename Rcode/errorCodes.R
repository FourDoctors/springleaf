

## ----functions to compute some properties of the variables

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

predictor.entropies <- function(vars, data=train, useNA='no') {
    df <- data.frame(
        list(
            variable = vars,
            entropy = sapply(vars, function(p) {
                                 tab <- as.numeric(table(data[,p]),
                                                   useNA = useNA)
                                 p <- tab/sum(tab)
                                 - sum(p * log2(p))
                             }),
            median = if(class(data[, p]) %in% c('numeric', 'integer')) {
                sapply(vars, function(p) median(data[,p], na.rm=TRUE))
            } else NA
            max = if(class(data[, p]) %in% c('numeric', 'integer')) {
                sapply(vars, function(p) max(data[,p], na.rm=TRUE))
            } else NA
            min = if(class(data[, p]) %in% c('numeric', 'integer')) {
                sapply(vars, function(p) min(data[,p], na.rm=TRUE))
            } else NA
            distinct_vals = sapply(vars, function(p) {
                                       length(unique(data[,p], na.rm=TRUE))
                                   }),
            nacount = sapply(vars, function(p) {
                                 sum(is.na(data[,p]))
                             }),
            datatype = sapply(vars, function(p) {
                                  class(data[,p])
                              })
            ),
        stringsAsFactors=FALSE
        )

    df$num_eff_vals <- 2**(df$entropy)
    df <- df[order(df$num_eff_vals),]
    df
}

## ----outlyingvalues------------------------------------------------------

outlying.values <- function(var, data=train, degree=2){
    uv <- train[,var]
    uv <- sort(uv[!is.na(uv)])
    uvneg <- unique(uv[ uv < 0])
    if(length(uvneg) == 1) negovs <- uvneg
    else negovs <- c()
    uv <- uv[ uv >= 0]
    luv <- log10(uv+1)
    if (length(luv) == 0) uvneg
    else {
        ovs <- unique(uv[ luv - mean(luv) > degree * sd(luv)])
        c(negovs, ovs)
    }
 }

## ----outlying values using decimal points in the value

dpoutlyers <- function(xs, data=train, offset=10, rarity = 0.001) {
    if (class(xs) != 'numeric' & class(xs) != 'integer') return(c())
    N <- length(xs)
    xs <- xs[!is.na(xs)]
    xsneg <- xs[xs < 0]
    xs <- xs[xs >= 0]
    olneg <- if (length(unique(xsneg)) == 1) unique(xsneg) else c()
    if(length(xs) == 0) return(olneg)
    txs <- table(xs)
    dps <- ceiling(log10(xs + offset))
    tdps <- table(dps)
    if(length(tdps) == 1) return(olneg)
    n <- max(dps)
    cdps <- rep(0, n)
    cdps[as.numeric(names(tdps))] <- as.numeric(tdps)
    if (length(cdps) <= 1) return(olneg)
    if ( (cdps[n-1] == 0) | (cdps[n]/N < rarity)) {
        polps <- unique(xs[dps == n])
        polps <- polps[polps > 10]
        uxs <- unique(xs)
        olpos <- polps[ polps > 2*max(setdiff(uxs, polps))]
        c(olneg, olpos )
    }
    else {
        olneg
    }
}
