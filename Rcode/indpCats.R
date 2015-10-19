independentCategories <- function(p, data) {
    xs <- as.character(data[, p])
    xs[is.na(xs)] <- "unknown"
    vs <- unique(xs)
    df <- data.frame(lapply(vs),
                     function(v) as.numeric(xs == v)
                     )
    names(df) <- paste(p, vs, sep=".")
    df
}

