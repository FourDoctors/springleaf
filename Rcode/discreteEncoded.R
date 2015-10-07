
discrete.code <- function(xs, by='entropy') {
    xs <- as.character(xs)
    xs[is.na(xs)] <- "NA"
    txs <- as.data.frame(table(xs),
                         stringsAsFactors=FALSE)
    row.names(txs) <- txs$xs
    txs$p <- txs$Freq/sum(txs$Freq)
    txs <- txs[order(txs$p, decreasing=TRUE),]
    txs$code <- as.character(1:nrow(txs))
    if(by == 'entropy') {
        ##n <- ceiling( 2 ** with(txs, -sum(p * log2(p))))
        n <- 2 ** (ceiling( with(txs, -sum(p * log2(p) ))))
    }
    else {
        txs$cp <- cumsum(txs$p)
        n <- which(txs$p < 1 - txs$cp)[1]
    }
    txs$code[n:nrow(txs)] <- as.character(n)
    txs
}

discrete.encoded <- function(xs, by='entropy') {
    txs <- discrete.code(xs, by=by)
    txs[as.character(xs), 'code']
}

huffman.encoded <- function(xs, max.loss=1) {
    hc <- huffman.code(xs)
    chc <- cuthoff(hc, max.loss=max.loss)
    hxs.c <- subset(chc, node.type == 'Leaf')[, c('isym', 'code')]
    sxs <- as.character(xs)
    sxs[is.na(sxs)] <- "NA"
    hxs.c[sxs, 'code']
}
