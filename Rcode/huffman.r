##-------Huffman code an ensemble------------

huffman.code <- function(xs) {
    txs <- table(xs, exclude=NULL)
    txs <- txs[txs > 0]
    nxs <- names(txs)
    nxs[is.na(nxs)] <- "NA"
    p <- as.numeric(txs)/sum(txs)
    symbol.codes <- data.frame(
        list(
            symbol = nxs,
            isym = nxs,
            mother = rep("", length(nxs)),
            msym = nxs,
            p = p,
            e = -p * log2(p),
            step = rep("", length(nxs)),
            node.type = "Leaf",
            left = NA,
            right = NA
            ),
        stringsAsFactors=FALSE
        )
    ##print(str(symbol.codes))
    ##print(symbol.codes)
    hc.concat.two <- function(sc) {
        ##print("--------------------")
        if (sum(sc$mother == "") == 1){
            sc
        } else {
            sc.nm <- subset(sc, mother=="")
            sc.m <- subset(sc, mother!="")
            sc.nm <- sc.nm[order(sc.nm$p), ]
            sc.nm$step[1] <- "0"
            sc.nm$step[2] <- "1"
            nmot <- paste("(", sc.nm$symbol[1], ",", sc.nm$symbol[2], ")", sep="")
            sc.nm$mother[1] <- nmot
            sc.nm$mother[2] <- nmot
            isym = paste('node', as.character(nrow(sc) + 1), sep='.')
            sc.nm$msym[1] <- isym
            sc.nm$msym[2] <- isym
            sc <- rbind(sc.m, sc.nm)
            merged <- c(symbol = nmot,
                        isym = isym,
                        mother = "",
                        msym = isym,
                        p = sc.nm$p[1] + sc.nm$p[2],
                        e = sc.nm$e[1] + sc.nm$e[2],
                        step = "",
                        node.type = "Node",
                        left = sc.nm$isym[2],
                        right = sc.nm$isym[1]
                        )
            nsc <- as.data.frame(
                rbind(sc, merged),
                stringsAsFactors=FALSE
                )
            nsc$p <- as.numeric(nsc$p)
            nsc$e <- as.numeric(nsc$e)
            ##print(nsc)
            hc.concat.two(nsc)
        }
    }

    get.code <- function(hc, symbol) {
        idx <- which(hc$symbol == symbol)
        s <- hc$step[idx]
        msym <- hc$mother[idx]
        if (msym == "") {
            s
        } else {
            paste(s, get.code(hc, msym), sep="")
        }
    }


    final.hc <-  hc.concat.two(symbol.codes)

    final.hc$code <- sapply(final.hc$symbol,
                            function(s) get.code(final.hc, s)
                            )
    final.hc$generation <- nchar(final.hc$code)
    row.names(final.hc) <- final.hc$isym
    ##  final.hc$loss <- with(final.hc, e + p * log2(p))
    final.hc$loss <- 0
    final.hc$pruned <- FALSE
    final.hc$leaved <- FALSE
    ##subset(final.hc, symbol %in% nxs)
    final.hc
}

loss.pruned <- function(hc) {
    ## expect hc to be the result of huffman.code
    with(hc, e + p * log2(p))
}

subtree <- function(hc, iroot) {
    ##iroot is the isym of the root to get the subtree under
    root <- hc[iroot,]
    if(root$node.type == 'Leaf') {
        iroot
    }
    else {
        ileft <- subtree(hc, root$left)
        iright <- subtree(hc, root$right)
        c(iroot, ileft, iright)
    }
}

loss.updated <- function(hc, node) {
    nmsym <-hc[node, 'mother']
    inmsym <- if (nmsym == "") NA  else with(hc, isym[symbol==nmsym])
    ##print(paste( "------------------------update tree looking up from",
    ##      node, "to", inmsym))
    if (hc[node, 'node.type'] == 'Leaf' |
        hc[node, 'leaved'] ) {
        ne <- with(hc[node,], - p *log2(p))
        newloss <- hc[node, 'loss'] +  hc[node, 'e'] - ne
        ##print(paste("-------------------loss  from", hc[node, 'loss'], "to", newloss))
        ##print(paste("-------------------e  from", hc[node, 'e'], "to", ne))
        hc[node, 'loss'] <- newloss
        hc[node, 'e'] <- ne
    } else {
        lsym <- hc[node, 'left']
        rsym <- hc[node, 'right']
        ne <- hc[lsym, 'e'] + hc[rsym, 'e']
        newloss <- hc[node, 'loss'] +  hc[node, 'e'] - ne
        ##print(paste("-------------------loss  from", hc[node, 'loss'], "to", newloss))
        ##print(paste("-------------------e  from", hc[node, 'e'], "to", ne))
        hc[node, 'loss'] <- newloss
        hc[node, 'e'] <- ne
    }
    if(is.na(inmsym)) {
        hc
    } else {
        loss.updated(hc, inmsym)
    }
}



cuthoff <- function(hc, max.loss=1) {
    root <- hc[hc$mother == "", 'isym']
    loss <- hc[root, 'loss']
    ##print(paste("lossed", loss))
    hc.old <- hc
    hc <- hc[order(with(hc, e + p * log2(p)),
                        decreasing=TRUE), ]
    choices <- which(hc$loss <= max.loss &
                         hc$node.type != 'Leaf' &
                             !hc$leaved &
                                 !hc$pruned
                     )

    if(length(choices) == 0 | loss >= max.loss) {
        hc.old
    }
    else {
        ip  <- hc[choices[length(choices)], 'isym']
        stp <- subtree(hc, ip)
        hc[stp, 'code'] <- hc[ip, 'code']
        hc[stp, 'pruned'] <- TRUE
        hc[ip, 'leaved'] <- TRUE
        hc[ip, 'pruned'] <- FALSE
        hc <- loss.updated(hc, ip)
        newloss <- hc[root, 'loss']
        if (newloss >= max.loss) {
      ##      print("---------------------rollback")
            hc.old
        } else {
            cuthoff(hc, max.loss)
        }
        ##hc
    }
}
