## cutting huffman tree is buggy.
## lets try a distribution we know

pdist <- c(a=0.25, b=0.25, c=0.2, d=0.15, e=0.15)
N <- 10000
xs <- c()
for(x in names(pdist)){
    xs <- c(xs, rep(x, N*pdist[x]))
}


hcols <- c("isym", "msym",
           "p", "e", "loss",
           "node.type", "left", "right",
           "pruned", "leaved", "code")


hc <- huffman.code(xs)
chc <- cuthoff(hc, max.loss=0.1)
subset(chc,
       node.type=="Leaf"
       )[, c('isym', 'p', 'code')]

