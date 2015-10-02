## compute area under the curve

roc.auc <- function(predictions, labels) {
    library(ROCR)
    pprd <- prediction(predictions=predictions, labels=labels)
    plot(performance(pprd, 'tpr', 'fpr'))
    auc <- performance(pprd, 'auc')
    unlist(slot(auc, "y.values"))
}

