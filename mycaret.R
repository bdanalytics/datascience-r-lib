mycaret.nearZeroVar <- function(x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE,
          foreach = FALSE, allowParallel = TRUE, ...)
{
    if (packageVersion("caret") != '6.0.68')
        stop("mycaret.nearZeroVar: Check if the bug is fixed, else create pull request")
    loadNamespace("caret")

    if (!foreach)
        return(nzv(x, freqCut = freqCut, uniqueCut = uniqueCut,
                   saveMetrics = saveMetrics, ...))
    # `%op%` <- getOper(foreach && allowParallel && getDoParWorkers() >
    `%op%` <- caret:::getOper(foreach && allowParallel && getDoParWorkers() >
                          1)
    if (saveMetrics) {
        res <- foreach(name = colnames(x), .combine = rbind) %op%
        {
            # This doesn't work when name is "0" / 0
            # r <- nzv(x[[name]], freqCut = freqCut, uniqueCut = uniqueCut,
            r <- nzv(x[, name], freqCut = freqCut, uniqueCut = uniqueCut,
                     saveMetrics = TRUE, ...)
            r[, "column"] <- name
            r
        }
        res <- res[, c(5, 1, 2, 3, 4)]
        rownames(res) <- as.character(res$column)
        res$column <- NULL
    }
    else {
        res <- foreach(name = colnames(x), .combine = c) %op%
        {
            # nzv(x[[name]], freqCut = freqCut, uniqueCut = uniqueCut,
            nzv(x[, name], freqCut = freqCut, uniqueCut = uniqueCut,
                    saveMetrics = FALSE, ...)
            if (length(r) > 0 && r == 1) 
                              TRUE
                            else FALSE     
        }                      
        res <- which(res)
        if (names) {
            res <- colnames(x)[res]                                         
        }
    }
    res
}