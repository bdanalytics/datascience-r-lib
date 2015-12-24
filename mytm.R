myreplacePunctuation <- function(x) {
    return(gsub("[[:punct:]]+", " ", gsub("\\b's", "", x)))
}

# myweightTfIdf <- function (m, normalize = TRUE) {
#     isDTM <- inherits(m, "DocumentTermMatrix")
#     if (isDTM)
#         m <- t(m)
#     if (normalize) {
#         cs <- col_sums(m)
#         if (any(cs == 0))
#             warning("empty document(s): ", paste(Docs(m)[cs ==
#                                                              0], collapse = " "))
#         names(cs) <- seq_len(nDocs(m))
#         m$v <- m$v/cs[m$j]
#     }
#     rs <- row_sums(m > 0)
#     if (any(rs == 0))
#         warning("unreferenced term(s): ", paste(Terms(m)[rs ==
#                                                              0], collapse = " "))
#     lnrs <- log2(nDocs(m)/rs)
#     lnrs[!is.finite(lnrs)] <- 0
#     m <- m * lnrs
#     attr(m, "weighting") <- c(sprintf("%s%s", "term frequency - inverse document frequency",
#                                       if (normalize) " (normalized)" else ""), "tf-idf")
#     if (isDTM)
#         t(m)
#     else m
# }
# class(myweightTfIdf) <- c("WeightFunction", class(myweightTfIdf))
# attr(myweightTfIdf, "name") <- "term frequency - inverse document frequency"
# attr(myweightTfIdf, "acronym") <- "tf-idf"

myweightTflog1p <- function (m)
{
    m$v <- log1p(m$v)
    attr(m, "weighting") <- c("log1p(term frequency)", "log1p(tf)")
    return(m)
}
myweightTflog1p <- structure(myweightTflog1p, class=c("WeightFunction", class(myweightTflog1p)),
                             name="log1p(term frequency)", acronym="tflog1p")