plotly()$ggplotly <- function (gg = last_plot(), 
                               kwargs = list(filename = NULL, fileopt = NULL, 
                                             width = NULL, height = NULL), 
                               session = "interactive") 
{
    if (!is.ggplot(gg)) {
        stop("gg must be a ggplot")
    }
    pargs <- gg2list(gg)
    if (!"auto_open" %in% names(kwargs)) {
        kwargs <- c(kwargs, auto_open = TRUE)
    }
    pargs$kwargs <- c(pargs$kwargs, kwargs)
    if (session == "interactive") {
        resp <- do.call(pub$plotly, pargs)
        if (pargs$kwargs$auto_open) {
            browseURL(resp$url)
        }
        invisible(list(data = pargs, response = resp))
    }
    else if (session == "notebook") {
        do.call(pub$irplot, pargs)
        invisible(list(data = pargs))
    }
    else if (session == "knitr") {
        do.call(pub$iplot, pargs)
        invisible(list(data = pargs))
    }
    else {
        stop("Value of session can be: 'interactive', 'notebook', or 'knitr'.")
    }
}