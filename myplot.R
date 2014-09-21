suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(doBy))

#ggplot(mtcars, aes(x=mpg)) + 
#	geom_histogram() + 
#	geom_vline(aes(xintercept=mean(mtcars$mpg), linetype="dotted"),
#             show_guide = TRUE) + 
#    geom_vline(aes(xintercept=median(mtcars$mpg), linetype="dashed"), 
#             show_guide = TRUE) + 
#    #theme(legend.position="top", legend.direction="horizontal")
#    scale_linetype_manual("V-lines" 
#    					  #, breaks=c("mean", "median")
#    					  , values=c("dotted", "dashed")
#    					  , labels=c("mean", "median")
#    					  )

myplot_box <- function(df, ycol_names, xcol_name=NULL, facet_spec=NULL) {
    if ((length(ycol_names) > 1) & (!missing(xcol_name)))
        stop("Multiple feats not implemented with x variable.", 
             "\n  Consider using facet parameter instead.")
    
    if (!is.factor(df[, xcol_name])) {
        xcol_name_par <- xcol_name
        xcol_name <- paste(xcol_name_par, "fctr", sep="_")
        warning("xcol_name:", xcol_name_par, " is not a factor; creating ", xcol_name)
        df[, xcol_name] <- as.factor(df[, xcol_name_par])
    }
    
    if (length(ycol_names) == 1) {
        if (missing(xcol_name)) {
            medians_df <- summaryBy(as.formula(paste0(ycol_names, " ~ factor(0)")), df, 
                                    FUN=c(median), na.rm=TRUE)
            g <- ggplot(df, aes_string(x=factor(0), y=ycol_names))
            g <- g + xlab(" ")            
        } else {
            medians_df <- summaryBy(as.formula(paste0(ycol_names, " ~ ", xcol_name)), df, 
                                    FUN=c(median), na.rm=TRUE)
            g <- ggplot(df, aes_string(x=xcol_name, y=ycol_names))            
        }
    } else {
        require(reshape)
        mltd_df <- melt(df,, measure.vars=ycol_names)
        require(doBy)        
        medians_df <- summaryBy(value ~ variable , mltd_df, FUN=c(median), na.rm=TRUE)
        g <- ggplot(mltd_df, aes(x=variable, y=value))
        g <- g + xlab(" ")
    }
    
    if (!is.null(facet_spec)) {
        stop("facets not supported yet")
        require(doBy)
        sum_df <- summaryBy(steps ~ . , df, FUN=c(median))
    } else {
    }
    
    g <- g + geom_boxplot() + stat_summary(fun.y=mean, pch=22, geom='point', color='red') +
             scale_y_continuous(labels=myformat_number)
    
    if (length(ycol_names) == 1) {
        aes_str <- paste0("y=", ycol_names, ".median * 1.05", 
                          ", label=myformat_number(round(", ycol_names, ".median))")
        aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
        g <- g + geom_text(data=medians_df, 
                           mapping=aes_mapping
                           , color="NavyBlue", size=3.5)
    } else {
        #print(medians_df)
        g <- g + geom_text(data=medians_df,
                           mapping=aes_string(x="variable", 
                                              y="value.median * 1.05",
                                              label="myformat_number(round(value.median))")
                           , color="NavyBlue", size=3.5)
        #print("median text layer applied")
    }
    
    g
}

myplot_hbar <- function(df, xcol_name, ycol_names, xlabel_formatter=NULL, facet_spec=NULL) {
    require(ggplot2)
    
    # Summarize data by xcol_name to get the proper display order 
    if (length(unique(df[, xcol_name])) != dim(df)[1]) {
        require(sqldf)
        sql <- paste0("SELECT ", xcol_name, ", ")
        sum_sub_clauses <- sapply(ycol_names, 
                                  function (col) { paste0(" SUM(", col, ") AS ",
                                                          col) })
        sum_clause <- paste(sum_sub_clauses, collapse=",")
        sql <- paste(sql, sum_clause, "FROM df GROUP BY", xcol_name, sep=" ")
        warning("Aggregating input dataframe:", sql)
        df <- sqldf(sql)
    }
    
    if (length(ycol_names) == 1) {
        df <- df[order(df[, ycol_names], decreasing=TRUE), ]
        df$xcol <- df[, xcol_name]
        df$ycol <- df[, ycol_names]
        g <- ggplot(df, aes(x=reorder(xcol, ycol), y=ycol))
        #g <- ggplot(df, aes_string(x=xcol_name, y=ycol_names))
        g <- g + geom_bar(stat="identity", colour="blue") + 
                 xlab(xcol_name) + ylab(ycol_names)
    } else {
        require(reshape)
        mltd_df <- melt(df, id=xcol_name, measure=ycol_names)
        #g <- ggplot(mltd_df, aes_string(x=xcol_name, y="value", fill="variable"))
        names(mltd_df)[1] <- "xcol_name"
        g <- ggplot(mltd_df, aes(x=reorder(xcol_name, value), y=value, fill=variable))        
        g <- g + geom_bar(stat="identity") + xlab(xcol_name)
    }
    
    if (!is.null(facet_spec) & (length(ycol_names) > 1)) {
        stop("Facet with multiple y cols not supported")
    }
    
    if (!(is.null(xlabel_formatter))) {
        g <- g + scale_x_discrete(labels=xlabel_formatter)
    }

    g + coord_flip()
}
#myplot_hbar(head(mysort_df(interval_activity_df, "steps.mean", desc=TRUE)), 
#            "interval", "steps.mean", xlabel_formatter=myformat_time_MS)
#myplot_hbar(health_event_grp_df, "event_grp", c("FATALITIES", "INJURIES"))
#myplot_hbar(health_event_grp_df, "event_grp", c("FATALITIES", "INJURIES"), 
#			facet_spec="whatever")

myplot_histogram <- function(df, hst_col_name, fill_col_name=NULL, 
                             show_stats=TRUE, facet_frmla=NULL) {
    require(ggplot2)
    
    if (is.null(fill_col_name)) {
        # Fill with raw counts
        g <- ggplot(df, aes_string(x=hst_col_name))
        g <- g + geom_histogram(aes(fill=..count..)) + 
                 scale_fill_gradient("Count", low="red", high="blue")           
    }
    else {
        # If fill variable has 5 or less unique values use raw data 
        if (length(unique(df[, fill_col_name])) <= 5) {
            
            # if fill variable is a factor use raw data
            if (class(df[, fill_col_name]) == "factor") {
                g <- ggplot(df, aes_string(x=hst_col_name, fill=fill_col_name))
            } else {
                # else create a factor of the fill variable    
                fill_col_fctr_name <- paste0(fill_col_name, "_fctr")
                df[, fill_col_fctr_name] <- as.factor(df[, fill_col_name])
                g <- ggplot(df, aes_string(x=hst_col_name, 
                                           fill=fill_col_fctr_name))
            }
        } else {
            # else fill with 5 groups of the data    
            fill_col_grp_name <- paste0(fill_col_name, "_grp")
            df[, fill_col_grp_name] <- cut(df[, fill_col_name], 5) 
            # Why does cut create labels with -ve values although min is 0 ?
            
            g <- ggplot(df, aes_string(x=hst_col_name, fill=fill_col_grp_name))
        }
        g <- g + geom_bar()
    }
    
    g <- g + scale_y_continuous(labels=myformat_number)
    if ((class(df[, hst_col_name]) == "integer") | 
        (class(df[, hst_col_name]) == "number"))
    	g <- g + scale_x_continuous(labels=myformat_number)
    
    # Add median & mean as vertical lines
    #aes_str <- paste0("xintercept=mean(df[, \"", hst_col_name, "\"], na.rm=TRUE),
    #                  linetype=\"", "dotted", "\"")
    #aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
    #g <- g + geom_vline(mapping=aes_mapping, show_guide=TRUE)

    if (show_stats) {
        g <- g + geom_vline(aes_string(xintercept=mean(df[, hst_col_name], na.rm=TRUE),
                                       linetype="\"dotted\""), show_guide=TRUE) 
        g <- g + geom_vline(aes_string(xintercept=median(df[, hst_col_name], na.rm=TRUE),
                                       linetype="\"dashed\""), show_guide=TRUE)         
    }

    # Add number of missing values as a horizontal line
    num_na <- sum(is.na(df[, hst_col_name]))
    if (num_na == 0) 
        g <- g + scale_linetype_manual(name="Stats", values=c("dotted", "dashed"), 
                                       labels=c("mean", "median"))
    else {
        g <- g + geom_hline(aes_string(yintercept=num_na,
                                       linetype="\"dotdash\""), show_guide=TRUE)  
        g <- g + scale_linetype_manual(name="Stats", values=c("dotted", "dashed", "dotdash"), 
                                       labels=c("mean", "median", "missing"))

    }
    
    #if ((class(facet_frmla) == "formula") | (!is.na(facet_frmla)))
    if (!missing(facet_frmla))
        g <- g + facet_grid(facet_frmla)
        
    # Lines legend messes up the fill legend
    g + guides(fill=guide_legend(override.aes=list(linetype=0)))
    
}
#myplot_histogram(entity_agg_date_df, "steps_sum", fill_col_name="date_dytyp")

myplot_line <- function(df, xcol_name, ycol_names, xlabel_formatter=NULL, 
                        facet_row_colnames=NULL, facet_col_colnames=NULL) {
    require(ggplot2)
    
    if (length(ycol_names) == 1) {
        if (class(df[, xcol_name]) == "character") df$xcol <- unclass(as.factor(df[, xcol_name]))
        else df$xcol <- df[, xcol_name]
        df$ycol <- df[, ycol_names]
        g <- ggplot(df, aes(x=xcol, y=ycol))
        g <- g + geom_line(colour="blue") + xlab(xcol_name) + ylab(ycol_names) 
    } else {
        require(reshape)
        id.vars <- xcol_name
        if (!(is.null(facet_row_colnames))) id.vars <- c(id.vars, facet_row_colnames)
        if (!(is.null(facet_col_colnames))) id.vars <- c(id.vars, facet_col_colnames)
        mltd_df <- melt(df, id=id.vars, measure=ycol_names)
        #myprint_df(mltd_df)
        g <- ggplot(mltd_df, aes_string(x=xcol_name))
        g <- g + geom_line(aes(y=value, colour=variable)) 
    }
    
    if (!(is.null(facet_row_colnames)) | !(is.null(facet_col_colnames))) {
        if (is.null(facet_row_colnames)) facet_row_colnames <- "."
        if (is.null(facet_col_colnames)) facet_col_colnames <- "."
        facet_formula <- reformulate(facet_col_colnames, facet_row_colnames)
        g <- g + facet_grid(facet_formula)
    }
    
    if (!(is.null(xlabel_formatter))) {
        g <- g + scale_x_continuous(labels=xlabel_formatter)            
    }
    
    g
}
#myplot_line(interval_activity_df, "interval", "steps.mean", 
#			xlabel_formatter=myformat_time_MS)
#myplot_line(prdct_feats_df, "fStep", c("trainErrorRate", "testErrorRate"), 
#            facet_spec="fStepGrp ~ .")
#myplot_line(prdct_feats_df, "fStep", c("trainErrorRate", "testErrorRate"))
#myplot_line(prdct_feats_df, "fStep", "trainErrorRate")

# plot.ly
myplot_plotly <- function(ggplot_obj) {
    suppressPackageStartupMessages(require(plotly))
    
    py <- plotly()
    
    pyout <- py$ggplotly(ggplot_obj)
    return(pyout$response$url)
}

myplot_scatter <- function(df, xcol_name, ycol_name, 
                           colorcol_name=NULL, jitter=FALSE, smooth=FALSE,
                           facet_rowcol_name=".", facet_colcol_name=".",
                           ylabel=NULL,
                           stats_df=NULL, predict_df=NULL, i_pkg="NULL") {

    if (!missing(i_pkg) & (i_pkg == "plotly") & is.factor(df[, xcol_name]))
        stop("plotly rendering of xvar as factor crashes")
        
    p <- ggplot(df, aes_string(x=xcol_name, y=ycol_name))
    
    if (!missing(colorcol_name))
        p <- p + geom_point() + aes_string(color=colorcol_name)
    else
        p <- p + geom_point(color="grey")
    
    facets <- paste(facet_rowcol_name, '~', facet_colcol_name)
    if (facets != '. ~ .')
        p <- p + facet_grid(facets)
    
    if (jitter)
        p <- p + geom_jitter()
    if (smooth)
        p <- p + geom_smooth()
    
    # Format y-axis
    if (!missing(ylabel))
        p <- p + ylab(ylabel)
    if (is.numeric(df[, ycol_name]))    
        p <- p + scale_y_continuous(labels=myformat_number)
    
    if (!missing(stats_df)) {
        # Display stats of x-axis feature
        aes_str <- paste0("linetype=\"dotted\", xintercept=as.numeric(", xcol_name, ")")
        aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))            
        p <- p + geom_vline(mapping=aes_mapping, 
                            data=stats_df, show_guide=TRUE)
        p <- p + scale_linetype_identity(guide="legend", name="Stats", labels=rownames(stats_df))
    }
    
    if (!missing(stats_df)) {
        # Plot the prediction point & conf. interval
        aes_str <- paste0("y=", ycol_name, ".predict.fit, x=", xcol_name)
        aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
        if (missing(i_pkg) | (i_pkg != "plotly"))
            p <- p + geom_point(aes_mapping,
                                data=predict_df,
                                color="red", pch=7, size=5)
    
        aes_str <- paste0(
            "ymax=", ycol_name, ".predict.upr, ymin=", ycol_name, ".predict.lwr, x=", xcol_name)
        aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
        if (missing(i_pkg) | i_pkg != "plotly")
            p <- p + geom_errorbar(aes_mapping,
                                   data=predict_df,    
                                   color="red", width=0.1)
    }                           
    
    # Plot the regression line
    p <- p + geom_smooth(method="lm")
    
    # linetype legend messes up the fill legend
    p <- p + guides(color=guide_legend(override.aes=list(linetype=0)))

    return(p)
}    