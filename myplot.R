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

# Potential Enhancements: Default to .frq if ycol_names = NULL
myplot_bar <- function(df, xcol_name, ycol_names, colorcol_name=NULL, facet_spec=NULL,
                        xlabel_formatter=NULL) {
    require(ggplot2)
    require(RColorBrewer)

    if (xcol_name == ".rownames")
        df[, xcol_name] <- rownames(df)

    # This is not always required; need to make this feature an argument
    # Summarize data by xcol_name to get the proper display order
#     if (length(unique(df[, xcol_name])) != dim(df)[1]) {
#         require(sqldf)
#         sql <- paste0("SELECT ", xcol_name, ", ")
#         sum_sub_clauses <- sapply(ycol_names,
#                                   function (col) { paste0(" SUM(", col, ") AS ",
#                                                           col) })
#         sum_clause <- paste(sum_sub_clauses, collapse=",")
#         sql <- paste(sql, sum_clause, "FROM df GROUP BY", xcol_name, sep=" ")
#         warning("Aggregating input dataframe:", sql)
#         df <- sqldf(sql)
#     }

    sum_df <- mycompute_stats_df(df = df, byvars_vctr = xcol_name)

    if (length(ycol_names) == 1) {
        df <- df[order(df[, ycol_names], decreasing = TRUE), ]
        g <- ggplot(df, aes_string(x = paste0("reorder(", xcol_name, ", ", ycol_names, ")"),
                                   y = ycol_names))
        if (is.null(colorcol_name)) g <- g + geom_bar(fill = "blue", stat = "identity") else
                            g <- g + geom_bar(aes_string(fill = colorcol_name), stat = "identity") +
                                    scale_color_brewer(type = "qual", palette = "Paired")
        g <- g + xlab(xcol_name) + ylab(ycol_names)
    } else {
        require(reshape2)
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

    aes_str <- paste0("x=", xcol_name,
                    ", y=", ycol_names, ".sum * 1.05",
                    ", label=myformat_number(round(", ycol_names, ".sum))")
    aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
    g <- g + geom_text(mapping=aes_mapping, data=sum_df, size=3.5)

    return(g)
}
#myplot_hbar(head(mysort_df(interval_activity_df, "steps.mean", desc=TRUE)),
#            "interval", "steps.mean", xlabel_formatter=myformat_time_MS)
#myplot_hbar(health_event_grp_df, "event_grp", c("FATALITIES", "INJURIES"))
#myplot_hbar(health_event_grp_df, "event_grp", c("FATALITIES", "INJURIES"),
#			facet_spec="whatever")

myplot_box <- function(df, ycol_names, xcol_name=NULL, facet_spec=NULL) {
    if ((length(ycol_names) > 1) & (!missing(xcol_name)))
        stop("Multiple feats not implemented with x variable.",
             "\n  Consider using facet parameter instead.")

    if (!is.null(xcol_name) && !is.factor(df[, xcol_name])) {
        xcol_name_par <- xcol_name
        xcol_name <- paste(xcol_name_par, "fctr", sep="_")
        warning("xcol_name:", xcol_name_par, " is not a factor; creating ", xcol_name)
        df[, xcol_name] <- as.factor(df[, xcol_name_par])
    }

    if (!is.null(facet_spec)) {
        stop("facets not supported yet")
        require(doBy)
        sum_df <- summaryBy(steps ~ . , df, FUN=c(median))
    }

    if (length(ycol_names) == 1) {
        if (is.null(xcol_name)) {
            medians_df <- mycompute_medians_df(df[, ycol_names, FALSE])
            g <- ggplot(df, aes_string(x=factor(0), y=ycol_names))
            g <- g + xlab(" ")
        } else {
#             medians_df <- summaryBy(as.formula(paste0(ycol_names, " ~ ", xcol_name)), df,
#                                     FUN=c(median), na.rm=TRUE)
            stats_df <- mycompute_stats_df(df=df[, c(ycol_names, xcol_name)],
                                               byvars_vctr=xcol_name)
            g <- ggplot(df, aes_string(x=xcol_name, y=ycol_names))
        }
    } else {
        require(reshape2)
        mltd_df <- melt(df,, measure.vars=ycol_names)
        require(doBy)
        medians_df <- summaryBy(value ~ variable , mltd_df, FUN=c(median), na.rm=TRUE)
        g <- ggplot(mltd_df, aes(x=variable, y=value))
        g <- g + xlab(" ")
    }

    g <- g + geom_boxplot(fill="grey80", color="blue") +
             stat_summary(fun.y=mean, pch=22, geom='point', color='red')

    offset = 1.05; offset_str = paste0(" * ", sprintf("%f", offset), " ")
    median_name <- grep(".median", names(stats_df), fixed=TRUE, value=TRUE)
    mean_name <- grep(".mean", names(stats_df), fixed=TRUE, value=TRUE)
    stats_df[, paste0(median_name, ".offset.mult")] <-
        ifelse(stats_df[, median_name] >= stats_df[, mean_name], 1.05, 0.95)
    #sav_g <- g
    if (length(ycol_names) == 1) {
        if (class(df[, ycol_names]) == "num")
            g <- g + scale_y_continuous(labels=myformat_number)

        if (!inherits(df[, ycol_names], "logical")) { # logical crashes- why ???
            aes_str <- paste0("y=", ycol_names, ".median", " * ",
                              ycol_names, ".median.offset.mult ",
                                ", label=myformat_number(round(", ycol_names, ".median))")
    #                           ", label=", ycol_names, ".median")
            aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
            g <- g + geom_text(data=stats_df,
                               mapping=aes_mapping
                               , color="NavyBlue", size=3.5)
        }
    } else {
        g <- g + geom_text(data=stats_df,
                           mapping=aes_string(x="variable",
                                              y="value.median", offset_str,
                                              label="myformat_number(round(value.median))")
                           , color="NavyBlue", size=3.5)
    }

    return(g)
}

myplot_hbar <- function(df, xcol_name, ycol_names, xlabel_formatter=NULL, facet_spec=NULL, ...)
    return(myplot_bar(df, xcol_name, ycol_names, xlabel_formatter, facet_spec, ...) +
               coord_flip())

myplot_histogram <- function(df, hst_col_name, fill_col_name=NULL,
                             show_stats=TRUE, facet_frmla=NULL) {
    require(ggplot2)

    if (inherits(df[, hst_col_name], "character")) {
        warning("converting ", hst_col_name, " to class:factor")
        df[, hst_col_name] <- as.factor(df[, hst_col_name])
    }

    if (is.null(fill_col_name)) {
        # Fill with raw counts
        p <- ggplot(df, aes_string(x = hst_col_name))
        p <- p + geom_histogram(aes(fill = ..count..))
        p <- p + scale_fill_gradient("Count", low = "red", high = "blue")
    }
    else {
        # If fill variable has 7 or less unique values use raw data
        if (length(unique(df[, fill_col_name])) <= 7) {

            # if fill variable is a factor use raw data
            if (class(df[, fill_col_name]) == "factor") {
                p <- ggplot(df, aes_string(x=hst_col_name, fill=fill_col_name))
            } else {
                # else create a factor of the fill variable
                fill_col_fctr_name <- paste0(fill_col_name, "_fctr")
                df[, fill_col_fctr_name] <- as.factor(df[, fill_col_name])
                p <- ggplot(df, aes_string(x=hst_col_name,
                                           fill=fill_col_fctr_name))
            }
        } else {
            # else fill with 5 groups of the data
            fill_col_grp_name <- paste0(fill_col_name, "_grp")
            df[, fill_col_grp_name] <- cut(df[, fill_col_name], 5)
            # Why does cut create labels with -ve values although min is 0 ?

            p <- ggplot(df, aes_string(x=hst_col_name, fill=fill_col_grp_name))
        }
        p <- p + geom_bar()
    }

    p <- p + scale_y_continuous(labels=myformat_number)
    if ((class(df[, hst_col_name]) == "integer") |
        (class(df[, hst_col_name]) == "number"))
        p <- p + scale_x_continuous(labels=myformat_number)

    if (show_stats) {
        if (is.numeric(df[, hst_col_name]))
            p <- p + geom_vline(aes_string(
                                xintercept = mean(df[, hst_col_name], na.rm = TRUE),
                                            linetype = "\"dotted\""))
        p <- p + geom_vline(aes_string(
                                xintercept = mycompute_median(df[, hst_col_name]),
                                        #linetype = "\"dashed\""), show_guide=TRUE)
                                            linetype = "\"dashed\""))
    }

    # Add number of missing values as a horizontal line
    num_na <- sum(is.na(df[, hst_col_name]))
    if (num_na > 0)
        p <- p + geom_hline(aes_string(yintercept = num_na,
                                       #linetype="\"dotdash\""), show_guide=TRUE)
                                        linetype = "\"dotdash\""))

    #if ((class(facet_frmla) == "formula") | (!is.na(facet_frmla)))
    if (mycheck_validarg(facet_frmla))
        p <- p + facet_grid(facet_frmla)

    if (show_stats) {
        # Display stats legend
        stats_legend_labels <- c("median")
        if (is.numeric(df[, hst_col_name]))
            stats_legend_labels <- c(stats_legend_labels, "mean")
        if (num_na > 0)
            stats_legend_labels <- c(stats_legend_labels, "missing")

        p <- p + scale_linetype_identity(guide="legend", name="Stats",
                                         labels=stats_legend_labels)
        # Lines legend messes up the fill legend
        p <- p + guides(fill=guide_legend(override.aes=list(linetype=0)))
    }

    return(p)
}
#print(myplot_histogram(entity_agg_date_df, "steps_sum", fill_col_name="date_dytyp"))

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
        require(reshape2)
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

    return(g)
}
#myplot_line(interval_activity_df, "interval", "steps.mean",
#			xlabel_formatter=myformat_time_MS)
#myplot_line(prdct_feats_df, "fStep", c("trainErrorRate", "testErrorRate"),
#            facet_spec="fStepGrp ~ .")
#myplot_line(prdct_feats_df, "fStep", c("trainErrorRate", "testErrorRate"))
#myplot_line(prdct_feats_df, "fStep", "trainErrorRate")

mypltModelStats <- function(df, measure, dim = NULL, scaleXFn = NULL, highLightIx = NULL,
                            title = NULL, fileName = NULL) {
    if (is.null(dim))
        dim <- setdiff(names(df), measure)

    df <- df[, c(measure, dim)]

    pltDf <- tidyr::gather_(df, 'key', 'value', gather_cols = measure)
    if (!is.null(highLightIx))
        bstDf <- tidyr::gather_(df[highLightIx, ], 'key', 'value',
                                gather_cols = measure)

    if (nrow(pltDf) > 0) {
        gp <- ggplot(pltDf, aes_string(x = dim[1], y = 'value'))

        if (length(dim) > 1) {
            aesStr <- sprintf("color = as.factor(%s)", dim[2])
            aesMap <- eval(parse(text = paste("aes(", aesStr, ")")))
            gp <- gp + geom_line(mapping = aesMap)
        } else
            gp <- gp + geom_line(color = 'blue')

        if (!is.null(scaleXFn) &&
            !is.null(scaleXFn[dim[1]])) {
            gp <- gp + switch(scaleXFn[dim[1]],
                              log10 = scale_x_log10(),
                              stop("switch error in mypltModelStats"))

            if (scaleXFn[dim[1]] == "log10") {
                #print("scaleXFn is log10")
                if (0 %in% unique(df[, dim[1]]))
                    for (key in measure) {
                        # hline if x-axis has log scale & x = 0 value needs to be highlighted
                        if (length(dim) > 1) {
                            aesStr <-
                                sprintf("yintercept = value, color = as.factor(%s)", dim[2])
                            aesMap <- eval(parse(text = paste("aes(", aesStr, ")")))
                            gp <- gp +
                                geom_hline(data = pltDf[(pltDf[, dim[1]] == 0  ) &
                                                            (pltDf[, 'key' ] == key) , ],
                                           mapping = aesMap,
                                           linetype = 'dashed')
                        } else
                            gp <- gp +
                                geom_hline(data = pltDf[(pltDf[, dim[1]] == 0  ) &
                                                            (pltDf[, 'key' ] == key) , ],
                                           aes(yintercept = value), color = 'blue',
                                           linetype = 'dashed')
                    }
            }
        }

        gp <- gp +
            ylab('') +
            scale_linetype_identity(guide = "legend") +
            theme(legend.position = "bottom")

        if (length(dim) < 3)
            gp <- gp + facet_grid(. ~ key,
                                  scales = "free", labeller = "label_both")
        gp <- gp + facet_grid(as.formula(paste(paste0(tail(dim, -2), collapse = "+"),
                                               "~ key")),
                              scales = "free", labeller = "label_both")

        if (!is.null(title))
            gp <- gp + ggtitle(title)

        if (!is.null(highLightIx))
            for (key in measure)
                gp <- gp + geom_point(data = bstDf[(bstDf$key == key), ],
                                      shape = 5, color = 'black', size = 3)

    }

    if (!is.null(fileName)) {
        savGP <- gp
        png(filename = fileName, width = 480 * 1, height = 480 * 1)
        print(gp)
        dev.off()

        gp <- savGP
    }

    return(gp)
}

mypltMultiple <- function(..., plotlist=NULL, cols) {
    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # Make the panel
    plotCols = cols                          # Number of columns of plots
    plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols

    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
    vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
        curRow = ceiling(i/plotCols)
        curCol = (i-1) %% plotCols + 1
        print(plots[[i]], vp = vplayout(curRow, curCol ))
    }

}

myplot_parcoord <- function (obs_df, obs_ix=1:nrow(obs_df), id_var=".rownames",
                             category_var=NULL) {
    require(lazyeval)

    # Setup id_df & remove id_var from range computation
    if (id_var != ".rownames") {
        id_df <- obs_df[obs_ix, id_var, FALSE]
        obs_df <- obs_df[, setdiff(names(obs_df), id_var), FALSE]
    } else id_df <- data.frame(.rownames=row.names(obs_df)[obs_ix])

    # Setup category_var -> Create a facet ???
    category_df <- id_df
    if (is.null(category_var)) {
        category_var <- ".category"; category_df[, category_var] <- as.factor(0)
    } else {
        category_df[, category_var] <- obs_df[obs_ix, category_var]
        obs_df <- obs_df[, setdiff(names(obs_df), category_var), FALSE]
    }


    ranges_mtrx <- apply(obs_df, 2L, range, na.rm = TRUE)
#     obs_scld_df <- as.data.frame(apply(obs_df, 2L, function(feat) {
#                     feat <- as.numeric(feat);
#                     feat_rng <- max(feat, na.rm = TRUE) - min(feat, na.rm = TRUE);
#                     feat_rng <- ifelse(feat_rng == 0, 1, feat_rng);
#                     return((feat - min(feat, na.rm = TRUE)) / feat_rng)
#                 }))
    # apply coerces data.frame to matrix; so factors are coerced into characters
    obs_scld_df <- as.data.frame(sapply(names(obs_df), function(feat) {
        feat <- as.numeric(obs_df[, feat]);
        feat_rng <- max(feat, na.rm = TRUE) - min(feat, na.rm = TRUE);
        feat_rng <- ifelse(feat_rng == 0, 1, feat_rng);
        return((feat - min(feat, na.rm = TRUE)) / feat_rng)
    }))

    obsT_df <- as.data.frame(t(obs_df))
    names(obsT_df) <- paste(".obs", names(obsT_df), sep=".");
    obsT_df$.var.name <- row.names(obsT_df)
    obsT_df$.var.pos <- 1:length(row.names(obsT_df))

    obsST_df <- as.data.frame(t(obs_scld_df))
    names(obsST_df) <- paste(".obs", names(obsST_df), sep=".");
    obsST_df$.var.name <- row.names(obsST_df)
    obsST_df$.var.pos <- 1:length(row.names(obsST_df))
    plt_violin_df <- tidyr::gather(obsST_df, key=obs, value=value, -.var.name, -.var.pos)

    obsHST_df <- as.data.frame(t(obs_scld_df[obs_ix, ]));
    names(obsHST_df) <- as.character(id_df[, id_var])
    obsHST_df$.var.name <- row.names(obsHST_df)
    obsHST_df$.var.pos <- 1:length(row.names(obsHST_df))
    plt_obsHST_df <- tidyr::gather_(obsHST_df, key_col = interp(id_var), value_col = "value",
        gather_cols = names(obsHST_df)[!grepl("(\\.var\\.name|\\.var\\.pos)",
                                             names(obsHST_df))])
    #plt_obsHST_df <- tidyr::gather(obsHST_df, id, value, -.var.name, -.var.pos)

    ranges_df <- cbind(as.data.frame(ranges_mtrx),
                       data.frame(.type = c("min", "max")))
    ranges_df <- tidyr::gather(ranges_df, key = .var, value = value, -.type)
    ranges_df$.y <- ifelse(ranges_df$.type == "min", -0.05, 1.05)
    ranges_df <- merge(ranges_df, obsT_df[, c(".var.name", ".var.pos")],
                       by.x = ".var", by.y = ".var.name", all.x = TRUE)
    ranges_df$.x <- ranges_df$.var.pos
    ranges_df <- subset(ranges_df, select = -.var.pos)
    ranges_df$label <- myformat_number(ranges_df$value)
    if (length(ix <- which(ranges_df$label %in% "NA")) > 0)
        ranges_df[ix, "label"] <- ranges_df[ix, "value"]

    plt_obsHST_df <- merge(plt_obsHST_df, category_df, x.all = TRUE)
    #     plt_obsHST_df[, category_var] <- NA
    #     plt_obsHST_df[plt_obsHST_df[, id_var] == 11448, glb_category_var] <- "Unknown#0"
    #     plt_obsHST_df[plt_obsHST_df[, id_var] == 11581, glb_category_var] <- "iPad4#1"
    #     plt_obsHST_df[plt_obsHST_df[, id_var] == 11583, glb_category_var] <- "Unknown#0"
    gp <- ggplot(plt_obsHST_df, aes(x=reorder(.var.name, .var.pos), y=value)) +
        geom_violin(data=plt_violin_df, aes(x=reorder(.var.name, .var.pos), y=value),
                    color="grey80", scale="width") +
        geom_line(data=plt_obsHST_df,
                  aes_string(group=id_var, color=id_var, linetype=category_var), size=1) +
        geom_point(data=plt_obsHST_df, aes_string(shape=category_var), size=3) +
        scale_color_brewer(type="qual", palette="Set1") +
        geom_vline(xintercept=1:length(names(obs_df)), color="grey50") +
        geom_text(data = ranges_df,
                  aes_string(x = ".x", y = ".y", label = "label"),
                  size = 3.5) +
        theme(axis.text.x=element_text(hjust=1, angle=45),
              axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
        xlab("") + ylab("")
    # ggtitle("Dummy")

    return(gp)
}

# plot.ly
myplot_plotly <- function(ggplot_obj) {
    suppressPackageStartupMessages(require(plotly))

    py <- plotly()

    pyout <- py$ggplotly(ggplot_obj)
    return(pyout$response$url)
}

myplot_prediction_classification <- function(df, feat_x, feat_y,
                            rsp_var, rsp_var_out, id_vars, prob_threshold=NULL) {

    if (feat_x == ".rownames")
        df[, ".rownames"] <- rownames(df)

    if (is.factor(df[, rsp_var])) { color_var <- rsp_var } else {
        color_var <- paste0(rsp_var, ".fctr")
        df[, color_var] <- as.factor(df[,rsp_var])
    }

    predct_accurate_var_name <- paste0(rsp_var_out, ".accurate")
    df[,  predct_accurate_var_name] <-
        (df[,rsp_var] == df[,rsp_var_out])
    predct_error_var_name <- paste0(rsp_var_out, ".error")
    df[, predct_error_var_name] <- 0
    predct_prob_var_name <- paste0(rsp_var_out, ".prob")
    tmp_vars <- grep("All\\.X", names(df), value=TRUE)
    #head(df[, c(glb_id_var, tmp_vars)])
    if (!all(is.na(df[, predct_accurate_var_name]))) {
        if (glb_is_binomial) {
            df[!df[, predct_accurate_var_name],  predct_error_var_name] <-
                df[!df[, predct_accurate_var_name], predct_prob_var_name] -
                prob_threshold
        } else {
            df[!df[, predct_accurate_var_name],  predct_error_var_name] <-
                1 - df[!df[, predct_accurate_var_name], predct_prob_var_name]
        }
    }

    # Attach labels to to prediction errors
    df$.label <- ""
    for (feat in c(feat_x, feat_y)) {
        if (class(df[, feat]) == "factor")
            next

        for (row_ix in c(head(which.min(df[, feat])),
                         head(which.max(df[, feat])),
                         head(which(!df[,  predct_accurate_var_name] &
                                 df[, feat] == min(df[, feat]))),
                         head(which(!df[,  predct_accurate_var_name] &
                                 df[, feat] == max(df[, feat])))
                         ))
            df[row_ix, ".label"] <-
                        ifelse(length( id_vars) > 0,
                                paste(df[row_ix,  id_vars], collapse=":"),
                                paste0(".", rownames(df)[row_ix]))
    }

    dsp_vars <- c(id_vars, grep(rsp_var, names(df), value = TRUE))
    if (any(!is.na(df[, predct_accurate_var_name]))) {
        print("Min/Max Boundaries: ")
        myprint_df(df[, c(dsp_vars, ".label")] %>%
            subset(.label != "") %>%
            arrange_(interp(~var, var = as.name(predct_error_var_name))))
#         myprint_df(orderBy(reformulate(predct_error_var_name),
#                            subset(df[, c(dsp_vars, ".label")], .label != "")))
        print("Inaccurate: ")
        myprint_df(df[!df[, predct_accurate_var_name], c(dsp_vars)] %>%
            arrange_(interp(~var, var = as.name(predct_error_var_name))))
#         myprint_df(orderBy(reformulate(predct_error_var_name),
#                            df[!df[, predct_accurate_var_name], c(dsp_vars)]))

        plt_df <- df
        tmpName <- gsub("\\*", "_", predct_accurate_var_name)
        plt_df[, tmpName] <- plt_df[, predct_accurate_var_name]
        gp <- ggplot(plt_df, aes_string(x = feat_x, y = feat_y)) +
            geom_point(aes_string(color = color_var,
                                  shape = tmpName),
                       position = "jitter") +
            scale_shape_manual(values = c(4,3)) + guides(shape = FALSE) +
            geom_text(aes_string(label = ".label"), color = "NavyBlue",
                      size = 3.5) +
            facet_wrap(reformulate(tmpName)) +
            scale_color_brewer(type = "qual", palette = "Set1")
        return(gp)
    } else return(NULL)
}

myplot_prediction_regression <- function(df, feat_x, feat_y, rsp_var, rsp_var_out, id_vars) {

    predct_err_name <- paste0(rsp_var_out, ".err")
    df[, predct_err_name] <-
        abs(df[,rsp_var] - df[,rsp_var_out])

    # Add labels to top 5 prediction errors
    df <- orderBy(reformulate(c("-", predct_err_name)), df)
    df$.label <- " "
    df$.label[1:min(5, nrow(df))] <- sapply(1:min(5, nrow(df)), function(row_ix)
        df[row_ix, ".label"] <- paste0(df[row_ix,  id_vars], collapse = ":"))
    print(head(df, 5))

    gp <- myplot_scatter(df, feat_x, feat_y) +
                         geom_text(aes_string(label = ".label"), color = "NavyBlue", size = 3.5)
    if (!all(is.na(df[, predct_err_name])))
        gp <- gp + geom_point(aes_string(size = predct_err_name), alpha = 0.4)

    return(gp)
}

##########################################
##Radar Plot Code
##########################################
myplot_radar <- function(radar_inp_df, instance_var=names(radar_inp_df)[1], facet_var=NULL) {

    ##Reorganizes radar_inp_df in the form:
    # instance plot_var1 plot_var2 plot_var3 plot_var4 facet
    # inst_lbl  -0.038   1.438      -0.571      0.832  facet_val
    ##where instance is the individual instance identifier
    ##facet is the facet variable
    ##and the variables from plot_var1..4 are used for grouping
    ##and thus should be individual lines on the radar plot

    radarFix <- function(df) {
        ##assuming the passed in data frame
        ##includes only columns listed above

        ##find increment
        theta <- seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-2))
        theta <- theta + pi/2

        ##create graph data frame
        graphData= data.frame(.instance="", x=0,y=0)
        graphData=graphData[-1,]

        for(i in as.character(df$.instance)) {
            instanceData <- df[df$.instance == i,]
            x0 <- y0 <- r <- 0.5

            for(j in c(2:(ncol(df)-1))) {
                ##set minimum value such that it occurs at 0. (center the data at -3 sd)
                #instanceData[,j]= instanceData[,j] #+ 3

                graphData <- rbind(graphData,
                    data.frame(.instance=i,
                                theta=theta[j-1],
                                x=r * instanceData[,j] * cos(theta[j-1]) + x0,
                                y=r * instanceData[,j] * sin(theta[j-1]) + y0,
                                label=ifelse((instanceData[,j] == max(df[,j], na.rm=TRUE)),
                                                names(instanceData)[j],
                                                ""),
                                label_max=ifelse((instanceData[,j] == max(df[,j], na.rm=TRUE)),
                                                format(instanceData[,j], digits=6),
                                                NA),
                                label_min=ifelse((instanceData[,j] == max(df[,j], na.rm=TRUE)),
                                                format(min(df[,j], na.rm=TRUE), digits=6),
                                                NA),
                                label_angle=ifelse(((180*theta[j-1]/pi)-90) < 90,
                                                   ((180*theta[j-1]/pi)-90),
                                                   ((180*theta[j-1]/pi)-270))
                                ))
            }

            ##completes the connection only if prev. attr was not NA
            if (!is.na(instanceData[, ncol(df)-1]))
                graphData <- rbind(graphData,
                        data.frame(.instance=i,
                                    theta=theta[1],
                                    x=r * instanceData[,2] * cos(theta[1]) + x0,
                                    y=r * instanceData[,2] * sin(theta[1]) + y0,
                                    label="",
                                    label_max=NA, label_min=NA,
                                    label_angle=0
                                    ))
        }

        # Fix this hack !
        #graphData <- mutate(graphData,
        #    label_angle=ifelse(label_angle > 90, 90 - label_angle, label_angle))

        graphData

    }

    # to debug radarFix
    #radarData <- radarFix(df=subset(radar_inp_df, facet=<sample>))

    radar_inp_df$.instance <- as.factor(radar_inp_df[, instance_var])
    radar_inp_df$.facet <- ifelse(is.null(facet_var), as.factor(0),
                                    as.factor(radar_inp_df[, facet_var]))

    # Keep only numeric or logical columns
    dfcols_df <- data.frame(type=sapply(radar_inp_df, class))
    keep_cols <- rownames(dfcols_df)[dfcols_df[, "type"] %in%
                                         c("integer", "numeric", "logical")]
    keep_cols <- setdiff(keep_cols, ".facet")
    radar_inp_df <- radar_inp_df[, union(keep_cols, c(".instance", ".facet"))]

    # Get rid of any cols that have all 0s or NAs or Infs
    na_knts <- sapply(names(radar_inp_df[ ,keep_cols]),
                        function(col) sum(is.na(radar_inp_df[, col])))
    na_knts <- na_knts[na_knts == nrow(radar_inp_df)]
    if (length(na_knts) > 0) {
        warning("Not plotting columns with all NAs: ", paste0(names(na_knts), collapse=","))
        keep_cols <- setdiff(keep_cols, names(na_knts))
    }

    all_zeros <- sapply(names(radar_inp_df[ ,keep_cols]),
                        function(col) sum(radar_inp_df[, col], na.rm=TRUE))
    all_zeros <- all_zeros[all_zeros == 0]
    if (length(all_zeros) > 0) {
        warning("Not plotting columns with all 0s: ", paste0(names(all_zeros), collapse=","))
        keep_cols <- setdiff(keep_cols, names(all_zeros))
    }

    all_Infs <- sapply(names(radar_inp_df[ ,keep_cols]),
                        function(col) sum(radar_inp_df[, col], na.rm=TRUE))
    all_Infs <- all_Infs[all_Infs == Inf]
    if (length(all_Infs) > 0) {
        warning("Not plotting columns with all Infs: ", paste0(names(all_Infs), collapse=","))
        keep_cols <- setdiff(keep_cols, names(all_Infs))
    }

    # Change remaining Infs to NAs
    radar_inp_df[radar_inp_df == Inf] <- NA

    # scale the data
    radar_scld_df <- cbind(radar_inp_df[, ".instance", FALSE],
                data.frame(scale(radar_inp_df[, keep_cols, FALSE], scale=TRUE, center=FALSE)),
                            radar_inp_df[, ".facet", FALSE])

    require(plyr)
    radarData <- ddply(radar_scld_df, .(.facet), radarFix)

    radar_tmp_df <- cbind(radar_inp_df[, ".instance", FALSE],
                            radar_inp_df[, keep_cols, FALSE],
                            radar_inp_df[, ".facet", FALSE])
    radarDataRange <- ddply(radar_tmp_df, .(.facet), radarFix)
    radarData$label_range <- paste0("[", radarDataRange$label_min, ", ",
                                         radarDataRange$label_max, "]")

    gp <- ggplot(radarData, aes(x=x, y=y, group=.instance)) +
            geom_path(alpha=0.5, aes(color=.instance)) +
            geom_point(aes(shape=.instance), alpha=0.2) +
            geom_text(aes(x=x*1.05, y=y*1.05,
                          label=label_range, angle=label_angle), size=3.5,
                      data=subset(radarData, !is.na(label_max))) +
            geom_text(aes(x=x*1.15, y=y*1.15,
                          label=label,angle=label_angle), size=3.5) +
            geom_segment(aes(x=0.5, y=0.5, xend=x, yend=y), linetype="dotted",
                         data=subset(radarData, !is.na(label_max))) +
            theme(axis.ticks.y = element_blank(), axis.text.y=element_blank(),
                    axis.title.y = element_blank()) +
            theme(axis.ticks.x = element_blank(), axis.text.x=element_blank(),
                    axis.title.x = element_blank()) +
            #scale_color_discrete(name=.instance)
            scale_color_brewer(type="qual", palette="Set1")

    if (length(unique(radarData$.facet)) > 1)
        gp + facet_wrap(~.facet, scales="free")

    return(gp)
}

myplot_scatter <- function(df, xcol_name, ycol_name,
                           colorcol_name=NULL, jitter=FALSE, smooth=FALSE,
                           facet_rowcol_name=".", facet_colcol_name=".",
                           ylabel=NULL,
                           stats_df=NULL, predict_df=NULL, i_pkg=NULL, group=NULL) {

    #cat("\nentering myplot_scatter:")
    if (xcol_name == ".rownames")
        df[, xcol_name] <- rownames(df)

    if (!is.null(colorcol_name)) {
        if (!inherits(df[, colorcol_name], "factor")) {
            warning("converting ", colorcol_name, " to class:factor")
            df[, paste0(colorcol_name, ".fctr")] <- as.factor(df[ ,colorcol_name])
            colorcol_name <- paste0(colorcol_name, ".fctr")
        }
    }

    p <- ggplot(df, aes_string(x=xcol_name, y=ycol_name))

    if (!missing(colorcol_name) & mycheck_validarg(colorcol_name))
        p <- p + geom_point() + aes_string(color=colorcol_name) +
                scale_color_brewer(type="qual", palette="Set1")
    else
        p <- p + geom_point(color="grey")

    facets <- paste(facet_rowcol_name, '~', facet_colcol_name)
    if (facets != '. ~ .')
        p <- p + facet_grid(facets, scales="free")

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
        aes_str <- paste0("linetype=\"dashed\", xintercept=as.numeric(",
                          xcol_name, ")")
        aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
#         p <- p + geom_vline(mapping = aes_mapping,
#                             data = stats_df, show_guide=TRUE)
        p <- p + geom_vline(, data = stats_df, mapping = aes_mapping)
        p <- p + scale_linetype_identity(guide="legend", name="Stats", labels=rownames(stats_df))
    }

    if (!missing(predict_df)) {
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
    if (smooth) {
        if (!mycheck_validarg(group)) {
            if (mycheck_validarg(i_pkg)) {
                if (!((i_pkg == "plotly") & is.factor(df[, xcol_name])))
                    p <- p + geom_smooth(method="lm")
            } else p <- p + geom_smooth(method="lm")
        } else {
            aes_str <- paste0("group=", group)
            aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
            if (mycheck_validarg(i_pkg)) {
                if (!((i_pkg == "plotly") & is.factor(df[, xcol_name])))
                    p <- p + geom_smooth(aes_mapping, method="lm")
            } else p <- p + geom_smooth(aes_mapping, method="lm")
        }
    }

    # linetype legend messes up the fill legend
    p <- p + guides(color=guide_legend(override.aes=list(linetype=0)))

    #cat("\nexiting myplot_scatter:")
    return(p)
}

myplot_violin <- function(df, ycol_names, xcol_name=NULL, facet_spec=NULL) {
    require(stringr)

#     if ((length(ycol_names) > 1) & (!missing(xcol_name)))
#         stop("Multiple feats not implemented with x variable.",
#              "\n  Consider using facet parameter instead.")

    if (!is.null(xcol_name) & !is.factor(df[, xcol_name])) {
        xcol_name_par <- xcol_name
        xcol_name <- paste(xcol_name_par, "fctr", sep="_")
        warning("xcol_name:", xcol_name_par, " is not a factor; creating ", xcol_name)
        df[, xcol_name] <- as.factor(df[, xcol_name_par])
    }

    if (length(ycol_names) == 1) {
        if (missing(xcol_name)) {
            medians_df <- summaryBy(as.formula(paste0(ycol_names, " ~ factor(0)")), df,
                                    FUN=c(median), na.rm=TRUE)
            p <- ggplot(df, aes_string(x=factor(0), y=ycol_names))
            p <- p + xlab(" ")
            if (is.numeric(df[, ycol_names]))
                p <- p + scale_y_continuous(labels = myformat_number)
        } else {
            medians_df <- summaryBy(as.formula(paste0(ycol_names, " ~ ", xcol_name)), df,
                                    FUN=c(median), na.rm=TRUE)
            p <- ggplot(df, aes_string(x=xcol_name, y=ycol_names))
            if (all(is.numeric(df[, ycol_names])))
                p <- p + scale_y_continuous(labels = myformat_number)

            shapiro <- data.frame()
            if (is.numeric(df[, ycol_names]) &&
                (length(unique(df[, ycol_names])) > 1)) {
                shapiro <- as.data.frame(tapply(df[, ycol_names], df[, xcol_name], shapiro.test))
                names(shapiro)[1] <- "shapiro.test"
                shapiro[, xcol_name] <- row.names(shapiro)
                shapiro[, paste0(ycol_names, ".shapiro.pval")] <-
                    sapply(shapiro$shapiro.test,
                           function(res) as.numeric(unlist(str_split(res, ","))[2]))
                shapiro <- shapiro[, c(xcol_name, paste0(ycol_names, ".shapiro.pval"))]
                shapiro[, paste0(ycol_names, ".shapiro.lbl")] <- paste("shapiro.pval:\n",
                            formatC(shapiro[, paste0(ycol_names, ".shapiro.pval")], format = "e"))
                #sprintf("%0.4e", shapiro[1, paste0(ycol_names, ".shapiro.pval")])
            }
        }
    } else {
        mltd_df <- melt(df,, measure.vars=ycol_names)
        if (is.null(xcol_name)) {
            medians_df <- summaryBy(value ~ variable , mltd_df, FUN=c(median), na.rm=TRUE)
            p <- ggplot(mltd_df, aes(x=variable, y=value))
            p <- p + xlab(" ")
        } else {
            medians_df <- summaryBy(reformulate(c("variable", xcol_name), "value") , mltd_df, FUN=c(median), na.rm=TRUE)
            p <- ggplot(mltd_df, aes_string(x=xcol_name, y="value"))
        }
        if (is.numeric(mltd_df[, "value"]))
            p <- p + scale_y_continuous(labels = myformat_number)
    }

    if (!is.null(facet_spec)) {
        stop("facets not supported yet")
        require(doBy)
        sum_df <- summaryBy(steps ~ . , df, FUN=c(median))
    } else {
    }

    p <- p + geom_violin(fill="grey80", color="blue") +
             stat_summary(fun.y=mean, pch=22, geom='point', color='red')


    if (length(ycol_names) == 1) {
        aes_str <- paste0("y=", ycol_names, ".median * 1.05",
                          ", label=myformat_number(round(", ycol_names, ".median))")
        aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
        p <- p + geom_text(data = medians_df, mapping = aes_mapping, color = "NavyBlue", size = 3.5)

        if (nrow(shapiro) > 0) {
            p <- p + geom_text(data = shapiro,
                            aes_string(x = xcol_name, label = paste0(ycol_names, ".shapiro.lbl")),
                               y = max(df[, ycol_names], na.rm = TRUE) * 1.05,
                               color = "NavyBlue", size = 3.5, inherit.aes = FALSE)
            p <- p + ylim(NA, max(df[, ycol_names], na.rm = TRUE) * 1.10)
        }
    } else {
        if (is.null(xcol_name)) {
            p <- p + geom_text(data=medians_df,
                           mapping=aes_string(x="variable",
                                              y="value.median * 1.05",
                                              label="myformat_number(round(value.median))")
                           , color="NavyBlue", size=3.5)
        } else {
            p <- p + geom_text(data=medians_df,
                           mapping=aes_string(x=xcol_name,
                                              y="value.median * 1.05",
                                              label="myformat_number(round(value.median))")
                           , color="NavyBlue", size=3.5)
            p <- p + facet_wrap(~ variable, scales="free")
        }
    }

    return(p)
}

myplotImg <- function(img) {
    require(reshape2)
    require(ggplot2)

    image <- apply(img, 1:2, function(v) rgb(v[1], v[2], v[3]))
    image <- melt(image)
    return(ggplot(image, aes(Var2, -Var1, fill = value)) +
               geom_raster() + scale_fill_identity() + xlab(" ") + ylab(" "))
}
