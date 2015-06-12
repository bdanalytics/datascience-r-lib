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

myplot_bar <- function(df, xcol_name, ycol_names, colorcol_name=NULL, facet_spec=NULL,
                        xlabel_formatter=NULL) {
    require(ggplot2)

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

    if (length(ycol_names) == 1) {
        df <- df[order(df[, ycol_names], decreasing=TRUE), ]
#         df$xcol <- df[, xcol_name]
#         df$ycol <- df[, ycol_names]
        g <- ggplot(df, aes_string(x=paste0("reorder(", xcol_name, ", ", ycol_names, ")"),
                                   y=ycol_names))
        if (is.null(colorcol_name)) g <- g + geom_bar(fill="blue", stat="identity") else
                            g <- g + geom_bar(aes_string(fill=colorcol_name), stat="identity")
        g <- g + xlab(xcol_name) + ylab(ycol_names)
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
        require(reshape)
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
    sav_g <- g
    if (length(ycol_names) == 1) {
        if (class(df[, ycol_names]) == "num")
            g <- g + scale_y_continuous(labels=myformat_number)

        aes_str <- paste0("y=", ycol_names, ".median", " * ",
                          ycol_names, ".median.offset.mult ",
                            ", label=myformat_number(round(", ycol_names, ".median))")
#                           ", label=", ycol_names, ".median")
        aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
        g <- g + geom_text(data=stats_df,
                           mapping=aes_mapping
                           , color="NavyBlue", size=3.5)
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
    return(myplot_bar(df, xcol_name, ycol_names, xlabel_formatter, facet_spec, ...) + coord_flip())

myplot_histogram <- function(df, hst_col_name, fill_col_name=NULL,
                             show_stats=TRUE, facet_frmla=NULL) {
    require(ggplot2)

    if (inherits(df[, hst_col_name], "character")) {
        warning("converting ", hst_col_name, " to class:factor")
        df[, hst_col_name] <- as.factor(df[, hst_col_name])
    }

    if (is.null(fill_col_name)) {
        # Fill with raw counts
        p <- ggplot(df, aes_string(x=hst_col_name))
        p <- p + geom_histogram(aes(fill=..count..)) +
                 scale_fill_gradient("Count", low="red", high="blue")
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
            p <- p + geom_vline(aes_string(xintercept=mean(df[, hst_col_name],
                                                           na.rm=TRUE),
                                linetype="\"dotted\""), show_guide=TRUE)
        p <- p + geom_vline(aes_string(xintercept=mycompute_median(df[, hst_col_name]),
                                       linetype="\"dashed\""), show_guide=TRUE)
    }

    # Add number of missing values as a horizontal line
    num_na <- sum(is.na(df[, hst_col_name]))
    if (num_na > 0)
        p <- p + geom_hline(aes_string(yintercept=num_na,
                                       linetype="\"dotdash\""), show_guide=TRUE)

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

    return(g)
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
    head(df[, c(glb_id_var, tmp_vars)])
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

    dsp_vars <- c(id_vars, grep(rsp_var, names(df), value=TRUE))
    print("Min/Max Boundaries: ")
    myprint_df(orderBy(reformulate(predct_error_var_name),
                       subset(df[, c(dsp_vars, ".label")], .label != "")))
    print("Inaccurate: ")
    myprint_df(orderBy(reformulate(predct_error_var_name),
                       df[!df[, predct_accurate_var_name], c(dsp_vars)]))

    return(ggplot(df, aes_string(x=feat_x, y=feat_y)) +
            geom_point(aes_string(color=color_var,
#    shape=paste0("factor(as.numeric(", predct_accurate_var_name, ") + 3)")),
#    shape=paste0("relevel(factor(as.numeric(", predct_accurate_var_name, ") + 3), ref=1)")),
                                    shape=predct_accurate_var_name),
                           position="jitter") +
            scale_shape_manual(values=c(4,3)) + guides(shape=FALSE) +
            geom_text(aes_string(label=".label"), color="NavyBlue", size=3.5) +
            facet_wrap(reformulate( predct_accurate_var_name)) +
            scale_color_brewer(type="qual", palette="Set1")
          )
}

myplot_prediction_regression <- function(df, feat_x, feat_y, rsp_var, rsp_var_out, id_vars) {

    predct_err_name <- paste0(rsp_var_out, ".err")
    df[, predct_err_name] <-
        abs(df[,rsp_var] - df[,rsp_var_out])

    # Add labels to top 5 prediction errors
    df <- orderBy(reformulate(c("-", predct_err_name)), df)
    df$.label <- " "
    df$.label[1:min(5, nrow(df))] <- sapply(1:min(5, nrow(df)), function(row_ix)
        df[row_ix, ".label"] <- paste0(df[row_ix,  id_vars], collapse=":"))
    print(head(df, 5))

    return(myplot_scatter(df, feat_x, feat_y) +
            geom_point(aes_string(size=predct_err_name), alpha=0.4, show_guide = TRUE) +
            geom_text(aes_string(label=".label"), color="NavyBlue", size=3.5))
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
        aes_str <- paste0("linetype=\"dashed\", xintercept=as.numeric(", xcol_name, ")")
        aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
        p <- p + geom_vline(mapping=aes_mapping,
                            data=stats_df, show_guide=TRUE)
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
#     if ((length(ycol_names) > 1) & (!missing(xcol_name)))
#         stop("Multiple feats not implemented with x variable.",
#              "\n  Consider using facet parameter instead.")

    if (!missing(xcol_name) & !is.factor(df[, xcol_name])) {
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
        } else {
            medians_df <- summaryBy(as.formula(paste0(ycol_names, " ~ ", xcol_name)), df,
                                    FUN=c(median), na.rm=TRUE)
            p <- ggplot(df, aes_string(x=xcol_name, y=ycol_names))
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
    }

    if (!is.null(facet_spec)) {
        stop("facets not supported yet")
        require(doBy)
        sum_df <- summaryBy(steps ~ . , df, FUN=c(median))
    } else {
    }

    p <- p + geom_violin(fill="grey80", color="blue") +
             stat_summary(fun.y=mean, pch=22, geom='point', color='red') +
             scale_y_continuous(labels=myformat_number)

    if (length(ycol_names) == 1) {
        aes_str <- paste0("linetype=\"dashed\", yintercept=as.numeric(", ycol_names, ")")
        aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
#         p <- p + geom_hline(data=medians_df,
#                             mapping=aes_mapping, show_guide=TRUE
#                             , color="black", size=1)
#         p <- p + scale_linetype_identity(guide="legend", name="Stats",
#                                          labels=rownames(medians_df))

        aes_str <- paste0("y=", ycol_names, ".median * 1.05",
                          ", label=myformat_number(round(", ycol_names, ".median))")
        aes_mapping <- eval(parse(text = paste("aes(", aes_str, ")")))
        p <- p + geom_text(data=medians_df,
                           mapping=aes_mapping
                           , color="NavyBlue", size=3.5)
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

