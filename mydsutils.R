# Balaji Iyengar's Data Science Process
# Created 2014-08-12
# Check if functions are assigned to proper data science process steps

## 00. Base
#suppressPackageStartupMessages(require(<<package name>>))

# https://github.com/hadley/pryr/blob/master/R/assign-delayed.r
#' Create an delayed binding.
#'
#' Infix form of \code{\link{delayedAssign}} which creates an \emph{delayed}
#' or lazy binding, which only evaluates the expression the first time it is
#' used.
#'
#' @usage x \%<d-\% value
#' @param x unquoted expression naming variable to create
#' @param value unquoted expression to evaluate the first time \code{name} is
#'   accessed
#' @export
#' @rdname assign-delayed
#' @examples
#' x %<d-% (a + b)
#' a <- 10
#' b <- 100
#' x
"%<d-%" <- function(x, value) {
    name <- substitute(x)
    value <- substitute(value)

    if (!is.name(name)) stop("Left-hand side must be a name")

    env <- parent.frame()
    call <- substitute(delayedAssign(deparse(name), value,
                                     eval.env = env, assign.env = env), list(value = value))
    eval(call)

    invisible()
}

## A helper function that tests whether an object is either NULL _or_
## a list of NULLs
is.NullObj <- function(x) is.null(x) | all(sapply(x, is.null))

## Recursively step down into list, removing all such objects
myrmNullObj <- function(x) {
    x <- Filter(Negate(is.NullObj), x)
    lapply(x, function(x) if (is.list(x)) myrmNullObj(x) else x)
}

## 01. 		import data

myimport_data <- function(specs, nrows=-1, comment=NULL,
							force_header=FALSE, print_diagn=TRUE, ...){
    if (!file.exists("./data")) dir.create("data")

    url <- specs$url; filename <- specs$name
    if (!is.null(url)) {
        url_split <- strsplit(url, "/", fixed = TRUE)
        if (is.null(names(url_split)))
            names(url_split) <- as.character(1:length(url_split))
        download_filepath <- c()
        for (ix in 1:length(url_split)) {
            thsURLSplit <- url_split[[ix]]
            download_filename <- gsub("%2F", "-", thsURLSplit[length(thsURLSplit)], fixed = TRUE)
            download_filepath[names(url_split)[ix]] <- paste("./data", download_filename, sep = "/")
            if (!file.exists(download_filepath[names(url_split)[ix]])) {
            	print(sprintf("Downloading file %s from %s...",
            	              download_filepath[names(url_split)[ix]], url))

                # Issue with downloading from https:// ??? currently downloads html doc even though url points to csv file
                # download.file(url, destfile=download_filepath, method="curl")
                download.file(url, destfile = download_filepath[names(url_split)[ix]],
                              method = "auto")
            }
        }

        url_split <- strsplit(url, ".", fixed = TRUE)
        if (is.null(names(url_split)))
            names(url_split) <- as.character(1:length(url_split))
        file_path <- c()
        for (ix in 1:length(url_split)) {
            thsURLSplit <- url_split[[ix]]
            download_filename_ext <- thsURLSplit[length(thsURLSplit)]
            if (download_filename_ext %in% c("zip", "tgz")) {
                if (is.null(filename)) {
        #           stop("Please specify which file(filename=) should be imported from ",
        #                  download_filepath)
        			filename <- substr(download_filename, 1, nchar(download_filename) - nchar(".zip"))
                }

                file_path[names(url_split)[ix]] <- paste("./data", filename, sep="/")
                if (!file.exists(file_path[names(url_split)[ix]])) {
            		print(sprintf("Unzipping file %s...", file_path[names(url_split)[ix]]))
                    unzip(download_filepath[names(url_split)[ix]], filename[names(url_split)[ix]])
                }
            } else file_path[names(url_split)[ix]] <- download_filepath[names(url_split)[ix]]
        }
    } else file_path <- paste("./data", filename, sep = "/")

    obsDf <- data.frame()
    for (ix in 1:length(file_path)) {
        # read.csv reads files with ext %in% c(".csv", ".csv.bz2)
        #	check if file contains header
        first_record <- read.csv(file_path[ix], header = FALSE, quote = "", nrows = 1)

    	if (!force_header) {
    		header <- FALSE

    		if (length(grep('^"', first_record[1, 1])) != 0)
    			header <- TRUE else {
    			col_names <- paste(first_record[,])
    			diffs <- setdiff(make.names(col_names), col_names)
    			if (length(diffs) == 0)
    				header <- TRUE else {
    				if (length(grep("^X", diffs)) == length(diffs))
    					header <- TRUE
    			}
    		}

    		if (!(header))
    		{
    			warning(file_path, " does not contain header")
    			print("first 10 records:")
    			#print(system(paste0("head ", file_path)))
    			#system(paste0("head ", file_path, " | cat")))
    			print(readLines(file_path, n=10))
    		}
    	} else header <- TRUE

        print(sprintf("Reading file %s...", file_path[ix]))
        if (is.null(specs$sep))
            specs$sep = ","

        if (specs$sep == "\t")
            df <- read.delim(file_path[ix], header = header, nrows = nrows, ...) else
            df <-   read.csv(file_path[ix], header = header, nrows = nrows, ...)

        if (nrows > 0)
        	warning("first ", nrows, " records read")

        print(sprintf("dimensions of data in %s: %s rows x %s cols", file_path[ix],
                      format(dim(df)[1], big.mark = ","),
                      format(dim(df)[2], big.mark = ",")))

        if (length(names(file_path)) > 1)
            df$.inp <- names(file_path)[ix]
        obsDf <- rbind(obsDf, df)
    }

	if (!(missing(comment)))
		comment(obsDf) <- comment

    if (print_diagn) {
        tmpDf <- obsDf
        if (length(chrCols <- myfind_chr_cols_df(tmpDf)) > 0) {
            for (chrCol in chrCols)
                if (max(nchar(tmpDf[, chrCol]), na.rm = TRUE) > 100) {
                    print(sprintf("   Truncating %s to first 100 chars...", chrCol))
                    tmpDf[, chrCol] <- substr(tmpDf[, chrCol], 1, 100)
                }
        }
		myprint_df(tmpDf)
		myprint_str_df(obsDf)
    }

    return(obsDf)
}
#activity_df <- myimport_data("activity.csv",
#                "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip")

myprint_df <- function(df, dims=FALSE) {
    if (dims)
        print(sprintf("Rows: %s; Cols: %s",
                      format(dim(df)[1], big.mark = ","),
                      format(dim(df)[2], big.mark = ",")))

    if (nrow(df) == 0) return()

    if (dim(df)[1] <= 20) print(df) else {
    	print(head(df))

		# print 6 sample obs
		print(df[sort(sample(1:dim(df)[1], 6)),, FALSE])

		print(tail(df))
    }
}

myprint_str_df <- function(df) {
    if (ncol(df) <= 20) print(str(df)) else {
        print(str(df[, 1:20]))

		if (ncol(df) > 40)
			# print 20 middle cols
			print(str(df[, sort(sample(21:(ncol(df)-20), 20))]))

        print(str(df[, (ncol(df) - 20):ncol(df)]))
        warning("[list output truncated]")
    }
}

## 02.	    cleanse data
## 02.1	    inspect/explore data

map_event_grp <- function(evtype) {
    retval <- as.factor("Other.Grp")
    evtype <- toupper(evtype)

         if (length(grep("TORNADO"                      , evtype)) > 0) { retval <- as.factor("Tornado.Grp")      }
    else if (length(grep("HEAT"                         , evtype)) > 0) { retval <- as.factor("Heat.Grp")         }
    else if (length(grep("COLD|CHILL"                   , evtype)) > 0) { retval <- as.factor("Cold.Grp")         }
    else if (length(grep("WINTER|SNOW"                  , evtype)) > 0) { retval <- as.factor("WinterStorm.Grp")  }
    else if (length(grep("NON TSTM WIND"                , evtype)) > 0) { retval <- as.factor("Wind.Grp")         }
    else if (length(grep("THUNDERSTORM|TSTM|THUNDERTORM", evtype)) > 0) { retval <- as.factor("ThunderStorm.Grp") }
    else if (length(grep("HURRICANE"                    , evtype)) > 0) { retval <- as.factor("Hurricane.Grp")    }
    else if (length(grep("WIND"                         , evtype)) > 0) { retval <- as.factor("Wind.Grp")         }
    else if (length(grep("ICE"                          , evtype)) > 0) { retval <- as.factor("WinterStorm.Grp")  }
    else if (length(grep("HEAVY RAIN"                   , evtype)) > 0) { retval <- as.factor("ThunderStorm.Grp") }
    else if (length(grep("FLOOD"                        , evtype)) > 0) { retval <- as.factor("Flood.Grp")        }
    else if (length(grep("LIGHTNING"                    , evtype)) > 0) { retval <- as.factor("Lightning.Grp")    }
    else if (length(grep("STORM SURGE"                  , evtype)) > 0) { retval <- as.factor("Flood.Grp")        }
    else if (length(grep("HAIL"                         , evtype)) > 0) { retval <- as.factor("Hail.Grp")         }
    else if (length(grep("TROPICAL STORM"               , evtype)) > 0) { retval <- as.factor("TropicalStorm.Grp")}
    else if (length(grep("DROUGHT"                      , evtype)) > 0) { retval <- as.factor("Drought.Grp")      }

    return(retval)
}

#subset(obs_df, select=-c(interval))

# sql <- "SELECT EVTYPE, SUM(FATALITIES) AS FATALITIES, SUM(INJURIES) AS INJURIES,
#                SUM(FATALITIES + INJURIES) AS health_dmg, SUM(1) AS storms_n
#         FROM storms_df
#         GROUP BY EVTYPE
#         HAVING health_dmg > 0
#         ORDER BY health_dmg DESC
#        "
#EVTYPE_health_df <- sqldf(sql)
#summary(EVTYPE_health_df)

myaggregate_numorlgcl <- function (df, by_names, func) {

    # Keep only numeric or logical columns
    dfcols_df <- data.frame(type=sapply(df, class))
    keep_cols <- rownames(dfcols_df)[dfcols_df[, "type"] %in%
                                         c("integer", "numeric", "logical")]
    # Drop cols in by_names
    keep_cols <- setdiff(keep_cols, by_names)

    # Build df with only numeric/logical & by cols
    subset_df <- cbind(df[, keep_cols, drop=FALSE], df[, by_names, drop=FALSE])
    names(subset_df) <- c(keep_cols, by_names)

    # Drop obs with NAs
    num_complete_cases <- sum(complete.cases(subset_df))
    if (num_complete_cases < dim(df)[1]) {
        num_excl_cases <- dim(df)[1] - num_complete_cases
        warning (sprintf("excluding %s (%0.1f pct) obs with NA",
                         prettyNum(num_excl_cases, big.mark=","),
                         num_excl_cases * 100.0 / dim(df)[1]))
        subset_df <- subset_df[complete.cases(subset_df), , drop=FALSE]
    }

    by_lst <- sapply(by_names, function(byvar) list(subset_df[, byvar]))
    agg_df <- aggregate(subset_df[, keep_cols], by_lst, func)

    # Add a suffix of the function name to the aggregated columns
    func_names_dct <- list(mean=mean, sum=sum)
    if (c(func) %in% func_names_dct) {
        name_suffix <- names(func_names_dct)[match(c(func), func_names_dct)]
        new_col_names <- sapply(keep_cols, function(name) paste(name,
                                                                name_suffix,
                                                                sep="_"))
        names(agg_df) <- c(by_names, new_col_names)
    } else warning("column names renaming unsupported for unknown func",
                   str(func))

    return(agg_df)
}
#entity_agg_date_df <- myaggregate_numorlgcl(subset(obs_df,
#                                                   select=-c(interval)),
#                                            "date", sum)

# same as all.equal() or identical() ?
mycheck_identity <- function(obj1, obj2) {

	if (class(obj1) == "data.frame") {
		# ensure row names are same
		if (rownames(obj1) != rownames(obj2)) {
			warning("data.frame(s) not identical due to differing row names")
			return (FALSE)
		}
	}

    result <- (obj1 != obj2)

    if (sum(is.na(result)) > 0) {
        # Objects contain NA
        #   ensure same number of NAs
        if (sum(is.na(obj1)) != sum(is.na(obj1))) {
            warning("Objects not identical due to differing sum(NA)")
            return (FALSE)
        }
        else {
            if (length(obj1) != length(obj2)) {
                warning("Objects not identical due to differing lengths")
                return (FALSE)
            }

            if (class(obj1) == "data.frame") {
                # ensure NA indexes are same
                if (sum(row.names(obj1[complete.cases(obj1), ]) !=
                        row.names(obj1[complete.cases(obj1), ])) > 0) {
                    warning("data.frame(s) not identical due to differing NA rows")
                    return (FALSE)
                }

                # ensure each column is similar
                retval <- TRUE
                for (col in 1:max(length(names(obj1)), length(names(obj2))))
                    retval <- retval & mycheck_identity(obj1[, col], obj2[, col])

                if (!retval) {
                    warning("data.frame(s) not identical due to differing col values")
                }

                return(mycheck_identity(obj1[complete.cases(obj1), ], obj2[complete.cases(obj2), ]))
            }
            else if (class(obj1) == "integer") {
                # ensure NA indexes are same
                if (sum(is.na(obj1) != is.na(obj2)) > 0) {
                    warning("integer(s) not identical due to differing NA positions")
                    return(FALSE)
                } else return(mycheck_identity(obj1[!is.na(obj1)], obj2[!is.na(obj2)]))
            }
            else stop("unsupported class")
        }
    }

    if (sum(result) > 0) { return(FALSE) } else { return(TRUE) }
}

mycheck_prime <- function(n) n == 2L || all(n %% 2L:ceiling(sqrt(n)) != 0)
# mycheck_prime(1)
# mycheck_prime(2)
# mycheck_prime(3)
# mycheck_prime(4)
# mycheck_prime(5)
# mycheck_prime(7)
# mycheck_prime(9)

sel_obs <- function(vars_lst, ignore.case=TRUE, perl=FALSE) {
    tmp_df <- glbObsAll

    # Does not work for Popular == NAs ???
    #     if (!is.null(Popular)) {
    #         if (is.na(Popular))
    #             tmp_df <- tmp_df[is.na(tmp_df$Popular), ] else
    #             tmp_df <- tmp_df[tmp_df$Popular == Popular, ]
    #     }
    #     if (!is.null(NewsDesk))
    #         tmp_df <- tmp_df[tmp_df$NewsDesk == NewsDesk, ]

    for (var in names(vars_lst)) {
        if (grepl(".contains", var))
            tmp_df <- tmp_df[grep(vars_lst[var],
                                  tmp_df[, unlist(strsplit(var, ".contains"))],
                                  ignore.case=ignore.case, perl=perl), ]
        else
            tmp_df <- tmp_df[tmp_df[, var] == vars_lst[var], ]
    }

    return(glbObsAll[, glb_id_var] %in% tmp_df[, glb_id_var])
}
#print(glbObsAll[sel_obs(list(description.contains="mini(?!m)"), perl=TRUE), "description"])

mydspObs <- function(.dot = ..., cols = c(NULL), all = FALSE) {
    feats <-
        union(c(glb_id_var, glb_rsp_var, glbFeatsCategory, cols, glbFeatsText),
              gsub("\\.contains$", "", names(.dot)))
    if (length(featsError <- setdiff(feats, names(glbObsAll))) > 0) {
        warning("mydsp_obs: ignoring missing cols: ",
                paste0(featsError, collapse = ", "))
        feats <- setdiff(feats, featsError)
    }
    tmp_df <- glbObsAll[sel_obs(.dot), feats, FALSE]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(list(description.contains="mini(?!m)"), perl=TRUE)
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

mycheck_problem_data <- function(df, featsExclude, fctrMaxUniqVals = 20, terminate=FALSE) {
    print(sprintf("numeric data missing in %s: ",
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    # refactor this code with myfind_numerics_missing
    numeric_missing <- sapply(setdiff(names(df), myfind_chr_cols_df(df)),
                              function(col) sum(is.na(df[, col])))
    numeric_missing <- numeric_missing[numeric_missing > 0]
    print(numeric_missing)
    numeric_feats_missing <- setdiff(names(numeric_missing),
                                     c(featsExclude, glb_rsp_var))
    if ((length(numeric_feats_missing) > 0) && terminate)
        stop("terminating due to missing values in: ", paste(numeric_feats_missing, collapse=", "))

    print(sprintf("numeric data w/ 0s in %s: ",
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    numeric_Zero <- sapply(setdiff(names(df), myfind_chr_cols_df(df)),
                          function(col) sum(df[, col] == 0, na.rm=TRUE))
    numeric_Zero <- numeric_Zero[numeric_Zero > 0]
    print(numeric_Zero)

    print(sprintf("numeric data w/ Infs in %s: ",
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    numeric_Inf <- sapply(setdiff(names(df), myfind_chr_cols_df(df)),
                 function(col) sum(df[, col] == Inf, na.rm=TRUE))
    numeric_Inf <- numeric_Inf[numeric_Inf > 0]
    print(numeric_Inf)
    numeric_feats_Inf <- setdiff(names(numeric_Inf),
                                     c(featsExclude, glb_rsp_var))
    if ((length(numeric_feats_Inf) > 0) && terminate)
        stop("terminating due to Inf values in: ", paste(numeric_feats_Inf, collapse=", "))

    print(sprintf("numeric data w/ NaNs in %s: ",
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    numeric_NaN <- sapply(setdiff(names(df), myfind_chr_cols_df(df)),
                          function(col) sum(is.nan(df[, col])))
    numeric_NaN <- numeric_NaN[numeric_NaN > 0]
    print(numeric_NaN)
    numeric_feats_NaN <- setdiff(names(numeric_NaN),
                                 c(featsExclude, glb_rsp_var))
    if ((length(numeric_feats_NaN) > 0) && terminate)
        stop("terminating due to NaN values in: ", paste(numeric_feats_NaN, collapse=", "))

    print(sprintf("string data missing in %s: ",
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(myfind_chr_cols_df(df), ".src"),
                 function(col) sum(df[, col] == "")))

    length_fctrs <- sapply(setdiff(myfind_fctr_cols_df(df),
                                   c(glb_rsp_var, featsExclude)),
                           function(feat) length(unique(df[, feat])))
    length_fctrs <- length_fctrs[length_fctrs > fctrMaxUniqVals]
    if (length(length_fctrs) > 0) {
        print(sprintf("factors with number of unique values greater than max: %d",
                      fctrMaxUniqVals))
        print(length_fctrs)
        if (terminate)
            stop("terminating script")
    }

}

mycheck_validarg <- function(value) {
	     if (is.null(value)) 	return(FALSE)
	else if (is.na(value))   	return(FALSE)
	else if (value == "None")	return(FALSE)	# for shiny inputs
	else return(TRUE)
}
# mycheck_validarg(NULL)
# mycheck_validarg(NA)
# mycheck_validarg("None")
# mycheck_validarg("WTF")
# mycheck_validarg(-2)
# mycheck_validarg(0)
# mycheck_validarg(FALSE)
# mycheck_validarg(TRUE)
# mycheck_validarg("none")

mycompute_median <- function(vector) {
    if (is.factor(vector))
        return(factor(levels(vector)[median(as.numeric(vector), na.rm=TRUE)],
                      levels(vector)))
#     if (class(vector) == "Date")
#     	return(as.Date(median(vector, na.rm=TRUE), origin="1970-01-01"))

    return(median(vector, na.rm=TRUE))
}

mycompute_stats <- function(vector, stats=median) {
    if (is.factor(vector))
        return(factor(levels(vector)[stats(as.numeric(vector), na.rm = TRUE)],
                      levels(vector)))

    if (any(class(tst <- try(stats(vector, na.rm = TRUE))) %in% c("try-error")))
        tst <- NA
    return(tst)
}

mycompute_medians_df <- function(df, byvars_lst=factor(0), keep.names=FALSE) {
    warning("deprecated to mycompute_stats_df")

	if (!any(class(df) %in% c("data.frame")))
		stop("df argument is not a data.frame: it is ", paste(class(pltLvl), collapse = ","))

	ret_df <- data.frame()

	# gather numeric & factor vars
    num_lst <- sapply(names(df), function(col)
        if (is.numeric(df[, col]) || is.factor(df[, col])) return(col))
    num_vctr <- setdiff(unlist(num_lst[!sapply(num_lst, is.null)]), byvars_lst)
    if (length(num_vctr) > 0)
	    ret_df <- summaryBy(as.formula(paste0(num_vctr, " ~ ", byvars_lst)), data=df,
    							FUN=median, keep.names=keep.names)

    # summaryBy does not compute stats for Date class variables
    non_num_lst <- sapply(names(df), function(col)
        if (!is.numeric(df[, col]) && !is.factor(df[, col])) return(col))
    non_num_vctr <- unlist(non_num_lst[!sapply(non_num_lst, is.null)])
    non_num_vctr <- setdiff(non_num_vctr, byvars_lst)
    for (var in non_num_vctr) {
    	#print("var="); print(var)
        #if (byvars_lst != factor(0))
        #	stop("mycompute_medians_df does not support byvars_lst=", byvars_lst)

        if (keep.names) new_name <- var else new_name <- paste0(var, ".median")
        if (nrow(ret_df) == 0) {
        	if (byvars_lst == factor(0)) {
        		ret_df <- as.data.frame(mycompute_median(df[, var]))
        	} else {
				ret_df <- as.data.frame(tapply(df[, var], df[, byvars_lst],
											FUN=mycompute_median))
			}
        	names(ret_df) <- new_name
        }
        else {
        	if (byvars_lst == factor(0)) {
        		ret_df[, new_name] <- mycompute_median(df[, var])
        	} else {
        		ret_df[, new_name] <- tapply(df[, var], df[, byvars_lst],
        								FUN=mycompute_median)
        	}
        }

		# tapply strips class info
        if (class(df[, var]) == "Date")
        	ret_df[, new_name] <- as.Date(ret_df[, new_name], origin="1970-01-01")
    }

    if (nrow(ret_df) == 1) rownames(ret_df) <- "median"

    return(ret_df)
}
# medians_df <- summaryBy(as.formula(paste0(ycol_names, " ~ factor(0)")), df,
#                                     FUN=c(median), na.rm=TRUE)
# medians_df <- summaryBy(value ~ variable , mltd_df, FUN=c(median), na.rm=TRUE)

mycompute_stats_df <- function(df, byvars_vctr = factor(0)) {

    if (!any(class(df) %in% c("data.frame")))
        stop("df argument is not a data.frame: it is ", paste(class(pltLvl), collapse = ","))

    ret_df <- data.frame()
    stats_fns <- c(.mean = mean, .median = median, .sum = sum)
    #stats_fns <- c(.mean = mean, .median = median)
    # names(stats_fns) <- c(".mean", ".median", ".sum")

    # gather numeric vars
    num_lst <- sapply(names(df), function(col)
        if (is.numeric(df[, col]) || is.factor(df[, col])) return(col))
    num_vctr <- setdiff(unlist(num_lst[!sapply(num_lst, is.null)]), byvars_vctr)
    if (length(num_vctr) > 0) {
        # Requires hard-coding of FUN ???
#         ret_df <- summaryBy(as.formula(paste0(num_vctr, " ~ ", byvars_vctr)), data=df,
#                             FUN=stats_fns)
        ret_df <- summaryBy(as.formula(paste0(paste0(num_vctr, collapse="+"),
                                              " ~ ", byvars_vctr)), data=df,
                            #FUN=interp(~stats_fns), na.rm=TRUE)
                            FUN=c(mean, median, sum), na.rm=TRUE)

        if (inherits(ret_df[, byvars_vctr], "factor") &&
            sum(is.na(ret_df[, byvars_vctr])) > 0) {
            row_names <- as.character(ret_df[, byvars_vctr])
            row_names[is.na(row_names)] <- "NA"
            row.names(ret_df) <- row_names
        } else row.names(ret_df) <- ret_df[, byvars_vctr]
    }

    # summaryBy does not compute stats for factor / Date class variables
    non_num_lst <- sapply(names(df), function(col)
        if (!is.numeric(df[, col]) &&
            !is.factor(df[, col]) &&
            !is.character(df[, col]))
            return(col))
    non_num_vctr <- unlist(non_num_lst[!sapply(non_num_lst, is.null)])
    non_num_vctr <- setdiff(non_num_vctr, byvars_vctr)
    for (var in non_num_vctr) {
        #print("var="); print(var)

        this_df <- data.frame()
        if (byvars_vctr == factor(0)) {
            statVal <- sapply(names(stats_fns), function(statName)
                                                mycompute_stats(df[, var], stats = stats_fns[[statName]]))
            statVal <- as.data.frame(t(statVal))
        } else {
            statVal <- sapply(names(stats_fns), function(statName)
                tapply(df[, var], df[, byvars_vctr], FUN = mycompute_stats, stats = stats_fns[[statName]]))
            statVal <- as.data.frame(statVal)
        }
        names(statVal) <- paste0(var, names(stats_fns))
        this_df <- mycbind_df(this_df, statVal)

        # tapply strips class info
        if (inherits(class(df[, var]), "Date")) {
            for (col in names(this_df))
                this_df[, col] <- as.Date(this_df[, col], origin = "1970-01-01")
        }

        ret_df <- mycbind_df(ret_df, this_df)
    }

#     if (nrow(ret_df) == 1) rownames(ret_df) <- "stats" else
#         ret_df[, byvars_vctr] <- row.names(ret_df)
    if (nrow(ret_df) == 1) rownames(ret_df) <- "stats"

    return(ret_df)
}

mycompute_entropy_df <- function(obs_df, entropy_var, by_var = NULL) {
    require(lazyeval)
    require(dplyr)
    require(tidyr)

    if (is.null(by_var)) {
        by_var <- ".default"
        obs_df$.default <- as.factor(".default")
    }

    grps <- obs_df %>%
        count_(c(by_var, entropy_var)) %>%
        dplyr::filter(n > 0) %>%
        dplyr::filter_(interp(~(!is.na(var)), var = as.name(entropy_var))) %>%
#         unite_(paste0(by_var, ".clusterid"),
#                c(interp(by_var), ".clusterid")) %>%
        spread_(interp(entropy_var), "n", fill = 0)

    #     head(grps)
    #     sum(grps$n)
    tmp.entropy <- sapply(1:nrow(grps),
                          function(row) entropy(as.numeric(grps[row, -1]), method = "ML"))
    tmp.knt <- sapply(1:nrow(grps),
                      function(row) sum(as.numeric(grps[row, -1])))
    grps$.entropy <- tmp.entropy; grps$.knt <- tmp.knt
    #print(grps)
    return(grps)
}

mycreate_tbl_df <- function(df, tbl_col_names) {

	ret_df <- as.data.frame(sort(table(df[, tbl_col_names], useNA="ifany")))
	names(ret_df) <- ".freq"
	ret_df[, tbl_col_names] <- rownames(ret_df)
	rownames(ret_df) <- 1:nrow(ret_df)

	return(ret_df[, c(tbl_col_names, ".freq")])
}

mycreate_xtab_df <- function(df, xtab_col_names) {
    require(doBy)
    require(reshape2)

    df[, "_n"] <- 1
    #xtab_col_names <- paste("`", xtab_col_names, "`", sep = "")
    count_df <- summaryBy(reformulate(xtab_col_names, "`_n`"), df, FUN=c(length))
    #count_df <- summaryBy(reformulate(xtab_col_names, "_n"), df, FUN=c(length))
    names(count_df) <- gsub("`", "", names(count_df))

    if (length(xtab_col_names) < 2)
    	return(count_df)

    cast_df <- dcast(count_df,
    	reformulate(tail(xtab_col_names, 1), head(xtab_col_names, -1)),
    	value.var="_n.length")

    for (col_ix in length(xtab_col_names):ncol(cast_df))
    	names(cast_df)[col_ix] <-
    	paste(tail(xtab_col_names, 1), names(cast_df)[col_ix], sep=".")

    return(cast_df)
}

mycreate_sqlxtab_df <- function(obs_df, xtab_col_names) {
    require(sqldf)
    # _n does not work with ggplot
    sql <- "SELECT "
    for (var in xtab_col_names)
        sql <- paste0(sql, "\"", var, "\", ")
    sql <- paste0(sql, " SUM(1) AS \".n\" ")
    sql <- paste0(sql, "FROM obs_df GROUP BY ")
    for (var in xtab_col_names)
        sql <- paste0(sql, "\"", var, "\"",
                      ifelse(var == tail(xtab_col_names, 1), " ", ", "))
    sql <- paste0(sql, "ORDER BY \".n\" DESC")
    return(sqldf(sql))
}

mydelete_cols_df <- function(df, colnames) {
    return(subset(df, select=names(df)[!names(df) %in% colnames]))
}

myfind_all_na_cols_df <- function(df) {
    na_cols <- which(as.numeric(colSums(is.na(df))) == nrow(df))
    return(names(df)[na_cols])
}

myfind_chr_cols_df <- function(df) {
    cols <- sapply(names(df),
                   function(col) ifelse(inherits(df[, col], "character"), col, ""))
    return(cols[cols != ""])
}
#myfind_chr_cols_df(glb_obs_df)

myfind_fctr_cols_df <- function(df) {
    cols <- sapply(names(df),
                   function(col) ifelse(inherits(df[, col], "factor"), col, ""))
    return(cols[cols != ""])
}
#myfind_fctr_cols_df(glbObsAll)

myfind_numerics_missing <- function(df, featsExclude = NULL) {
    numeric_missing <- sapply(setdiff(names(df), myfind_chr_cols_df(df)),
                              function(col) sum(is.na(df[, col])))
    numeric_missing <- numeric_missing[numeric_missing > 0]
    #print(numeric_missing)
    numeric_feats_missing <- setdiff(names(numeric_missing),
                                     c(featsExclude, glb_rsp_var))
    if (length(numeric_feats_missing) > 0) {
        print("Missing data for numerics:")
        print(numeric_missing)
    }
    return(numeric_feats_missing)
}

myfind_dups_df <- function(df) { stop("use native duplicated R fn") }

myformat_number <- function(x) {
    if (class(x) != "num") x <- as.numeric(x)
    return(sapply(x, function(elem)
        if (is.na(elem)) format(elem) else
        if (elem < 1)    format(elem, digits=4, nsmall=4, scientific=FALSE) else
        if (elem < 10)   format(elem, digits=2, nsmall=2, scientific=FALSE) else
        if (elem < 100)  format(elem, nsmall=1, scientific=FALSE) else
                         format(elem, big.mark=',', digits=0, scientific=FALSE)))

    return(format(x, big.mark=',', digits=4, scientific=FALSE)) # 000's separator
    #format(x, digits=4, scientific=FALSE)	# other format options
}
myformat_number(c(1000, 100, 10, 1, 0.1, 0.01, 0.001, 0.0001))

myformat_time_MS <- function(x) {
    if (class(x) != "num") x <- as.numeric(x)
    m <- as.integer(x / 100)
    s <- x %% 100
    return(sprintf('%02d:%02d', m, s)) # Format the string as MM:SS
}
#myformat_time_MS(0)
#myformat_time_MS(2355)

mysort_df <- function(df, col_name, desc=FALSE) {
    return(df[order(df[, col_name], decreasing=desc), ])
}

## 02.2	    manage (impute/delete) missing data
#intersect(names(obs_df), names(entity_agg_intrvl_df))
#entimptd_df <- join(obs_df, entity_agg_intrvl_df, by="interval")
#entimptd_df <- mutate(entimptd_df, steps_imputed=ifelse(is.na(steps), steps_mean,
#                                                        steps))

## 02.3	    encode/retype data (convert types; map codes)
mycheck_map_results <- function(mapd_df, from_col_name, to_col_name, print.all=FALSE) {
    if (length(unique(mapd_df[, from_col_name])) == nrow(mapd_df))
        map_summry_df <- mapd_df else {
        require(sqldf)

        # _n does not work with ggplot
        sql <- "SELECT "
        sql <- paste0(sql, "\"", from_col_name, "\", ")
        sql <- paste0(sql, "\"", to_col_name, "\" ")
      	sql <- paste0(sql, ", SUM(1) AS \".n\" ")
#     	sql <- paste0("SELECT ", paste(from_col_name, to_col_name, sep=","), ", SUM(1) AS \".n\" ")
        sql <- paste0(sql, "FROM mapd_df GROUP BY ")
        sql <- paste0(sql, "\"", from_col_name, "\", ")
        sql <- paste0(sql, "\"", to_col_name, "\" ")
        sql <- paste0(sql, "ORDER BY \".n\" DESC")
#         sql <- paste(sql, "FROM mapd_df GROUP BY", paste(from_col_name, to_col_name, sep=","),
#                      "ORDER BY _n DESC", sep=" ")
        map_summry_df <- sqldf(sql)
    }

    if (print.all) print(map_summry_df) else myprint_df(map_summry_df)

	# Works only for 1:1 mapping;
	#	Use fill aesthetic to display n:m mappings ?
	#		Create a variable that contains n:m ratio for each value of to_col_name ?

    if (".n" %in% names(map_summry_df)) {
    	print(ggplot(map_summry_df, aes_string(x=to_col_name, y=".n",
                                           fill=paste0("factor(", from_col_name, ")"))) +
                     geom_bar(stat="identity") + coord_flip())
    # 	print(myplot_hbar(map_summry_df[1:min(nrow(map_summry_df), 10),], to_col_name, "_n"))
    } else {
        # Continous distribution
        print(myplot_scatter(map_summry_df, from_col_name, to_col_name))
    }
}

# mymap <- function(df, from_col_name, to_col_name, map_func) {
#     df[, to_col_name] <- sapply(df[, from_col_name], map_func)

mymap_codes <- function(df, from_col_name, to_col_name,
						map_df, map_join_col_name=from_col_name,
								map_tgt_col_name=to_col_name) {

# 	if (length(intersect(names(df), names(map_df))) > 0)
# 		warning("potential column join conflicts: ", intersect(names(df), names(map_df)))

	ret_df <- merge(df, map_df[, c(map_join_col_name, map_tgt_col_name)],
                    by.x=from_col_name, by.y=map_join_col_name, all.x=TRUE)

#     df[, to_col_name] <- sapply(df[, from_col_name], map_func)

	mycheck_map_results(ret_df, from_col_name, to_col_name)
    return(ret_df)
}

## 03.	    extract features
## 03.1	    create feature types
mycreate_date2daytype <- function (df, date_col_name) {
    new_df <- df
    varname_suffix_sep <- "."

    day_col_name <- paste(date_col_name, "day", sep=varname_suffix_sep)
    new_df[, day_col_name] <- weekdays(as.Date(df[, date_col_name]))

    daytype_col_name <- paste(date_col_name, "dytyp", sep=varname_suffix_sep)
    new_df[, daytype_col_name] <- ifelse((new_df[, day_col_name] == "Saturday") |
                                             (new_df[, day_col_name] == "Sunday"),
                                         "weekend", "weekday")
    count_df <- mycreate_xtab_df(new_df, c(date_col_name, "date.dytyp"))
    myprint_df(count_df)
    return(new_df)
}
#entity_agg_date_df <- mycreate_date2daytype(entity_agg_date_df, "date")

mycount_pattern_occ <- function(pattern, str_vctr, perl=FALSE)
    sapply(str_vctr,
           function(str) sum(gregexpr(pattern, str, ignore.case=TRUE, perl=perl)[[1]] > 0))

mykntpar_pattern_occ <- function(pattern, str_vctr, perl=FALSE)
    unlist(plyr::llply(str_vctr,
            function(str) sum(gregexpr(pattern, str, ignore.case = TRUE, perl = perl)[[1]] > 0),
                       .parallel = TRUE))

myextract_dates_df <- function(df, vars, id_vars, rsp_var) {
    keep_feats <- c(NULL)
    dates_df            <- df[, id_vars, FALSE]
    dates_df[, rsp_var] <- df[, rsp_var, FALSE]

    for (var in vars) {
        #dates_df <- data.frame(.date=strptime(df[, var], "%Y-%m-%d %H:%M:%S"))
        dates_df <- cbind(dates_df,
                          data.frame(.date = strptime(df[, var],
                                                      glbFeatsDateTime[[var]]["format"],
                                                     tz = glbFeatsDateTime[[var]]["timezone"])))
        #         print(dates_df[is.na(dates_df$.date), c("ID", "Arrest.fctr", ".date")])
        #         print(glbObsAll[is.na(dates_df$.date), c("ID", "Arrest.fctr", "Date")])
        #         print(head(glbObsAll[grepl("4/7/02 .:..", glbObsAll$Date), c("ID", "Arrest.fctr", "Date")]))
        #         print(head(strptime(glbObsAll[grepl("4/7/02 .:..", glbObsAll$Date), "Date"], "%m/%e/%y %H:%M"))
        # Wrong data during EST->EDT transition
        #         tmp <- strptime("4/7/02 2:00","%m/%e/%y %H:%M:%S"); print(tmp); print(is.na(tmp))
        #         dates_df[dates_df$ID == 2068197, .date] <- tmp
        #         grep("(.*?) 2:(.*)", glbObsAll[is.na(dates_df$.date), "Date"], value=TRUE)
        #         dates_df[is.na(dates_df$.date), ".date"] <-
        #             data.frame(.date=strptime(gsub("(.*?) 2:(.*)", "\\1 3:\\2",
        #                 glbObsAll[is.na(dates_df$.date), "Date"]), "%m/%e/%y %H:%M"))$.date
        if (sum(is.na(dates_df$.date)) > 0) {
            tmpDf <- df[is.na(dates_df$.date), c(id_vars, rsp_var, var)]
            print(dim(tmpDf)); print(tmpDf)
            stop("NA POSIX dates for ", var)
        }

        .date <- dates_df$.date
        dates_df[, paste0(var, ".POSIX")] <- .date
        dates_df[, paste0(var, ".year")] <- as.numeric(format(.date, "%Y"))
        dates_df[, paste0(var, ".year.fctr")] <- as.factor(format(.date, "%Y"))
        dates_df[, paste0(var, ".month")] <- as.numeric(format(.date, "%m"))
        dates_df[, paste0(var, ".month.fctr")] <- as.factor(format(.date, "%m"))
        dates_df[, paste0(var, ".date")] <- as.numeric(format(.date, "%d"))
        dates_df[, paste0(var, ".date.fctr")] <-
            cut(as.numeric(format(.date, "%d")), 5) # by month week
        dates_df[, paste0(var, ".juliandate")] <- as.numeric(format(.date, "%j"))
        if (is.numeric(dates_df[, rsp_var]))
            print(myplot_scatter(dates_df, paste0(var, ".juliandate"), rsp_var)) else
        { # Classification
            if (length(unique(dates_df[, paste0(var, ".year")])) <= 5)
                print(myplot_histogram(df = dates_df, hst_col_name = paste0(var, ".juliandate")) +
                          facet_grid(as.formula(paste0(var, ".year", "~", rsp_var)))) else {
                pltDf <- dates_df
                pltDf[, paste0(var, ".year", ".cut")] <- cut(pltDf[, paste0(var, ".year")], 5)
                print(myplot_histogram(df = pltDf, hst_col_name = paste0(var, ".juliandate")) +
                          facet_grid(as.formula(paste0(var, ".year.cut", "~", rsp_var))))
            }
        }

        # wkday Sun=0; Mon=1; ...; Sat=6
        dates_df[, paste0(var, ".wkday")] <- as.numeric(format(.date, "%w"))
        if (is.numeric(dates_df[, rsp_var]))
            print(myplot_scatter(dates_df, paste0(var, ".wkday"), rsp_var)) else
            print(myplot_histogram(dates_df, paste0(var, ".wkday")) +
                      facet_grid(as.formula(paste0(var, ".year", "~", rsp_var))))
        dates_df[, paste0(var, ".wkday.fctr")] <- as.factor(format(.date, "%w"))
        dates_df[, paste0(var, ".wkend")] <-
            as.numeric(dates_df[, paste0(var, ".wkday")] %in% c(0, 6))

        # Get US Federal Holidays for relevant years
        require(XML)
        hldays <- c(NULL)
        for (year in sort(unique(format(.date, "%Y")))) {
            doc.url <- paste0('http://about.usps.com/news/events-calendar/',
                              year, '-federal-holidays.htm')
            print(sprintf("   accessing url: %s", doc.url))
            doc.html <- try(htmlTreeParse(doc.url, useInternalNodes = TRUE))
            if (inherits(doc.html, "try-error")) {
                warning("unable to access url:", doc.url, "; skipping ...")
                next
            }

            #         # Extract all the paragraphs (HTML tag is p, starting at
            #         # the root of the document). Unlist flattens the list to
            #         # create a character vector.
            #         doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
            #         # Replace all \n by spaces
            #         doc.text = gsub('\\n', ' ', doc.text)
            #         # Join all the elements of the character vector into a single
            #         # character string, separated by spaces
            #         doc.text = paste(doc.text, collapse = ' ')

            # parse the tree by tables
            # txt <- unlist(strsplit(xpathSApply(doc.html, "//*/table", xmlValue), "\n"))
            # parse the tree by span & ul
            txt <- xpathSApply(doc.html, "//span/ul", xmlValue)
            if (length(txt) == 0) {
                warning("  xpathSApply did not work for url:", doc.url, "; skipping ...")
                next
            }
            txt <- unlist(strsplit(txt, "\n"))

            # do some clean up with regular expressions
            txt <- grep("day, ", txt, value = TRUE)
            txt <- trimws(gsub("(.*?)day, (.*)", "\\2", txt))
            txt <- gsub("(.+) ([[:digit:]]+) (.*)", "\\1 \\2", txt)
            #         txt <- gsub("\t","",txt)
            #         txt <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", txt, perl=TRUE)
            #         txt <- txt[!(txt %in% c("", "|"))]
            hldays <- union(hldays,
                    format(strptime(paste(txt, " ", year, sep = ""), "%B %e %Y"), "%Y-%m-%d"))
            if (any(is.na(hldays)))
                warning("US Federal Holidays not found for year: ", year)
        }
        dates_df[, paste0(var, ".hlday")] <-
            ifelse(format(.date, "%Y-%m-%d") %in% hldays, 1, 0)
        #print(table(format(dates_df[dates_df[, paste0(var, ".hlday")] == 1, ".date"], "%Y-%m-%d")))

        # NYState holidays 1.9., 13.10., 11.11., 27.11., 25.12. for 2014
        print("**********")
        print(sprintf("Consider adding state & city holidays for glbFeatsDateTime: %s", var))
        print("**********")
        if (is.numeric(dates_df[, rsp_var]))
            print(myplot_scatter(dates_df, paste0(var, ".hlday"), rsp_var)) else
            print(myplot_histogram(dates_df, paste0(var, ".hlday")) +
                      facet_grid(as.formula(paste0(var, ".year", "~", rsp_var))))

        dates_df[, paste0(var, ".hour")] <- as.numeric(format(.date, "%H"))
        if (is.numeric(dates_df[, rsp_var]))
            print(myplot_scatter(dates_df, paste0(var, ".hour"), rsp_var)) else
            print(myplot_histogram(dates_df, paste0(var, ".hour")) +
                  facet_grid(as.formula(paste0(var, ".year", "~", rsp_var))))
        dates_df[, paste0(var, ".hour.fctr")] <-
            if (length(unique(vals <- as.numeric(format(.date, "%H")))) <= 1)
                vals else cut(vals, 3) # by work-shift
        dates_df[, paste0(var, ".minute")] <- as.numeric(format(.date, "%M"))
        dates_df[, paste0(var, ".minute.fctr")] <-
            if (length(unique(vals <- as.numeric(format(.date, "%M")))) <= 1)
                vals else cut(vals, 4) # by quarter-hours
        dates_df[, paste0(var, ".second")] <- as.numeric(format(.date, "%S"))
        dates_df[, paste0(var, ".second.fctr")] <-
            if (length(unique(vals <- as.numeric(format(.date, "%S")))) <= 1)
                vals else cut(vals, 4) # by quarter-minutes

        dates_df[, paste0(var, ".day.minutes")] <-
            60 * dates_df[, paste0(var, ".hour")] + dates_df[, paste0(var, ".minute")]
        #         print(gp <- myplot_box(df=dates_df, ycol_names="PubDate.day.minutes",
        #                                xcol_name=rsp_var))
        #         print(gp <- myplot_scatter(df=dates_df, xcol_name=".rownames",
        #                         ycol_name="PubDate.day.minutes", colorcol_name=rsp_var))
        #         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate",
        #                         ycol_name="PubDate.day.minutes.poly.1", colorcol_name=rsp_var))
        #         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.day.minutes",
        #                         ycol_name="PubDate.day.minutes.poly.4", colorcol_name=rsp_var))
        #
        #         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate",
        #                         ycol_name="PubDate.day.minutes", colorcol_name=rsp_var, smooth=TRUE))
        #         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate",
        #                         ycol_name="PubDate.day.minutes.poly.4", colorcol_name=rsp_var, smooth=TRUE))
        #         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate",
        #                         ycol_name=c("PubDate.day.minutes", "PubDate.day.minutes.poly.4"),
        #                         colorcol_name=rsp_var))

        #         print(gp <- myplot_scatter(df=subset(dates_df, Popular.fctr=="Y"),
        #                                    xcol_name=paste0(var, ".juliandate"),
        #                         ycol_name=paste0(var, ".day.minutes", colorcol_name=rsp_var))
        #         print(gp <- myplot_box(df=dates_df, ycol_names=paste0(var, ".hour"),
        #                                xcol_name=rsp_var))
        #         print(gp <- myplot_bar(df=dates_df, ycol_names=paste0(var, ".hour.fctr"),
        #                                xcol_name=rsp_var,
        #                                colorcol_name=paste0(var, ".hour.fctr")))
        keep_feats <- union(keep_feats, paste(var,
            c(".POSIX", ".year.fctr", ".month.fctr", ".date.fctr", ".juliandate",
              ".wkday.fctr", ".wkend", ".hlday",
              ".hour.fctr", ".minute.fctr", ".second.fctr",
              ".day.minutes"),
                                                sep = ""))
        if (length(missFeats <- setdiff(keep_feats, names(dates_df))) > 0)
            warning("Missing features for glbFeatsDateTime: ", var, " :", missFeats)
        keep_feats <- intersect(keep_feats, names(dates_df))
    }
    #myprint_df(dates_df)
    return(dates_df[, keep_feats])
}

myextractTimePoly <- function(obs, feat) {
    retObs <- data.frame()

    if ((unq_vals_n <- length(unique(obs[, paste0(feat, ".day.minutes")]))) > 1) {
        max_degree <- min(unq_vals_n, 5)

        # setup appropriate row.names
        retObs <- obs[, paste0(feat, ".day.minutes"), FALSE]
        retObs[, paste0(feat, ".day.minutes.poly.", 1:max_degree)] <-
            as.matrix(poly(obs[, paste0(feat, ".day.minutes")], max_degree))
        return(retObs[, -1, FALSE])
    } else return(retObs)
}


myextractTimeLags <- function(Obs, FeatTime, rsp_var, rsp_var_raw, impute.na = "TRUE") {
    # Create features that measure the gap between previous timestamp in the data

    addedFeats <- c()
    require(zoo)

    # Find provided order of observations
    Obs$.order <- seq(1:nrow(Obs))
    Obs <- orderBy(reformulate(paste0(FeatTime, ".POSIX")), Obs)
    z <- zoo(as.numeric(as.POSIXlt(Obs[, paste0(FeatTime, ".POSIX")])))
    Obs[, paste0(FeatTime, ".zoo")] <- z; addedFeats <- union(addedFeats, paste0(FeatTime, ".zoo"))
    #print(head(Obs[, c(glb_id_var, FeatTime, paste0(FeatTime, ".zoo"))]))
    b <- zoo(, seq(nrow(Obs)))

    # last<k> computes the time difference between an obs & its kth obs
    for (k in head((2 ^ (1:floor(log(length(z), 2)))), 5)) {
        if (is.na(k)) break
        last <- as.numeric(merge(z - lag(z, k), b, all = TRUE))
        if (!as.logical(impute.na))
            last[is.na(last)] <- 0
        Obs[, paste0(FeatTime, ".last", as.character(k), ".log1p")] <- log1p(last)
        addedFeats <- union(addedFeats, paste0(FeatTime, ".last", as.character(k), ".log1p"))
#         print(gp <- myplot_violin(df = Obs,
#                                  ycol_names = paste0(FeatTime, ".last", as.character(k), ".log1p"),
#                                   xcol_name = rsp_var))
    }

    if (length(myfind_numerics_missing(Obs[,
                                          grepl(FeatTime, names(Obs), fixed = TRUE)], NULL)) > 0) {
        impObs <- myimputeMissingData(Obs[, setdiff(names(Obs),
                c(rsp_var, rsp_var_raw, paste0(FeatTime, ".POSIX"), paste0(FeatTime, ".zoo")))])
        Obs <- cbind(Obs[, setdiff(names(Obs), names(impObs))], impObs)
    }
    retObs <- orderBy(~.order, Obs[, c(".order", addedFeats), FALSE])

    return(retObs[, -1, FALSE])
}

mygetTxtTerms <- function(terms_TDM, rspVctr,
						  compute.cor.y = FALSE, compute.nzv = FALSE, compute.chisq = FALSE,
						  compute.classWeights = FALSE) {
	tmEnter <- as.numeric(proc.time()["elapsed"])
	print(sprintf("mygetTxtTerms: enter: elapsed: %0.2f secs",
				  as.numeric(proc.time()["elapsed"]) - tmEnter))

	terms_mtrx <- as.matrix(as.TermDocumentMatrix(terms_TDM))
	docms_mtrx <- as.matrix(as.DocumentTermMatrix(terms_TDM))
	terms_df <- data.frame(term = dimnames(terms_mtrx)$Terms,
						   weight = rowSums(terms_mtrx),
						   freq = rowSums(terms_mtrx > 0))
	terms_df$pos <- 1:nrow(terms_df)
	print(sprintf("mygetTxtTerms: terms_df setup: elapsed: %0.2f secs",
				  as.numeric(proc.time()["elapsed"]) - tmEnter))

	if (compute.cor.y) {
		terms_df$cor.y <- as.vector(
			cor(docms_mtrx[!is.na(rspVctr),],
				as.numeric(rspVctr[!is.na(rspVctr)]),
							  use = "pairwise.complete.obs"))
		terms_df$cor.y.abs <- abs(terms_df$cor.y)
#         .rnorm.cor.y.abs <- abs(cor(glbObsAll[glbObsAll$.src == "Train", ".rnorm"],
#                         as.numeric(glbObsAll[glbObsAll$.src == "Train", glb_rsp_var]),
#                                 use = "pairwise.complete.obs"))
		print(sprintf("mygetTxtTerms: terms_df cor.y: elapsed: %0.2f secs",
					  as.numeric(proc.time()["elapsed"]) - tmEnter))
	}

	# compute nzv before chisq.test since terms with nzv.freqRatio beyond a max threshold are not chisq.significant ???
	if (compute.nzv) {
		nzv_df <-
#                 caret::nzv(docms_mtrx[glbObsAll$.src == "Train",], freqCut = glbFeatsNzvFreqMax,
#                             uniqueCut = glbFeatsNzvUniqMin, saveMetrics = TRUE)
			# caret::nearZeroVar(docms_mtrx[glbObsAll$.src == "Train",],
			mycaret.nearZeroVar(docms_mtrx[glbObsAll$.src == "Train",],
							   freqCut = glbFeatsNzvFreqMax, uniqueCut = glbFeatsNzvUniqMin,
							   saveMetrics = TRUE, foreach = TRUE)
		terms_df$nzv.freqRatio <- nzv_df$freqRatio
#         terms_df$nzv.freqRatio.cut.fctr <- cut(terms_df$nzv.freqRatio,
#                                                breaks = sort(c(min(terms_df$nzv.freqRatio),
#                                                                 glbFeatsNzvFreqMax,
#                                                            max(terms_df$nzv.freqRatio))))
		terms_df$nzv.percentUnique <- nzv_df$percentUnique
#         terms_df$nzv.percentUnique.cut.fctr <- cut(terms_df$nzv.percentUnique,
#               breaks = sort(c(min(terms_df$nzv.percentUnique) - .Machine$double.neg.eps,
#                                                             glbFeatsNzvUniqMin,
#                                                       max(terms_df$nzv.percentUnique))))
#         terms_df$nzv.quad.fctr <- as.factor(paste0("fRatio:",
#          terms_df$nzv.freqRatio.cut.fctr,
#                                             "\n%Unq:", terms_df$nzv.percentUnique.cut.fctr))
		terms_df$nzv <- nzv_df$nzv
		print(sprintf("mygetTxtTerms: terms_df nzv: elapsed: %0.2f secs",
					  as.numeric(proc.time()["elapsed"]) - tmEnter))
	}

	# to check is nzv.freqRatio max threshold is properly set
# summary(full_terms_df$nzv.freqRatio)
# full_terms_df$chisq.pval.significant <-
#     cut(full_terms_df$chisq.pval, breaks = c(0, 0.05, max(full_terms_df$chisq.pval, na.rm = TRUE)))
# ggplot(full_terms_df, mapping = aes(x = nzv.percentUnique, y = nzv.freqRatio)) +
#     geom_point(aes(color = chisq.pval.significant), position = "jitter")
# significant_terms_df <- subset(full_terms_df, chisq.pval < 0.05)
# summary(significant_terms_df$nzv.freqRatio)

	if (compute.chisq) {
		naDf <- data.frame(chisq.stat = NA, chisq.pval = NA)
		trnObsIx <- !is.na(rspVctr)
		chisqDf <- foreach(ix = 1:nrow(terms_df), .combine = rbind.data.frame) %dopar%
		# chisqDf <- foreach(ix = 1:nrow(terms_df), .combine = rbind.data.frame) %do%
		# chisqDf <- foreach(ix = 1:30, .combine = rbind) %do%
#                 if ((length(unique(docms_mtrx[trnObsIx, ix])) > 1) &&
#                     (terms_df[ix, "nzv.freqRatio"] <= 3265)) # Empirical setting
			if (length(unique(docms_mtrx[trnObsIx, ix])) > 1)
			{
				chisq <- chisq.test(docms_mtrx[trnObsIx, ix],
									rspVctr[trnObsIx])
				tmpDf <- data.frame(chisq.stat = chisq$statistic,
									chisq.pval = chisq$p.value)
			} else {
				tmpDf <- naDf
			}
#         terms_df[ix, "chisq.stat"] <- chisq$statistic
#         terms_df[ix, "chisq.pval"] <- chisq$p.value
		terms_df <- cbind(terms_df, chisqDf)
		print(sprintf("mygetTxtTerms: terms_df chisq.test: elapsed: %0.2f secs",
					  as.numeric(proc.time()["elapsed"]) - tmEnter))
	}

	if (compute.classWeights) {
		for (cls in unique(glbObsAll[, glb_txt_cor_var])) {
			if (!is.na(cls)) {
				obsMask <- as.numeric(!is.na(glbObsAll[, glb_txt_cor_var]) &
										(glbObsAll[, glb_txt_cor_var] == cls))
			} else {
				obsMask <- as.numeric(is.na(glbObsAll[, glb_txt_cor_var]))
			}
			terms_df[, paste0("weight.mean.", as.character(cls))] <-
				colSums(t(terms_mtrx) * obsMask) * 1.0 / sum(obsMask)
		}
		print(sprintf("mygetTxtTerms: terms_df weight.glb_txt_cor_var: elapsed: %0.2f secs",
				  as.numeric(proc.time()["elapsed"]) - tmEnter))
	}

	print(sprintf("mygetTxtTerms: exit: elapsed: %0.2f secs",
				  as.numeric(proc.time()["elapsed"]) - tmEnter))

	# Check all calls to get_terms_DTM_terms to change returned order assumption
	return(terms_df <- orderBy(~ -weight, terms_df))
}

myimputeMissingData <- function(inpObs, miceSeed = 144) {
    require(mice)

    set.seed(miceSeed)
    print("Summary before imputation: ")
    print(summary(inpObs))
    retObs <- complete(mice(inpObs[, setdiff(names(inpObs), myfind_chr_cols_df(inpObs))]))
    print(summary(retObs))

    ret_vars <- sapply(names(retObs),
                       function(col) ifelse(!identical(retObs[, col],
                                                       inpObs[, col]),
                                            col, ""))
    ret_vars <- ret_vars[ret_vars != ""]

    # complete(mice()) changes attributes of factors even though values don't change
    for (col in ret_vars) {
        if (inherits(retObs[, col], "factor")) {
            if (identical(as.numeric(retObs[, col]),
                          as.numeric(inpObs[, col])))
                ret_vars <- setdiff(ret_vars, col)
        }
    }
    return(retObs[, ret_vars, FALSE])
}

## 03.2		filter features
#features_lst <- features_lst[features_lst != "price"]
#fctrs_lst <- fctrs_lst[!sapply(fctrs_lst, is.null)]

## 03.3     create feature combinations
## 03.4		add random variable

## 04.	    partition data
## 04.1	    simple shuffle sample
## 04.2     stratified shuffle sample
mypartition_data <- function(more_stratify_vars=NULL) {
    print(" "); print(sprintf("nrow(obs_df): %s", format(nrow(obs_df), big.mark=',')))
    if (missing(more_stratify_vars)) {
        print(table(obs_df[, response_varname]))
        keep_cols <- response_varname
    } else {
        entity_tbl <- table(obs_df[, response_varname],
                            obs_df[, more_stratify_vars])
        print(entity_tbl)
        keep_cols <- c(response_varname, more_stratify_vars)
    }

    inTrainValidate <- createDataPartition(y=obs_df[, response_varname],
                                           p=0.8, list=FALSE)
    train_validate_df <- obs_df[inTrainValidate, keep_cols]

    inTest <- setdiff(seq(1:nrow(obs_df)), inTrainValidate)
    inTest <- matrix(inTest, nrow=length(inTest),
                     dimnames=dimnames(inTrainValidate))
    test_df <- obs_df[inTest, keep_cols]

    inTrain <- createDataPartition(y=train_validate_df[, response_varname],
                                   p=0.8, list=FALSE)
    train_df <- train_validate_df[inTrain, keep_cols]

    inValidate <- setdiff(seq(1:nrow(train_validate_df)), inTrain)
    inValidate <- matrix(inValidate, nrow=length(inValidate),
                     dimnames=dimnames(inTrain))
    validate_df <- train_validate_df[inValidate, keep_cols]

    chk_nrow <- nrow(train_df) + nrow(validate_df) + nrow(test_df)
    if (nrow(obs_df) != chk_nrow)
        stop("obs_df not partitioned properly; nrow(obs_df): ",
             format(nrow(train_df), big.mark=','),
             " nrow of train+validate+test_df:",
             format(chk_nrow, big.mark=','))

    print(" "); print(sprintf("nrow(train_df): %s", format(nrow(train_df), big.mark=',')))
    print(suppressWarnings(formatC(table(train_df$classe,
        train_df[, more_stratify_vars]) / entity_tbl, digits=3, format='f')))

    print(" "); print(sprintf("nrow(validate_df): %s", format(nrow(validate_df), big.mark=',')))
    print(suppressWarnings(formatC(table(validate_df$classe,
        validate_df[, more_stratify_vars]) / entity_tbl, digits=3, format='f')))

    print(" "); print(sprintf("nrow(test_df): %s", format(nrow(test_df), big.mark=',')))
    print(suppressWarnings(formatC(table(test_df$classe,
        test_df[, more_stratify_vars]) / entity_tbl, digits=3, format='f')))

    return(list(inTrain=inTrain, inValidate=inValidate, inTest=inTest))
}
# mypartition_data_lst <- mypartition_data(more_stratify_vars="user_name")
# keep_cols <- c(feats_df$feature, response_varname, id_varnames)
# train_df <- obs_df[mypartition_data_lst$inTrain, keep_cols]
# validate_df <- obs_df[mypartition_data_lst$inValidate, keep_cols]
# test_df <- obs_df[mypartition_data_lst$inTest, keep_cols]

# all.equal(train_save_df, train_df)
# setdiff(union(names(train_save_df), names(train_df)), intersect(names(train_save_df), names(train_df)))


## 04.3	    cross-validation sample

## 05.      select features
#intersect(names(obs_df), names(entity_agg_intrvl_df))
#entimptd_df <- join(obs_df, entity_agg_intrvl_df, by="interval")
#entimptd_df <- mutate(entimptd_df, steps_imputed=ifelse(is.na(steps), steps_mean,
#                                                        steps))

## 05.1	    collect all non_na_numeric features
## 05.2	    remove row keys & prediction variable
## 05.3	    remove features that should not be part of estimation
## 05.4	    select significant features
myselect_features <- function(entity_df,  exclude_vars_as_features, rsp_var) {
	require(dplyr)

	# Collect numeric vars
    vars_tbl <- summary( entity_df)
    numeric_vars <- names( entity_df)[grep("^Min.", vars_tbl[1,])]

	# Collect factor & logical vars
	class_vctr <- sapply(names(entity_df), function(col_name) class(entity_df[, col_name]))
	factor_vars <- names(class_vctr[sapply(class_vctr,
	                                       function(thsVar) return(any(thsVar %in% c("factor"))))])
	logical_vars <- names(class_vctr[class_vctr == "logical"])

    # Exclude rsp_var & user-specified features
    #   keep user-specified excl. features since their cor.y is useful later
    sel_feats <- setdiff(c(numeric_vars, factor_vars, logical_vars),
#    						union(rsp_var, exclude_vars_as_features))
	                        rsp_var)

    feats_df <- data.frame(id=sel_feats,
                cor.y=cor(data.matrix(entity_df[, sel_feats]),
                            y=as.numeric( entity_df[, rsp_var]),
                            use="pairwise.complete.obs"))
#     feats_df <- mutate(feats_df, exclude.as.feat =
#                            ifelse(id %in% exclude_vars_as_features, 1, 0))
    feats_df$exclude.as.feat <- sapply(feats_df$id, function(id)
                           ifelse(id %in% exclude_vars_as_features, 1, 0))
    feats_df <- orderBy(~ -cor.y.abs, mutate(feats_df, cor.y.abs=abs(cor.y)))
    row.names(feats_df) <- feats_df$id
    return(feats_df)
}

# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg,
#              subset(cars_df, am_fctr == "manual")$mpg,
#              var.equal=FALSE)$conf)

## 05.5	    id features / create feature combinations for highly correlated features
myfind_cor_features <- function(feats_df, obs_df, rsp_var, nzv.freqCut=95/5, nzv.uniqueCut=10) {
	require(reshape2)
    require(caret)

	feats_df[, "cor.high.X"] <- NA
    nzv_df <- nearZeroVar(obs_df[, setdiff(names(obs_df), c(rsp_var, myfind_chr_cols_df(obs_df)))],
                          freqCut=nzv.freqCut, uniqueCut=nzv.uniqueCut, saveMetrics=TRUE)
    #nzv_df$id <- row.names(nzv_df)
#     print(setdiff(union(row.names(feats_df), row.names(nzv_df)), intersect(row.names(feats_df), row.names(nzv_df))))
    #print(row.names(feats_df))
    feats_df <- merge(feats_df, nzv_df, by="row.names", all=TRUE)
    row.names(feats_df) <- feats_df$id
    feats_df <- subset(feats_df, select=-Row.names)
#     feats_df$myNearZV <- ifelse(feats_df$zeroVar |
#     	(feats_df$nzv & (feats_df$freqRatio > (nrow(subset(obs_df, .src == "Train")) / 4))),
#     							TRUE, FALSE)

    cor_threshold <- feats_df[feats_df$id == ".rnorm", "cor.y.abs"]
# 	feats_df <- mutate(feats_df,
#                        is.cor.y.abs.low=ifelse(cor.y.abs >= cor_threshold, FALSE, TRUE))
	feats_df$is.cor.y.abs.low <- (feats_df$cor.y.abs < cor_threshold)
	if (nrow(feats_df) == 1)
		return(feats_df)

	chk_feats <- subset(feats_df, (exclude.as.feat == 0) & !zeroVar)$id
#     if (checkConditionalX) {
#         require(caret)
#         empty_dstrb_feats <- sort(chk_feats[checkConditionalX(obs_df[, chk_feats],
#                                                          obs_df[, rsp_var])])
#         feats_df[feats_df$id %in% empty_dstrb_feats, "is.ConditionalX.y"] <- FALSE
#         chk_feats <- setdiff(chk_feats, empty_dstrb_feats)
#         feats_df[feats_df$id %in% chk_feats, "is.ConditionalX.y"] <- TRUE
#     }
	chk_feats <- sort(subset(feats_df, (exclude.as.feat == 0) &
	                                   (!is.cor.y.abs.low) &
                                       #(is.ConditionalX.y)
                                       !zeroVar
                             )$id)
	if (length(chk_feats) > 100)
		stop("Number of feats to check for correlation: ",  length(chk_feats), " exceeds 100")
    repeat {
    	if (length(chk_feats) == 1)
    		break

        corxx_mtrx <- cor(data.matrix(obs_df[, chk_feats]),
        						use="pairwise.complete.obs")
        abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
        #print(abs_corxx_mtrx)
        if (max(abs_corxx_mtrx, na.rm=TRUE) < 0.7) break

        row_ix <- ceiling(which.max(abs_corxx_mtrx) / ncol(abs_corxx_mtrx))
        col_ix <- which.max(abs_corxx_mtrx[row_ix, ])
        feat_1 <- rownames(abs_corxx_mtrx)[row_ix]
        feat_2 <- rownames(abs_corxx_mtrx)[col_ix]
        print(sprintf("cor(%s, %s)=%0.4f", feat_1, feat_2, corxx_mtrx[row_ix, col_ix]))
#         print(myplot_scatter( obs_df, feat_1, feat_2))

        print(sprintf("cor(%s, %s)=%0.4f", rsp_var, feat_1,
             feats_df[feats_df$id == feat_1, "cor.y"]))
    #     print(myplot_scatter( obs_df, rsp_var, feat_2))
        print(sprintf("cor(%s, %s)=%0.4f", rsp_var, feat_2,
             feats_df[feats_df$id == feat_2, "cor.y"]))
    #     print(myplot_scatter( obs_df, rsp_var, feat_2))

#         plot_df <- melt( obs_df, id.vars=rsp_var, measure.vars=c(feat_1, feat_2))
#         print(myplot_scatter(plot_df, rsp_var, "value",
#                              facet_colcol_name="variable", smooth=TRUE))

#         if ( id_var %in% c(feat_1, feat_2)) drop_feat <-  id_var else {
#   	  if (intersect( id_vars, c(feat_1, feat_2)))
# 		if (feat_1 %in%  exclude_vars_as_features) drop_feat <- feat_1 else {
# 			if (feat_2 %in% exclude_vars_as_features) drop_feat <- feat_2 else {
				drop_feat <- ifelse(
					abs( feats_df[ feats_df$id == feat_1, "cor.y"]) >=
					abs( feats_df[ feats_df$id == feat_2, "cor.y"]),
									feat_2, feat_1)
                if (drop_feat == feat_1)
                    feats_df[feats_df$id == feat_1, "cor.high.X"] <- feat_2 else
                    feats_df[feats_df$id == feat_2, "cor.high.X"] <- feat_1
				#feats_df[feats_df$id %in% c(feat_1, feat_2), ]
# 			}
# 		}
#         }
        warning("Identified ", drop_feat, " as highly correlated with ",
                ifelse(drop_feat == feat_1, feat_2, feat_1))
        #print("checking correlations for features:")
        #print(chk_feats <- chk_feats[chk_feats != drop_feat])
        chk_feats <- chk_feats[chk_feats != drop_feat]
    }

# 	feats_df[, "cor.low"] <- 0
#     feats_df[feats_df$id %in% chk_feats, "cor.low"] <- 1
    return(feats_df)
}

myget_vectorized_obs_df <- function(obs_df, rsp_var, indep_vars) {
    # Convert all factors (incl. interactions) to dummy vectors
    vctobs_df <- obs_df[, c(rsp_var, unique(unlist(strsplit(indep_vars, "[:|*]"))))]

    # Need to modify for interaction vars that are features
    fctr_vars <- grep(".fctr", indep_vars, fixed=TRUE, value=TRUE)
    if (length(fctr_vars) > 0)
        vctobs_df <- cbind(vctobs_df[, -grep(gsub(".", "\\.",
                                paste0(unique(unlist(strsplit(fctr_vars, "[:]"))), collapse="|"),
                                                    fixed=TRUE), names(vctobs_df))],
                            model.matrix(reformulate(c(0, fctr_vars)), vctobs_df))
    # Because predictors is a function for rfe
    #predictors_vctr <- setdiff(names(vctobs_df), rsp_var)
    return(vctobs_df)
}

## 05.5.1	add back in key features even though they might have been eliminated
## 05.5.2   cv of significance
## 05.6     scale / normalize selected features for data distribution requirements in various models

## 06.	    select models
## 06.1	    select base models
## 06.1.1	regression models
# prediction_mdl <- lm(reformulate(features_lst, response="price"),
#                      data = diamonds_df)

## 06.1.2	classification models
## 06.1.3	clustering models
## 06.1.4	dimensionality reduction models
## 06.2	    select ensemble models

## 07.	    design models
## 07.1	    identify model parameters (e.g. # of neighbors for knn, # of estimators for ensemble models)

## 08.	    fit models

myadjustInteractionFeats <- function(featsDf, vars_vctr) {
    for (feat in subset(featsDf, !is.na(interaction.feat))$id)
        if (feat %in% vars_vctr)
            vars_vctr <- union(setdiff(vars_vctr, feat),
                paste0(featsDf[featsDf$id == feat, "interaction.feat"], ":",
                       feat))
    return(vars_vctr)
}

mygetIndepVar <- function(featsDf) {
	return(myadjustInteractionFeats(featsDf,
									  subset(featsDf, !nzv & (exclude.as.feat != 1))[, "id"]))
}

myprint_mdl <- function(mdl) {
	if (!inherits(mdl, "train"))
        stop("Not a legitimate \"train\" object")

    lcl_mdl <- mdl$finalModel

    if (inherits(lcl_mdl, "bagEarth")) {
        # plot crashes in certain conditions
        x <- lcl_mdl
        if (is.list(x) && all(c("x", "y") %in% names(x)))
            plot(lcl_mdl, ask=FALSE)

        print(summary(lcl_mdl))
        return(TRUE)
    }

    if (inherits(lcl_mdl, "bayesglm")) {
        # plot crashes in certain conditions
        require(Matrix)
        mqr <- qr(lcl_mdl)
        if (length(lcl_mdl$residuals) == as.integer(nrow(mqr$qr)))
            plot(lcl_mdl, ask=FALSE)

        print(summary(lcl_mdl))
        return(TRUE)
    }

    if (inherits(lcl_mdl, "glmnet")) {
        plot(lcl_mdl, ask=FALSE)
        print(summary(lcl_mdl))
        # Need to find array offset of best-tuned lambda
        if (length(exact <- which(lcl_mdl$lambda == lcl_mdl$lambdaOpt)) != 0)
            stop("not implemented yet")
        else {

            lclGetCoefs <- function(pos) {
                coefs <- coef(lcl_mdl)
                if (class(coefs) != "list") {
                    coefs_pos <- coefs[, pos]
                    print(coefs_pos[coefs_pos != 0])
                } else { # Multinomial model has coefs for each class
                    for (cls in names(coefs)) {
                        print(sprintf("class: %s:", cls))
                        coefs_pos <- coefs[[cls]][, pos]
                        print(coefs_pos[coefs_pos != 0])
                    }
                    # For multinomials only the last class coefs are returned
                    coefs_pos <- coefs[[tail(names(coefs), 1)]][, pos]
                }
                return(coefs_pos)
            }

            print("min lambda > lambdaOpt:")
            if (length(positions <- which(lcl_mdl$lambda > lcl_mdl$lambdaOpt)) > 0) {
                coefs_left <- lclGetCoefs(max(positions))
            } else coefs_left <- NULL

            print("max lambda < lambdaOpt:")
            if (length(positions <- which(lcl_mdl$lambda < lcl_mdl$lambdaOpt)) > 0) {
                coefs_rght <- lclGetCoefs(min(positions))
            } else coefs_rght <- NULL

            if (length(feats <- setdiff(names(coefs_left), names(coefs_rght))) > 0) {
                print("Feats mismatch between coefs_left & rght:")
                print(feats)
            }
            if (length(feats <- setdiff(names(coefs_rght), names(coefs_left))) > 0) {
                print("Feats mismatch between coefs_rght & left:")
                print(feats)
            }
        }
        return(TRUE)
    }

    if (inherits(lcl_mdl, "ksvm")) {
        if (type(lcl_mdl) == "C-svc" || type(lcl_mdl) == "nu-svc")
            # plots of regression (eps-svr) not supported
            plot(lcl_mdl, ask=FALSE)

        print(summary(lcl_mdl))
        return(TRUE)
    }

    if (inherits(lcl_mdl, "lda")) {
        # plot.lda fails due to lcl_mdl$call not being setup properly via caret
        #lcl_mdl$terms <- mdl$terms
        if (all(!grepl(":", dimnames(lcl_mdl$means)[[2]]))) {
            # Kludge doesn't work with interaction vars
            lclTrainingData <-
                myget_vectorized_obs_df(mdl$trainingData,
                                        rsp_var = names(mdl$trainingData)[1],
                                    indep_vars = tail(names(mdl$trainingData), -1))
            lcl_mdl$call$x <-
                expression(lclTrainingData[, dimnames(lcl_mdl$means)[[2]]])
            lcl_mdl$call$grouping <- expression(lclTrainingData[, 1])
            plot(lcl_mdl, ask = FALSE)
        }

        print(summary(lcl_mdl))
        return(TRUE)
    }

    if (inherits(lcl_mdl, "list")) {
		# Custom model
    	print(summary(lcl_mdl))
    	return(TRUE)
    }

    if (inherits(lcl_mdl, c("nnet", "avNNet"))) {
        # native plot crashes in certain conditions
        #plot(lcl_mdl, ask=FALSE)
        #         require(devtools)
        #         source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
        # plot.nnet(lcl_mdl)

        if (inherits(lcl_mdl, c("nnet"))) {
            require(NeuralNetTools)
            par(mar = numeric(4), family = 'serif')
            plotnet(mdl, alpha = 0.6)
            # plotnet(lcl_mdl, alpha = 0.6)
        } else if (inherits(lcl_mdl, c("avNNet"))) {
#             The problem is that the ‘avNNet’ method of train creates a final model that averages multiple nnet models to create the final output. As far as I can tell, the output doesn’t include enough information to use the plotting function. My suggestion is to recreate the individual models that were used to make the averaged models, then look at each model separately. Try this code, it isolates individual models from the output, recreates them, then uses the NeuralNetTools functions.

#             mod <- train(Y1 ~ X1 + X2 + X3, method = 'avNNet', data = neuraldat,
#                          linout = TRUE)
#
#             allmods <- mod$finalModel$model
#
#             mod <- allmods[[1]] # first model, change this number to look at the other models
#             wts <- mod$wts
#             decay <- mod$decay
#             struct <- mod$n
#
#             # recreate
#             recmod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, Wts = wts, decay = decay,
#                            size = struct[2], maxit = 0)
#
#             # use the functions to look at the individual model
#             plotnet(recmod)
        }

        print(summary(lcl_mdl))
        return(TRUE)
    }

    if (inherits(lcl_mdl, "rpart")) {
    	require(rpart.plot)
    	prp(lcl_mdl)

    	lcl_mdl$call <- lcl_mdl$call[-3]	# Hide "data" printing
    	print(summary(lcl_mdl))
    	return(TRUE)
    }

        plot(lcl_mdl, ask=FALSE)
    	print(summary(lcl_mdl))
    	return(TRUE)

#    stop("not implemented yet: ", class(lcl_mdl))

# 	#Customized plots for each method
# 	if (method == "glm") plot(native_mdl <-
# 			glm(reformulate(indep_vars_vctr, response=rsp_var), data=fit_df,
# 				   family="binomial"), ask=FALSE) else
# 	if (method == "rpart") {
# 		require(rpart)
# 	    require(rpart.plot)
# 	    if (is.null(loss_mtrx)) {
# 			prp(native_mdl <-
# 				rpart(reformulate(indep_vars_vctr, response=rsp_var), data=fit_df,
# 					   method="class", control=rpart.control(cp=mdl$bestTune$cp)))
# 		} else
# 			prp(native_mdl <-
# 				rpart(reformulate(indep_vars_vctr, response=rsp_var), data=fit_df,
# 					   method="class", control=rpart.control(cp=mdl$bestTune$cp),
# 					   parms=list(loss=loss_mtrx)))
# 	} else
# 		stop("not implemented yet")

}

mycompute_confusion_df <- function(obs_df, actual_var, predct_var) {

# 	print(as.table(confusionMatrix(obs_df[, predct_var],
# 								   obs_df[, actual_var])))

	#	Create a dummy matrix of 0s with actual outcomes
	#	& merge in appropriate predicted outcomes cols
	mrg_obs_xtab_df <- orderBy(reformulate(actual_var),
								mycreate_tbl_df(obs_df, actual_var)[, 1, FALSE])
	for (val in unique(mrg_obs_xtab_df[, 1]))
		mrg_obs_xtab_df[, paste(predct_var, val, sep=".")] <- 0
	#print(mrg_obs_xtab_df)

	#obs_xtab_df <- mycreate_xtab_df(obs_df, c(actual_var, predct_var))
	obs_xtab_df <- mycreate_sqlxtab_df(obs_df, c(actual_var, predct_var))
# 	obs_xtab_df <- tidyr::spread_(obs_xtab_df, "Popular.fctr.predict.MFO###myMFO_classfr", ".n")
	obs_xtab_df <- tidyr::spread_(obs_xtab_df, predct_var, ".n")
	names(obs_xtab_df)[2:ncol(obs_xtab_df)] <-
	    paste(predct_var, names(obs_xtab_df)[2:ncol(obs_xtab_df)], sep = ".")
	obs_xtab_df[is.na(obs_xtab_df)] <- 0
	#print(obs_xtab_df)

	for (col_ix in 2:ncol(obs_xtab_df))
		mrg_obs_xtab_df <- merge(mrg_obs_xtab_df[,
			-which(names(mrg_obs_xtab_df) == names(obs_xtab_df)[col_ix])],
								 obs_xtab_df[, c(1, col_ix)], all.x = TRUE)

	mrg_obs_xtab_df <- mrg_obs_xtab_df[, sort(names(mrg_obs_xtab_df))]
	mrg_obs_xtab_df[is.na(mrg_obs_xtab_df)] <- 0
# 	print(mrg_obs_xtab_df)
	return(mrg_obs_xtab_df)
}

mycompute_classifier_f.score <- function(mdl, obs_df, proba_threshold,
										rsp_var, rsp_var_out) {

	if ((class(obs_df[, rsp_var]) != "factor") |
		(length(levels(obs_df[, rsp_var])) != 2))
		stop("expecting a factor with two levels:", rsp_var)
	obs_df[, rsp_var_out] <-
		factor(levels(obs_df[, rsp_var])[
			(obs_df[, paste0(rsp_var_out, ".prob")] >=
				proba_threshold) * 1 + 1], levels(obs_df[, rsp_var]))

	#	Create a dummy matrix of 0s with actual outcomes
	#	& merge in appropriate predicted outcomes cols
# 	mrg_obs_xtab_df <- orderBy(reformulate(rsp_var),
# 								mycreate_tbl_df(obs_df, rsp_var)[, 1, FALSE])
# 	for (val in unique(mrg_obs_xtab_df[, 1]))
# 		mrg_obs_xtab_df[, paste(rsp_var_out, val, sep=".")] <- 0
# 	#print(mrg_obs_xtab_df)
#
# 	obs_xtab_df <- mycreate_xtab(obs_df, c(rsp_var, rsp_var_out))
# 	obs_xtab_df[is.na(obs_xtab_df)] <- 0
# 	#print(obs_xtab_df)
#
# 	for (col_ix in 2:ncol(obs_xtab_df))
# 		mrg_obs_xtab_df[, names(obs_xtab_df)[col_ix]] <-
# 			obs_xtab_df[, names(obs_xtab_df)[col_ix]]
	mrg_obs_xtab_df <- mycompute_confusion_df(obs_df, rsp_var, rsp_var_out)
	#print(mrg_obs_xtab_df)

	# This F-score formula ignores NAs in prediction.
	#	FN should be FN + knt(actual == +ve, predicted == NA) ???
	#	knt(actual == -ve, predicted == NA) may be ignored because that adds to TN which is
	#		not used in f.score ???
	#obs_f_score <- 2 * precision * recall / (precision + recall)
	#obs_f_score <- (2 * TP) / ((2 * TP) + FP + FN)
	return(f.score.obs <- (2 * mrg_obs_xtab_df[2,3]) /
					((2 * mrg_obs_xtab_df[2,3]) +
					mrg_obs_xtab_df[1,3] + mrg_obs_xtab_df[2,2]))
}

mycbind_df <- function(df1, df2) {
    if (ncol(df1) == 0) return(df2)
    if (ncol(df2) == 0) return(df1)

    if (nrow(df1) != nrow(df2))
        stop("not implemented yet")

    return(cbind(df1, df2))
}

myrbind_df <- function(df1, df2) {
	if ((nrow(df1) == 0) | (nrow(df2) == 0))
		return(rbind(df1, df2))

	if (length(union_names <- union(names(df1), names(df2))) !=
		length(intersect(names(df1), names(df2)))) {
		df1[ , setdiff(union_names, names(df1))] <- NA
		df2[ , setdiff(union_names, names(df2))] <- NA
	}

	return(rbind(df1[, union_names], df2[, union_names]))
}

mycreate_MFO_classfr <- function() {
	algrthm <- list(label="MFO.Classifier", type="Classification",
								library=NULL)
	algrthm$parameters <- data.frame(parameter=c("parameter"),
												class=c("character"),
												label=c("parameter"))
	algrthm$grid <- function (x, y, len = NULL, search = "grid")
	    data.frame(parameter = "none")
	algrthm$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
		print("in MFO.Classifier$fit")
		unique.vals <- sort(unique(y))
		unique.prob <- sort(table(y) / length(y), decreasing=TRUE)
		MFO.val <- names(unique.prob)[1]

		print("unique.vals:"); 	print(unique.vals)
		print("unique.prob:"); 	print(unique.prob)
		print("MFO.val:"); 		print(MFO.val)
		return(list(unique.vals=unique.vals, unique.prob=unique.prob,
                    MFO.val=MFO.val, x.names=dimnames(x)[[2]]))
	}
	algrthm$predict <- function(modelFit, newdata, preProc=NULL,
											submodels=NULL) {
		print("entr MFO.Classifier$predict")
		y <- factor(rep(modelFit$MFO.val, nrow(newdata)), levels=modelFit$unique.vals)
		if ((length(unique(y)) > 1) | (unique(y) != as.character(modelFit$MFO.val)))
			stop("this should not happen")
		print("exit MFO.Classifier$predict")
		return(y)
	}
	algrthm$prob <- function(modelFit, newdata, preProc=NULL, submodels=NULL) {
		print("in MFO.Classifier$prob")
		prob_df <- as.data.frame(matrix(rep(modelFit$unique.prob, nrow(newdata)),
										byrow=TRUE, ncol=length(modelFit$unique.prob),
								dimnames=list(NULL, as.character(modelFit$unique.vals))))
		print(head(prob_df))
		return(prob_df)
	}
	algrthm$sort <- function(x) x
	algrthm$levels <- function(x) levels(x)
	algrthm$varImp <- function(modelFit) {
	    print("in MFO.Classifier$varImp")
	    varimp_df <- data.frame(Overall=rep(0, length(modelFit$x.names)))
        rownames(varimp_df) <- modelFit$x.names
	    print(varimp_df)
	    return(varimp_df)
	}
	#print(algrthm)
	return(algrthm)
}

mycreate_random_classfr <- function() {
	algrthm <- list(label="Random.Classifier", type="Classification",
								library=NULL)
	algrthm$parameters <- data.frame(parameter=c("parameter"),
												class=c("character"),
												label=c("parameter"))
	algrthm$grid <- function (x, y, len = NULL, search = "grid")
	    data.frame(parameter = "none")
	algrthm$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
		return(list(unique.vals=sort(unique(y)), unique.prob=table(y) / length(y)))
	}
	algrthm$predict <- function(modelFit, newdata, preProc=NULL,
											submodels=NULL) {
		return(sample(modelFit$unique.vals, nrow(newdata), replace=TRUE,
						prob=modelFit$unique.prob))
	}
	algrthm$prob <- function(modelFit, newdata, preProc=NULL, submodels=NULL) {
		print("in Random.Classifier$prob")
		# Bass-ackwards: b/c in this case, outcomes are easier to predict than probs

		outcomes_vctr <- sample(modelFit$unique.vals, nrow(newdata), replace=TRUE,
								prob=modelFit$unique.prob)
		prob_df <- as.data.frame(matrix(rep(modelFit$unique.prob, nrow(newdata)),
										byrow=TRUE, ncol=length(modelFit$unique.prob),
								dimnames=list(NULL, as.character(modelFit$unique.vals))))
		for (row_ix in 1:nrow(newdata)) {
			max_col_ix <- which.max(prob_df[row_ix, ])
			out_col_name <- as.character(outcomes_vctr[row_ix])
			if (out_col_name != names(prob_df)[max_col_ix]) {
				#print(row_ix)
				tmp <- prob_df[row_ix, out_col_name]
				prob_df[row_ix, out_col_name] <- prob_df[row_ix, max_col_ix]
				prob_df[row_ix, max_col_ix] <- tmp
			}
		}
		#myprint_df(prob_df)
		return(prob_df)
	}
	algrthm$sort <- function(x) x
	algrthm$levels <- function(x) levels(x)
	#print(algrthm)
	return(algrthm)
}

# For debugging purposes
myfit_baseln_classfr <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    print("in Baseline.Classifier$fit")

    print("class(x):"); print(class(x))
    print("dimnames(x)[[2]]:"); print(dimnames(x)[[2]])
    print("length(x):"); print(length(x))
    print("head(x):"); print(head(x))

    print("class(y):"); print(class(y))
    #print("names(y):"); print(names(y))
    print("length(y):"); print(length(y))
    print("head(y):"); print(head(y))

    x_names <- setdiff(dimnames(x)[[2]], ".rnorm")
    # 		if (length(x_name) > 1)
    # 			stop("Baseline.Classifier currently supports only var vs. ", paste(x_name, sep=" "))

    if (inherits(y, "factor")) {
        if (!identical(sort(unique(x) + 1), sort(as.numeric(unique(y))))) {
            # x comes in as a dummy variable
            #   + 1 will work for binomials only ???
            #   For multinomials, x would be a matrix ???
            print("Baseline.Classifier:fit: Unique x values: ");
            print(sort(unique(x) + 1))
            print("Baseline.Classifier:fit: Unique y values: ");
            print(sort(unique(y)))
            stop("Baseline.Classifier expects identical x & y unique vals.")
        }
    }

    fit_df <- data.frame(x=x, y=y)
    map_freq_df <- mycreate_sqlxtab_df(fit_df, names(fit_df))
    print("    map_freq_df:"); print(map_freq_df)

    # To extract y value with the highest frequency
    #   map_freq_df[map_freq_df[, x_names] == 1, ][1, "y"]

    map_df <- data.frame(x = sort(unique(x)))
    map_df$y <- sapply(map_df$x,
        function (val) map_freq_df[map_freq_df[, x_names] == val, ][1, "y"])

    print("    map_df:"); print(map_df)

    #return(list(x_names=x_names, x_vals=as.character(unique(y))))
    return(list(x_names=x_names, map_df=map_df))
}

# predict fn needs to be called from prob fn where it's not accessible thru modelFit
mypredict_baseln_classfr <- function(modelFit, newdata, preProc=NULL,
                            submodels=NULL) {
    print("in Baseline.Classifier$predict")
    print("class(newdata):"); print(class(newdata))
    print("head(newdata):"); print(head(newdata))

    print("x_names: "); print(modelFit$x_names)
    #print("x_vals: "); print(modelFit$x_vals)

#     y <- factor(rep(modelFit$x_vals[1], nrow(newdata)), levels=modelFit$x_vals)
#     for (row_ix in 1:nrow(newdata)) {
#         if (sum(newdata[row_ix, modelFit$x_names]) > 0) {
#             # not the default level in the factor
#             y[row_ix] <- factor(modelFit$x_vals[which(newdata[row_ix, ] == 1) + 1],
#                                 levels=modelFit$x_vals)
#         }
#     }

    map_df <- modelFit$map_df
    y <- sapply(newdata[, modelFit$x_names],
                function(x_val) map_df[map_df$x == x_val, "y"])

    print("length(y):"); print(length(y))
    print("head(y):"); print(head(y))
    return(y)
}

mycreate_baseln_classfr <- function() {
	algrthm <- list(label="Baseline.Classifier", type="Classification",
								library=NULL)
	algrthm$parameters <- data.frame(parameter=c("parameter"),
												class=c("character"),
												label=c("parameter"))
	algrthm$grid <- function (x, y, len = NULL, search = "grid")
	    data.frame(parameter = "none")
	algrthm$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...)
		myfit_baseln_classfr(x, y, wts, param, lev, last, weights, classProbs, ...)
	algrthm$predict <- function(modelFit, newdata, preProc=NULL,
											submodels=NULL) {
	    return(mypredict_baseln_classfr(modelFit, newdata, preProc=NULL,
	                                    submodels=NULL))
	}
	algrthm$prob <- function(modelFit, newdata, preProc=NULL, submodels=NULL) {
		print("in Baseline.Classifier$prob")
		# Bass-ackwards: b/c in this case, outcomes are easier to predict than probs

	    print("    class(modelFit):"); print(class(modelFit))
	    print("    modelFit:"); print(modelFit)
		outcomes_vctr <- mypredict_baseln_classfr(modelFit, newdata)
		print("    outcomes_vctr:"); print(outcomes_vctr)
		col_names <- as.character(sort(unique(outcomes_vctr)))
		prob_df <- as.data.frame(matrix(rep(0, length(unique(outcomes_vctr)) *
		                                        nrow(newdata)),
										byrow=TRUE, nrow=nrow(newdata),
						dimnames=list(NULL, col_names)))
		for (row_ix in 1:nrow(newdata)) {
			out_col_ix <- grep(as.character(outcomes_vctr[row_ix]), col_names)
			prob_df[row_ix, out_col_ix] <- 1
		}
		print("    head(prob_df): "); print(head(prob_df))
		return(prob_df)
	}
	algrthm$sort <- function(x) x
	algrthm$levels <- function(x) levels(x)
	#print(algrthm)
	return(algrthm)
}

myextract_actual_feats <- function(vars) {
#     return(unique(unlist(lapply(strsplit(vars, ".fctr|:"), function(var_components_lst)
#     { if (length(var_components_lst) == 1) return(var_components_lst) else
#         return(paste0(var_components_lst[seq(1, length(var_components_lst), 2)], ".fctr")) }))))

    # Remove `` wrapper, if any
    ret_vars <- gsub("^`(.*)`$", "\\1", vars)

    # Split interactions, if any
    ret_vars <- unique(unlist(strsplit(ret_vars, "[:|\\*]")))

    # Remove values of factors, if any
    ret_vars <- unique(gsub("\\.fctr(.*)", "\\.fctr", ret_vars))

    return(ret_vars)
}

mygetPredictIds <- function(rsp_var, mdlId = NULL) {
    rsp_var_out <- paste0(rsp_var, ".", gsub("#", ".", mdlId))
    return(list(value   = rsp_var_out,
                prob    = paste0(rsp_var_out, ".prob"),
                is.acc  = paste0(rsp_var_out, ".is.acc"), # accurate
                err     = paste0(rsp_var_out, ".err"),
                err.abs = paste0(rsp_var_out, ".err.abs")))
}

myparseMdlId <- function(mdlId) {
    mdlComps <- unlist(strsplit(mdlId, "[#]"))
    return(list(family     = mdlComps[1],
                preProcess = mdlComps[2],
                resample   = plyr::revalue(mdlComps[3],c(NULL
                                    , " " = "none" # shd be "" but R treats it as an error
                                                        , "rcv" = "repeatedcv"
                                                        ),
                                           warn_missing = FALSE),
                alg        = mdlComps[4]))
}

mypredict_mdl <- function(mdl, df, rsp_var, label,
							model_summaryFunction=NULL, model_metric=NULL,
							model_metric_maximize=NULL,
							ret_type="stats") {
	if (!(ret_type %in% c("stats", "raw", "prob")))
		stop("ret_type: ", ret_type, " not supported")

	if (!(ret_type %in% c("stats", "raw")))
		stop("ret_type: ", ret_type, " not supported yet")

	if (mdl$modelType == "Classification") {
		is.binomial <- (length(unique(df[, rsp_var])) == 2)
	}
	stats_df <- data.frame(id = mdl$.myId)
	rsp_var_out <- mygetPredictIds(rsp_var)$value

	if ((mdl$modelType == "Classification") && is.binomial) {
	    requireNamespace("pROC")
	    df[, rsp_var_out] <- predict(mdl, newdata = df, type = "raw")
	    if (!is.ordered(df[, rsp_var_out]))
	        df[, rsp_var_out] <- ordered(df[, rsp_var_out], levels = levels(df[, rsp_var]))
	    rocObject <- try(pROC::roc(df[, rsp_var], df[, rsp_var_out]))
	    stats_df[, paste0("max.AUCpROC.", label)] <-
	        if (class(rocObject)[1] == "try-error") NA else rocObject$auc
	    stats_df[, paste0("max.Sens.", label)] <-
	        sensitivity(df[, rsp_var_out], df[, rsp_var], levels(df[, rsp_var])[1])
	    stats_df[, paste0("max.Spec.", label)] <-
	        specificity(df[, rsp_var_out], df[, rsp_var], levels(df[, rsp_var])[2])

		require(ROCR)
		df[, paste0(rsp_var_out, ".prob")] <- predict(mdl, newdata = df, type = "prob")[, 2]
		ROCRpred <- prediction(df[, paste0(rsp_var_out, ".prob")], df[, rsp_var])
		stats_df[, paste0("max.AUCROCR.", label)] <-
		    as.numeric(performance(ROCRpred, "auc")@y.values)

		ROCRperf <- performance(ROCRpred, "tpr", "fpr")
		if (length(ROCRperf@y.values[[1]]) > 2) {
			plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0, 1, 0.1),
			     text.adj = c(-0.2,1.7))
		}

		thresholds_df <- data.frame(threshold = seq(0.0, 1.0, 0.1))
		thresholds_df$f.score <- sapply(1:nrow(thresholds_df), function(row_ix)
		    mycompute_classifier_f.score(mdl, obs_df = df,
		                                 proba_threshold = thresholds_df[row_ix, "threshold"],
		                                 rsp_var, rsp_var_out))
		#print(thresholds_df)
		# Avoid picking 0.0 as threshold
		prob_threshold <- orderBy(~ -f.score -threshold, thresholds_df)[1, "threshold"]
# 		print(sprintf("Classifier Probability Threshold: %0.4f to maximize f.score.%s",
# 					  prob_threshold, label))
		# print(myplot_line(thresholds_df, "threshold", "f.score"))
		print(myplot_line(thresholds_df, "threshold", "f.score") +
		          geom_point(data = subset(thresholds_df, threshold == prob_threshold),
		                     mapping = aes(x = threshold, y = f.score),
		                     shape = 5, color = "red", size = 4))
		stats_df[, paste0("opt.prob.threshold.", label)] <- prob_threshold

		df[, rsp_var_out] <-
			factor(levels(df[, rsp_var])[
				(df[, paste0(rsp_var_out, ".prob")] >=
					prob_threshold) * 1 + 1], levels(df[, rsp_var]))

		#print(mycreate_xtab_df(df, c(rsp_var, rsp_var_out)))
# 		print(df %>%
# 		      mycreate_sqlxtab_df(c(rsp_var, rsp_var_out)) %>%
# 		      tidyr::spread_(rsp_var_out, ".n"))
		stats_df[ , paste0("max.f.score.", label)] <-
			mycompute_classifier_f.score(mdl, df,
										 prob_threshold,
										 rsp_var, rsp_var_out)
	} else df[, rsp_var_out] <- predict(mdl, newdata = df, type = "raw")


    if (mdl$modelType == "Classification") {
    	conf_lst <- confusionMatrix(df[, rsp_var_out], df[, rsp_var])
    	print(t(conf_lst$table))
    	print(conf_lst$overall)

    	for (stat in c("Accuracy", "AccuracyLower", "AccuracyUpper", "Kappa")) {
    		stats_df[, paste0("max.", stat, ".", label)] <- conf_lst$overall[stat]
    	}

    	if (!is.null(model_summaryFunction)) {
    		df$obs <- df[, rsp_var]; df$pred <- df[, rsp_var_out]
            results <- model_summaryFunction(df, lev = levels(df$obs))
            # Add unk.* for metrics that are returned from summaryFunction but not specified as key metric ?
        	stats_df[, paste0(ifelse(model_metric_maximize, "max.", "min."),
        							model_metric, ".", label)] <- results[model_metric]
    	}
    } else
    if (mdl$modelType == "Regression") {
        SSE <- sum((df[, rsp_var_out] - df[, rsp_var]) ^ 2)
        #stats_df[, paste0("max.R.sq.", label)] <- 1 - (SSE / sum((df[, rsp_var] - mean(mdl$finalModel$fitted.values)) ^ 2))
        stats_df[, paste0("max.R.sq.", label)] <- 1 -
            (SSE / sum((df[, rsp_var] - mean(predict(mdl, mdl$trainingData, "raw"))) ^ 2))
        stats_df[, paste0("min.RMSE.", label)] <-
            (sum((df[, rsp_var_out] - df[, rsp_var]) ^ 2) / (nrow(df) - 0)) ^ 0.5

        if (label == "fit") {
            if (inherits(mdl$finalModel, c("glmnet", "ksvm", "list",
                                           "nnet", "avNNet",
                                           "rpart", "randomForest"))) {
                # list implies custom model
                # summary(glmnet)$adj.r.squared causes an error
                # summary(randomForest)$r.squared causes an error
                # summary(rpart) always spits out stuff
            } else {
                #models_df$max.R.sq.fit <- summary(mdl$finalModel)$r.squared
                stats_df$max.Adj.R.sq.fit <- summary(mdl$finalModel)$adj.r.squared
                #models_df$min.SSE.fit <- sum(mdl$finalModel$residuals ^ 2)
                stats_df$min.aic.fit <- mdl$finalModel$aic
            }
        }


        if (!is.null(stats_df[, paste0("max.R.sq.", label)]) &&
            (length(grep(paste0("max.Adj.R.sq.", label), names(stats_df), fixed = TRUE, value = TRUE)) == 0))
            stats_df[, paste0("max.Adj.R.sq.", label)] <- 1.0 -
               ((1.0 - stats_df[, paste0("max.R.sq.", label)]) *
                (nrow(df) - 1) /
                (nrow(df) - nrow(myget_feats_importance(mdl)) - 1))
    }

	if (ret_type == "stats") return(stats_df) else
	if (ret_type == "raw") return(df[, rsp_var_out])
}

myinit_mdl_specs_lst <- function(mdl_specs_lst=list()) {
    require(caret)
#     if (packageVersion("caret") != "6.0.57")
#         stop("unexpected caret version: ", packageVersion("caret"), "\n check defaults in caret package")
    # check oob method in trainControl for different algorithms in myfit_mdl

    # Refactor to separate mdl_specs_lst into inpSpecs vs. retSpecs
    inpSpecs <- mdl_specs_lst
    for (spec in c("id.prefix", "type", "tune.df",
                   # train params that feed defaults for trainControl params
                   "train.preProcess",
                   # trainControl params
        "trainControl.method", "trainControl.number", "trainControl.repeats",
        "trainControl.classProbs", "trainControl.summaryFunction",
        #"trainControl.allowParallel",
                   # train params
                   "train.method", "train.metric", "train.maximize")) {
    #     function specs ()

        if (!any(grepl(spec, names(mdl_specs_lst), fixed = TRUE))) {
            # mdl_specs_lst[[spec]] <- NULL # this does not work for preProc.method
            mdl_specs_lst <- append(mdl_specs_lst, list(last.item = NULL))
            names(mdl_specs_lst) <-
                gsub("last.item", spec, names(mdl_specs_lst), fixed = TRUE)
        }

        if ((spec == "trainControl.number") && is.null(mdl_specs_lst[[spec]]))
            mdl_specs_lst[[spec]] <-
                ifelse(!is.null(mdl_specs_lst[["train.preProcess"]]) &&
                           grepl("cv", mdl_specs_lst[["train.preProcess"]]), 10, 25)
        if ((spec == "trainControl.repeats") && is.null(mdl_specs_lst[[spec]]))
            mdl_specs_lst[[spec]] <-
                ifelse(!is.null(mdl_specs_lst[["train.preProcess"]]) &&
                           grepl("cv", mdl_specs_lst[["train.preProcess"]]),
                                            1, mdl_specs_lst[["trainControl.number"]])
        if ((spec == "trainControl.classProbs") && is.null(mdl_specs_lst[[spec]]))
            mdl_specs_lst[[spec]] <- FALSE
        if ((spec == "trainControl.summaryFunction") &&
            is.null(mdl_specs_lst[[spec]]))
            mdl_specs_lst[[spec]] <- defaultSummary
#         if ((spec == "trainControl.allowParallel") && is.null(mdl_specs_lst[[spec]]))
#             mdl_specs_lst[[spec]] <- TRUE

        if ((spec == "train.metric") && is.null(mdl_specs_lst[[spec]]))
            mdl_specs_lst[[spec]] <- ifelse(mdl_specs_lst[["type"]] == "classification",
                                            "Accuracy", "RMSE")
        if ((spec == "train.maximize") && is.null(mdl_specs_lst[[spec]]))
            mdl_specs_lst[[spec]] <-
                ifelse(mdl_specs_lst[["train.metric"]]  %in% c("RMSE", "logLoss"),
                       FALSE, TRUE)
    }

    retSpecs <- mdl_specs_lst

    # change id.prefix to mdlFamily
    # Build id to access specs by id
    retSpecs[["id"]] <- paste(retSpecs[["id.prefix"]],
                                   retSpecs[["train.preProcess"]],
                                   plyr::revalue(retSpecs[["trainControl.method"]], c(
                                       "none" = "",
                                       "repeatedcv" = "rcv"),
                                       warn_missing = FALSE),
                                   retSpecs[["train.method"]],
                                   sep = "#")

    if ((!is.null(allowParallelSpecs <- inpSpecs[["trainControl.allowParallel"]])) &&
        (!is.null(spec <- allowParallelSpecs[[unlist(retSpecs["id"])]])))
        retSpecs[["trainControl.allowParallel"]] <- as.logical(spec) else
        retSpecs[["trainControl.allowParallel"]] <- TRUE

    return(retSpecs)
}

mygen_seeds <- function(seeds_lst_len, seeds_elmnt_lst_len) {
    seeds <- vector(mode = "list", length = seeds_lst_len)
    start_multiplier <- tail(myget_primes(seeds_lst_len + 2), 1)
    for (i in 1:(seeds_lst_len - 1)) {
        #print(sprintf("setting seeds for i: %d", i))
        seeds[[i]] <- tail(seq( 1001 + start_multiplier * ((i + 1) ^ 3),
                                1001 + start_multiplier * ((i + 1) ^ 4),
                                by = start_multiplier), seeds_elmnt_lst_len)
    }
    ## For the last element:
    seeds[[length(seeds)]] <- 997
    return(seeds)
}
#mygen_seeds(seeds_lst_len=(glb_rcv_n_folds * glb_rcv_n_repeats) + 1, seeds_elmnt_lst_len=9)

myfit_mdl <- function(mdl_specs_lst, indepVar, rsp_var, fit_df, OOB_df=NULL) {
    #spec_indepVar <- indepVar

    startTm <- proc.time()["elapsed"]
    print(sprintf("myfit_mdl: enter: %f secs", proc.time()["elapsed"] - startTm))
    print(sprintf("fitting model: %s", mdl_specs_lst[["id"]]))

    if (!(mdl_specs_lst[["type"]] %in% c("regression", "classification")))
        stop("unsupported model type: ", mdl_specs_lst[["type"]])
    if (mdl_specs_lst[["type"]] == "classification")
	    mdl_specs_lst[["is.binomial"]] <- (length(unique(fit_df[, rsp_var])) == 2)

	models_df <- data.frame(id=mdl_specs_lst[["id"]])

	if ((length(indepVar) == 1) && any(indepVar %in% "."))
    	indepVar <- setdiff(names(fit_df), rsp_var)

	print(sprintf("    indepVar: %s", paste(indepVar, collapse=",")))
    models_df$feats	<- paste(indepVar, collapse=",")

    rsp_var_out <- paste0(rsp_var, ".predict.", mdl_specs_lst[["id"]])

    mdl_specs_lst <- append(mdl_specs_lst, list(train.tuneGrid=NULL))
    # tune_params_df is needed later irrespective of mdl_specs_lst[["train.tuneGrid"]]
    if (nrow(subset(modelLookup(), model == mdl_specs_lst[["train.method"]])) > 0)
        mdl_specs_lst[["tune.params.df"]] <-
            getModelInfo(mdl_specs_lst[["train.method"]])[[mdl_specs_lst[["train.method"]]]]$parameters
    if (!is.null(mdl_specs_lst[["tune.df"]]) &&
        (nrow(lcl_tune_models_df <- mdl_specs_lst[["tune.df"]]) > 0) &&
        (nrow(lcl_tune_models_df <-
              subset(lcl_tune_models_df, mdlId == mdl_specs_lst[["id"]])) > 0)) {
        if (length(tune_params_vctr <- lcl_tune_models_df$parameter) > 0) {
			args_lst <- list()
			for (param_ix in 1:length(tune_params_vctr)) {
			    vals <- subset(lcl_tune_models_df,
			                    (parameter == tune_params_vctr[param_ix]))$vals
			    if (!is.null(vals) && !is.na(vals))
			        args_lst[[tune_params_vctr[param_ix]]] <-
			            # strsplit does not work when there are multiple blanks
			            #   consider using stringr::str_split
			            as.numeric(unlist(strsplit(vals, "[ ,]"))) else
				    args_lst[[tune_params_vctr[param_ix]]] <- seq(
#   lcl_tune_models_df[ lcl_tune_models_df$parameter==tune_params_vctr[param_ix], "min"],
# 	lcl_tune_models_df[ lcl_tune_models_df$parameter==tune_params_vctr[param_ix], "max"],
# 	lcl_tune_models_df[ lcl_tune_models_df$parameter==tune_params_vctr[param_ix], "by"])
	               subset(lcl_tune_models_df, parameter == tune_params_vctr[param_ix])$min,
	               subset(lcl_tune_models_df, parameter == tune_params_vctr[param_ix])$max,
	               subset(lcl_tune_models_df, parameter == tune_params_vctr[param_ix])$by)
			}

			mdl_specs_lst[["train.tuneGrid"]] <- do.call("expand.grid", args_lst)
		}
    } else         # For some odd reason caret does not set this up

        # bagEarth should use OOB, repeatedcv mdl$results contains only one row
            if (mdl_specs_lst[["train.method"]] == "bagEarth") {
        mdl_specs_lst[["train.tuneGrid"]] <- expand.grid(
            nprune=tail(2^(1:as.integer(log2(length(setdiff(names(
                myget_vectorized_obs_df(fit_df, rsp_var, indepVar)), rsp_var))))), 5), # same as RFE
            # degree == Maximum degree of interaction (Friedman’s mi). Default is 1, meaning build an additive model (i.e., no interaction terms).
            degree=1)
#     } else  if (mdl_specs_lst[["train.method"]] == "svmLinear") {
#         mdl_specs_lst[["train.tuneGrid"]] <- expand.grid(C=10^(-2:2))
    }
#     print(mdl_specs_lst[["train.tuneGrid"]])

	if (mdl_specs_lst[["train.method"]] == "myrandom_classfr") mdl_specs_lst[["train.method"]] <-
        mycreate_random_classfr() else
	if (mdl_specs_lst[["train.method"]] == "mybaseln_classfr") mdl_specs_lst[["train.method"]] <-
        mycreate_baseln_classfr() else
	if (mdl_specs_lst[["train.method"]] == "myMFO_classfr") 	mdl_specs_lst[["train.method"]] <-
        mycreate_MFO_classfr()

    if (is.null(mdl_specs_lst[["trainControl.method"]]))
        mdl_specs_lst[["trainControl.method"]] <-
            ifelse(!is.null(mdl_specs_lst[["trainControl.number"]]) &&
                       (mdl_specs_lst[["trainControl.number"]] > 0), "repeatedcv", "none")

    if (!inherits(mdl_specs_lst[["train.method"]], "list") &&
        (mdl_specs_lst[["trainControl.method"]] %in% c("cv", "repeatedcv"))) {
        # cv is not useful for these algorithms
        for (alg in c("bag", "bagEarth", "rf")) {
            #grep(alg, names(getModelInfo()), ignore.case=TRUE, value=TRUE)
            if (grepl(alg, mdl_specs_lst[["train.method"]], ignore.case=TRUE))
                mdl_specs_lst[["trainControl.method"]] <-
                	if (is.null(OOB_df)) "none" else "oob"
        }
    }

    mdl_specs_lst[["train.tuneLength"]] <-
        ifelse(mdl_specs_lst[["trainControl.method"]] == "none", 1,
                        ifelse(!is.null(mdl_specs_lst[["train.tuneGrid"]]),
                               nrow(mdl_specs_lst[["train.tuneGrid"]]), 5))

    if (mdl_specs_lst[["trainControl.method"]] != "none") {
        seeds_lst_len <- mdl_specs_lst[["trainControl.number"]] *
                         mdl_specs_lst[["trainControl.repeats"]] + 1
        seeds <- vector(mode = "list", length = seeds_lst_len)
        start_multiplier <- tail(myget_primes(seeds_lst_len + 2), 1)
        nMdlsPerCvSample <- switch(mdl_specs_lst[["train.method"]],

                                   nnet    = mdl_specs_lst[["train.tuneLength"]] ^ 2,
                                   avNNet  = mdl_specs_lst[["train.tuneLength"]] ^ 2,

                                   svmPoly = mdl_specs_lst[["train.tuneLength"]] ^ 2 * 3,

                                   dummy   = mdl_specs_lst[["train.tuneLength"]])
        if (is.null(nMdlsPerCvSample))
            nMdlsPerCvSample <- mdl_specs_lst[["train.tuneLength"]]

        for (i in 1:(seeds_lst_len - 1)) {
            #print(sprintf("setting seeds for i: %d", i))
            endExp <- 3;
            while (is.null(seeds[[i]]) || (length(seeds[[i]]) < nMdlsPerCvSample)) {
                seeds[[i]] <- tail(seq( 1001 + start_multiplier * ((i + 1) ^ 2),
                                        1001 + start_multiplier * ((i + 1) ^ endExp),
                                        by = start_multiplier), nMdlsPerCvSample)
                endExp <- endExp + 1;
            }
        }
        ## For the last model:
        seeds[[length(seeds)]] <- 997
    } else seeds <- NA
    #seeds <- NA

    #grep("trainControl", names(mdl_specs_lst), value=TRUE)

#     if (packageVersion("caret") != "6.0.58")
#         stop("Review caret kludges")

	mdl_specs_lst[["trainControl"]] <- trainControl(
	    method = mdl_specs_lst[["trainControl.method"]],
	    number = mdl_specs_lst[["trainControl.number"]],
	    repeats = mdl_specs_lst[["trainControl.repeats"]],
	    verboseIter = TRUE, returnResamp = "all", savePredictions = TRUE,
        summaryFunction = mdl_specs_lst[["trainControl.summaryFunction"]],
	    seeds = seeds,
	    allowParallel = mdl_specs_lst[["trainControl.allowParallel"]])

    # Need to figure out how to handle glb_featsimp_df; has PC1:PCn as feature names
    #     if (!inherits(mdl_specs_lst[["train.method"]], "list") && (mdl_specs_lst[["train.method"]] == "rf")) {
    #     	 print("performing pca pre-processing for rf")
    #     	 preProc_mthd <- c("pca")
    #     }
    if (!is.null(mdl_specs_lst[["train.preProcess"]])) {
        if (mdl_specs_lst[["train.preProcess"]] == "NULL")
            mdl_specs_lst[["train.preProcess"]] <- NULL else
        if (grepl(".", mdl_specs_lst[["train.preProcess"]], fixed=TRUE))
            mdl_specs_lst[["train.preProcess"]] <-
                unlist(strsplit(mdl_specs_lst[["train.preProcess"]], "[.]"))
    }

	# varImp crashes for bayesglm if char/string features are present in fit_df
	fit_df <- fit_df[, c(rsp_var, sort(unique(unlist(strsplit(indepVar, "[:\\*]")))))]

	# preProcess method %in% c("pca", "range") train crashes if a column has no variance (might result from a dummy var)
	if (!is.null(mdl_specs_lst[["train.preProcess"]]) &&
	    grepl("(ica|pca|range)", mdl_specs_lst[["train.preProcess"]])) {
    	vctr_fit_df <- myget_vectorized_obs_df(fit_df, rsp_var = rsp_var, indepVar = indepVar)
    	unqlen_cols <- sapply(names(vctr_fit_df), function(col)
    	                                            length(unique(vctr_fit_df[, col])))
    	if (length(problem_cols <- unqlen_cols[unqlen_cols <= 1]) > 0) {
    	    warning(paste0("myfit_mdl: preProcess method: range currently does not work for columns with no variance: ", paste0(names(problem_cols), collapse=", ")), immediate.=TRUE)
    	    return(NULL)
#     	    indepVar <- setdiff(names(vctr_fit_df), c(rsp_var, names(problem_cols)))
#     	    fit_df <- vctr_fit_df[, c(rsp_var, indepVar)]
    	}
	}

	# Check if mdl_specs_lst[["train.method"]] is not a list (non-custom models only)
	#   else it is a string
# 	if (mdl_specs_lst[["train.method"]] %in% c("bagEarth", "svmLinear")) {
# 	    print(sprintf("User-specified Tuning Grid Length: %d",
# 	                  mdl_specs_lst[["train.tuneLength"]]))
# 	    print(mdl_specs_lst[["train.tuneGrid"]])
# 	}

	print(sprintf("myfit_mdl: setup complete: %f secs", proc.time()["elapsed"] - startTm))
	set.seed(111)
	mdl <- train(reformulate(sort(indepVar), response=rsp_var), data=fit_df
	#mdl <- train(reformulate(".", response=rsp_var), data=fit_df # does not handle interaction var specs
				 , method=mdl_specs_lst[["train.method"]]
				 , preProcess=mdl_specs_lst[["train.preProcess"]]
				 , metric=mdl_specs_lst[["train.metric"]]
				 , maximize=mdl_specs_lst[["train.maximize"]]
				 , trControl=mdl_specs_lst[["trainControl"]]
				 , tuneGrid=mdl_specs_lst[["train.tuneGrid"]]
				 , tuneLength=mdl_specs_lst[["train.tuneLength"]])

	print(sprintf("myfit_mdl: train complete: %f secs", proc.time()["elapsed"] - startTm))

	# Make this assignment earlier than at the end, to facilitate debugging of the model
	mdl$.myId <- mdl_specs_lst[["id"]]
	models_lst <- glb_models_lst; models_lst[[mdl_specs_lst[["id"]]]] <- mdl;
	glb_models_lst <<- models_lst

	#print(mdl$bestTune)
	if ((nrow(mdl$results) > 1) & (mdl$bestTune[1, 1] != "none")) {
		# print(ggplot(mdl) + geom_vline(xintercept=mdl$bestTune[1, 1], linetype="dotted"))
	    print(ggplot(mdl, highBestTune = TRUE))
		for (param in (params_vctr <- as.character(mdl_specs_lst[["tune.params.df"]][, "parameter"]))) {
			if ((mdl$bestTune[1, param] == min(mdl$results[, param])) |
				(mdl$bestTune[1, param] == max(mdl$results[, param])))
				warning("model's bestTune found at an extreme of tuneGrid for parameter: ", param)
		}
	}
	myprint_mdl(mdl)

    #models_df$n.fit <- nrow(fit_df)
	models_df$max.nTuningRuns <- nrow(mdl$results)
    models_df$min.elapsedtime.everything <- mdl$times$everything["elapsed"]
    models_df$min.elapsedtime.final      <- mdl$times$final["elapsed"]

    print(sprintf("myfit_mdl: train diagnostics complete: %f secs", proc.time()["elapsed"] - startTm))

	# compute/gather fit & OOB prediction stats
    for (obs in c("fit", "OOB")) {
        # print(sprintf("    calling mypredict_mdl for %s:", obs))
        if (obs == "fit") obs_df <- fit_df else obs_df <- OOB_df
        if (!is.null(obs_df))
    		models_df <- merge(models_df,
    		    mypredict_mdl(mdl, df = obs_df, rsp_var, label = obs,
                            mdl_specs_lst[["trainControl.summaryFunction"]],
                            mdl_specs_lst[["train.metric"]],
                            mdl_specs_lst[["train.maximize"]],
                            ret_type = "stats"),
    		                   all.x = TRUE)
    }

    print(sprintf("myfit_mdl: predict complete: %f secs", proc.time()["elapsed"] - startTm))

	if (nrow(mdl$results) > 0) {
		myresults_df <- mdl$results
		for (param in names(mdl$bestTune)) {
			myresults_df <- myresults_df[myresults_df[, param] == mdl$bestTune[1, param], ]
		}
		if (nrow(myresults_df) != 1)
		    stop ("this should not happen")
		for (result in setdiff(names(myresults_df), names(mdl$bestTune))) {
		    metric_pfx <- ifelse(length(grep("Rsquared", result) > 0), "max.",
		                         ifelse(mdl_specs_lst[["train.maximize"]], "max.", "min."))
		    models_df[1, paste0(metric_pfx, result, ".fit")] <- myresults_df[1, result]
		}
	}

	print(models_df)
	if (nrow(glb_models_df) > 0)
        all_models_df <- myrbind_df(subset(glb_models_df, id != models_df$id),
                                    models_df) else
        all_models_df <- models_df
    row.names(all_models_df) <- all_models_df$id
	glb_models_df <<- all_models_df

	print(sprintf("myfit_mdl: exit: %f secs", proc.time()["elapsed"] - startTm))
    return(list("model" = mdl, "models_df" = models_df))
}

## 08.1	    fit on simple shuffled sample
## 08.2     fit on stratified shuffled sample
## 08.3     fit on cross-validated samples
## 08.4		fit on all

## 09.	    test model results  for k-fold0 test set
# mpg_residuals_df <- data.frame(mpg_fit$residuals, cars_df$mpg, cars_df$am_fctr)
# #print(summaryBy(mpg_fit.residuals ~ cars_df.am_fctr, data=mpg_residuals_df, FUN=mean))
# print(myplot_violin(mpg_residuals_df, "mpg_fit.residuals", "cars_df.am_fctr"))

## 09.1	    collect votes from each cross-validation for each model
## 09.2	    collect votes from each model
## 09.3     export cv test data for inspection

## 10.	    build finalized model on all training data
myextract_mdl_feats <- function(sel_mdl, entity_df) {

			if ("coefficients" %in% names( sel_mdl)) {
		# mdl is lm OR glm.binomial
		stop("not implemented yet")
		plot_vars_df <- as.data.frame(summary( sel_mdl)$coefficients)
		names(plot_vars_df)[length(names(plot_vars_df))] <- "Pr.z"
		# Get rid of (Intercept)
		plot_vars_df <- orderBy(~Pr.z, plot_vars_df[2:nrow(plot_vars_df),])
		#print(plot_vars_df <- subset(plot_vars_df, Pr.z < 0.1))
    } else if ("variable.importance" %in% names( sel_mdl)) {
    	# mdl is rpart.class
		plot_vars_df <- as.data.frame( sel_mdl$variable.importance)
		names(plot_vars_df)[length(names(plot_vars_df))] <- "importance"
		plot_vars_df <- orderBy(~ -importance, plot_vars_df)
		#print(plot_vars_df)
    } else if ("importance" %in% names( sel_mdl)) {
    	# mdl is randomForest
		plot_vars_df <- as.data.frame( sel_mdl$importance)
		names(plot_vars_df)[length(names(plot_vars_df))] <- "importance"
		plot_vars_df <- orderBy(~ -importance, plot_vars_df)
		#print(plot_vars_df)
    } else if (any(class( sel_mdl) %in% "train")) {
    	# mdl is caret::train
        require(caret)

        if ((inherits(sel_mdl$finalModel, "randomForest")) &&
            (sel_mdl$modelType == "Regression")) {
            # varImp for randomForest regression crashes in caret version:6.0.41
            #   randomForest::importance provided IncNodePurity but
            #       caret is looking for %IncMSE
            plot_vars_df <- as.data.frame(importance(sel_mdl$finalModel))
            plot_vars_df[, paste0(".scld.", names(plot_vars_df)[1])] <- plot_vars_df[, 1] * 100.0 / max(plot_vars_df[, 1])
            plot_vars_df <- plot_vars_df[, paste0(".scld.", names(plot_vars_df)[1]), FALSE]
            names(plot_vars_df) <- "Overall"
        } else plot_vars_df <- varImp(sel_mdl)$importance
		names(plot_vars_df)[length(names(plot_vars_df))] <- "importance"
		plot_vars_df <- orderBy(~ -importance, plot_vars_df)
		#print(plot_vars_df)
    } else stop("not implemented yet")

    plot_vars_df$id <- rownames(plot_vars_df)
    # Enhancement:
        # Manage interaction terms: make.names() replaces ":" with "."
        # 1. Term could be surrounded by "`"
        # 2. Term could have ":" but be a factor value
        # 3. Term contains interactions with one or more terms, separated by ":"
    plot_vars_df$fit.feat <- (plot_vars_df$id %in% names(entity_df))

    if (nrow(dummy_vars_df <- subset(plot_vars_df, !fit.feat)) > 0) {
		dummy_vars_df$root1.feat <- sapply(1:nrow(dummy_vars_df), function(row_ix)
			paste0(unlist(strsplit(dummy_vars_df[row_ix, "id"], ".fctr", fixed=TRUE))[1],
					".fctr"))
        # caret might be adding a prefix of "`" to dummy vars
        dummy_vars_df$root.feat <- sapply(1:nrow(dummy_vars_df), function(row_ix)
            ifelse((chrs <- unlist(strsplit(dummy_vars_df[row_ix, "root1.feat"], "")))[1] == "`",
                   paste0(tail(chrs, -1), collapse=""), dummy_vars_df[row_ix, "root1.feat"]))
		#print(dummy_vars_df)
		dummy_vars_df <- mutate(dummy_vars_df,
								vld.fit.feat=(root.feat %in% names( entity_df))
								)
		#print(dummy_vars_df)
		if (nrow(subset(dummy_vars_df, !vld.fit.feat)) > 0)
			stop("Dummy variables not recognized")
        dummy_imp_vctr <- tapply(dummy_vars_df$importance, dummy_vars_df$root.feat, max,
                                 na.rm=TRUE)
        vld_plot_vars_df <- merge(subset(plot_vars_df, fit.feat)[, c("id", "importance")],
                                  data.frame(id=names(dummy_imp_vctr),
                                             importance=dummy_imp_vctr),
                                  by="id", all=TRUE)
        row.names(vld_plot_vars_df) <- vld_plot_vars_df$id
        if (length(grep("importance.x", names(vld_plot_vars_df), fixed=TRUE)) > 0) {
            # dups caused by blank values in factor ?
            vld_plot_vars_df$importance <- sapply(1:nrow(vld_plot_vars_df),
                function(row_ix) max(vld_plot_vars_df[row_ix, "importance.x"],
                                     vld_plot_vars_df[row_ix, "importance.y"],
                                    na.rm=TRUE))
            vld_plot_vars_df <- vld_plot_vars_df[, c("id", "importance")]
        }
# 		vld_plot_vars_df <- rbind(subset(plot_vars_df, fit.feat)[, c("id", "importance")],
# 									data.frame(id=names(dummy_imp_vctr),
# 			                                   importance=dummy_imp_vctr))
    } else vld_plot_vars_df <- plot_vars_df

    #print(orderBy(~ -importance, vld_plot_vars_df))
    return(orderBy(~ -importance, vld_plot_vars_df))
}

mymerge_feats_importance <- function(feats_df, sel_mdl, entity_df) {
    plot_vars_df <- myextract_mdl_feats(sel_mdl, entity_df)
    mrg_feats_df <- orderBy(~ -importance,
        merge(feats_df, plot_vars_df[,c("id", "importance")], all.x=TRUE))
    row.names(mrg_feats_df) <- mrg_feats_df$id
    return(mrg_feats_df)
}

require(caret)

myget_primes <- function(n) {
    p <- 2:n
    i <- 1
    while (p[i] <= sqrt(n)) {
        p <-  p[p %% p[i] != 0 | p==p[i]]
        i <- i+1
    }
    return(p)
}
#myget_primes(11)

myget_feats_importance <- function(mdl, featsimp_df = NULL) {
    # For some models, if there is only one feature, varImp returns NaN due to bug in scaling
    #   length(attr(mdl$terms, "variables")) == 2 ???

    if ((inherits(mdl$finalModel, "rpart")) &&
        is.null(mdl$splits)) {
        # varImp crashes for an empty tree
        return(NULL)
    }

    if (length(names(varImp(mdl))) > 1) {
        # multinomial classification model
        thisimp_df <- varImp(mdl)$importance
    } else # Regression or Binomial Classification
    if ((inherits(mdl$finalModel, "randomForest")) &&
            (mdl$modelType == "Regression")) {
        # varImp for randomForest regression crashes in caret version:6.0.41
        #   randomForest::importance provided IncNodePurity but
        #       caret is looking for %IncMSE
        thisimp_df <- as.data.frame(importance(mdl$finalModel))
        thisimp_df[, paste0(".scld.", names(thisimp_df)[1])] <-
            thisimp_df[, 1] * 100.0 / max(thisimp_df[, 1])
        thisimp_df <- thisimp_df[, paste0(".scld.", names(thisimp_df)[1]), FALSE]
        names(thisimp_df) <- "Overall"
    } else thisimp_df <- varImp(mdl)$importance

    if (length(names(thisimp_df)) > 1) {
        # Multinomial
        names(thisimp_df) <-
            paste0(gsub("#",".",mdl$.myId, fixed = TRUE), ".imp.", names(thisimp_df))

        # Used for sorting; already sorted in descending order by importance across classes
        thisimp_df$imp <- 0 - (1:nrow(thisimp_df))
    } else {
        thisimp_df$imp <- thisimp_df[, 1] # Used for sorting
        names(thisimp_df)[1] <- paste0(gsub("#",".",mdl$.myId, fixed = TRUE), ".imp")
    }

    if (is.null(featsimp_df) ||
        (length(setdiff(names(subset(featsimp_df, select = -imp)), names(thisimp_df))) == 0))
        featsimp_df <- thisimp_df else {
        featsimp_df <- merge(subset(featsimp_df, select = -imp), thisimp_df,
                             by = "row.names", all = TRUE)
        row.names(featsimp_df) <- featsimp_df$Row.names
        featsimp_df <- subset(featsimp_df, select = -Row.names)
    }

#     if (!is.null(mdl$.myId))
#         featsimp_df[, paste0(mdl$.myId, ".imp")] <- featsimp_df$imp
    return(orderBy(~ -imp, featsimp_df))
}

## 11.	    predict results for new data
## 11.1	    fit models with data to predict
## 11.2	    collect votes from each cross-validation for each model
## 11.3	    collect votes from each model

## 12.	    export results
