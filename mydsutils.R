# Balaji Iyengar's Data Science Process
# Created 2014-08-12
# Check if functions are assigned to proper data science process steps

#suppressPackageStartupMessages(require(<<package name>>))

## 01. 		import data

myimport_data <- function(url, filename=NULL, nrows=-1, comment=NULL,
							force_header=FALSE, print_diagn=TRUE, ...){
    if (!file.exists("./data")) dir.create("data")

    url_split <- strsplit(url, "/", fixed=TRUE)[[1]]
    download_filename <- gsub("%2F", "-", url_split[length(url_split)], fixed=TRUE)
    download_filepath <- paste("./data", download_filename, sep="/")
    if (!file.exists(download_filepath)) {
    	print(sprintf("Downloading file %s from %s...", download_filepath, url))
        download.file(url, destfile=download_filepath, method="curl")
    }

    url_split <- strsplit(url, ".", fixed=TRUE)[[1]]
    download_filename_ext <- url_split[length(url_split)]
    if (download_filename_ext == "zip") {
        if (is.null(filename)) {
#           stop("Please specify which file(filename=) should be imported from ",
#                  download_filepath)
			filename <- substr(download_filename, 1, nchar(download_filename) - nchar(".zip"))
        }

        file_path <- paste("./data", filename, sep="/")
        if (!file.exists(file_path)) {
    		print(sprintf("Unzipping file %s...", file_path))
            unzip(download_filepath, filename)
        }
    } else file_path <- download_filepath

    # read.csv reads files with ext %in% c(".csv", ".csv.bz2)
    #	check if file contains header
    first_record <- read.csv(file_path, header=FALSE, quote="", nrows=1)

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

    print(sprintf("Reading file %s...", file_path))
    df <- read.csv(file_path, header=header, nrows=nrows, ...)

    if (nrows > 0)
    	warning("first ", nrows, " records read")

    print(sprintf("dimensions of data in %s: %s rows x %s cols", file_path,
                  format(dim(df)[1], big.mark=","),
                  format(dim(df)[2], big.mark=",")))

	if (!(missing(comment)))
		comment(df) <- comment

    if (print_diagn) {
		myprint_df(df)
		myprint_str_df(df)
    }

    return(df)
}
#activity_df <- myimport_data("activity.csv",
#                "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip")

myprint_df <- function(df, dims=FALSE) {
    if (dims)
        print(sprintf("Rows: %s; Cols: %s",
                      format(dim(df)[1], big.mark=","),
                      format(dim(df)[2], big.mark=",")))

    if (dim(df)[1] <= 20) print(df) else {
    	print(head(df))

		# print 6 sample obs
		print(df[sort(sample(1:dim(df)[1], 6)),])

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

#subset(entity_df, select=-c(interval))

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
#entity_agg_date_df <- myaggregate_numorlgcl(subset(entity_df,
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
        return(factor(levels(vector)[median(as.numeric(vector), na.rm=TRUE)], levels(vector)))
#     if (class(vector) == "Date")
#     	return(as.Date(median(vector, na.rm=TRUE), origin="1970-01-01"))

    return(median(vector, na.rm=TRUE))
}

mycompute_medians_df <- function(df, byvars_lst=factor(0), keep.names=FALSE) {
	if (class(df) != "data.frame")
		stop("df argument is not a data.frame: it is ", class(df))

	ret_df <- data.frame()

	# gather numeric vars
    num_lst <- sapply(names(df), function(col) if (is.numeric(df[, col])) return(col))
    num_vctr <- unlist(num_lst[!sapply(num_lst, is.null)])
    if (length(num_vctr) > 0)
	    ret_df <- summaryBy(as.formula(paste0(num_vctr, " ~ ", byvars_lst)), data=df,
    							FUN=median, keep.names=keep.names)

    # summaryBy does not compute stats for factor or Date class variables
    non_num_lst <- sapply(names(df), function(col) if (!is.numeric(df[, col])) return(col))
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

mycreate_tbl_df <- function(df, tbl_col_name) {

	ret_df <- as.data.frame(sort(table(df[, tbl_col_name])))
	names(ret_df) <- ".freq"
	ret_df[, tbl_col_name] <- rownames(ret_df)
	rownames(ret_df) <- 1:nrow(ret_df)

	return(ret_df[, c(tbl_col_name, ".freq")])
}

mycreate_xtab <- function(df, xtab_col_names) {
    require(doBy)
    require(reshape2)

    df[, "_n"] <- 1
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

#     print(xtabs(~ Month_fctr + Arrest, entity_df))
#
# 	print(prblm_3_2_xtb <- xtabs(~ Month + Arrest, entity_df))
# 	print(prblm_3_2_df <- data.frame(dimnames(prblm_3_2_xtb)[[1]],
#                                  prblm_3_2_xtb[, 1],
#                                  prblm_3_2_xtb[, 2]))
# 	names(prblm_3_2_df) <- c(names(dimnames(prblm_3_3_xtb))[1],
#     paste(names(dimnames(prblm_3_3_xtb))[2], dimnames(prblm_3_2_xtb)[[2]][1], sep="."),
#     paste(names(dimnames(prblm_3_3_xtb))[2], dimnames(prblm_3_2_xtb)[[2]][2], sep="."))
# 	print(prblm_3_2_df)
#
# 	print(prblm_3_3_tbl <- table(entity_df$Year, entity_df$Arrest))
# 	print(prblm_3_3_df <- data.frame(Year=dimnames(prblm_3_3_tbl)[[1]],
#                                  Arrest_FALSE=prblm_3_3_tbl[, 1],
#                                  Arrest_TRUE =prblm_3_3_tbl[, 2]))

}

mydelete_cols_df <- function(df, colnames) {
    return(subset(df, select=names(df)[!names(df) %in% colnames]))
}

myfind_all_na_cols_df <- function(df) {
    na_cols <- which(as.numeric(colSums(is.na(df))) == nrow(df))
    return(names(df)[na_cols])
}

myformat_number <- function(x) {
    if (class(x) != "num") x <- as.numeric(x)
    return(format(x, big.mark=',')) # 000's separator
    #format(x, digits=4, scientific=FALSE)	# other format options
}

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
#require(plyr)
#intersect(names(entity_df), names(entity_agg_intrvl_df))
#entimptd_df <- join(entity_df, entity_agg_intrvl_df, by="interval")
#entimptd_df <- mutate(entimptd_df, steps_imputed=ifelse(is.na(steps), steps_mean,
#                                                        steps))

## 02.3	    encode/retype data (convert types; map codes)
mycheck_map_results <- function(mapd_df, from_col_name, to_col_name) {
    if (length(unique(mapd_df[, from_col_name])) == nrow(mapd_df)) map_summry_df <- mapd_df else {
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

	myprint_df(map_summry_df)

	# Works only for 1:1 mapping;
	#	Use fill aesthetic to display n:m mappings ?
	#		Create a variable that contains n:m ratio for each value of to_col_name ?

	print(ggplot(map_summry_df, aes_string(x=to_col_name, y=".n",
                                       fill=paste0("factor(", from_col_name, ")"))) +
                 geom_bar(stat="identity") + coord_flip())
# 	print(myplot_hbar(map_summry_df[1:min(nrow(map_summry_df), 10),], to_col_name, "_n"))
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
    count_df <- mycreate_xtab(new_df, c(date_col_name, "date.dytyp"))
    myprint_df(count_df)
    return(new_df)
}
#entity_agg_date_df <- mycreate_date2daytype(entity_agg_date_df, "date")

## 03.2		filter features
#features_lst <- features_lst[features_lst != "price"]
#fctrs_lst <- fctrs_lst[!sapply(fctrs_lst, is.null)]

## 03.3     create feature combinations
## 03.4		add random variable

## 04.	    partition data
## 04.1	    simple shuffle sample
## 04.2     stratified shuffle sample
mypartition_data <- function(more_stratify_vars=NULL) {
    print(" "); print(sprintf("nrow(entity_df): %s", format(nrow(entity_df), big.mark=',')))
    if (missing(more_stratify_vars)) {
        print(table(entity_df[, response_varname]))
        keep_cols <- response_varname
    } else {
        entity_tbl <- table(entity_df[, response_varname],
                            entity_df[, more_stratify_vars])
        print(entity_tbl)
        keep_cols <- c(response_varname, more_stratify_vars)
    }

    inTrainValidate <- createDataPartition(y=entity_df[, response_varname],
                                           p=0.8, list=FALSE)
    train_validate_df <- entity_df[inTrainValidate, keep_cols]

    inTest <- setdiff(seq(1:nrow(entity_df)), inTrainValidate)
    inTest <- matrix(inTest, nrow=length(inTest),
                     dimnames=dimnames(inTrainValidate))
    test_df <- entity_df[inTest, keep_cols]

    inTrain <- createDataPartition(y=train_validate_df[, response_varname],
                                   p=0.8, list=FALSE)
    train_df <- train_validate_df[inTrain, keep_cols]

    inValidate <- setdiff(seq(1:nrow(train_validate_df)), inTrain)
    inValidate <- matrix(inValidate, nrow=length(inValidate),
                     dimnames=dimnames(inTrain))
    validate_df <- train_validate_df[inValidate, keep_cols]

    chk_nrow <- nrow(train_df) + nrow(validate_df) + nrow(test_df)
    if (nrow(entity_df) != chk_nrow)
        stop("entity_df not partitioned properly; nrow(entity_df): ",
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
# train_df <- entity_df[mypartition_data_lst$inTrain, keep_cols]
# validate_df <- entity_df[mypartition_data_lst$inValidate, keep_cols]
# test_df <- entity_df[mypartition_data_lst$inTest, keep_cols]

# all.equal(train_save_df, train_df)
# setdiff(union(names(train_save_df), names(train_df)), intersect(names(train_save_df), names(train_df)))


## 04.3	    cross-validation sample

## 05.      select features
#require(plyr)
#intersect(names(entity_df), names(entity_agg_intrvl_df))
#entimptd_df <- join(entity_df, entity_agg_intrvl_df, by="interval")
#entimptd_df <- mutate(entimptd_df, steps_imputed=ifelse(is.na(steps), steps_mean,
#                                                        steps))

## 05.1	    collect all non_na_numeric features
## 05.2	    remove row keys & prediction variable
## 05.3	    remove features that should not be part of estimation
## 05.4	    select significant features
myselect_features <- function( entity_df,  exclude_vars_as_features, rsp_var) {
	require(plyr)

	# Collect numeric vars
    vars_tbl <- summary( entity_df)
    numeric_vars <- names( entity_df)[grep("^Min.", vars_tbl[1,])]

	# Collect factor vars
	class_vctr <- sapply(names(entity_df), function(col_name) class(entity_df[, col_name]))
	factor_vars <- names(class_vctr[class_vctr == "factor"])

    # Exclude rsp_var & user-specified features
    #   keep user-specified excl. features since their cor.y is useful later
    sel_feats <- setdiff(union(numeric_vars, factor_vars),
#    						union(rsp_var, exclude_vars_as_features))
	                        rsp_var)

    feats_df <- data.frame(id=sel_feats,
                cor.y=cor(data.matrix(entity_df[, sel_feats]),
                            y=as.numeric( entity_df[, rsp_var]),
                            use="pairwise.complete.obs"))
    feats_df <- mutate(feats_df, exclude.as.feat = ifelse(id %in% exclude_vars_as_features, 1, 0))
	feats_df <- orderBy(~ -cor.y.abs, mutate(feats_df, cor.y.abs=abs(cor.y)))
    return(feats_df)
}

# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg,
#              subset(cars_df, am_fctr == "manual")$mpg,
#              var.equal=FALSE)$conf)

## 05.5	    id features / create feature combinations for highly correlated features
myfind_cor_features <- function(feats_df, entity_df, rsp_var, checkConditionalX=FALSE) {
	require(reshape2)

	feats_df[, "cor.high.X"] <- NA
    if (checkConditionalX) feats_df[, "is.ConditionalX.y"] <- NA
    cor_threshold <- feats_df[feats_df$id == ".rnorm", "cor.y.abs"]
	feats_df <- mutate(feats_df,
                       is.cor.y.abs.low=ifelse(cor.y.abs >= cor_threshold, FALSE, TRUE))
	if (nrow(feats_df) == 1)
		return(feats_df)

	chk_feats <- subset(feats_df, (exclude.as.feat == 0))$id
    if (checkConditionalX) {
        require(caret)
        empty_dstrb_feats <- sort(chk_feats[checkConditionalX(entity_df[, chk_feats],
                                                         entity_df[, rsp_var])])
        feats_df[feats_df$id %in% empty_dstrb_feats, "is.ConditionalX.y"] <- FALSE
        chk_feats <- setdiff(chk_feats, empty_dstrb_feats)
        feats_df[feats_df$id %in% chk_feats, "is.ConditionalX.y"] <- TRUE
    }
	chk_feats <- sort(subset(feats_df, (exclude.as.feat == 0) &
	                                   (!is.cor.y.abs.low) &
                                       (is.ConditionalX.y))$id)
    repeat {
    	if (length(chk_feats) == 1)
    		break

        corxx_mtrx <- cor(data.matrix(entity_df[, chk_feats]),
        						use="pairwise.complete.obs")
        abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
        #print(abs_corxx_mtrx)
        if (max(abs_corxx_mtrx, na.rm=TRUE) < 0.7) break

        row_ix <- ceiling(which.max(abs_corxx_mtrx) / ncol(abs_corxx_mtrx))
        col_ix <- which.max(abs_corxx_mtrx[row_ix, ])
        feat_1 <- rownames(abs_corxx_mtrx)[row_ix]
        feat_2 <- rownames(abs_corxx_mtrx)[col_ix]
        print(sprintf("cor(%s, %s)=%0.4f", feat_1, feat_2, corxx_mtrx[row_ix, col_ix]))
        print(myplot_scatter( entity_df, feat_1, feat_2))

        print(sprintf("cor(%s, %s)=%0.4f", rsp_var, feat_1,
             feats_df[feats_df$id == feat_1, "cor.y"]))
    #     print(myplot_scatter( entity_df, rsp_var, feat_2))
        print(sprintf("cor(%s, %s)=%0.4f", rsp_var, feat_2,
             feats_df[feats_df$id == feat_2, "cor.y"]))
    #     print(myplot_scatter( entity_df, rsp_var, feat_2))

        plot_df <- melt( entity_df, id.vars=rsp_var, measure.vars=c(feat_1, feat_2))
        print(myplot_scatter(plot_df, rsp_var, "value",
                             facet_colcol_name="variable", smooth=TRUE))

#         if ( id_var %in% c(feat_1, feat_2)) drop_feat <-  id_var else {
#   	  if (intersect( id_vars, c(feat_1, feat_2)))
# 		if (feat_1 %in%  exclude_vars_as_features) drop_feat <- feat_1 else {
# 			if (feat_2 %in% exclude_vars_as_features) drop_feat <- feat_2 else {
				drop_feat <- ifelse(
					abs( feats_df[ feats_df$id == feat_1, "cor.y"]) >=
					abs( feats_df[ feats_df$id == feat_2, "cor.y"]),
									feat_2, feat_1)
                if (drop_feat == feat_1)
                    feats_df[feats_df$id == feat_2, "cor.high.X"] <- drop_feat else
                    feats_df[feats_df$id == feat_1, "cor.high.X"] <- drop_feat
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
myprint_mdl <- function(mdl) {
	if (!inherits(mdl, "train"))
        stop("Not a legitimate \"train\" object")

    lcl_mdl <- mdl$finalModel

    if (inherits(lcl_mdl, "list")) {
		# Custom model
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

	print(as.table(confusionMatrix(obs_df[, predct_var],
								   obs_df[, actual_var])))

	#	Create a dummy matrix of 0s with actual outcomes
	#	& merge in appropriate predicted outcomes cols
	mrg_obs_xtab_df <- orderBy(reformulate(actual_var),
								mycreate_tbl_df(obs_df, actual_var)[, 1, FALSE])
	for (val in unique(mrg_obs_xtab_df[, 1]))
		mrg_obs_xtab_df[, paste(predct_var, val, sep=".")] <- 0
	#print(mrg_obs_xtab_df)

	obs_xtab_df <- mycreate_xtab(obs_df, c(actual_var, predct_var))
	obs_xtab_df[is.na(obs_xtab_df)] <- 0
	#print(obs_xtab_df)

	for (col_ix in 2:ncol(obs_xtab_df))
		mrg_obs_xtab_df <- merge(mrg_obs_xtab_df[,
			-which(names(mrg_obs_xtab_df) == names(obs_xtab_df)[col_ix])],
								 obs_xtab_df[, c(1, col_ix)], all.x=TRUE)

	mrg_obs_xtab_df <- mrg_obs_xtab_df[, sort(names(mrg_obs_xtab_df))]
	mrg_obs_xtab_df[is.na(mrg_obs_xtab_df)] <- 0
	print(mrg_obs_xtab_df)
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
	algrthm$grid <- function (x, y, len = NULL) data.frame(parameter = "none")
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
		print("in MFO.Classifier$predict")
		y <- factor(rep(modelFit$MFO.val, nrow(newdata)), levels=modelFit$unique.vals)
		if ((length(unique(y)) > 1) | (unique(y) != as.character(modelFit$MFO.val)))
			stop("this should not happen")

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
	algrthm$grid <- function (x, y, len = NULL) data.frame(parameter = "none")
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
		return(prob_df)
	}
	algrthm$sort <- function(x) x
	algrthm$levels <- function(x) levels(x)
	#print(algrthm)
	return(algrthm)
}

mycreate_baseln_classfr <- function() {
	algrthm <- list(label="Baseline.Classifier", type="Classification",
								library=NULL)
	algrthm$parameters <- data.frame(parameter=c("parameter"),
												class=c("character"),
												label=c("parameter"))
	algrthm$grid <- function (x, y, len = NULL) data.frame(parameter = "none")
	algrthm$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
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

		return(list(x_names=x_names, x_vals=as.character(unique(y))))
	}
	algrthm$predict <- function(modelFit, newdata, preProc=NULL,
											submodels=NULL) {
		print("in Baseline.Classifier$predict")
		print("class(newdata):"); print(class(newdata))
		print("head(newdata):"); print(head(newdata))

		print("x_names: "); print(modelFit$x_names)
		print("x_vals: "); print(modelFit$x_vals)

		y <- factor(rep(modelFit$x_vals[1], nrow(newdata)), levels=modelFit$x_vals)
		for (row_ix in 1:nrow(newdata)) {
			if (sum(newdata[row_ix, modelFit$x_names]) > 0) {
				# not the default level in the factor
				y[row_ix] <- factor(modelFit$x_vals[which(newdata[row_ix, ] == 1) + 1],
									levels=modelFit$x_vals)
			}
		}

		print("length(y):"); print(length(y))
		print("head(y):"); print(head(y))
		return(y)
	}
	algrthm$prob <- function(modelFit, newdata, preProc=NULL, submodels=NULL) {
		print("in Baseline.Classifier$prob")
		# Bass-ackwards: b/c in this case, outcomes are easier to predict than probs

		outcomes_vctr <- predict(modelFit, newdata)
		prob_df <- as.data.frame(matrix(rep(0, length(unique(outcomes_vctr)) * nrow(newdata)),
										byrow=TRUE, nrow=nrow(newdata),
								dimnames=list(NULL, as.character(unique(outcomes_vctr)))))
		for (row_ix in 1:nrow(newdata)) {
			out_col_ix <- grep(as.character(outcomes_vctr[row_ix]),
								as.character(unique(outcomes_vctr)))
			prob_df[row_ix, out_col_ix] <- 1
		}
		return(prob_df)
	}
	algrthm$sort <- function(x) x
	algrthm$levels <- function(x) levels(x)
	#print(algrthm)
	return(algrthm)
}

mypredict_mdl <- function(mdl, df, rsp_var, rsp_var_out, model_id_method, label,
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
	stats_df <- data.frame(model_id=model_id_method)

	if ((mdl$modelType == "Classification") && is.binomial) {
		require(ROCR)

		df[, paste0(rsp_var_out, ".prob")] <-
			predict(mdl, newdata=df, type="prob")[, 2]
		ROCRpred <- prediction(df[, paste0(rsp_var_out, ".prob")],
							   df[, rsp_var])
		stats_df[, paste0("max.auc.", label)] <-
			as.numeric(performance(ROCRpred, "auc")@y.values)

		ROCRperf <- performance(ROCRpred, "tpr", "fpr")
		if (length(ROCRperf@y.values[[1]]) > 2) {
			plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2,1.7))

			thresholds_df <- data.frame(threshold=seq(0.0, 1.0, 0.1))
			thresholds_df$f.score <- sapply(1:nrow(thresholds_df), function(row_ix)
				mycompute_classifier_f.score(mdl, obs_df=df,
									proba_threshold=thresholds_df[row_ix, "threshold"],
											  rsp_var,
											  rsp_var_out))
			print(thresholds_df)
			print(myplot_line(thresholds_df, "threshold", "f.score"))

# 			prob_threshold <- thresholds_df[which.max(thresholds_df$f.score),
# 													 "threshold"]
            # Avoid picking 0.0 as threshold
			prob_threshold <- orderBy(~ -f.score -threshold, thresholds_df)[1, "threshold"]
		} else # the plot crashes
			prob_threshold <- 0.5

		print(sprintf("Classifier Probability Threshold: %0.4f to maximize f.score.%s",
					  prob_threshold, label))
		stats_df[, paste0("opt.prob.threshold.", label)] <- prob_threshold

		df[, rsp_var_out] <-
			factor(levels(df[, rsp_var])[
				(df[, paste0(rsp_var_out, ".prob")] >=
					prob_threshold) * 1 + 1], levels(df[, rsp_var]))

		print(mycreate_xtab(df, c(rsp_var, rsp_var_out)))
		stats_df[ , paste0("max.f.score.", label)] <-
			mycompute_classifier_f.score(mdl, df,
										 prob_threshold,
										 rsp_var, rsp_var_out)
	} else df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")


    if (mdl$modelType == "Classification") {
    	conf_lst <- confusionMatrix(df[, rsp_var_out], df[, rsp_var])
    	print(t(conf_lst$table))
    	print(conf_lst$overall)

    	for (stat in c("Accuracy", "AccuracyLower", "AccuracyUpper", "Kappa")) {
    		stats_df[, paste0("max.", stat, ".", label)] <- conf_lst$overall[stat]
    	}

    	if (!is.null(model_summaryFunction)) {
    		df$obs <- df[, rsp_var]; df$pred <- df[, rsp_var_out]
            result <- model_summaryFunction(df)
            if (length(result) > 1) {
                warning("Expecting 1 metric: ", model_metric, "; recd: ", paste0(names(result), collapse=", "),
                        "; retaining ", names(result)[1], " only")
                result <- result[1]
            }
    		stats_df[, paste0(ifelse(model_metric_maximize, "max.", "min."),
    							model_metric, ".", label)] <- result
    	}
    } else
    if (mdl$modelType == "Regression") {
        SSE <- sum((df[, rsp_var_out] - df[, rsp_var]) ^ 2)
        #stats_df[, paste0("max.R.sq.", label)] <- 1 - (SSE / sum((df[, rsp_var] - mean(mdl$finalModel$fitted.values)) ^ 2))
        stats_df[, paste0("max.R.sq.", label)] <- 1 -
            (SSE / sum((df[, rsp_var] - mean(predict(mdl, mdl$trainingData, "raw"))) ^ 2))
        stats_df[, paste0("min.RMSE.", label)] <- (sum((df[, rsp_var_out] - df[, rsp_var]) ^ 2) / (nrow(df) - 0)) ^ 0.5
    }

	if (ret_type == "stats") return(stats_df) else
	if (ret_type == "raw") return(df[, rsp_var_out])
}

myfit_mdl <- function(model_id, model_method, model_type="classification",
						  indep_vars_vctr, rsp_var, rsp_var_out,
						  fit_df, OOB_df=NULL,
						  n_cv_folds=0, tune_models_df=NULL,
						  model_loss_mtrx=NULL, model_summaryFunction=NULL,
						  model_metric=NULL, model_metric_maximize=NULL) {
	require(caret)

	model_id_method <- paste0(model_id, ".", model_method)
    print(sprintf("fitting model: %s", model_id_method))

    if (!(model_type %in% c("regression", "classification")))
        stop("unsupported model_type: ", model_type)
    if (model_type == "classification")
	    if (is.binomial <- (length(unique(fit_df[, rsp_var])) == 2)) {
	        #require(ROCR)
	    }

	models_df <- data.frame(model_id=model_id_method,
							model_method=model_method)

	if ((length(indep_vars_vctr) == 1) & any(indep_vars_vctr %in% "."))
    	indep_vars_vctr <- setdiff(names(fit_df), rsp_var)

#     if (!(".rnorm" %in% indep_vars_vctr))
#     	indep_vars_vctr <- union(indep_vars_vctr, ".rnorm")

    # rpart does not like .rnorm
    if ((model_method == "rpart") & (".rnorm" %in% indep_vars_vctr))
    	indep_vars_vctr <- setdiff(indep_vars_vctr, ".rnorm")

	print(sprintf("    indep_vars: %s", paste(indep_vars_vctr, collapse=", ")))
    models_df$feats	<- paste(indep_vars_vctr, collapse=", ")

    rsp_var_out <- paste0(rsp_var_out, model_id, ".", model_method)

    mytuneGrid <- NULL
#     if (!is.null(tune_models_df) &
#     	(nrow(subset(modelLookup(), model == model_method)) > 0)) {
# 		tune_params_df <- getModelInfo(model_method)[[model_method]]$parameters

    # tune_params_df is needed later irrespective of mytuneGrid
    if (nrow(subset(modelLookup(), model == model_method)) > 0)
        tune_params_df <- getModelInfo(model_method)[[model_method]]$parameters

    if (!is.null(tune_models_df) &
        (nrow(subset(modelLookup(), model == model_method)) > 0)) {
        if (length((tune_params_vctr <- intersect( tune_models_df$parameter,
											tune_params_df$parameter))) > 0) {
			args_lst <- NULL
			for (param_ix in 1:length(tune_params_vctr))
				args_lst[[param_ix]] <- seq(
	 tune_models_df[ tune_models_df$parameter==tune_params_vctr[param_ix], "min"],
	 tune_models_df[ tune_models_df$parameter==tune_params_vctr[param_ix], "max"],
	 tune_models_df[ tune_models_df$parameter==tune_params_vctr[param_ix], "by"])

			# For single values, convert to list
            if (length(args_lst[[1]]) == 1) args_lst <- list(args_lst)
            names(args_lst) <- tune_params_vctr
			mytuneGrid <- do.call("expand.grid", args_lst)
		}
    }
#     print(mytuneGrid)

	if (model_method == "myrandom_classfr") model_method <- mycreate_random_classfr() else
	if (model_method == "mybaseln_classfr") model_method <- mycreate_baseln_classfr() else
	if (model_method == "myMFO_classfr") 	model_method <- mycreate_MFO_classfr()

	set.seed(111)
    methodControl <- ifelse(n_cv_folds > 0, "cv", "none")
    if (!inherits(model_method, "list") && (model_method == "rf"))
    	 methodControl <- "oob"    # cv is not useful for rf

	if (is.null(model_summaryFunction))
		myControl <- trainControl(method=methodControl, number=n_cv_folds, verboseIter=TRUE) else
		myControl <- trainControl(method=methodControl, number=n_cv_folds,
                                  summaryFunction=model_summaryFunction, verboseIter=TRUE)

	model_metric <- ifelse(!is.null(model_metric), model_metric,
					 			ifelse(is.factor(fit_df[, rsp_var]), "Accuracy", "RMSE"))
	model_metric_maximize <- ifelse(!is.null(model_metric_maximize), model_metric_maximize,
				 					ifelse(model_metric == "RMSE", FALSE, TRUE))

	mdl <- train(reformulate(indep_vars_vctr, response=rsp_var), data=fit_df,
				 method=model_method, trControl=myControl,
				 tuneGrid=mytuneGrid,
				 tuneLength=ifelse(n_cv_folds == 0, 1,
				 					ifelse(!is.null(mytuneGrid), nrow(mytuneGrid), 3)),
				 metric=model_metric,
				 maximize=model_metric_maximize)

	#print(mdl$bestTune)
	if ((nrow(mdl$results) > 1) & (mdl$bestTune[1, 1] != "none")) {
		print(ggplot(mdl) + geom_vline(xintercept=mdl$bestTune[1, 1], linetype="dotted"))
		for (param in (params_vctr <- as.character(tune_params_df[, "parameter"]))) {
			if ((mdl$bestTune[1, param] == min(mdl$results[, param])) |
				(mdl$bestTune[1, param] == max(mdl$results[, param])))
				warning("model's bestTune found at an extreme of tuneGrid for parameter: ", param)
		}
	}

	myprint_mdl(mdl)

    models_lst <- glb_models_lst; models_lst[[model_id_method]] <- mdl; glb_models_lst <<- models_lst

	#models_df$n.fit <- nrow(fit_df)
	models_df$max.nTuningRuns <- nrow(mdl$results)
    models_df$min.elapsedtime.everything <- mdl$times$everything["elapsed"]
    models_df$min.elapsedtime.final      <- mdl$times$final["elapsed"]

	# compute/gather fit & OOB prediction stats
		models_df <- merge(models_df, mypredict_mdl(mdl, df=fit_df,
						rsp_var, rsp_var_out, model_id_method, label="fit",
						model_summaryFunction, model_metric,
						model_metric_maximize,ret_type="stats"), all.x=TRUE)
	if (!is.null(OOB_df))
		models_df <- merge(models_df, mypredict_mdl(mdl, df=OOB_df,
						rsp_var, rsp_var_out, model_id_method, label="OOB",
						model_summaryFunction, model_metric,
						model_metric_maximize,ret_type="stats"), all.x=TRUE)

    if (inherits(mdl$finalModel, c("list", "randomForest", "rpart"))) {
    	# list implies custom model
    	# summary(randomForest)$r.squared causes an error
    	# summary(rpart) always spits out stuff
    } else {
		#models_df$max.R.sq.fit <- summary(mdl$finalModel)$r.squared
		models_df$max.Adj.R.sq.fit <- summary(mdl$finalModel)$adj.r.squared
		#models_df$min.SSE.fit <- sum(mdl$finalModel$residuals ^ 2)
		models_df$min.aic.fit <- mdl$finalModel$aic
    }

	if (nrow(mdl$results) > 0) {
		myresults_df <- mdl$results
		for (param in names(mdl$bestTune)) {
			myresults_df <- myresults_df[myresults_df[, param] == mdl$bestTune[1, param], ]
    		if (nrow(myresults_df) != 1)
    			stop ("this should not happen")
    		for (result in setdiff(names(myresults_df), names(mdl$bestTune))) {
    		    metric_pfx <- ifelse(length(grep("Rsquared", result) > 0), "max.",
                                     ifelse(model_metric_maximize, "max.", "min."))
    			models_df[1, paste0(metric_pfx, result, ".fit")] <- myresults_df[1, result]
    		}
		}
	}

	print(models_df)
	glb_models_df <<- myrbind_df(glb_models_df, models_df)

    return(list("model"=mdl, "models_df"= models_df))
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
    plot_vars_df$fit.feat <- (plot_vars_df$id %in% names( entity_df))

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

		vld_plot_vars_df <- rbind(subset(plot_vars_df, fit.feat)[, c("id", "importance")],
									data.frame(id=unique(dummy_vars_df$root.feat),
			importance=tapply(dummy_vars_df$importance, dummy_vars_df$root.feat, max, na.rm=TRUE)))
    } else vld_plot_vars_df <- plot_vars_df

    #print(orderBy(~ -importance, vld_plot_vars_df))
    return(orderBy(~ -importance, vld_plot_vars_df))
}

mymerge_feats_importance <- function(feats_df, sel_mdl, entity_df) {
    plot_vars_df <- myextract_mdl_feats( sel_mdl, entity_df)
    return(orderBy(~ -importance, merge( feats_df, plot_vars_df[,c("id", "importance")], all=TRUE)))
}

## 11.	    predict results for new data
## 11.1	    fit models with data to predict
## 11.2	    collect votes from each cross-validation for each model
## 11.3	    collect votes from each model

## 12.	    export results
