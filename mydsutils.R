# Balaji Iyengar's Data Science Process
# Created 2014-08-12
# Check if functions are assigned to proper data science process steps

#suppressPackageStartupMessages(require(<<package name>>))

## 01. 		import data

myimport_data <- function(url, filename=NULL, nrows=-1, comment=NULL, print_diagn=TRUE, ...){
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
        if (is.null(filename))
            stop("Please specify which file(filename=) should be imported from ",
                 download_filepath)

        file_path <- paste("./data", filename, sep="/")
        if (!file.exists(file_path)) {
    		print(sprintf("Unzipping file %s...", file_path))
            unzip(file_path)
        }
    } else file_path <- download_filepath

    # read.csv reads files with ext %in% c(".csv", ".csv.bz2)
    #	check if file contains header
    first_record <- read.csv(file_path, header=FALSE, quote="", nrows=1)
    header <- FALSE
	
	col_names <- paste(first_record[,])
	if ((all(make.names(col_names) == col_names)) ||
		(length(grep('"', first_record[1, 1])) != 0))
    	header <- TRUE

	if (!(header))
	{
    	warning(file_path, " does not contain header")
	    print("first 10 records:")
	    #print(system(paste0("head ", file_path)))
	    #system(paste0("head ", file_path, " | cat")))
	    print(readLines(file_path, n=10))
	}

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
# mymap <- function(df, from_col_name, to_col_name, map_func) {
#     df[, to_col_name] <- sapply(df[, from_col_name], map_func)

mymap_codes <- function(df, from_col_name, to_col_name, 
						map_df, map_join_col_name=from_col_name, map_tgt_col_name=to_col_name) {
						
# 	if (length(intersect(names(df), names(map_df))) > 0)						
# 		warning("potential column join conflicts: ", intersect(names(df), names(map_df)))						
		
	ret_df <- merge(df, map_df[, c(map_join_col_name, map_tgt_col_name)], 
                    by.x=from_col_name, by.y=map_join_col_name, all.x=TRUE)
		
#     df[, to_col_name] <- sapply(df[, from_col_name], map_func)

    if (length(unique(ret_df[, from_col_name])) == nrow(ret_df)) map_summry_df <- ret_df else {
        require(sqldf)
        sql <- paste0("SELECT ", paste(from_col_name, to_col_name, sep=","), ", SUM(1) AS _n ")
        sql <- paste(sql, "FROM ret_df GROUP BY", paste(from_col_name, to_col_name, sep=","),
                     "ORDER BY _n DESC", sep=" ")
        map_summry_df <- sqldf(sql)
    }

	myprint_df(map_summry_df)
	
	# Works only for 1:1 mapping; 
	#	Use fill aesthetic to display n:m mappings ?
	#		Create a variable that contains n:m ratio for each value of to_col_name ?
	print(myplot_hbar(map_summry_df[1:min(nrow(map_summry_df), 10),], to_col_name, "_n"))

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
myselect_features <- function() {

	# Collect numeric vars
    vars_tbl <- summary(glb_entity_df)
    numeric_vars <- names(glb_entity_df)[grep("^Min.", vars_tbl[1,])]
    
    # Exclude user-specified features
    numeric_vars <- setdiff(numeric_vars, glb_exclude_vars_as_features)
    
    # Check for NAs
    naknts_vctr <- sapply(numeric_vars, 
                          function(col) sum(is.na(glb_entity_df[, col])))
    naknts_vctr <- naknts_vctr[naknts_vctr > 0]
    warning("Ignoring features due to NAs:", paste(names(naknts_vctr), collapse=", "))
    sel_feats <- setdiff(setdiff(numeric_vars, names(naknts_vctr)), glb_predct_var)
    feats_df <- data.frame(id=sel_feats,
                cor.y=cor(glb_entity_df[, sel_feats], 
                            y=glb_entity_df[, glb_predct_var])[,1])
	feats_df <- orderBy(~ -cor.y.abs, mutate(feats_df, cor.y.abs=abs(cor.y)))
    return(feats_df)
}

# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg,
#              subset(cars_df, am_fctr == "manual")$mpg,
#              var.equal=FALSE)$conf)

## 05.5	    remove features / create feature combinations for highly correlated features
mydelete_cor_features <- function() {

    repeat {
        print(corxx_mtrx <- cor(glb_entity_df[, glb_feats_df$id]))
        abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
        print(abs_corxx_mtrx)
        if (max(abs_corxx_mtrx, na.rm=TRUE) < 0.7) break
        
        row_ix <- ceiling(which.max(abs_corxx_mtrx) / ncol(abs_corxx_mtrx))
        col_ix <- which.max(abs_corxx_mtrx[row_ix, ])
        feat_1 <- rownames(abs_corxx_mtrx)[row_ix]
        feat_2 <- rownames(abs_corxx_mtrx)[col_ix]
        print(sprintf("cor(%s, %s)=%0.4f", feat_1, feat_2, corxx_mtrx[row_ix, col_ix]))
        print(myplot_scatter(glb_entity_df, feat_1, feat_2))
        
        print(sprintf("cor(%s, %s)=%0.4f", glb_predct_var, feat_1, 
            glb_feats_df[glb_feats_df$id == feat_1, "cor.y"]))
    #     print(myplot_scatter(glb_entity_df, glb_predct_var, feat_2))
        print(sprintf("cor(%s, %s)=%0.4f", glb_predct_var, feat_2, 
            glb_feats_df[glb_feats_df$id == feat_2, "cor.y"]))
    #     print(myplot_scatter(glb_entity_df, glb_predct_var, feat_2))
    
        plot_df <- melt(glb_entity_df, id.vars=glb_predct_var, measure.vars=c(feat_1, feat_2))
    #     print(myplot_scatter(plot_df, glb_predct_var, "value", colorcol_name="variable"))    
        print(myplot_scatter(plot_df, glb_predct_var, "value", 
                             facet_colcol_name="variable", smooth=TRUE))    
    
#         if (glb_id_var %in% c(feat_1, feat_2)) drop_feat <- glb_id_var else {
#   	  if (intersect(glb_id_vars, c(feat_1, feat_2)))
		if (feat_1 %in% glb_exclude_vars_as_features) drop_feat <- feat_1 else {
			if (feat_2 %in% glb_exclude_vars_as_features) drop_feat <- feat_2 else {
				drop_feat <- ifelse(
					abs(glb_feats_df[glb_feats_df$id == feat_1, "cor.y"]) >=
					abs(glb_feats_df[glb_feats_df$id == feat_2, "cor.y"]),
									feat_2, feat_1)
			}
		}
#         }
        warning("Dropping ", drop_feat, " as a feature")
        print(glb_feats_df <- subset(glb_feats_df, id != drop_feat))
    }
    
    # print(glb_feats_df)
    return(glb_feats_df)
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

## 08.	    run models
mybuild_models_df_row <- function(indep_vars_vctr, n.fit, 
                                  R.sq.fit=NULL, R.sq.OOB=NULL, 
                                  Adj.R.sq.fit=NULL, 
                                  SSE.fit=NULL, SSE.OOB=NULL,
                                  f.score.OOB=NULL)
{
    return(data.frame(feats=paste(indep_vars_vctr, collapse=", "),
        #call.formula=toString(summary(mdl)$call$formula),
        n.fit=n.fit,
        R.sq.fit=ifelse(is.null(R.sq.fit), NA, R.sq.fit),
        R.sq.OOB=R.sq.OOB,
        Adj.R.sq.fit=ifelse(is.null(Adj.R.sq.fit), NA, Adj.R.sq.fit),
        SSE.fit=SSE.fit,
        SSE.OOB=SSE.OOB,
        f.score.OOB=ifelse(is.null(f.score.OOB), NA, f.score.OOB))
    )
}    

myrun_mdl_lm <- function(indep_vars_vctr, fit_df=NULL, OOB_df=NULL) {
    
    if (length(indep_vars_vctr) == 1)
	    if (indep_vars_vctr == ".")
    	    indep_vars_vctr <- setdiff(names(fit_df), glb_predct_var)
    
    mdl <- lm(reformulate(indep_vars_vctr, 
                            response=glb_predct_var), data=fit_df)
    if (!is.null(OOB_df)) {
    	OOB_df[, glb_predct_var_name] <- predict(mdl, newdata=OOB_df)
		print(SSE.OOB <- sum((OOB_df[, glb_predct_var_name] - 
							  OOB_df[, glb_predct_var]) ^ 2))
		print(R.sq.OOB <- 1 - (SSE.OOB * 1.0 / 
							sum((OOB_df[, glb_predct_var] - 
								#mean(OOB_df[, glb_predct_var])
								mean(mdl$fitted.values)    
							) ^ 2)))	
    } else {SSE.OOB <- NA; R.sq.OOB <- NA}
    
#   s2T <- sum(anova(mdl)[[2]]) / sum(anova(mdl)[[1]])
# 	MSE <- anova(mdl)[[3]][2]
# 	print(adj.R2 <- (s2T - MSE) / s2T)
#	adj.R2 undefined for OOB ?
                           
    lcl_models_df <- mybuild_models_df_row(indep_vars_vctr, n.fit=nrow(fit_df),
                                           R.sq.fit=summary(mdl)$r.squared,
                                           R.sq.OOB=R.sq.OOB, 
                                           Adj.R.sq.fit=summary(mdl)$r.squared, 
                                           SSE.fit=sum(mdl$residuals ^ 2), 
                                           SSE.OOB=SSE.OOB)
                                           
    print(summary(glb_mdl <<- mdl));
    print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, 
              glb_models_df <<- rbind(glb_models_df, lcl_models_df)))    
    return(list("model"=mdl, "models_df"=lcl_models_df))
}

myrun_mdl_glm <- function(indep_vars_vctr, fit_df=NULL, OOB_df=NULL) {
    
    if (length(indep_vars_vctr) == 1)
        if (indep_vars_vctr == ".")
    	    indep_vars_vctr <- setdiff(names(fit_df), glb_predct_var)
    
    mdl <- glm(reformulate(indep_vars_vctr, 
                            response=glb_predct_var), data=fit_df, 
               family="binomial")
    if (!is.null(OOB_df)) {
    	OOB_df[, glb_predct_var_name] <- (predict(mdl, 
                        newdata=OOB_df, type="response") >= 0.5) * 1.0
		print(SSE.OOB <- sum((OOB_df[, glb_predct_var_name] - 
							  OOB_df[, glb_predct_var]) ^ 2))
		print(R.sq.OOB <- 1 - (SSE.OOB * 1.0 / 
							sum((OOB_df[, glb_predct_var] - 
								#mean(OOB_df[, glb_predct_var])
								mean(mdl$fitted.values)    
							) ^ 2)))
		OOB_xtab_df <- mycreate_xtab(OOB_df, c(glb_predct_var, glb_predct_var_name))
		#OOB_f_score <- 2 * precision * recall / (precision + recall)
		#OOB_f_score <- (2 * TP) / ((2 * TP) + FP + FN)
		f.score.OOB <- (2 * (OOB_xtab_df[1,2] + OOB_xtab_df[2,3])) / 
						((2 * (OOB_xtab_df[1,2] + OOB_xtab_df[2,3])) + 
						OOB_xtab_df[1,3] + OOB_xtab_df[2,2])
    } else {SSE.OOB <- NA; R.sq.OOB <- NA; f.score.OOB <- NA}
    
    lcl_models_df <- mybuild_models_df_row(indep_vars_vctr, n.fit=nrow(fit_df),
                                           R.sq.fit=summary(mdl)$r.squared,
                                           R.sq.OOB=R.sq.OOB, 
                                           Adj.R.sq.fit=summary(mdl)$r.squared, 
                                           SSE.fit=sum(mdl$residuals ^ 2), 
                                           SSE.OOB=SSE.OOB,
                                           f.score.OOB=f.score.OOB)

    print(summary(glb_mdl <<- mdl));
    print(orderBy(~ -f.score.OOB, 
                  glb_models_df <<- rbind(glb_models_df, lcl_models_df)))                                               
    return(list("model"=mdl, "models_df"=lcl_models_df))
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
myextract_mdl_feats <- function() {
    plot_vars_df <- as.data.frame(summary(glb_sel_mdl)$coefficients)
    names(plot_vars_df)[length(names(plot_vars_df))] <- "Pr.z"
    # Get rid of (Intercept)
    plot_vars_df <- orderBy(~Pr.z, plot_vars_df[2:nrow(plot_vars_df),])
    #print(plot_vars_df <- subset(plot_vars_df, Pr.z < 0.1))
    plot_vars_df$id <- rownames(plot_vars_df)
    #print(plot_vars_df)
    return(plot_vars_df)
}

mymerge_feats_Pr.z <- function() {
    plot_vars_df <- myextract_mdl_feats()
    return(orderBy(~Pr.z, merge(glb_feats_df, plot_vars_df[,c("id", "Pr.z")])))
}

## 11.	    predict results for new data
## 11.1	    run models with data to predict
## 11.2	    collect votes from each cross-validation for each model
## 11.3	    collect votes from each model

## 12.	    export results
