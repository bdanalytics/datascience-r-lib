# Balaji Iyengar's Data Science Process
# Created 2014-08-12
# Check if functions are assigned to proper data science process steps

#suppressPackageStartupMessages(require(<<package name>>))

## 01. 		import data

myimport_data <- function(url, filename=NULL, nrows=-1, comment=NULL, force_header=FALSE, 
							print_diagn=TRUE, ...){
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
    
    # Exclude user-specified features
    numeric_vars <- setdiff(numeric_vars,  exclude_vars_as_features)
    
    # Check for NAs
#     naknts_vctr <- sapply(numeric_vars, 
#                           function(col) sum(is.na( entity_df[, col])))
#     naknts_vctr <- naknts_vctr[naknts_vctr > 0]
#     if (length(naknts_vctr) > 0)
# 	    warning("Ignoring features due to NAs:", paste(names(naknts_vctr), collapse=", "))
# 
#     sel_feats <- setdiff(setdiff(numeric_vars, names(naknts_vctr)), rsp_var)
    sel_feats <- setdiff(numeric_vars, rsp_var)    
    feats_df <- data.frame(id=sel_feats,
                cor.y=cor( entity_df[, sel_feats], 
                            y=as.numeric( entity_df[, rsp_var]), 
                            use="pairwise.complete.obs"))
	feats_df <- orderBy(~ -cor.y.abs, mutate(feats_df, cor.y.abs=abs(cor.y)))
    return(feats_df)
}

# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg,
#              subset(cars_df, am_fctr == "manual")$mpg,
#              var.equal=FALSE)$conf)

## 05.5	    remove features / create feature combinations for highly correlated features
mydelete_cor_features <- function( feats_df,  entity_df, rsp_var,
								  exclude_vars_as_features) {
	require(reshape2)

	if (nrow( feats_df) == 1)
		return(data.frame(id= feats_df$id, cor.low=1))

	 feats_df <-  feats_df
    repeat {
    	if (nrow( feats_df) == 1)
    		break
    		
        print(corxx_mtrx <- cor( entity_df[,  feats_df$id], use="pairwise.complete.obs"))
        abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
        print(abs_corxx_mtrx)
        if (max(abs_corxx_mtrx, na.rm=TRUE) < 0.7) break
        
        row_ix <- ceiling(which.max(abs_corxx_mtrx) / ncol(abs_corxx_mtrx))
        col_ix <- which.max(abs_corxx_mtrx[row_ix, ])
        feat_1 <- rownames(abs_corxx_mtrx)[row_ix]
        feat_2 <- rownames(abs_corxx_mtrx)[col_ix]
        print(sprintf("cor(%s, %s)=%0.4f", feat_1, feat_2, corxx_mtrx[row_ix, col_ix]))
        print(myplot_scatter( entity_df, feat_1, feat_2))
        
        print(sprintf("cor(%s, %s)=%0.4f", rsp_var, feat_1, 
             feats_df[ feats_df$id == feat_1, "cor.y"]))
    #     print(myplot_scatter( entity_df, rsp_var, feat_2))
        print(sprintf("cor(%s, %s)=%0.4f", rsp_var, feat_2, 
             feats_df[ feats_df$id == feat_2, "cor.y"]))
    #     print(myplot_scatter( entity_df, rsp_var, feat_2))
    
        plot_df <- melt( entity_df, id.vars=rsp_var, measure.vars=c(feat_1, feat_2))
        print(myplot_scatter(plot_df, rsp_var, "value", 
                             facet_colcol_name="variable", smooth=TRUE))    
    
#         if ( id_var %in% c(feat_1, feat_2)) drop_feat <-  id_var else {
#   	  if (intersect( id_vars, c(feat_1, feat_2)))
		if (feat_1 %in%  exclude_vars_as_features) drop_feat <- feat_1 else {
			if (feat_2 %in%  exclude_vars_as_features) drop_feat <- feat_2 else {
				drop_feat <- ifelse(
					abs( feats_df[ feats_df$id == feat_1, "cor.y"]) >=
					abs( feats_df[ feats_df$id == feat_2, "cor.y"]),
									feat_2, feat_1)
			}
		}
#         }
        warning("Dropping ", drop_feat, " as a feature")
        print( feats_df <- subset( feats_df, id != drop_feat))
    }
    
     feats_df$cor.low <- 1
    # print( feats_df)
    return( feats_df[, c("id", "cor.low")])
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
mybuild_models_df_row <- function(method, indep_vars_vctr, n.fit, 
							     	inv.elapsedtime.everything,
     								inv.elapsedtime.final,
                                  R.sq.fit=NULL, R.sq.OOB=NULL, 
                                  Adj.R.sq.fit=NULL, 
                                  SSE.fit=NULL, SSE.OOB=NULL,
                                  AIC.fit=NULL, 
                                  auc.fit=NULL, auc.OOB=NULL,
                                  accuracy.fit,
                                  accuracySD.fit)
{
    return(data.frame(
    	method=method,
    	#feats=paste(method, paste(indep_vars_vctr, collapse=", "), sep=":"),
    	feats=paste(indep_vars_vctr, collapse=", "),    	
        #call.formula=toString(summary(mdl)$call$formula),
        n.fit=n.fit,
		inv.elapsedtime.everything=inv.elapsedtime.everything,
     	inv.elapsedtime.final=inv.elapsedtime.final,
        R.sq.fit=ifelse(is.null(R.sq.fit), NA, R.sq.fit),
        R.sq.OOB=ifelse(is.null(R.sq.OOB), NA, R.sq.OOB), 
        Adj.R.sq.fit=ifelse(is.null(Adj.R.sq.fit), NA, Adj.R.sq.fit),
        SSE.fit=ifelse(is.null(SSE.fit), NA, SSE.fit),
        SSE.OOB=ifelse(is.null(SSE.OOB), NA, SSE.OOB),
        AIC.fit=ifelse(is.null(AIC.fit), NA, AIC.fit),
        auc.fit=ifelse(is.null(auc.fit), NA, auc.fit),
        auc.OOB=ifelse(is.null(auc.OOB), NA, auc.OOB),
        accuracy.fit=ifelse(is.null(accuracy.fit), NA, accuracy.fit),
        accuracySD.fit=ifelse(is.null(accuracySD.fit), NA, accuracySD.fit)        
    ))
}    

myrun_mdl_lm <- function(indep_vars_vctr, rsp_var, rsp_var_out, 
						fit_df, OOB_df=NULL) {
    
    if (length(indep_vars_vctr) == 1)
	    if (indep_vars_vctr == ".")
    	    indep_vars_vctr <- setdiff(names(fit_df), rsp_var)
    
    mdl <- lm(reformulate(indep_vars_vctr, 
                            response=rsp_var), data=fit_df)
	plot(mdl, ask=FALSE)
	                            
    if (!is.null(OOB_df)) {
    	OOB_df[, rsp_var_out] <- predict(mdl, newdata=OOB_df)
		print(SSE.OOB <- sum((OOB_df[, rsp_var_out] - 
							  OOB_df[, rsp_var]) ^ 2))
		print(R.sq.OOB <- 1 - (SSE.OOB * 1.0 / 
							sum((OOB_df[, rsp_var] - 
								#mean(OOB_df[, rsp_var])
								mean(mdl$fitted.values)    
							) ^ 2)))	
    } else {SSE.OOB <- NA; R.sq.OOB <- NA}
    
#   s2T <- sum(anova(mdl)[[2]]) / sum(anova(mdl)[[1]])
# 	MSE <- anova(mdl)[[3]][2]
# 	print(adj.R2 <- (s2T - MSE) / s2T)
#	adj.R2 undefined for OOB ?
                           
     models_df <- mybuild_models_df_row(indep_vars_vctr, n.fit=nrow(fit_df),
                                           R.sq.fit=summary(mdl)$r.squared,
                                           R.sq.OOB=R.sq.OOB, 
                                           Adj.R.sq.fit=summary(mdl)$r.squared, 
                                           SSE.fit=sum(mdl$residuals ^ 2), 
                                           SSE.OOB=SSE.OOB)
                                           
    print(summary(glb_mdl <<- mdl));
#     print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, 
              glb_models_df <<- rbind(glb_models_df,  models_df)
#             ))    
    return(list("model"=mdl, "models_df"= models_df))
}

mycompute_confusion_df <- function(obs_df, actual_var, predct_var) {

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
	#print(mrg_obs_xtab_df)			
	return(mrg_obs_xtab_df)			
}

mycompute_classifier_f.score <- function(mdl, obs_df, proba_threshold,
										rsp_var, rsp_var_out) {
	
	if ((class(obs_df[, rsp_var]) != "factor") | 
		(length(levels(obs_df[, rsp_var])) != 2))
		stop("expecting a factor with two levels:", rsp_var)
	obs_df[, rsp_var_out] <- 
		factor(levels(obs_df[, rsp_var])[
			(obs_df[, paste0(rsp_var_out, ".proba")] >= 
				proba_threshold) * 1 + 1])

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

myrun_mdl_classification <- function(indep_vars_vctr, rsp_var, rsp_var_out, 
						  fit_df, OOB_df=NULL, method="glm",  
						  tune_models_df=NULL, n_cv_folds=NULL, 
						  loss_mtrx=NULL, summaryFunction=NULL, metric=NULL, maximize=NULL) {

### Does not work for multinomials yet

    require(ROCR)
    require(caret)
    
#     print(indep_vars_vctr)
#     print(method)
	
	is.binomial <- (length(unique(fit_df[, rsp_var])) == 2)    
	if (length(indep_vars_vctr) == 1)
        if (indep_vars_vctr == ".")
    	    indep_vars_vctr <- setdiff(names(fit_df), rsp_var)
    	    
   tuneGrid <- NULL 	    
   if (!is.null(tune_models_df)) {
		tune_params_df <- getModelInfo(method)[[method]]$parameters
		if (length((tune_params_vctr <- intersect( tune_models_df$parameter, 
											tune_params_df$parameter))) > 0) {
			args_lst <- NULL
			for (param_ix in 1:length(tune_params_vctr))								
				args_lst[[param_ix]] <- seq(
	 tune_models_df[ tune_models_df$parameter==tune_params_vctr[param_ix], "min"], 
	 tune_models_df[ tune_models_df$parameter==tune_params_vctr[param_ix], "max"], 
	 tune_models_df[ tune_models_df$parameter==tune_params_vctr[param_ix], "by"])

			names(args_lst) <- tune_params_vctr
			tuneGrid <- do.call("expand.grid", args_lst)				
		}
   } 	        
#     print(tuneGrid)
	
	if (is.null(n_cv_folds)) n_cv_folds <- 3

	if (is.null(summaryFunction)) {
		myControl <- trainControl(method="cv", number=n_cv_folds, verboseIter=TRUE)
	} else {
		myControl <- trainControl(method="cv", number=n_cv_folds, verboseIter=TRUE,
									summaryFunction=summaryFunction)
	}								
		
	if (is.null(metric)) {	
	    mdl <- train(reformulate(indep_vars_vctr, response=rsp_var), data=fit_df,
    	             method=method, 
        	         trControl=myControl, 
            	     tuneGrid=tuneGrid)
    } else {
    	mdl <- train(reformulate(indep_vars_vctr, response=rsp_var), data=fit_df,
    	             method=method, 
        	         trControl=myControl, 
            	     tuneGrid=tuneGrid, metric=metric, maximize=maximize)
    }        	     

	if (mdl$bestTune[1, 1] != "none") 
		print(ggplot(mdl) + geom_vline(xintercept=mdl$bestTune[1, 1], linetype="dotted")) 

	# Customized plots for each method
	if (method == "glm") plot(native_mdl <- 
			glm(reformulate(indep_vars_vctr, response=rsp_var), data=fit_df, 
				   family="binomial"), ask=FALSE) else
	if (method == "rpart") {
		require(rpart)
	    require(rpart.plot)
		prp(native_mdl <- 
			rpart(reformulate(indep_vars_vctr, response=rsp_var), data=fit_df, 
            	   method="class", control=rpart.control(cp=mdl$bestTune$cp)))
	} else		                  
	if (method == "rf") {
		require(rpart)
		require(randomForest)
		plot(native_mdl <- 
			randomForest(reformulate(indep_vars_vctr, response=rsp_var), data=fit_df, 
            	   control=rpart.control(mtry=mdl$bestTune$mtry)))
	} else		                  
		stop("not implemented yet")		                  

	if (is.binomial) {               
		fit_df[, paste0(rsp_var_out, ".proba")] <- 
			predict(mdl, newdata=fit_df, type="prob")[, 2]		
		ROCRpred <- prediction(fit_df[, paste0(rsp_var_out, ".proba")],
							   fit_df[, rsp_var])
		auc.fit <- as.numeric(performance(ROCRpred, "auc")@y.values)
	} else auc.fit <- NULL
               
    if (!is.null(OOB_df) & is.binomial) {
    	OOB_df[, paste0(rsp_var_out, ".proba")] <- 
    		predict(mdl, newdata=OOB_df, type="prob")[, 2]
    	ROCRpred <- prediction(OOB_df[, paste0(rsp_var_out, ".proba")],
        	                   OOB_df[, rsp_var])
    	auc.OOB <- as.numeric(performance(ROCRpred, "auc")@y.values)
    } else {auc.OOB <- NULL}
    
     models_df <- mybuild_models_df_row(method, indep_vars_vctr, n.fit=nrow(fit_df),
     	inv.elapsedtime.everything=1.0 / mdl$times$everything["elapsed"],
     	inv.elapsedtime.final     =1.0 / mdl$times$final["elapsed"],
                                           R.sq.fit=native_mdl$r.squared, #R.sq.OOB=R.sq.OOB, 
                                           Adj.R.sq.fit=native_mdl$r.squared, 
                                           SSE.fit=sum(native_mdl$residuals ^ 2), #SSE.OOB=SSE.OOB,
                                           AIC.fit=native_mdl$aic,
                                           auc.fit=auc.fit, auc.OOB=auc.OOB,
    	accuracy.fit=mdl$results[mdl$results[, names(mdl$finalModel$tuneValue)[1]] == 
    							 mdl$finalModel$tuneValue[1, 1], 
    							"Accuracy"],
    	accuracySD.fit=mdl$results[mdl$results[, names(mdl$finalModel$tuneValue)[1]] == 
    							 mdl$finalModel$tuneValue[1, 1], 
    							"AccuracySD"]
    							    	)

    print(mdl$finalModel)
    models_lst <- glb_models_lst
    models_lst[[length(glb_models_lst) + 1]] <- mdl
    glb_models_lst <<-  models_lst
    
#      models_fn_lst <- glb_models_fn_lst
#      models_fn_lst[[length(glb_models_lst) + 1]] <- myrun_mdl_classification
#     glb_models_fn_lst <<-  models_lst

#     print(orderBy(~ -auc.OOB, 
                  glb_models_df <<- rbind(glb_models_df,  models_df)
#          ))
#     return(list("model"=mdl, "model_fn"= myrun_mdl_classification, "models_df"= models_df))
#     print("exiting myrun_mdl_classification")
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
myextract_mdl_feats <- function( sel_mdl,  entity_df) {

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
		plot_vars_df <- varImp( sel_mdl)$importance
		names(plot_vars_df)[length(names(plot_vars_df))] <- "importance"
		plot_vars_df <- orderBy(~ -importance, plot_vars_df)
		#print(plot_vars_df)    
    } else stop("not implemented yet")
    
    plot_vars_df$id <- rownames(plot_vars_df)    
    plot_vars_df$fit.feat <- (plot_vars_df$id %in% names( entity_df))
    
    if (nrow(dummy_vars_df <- subset(plot_vars_df, !fit.feat)) > 0) {
		dummy_vars_df$root.feat <- sapply(1:nrow(dummy_vars_df), function(row_ix)
			paste0(unlist(strsplit(dummy_vars_df[row_ix, "id"], ".fctr", fixed=TRUE))[1], 
					".fctr"))
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

mymerge_feats_importance <- function( feats_df,  sel_mdl,  entity_df) {
    plot_vars_df <- myextract_mdl_feats( sel_mdl,  entity_df)
    return(orderBy(~ -importance, merge( feats_df, plot_vars_df[,c("id", "importance")], all=TRUE)))
}

## 11.	    predict results for new data
## 11.1	    run models with data to predict
## 11.2	    collect votes from each cross-validation for each model
## 11.3	    collect votes from each model

## 12.	    export results
