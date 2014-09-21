library(ISLR)
require(MASS)
require(class)
require(boot)		# Resampling

rm(list = ls())
system("ls")

### Help
?rnorm
help.search("rnorm")
args("rnorm")
rnorm					# shows the code

?Boston
typeof(bubba)
class(Auto)

attributes(object); attr(object, attr_name)
summary(object)
str(object)
names(Boston)
colnames(occur) <- c("A","B","C");      attr(fxt, "col.vars")
rownames(sexsmoke)<-c("male","female"); attr(fxt, "row.vars")
dim(z); length(object)
object.size(fakeData); print(object.size(fakeData), units="Mb"); 
methods(class="object_size"); methods(print); ?print.object_size

### Debugging
traceback() # prints function call stack immediately after an error
debug()     # flags a function to run line by line
            # 'n' to execute next line
browser()   # suspends execution of a function & puts it in debug mode
trace()     # insert debugging code into a function
options(error = recover)    # modify error behavior 
                            # browse function call stack

### Profiling
system.time()
Rprof()         # Do not use system.time() w/ Rprof()
summaryRprof()

### data types
is.numeric(a)

tree$C <- factor(tree$C)    # categorical variables; summary provides freqs rather than numeric stats
is.factor(a)
contrasts(Carseats$ShelveLoc)

a = TRUE
b = FALSE

### vectors, data, matrices, subsetting; index begins with 1

x=c(2,7,5)	        # combine
a = numeric(10)	    # list of 10 zeros
a = character(20)   # list of null characters
y=seq(from=4,length=3,by=3)
x[2]; x[2:3]; x[-2]; x[-c(1,2)]
x[0]		# displays type
X[1:5,]; X$var2; X[, 1]; X[, "var1"]; X[1:2, "var2"]
X[(X$var1 <= 3 & X$var3 > 11),]; X[(X$var1 <= 3 | X$var3 > 15),]    # Includes NAs
X[which(X$var2 > 8),]                                               # Excludes NAs

z=matrix(seq(1,12),4,3)
z[3:4,2:3]; z[,2:3]; z[,1]; z[,1,drop=FALSE]
sexsmoke<-matrix(c(70,120,65,140),ncol=2,byrow=TRUE)
Xlag=cbind(Lag1,Lag2)

results <- table(factor(c("A","A","B","A","B","B","C","A","C")))
occur <- as.table(occur)
a <- c("Sometimes","Sometimes","Never","Always","Always","Sometimes","Sometimes","Never")
b <- c("Maybe","Maybe","Yes","Maybe","Maybe","No","Yes","No")
results <- table(a,b)

with(NEI_baltimore_md_yrttl_motor_df, {
    print(parent.frame(2)$data)
    print(names(parent.frame(2)$data))
    assign(names(parent.frame(2)$data)[2], "emissions_pm_2_pt_5")
    #colnames(parent.frame(2)$data)[2] <- "emissions_pm_2_pt_5"
    # Need to convert year to factor to get correct x-axis labels
    #   otherwise labels appear as 1998.5 etc.
    
    #year_fr <- as.factor(year)
})

tolower(names(cameraData)); toupper()
nchar("Jeffrey Leek") # count characters 
require(stringr); 
substr("Jeffrey Leek", 1, 7)
str_trim("Jeff     ")
sub("data/", "data.", fileUrl) # replace (first) chars in a string
gsub("data/", "data.", fileUrl) # globally replace chars in a string
des3_col_names <- gsub("\\(\\)", "", des2_col_names)
paste(str1, str2, sep=".")      # concatenate strings
paste0(str1, str2)
grep("fBodyAcc-bandsEnergy()", features_Df$Name.Nbr) # string matching
grep("-mean|-std", features_Df$Name.Nbr)
grep("-mean|-std", features_Df$Name.Nbr, value=TRUE)
distinct_colors <- grep("^[^grey]", colors(), value=TRUE)
distinct_colors <- setdiff(distinct_colors, grep("gray", distinct_colors, value=TRUE))
grepl_res <- grepl("Alameda", cameraData$intersection) # creates logical vector
table(grepl_res) # counts occurences
cameraData2 <- cameraData[!grepl_res, ]
# find char pos in a string 
strsplit(names(cameraData), "\\.")
sapply(strsplit(desc_col_names[81], ''), function(x) which(x == '.'))
sapply(splitNames, function(x) {x[1]})

# regular expressions
"^i think"  # matches strings that begin with 'i think'
"morning$"  # matches strings that end with 'morning'
"[Bb][Uu][Ss][Hh]"  # matches strings that contain all case 
                    #   permutations of Bush
"[0-9]" # matches strings that contain a number
"[a-zA-Z]"  # matches strings that contain an uppercase OR lowercase letter
"[^?.]$"    # matches strings that do NOT contain '?' OR '.' at the end  
"9.11"      # matches strings that contains '9' followed by ANY/NO char
            #   followed by '11'
"flood|fire"    # matches strings that contain 'flood' OR 'fire'
"^([Gg]ood|[Bb]ad)" # matches strings that begin with 'Good' OR 'good'
                    #   OR 'Bad' OR 'bad'
"[Gg]eorge( [Ww]\.)? [Bb]ush"   # '?' expression is optional
"(.*)"  # * ANY repetition of chars enclosed in '()'
        #   * is greedy - matches LONGEST possible string that satisfies the 
        #   regex 
        #   * greediness can be turned off with a ?, as in "^s(.*?)s$"
"(.+)"  # + at least ONE char enclosed in '()'
"[0-9]+ (.*)[0-9]+" # at least TWO numbers with at least ONE space in between
"[Bb]ush( +[^ ]+ +){1,5} debate"    # matches strings that contain 
                                    #   ('Bush' OR 'bush') 
                                    #   AND have 1-5 words apart from 
                                    #   ' debate' AND followed by ' debate'  
"{m,n}" # at least m but not more than n matches
"{m}"   # exactly m matches
"{m,}"  # at least m matches
"\1"    # refers to FIRST item in a regex that is enclosed in '()' 
"\2"    # refers to SECOND item in a regex that is enclosed in '()' 
"+([a-zA-z]+) +\1 +" # matches strings that have a word repeated separated by one space

# date & time
date()  # returns a character vector
Sys.Date()  # returns an object of "Date" class
format(Sys.Date, "%a %b %d")    # %d:   day as number (0-31)
                                # %a:   abbreviated weekday
                                # %A:   unabbreviated weekday
                                # %m:   month as number (01-12)
                                # %b:   abbreviated month
                                # %B:   unabbreviated month
                                # %y:   2 digit year
                                # %Y:   4 digit year
as.Date("1jan1960", "%d%b%Y")
weekdays(Sys.Date()); months(Sys.Date())
julian(Sys.Date())  # number of days since 1970-01-01
Sys.timezone()

require(lubridate)
ymd("20140108")
mdy("08/04/2013")
dmy("03-04-2013")

mylist <- list(letters=c("A", "b", "c")
               , numbers = 1:3
               , matrix(1:25, ncol=5))
head(mylist)
mylist[1]; mylist$letters; mylist[[1]]

# Data type conversions
as.numeric()
pm2_5_daily_subset_df$State.Name_str <- sapply(pm2_5_daily_subset_df$State.Name
                                               , toString)
airquality_my <- transform(airquality, Month = factor(Month))

paste(power_raw_df[2879, "Date"], power_raw_df[2879, "Time"])

power_raw_df$date <- as.Date(power_raw_df$Date, format="%d/%m/%Y")
head(power_raw_df[, c("date", "Date")])
tail(power_raw_df[, c("date", "Date")])

my_tm <- strptime(power_raw_df[2879, "Time"], format="%H:%M:%S")
my_tm$sec; my_tm$min; my_tm$hour
strptime(power_raw_df[1, "Time"])

## Create time series of Global active power
power_ts <- ts(power_df[, c("Sub_metering_1", "Sub_metering_2",
                            "Sub_metering_3")],
               start=c(power_df[1, "sample_pxt"]$year,
                       power_df[1, "sample_pxt"]$mon,
                       power_df[1, "sample_pxt"]$mday,
                       power_df[1, "sample_pxt"]$hour,
                       power_df[1, "sample_pxt"]$min))


# Data generation
rep(1:9, len=54)
seq(1, 10, by=2); seq(1, 10, length=3); seq(along=c(1, 3, 8, 25, 100))

set.seed(1)
x=runif(50)
y=rnorm(50)

help(Distributions)
## Normal Distribution
help(Normal)
dnorm(0)		# height of probability density function
pnorm(0)		# cumulative density function
qnorm(0)		# inverse cumulative density function (quantiles)
rnorm(0)		# random number generation

## t Distribution
help(TDist)
dt(x)
pt(x)
qt(x)
rt(x)

## Binomial Distribution
help(Binomial)
dbinom; pbinom; qbinom; rbinom

## Chi-squared Distribution
help(Chisquare)
dchisq; pchisq; qchisq; rchisq

## Poisson Distribution
rpois; dpois; ppois; qpois

## Custom types
sample(1:10, 4)
sample(letters, 5)
sample(1:10) ## permutation
sample(1:10, replace=TRUE)  # sample w/ replacement
dateDownloaded <- date()

yesno <- sample(c("yes", "no"), size=10, replace=TRUE); 
yesnofac <- factor(yesno, levels=c("yes", "no")); relevel(yesnofac, ref="yes"); as.numeric(yesnofac)

set.seed(13435)
X <- data.frame("var1"=sample(1:5), "var2"=sample(6:10), "var3"=sample(11:15))
X <- X[sample(1:5),]; X$var2[c(1,3)] <- NA

# 01. import data
pwd()
getwd()
setwd()

## Inspect data file
#system("ls -alF data/")
#system("head data/household_power_consumption.txt")
# check missing values handling
#system("grep -n '?' data/household_power_consumption.txt | head")
#mean(is.na(x))     # what proportion of vector has missing values ?
#power_df <- read.table("data/household_power_consumption.txt", header=TRUE,
#                       sep=";", na.strings = "?", nrows=6850)
#tail(power_df,11)
# check that file has 2,075,259 obs (+1 header)
#system("wc -l data/household_power_consumption.txt")
pipe('grep "^[1-2]/2/2007" "household_power_consumption.txt"')

read_file <- function(entity_name, file_type, col_names) {
    # Refactor to import all file types and check for dups of column names
    
    file_name <- paste("./", entity_name, "/", file_type, "_", entity_name
                       , ".txt", sep="")
    message("    reading file '", file_name, "'...")
    print(  "    sample records:")
    system(paste("head ", file_name, sep=""))
    file_type_Df <- read.table(file_name, header=FALSE)
    names(file_type_Df) <- col_names
    print(  "    sample obs:")
    print(head(file_type_Df))
    print(  "    Df summary:")
    print(summary(file_type_Df))
    
    if (length(table(des3_col_names)) != 82) {
        message("possible dups in descriptive variable names")
        print(des3_col_names)
        stop("assertion error; stopping")
    }
    
    return(file_type_Df)
}

read_data <- function(entity_name) {
    message("reading files for ", entity_name, " data...")
    #system(paste("ls -alrtF", entity_name, sep=" "))
    
    entity_subject_Df <- read_file(entity_name, "subject", "subject.nbr")
    entity_y_Df <- read_file(entity_name, "y", "y.activity.nbr")
    entity_X_Df <- read_file(entity_name, "X", features_Df$Name.Nbr)
    entity_Df <- cbind(entity_subject_Df, entity_y_Df, entity_X_Df)       
    return (entity_Df)
}

load("~/Downloads/7.R.RData")
SCC <- readRDS("Source_Classification_Code.rds")

data(UCBAdmissions)
DF = as.data.frame(UCBAdmissions)

dir()
if (!file.exists("directoryName")) {dir.create("directoryName")}
list.files("./data")

require(descr); file.head("./data/restaurants.csv", n, truncate.cols = TRUE)
system("wc -l ./data/restaurants.csv")

Auto <- read.csv("../Auto.csv")  # read.csv2()
                                # quote="", na.strings, nrows, skip

cameraData <- read.table("./data/cameras.csv", sep=",", header=TRUE)

read.fwf()	# fixed width file

fileUrl <- "https://data/baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile="./data/cameras.csv", method="curl")
dateDownloaded <- date()

library(data.table) # faster than data.frame
train_X_dt <- fread("./train/X_train.txt", header=FALSE, sep=' ')

DF <- data.frame(x=rnorm(9), y=rep(c("a", "b", "c"), each=3), z=rnorm(9))
head(DF, 3)
DT <- data.table(x=rnorm(9), y=rep(c("a", "b", "c"), each=3), z=rnorm(9))
head(DT, 3)
tables()
DT[, list(mean(x), sum(z))]
DT[, table(y)]
DT[, w:=z^2]    # Adding new columns
DT[, m:= {tmp <- (x+z); log2(tmp+5)}]
DT[, a:=x>0]
DT[, b:= mean(x+w),by=a]
DT[, .N,by=x]   # .N is count/size
{setkey(DT, x); DT['a']}
{setkey(DT1, x); setkey(DT2, x); merge(DT1, DT2)}

# XLS files
library(xlsx)   # XLConnect; XLConnect vignette
cameraData <- read.xlsx("./data/cameras.xslx", sheetIndex=1, header=TRUE)
            # read.xlsx2(); faster but unstable for reading subsets of rows
            # read.xlsx(colIndex=2:3, rowIndex=1:4)
            # write.xlsx()

# Web & XML & httr
con <- url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode <- readLines(con)
close(con)

library(XML)
url <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html <- htmlTreeParse(url, useInternalNodes=T)
xpathSApply(html, "//title", xmlValue)
xpathSApply(html, "//td[@id='col-citedby']", xmlValue)

library(httr)


url <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html2 <- GET(url)
content2 <- content(html2, as="text")
parsedHtml <- htmlParse(content2, asText=TRUE)
xpathSApply(parsedHtml, "//title", xmlValue)

pg2 <- GET("http://httpbin.org/basic-auth/user/passwd"
           , authenticate("user", "passwd"))

google <- handle("http://google.com")
pg1 <- GET(handle=google, path="/")
pg2 <- GET(handle=google, path="search")

fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl, useInternal=TRUE) # htmlTreeParse
restaurants.Baltimore <- xmlTreeParse("http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
                                      , isURL=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
rootNode[[1]]
rootNode[[1]][[1]]
xmlSApply(rootNode, xmlValue)
xpathSApply(rootNode, "//name", xmlValue)
scores <- xpathSApply(doc, "//li[@class='score']", xmlValue)

library(jsonlite) # jsonlite vignette
jsonData <- fromJSON("https://api.github.com/users/jtleek/repos")
names(jsonData)
names(jsonData$owner)
jsonData$owner$login
myjson <- toJSON(iris, pretty=TRUE)
cat(myjson)

# mysql
library("RMySQL")

ucscDb <- dbConnect(MySQL(), user="genome", host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb, "show databases;")
dbDisconnect(ucscDb)

hg19 <- dbConnect(MySQL(), user="genome", db="hg19", 
                  host="genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(hg19)
dbListFields(hg19, "affyU133Plus2")
dbGetQuery(hg19, "select count(*) from affyU133Plus2;")
affyData <- dbReadTable(hg19, "affyU133Plus2")
query <- dbSendQuery(hg19, 
                     "select * from affyU133Plus2 where misMatches between 1 and 3;")
affyMis <- fetch(query)
quantile(affyMis$misMatches)

affyMisSmall <- fetch(query, n=10)
dbClearResult(query)
dbDisconnect(hg19)

# HDF5
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")

library(rhdf5)
created <- h5createFile("example.h5")
created <- h5createGroup("example.h5", "foo")
created <- h5createGroup("example.h5", "baa")
created <- h5createGroup("example.h5", "foo/foobaa")
A <- matrix(1:10, nr=5, nc=2)
h5write(A, "example.h5", "foo/A")
readA <- h5read("example.h5", "foo/A")
h5write(c(12, 13, 14), "example.h5", "foo/A", index=list(1:3, 1))
readA2 <- h5read("example.h5", "foo/A")
readA2.1to3.1 <- h5read("example.h5", "foo/A", index=list(1:3, 1))
B <- array(seq(0.1, 2.0, by=0.1), dim=c(5,2,2))
attr(B, "scale") <- "liter"
h5write(B, "example.h5", "foo/foobaa/B")
readB <- h5read("example.h5", "foo/foobaa/B")
df <- data.frame(1L:5L, seq(0, 1, length.out=5)
                 , c("ab", "cde", "fghi", "a", "s"), stringsAsFactors=FALSE)
h5write(df, "example.h5", "df")
readdf <- h5read("example.h5", "df")
names(df) <- c("C1", "C2", "C3")
h5write(df, "example.h5", "df2")
readdf2 <- h5read("example.h5", "df2")
h5ls("example.h5")

# APIs
myapp <- oauth_app("twitter", key="yourConsumerKeyHere"
                   , secret="yourConsumerSecretHere")
sig <- sign_oauth1.0(myapp, token="yourTokenHere"
                     , token_secret="yourTokenSecretHere")
homeTL <- GET("htpps://api.twitter.com/1.1/statuses/home_timeline.json", sig)
json1 <- content(homeTL)
json2 <- jsonlite::fromJSON(toJSON(json1)); json2[1,1:4]

data.frame(lda.pred)[1:5,]

a <- c(1,2,3,4)
b <- c(2,4,6,8)
levels <- factor(c("A","B","A","B"))
bubba <- data.frame(first=a,second=b,f=levels)

# Data Inspection -> Elementary stuff in Help section
head(restData, n=3); tail(restData)

agricultureLogical <- (acsDf$ACR == 3) & (acsDf$AGS == 6); which(agricultureLogical)

colSums(is.na(restData))
all(colSums(is.na(restData)) == 0)
any(is.na(restData$councilDistrict))
all(restData$zipCode > 0)

min(a, b)        # Least value element in both vectors
pmin(a, b)		# Entire vector compared			
mean(a)
colMeans(df)
var(a)
sd(a)
apply(samsung_subset_df[, 1:3], 2, min) # min of each vector
samsung_sbst_scld_df <- data.frame(scale(samsung_subset_df[, 1:561]))
tidy_Df <- aggregate(subset(actlbl_stt_Df
                            , select=-c(subject.nbr, Activity, Activity.nbr))
                     , list(Subject = actlbl_stt_Df$subject.nbr
                            , Activity = actlbl_stt_Df$Activity), mean)
subset(samsungData, subject == 1)
with(samsungData[, 1:3], {
    this_df <- parent.frame(2)$data
    distanceMatrix <- dist(samsungData[, 1:3])
    hclustering <- hclust(distanceMatrix)
    myplclust(hclustering, lab.col = unclass(samsungData$activity))
})
quantile(restData$councilDistrict, na.rm=TRUE)
quantile(restData$councilDistrict, probs=c(0.5, 0.75, 0.9))
table(restData$zipCode, useNA="ifany"); table(restData$councilDistrict, restData$zipCode)
summary(as.data.frame(Df))

sum(a); with(Hitters, sum(is.na(Salary)))
sum(is.na(restData$councilDistrict))

table(restData$zipCode %in% c("21212")); table(restData$zipCode %in% c("21212", "21213"))
table(restData$zipWrong, restData$zipCode < 0)
xt <- xtabs(Freq ~ Gender + Admit, data=DF)
xt <- xtabs(breaks ~., data=warpbreaks); ftable(xt)

plot(medv~lstat,Boston)
attach(Boston)
search()
par(mfrow=c(1,1))
plot(medv~lstat)

# Data Sampling
## Subsets
train = Year<2005
Direction.2005=Smarket$Direction[!train]
Smarket.2005=subset(Smarket,Year==2005)
restData[restData$zipCode %in% c("21212", "21213"), ]
subset(sorted_Df, select=-Long.Name)    # Exclude specific columns

# Data Transformation
## Rename vars
## Get rid of suffix beginning with "." but only if followed by a number
extract_prefix <- function(str) {
    str_chrs <- strsplit(str, '')
    dot_pos <- sapply(str_chrs, function(x) which(x == '.'))
    #sprintf("str='%s'", str)
    #sprintf("str='%s'; dot_pos=%d", str, dot_pos)
    if (class(dot_pos) == "list") {dot_pos <- 0}    # Unsure why this happens
    if ((dot_pos > 0) &
            (str_chrs[[1]][dot_pos+1] >= "0") & 
            (str_chrs[[1]][dot_pos+1] <= "9")) { 
        return(paste0(str_chrs[[1]][1:dot_pos-1], collapse=""))
    }
    else {return(str)}
}

cylinders=as.factor(cylinders)
Hitters = na.omit(Hitters)
c = sort(a,decreasing = TRUE)
sort(X$var1); sort(X$var1, decreasing=TRUE); sort(X$var1, na.last=TRUE)
X[order(X$var1), ]; X[order(X$var1, X$var3), ]

require(plyr); arrange(X, var1); arrange(X, desc(var1))

# Adding columns
X$var4 <- rnorm(5); warpbreaks$replicate <- rep(1:9, len=54)
restData$zipWrong <- ifelse(restData$zipCode < 0, TRUE, FALSE)
restData$zipGroups <- cut(restData$zipCode, breaks=quantile(restData$zipCode))
require(Hmisc); restData$zipGroups2 <- cut2(restData$zipCode, g=4)
restData$zcf <- factor(restData$zipCode)
require(Hmisc); require(plyr); restData2 <- mutate(restData, zipGroups=cut2(zipCode, g=4))
abs(x); sqrt(x); ceiling(x); floor(x); round(x, digits=n); signif(x, digits=n)
cos(x); sin(x)
log(x); exp(x); log2(x); log10(x)
mutate  # add new vars

## calculate deciles
cutpoints <- quantile(maacs$logno2_new, seq(0, 1, length=11), na.rm=TRUE)
maacs$no2dec <- cut(maacs$logno2_new, cutpoints)

cbind(X, rnorm(5))

fix(Carseats)   # manual editing of data frame

# Pivot down
require(reshape2)
mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars, id=c("carname", "gear", "cyl"), measure.vars=c("mpg", "hp"))

# Pivot up
cylData <- dcast(carMelt, cyl ~ variable); cylData <- dcast(carMelt, cyl ~ variable, mean);
tapply(InsectSprays$count, InsectSprays$spray, sum)

spIns <- split(InsectSprays$count, InsectSprays$spray)
sprCount <- lapply(spIns, sum); unlist(sprCount)

sapply(spIns, sum)

require(plyr)
ddply(InsectSprays, .(spray), summarize, sum=sum(count))
spraySums <- ddply(InsectSprays, .(spray), summarize, sum=ave(count, FUN=sum))

acast   # casting as multi-dimensional arrays
arrange # fast reordering

# Aggregations
tidy_Df <- aggregate(subset(actlbl_stt_Df
                            , select=-c(subject.nbr, Activity, Activity.nbr))
                     , list(Subject = actlbl_stt_Df$subject.nbr
                            , Activity = actlbl_stt_Df$Activity), mean)


### Data Merging
fileUrl1 = "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 = "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1, destfile="./data/reviews.csv", method="curl")
download.file(fileUrl2, destfile="./data/solutions.csv", method="curl")
reviews <- read.csv("./data/reviews.csv"); solutions <- read.csv("./data/solutions.csv")
intersect(names(reviews), names(solutions))
mergedData <- merge(reviews, solutions, by.x="solution_id", by.y="id", all=TRUE)

require(plyr)
df1 <- data.frame(id=sample(1:10), x=rnorm(10))
df2 <- data.frame(id=sample(1:10), y=rnorm(10))
df3 <- data.frame(id=sample(1:10), z=rnorm(10))
intersect(names(df1), names(df2))
arrange(join(df1, df2), id)

dfList = list(df1, df2, df3)
join_all(dfList)

## LOOCV
glm.fit=glm(mpg~horsepower, data=Auto)
cv.glm(Auto,glm.fit)$delta #pretty slow (doesnt use formula (5.2) on page 180)
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}
loocv(glm.fit)
cv.error=rep(0,5)
degree=1:5
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

## 10-fold CV
cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")

## Bootstrap
alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Portfolio$X,Portfolio$Y)
# What is the standard error of alpha?
alpha.fn=function(data, index){
  with(data[index,],alpha(X,Y))
}
alpha.fn(Portfolio,1:100)
set.seed(1)
alpha.fn (Portfolio,sample(1:100,100,replace=TRUE))
boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)
tsboot.out=boot(Portfolio,alpha.fn,R=1000,sim="fixed",l=100)

### PCA
set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
par(mar = rep(0.2, 4))
heatmap(dataMatrix)
set.seed(678910)
for (i in 1:40) {
    # flip a coin
    coinFlip <- rbinom(1, size = 1, prob = 0.5)
    # if coin is heads add a common pattern to that row
    if (coinFlip) {
        dataMatrixP[i, ] <- dataMatrix[i, ] + rep(c(0, 3), each = 5)
    }
}
par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
par(mar = rep(0.2, 4))
heatmap(dataMatrix)
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[, 1], 40:1, , xlab = "Row", ylab = "First left singular vector",
     pch = 19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained",
     pch = 19)
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[, 1], svd1$v[, 1], pch = 19, xlab = "Principal Component 1",
     ylab = "Right Singular Vector 1")
abline(c(0, 1))
constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1),each=5)}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of variance explained",pch=19)
set.seed(678910)
for (i in 1:40) {
    # flip a coin
    coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
    coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
    # if coin is heads add a common pattern to that row
    if (coinFlip1) {
        dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), each = 5)
    }
    if (coinFlip2) {
        dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), 5)
    }
}
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rep(c(0, 1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(rep(c(0, 1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2")
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd2$v[, 1], pch = 19, xlab = "Column", ylab = "First right singular vector")
plot(svd2$v[, 2], pch = 19, xlab = "Column", ylab = "Second right singular vector")
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Percent of variance explained",
     pch = 19)
dataMatrix2 <- dataMatrixOrdered
## Randomly insert some missing data
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA
svd1 <- svd(scale(dataMatrix2)) ## Doesn't work!
library(impute) ## Available from http://bioconductor.org
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=FALSE)] <- NA
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered)); svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2)); plot(svd1$v[,1],pch=19); plot(svd2$v[,1],pch=19)

dimnames(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
pca.out = prcomp(USArrests, scale = TRUE)
pca.out
names(pca.out)
biplot(pca.out, scale = 0)

load("data/face.rda")
image(t(faceData)[, nrow(faceData):1])
svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2), pch = 19, xlab = "Singular vector", ylab = "Variance explained")
svd1 <- svd(scale(faceData))
## Note that %*% is matrix multiplication
# Here svd1$d[1] is a constant
approx1 <- svd1$u[, 1] %*% t(svd1$v[, 1]) * svd1$d[1]
# In these examples we need to make the diagonal matrix out of d
approx5 <- svd1$u[, 1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[, 1:5])
approx10 <- svd1$u[, 1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[, 1:10])
par(mfrow = c(1, 4))
image(t(approx1)[, nrow(approx1):1], main = "(a)")
image(t(approx5)[, nrow(approx5):1], main = "(b)")
image(t(approx10)[, nrow(approx10):1], main = "(c)")
image(t(faceData)[, nrow(faceData):1], main = "(d)") ## Original data

### Plots
## Base System
require(graphics)
require(grDevices)

# Default values
par(c("pch", "lty", "lwd", "col", "las", "bg", "mar", "oma", "mfrow", "mfcol"))
par(xaxt="n"|"s")
xlab=, ylab=, type="n"
plot(), lines(), points(), text(), title(main=""), mtext("", outer=), axis()
example(points)
legend("topright", attr(power_ts, "dimnames")[[2]], lty=1,
       col=c("black", "red", "blue"))

# Histogram
require(datasets)
hist(airquality$Ozone)
hist(power_df$Global_active_power, col="red", main="Global Active Power", 
     xlab="Global Active Power (kilowatts)")

# Scatterplot
library(datasets)
with(airquality, plot(Wind, Ozone
                      , main="Ozone and Wind in New York City"
                      , pch=20
                      , col="blue"))

with(airquality, plot(Wind, Ozone
                      , main="Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col="blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col="red"))
legend("topright", pch=1, col=c("blue", "red")
       , legend=c("May", "Other Months"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City",
                      pch = 20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)

par(mfrow = c(1, 2))
with(airquality, {
                    plot(Wind, Ozone, main = "Ozone and Wind")
                    plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
})

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
    plot(Wind, Ozone, main = "Ozone and Wind")
    plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
    plot(Temp, Ozone, main = "Ozone and Temperature")
    mtext("Ozone and Weather in New York City", outer = TRUE)
})

# Boxplot
library(datasets)
airquality_my <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality_my, xlab = "Month", ylab = "Ozone (ppb)")

# Time Series
with(df, plot(DateTime,Global_active_power, type="l", xlab="",
              ylab="Global Active Power (kilowatts)"), xaxt = "n")
plot.ts(power_ts, ylab="Energy sub metering", xlab=NULL, plot.type="single",
        col=c("black", "red", "blue"))
plot.ts(power_ts[, "Global_active_power"], 
        ylab="Global Active Power", xlab=NULL, cex.lab=0.5, cex.axis=0.8)

# Files
pdf(file = "myplot.pdf") ## Open PDF device; create 'myplot.pdf' in my working directory
## Create plot and send to a file (no plot appears on screen)
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data") ## Annotate plot; still nothing on screen
dev.off() ## Close the PDF file device
## Now you can view the file 'myplot.pdf' on your computer

dev.copy(png, file = "plot1.png", height=480, width=480); dev.off()

# SVG (Scalable Vector Graphics) format for web-based plots
# Vector formats are good for line drawings and plots with solid colors using a
#   modest number of points
# Bitmap formats are good for plots with a large number of points, natural 
#   scenes or webbased plots

# for plotting on multiple devices simultaenously
dev.cur(); dev.set(<integer>)

# copying plots
library(datasets)
with(faithful, plot(eruptions, waiting)) ## Create plot on screen device
title(main = "Old Faithful Geyser data") ## Add a main title
dev.copy(png, file = "geyserplot.png") ## Copy my plot to a PNG file
dev.copy2pdf("geyserplot.pdf")
dev.off() ## Don't forget to close the PNG device!


## Lattice System
require(lattice)
require(grid)

# scatter plots
xyplot(y ~ x | f * g, data=df, layout=c(5,1))
xyplot(y ~ x | f, layout=c(2, 1), panel = function(x, y, ...) {
    panel.xyplot(x, y, ...) ## First call the default panel function for 'xyplot'
    panel.abline(h = median(y), lty=2) ## Add a horizontal line at the median
    panel.lmline(x, y, col=2)
})

splom

# violin string dot plots
dotplot

# box plots
bwplot

# box plots with actual points
stripplot

# histograms
histogram

# images
levelplot
contourplot

## ggplot2 System (gRAMMER gRAPHICS)
require(ggplot2)
theme(legend.position="none"); theme_gray(); theme_bw(base_family="Times")

# histogram
qplot(hwy, data=mpg, fill=drv)
qplot(hwy, data=mpg, fill=~ drv, binwidth=2)
qplot(log(eno), data=maacs, geom="density", color=mopos)

g <- ggplot(maacs, aes(logpm25, NocturnalSympt))
p <- g + geom_point(); print(p) # OR auto-print with g + geom_point()
g + geom_point(color="steelblue" | aes(color=bmicat), size=4, alpha=1/2) + 
    facet_grid(. ~ bmicat) + 
    geom_smooth(size=4, linetype=3, method="lm", se=FALSE) + 
    labs(title="MAACS Cohort") + 
    labs(x=expression("log " * PM[2.5]), y="Nocturnal Symptoms") +
    theme_bw(base_family="Times") +
    ylim(-3, 3) | coord_cartesian(ylim=c(-3, 3)) + 

# scatter plot
qplot(displ, hwy, data=mpg, color=drv, geom=c("point", "smooth"), method="lm")
qplot(displ, hwy, data=mpg, shape=drv, geom=c("point", "smooth"))
qplot(displ, hwy, data=mpg, facets=. ~ drv)

ggplot()

## refactor for plot package
# Single Plots
plot(1:20,1:20,pch=1:20,cex=2)		# marker types

with(pollution, plot(latitude, pm25, col=region))
abline(h = 12, lwd = 2, lty = 2)

stripchart(w1$vals)
stripchart(Xy)
stripchart(Xy$y, method="stack")
stripchart(Xy$y, method="jitter")
stripchart(Xy$y, method="jitter", vertical=TRUE)
stripchart(Xy$y, method="jitter", main='Xy', xlab='y')
title('Leaf BioMass in High CO2 Environment',xlab='BioMass of Leaves')

boxplot(Xy,main='Box plot',ylab='All')

boxplot(pm2_5_annual_df$pm2.5, col="blue")
abline(h=12)

boxplot(pm25 ~ region, data = pollution, col = "red")

hist(w1$vals,main="Distribution of w1",xlab="w1",breaks=12,xlim=c(0,10))

hist(pm2_5_annual_df$pm2.5, col = "green", breaks=20)
abline(v=12, lwd=2)
abline(v=median(pm2_5_annual_df$pm2.5), col="magenta", lwd=4)
rug(pm2_5_annual_df$pm2.5)

barplot(table(pollution$region), col="wheat"
        , main="Number of Counties in Each Region")

# Combination Plots
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))

library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))

hist(Xy$y,main='Combo plot',xlab='y',ylim=c(0,16))
boxplot(Xy$y,horizontal=TRUE,at=16,add=TRUE,axes=FALSE)
stripchart(Xy$y,add=TRUE,at=15)

plot(Auto$cylinders,Auto$mpg)
plot(Auto$cyl,Auto$mpg)
plot(x,y,xlab="Random Uniform",ylab="Random Normal",pch="*",col="blue")
pdf(file="../mpg.pdf")
par(mfrow=c(2,1))
plot(x,y)
hist(y)
par(mfrow=c(1,1))

dev.off()
pairs(Auto,col="brown")
pairs(mpg~cylinders+acceleration+weight,Auto)
pairs(Smarket,col=Smarket$Direction)

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

### Functions
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)
regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)

## Regression
### Linear Regression
fit1=lm(medv~lstat,data=Boston)
fit2=lm(medv~lstat+age,data=Boston)
fit3=lm(medv~.,Boston)
fit4=update(fit3,~.-age-indus)
fit5=lm(medv~lstat*age,Boston)
fit6=lm(medv~lstat +I(lstat^2),Boston); summary(fit6)	# Regular polynomials
fit7=lm(medv~poly(lstat,4))								# Orthogonal polynomials
fita = lm(wage ~ education, data = Wage)
fitb = lm(wage ~ education + age, data = Wage)
fitc = lm(wage ~ education + poly(age, 2), data = Wage)
fitd = lm(wage ~ education + poly(age, 3), data = Wage)
anova(fita, fitb, fitc, fitd)							# Model selection

fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)

fit1
summary(fit1)
fit1.mse = mean(fit1$residuals ^ 2)
abline(fit1,col="red")
names(fit1)
confint(fit1)
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")
points(lstat,fitted(fit6),col="red",pch=20)
points(lstat,fitted(fit7),col="blue",pch=20)

par(mfrow=c(2,2))
plot(fit3)


####plot of the fitted function, along with the standard errors of the fit.
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands = cbind(preds$fit + 2 * preds$se, preds$fit - 2 * preds$se)
plot(age, wage, col = "darkgrey")
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, col = "blue", lty = 2)

plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")
dev.off()
pairs(Auto,col="brown")
pairs(mpg~cylinders+acceleration+weight,Auto)

### Ridge Regression
library(glmnet)
x = model.matrix(Salary ~ . - 1, data = Hitters)
y = Hitters$Salary
fit.ridge = glmnet(x, y, alpha = 0)
plot(fit.ridge, xvar = "lambda", label = TRUE)
cv.ridge = cv.glmnet(x, y, alpha = 0)
plot(cv.ridge)

### Lasso Regression
fit.lasso = glmnet(x, y)
plot(fit.lasso, xvar = "lambda", label = TRUE)
cv.lasso = cv.glmnet(x, y)
plot(cv.lasso)
coef(cv.lasso)
lasso.tr = glmnet(x[train, ], y[train])
lasso.tr
pred = predict(lasso.tr, x[-train, ])
dim(pred)
rmse = sqrt(apply((y[-train] - pred)^2, 2, mean))
plot(log(lasso.tr$lambda), rmse, type = "b", xlab = "Log(lambda)")
lam.best = lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr, s = lam.best)

### Splines
require(splines)

#### Cubic Splines
fit = lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
plot(age, wage, col = "darkgrey")
lines(age.grid, predict(fit, list(age = age.grid)), col = "darkgreen", lwd = 2)
abline(v = c(25, 40, 60), lty = 2, col = "darkgreen")

#### Smoothing splines
fit = smooth.spline(age, wage, df = 16)
fit = smooth.spline(age, wage, cv = TRUE)
lines(fit, col = "red", lwd = 2)

### Generalized Additive Models
require(gam)
gam1 = gam(wage ~ s(age, df = 4) + s(year, df = 4) + education, data = Wage)
par(mfrow = c(1, 3))
plot(gam1, se = T)

### Random Forests
require(randomForest)
require(MASS)
set.seed(101)
dim(Boston)
train = sample(1:nrow(Boston), 300)
rf.boston = randomForest(medv ~ ., data = Boston, subset = train)
oob.err = double(13)
test.err = double(13)
for (mtry in 1:13) {
    fit = randomForest(medv ~ ., data = Boston, subset = train, mtry = mtry, 
        ntree = 400)
    oob.err[mtry] = fit$mse[400]
    pred = predict(fit, Boston[-train, ])
    test.err[mtry] = with(Boston[-train, ], mean((medv - pred)^2))
    cat(mtry, " ")
}
matplot(1:mtry, cbind(test.err, oob.err), pch = 19, col = c("red", "blue"), 
    type = "b", ylab = "Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 19, col = c("red", "blue"))

### Boosting
require(gbm)
boost.boston = gbm(medv ~ ., data = Boston[train, ], distribution = "gaussian", 
    n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(boost.boston)
plot(boost.boston, i = "lstat")
plot(boost.boston, i = "rm")
n.trees = seq(from = 100, to = 10000, by = 100)
predmat = predict(boost.boston, newdata = Boston[-train, ], n.trees = n.trees)
dim(predmat)
berr = with(Boston[-train, ], apply((predmat - medv)^2, 2, mean))
plot(n.trees, berr, pch = 19, ylab = "Mean Squared Error", xlab = "# Trees", 
    main = "Boosting Test Error")
abline(h = min(test.err), col = "red")

## Classification
### Logistic Regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial, subset=train)
fit = glm(I(wage > 250) ~ poly(age, 3), data = Wage, family = binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

preds = predict(fit, list(age = age.grid), se = T)
se.bands = preds$fit + cbind(fit = 0, lower = -2 * preds$se, upper = 2 * preds$se)
se.bands[1:5, ]
prob.bands = exp(se.bands)/(1 + exp(se.bands))
matplot(age.grid, prob.bands, col = "blue", lwd = c(2, 1, 1), lty = c(1, 2, 
    2), type = "l", ylim = c(0, 0.1))
points(jitter(age), I(wage > 250)/10, pch = "|", cex = 0.5)

### Generalized Additive Models
gam2 = gam(I(wage > 250) ~ s(age, df = 4) + s(year, df = 4) + education, data = Wage, 
    family = binomial)
gam2a = gam(I(wage > 250) ~ s(age, df = 4) + year + education, data = Wage, 
    family = binomial)
anova(gam2a, gam2, test = "Chisq")
par(mfrow = c(1, 3))
lm1 = lm(wage ~ ns(age, df = 4) + ns(year, df = 4) + education, data = Wage)
plot.gam(lm1, se = T)    
plot(gam2)

### Linear Discriminant Analysis
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket, subset=Year<2005)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit,Smarket.2005)
lda.pred[1:5,]
class(lda.pred)
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

### K-Nearest Neighbors
library(class)
attach(Smarket)
Xlag=cbind(Lag1,Lag2)
train=Year<2005
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])

### Decision Trees
require(ISLR)
require(tree)
attach(Carseats)
hist(Sales)
High = ifelse(Sales <= 8, "No", "Yes")
Carseats = data.frame(Carseats, High)
tree.carseats = tree(High ~ . - Sales, data = Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
set.seed(1011)
train = sample(1:nrow(Carseats), 250)
tree.carseats = tree(High ~ . - Sales, Carseats, subset = train)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.pred = predict(tree.carseats, Carseats[-train, ], type = "class")
with(Carseats[-train, ], table(tree.pred, High))
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
plot(cv.carseats)
prune.carseats = prune.misclass(tree.carseats, best = 13)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred = predict(prune.carseats, Carseats[-train, ], type = "class")
with(Carseats[-train, ], table(tree.pred, High))

### SVM
library(e1071)

### SVM_Linear
set.seed(10111)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1, ] = x[y == 1, ] + 1
plot(x, col = y + 3, pch = 19)

dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
plot(svmfit, dat)
make.grid = function(x, n = 75) {
    grange = apply(x, 2, range)
    x1 = seq(from = grange[1, 1], to = grange[2, 1], length = n)
    x2 = seq(from = grange[1, 2], to = grange[2, 2], length = n)
    expand.grid(X1 = x1, X2 = x2)
}
xgrid = make.grid(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = 0.2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index, ], pch = 5, cex = 2)
beta = drop(t(svmfit$coefs) %*% x[svmfit$index, ])
beta0 = svmfit$rho
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = 0.2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index, ], pch = 5, cex = 2)
abline(beta0/beta[2], -beta[1]/beta[2])
abline((beta0 - 1)/beta[2], -beta[1]/beta[2], lty = 2)
abline((beta0 + 1)/beta[2], -beta[1]/beta[2], lty = 2)

predict_errors = rep(0, 1000)
for (run_ix in 1:1000) {
	sample_0 = mvrnorm(n=50, mu=rep(0,10), Sigma=diag(10))
	sample_1 = mvrnorm(n=50, mu=c(rep(1,5),rep(0,5)), Sigma=diag(10))
	sample_X = rbind(sample_0, sample_1)
	train_df = data.frame(sample_X)
	sample_y = c(rep(0,50), rep(1,50))
	train_df$y = as.factor(sample_y)
	#plot(train_df$X1, train_df$X2, col = as.numeric(train_df$y) + 2, pch = 19)
	radial.svm.fit = svm(y ~ ., data = train_df, kernel = "linear", cost = 10, scale = FALSE)

	sample_0 = mvrnorm(n=500, mu=rep(0,10), Sigma=diag(10))
	sample_1 = mvrnorm(n=500, mu=c(rep(1,5),rep(0,5)), Sigma=diag(10))
	sample_X = rbind(sample_0, sample_1)
	test_df = data.frame(sample_X)
	sample_y = c(rep(0,500), rep(1,500))
	test_df$y = as.factor(sample_y)	
	test_df$predict_y = predict(radial.svm.fit, sample_X)
	predict_errors [run_ix] = sum(test_df$y != test_df$predict_y) 
}
mean(predict_errors) / 1000 


### SVM_Radial
load(url("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/ESL.mixture.rda"))
names(ESL.mixture)
rm(x, y)
attach(ESL.mixture)
plot(x, col = y + 1)
dat = data.frame(y = factor(y), x)
fit = svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "radial", cost = 5)
xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(fit, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.2)
points(x, col = y + 1, pch = 19)
func = predict(fit, xgrid, decision.values = TRUE)
func = attributes(func)$decision
xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(fit, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.2)
points(x, col = y + 1, pch = 19)
contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
contour(px1, px2, matrix(prob, 69, 99), level = 0.5, add = TRUE, col = "blue", 
    lwd = 2)

sample_0 = mvrnorm(n=50, mu=rep(0,10), Sigma=diag(10))
sample_1 = mvrnorm(n=50, mu=c(rep(1,5),rep(0,5)), Sigma=diag(10))
sample_X = rbind(sample_0, sample_1)
train_df = data.frame(sample_X)
sample_y = c(rep(0,50), rep(1,50))
train_df$y = as.factor(sample_y)
plot(train_df$X1, train_df$X2, col = as.numeric(train_df$y) + 2, pch = 19)
radial.svm.fit = svm(y ~ ., data = train_df, kernel = "radial", cost = 10, scale = FALSE)
summary(radial.svm.fit)

# This should work; but doesn't
make.grid = function(df_x, n = 75) {
    grange = apply(df_x[c('X1', 'X2')], 2, range)
    x1 = seq(from = grange[1, 1], to = grange[2, 1], length = n)
    x2 = seq(from = grange[1, 2], to = grange[2, 2], length = n)
    x12.grid = expand.grid(X1 = x1, X2 = x2)
    x12.grid$X3 = rep(mean(df_x$X3), n)
    x12.grid$X4 = rep(mean(df_x$X4), n)
    x12.grid$X5 = rep(mean(df_x$X5), n)
    x12.grid$X6 = rep(mean(df_x$X6), n)
    x12.grid$X7 = rep(mean(df_x$X7), n)
    x12.grid$X8 = rep(mean(df_x$X8), n)
    x12.grid$X9 = rep(mean(df_x$X9), n)
    x12.grid$X10 = rep(mean(df_x$X10), n)
    return(list(x12.grid, x1, x2))
}
list[x12.grid, x12.x1, x12.x2] = make.grid(train_df)

make.grid = function(df_x, n = 75) {
    grange = apply(df_x[c('X1', 'X2')], 2, range)
    x1 = seq(from = grange[1, 1], to = grange[2, 1], length = n)
    x2 = seq(from = grange[1, 2], to = grange[2, 2], length = n)
    x12.grid = expand.grid(X1 = x1, X2 = x2)
    x12.grid$X3 = rep(mean(df_x$X3), n)
    x12.grid$X4 = rep(mean(df_x$X4), n)
    x12.grid$X5 = rep(mean(df_x$X5), n)
    x12.grid$X6 = rep(mean(df_x$X6), n)
    x12.grid$X7 = rep(mean(df_x$X7), n)
    x12.grid$X8 = rep(mean(df_x$X8), n)
    x12.grid$X9 = rep(mean(df_x$X9), n)
    x12.grid$X10 = rep(mean(df_x$X10), n)
    return(x12.grid)
}
x12.grid = make.grid(train_df)
get.grid.x1 = function(df_x, n = 75) {
    grange = apply(df_x[c('X1', 'X2')], 2, range)
    x1 = seq(from = grange[1, 1], to = grange[2, 1], length = n)
    x2 = seq(from = grange[1, 2], to = grange[2, 2], length = n)
    x12.grid = expand.grid(X1 = x1, X2 = x2)
    x12.grid$X3 = rep(mean(df_x$X3), n)
    x12.grid$X4 = rep(mean(df_x$X4), n)
    x12.grid$X5 = rep(mean(df_x$X5), n)
    x12.grid$X6 = rep(mean(df_x$X6), n)
    x12.grid$X7 = rep(mean(df_x$X7), n)
    x12.grid$X8 = rep(mean(df_x$X8), n)
    x12.grid$X9 = rep(mean(df_x$X9), n)
    x12.grid$X10 = rep(mean(df_x$X10), n)
    return(x1)
}
x12.x1 = get.grid.x1(train_df)
get.grid.x2 = function(df_x, n = 75) {
    grange = apply(df_x[c('X1', 'X2')], 2, range)
    x1 = seq(from = grange[1, 1], to = grange[2, 1], length = n)
    x2 = seq(from = grange[1, 2], to = grange[2, 2], length = n)
    x12.grid = expand.grid(X1 = x1, X2 = x2)
    x12.grid$X3 = rep(mean(df_x$X3), n)
    x12.grid$X4 = rep(mean(df_x$X4), n)
    x12.grid$X5 = rep(mean(df_x$X5), n)
    x12.grid$X6 = rep(mean(df_x$X6), n)
    x12.grid$X7 = rep(mean(df_x$X7), n)
    x12.grid$X8 = rep(mean(df_x$X8), n)
    x12.grid$X9 = rep(mean(df_x$X9), n)
    x12.grid$X10 = rep(mean(df_x$X10), n)
    return(x2)
}
x12.x2 = get.grid.x2(train_df)

y.grid = predict(radial.svm.fit, x12.grid)
plot(x12.grid$X1, x12.grid$X2, col = c("green", "blue")[as.numeric(y.grid)], pch = 20, cex = 0.2)
points(train_df$X1, train_df$X2, col = as.numeric(train_df$y) + 2, pch = 19)
points(train_df$X1[radial.svm.fit$index], train_df$X2[radial.svm.fit$index], pch = 5, cex = 2)
radial.svm.func = attributes(predict(radial.svm.fit, x12.grid, decision.values = TRUE))$decision
contour(x12.x1, x12.x2, matrix(radial.svm.func, 75, 75), level = 0, add = TRUE)
#contour(x12.x1, x12.x2, matrix(prob, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)

train_df$predict_y = predict(radial.svm.fit, sample_X)
predict_error = sum(train_df$y != train_df$predict_y)  

predict_errors = rep(0, 1000)
for (run_ix in 1:1000) {
	sample_0 = mvrnorm(n=50, mu=rep(0,10), Sigma=diag(10))
	sample_1 = mvrnorm(n=50, mu=c(rep(1,5),rep(0,5)), Sigma=diag(10))
	sample_X = rbind(sample_0, sample_1)
	train_df = data.frame(sample_X)
	sample_y = c(rep(0,50), rep(1,50))
	train_df$y = as.factor(sample_y)
	#plot(train_df$X1, train_df$X2, col = as.numeric(train_df$y) + 2, pch = 19)
	radial.svm.fit = svm(y ~ ., data = train_df, kernel = "radial", cost = 10, scale = FALSE)

	sample_0 = mvrnorm(n=500, mu=rep(0,10), Sigma=diag(10))
	sample_1 = mvrnorm(n=500, mu=c(rep(1,5),rep(0,5)), Sigma=diag(10))
	sample_X = rbind(sample_0, sample_1)
	test_df = data.frame(sample_X)
	sample_y = c(rep(0,500), rep(1,500))
	test_df$y = as.factor(sample_y)	
	test_df$predict_y = predict(radial.svm.fit, sample_X)
	predict_errors [run_ix] = sum(test_df$y != test_df$predict_y) 
}
mean(predict_errors) / 1000 

### SVM_Logisitic
predict_errors = rep(0, 1000)
for (run_ix in 1:1000) {
	sample_0 = mvrnorm(n=50, mu=rep(0,10), Sigma=diag(10))
	sample_1 = mvrnorm(n=50, mu=c(rep(1,5),rep(0,5)), Sigma=diag(10))
	sample_X = rbind(sample_0, sample_1)
	train_df = data.frame(sample_X)
	sample_y = c(rep(0,50), rep(1,50))
	train_df$y = as.factor(sample_y)
	#plot(train_df$X1, train_df$X2, col = as.numeric(train_df$y) + 2, pch = 19)
	radial.svm.fit = svm(y ~ ., data = train_df, kernel = "sigmoid", cost = 10, scale = FALSE)

	sample_0 = mvrnorm(n=500, mu=rep(0,10), Sigma=diag(10))
	sample_1 = mvrnorm(n=500, mu=c(rep(1,5),rep(0,5)), Sigma=diag(10))
	sample_X = rbind(sample_0, sample_1)
	test_df = data.frame(sample_X)
	sample_y = c(rep(0,500), rep(1,500))
	test_df$y = as.factor(sample_y)	
	test_df$predict_y = predict(radial.svm.fit, sample_X)
	predict_errors [run_ix] = sum(test_df$y != test_df$predict_y) 
}
mean(predict_errors) / 1000 

## Clustering
### K-means
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)
kmeansObj$cluster
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n")

set.seed(101)
x = matrix(rnorm(100 * 2), 100, 2)
xmean = matrix(rnorm(8, sd = 4), 4, 2)
which = sample(1:4, 100, replace = TRUE)
x = x + xmean[which, ]
plot(x, col = which, pch = 19)
km.out = kmeans(x, 4, nstart = 15)
km.out
plot(x, col = km.out$cluster, cex = 2, pch = 1, lwd = 2)
points(x, col = which, pch = 19)
points(x, col = c(4, 3, 2, 1)[which], pch = 19)

### Hierarchial Clustering
set.seed(1234)
#par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)
hClusters <- cutree(hClustering, k=3)
myplclust <- function(hclust, lab = hclust$labels, lab.col = rep(1, length(hclust$labels)),
                      hang = 0.1, ...) {
    ## modifiction of plclust for plotting hclust objects *in colour*! Copyright
    ## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
    ## of labels of the leaves of the tree lab.col: colour for the labels;
    ## NA=default device foreground colour hang: as in hclust & plclust Side
    ## effect: A display of hierarchical cluster with coloured leaf labels.
    y <- rep(hclust$height, 2)
    x <- as.numeric(hclust$merge)
    y <- y[which(x < 0)]
    x <- x[which(x < 0)]
    x <- abs(x)
    y <- y[order(x)]
    x <- x[order(x)]
    plot(hclust, labels = FALSE, hang = hang, ...)
    text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order],
         col = lab.col[hclust$order], srt = 90, adj = c(1, 0.5), xpd = NA, ...)
}
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))
set.seed(143)
dataMatrix <- as.matrix(dataFrame)
dataMatrix2 <- as.matrix(dataFrame)[sample(1:12), ]
heatmap(dataMatrix2)

hc.complete = hclust(dist(x), method = "complete")
plot(hc.complete)
hc.single = hclust(dist(x), method = "single")
plot(hc.single)
hc.average = hclust(dist(x), method = "average")
plot(hc.average)
hc.cut = cutree(hc.complete, 4)
table(hc.cut, which)
table(hc.cut, km.out$cluster)
plot(hc.complete, labels = which)

## Modeling Techniques
### Best Subset
library(leaps)
regfit.full = regsubsets(Salary ~ ., data = Hitters, nvmax=19)
reg.summary = summary(regfit.full)
names(reg.summary)
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], pch = 20, col = "red")
plot(regfit.full, scale = "Cp")
coef(regfit.full, 10)

### Forward Stepwise Selection
regfit.fwd = regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
plot(regfit.fwd, scale = "Cp")

### Model Selection Using Validation Set
dim(Hitters)
set.seed(1)
train = sample(seq(263), 180, replace = FALSE)
train
regfit.fwd = regsubsets(Salary ~ ., data = Hitters[train, ], nvmax = 19, method = "forward")
val.errors = rep(NA, 19)
x.test = model.matrix(Salary ~ ., data = Hitters[-train, ])  # notice the -index!
for (i in 1:19) {
    coefi = coef(regfit.fwd, id = i)
    pred = x.test[, names(coefi)] %*% coefi
    val.errors[i] = mean((Hitters$Salary[-train] - pred)^2)
}
plot(sqrt(val.errors), ylab = "Root MSE", ylim = c(300, 400), pch = 19, type = "b")
points(sqrt(regfit.fwd$rss[-1]/180), col = "blue", pch = 19, type = "b")
legend("topright", legend = c("Training", "Validation"), col = c("blue", "black"), 
    pch = 19)
predict.regsubsets = function(object, newdata, id, ...) {
    form = as.formula(object$call[[2]])
    mat = model.matrix(form, newdata)
    coefi = coef(object, id = id)
    mat[, names(coefi)] %*% coefi
}

### Model Selection Using Cross-Validation
set.seed(11)
folds = sample(rep(1:10, length = nrow(Hitters)))
folds
table(folds)
cv.errors = matrix(NA, 10, 19)
for (k in 1:10) {
    best.fit = regsubsets(Salary ~ ., data = Hitters[folds != k, ], nvmax = 19, 
        method = "forward")
    for (i in 1:19) {
        pred = predict(best.fit, Hitters[folds == k, ], id = i)
        cv.errors[k, i] = mean((Hitters$Salary[folds == k] - pred)^2)
    }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch = 19, type = "b")    

# Data Exportation(?)
write.table(tidy_Df, "UCIHAR_tidy.txt", row.names=FALSE)

# Session Termination
q()


