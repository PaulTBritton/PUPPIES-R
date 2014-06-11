#
# Collection of functions to match wildcard patterns
# and filter data from data frames
#
# Author: Paul Thomas Britton
#
###################################

# advanced glob pattern to regular expression converter
aglob2rx <- function(pattern,trim.head=TRUE,trim.tail=TRUE) {
    p <- gsub("\\.", "\\\\.", paste("^", pattern, "$",sep=""))
    p <- gsub("\\?", ".", gsub("\\*", ".*", p))
#    p <- gsub("([^\\])\\(", "\\1\\\\(", p)
#    p <- gsub("([^\\])\\[", "\\1\\\\[", p)
    p <- gsub("([^\\])\\{", "\\1\\\\{", p)
    if (trim.tail) 
        p <- sub("\\.\\*\\$$", "", p)
    if (trim.head) 
        p <- sub("\\^\\.\\*", "", p)
    return(p)
}

# define a generic function torx that takes different wildcardclasses and
# converts them to regular expressions
torx <- function(pattern) UseMethod("torx")
torx.aglob <- function(pattern) aglob2rx(pattern)
torx.glob <- function(pattern) glob2rx(pattern)
torx.default <- function(pattern) pattern

# trim white space characters from a string
trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x) sub("\\s+$", "", x)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


# return a subset of data based on a pass through filter in
# regex format
filterdata <- function(filter,data) data[grep(filter,names(data))]

