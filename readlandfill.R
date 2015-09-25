library(stringr)
library(data.table)
# set working directory
setwd("~/Documents/Python-Projects/es-and-t")

# the objective of this code is to read and clean landfill txt or table file
txt <- readLines("uslandfills.txt") # this works
header<-txt[1]
tab<-read.table("uslandfills.txt", fill=T) # misread  incomplete columns
datl<-fread("uslandfills.txt", sep=" ") # didnt process due to error on line 111

# counts number of data selects out incomplete data
c.fields = count.fields('uslandfills.txt')
tab2 = tab[ - which(c.fields != max(c.fields)),]

# use after readLines
post<-do.call(rbind, strsplit(txt, " ")) # does not work missing columns

lapply(dat,function(x) strsplit(x,"\\s.+"))

# split string by capital letters
strsplit(x,"\\s.+") # yields only first number
strsplit(dat[99],"[A-Z]")
strsplit(x,"^[A-Z].+")
# detect empty lines and delete
dat <- txt[(sapply(txt, nchar) > 0)]

# get rid of all whitespace
dats<-gsub("\\s", "", dat)

x<-dat[1100]
#find capital words
i<-grepl("\b[A-Z].*\b",x)
i<-grep("^[A-Z]",x)
i<-grepl("[0-7]",x)
# pull out numerical two digit
s<- strsplit(dat, "[^[:digit:]]")
solution<- as.numeric(unlist(s))
ssolution <- unique(solution[!is.na(solution)])

assignFields <- function(x) {
  #id name city county state zip
  out <- character(6)
  
  # get two character state
  i <- grep("(?!^.*[A-Z]{2,}.*$)^[A-Za-z]*$",x)
  i <- grep("^[A-Z]([a-z][A-Z]?)+$",x)
 x[i]
  # regular expression explaination
  #^
  #  [A-Z]    // Start with an uppercase Letter
  #(        // A Group of:
  #  [a-z]  // mandatory lowercase letter
  #[A-Z]? // an optional Uppercase Letter at the end
  #// or in between lowercase letters
  #)+       // This group at least one time
  #$
  
  out[5] <- x[i]
  # get numerical zip
  i <- which(as.numeric(x) < 999)
  out[6] <- ifelse(length(i)>0, x[i], NA)
  # get numerial id
  i <- which(as.numeric(x) < 999)
  out[1] <- ifelse(length(i)>0, x[i], NA)
  # get landfill name, city, county 
  # separate
  i <- grepl("[[:alpha:]]",x)
  i <- grepl("[[:upper:]]",x)
  
  return(out)
}

assignFields(dat[-1])