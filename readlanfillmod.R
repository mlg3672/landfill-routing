# the objective of this code is to read and get lat lon data from landfill table file
library(stringr)
library(data.table)
library(plyr)
# set working directory
setwd("~/Documents/Python-Projects/landfill-routing")

# read data ---------------------
txt <- readLines("uslandfill_mod.csv") # reads each line as txt string - not helpful
header<-txt[1] # gets header
tab<-read.table("uslandfill_mod.csv", fill=T) # misread  incomplete columns
datl<-fread("uslandfill_mod.csv", sep=",") # worked put values in table lumps txt 
dat<-read.csv("uslandfill_mod.csv", colClasses="character")# reads to df - best option !

# clean data ----------------
## set column names
colnames(dat)<- c("name","city","county","state","zip")
dats<-dat[,1:5]
dim(dats)

## check that all zip codes have 5 digits
dat[nchar(dat[,5])!=5,]#MA has 4 digits - fixed!
NAzip<-dat[is.na(dat[,5])==T,]#2493
NAstate<-dat[is.na(dat[,4])==T,]#0
NAcity<-dat[is.na(dat[,2])==T,]#1939
NAcounty<-dat[is.na(dat[,3])==T,] #476 AK only state with no city,zip or county - fixed!

# delete territories Puerto Rico "PR", Virgin Islands "VI", Guam "GU", "MP", "AS"
dat<-dat[dat$state!="PR",]
dat<-dat[dat$state!="VI",]
dat<-dat[dat$state!="GU",]
dat<-dat[dat$state!="MP",]
dat<-dat[dat$state!="AS",]
unique(dat$state)

split(df, list(df$state), drop = TRUE) # split df by state abbrev

dat[-dat$state=="VI",]
## select all cases by location
unique(dat$state)
MAcases<-dat[grepl("MA",dat$state),]
head(MAcases)

# counts number of data selects out incomplete data
c.fields = count.fields('uslandfill_mod.csv')
tab2 = tab[ - which(c.fields != max(c.fields)),] # correctly identies 1 obv. missing