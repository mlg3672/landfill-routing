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
NAzip<-is.na(dat$zip)#2493
NAstate<-is.na(dat$state)#0
NAcity<-is.na(dat$city)#1939
NAcounty<-is.na(dat$county) #476 AK only state with no city,zip or county - fixed!

# delete territories Puerto Rico "PR", Virgin Islands "VI", Guam "GU", "MP", "AS"
dat<-dat[dat$state!="PR",]
dat<-dat[dat$state!="VI",]
dat<-dat[dat$state!="GU",]
dat<-dat[dat$state!="MP",]
dat<-dat[dat$state!="AS",]
unique(dat$state)

# import Alaska data
AKcases<-read.csv("AK_landfills.csv",colClasses = "character")
summary(AKcases)
AKcases<-AKcases[c(1:18),c(2,7:12)]
colnames(AKcases)<- c("name","city","county","state","zip","lat","lon")

## select all cases by location
unique(dat$state)
MAcases<-dat[grepl("MA",dat$state),]
head(MAcases)
# OR split(df, list(df$state), drop = TRUE) # split df by state abbrev

##  find lat lon -----------
library(ggmap)
library(plyr)
wzip<-dat[!NAzip & NAcity,]#find rows with zip but no city value
wcity<-dat[!NAcity & NAzip,]# find rows with city value but no zip - 3100
wzcity<-dat[!NAzip & !NAcity,]# find rows with city and zip - 1055
summary(wcity)
summary(wzip)

gisdf1 <-geocode(paste(c("landfill"),wzip$state, wzip$zip,"US",sep=","))
gisdf2 <-geocode(paste(c("landfill"),wcity$city, wcity$state,"US",sep=","))
gisdf3 <-geocode(paste(c("landfill"),wzcity$city,wzcity$state, wzcity$zip,"US",sep=","))


