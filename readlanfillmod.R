# the objective of this code is to read and get lat lon data from file of locations
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
dat<-read.csv("uslandfill_mod.csv", colClasses="character")# reads destination  points to df - best option !
dato<-read.csv("openpv.csv", colClasses= "character") # read start points to df

# clean data ----------------
## set column names
colnames(dat)<- c("name","city","county","state","zip")
dats<-dat[,1:5]
dim(dats)

## subset data
datos<-dato[,c(2:3,6:8)]
colnames(datos)<-c("state","size","lat","lon","zip")
dim(datos)

## check that all zip codes have 5 digits
dat[nchar(dat[,5])!=5,]#MA has 4 digits - fixed!

## find NA values
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

datos<-datos[datos$state!="PR",]
unique(datos$state)

# import Alaska data
AKcases<-read.csv("AK_landfills.csv",colClasses = "character")
summary(AKcases)
AKcases<-AKcases[c(1:18),c(2,7:12)]
colnames(AKcases)<- c("name","city","county","state","zip","lat","lon")

## select all cases by location
unique(dat$state)
NYcases<-dat[grepl("NY",dat$state),]
head(NYcases)
# OR split(df, list(df$state), drop = TRUE) # split df by state abbrev

##  find lat lon -----------
library(ggmap)
library(plyr)
wzip<-dat[!NAzip & NAcity,]#find rows with zip but no city value
wcity<-dat[!NAcity & NAzip,]# find rows with city value but no zip - 3100
wzcity<-dat[!NAzip & !NAcity,]# find rows with city and zip - 1055
summary(wcity)
summary(wzip)
summary(wzcity)

#geocode
gisdf11 <-geocode(paste(wzip$state, wzip$zip,"USA",sep=","))
gisdf2 <-geocode(paste(c("landfill"),wcity$city, wcity$state,"USA",sep=","))
gisdf3 <-geocode(paste(c("landfill"),wzcity$city,wzcity$state, wzcity$zip,"USA",sep=","))

# bind columns of lat lon
wzipgis<-cbind(gisdf1,wzip)
wcitygis<-cbind(gisdf2,wcity)
wzcitygis<-cbind(gisdf3,wzcity)

getto<-rbind(wzipgis, wcitygis,wzcitygis) # bind all dataframes

# add region code column
NYcases$region<-rep("000",dim(NYcases)[1])
## code 001 for west coast states
t0<-data.frame(c("HI","AK","OR","WA","ID","LA","WY","MO",
                 "AZ","NM","TX","OK","KS","NV","UT","CO",
                 "CA","NB"))
t1<-as.list(t(t0))
t2<-paste(t1,collapse = "|")
NYcases[grepl(t2,NYcases$county),]$region<-"002"
## code 002 for 
t0<-data.frame(c("IO", "IL", "CT","SD", "NY", "NJ", "IN", 
                 "WI", "WV"," ND", "MN", "MI", "KY", "OH", 
                 "PA", "TN", "MS"))
## code 003 for 
t0<-data.frame(c("RI", "GA", "SC", "TN", "MS","AL", "SC", "FL", "NC"))
## code 004 for DC, VA, DE, MD
t0<-data.frame(c("DC","VA","DE","MD"))
## code 005 for VT, NH, ME
t0<-data.frame(c("VT","NH","ME"))

#plot lat lon pairs
XYpairs <- data.frame(lon=wcitygis$lon,lat=wcitygis$lat)
plot(XYpairs)
dim(XYpairs)

Ypairs <-data.frame(lon=datos$lon,lat=datos$lat, size=datos$size, stringsAsFactors = F)


# find distance between points
routes<-data.frame(distance=rep(0,dim(XYpairs)[1]))
for (x in 1:dim(XYpairs)[1]) {
  from=as.numeric(Ypairs[x,1:2])
  to=as.numeric(XYpairs[x,])
  distance <- mapdist(from, to, mode="driving", output="simple")
  routes$from.lon[x]<-from[1]
  routes$from.lat[x]<-from[2]
  routes$to.lon[x]<-to[1]
  routes$to.lat[x]<-to[2]
  routes$distance[x]<-distance$miles
}


# write data to csv
write.csv(x=routes,file="distance_lf.csv")
