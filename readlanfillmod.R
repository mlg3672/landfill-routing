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

# erase duplicated rows
dat<-dat[!duplicated(dat),]

# import Alaska data
AKcases<-read.csv("AK_landfills.csv",colClasses = "character")
summary(AKcases)
AKcases<-AKcases[c(1:18),c(2,7:12)]
colnames(AKcases)<- c("name","city","county","state","zip","lon","lat")

dat<-dat[!grepl("AK",dat$state),] # delete Alaska from original dataframe

dat<-rbind(AKcases[,2:7],dat) # merge dataframes

## select all cases by location
unique(dat$state)
NYcases<-dat[grepl("NY",dat$state),]
head(NYcases)
# OR split(df, list(df$state), drop = TRUE) # split df by state abbrev

# add region code column -----
dat$region<-rep("000",dim(dat)[1])
datos$region<-rep("000",dim(datos)[1])
## code 001 for west coast 
t0<-data.frame(c("HI","AR","OR","WA","ID","LA","WY","MO",
                 "AZ","NM","TX","OK","KS","NV","UT","CO",
                 "CA","NE","MT","AK"))

dat<-regioncode(t0,"001",dat)
datos<-regioncode(t0,"001",datos) 
## code 002 for midwest 
t0<-data.frame(c("IA", "IL", "CT","SD", "NY", "NJ", "IN", 
                 "WI", "WV","ND", "MN", "MI", "KY", "OH", 
                 "PA", "TN", "MS"))
dat<-regioncode(t0,"002",dat)
datos<-regioncode(t0,"002",datos) 

## code 003 for south
t0<-data.frame(c("RI", "GA", "SC", "TN", "MS","AL", "SC", "FL", "NC","MA"))
dat<-regioncode(t0,"003",dat)
datos<-regioncode(t0,"003",datos) 

## code 004 for mid-atlantic 
t0<-data.frame(c("DC","VA","DE","MD"))
dat<-regioncode(t0,"004",dat)
datos<-regioncode(t0,"004",datos) 

## code 005 for new england
t0<-data.frame(c("VT","NH","ME"))
dat<-regioncode(t0,"005",dat)
datos<-regioncode(t0,"005",datos) 

regioncode<-function(t0,code,tab){
  t1<-as.list(t(t0))
  t2<-paste(t1,collapse = "|")
  tab[grepl(t2,tab$state),]$region<-code
  return(tab)
}

##  find lat lon -----------
library(ggmap)
library(plyr)
wozcity<-dat[NAzip & NAcity,] #find rows with only state value -1824
wzip<-dat[!NAzip & NAcity,]#find rows with zip but no city value - 109
wcity<-dat[!NAcity & NAzip,]# find rows with city value but no zip - 656
wzcity<-dat[!NAzip & !NAcity,]# find rows with city and zip - 946
summary(wcity)
summary(wzip)
summary(wzcity)

#geocode -----
gisdf11 <-geocode(paste(wzip$state, wzip$zip,"USA",sep=","))
gisdf2 <-geocode(paste(c("landfill"),wcity$city, wcity$state,"USA",sep=","))
gisdf3 <-geocode(paste(c("landfill"),wzcity$city,wzcity$state, wzcity$zip,"USA",sep=","))

# bind columns of lat lon
wzipgis<-cbind(wzip,gisdf1)
wcitygis<-cbind(wcity,gisdf2)
wzcitygis<-cbind(wzcity,gisdf3)

geto<-rbind(wzipgis, wcitygis) # bind all dataframes

#plot lat lon pairs -----
XYpairs <- data.frame(lon=geto$lon,lat=geto$lat,region=geto$region, state=geto$state)
plot(XYpairs)
dim(XYpairs)

Ypairs <-data.frame(lon=datos$lon,lat=datos$lat, size=datos$size, 
                    state=datos$state, stringsAsFactors = F)
# test set
YYpairs<-Ypairs[1:5,]
XYYpairs<-XYpairs[1:5,]

#split pairs by region 
region1s<-datos[grepl("001",datos$region),]
region1t<-dat[grepl("001",dat$region),]

# find distance between points -----
routes<-data.frame(from.lon=NA,from.lat=NA,to.lon=NA,to.lat=NA,distance=NA,size=NA)
for (y in 1:dim(YYpairs)[1]){
  from = as.numeric(YYpairs[y,1:2])
  for (x in 1:dim(XYYpairs)[1]) {
    to = as.numeric(XYYpairs[x,])
    distance <- mapdist(from, to, mode="driving", output="simple")
    newrow<-data.frame(from.lon = from[1],from.lat = from[2],
                       to.lon = to[1], to.lat = to[2], 
                       distance = distance$miles, size=YYpairs[y,"size"], from.state, to.state)
    routes<-rbind(newrow, routes)
    
    }
  }


# write data to csv -----
write.csv(x=routes,file="distance_lf.csv")
