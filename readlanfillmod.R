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

# import Alaska data -----
AKcases<-read.csv("AK_landfills.csv",colClasses = "character")
summary(AKcases)
AKcases<-AKcases[c(1:18),c(2,7:12)]
colnames(AKcases)<- c("name","city","county","state","zip","lon","lat")

dat<-dat[!grepl("AK",dat$state),] # delete Alaska from original dataframe

dat<-rbind(AKcases[,2:7],dat) # merge dataframes

#import WV data -----
WVcases<-read.csv("WV_landfills.csv",colClasses = "character")
summary(WVcases)
WVgis<-geocode(paste(WVcases$address, "USA",sep=","))
WVcases<-cbind(WVcases, WVgis)

dat<-dat[!grepl("WV",dat$state),] # delete WV from original dataframe

dat<-rbind(WVcases[,2:7],dat) # merge dataframes - doesnt work at lon lat cols to dat

#import IL data -----
ILcases<-read.csv("IL_landfills.csv",colClasses = "character")
summary(ILcases)
ILcases<-ILcases[-40,]
ILgis<-geocode(paste(ILcases$address, ILcases$city, ILcases$state, "USA",sep=","))
ILcases<-cbind(ILcases,ILgis)

# insert zip column
ILcases<-data.frame(city = ILcases$city, county = ILcases$county,
                    state = ILcases$state, zip=rep(NA,dim(ILcases)[1]), 
                    lon = ILcases$lon, lat = ILcases$lat)
dat<-dat[!grepl("IL",dat$state),] # delete IL from original dataframe

dat<-rbind(ILcases[,2:6],dat) # merge dataframes

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
wnone<-dat[NAzip & NAcity & NAcounty,]# 3 states IL, WV, and AK
wcounty<-dat[NAzip & NAcity & !NAcounty,] #find rows with only state value -1596
wzip<-dat[!NAzip & NAcity,]#find rows with zip but no city value - 109
wcity<-dat[!NAcity & NAzip,]# find rows with city value but no zip - 656
wzcity<-dat[!NAzip & !NAcity,]# find rows with city and zip - 946

#geocode -----
gisdf0<-geocode(paste(c("landfill"),wcounty$county, wcounty$state,"USA",sep=","))
gisdf1 <-geocode(paste(wzip$state, wzip$zip,"USA",sep=","))
gisdf2 <-geocode(paste(c("landfill"),wcity$city, wcity$state,"USA",sep=","))
gisdf3 <-geocode(paste(c("landfill"),wzcity$city,wzcity$state, wzcity$zip,"USA",sep=","))

# bind columns of lat lon
wcountygis<-cbind(wcounty,gisdf0)
wzipgis<-cbind(wzip,gisdf1)
wcitygis<-cbind(wcity,gisdf2)
wzcitygis<-cbind(wzcity,gisdf3)

geto<-rbind(wzipgis, wcitygis,wzcitygis,wcountygis) # bind all dataframes

#plot lat lon pairs -----
XYpairs <- data.frame(lon=geto$lon,lat=geto$lat, state=geto$state, region=geto$region)
plot(XYpairs$lon, XYpairs$lat)
dim(XYpairs)

Ypairs <-data.frame(lon=datos$lon,lat=datos$lat, size=datos$size, 
                    state=datos$state, region=datos$region, stringsAsFactors = F)
# test set
YYpairs<-Ypairs[1:1000,]
XYYpairs<-XYpairs[1:1000,]

#split pairs by region 
XYYpairs<-XYpairs[grepl("001",XYpairs$region),]
YYpairs<-Ypairs[grepl("001",Ypairs$region),]

## reduce the number of points by elimination --------
# due to limiation of gmap to 2000 queries 

# find nearest LF point, eliminate most frequent
require(geosphere)
p1<-XYpairs[1,1:2]
p2<-XYpairs[2,1:2]
distGeo(p1,p2)# error could not find function

all<-distm(XYpairs[1:2,1:2]) # produces matrix of as the crow flies distances
NAall<-is.na(all[1,]) # if use which.min not necessary
NAall[1]<-T # if use which.min not necessary

Nearest<-function(df) {
  # takes a data frame with x-lon and y-lat as first two columns
  # finds the points at the nearest distance assuming planar
  df$nearest<-NA
  df$y.near<-NA
  matrix<-distm(df[,1:2]) # use to produce distance matrix
  for (x in 1:dim(df)[1]) {
    index<-which.min(matrix[x,-x]) # finds min index
    if (!is.null(index)){
      if (x <= index){
        index<-index+1
        df$y.near[x]<-index
        }
      else {
        df$y.near[x]<-index
        }
      df$nearest[x]<-matrix[x,index]
      }
    }
  return(df)
}
Nearest(XYYpairs[1:30,1:2])
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

ReducePoints<-function(df,x){
  # find the most popular points and delete
  df<-df[!is.na(df$lon),]
  while (dim(df)[1] > x){
    df<-Nearest(df)
    m<-Mode(df$y.near)
    df<-df[-m,]
    plot(df$lon,df$lat)
  }
  return(df)
}

ReducePoints2<-function(df,x){
  #find the x most remote points
  df<-df[!is.na(df$lon),]
  df<-Nearest(df)
  cf<-df[order(df$nearest,decreasing = T),]
  cf<-cf[1:x,]
  plot(cf$lon,cf$lat)
  return(cf)
}

# run 
ReducePoints(XYYpairs,x=3)
plot(XYYpairs$lon,XYYpairs$lat)
ReducePoints2(XYYpairs,x=100)

# reduce start points by combining ---- 
set.seed(100)
require(graphics)
#cluster analysis
datf<-Ypairs[grepl("004",Ypairs$region),] # assign region
x <- data.frame(lon=as.numeric(datf$lon), lat = as.numeric(datf$lat))
colnames(x) <- c("x", "y") #change column name
(cl <- kmeans(x, 10)) # determine number of clusers

#plot PV clusters and centers
plot(x, col = cl$cluster)#ylim=c(36,41),xlim=c(-81,-73))
points(cl$centers, col = 1:2, pch = 8, cex = 2)

#run combination function
YpairsR4<-CombinePoints2(cl,datf) # combined PV locations df

#combination function
CombinePoints2 <-function(cl,df){
  # cl is kmeans output
  # df is original df with size and lon lat data
  newdf<-data.frame(lon=as.numeric(),lat=as.numeric(),
                    size=as.numeric(),number=as.numeric())
  for (x in 1:dim(cl$centers)[1]) {
    cf<-df[cl$cluster==x,]
    size<-sum(as.numeric(cf$size))
    nos<-dim(cf)[1]
    newdf<-rbind(newdf, matrix(c(cl$centers[x,1],cl$centers[x,2],size, nos), ncol=4))
  }
  colnames(newdf)<-c("lon","lat","size","number")
  return(newdf)
}


# Pros : quick
# Cons : centers are not actual points in data



# find distance between points -----
FindRoutes<-function(start,end){
  routes<-data.frame(from.lon=NA,from.lat=NA,to.lon=NA,to.lat=NA,
                   distance=NA,size=NA,from.state=NA, to.state=NA)
  for (y in 1:dim(start)[1]){
    from = as.numeric(start[y,1:2])
    for (x in 1:dim(end)[1]) {
      to = as.numeric(end[x,1:2])
      distance <- mapdist(from, to, mode="driving", output="simple")
      newrow<-data.frame(from.lon = from[1],from.lat = from[2],
                         to.lon = to[1], to.lat = to[2], 
                         distance = distance$miles, size=start[y,"size"], 
                         from.state=start[x,]$state, to.state=end[y,]$state)
      routes<-rbind(newrow, routes)
      }
  }
  return(routes)
  }


# write data to csv -----
write.csv(x=routes,file="distance_lf.csv")
