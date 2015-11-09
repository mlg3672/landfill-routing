# the objective of this code is to read and get lat lon data from file of locations
# then export to txt file for further analysis
library(stringr)
library(data.table)
library(plyr)
# set working directory
setwd("~/Documents/Python-Projects/landfill-routing")

# read data ---------------------
dat<-read.csv("uslandfill_mod.csv", colClasses="character")# reads destination  points to df - best option !
dato<-read.csv("openpv.csv", colClasses= "character") # read start points to df

# clean data ----------------
## set column names for LF
colnames(dat)<- c("name","city","county","state","zip")
dats<-dat[,1:5]
dim(dats)

## subset PV data
datos<-dato[,c(2:3,6:8)]
colnames(datos)<-c("state","size","lat","lon","zip")
dim(datos)

## check that all zip codes have 5 digits in LF
dat[nchar(dat[,5])!=5,]#MA has 4 digits - fixed!

 #476 AK only state with no city,zip or county - fixed!

# delete territories Puerto Rico "PR", Virgin Islands "VI", Guam "GU", "MP", "AS"
dat<-dat[dat$state!="PR",]
dat<-dat[dat$state!="VI",]
dat<-dat[dat$state!="GU",]
dat<-dat[dat$state!="MP",]
dat<-dat[dat$state!="AS",]
unique(dat$state)

datos<-datos[datos$state!="PR",]
unique(datos$state)

#change DC to MD
datos[datos$state=="DC",1]<-"MD"

# erase duplicated rows
dat<-dat[!duplicated(dat),]

# import Alaska data -----
AKcases<-read.csv("AK_landfills.csv",colClasses = "character")
summary(AKcases)
AKcases<-AKcases[c(1:18),c(2,7:12)]
colnames(AKcases)<- c("name","city","county","state","zip","lon","lat")

dat<-dat[!grepl("AK",dat$state),] # delete Alaska from original dataframe

#later add data to df


#import WV data -----
dat<-dat[!grepl("WV",dat$state),] # delete WV from original dataframe
WVcases<-read.csv("WV_landfills.csv",colClasses = "character")
summary(WVcases)

dat<-rbind(WVcases[1:18,1:5],dat) # merge dataframes 

#import IL data -----
library(ggmap)
library(plyr)
ILcases<-read.csv("IL_landfills.csv",colClasses = "character")
summary(ILcases)
ILcases<-ILcases[-40,]
ILcases$zip<-"NA"# insert zip column
ILgis<-geocode(paste(ILcases$address, ILcases$city, ILcases$state, "USA",sep=","))
ILcases<-cbind(ILcases,ILgis)
colnames(ILcases)<- c("name","city","county","state","zip","lon","lat")
dat<-dat[!grepl("IL",dat$state),] # delete IL from original dataframe




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

## run code 004 for mid-atlantic 
t0<-data.frame(c("DC","VA","DE","MD"))
dat<-regioncode(t0,"004",dat)
datos<-regioncode(t0,"004",datos) 

## run code 005 for new england
t0<-data.frame(c("VT","NH","ME"))
dat<-regioncode(t0,"005",dat)
datos<-regioncode(t0,"005",datos) 

regioncode<-function(t0,code,tab){
  # t0 is a dataframe of strings,states in region
  # code is a string which designates region
  # tab is a dataframe which needs to be labeled 
  t1<-as.list(t(t0))
  t2<-paste(t1,collapse = "|")
  tab[grepl(t2,tab$state),]$region<-code
  return(tab)
}

##  find lat lon -----------
library(ggmap)
library(plyr)


# FindLatLong geocoding function
FindLatLong<-function(df){
  # takes a dataframe with city, county, state, zip columns
  # determines missing information and geocodes
  # returns geocoded matrix
  
  ## find NA values
  NAzip<-is.na(df$zip)
  NAstate<-is.na(df$state)
  NAcity<-is.na(df$city)
  NAcounty<-is.na(df$county)
  
  #select values
  wnone<-df[NAzip & NAcity & NAcounty,]# rows with no values
  wcounty<-df[NAzip & NAcity & !NAcounty,] #find rows with only county
  wzip<-df[!NAzip & NAcity,]#find rows with zip but no city value
  wcity<-df[!NAcity & NAzip,]# find rows with city value but no zip 
  wzcity<-df[!NAzip & !NAcity,]# find rows with city and zip 
  
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
  
  geto<-rbind(wcountygis,wzipgis, wcitygis,wzcitygis) # bind all dataframes
  return(geto)
}

# region geocode, plot, reduce
endR1<-dat[dat$state==c("HI","AR","OR","WA","ID","LA","WY","MO",
                        "AZ","NM","TX","OK","KS","NV","UT","CO",
                        "CA","NE","MT","AK"),] 
endR1gis<-FindLatLong(endR1) # geocode
plot(endR1$lon,endR1$lat) # plot region locations
endR1<-endR1[!is.na(endR1$lon),] # eliminate NA values
subendR1<-ReducePoints2(endR1,x=5) # reduce points

 # run regioncode-->



# add AK, IL data already geocoded
dat<-rbind(AKcases[,2:7],dat) # merge AK data
dat<-rbind(ILcases[,2:6],dat) # merge IL data

#plot lat lon pairs -----
endpoints <- data.frame(lon=geto$lon,lat=geto$lat, state=geto$state, region=geto$region)
plot(endpoints$lon, endpoints$lat)
dim(endpoints)

allstart <-data.frame(lon=datos$lon,lat=datos$lat, size=datos$size, 
                    state=datos$state, region=datos$region, stringsAsFactors = F)
# test set
substart<-allstart[1:1000,]
subend<-endpoints[1:1000,]

#split pairs by region 
subendR1<-endpoints[grepl("001",endpoints$region),]
substartR1<-allstart[grepl("001",allstart$region),]

## reduce the number of points by elimination --------
# due to limiation of gmap to 2000 queries 

# find nearest LF point, eliminate most frequent
require(geosphere)
p1<-endpoints[1,1:2]
p2<-endpoints[2,1:2]
distGeo(p1,p2)# error could not find function

all<-distm(endpoints[1:2,1:2]) # produces matrix of as the crow flies distances
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
ReducePoints(subend,x=3)
plot(subend$lon,subend$lat)
subendR4<-ReducePoints2(subend,x=100)

# reduce start points by combining ---- 
set.seed(100)
require(graphics)
#cluster analysis
datf<-allstart[grepl("004",allstart$region),] # assign region
x <- data.frame(lon=as.numeric(datf$lon), lat = as.numeric(datf$lat))
colnames(x) <- c("x", "y") #change column name
(cl <- kmeans(x, 10)) # determine number of clusers

#plot PV clusters and centers
plot(x, col = cl$cluster)#ylim=c(36,41),xlim=c(-81,-73))
points(cl$centers, col = 1:2, pch = 8, cex = 2)

#run combination function
substartR4<-CombinePoints2(cl,datf) # combined PV locations df

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
  colnames(newdf)<-c("lon","lat","size","number") # missing stae
  return(newdf)
}


# Pros : quick
# Cons : centers are not actual points in data



# find distance between points -----
require(plyr)
#function
FindRoutes<-function(start,end){
  routes<-data.frame(from.lon=as.numeric(),from.lat=as.numeric(),
                     to.lon=as.numeric(),to.lat=as.numeric(),
                   distance=as.numeric(),size=as.numeric(),
                   from.state=as.character(), to.state=as.character())
  for (y in 1:dim(start)[1]){
    from<-as.numeric(start[y,1:2])
    for (x in 1:dim(end)[1]) {
      print("x is",str(x))
      to<-as.numeric(end[x,1:2])
      distance <- mapdist(from, to, mode="driving", output="simple")
      newrow<-matrix(c(from[1],
                       from[2],
                       to[1],
                       to[2],
                       distance$miles,
                       start[y,"size"],
                       "NA", 
                       end[y,]$state),ncol=8)
      
      print(newrow)
      routes<-rbind(newrow, routes)
      }
  }
  colnames(routes)<-c("from.lon","from.lat","to.lon","to.lat",
                      "distance","size", "from.state", "to.state")
  return(routes)
  }

# function to generate code for optimization program
GenCodes<-function(start,end,routes){
  code<-data.frame(from=as.character(),to=as.character(),
                 routeid=as.numeric(),distance=as.numeric(),
                 size=as.numeric())
  i=0
  for (x in 1:dim(start)[1]){
    s<-paste0("AA",LETTERS[x])
    for (y in 1:dim(end)[1]){
      i<-i+1
      print("i is",str(i))
      dist<-as.numeric(as.character(routes[i,"distance"]))
      size<-as.numeric(as.character(routes[i,"size"]))
      newrow<-matrix(c(s,"RG4",y,dist,size),ncol=5)
      print(newrow)
      code<-rbind(newrow,code)
      }
    }
  colnames(code)<-c("from","to","routeid","distance","size")
  return(code)
  }
#run
routes<-FindRoutes(start=substartR4,end=subendR4)
code<-GenCodes(start=substartR4,end=subendR4, routes=routes)
# write data to csv -----
write.csv(x=routes,file="scheduleR4.csv") #write to csv file
write.table(routes, "schedule3R4.txt", quote=F,sep=",") # write to text file
write.table(code, "schedule2R4.txt", quote=F,sep=",") # write to text file
