# the objective of this code is to read and get lat lon data from file of locations
# then combine start and end points based on location 
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
  wcountygis<-cbind(gisdf0,wcounty)
  wzipgis<-cbind(gisdf1,wzip)
  wcitygis<-cbind(gisdf2,wcity)
  wzcitygis<-cbind(gisdf3,wzcity)
  
  geto<-rbind(wcountygis,wzipgis, wcitygis,wzcitygis) # bind all dataframes
  return(geto)
}



# add AK, IL data already geocoded 
#dat<-rbind(AKcases,dat) # merge AK data - done region 1 above
#dat<-rbind(ILcases,dat) # merge IL data - done region 2 above

#  limitation of gmap to 2000 queries 

# find nearest LF point, eliminate most frequent
require(geosphere)

Nearest<-function(df) {
  # takes a data frame with x-lon and y-lat as first two columns
  # finds the points at the nearest distance assuming planar
  df$nearest<-rep(NA,dim(df)[1])
  print("add nearest row worked")
  df$y.near<-rep(NA,dim(df)[1])
  print("add near row worked")
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

 
set.seed(100)
require(graphics)

FindClusters<- function(df,n){
  # cluster analysis
  # n is the number of clusters
  x <- data.frame(lon=as.numeric(df$lon), lat = as.numeric(df$lat))
  colnames(x) <- c("x", "y") #change column name
  (cl <- kmeans(x, n)) # determine number of clusers
  plot(x, col = cl$cluster)# #plot PV clusters 
  points(cl$centers, col = 1:2, pch = 8, cex = 2) # plot centers
  return(cl)
}

#combination function
CombinePoints2 <-function(cl,df){
  # cl is kmeans output
  # df is original df with size and lon lat data
  newdf<-data.frame(lon=as.numeric(),lat=as.numeric(),
                    size=as.numeric(),number=as.numeric(),
                    dist=as.numeric())
  for (x in 1:dim(cl$centers)[1]) {
    cf<-df[cl$cluster==x,]
    size<-sum(as.numeric(cf$size))
    p1<-cl$centers[x,1:2]
    dist<-0
    for (a in 1:dim(cf)[1]){
      p2<-matrix(c(as.numeric(cf$lon[1]),as.numeric(cf$lat[1])),ncol=2)
      newdist<-distMeeus(p1,p2)
      dist<-dist + newdist
    }
    #print(dist,dist/1e3)
    nos<-dim(cf)[1]
    newdf<-rbind(newdf, matrix(c(cl$centers[x,1],cl$centers[x,2],size, nos,dist/1e3), ncol=5))
  }
  colnames(newdf)<-c("lon","lat","size","number","distkm") # missing state
  return(newdf)
}


require(plyr)
#function
FindRoutes<-function(start,end){
  routes<-data.frame(from.lon=as.numeric(),from.lat=as.numeric(),
                     to.lon=as.numeric(),to.lat=as.numeric(),
                   distance=as.numeric(),size=as.numeric(),
                   number=as.numeric(), to.state=as.character())
  for (y in 1:dim(start)[1]){
    from<-as.numeric(start[y,1:2])
    for (x in 1:dim(end)[1]) {
      #print("x is",str(x))
      to<-as.numeric(end[x,1:2])
      distance <- mapdist(from, to, mode="driving", output="simple")
      newrow<-matrix(c(from[1],
                       from[2],
                       to[1],
                       to[2],
                       distance$km,
                       start[y,"distkm"],
                       start[y,"size"],
                       start[y,"number"],
                       end[y,]$state),ncol=9)
      
      #print(newrow)
      routes<-rbind(newrow, routes)
      }
  }
  colnames(routes)<-c("from.lon","from.lat","to.lon","to.lat","routekm",
                      "clusterkm","size", "number", "to.state")
  return(routes)
  }

# function to generate code for optimization program
GenCodes<-function(start,end,routes){
  code<-data.frame(from=as.character(),to=as.character(),
                 routeid=as.numeric(),distance=as.numeric(),
                 distance2=as.numeric(),size=as.numeric())
  i=0
  for (x in 1:dim(start)[1]){
    s<-paste0("AA",LETTERS[x])
    for (y in 1:dim(end)[1]){
      i<-i+1
      #print("i is",str(i))
      dist<-as.numeric(as.character(routes[i,"routekm"]))
      size<-as.numeric(as.character(routes[i,"size"]))
      number<-as.numeric(as.character(routes[i,"number"]))
      dist2<-as.numeric(as.character(routes[i,"clusterkm"]))
      newrow<-matrix(c(s,"RG1",y,dist,dist2,size,number),ncol=7)
      #print(newrow)
      code<-rbind(newrow,code)
      }
    }
  colnames(code)<-c("from","to","routeid","routekm","clusterkm","size","installs")
  return(code)
  }

WriteData<-function(routes,code,r){
  fname<-paste0("scheduleR",r,".csv")
  fname2<-paste0("scheduleR2",r,".csv")
  fname3<-paste0("scheduleR3",r,".csv")
  write.csv(x=routes,fname) #write routes to csv file
  write.table(routes, fname2, quote=F,sep=",") # write routes to text file
  write.table(code, fname3, quote=F,sep=",") # write code to text file
}

# rearrange PV data columns
allstart <-data.frame(lon=datos$lon,lat=datos$lat, size=datos$size, 
                      state=datos$state, stringsAsFactors = F)

# region 1 geocode, endpts ----
endR1<-dat[dat$state %in% c("HI","AR","OR","WA","ID","LA","WY","MO",
                            "AZ","NM","TX","OK","KS","NV","UT","CO",
                            "CA","NE","MT","AK"),] 
endR1gis<-FindLatLong(endR1) # geocode
endR1gis<-cbind(endR1gis,endR1) # merge gis end data
endR1gis<-rbind(AKcases,endR1gis) # merge AK end data
plot(endR1gis$lon,endR1gis$lat) # plot end locations

subendR1<-ReducePoints2(endR1gis,x=10) # reduce endpoints
plot(subendR1$lon,subendR1$lat)


startR1<-allstart[allstart$state %in% c("HI","AR","OR","WA","ID","LA","WY","MO",
                                        "AZ","NM","TX","OK","KS","NV","UT","CO",
                                        "CA","NE","MT","AK"),] # split pv by region
cl<-FindClusters(startR1,n=10) # cluster anlaysis on pv
substartR1<-CombinePoints2(cl,startR1) # combin PV locations

routes1<-FindRoutes(start=substartR1,end=subendR1) # find routes
code1<-GenCodes(start=substartR1,end=subendR1, routes=routes1) # get codes
WriteData(routes1,code1,r=1)# write to text files

# region 2 geocode --------
endR2<-dat[dat$state %in% c("IA", "IL", "CT","SD", "NY", "NJ", "IN", 
                            "WI", "WV","ND", "MN", "MI", "KY", "OH", 
                            "PA", "TN", "MS"),] 
endR2gis<-FindLatLong(endR2) # geocode
endR2gis<-cbind(endR2gis,endR2) # merge gis dat
endR2gis<-rbind(endR2gis,ILcases) # merge IL data
plot(endR2gis$lon,endR2gis$lat) # plot locations

subendR2<-ReducePoints2(endR2gis,x=10) # reduce points
plot(subendR2$lon,subendR2$lat)


startR2<-allstart[allstart$state %in% c("IA", "IL", "CT","SD", "NY", "NJ", "IN", 
                                        "WI", "WV","ND", "MN", "MI", "KY", "OH", 
                                        "PA", "TN", "MS"),] # split pv by region
cl<-FindClusters(startR2,n=10) # cluster anlaysis on pv
substartR2<-CombinePoints2(cl,startR2) # combin PV locations

routes2<-FindRoutes(start=substartR2,end=subendR2) # find routes
code2<-GenCodes(start=substartR2,end=subendR2, routes=routes2) # get codes
WriteData(routes2,code2,r=2)# write to text files

# region 3 geocode --------
endR3<-dat[dat$state %in% c("RI", "GA", "SC", "TN", "MS","AL", "SC", "FL", "NC","MA"),] 
endR3gis<-FindLatLong(endR3) # geocode
endR3gis<-cbind(endR3gis,endR3) # merge gis data
plot(endR3gis$lon,endR3gis$lat) # plot locations

subendR3<-ReducePoints2(endR3gis,x=10) # reduce points
plot(subendR3$lon,subendR3$lat)

startR3<-allstart[allstart$state %in% c("RI", "GA", "SC", "TN", "MS",
                                        "AL", "SC", "FL", "NC","MA"),] # split pv by region
cl<-FindClusters(startR3,n=10) # cluster anlaysis on pv
substartR3<-CombinePoints2(cl,startR3) # combin PV locations

routes3<-FindRoutes(start=substartR3,end=subendR3) # find routes
code3<-GenCodes(start=substartR3,end=subendR3, routes=routes3) # get codes
WriteData(routes3,code3,r=3)# write to text files

# region 4 geocode --------
endR4<-dat[dat$state %in% c("DC","VA","DE","MD"),] 
endR4gis<-FindLatLong(endR4) # geocode
endR4gis<-cbind(endR4gis,endR4) # merge gis data
plot(endR4gis$lon,endR4gis$lat) # plot locations

subendR4<-ReducePoints2(endR4gis,x=10) # reduce points
plot(subendR4$lon,subendR4$lat)

startR4<-allstart[allstart$state %in% c("DC","VA","DE","MD"),] # split pv by region
cl<-FindClusters(startR4,n=10) # cluster anlaysis on pv
substartR4<-CombinePoints2(cl,startR4) # combin PV locations

routes4<-FindRoutes(start=substartR4,end=subendR4) # find routes
code4<-GenCodes(start=substartR4,end=subendR4, routes=routes4) # get codes
WriteData(routes4,code4,r=4)# write to text files

# region 5 geocode --------
endR5<-dat[dat$state %in% c("VT","NH","ME"),] 
endR5gis<-FindLatLong(endR5) # geocode
endR5gis<-cbind(endR5gis,endR5) # merge gis dat
plot(endR5gis$lon,endR5gis$lat) # plot locations

subendR5<-ReducePoints2(endR5gis,x=10) # reduce points
plot(subendR5$lon,subendR5$lat)

startR5<-allstart[allstart$state %in% c("VT","NH","ME"),] # split pv by region
cl<-FindClusters(startR5,n=10) # cluster anlaysis on pv
substartR5<-CombinePoints2(cl,startR5) # combin PV locations

routes5<-FindRoutes(start=substartR5,end=subendR5) # find routes
code5<-GenCodes(start=substartR5,end=subendR5, routes=routes5) # get codes
WriteData(routes5,code5,r=5)# write to text files

#endR1g<-endR1gis[!is.na(endR1gis$lon),] # eliminate NA values
