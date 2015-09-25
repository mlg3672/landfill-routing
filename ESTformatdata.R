library(ggmap)
## set working directory
setwd("~/Documents/Python-Projects/es-and-t")

## read to datatable
landfills<-read.csv("landfills.csv")
colnames(landfills)<-c("name","address","city","state","zip")
dim
solarcells<-read.csv("openpv.csv")
colnames(solarcells)<-c("zipcode","state","kWDC", "cost","date","lat","long")

# assign landfill id
landfill$id <-c(1:length(landfills))

# geocode landfill locations
gc<-as.numeric(geocode(paste(landfills$address, 
                             landfills$city,
                             landfills$state,
                             landfills$zip
                           )))
landfills$long < gc[1]
landfill$lat<- gc[2]

## get routes, make network dataframe
newdist = {}
colnames(newdist)= c("from","to","kW","state")


for (x in 1:length(landfills)) {
  
  from<-as.numeric(data.frame(long=landfills$long,lat=landfills$lat[x]))
  
  for (y in 1:length(solarcells)) {
    
    to <- as.numeric(data.frame(long=solarcells$long[y],lat=solarcells$lat[y]))
  
    if (landfills$state == solarcells$state) {
      distance<-mapdist(from,to,mode="driving",output="simple")
      newdist<-rbind(newdist,c(landfill$id[x], 
                          solarcells$id[y],distance,
                          kW=solarcells$size[y],
                          state=solarcells$state[y]))
      
    }
    else { next }

  }
}