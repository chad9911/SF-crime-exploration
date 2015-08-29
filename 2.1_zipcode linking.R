
# setwd("Z:/r projects/sacrime")

# Load libraries

library(leaflet)
library(dplyr)
library(rgdal)

# Re-load cleaned train data

load(file = "train_modified.RData")
cri <- train.clean

# Load San Francisco zip-code area shapefile from 'sf_zipcode' folder
# The area data here is obatined from <data.sfgov.org>

sf <- readOGR(dsn = "sf_zipcode", layer = "sfzipcodes")
sf <- spTransform(sf, CRS("+proj=longlat +datum=WGS84"))


# Plot example - this part can be skipped

m <- leaflet(sf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons() %>%
  addMarkers(lng <- cri[cri$Category == "GAMBLING",c("X")], lat = cri[cri$Category == "GAMBLING",c("Y")]) %>%
  setView(lng <- cri[cri$Category == "GAMBLING",c("X")][1], lat = cri[cri$Category == "GAMBLING",c("Y")][1],zoom = 12)


#================ Prepare for Points-in-Polygon operation

cri.cord <- data.frame(X = cri$X, Y = cri$Y)
cri.cord <- SpatialPoints(cri.cord)
# coordinates(cri.cord) <- c("longitude","latitude")
# cri.cord <- SpatialPointsDataFrame(cri.cord)
proj4string(cri.cord) <- proj4string(sf)

sf.poly <- sf@polygons

getPostcode <- function(SP_unit,row.n){
  for(i in 1:length(sf.poly)){
    a <- SpatialPolygons(sf.poly[i])
    proj4string(a) <- proj4string(cri.cord)
    b <- over(SP_unit[row.n],a)
    if(!is.na(b)){
      return(sf$ZIP_CODE[i])
    }
  }
}



#================ Find postcode in train dataset through Points-in-Polygon operation
#================ A progress bar has been placed here for visual idication of current progress
#================ CAULTION - the loop below will normally take more than 10 hours to complete

train_postcode = c()

total <- nrow(cri)

pb <- winProgressBar(title="Example progress bar", label="0% done", min=0, max=total, initial=0)

for(i in length(train_postcode)+1:total){
  train_postcode[i] <- if(!is.null(getPostcode(cri.cord,i))){
    getPostcode(cri.cord,i)
  } else {
    ""
  }
  info <- sprintf("%f%% done", (i/total)*100)
  setWinProgressBar(pb, i/total*100, label=info)
}

close(pb)

#================ Insert zip code vector back into our train data frame

cri <- data.frame(cri,Zipcode = train_Zipcode)

#================ Plot a map show number of total crimes in each zipcode area

cri_zip <- cri %>%
  group_by(Zipcode) %>%
  summarise(Number_of_Crimes = n()) %>%
  data.frame()

cri_zip <- cri_zip[!cri_zip$Zipcode == "",]
sf@data <- merge(sf@data,cri_zip,by.x="ZIP_CODE",by.y="Zipcode")

pal <- colorNumeric(palette = "Reds", domain = sf$Number_of_Crimes)

m <- leaflet(sf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = F, color = ~pal(Number_of_Crimes)) %>%
  addLegend("bottomright", pal = pal, values = ~Number_of_Crimes,
            title = "Number of crimes ('0)",
            opacity = 1
  )

#================ Save train data set with Zip code column

train.zip <- cri
save(train.zip, file = "train_modified2.RData")