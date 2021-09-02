#ShapeFile from World Bank
#https://datacatalog.worldbank.org/dataset/world-bank-official-boundaries/resource/12cc761c-bb67-45a7-82ff-eb64b67bdbdc

library(sf)
location <- read_sf("./WB_countries_Admin0_10m/WB_countries_Admin0_10m.shp")
seaMap<-location[location$SUBREGION=="South-Eastern Asia",]

#remove Singapore
seaMap<-seaMap[!seaMap$FORMAL_EN=="Republic of Singapore",]

#simpler names
RegionNames<-c("Indonesia","Malaysia","Vietnam", "Cambodia",
               "Laos","Thailand","Timor-Leste","Brunei","Myanmar","Philippines")  

seaMap$RegionNames<-RegionNames

#Create Shapefiles for each region
setwd("./SFregions")
lapply(RegionNames, function(x){
  
  loc <- seaMap[seaMap$RegionNames==x, ]
  st_write(loc,dsn = gsub(" ", "_", x, fixed = TRUE), 
           layer = gsub(" ", "_", x, fixed = TRUE),
           driver = 'ESRI Shapefile')
  
})
