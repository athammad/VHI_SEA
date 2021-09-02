
library(data.table)
library(lubridate)
#### Data Preparation ####

#Read GEE
setwd("~/pCloudDrive/Dropbox/VHI_SEA")

#update RegionNames by removing Bruenei
RegionNames<-c("Indonesia","Malaysia","Vietnam", "Cambodia",
               "Laos","Thailand","Timor-Leste","Myanmar","Philippines") 


covariates<-lapply(tolower(RegionNames),function(cr){
  
  RAIN<-fread(paste0("./GEE_Data/",cr,"/RAIN.csv"))
  TEMP<-fread(paste0("./GEE_Data/",cr,"/TEMP.csv"))
  HUM<-fread(paste0("./GEE_Data/",cr,"/HUM.csv"))
  HUM<-HUM[,mean(Specific_humidity_height_above_ground),by= timeFormat]
  setnames(HUM,c('timeFormat', 'humidity'))
  SOL<-fread(paste0("./GEE_Data/",cr,"/SOL.csv"))
  SOL<-SOL[,mean(`Downward_Short-Wave_Radiation_Flux_surface_6_Hour_Average`),by= timeFormat]
  setnames(SOL,c('timeFormat', 'solar'))
  CLOUD<-fread(paste0("./GEE_Data/",cr,"/CLOUD.csv"))
  CLOUD<-CLOUD[,mean(cloud_fraction),by= timeFormat]
  setnames(CLOUD,c('timeFormat', 'cloud'))
  #RESCALE VALUE OF CLOUD
  CLOUD$cloud<-scales::rescale(CLOUD$cloud)
  
  covs<-list(RAIN,TEMP,HUM,SOL,CLOUD)
  

  #Set Dates format
    covars<-lapply(covs,function(x){
      x[,Date:=as.Date(timeFormat, '%d-%m-%Y')]
      #x<-unique(x,by="Date")
      x$timeFormat<-NULL
      x
  
  })
  #Merge
  covariates = Reduce(function(...) merge(..., all = TRUE), covars)
  setnames(covariates,c('Date','Rain','Temp','Hum','Sol','Cloud'))  
  
})

lapply(covariates,function(x)lapply(x, class))
names(covariates)<-RegionNames


###### CHECKS
#check for missing dates

lapply(covariates,function(x){
  date_range <- seq(min(x$Date), max(x$Date), by = 1) 
  date_range[!date_range %in% x$Date] 
  
  setkey(x,'Date')
  
})


#check N missing 
lapply(covariates,function(x){
  list(naniar::pct_miss(x),# percentage of missing value in the data.
       naniar::n_miss(x), # number of missing value in the data.
       naniar::n_complete(x) # without missing value
  )
  
})



#MISSING IMPUT
library(missRanger)
covariates<-lapply(covariates,function(x){
  x$Year<-year(x$Date)
  x$Month<-month(x$Date)
  x$Week<-week(x$Date)
  
  storeDate<-x$Date
  x$Date<-NULL
  x<-missRanger(x,seed = 1234)
  x$Date<-storeDate
  x
})


############################################################################################
#SETTING WEEK OF THE YEAR
covariates<-lapply(covariates,function(x){
                x$yd<-yday(x$Date)
                x$Week<-cut(x$yd,seq(1,365,7),include.lowest=T,labels = F)
                x[is.na(Week),Week:=52]
})

#bring data on a weekly level like VHI
covariates<-lapply(covariates,function(x){
                x$YW<-paste0(x$Year, x$Week)
                #length(unique(x$YW))
                x<-x[, lapply(.SD, mean), by=YW, .SDcols = c("Rain","Temp","Hum","Sol","Cloud")]
                #x$Month<-month(x$Date)

})



### Read VHI ###

#these measurements are processed to reduce long-term noise

allVHI<-lapply(RegionNames,function(x){
  country<- as.data.table(readr::read_csv(paste0("./VHI_Data/",x), 
                                       col_names = FALSE))
  country<-country[,.(X1,X2,X7)]
  names(country)<-c('Year','Week','VHI')
  country$Date<-as.Date(paste(country$Year, country$Week, 1, sep="-"), "%Y-%U-%u")
  country$Month<-lubridate::month(country$Date)
  country$YW<-paste0(country$Year, country$Week)
  country$VHI[country$VHI<0]<-NA
  country<-country[Date%in%c(as.Date("2000-04-01"):as.Date("2021-08-16"))]
  #summary(country)

  setkey(country, "Date")
  
})
###### CHECKS
#check for missing dates
lapply(allVHI,function(x){
  date_range <- seq(min(x$Date), max(x$Date), by = 7) 
  date_range[!date_range %in% x$Date] 

  
})
#MISSING DATES:"2001-01-01" "2007-01-01" "2012-12-31" "2018-01-01"

#check N missing 
lapply(allVHI,function(x){
  list(naniar::pct_miss(x),# percentage of missing value in the data.
       naniar::n_miss(x), # number of missing value in the data.
       naniar::n_complete(x) # without missing value
  )
  
})

#FINAL STRUCTURE FOR VHI
#1) PADDING THE MISSING DATES:"2001-01-01" "2007-01-01" "2012-12-31" "2018-01-01"
#2) RECONSTRUCTING THE WEEK INDEX AFTER PADDING 
#3) NA.APPROX for the few missing points on VHI (22 for each country)
allVHI<-lapply(allVHI, function(x){
  x<-padr::pad(x)
  x$Month<-lubridate::month(x$Date)
  x$Year<-lubridate::year(x$Date)
  x$yd<-yday(x$Date)
  x$Week<-cut(x$yd,seq(1,365,7),include.lowest=T,labels = F)
  x[is.na(Week),Week:=52]
  x$YW<-paste0(x$Year, x$Week)
  x$yd<-NULL
  x$VHI<-zoo::na.approx(x$VHI)
  setkey(x,'Date')
  
})


names(allVHI)<-RegionNames

############################################################################################


#Merge All the Data
SeaTS<-merge(rbindlist(covariates,idcol="CRN"),
             rbindlist(allVHI,idcol="CRN"),
             by=c("CRN",'YW'),all = TRUE)

names(SeaTS)
SeaTS<-SeaTS[!YW%in%c("200013","202131","202132","202133"),]# remove days with no data

setkeyv(SeaTS, c("CRN","Date"))


#MISSING DATES:"2001-01-01" "2007-01-01" "2012-12-31" "2018-01-01"

summary(SeaTS)
summary(SeaTS[CRN=="Laos"])
plot.ts(SeaTS[CRN=="Laos",VHI])

lapply(split.data.frame(SeaTS,SeaTS$CRN),function(x){
  date_range <- seq(min(x$Date), max(x$Date), by = 7) 
  date_range[!date_range %in% x$Date] 
})


############################################################################################

#Lags and Means Over Time

#Means
#yearly RAIN & TEMP
YearMean<-SeaTS[, lapply(.SD, mean, na.rm=TRUE), by=c("CRN","Year"), .SDcols=c("Rain","Temp")]
#monthly ALL
MonthMean<-SeaTS[, lapply(.SD, mean, na.rm=TRUE), by=c("CRN","Year","Month"), .SDcols=c("Rain","Temp","Hum",'Sol')]
#Sums RAIN month
MonthSum<-SeaTS[, lapply(.SD, sum, na.rm=TRUE), by=c("CRN","Year","Month"), .SDcols=c("Rain") ]

agreg<-merge(MonthMean,MonthSum, by=c("CRN","Year","Month"))
setnames(agreg,c("CRN","Year","Month","MmeanRain","MmeanTemp","MmeanHum","MmeanSol","MsumRain"))
agreg<-merge(agreg,YearMean, by=c("CRN","Year"),allow.cartesian = T)
setnames(agreg,c("Rain","Temp"), c("YmeanRain","YmeanTemp"))

#MERGE THE AGGREGATION WITH THE MAIN DATASET
SeaTS<-merge(SeaTS,agreg, by=c("CRN","Year","Month"),allow.cartesian = T)
names(SeaTS)
summary(SeaTS)
#Sine Cosine of time 
sinner<-as.data.table(cyclic_encoding(SeaTS$Date, c("week", "month","year")))
SeaTS<-cbind(SeaTS,sinner)

#REMOVE USELESS VARS FOR NOW
SeaTS<-SeaTS[,-c("Year" ,"Month", "Week","YW"),with=F]


summary(SeaTS)
#fwrite(SeaTS,"SeaTS.csv")
rm(list=setdiff(ls(), "SeaTS"))
gc()

