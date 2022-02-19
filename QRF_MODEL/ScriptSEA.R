rm(list=ls())
library(data.table)
library(lubridate)
library(zoo)
#### Data Preparation ####

#Read GEE
setwd("~/VHI_SEA")

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

#remove usless vars
SeaTS<-SeaTS[,-c("YW","Week"),with=F]
fwrite(SeaTS,"SeaTS.csv")
#SeaTS<-fread("SeaTS.csv")
#lapply(SeaTS, class)


#1)CREATE THE LAGS


scenarios<-lapply(c(2,4,8,12), function(lagval){
  
lagSeaTS<-copy(SeaTS)

lagSeaTS$DateVHI<-lagSeaTS$Date # copy actual date of VHI outcome (for plotting later)

cols = c('VHI','Rain','Temp','Hum','Sol',"Cloud")
anscols = paste("lag", cols,lagval, sep="_")
lagSeaTS[, (anscols) := shift(.SD, lagval, "lag"), .SDcols=cols,by="CRN"]

TimeIDs<-c("Year","Month","Date")# lag of time variables for future merge!!!!
lagSeaTS[, (TimeIDs) := shift(.SD, lagval, "lag"), .SDcols=TimeIDs,by="CRN"]

#2)remove current vars except for VHI, Year and Month
lagSeaTS<-lagSeaTS[,-c('Rain','Temp','Hum','Sol',"Cloud"),with=F]

lagSeaTS<-na.omit(lagSeaTS)

#3) calculate the means, sums and rolls over the lags
#############
#Means
#yearly RAIN & TEMP
YearMean<-lagSeaTS[, lapply(.SD, mean, na.rm=TRUE), by=c("CRN","Year"), .SDcols=paste("lag", c("Rain","Temp"),lagval, sep="_")]
#monthly ALL
MonthMean<-lagSeaTS[, lapply(.SD, mean, na.rm=TRUE), by=c("CRN","Year","Month"), .SDcols=paste("lag",c("Rain","Temp","Hum",'Sol'),lagval, sep="_")]
#Sums RAIN month
MonthSum<-lagSeaTS[, lapply(.SD, sum, na.rm=TRUE), by=c("CRN","Year","Month"), .SDcols=paste("lag", c("Rain"),lagval, sep="_") ]

#############
#ROLLSUMS
Sumcols<-paste("lag", c("Rain"),lagval, sep="_")
lagSeaTS[ , paste0("rollS_",Sumcols) := lapply(.SD, function(x) rollsumr(x,k = lagval, fill = NA,align = "right")), .SDcols = Sumcols,by="CRN"]
#ROLLMEANS
Meancols<-paste("lag",c("Rain","Temp","Hum",'Sol'),lagval, sep="_")
lagSeaTS[ , paste0("rollM_",Meancols) := lapply(.SD, function(x) rollmeanr(x,k = lagval, fill = NA,align = "right")), .SDcols = Meancols,by="CRN"]

agreg<-merge(MonthMean,MonthSum, by=c("CRN","Year","Month"))
setnames(agreg,c("CRN","Year","Month","MmeanRain","MmeanTemp","MmeanHum","MmeanSol","MsumRain"))
agreg<-merge(agreg,YearMean, by=c("CRN","Year"),allow.cartesian = T)
setnames(agreg,paste("lag", c("Rain","Temp"),lagval, sep="_"), c("YmeanRain","YmeanTemp"))

#MERGE THE AGGREGATION WITH THE MAIN DATASET
lagSeaTSX<-merge(lagSeaTS,agreg, by=c("CRN","Year","Month"),allow.cartesian = T)

#Sine Cosine of time 
sinner<-as.data.table(cyclic_encoding(lagSeaTSX$Date, c("month","year")))
lagSeaTSX<-cbind(lagSeaTSX,sinner)

lagSeaTSX<-na.omit(lagSeaTSX)
lagSeaTSX
})
length(scenarios)
names(scenarios)<-c("W2","M1","M2","M3")
names(scenarios$M1)

View(scenarios$M1)

#####################################################################################################


rm(list=setdiff(ls(), c("scenarios","SeaTS","SEAmodels")))
gc()
summary(SeaTS$Date)
summary(scenarios$W2$Date)
summary(scenarios$M1$Date)
summary(scenarios$M2$Date)

tail(scenarios$W2$VHI)
tail(scenarios$M1$VHI)
tail(scenarios$M2$VHI)
