library(tidyverse)
library(varhandle)
library(tidyr)
library(caret)
library(margins)
library(broom)
library(lmtest)
library(sandwich)

countries <- c("Indonesia", "Malaysia", "Viet Nam", "Cambodia", "Lao People's Democratic Republic", "Thailand", "Timor-Leste", "Myanmar", "Philippines")

#

VHI <- read.csv("intPreds.csv", stringsAsFactors = F)
VHI$CRN <- ifelse(VHI$CRN == "Laos", "Lao People's Democratic Republic", VHI$CRN)
VHI$CRN <- ifelse(VHI$CRN == "Vietnam", "Viet Nam", VHI$CRN)

VHI$month <- lubridate::month(VHI$time)
VHI$year <- as.character(lubridate::year(VHI$time))
VHI$iso3c <-  countrycode::countrycode(VHI$CRN, "country.name", "iso3c")

#

VHI <- group_by(VHI, Scen, month, year, iso3c, CRN) %>% summarise(q1=mean(q1, na.rm=T), q9=mean(q9, na.rm=T), avg=mean(preds, na.rm=T))

VHI$widht <- VHI$q9-VHI$q1
VHI$interval_pred_ratio <- VHI$widht/VHI$avg

#

list <- list.files("D:/OneDrive - IIASA/Current papers/VHI/faoprices", full.names=T, pattern=".csv")
filenames <- gsub(".csv", "", basename(list)) 
prices <- lapply(list, read.csv)

prices[[1]]$price <- rowMeans(prices[[1]][,2:5], na.rm=T)
prices[[1]] <- dplyr::select(prices[[1]], 1, 6)

colnames(prices[[2]])[2] <- "price"
colnames(prices[[3]])[2] <- "price"
colnames(prices[[4]])[2] <- "price"
colnames(prices[[5]])[2] <- "price"
colnames(prices[[6]])[2] <- "price"
prices[[7]]$price <- rowMeans(prices[[7]][,2:3], na.rm=T)
prices[[7]] <- dplyr::select(prices[[7]], 1, 4)


for (i in 1:length(prices)){
  
  prices[[i]]$iso3c <- countrycode::countrycode(filenames, "country.name", "iso3c")[i]
  
}

prices <- data.table::rbindlist(prices, idcol="iso3c")

require(zoo)

prices$ï..Date.Monthly <- as.character(prices$ï..Date.Monthly)


prices$year= paste0("20", substr(prices$ï..Date.Monthly , 5, 6))
prices$month= match(substr(prices$ï..Date.Monthly , 1, 3),month.abb)

prices[,1] <- NULL

#

prices_merged <- merge(prices, VHI, by.x=c("year", "month", "iso3c"),  by.y=c("year", "month", "iso3c"))

prices_merged$lag1 <- lag(prices_merged$avg, 1)
prices_merged$lag3 <- lag(prices_merged$avg, 3)
prices_merged$lag6 <- lag(prices_merged$avg, 6)
prices_merged$lag12 <- lag(prices_merged$avg, 12)
prices_merged$lag24 <- lag(prices_merged$avg, 24)
prices_merged$lag48 <- lag(prices_merged$avg, 48)
prices_merged$lag72 <- lag(prices_merged$avg, 72)

#

ols1 <- lm(price ~ avg + as.factor(CRN) + lag1 + lag3 + lag6 + lag12 + lag24 + lag48 + lag72 + as.factor(month)*as.factor(year) + widht, data=prices_merged)
coeftest(ols1, vcov = vcovHC(ols1, type="HC1"))

ols2 <- lm(lag(price) ~ avg + as.factor(CRN) + lag1 + lag3 + lag6 + lag12 + lag24 + lag48 + lag72 + as.factor(month)*as.factor(year) + widht, data=prices_merged)
coeftest(ols2, vcov = vcovHC(ols2, type="HC1"))

ols3 <- lm(lag(price, 2) ~ avg + as.factor(CRN) + lag1 + lag3 + lag6 + lag12 + lag24 + lag48 + lag72 + as.factor(month)*as.factor(year) + widht, data=prices_merged)
coeftest(ols3, vcov = vcovHC(ols3, type="HC1"))

ols4 <- lm(lag(price, 3) ~ avg + as.factor(CRN) + lag1 + lag3 + lag6 + lag12 + lag24 + lag48 + lag72 + as.factor(month)*as.factor(year) + widht, data=prices_merged)
coeftest(ols4, vcov = vcovHC(ols4, type="HC1"))

#

library(stargazer)
stargazer(ols1, ols2, ols3, ols4, type = 'text', out = 'food_sec_reg_table_new.tex')

library(coefplot)

multiplot(ols1, ols2, ols3, ols4, names = c("price", "lag(price)", "lag(price, 2)", "lag(price, 3)"), coefficients = "avg")

# #
# 
# prices_merged_r <- filter(prices_merged, Scen=="W2")
# 
# ols1 <- lm(price ~ avg + as.factor(CRN) + lag1 + lag3 + lag6 + lag12 + lag24 + lag48 + lag72 + as.factor(month)*as.factor(year) + widht, data=prices_merged_r)
# coeftest(ols1, vcov = vcovHC(ols1, type="HC1"))
# 
# ols2 <- lm(lag(price) ~ avg + as.factor(CRN) + lag1 + lag3 + lag6 + lag12 + lag24 + lag48 + lag72 + as.factor(month)*as.factor(year) + widht, data=prices_merged_r)
# coeftest(ols2, vcov = vcovHC(ols2, type="HC1"))
# 
# ols3 <- lm(lag(price, 2) ~ avg + as.factor(CRN) + lag1 + lag3 + lag6 + lag12 + lag24 + lag48 + lag72 + as.factor(month)*as.factor(year) + widht, data=prices_merged_r)
# coeftest(ols3, vcov = vcovHC(ols3, type="HC1"))
# 
# ols4 <- lm(lag(price, 3) ~ avg + as.factor(CRN) + lag1 + lag3 + lag6 + lag12 + lag24 + lag48 + lag72 + as.factor(month)*as.factor(year) + widht, data=prices_merged_r)
# coeftest(ols4, vcov = vcovHC(ols4, type="HC1"))
# 
# #
# 
# #####
# 
# prices_merged_r <- filter(prices_merged, Scen=="M1")
# 
# ols1_1 <- lm(price ~ avg + as.factor(CRN) + lag1 + lag3 + lag6 + lag12 + lag24 + lag48 + lag72 + as.factor(month)*as.factor(year) + widht, data=prices_merged_r)
# coeftest(ols1_1, vcov = vcovHC(ols1_1, type="HC1"))
# 
# ols2_1 <- lm(lag(price) ~ avg + as.factor(CRN) + lag1 + lag3 + lag6 + lag12 + lag24 + lag48 + lag72 + as.factor(month)*as.factor(year) + widht, data=prices_merged_r)
# coeftest(ols2_1, vcov = vcovHC(ols2_1, type="HC1"))
# 
# ols3_1 <- lm(lag(price, 2) ~ avg + as.factor(CRN) + lag1 + lag3 + lag6 + lag12 + lag24 + lag48 + lag72 + as.factor(month)*as.factor(year) + widht, data=prices_merged_r)
# coeftest(ols3_1, vcov = vcovHC(ols3_1, type="HC1"))
# 
# ols4_1 <- lm(lag(price, 3) ~ avg + as.factor(CRN) + lag1 + lag3 + lag6 + lag12 + lag24 + lag48 + lag72 + as.factor(month)*as.factor(year) + widht, data=prices_merged_r)
# coeftest(ols4_1, vcov = vcovHC(ols4_1, type="HC1"))

########################

library(readxl)
X2015_2020_food_spending_update_july_2021 <- read_excel("2015-2020-food-spending_update-july-2021.xlsx", sheet = "2020")

#

Rice_Consumption_Per_Capita <- read_excel("food_expenditure/Rice Consumption Per Capita.xlsx")

#

X2015_2020_food_spending_update_july_2021$iso3c <- countrycode::countrycode(X2015_2020_food_spending_update_july_2021$Country, "country.name", "iso3c")

Rice_Consumption_Per_Capita$iso3c <- countrycode::countrycode(Rice_Consumption_Per_Capita$Country, "country.name", "iso3c")

merger <- merge(X2015_2020_food_spending_update_july_2021, Rice_Consumption_Per_Capita, by="iso3c")

merger <- merger %>% dplyr::select(1, 3, 6)

#################################

library(data.table)
SeaTS<-fread("seaTS.csv")

multiDist<-lapply(unique(SeaTS$CRN),function(x){
  
  vhiTs<-SeaTS[CRN==x,VHI]
  
  #calculate quantile (0,10th,25th percentiles)
  theQuant<-quantile(x = vhiTs,probs = c(0,.10,.25))
  #calculate how many sd away from the mean
  dist<-unname((theQuant-mean(vhiTs))/sd(vhiTs))
  
  return(c(theQuant,dist))
})
sdDist<-as.data.frame(do.call(rbind,multiDist))
names(sdDist)[4:6]<-c("SD0%","SD10%","SD25%")
sdDist$Country<-unique(SeaTS$CRN)

sdDist$iso3c <- countrycode::countrycode(sdDist$Country, 'country.name', 'iso3c')
merger <- merge(merger, sdDist, by='iso3c')

#####################

avg_coef <- mean(c(ols1$coefficients[2], ols2$coefficients[2], ols3$coefficients[2], ols4$coefficients[2]))

merger$potential_percent_increase_abs <- -(merger$`2015` * as.numeric(prices_merged %>% group_by(CRN) %>% summarise(sd=sd(avg)) %>% summarise(sd=median(sd))*avg_coef*abs(merger$`SD0%`)))

merger$potential_percent_increase <- -(merger$`2015` * as.numeric(prices_merged %>% group_by(CRN) %>% summarise(sd=sd(avg)) %>% summarise(sd=median(sd))*avg_coef*abs(merger$`SD0%`))) / merger$expenditure * 100

prices_merged$year <- as.numeric(prices_merged$year)

most_recent_price <- group_by(prices_merged, iso3c) %>% filter(year==2019) %>% summarise(price=mean(price, na.rm=T))

merger <- merge(merger, most_recent_price, by="iso3c")

#

merger <- filter(merger, iso3c %in% prices_merged$iso3c)

merger$price_increase <- -(as.numeric(prices_merged %>% group_by(CRN) %>% summarise(sd=sd(avg)) %>% summarise(sd=median(sd))*avg_coef*abs(merger$`SD0%`))) / merger$price

sds <- prices_merged %>% group_by(CRN) %>% summarise(sd=sd(avg))

sds$iso3c <- countrycode::countrycode(sds$CRN, "country.name", "iso3c")

merger <- merge(merger, sds, by="iso3c")

library(ggsci)

library(gridExtra)

a_plot <- ggplot(merger, aes(width=.75))+
  theme_classic()+
  geom_col(aes(x=Country, y=potential_percent_increase/100, fill=Country), position = "dodge")+
  xlab("")+
  ylab("% change in yearly HH food expenditure")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = scales::label_percent(accuracy = 1))+
  scale_fill_lancet()

b_plot <- ggplot(merger, aes(width=.75))+
  theme_classic()+
  geom_col(aes(x=Country, y=price_increase, fill=Country), position = "dodge")+
  xlab("")+
  ylab("% change in yearly mean rice price")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = scales::label_percent(accuracy = 1))+
  scale_fill_lancet()

library(patchwork)

c <-  b_plot + a_plot + plot_annotation(tag_levels = 'A') + plot_layout(ncol=1, guides = 'collect') 

ggsave("food_sec_plot.png", c, scale=1.1, height = 6, width = 6)

###

merger2 <- merger %>% dplyr::select(Country, expenditure, price, sd)
colnames(merger2) <- c("Country", "Yearly avg. HH. \nfood expenditure (2020)", "Rice price (2019)", "SD VHI")
rownames(merger2) <- NULL

merger2[,2:4] <- round(merger2[,2:4], 1)

stargazer::stargazer(merger2, summary = F, digits=1)




