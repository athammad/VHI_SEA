library(data.table)
library(ggplot2)
library(purrr)
options(bitmapType="cairo")




QpercPlot<-map_depth(SEAmodels,3,"PredsQ")
QpercPlot<-map_depth(QpercPlot,2,"quants")
QpercPlot<-lapply(QpercPlot,lapply, as.data.table)

hope<-rbindlist(flatten(QpercPlot),use.names = T, idcol="ID")
dim(hope)
hope$Scen<-rep(names(QpercPlot),each=(52*5)*9)

preds<-map_depth(SEAmodels,3,"testRes")
preds<-lapply(preds,lapply, as.data.table)
preds<-rbindlist(flatten(preds),use.names = T)

actual<-map_depth(SEAmodels,3,"theTesting")
actual<-lapply(actual,lapply, as.data.table)
actual<-rbindlist(flatten(actual),use.names = F)

intervalMetrics<-cbind(hope,preds,actual$aux.VHI)
setnames(intervalMetrics,c("CRN","q1","q9","Scen","preds","actual"))
setcolorder(intervalMetrics,c("Scen","CRN"))
anyNA(intervalMetrics)
intervalMetrics$time<-rep(rep(scenarios$W2[CRN=="Indonesia",list(value=last(Date,52*5))]$value,4),9)


########################################################################################################
allPlotsIntervals<-lapply(unique(intervalMetrics$CRN),function(x){
  
camPlot<-intervalMetrics[CRN==x,]
camPlot$time<-rep(scenarios$W2[CRN==x,list(value=last(Date,52*5))]$value,4)

#RColorBrewer::brewer.pal(4,"Blues")

p<-ggplot(camPlot[Scen=="W2",], aes(time)) + 
geom_ribbon(data=camPlot[Scen=="M3"], aes(ymin = q1, ymax = q9,color="3 Months"), fill = "#EFF3FF") +
geom_ribbon(data=camPlot[Scen=="M2"], aes(ymin = q1, ymax = q9,color="2 Months"), fill = "#BDD7E7") + 
geom_ribbon(data=camPlot[Scen=="M1"], aes(ymin = q1, ymax = q9,color="1 Month"), fill = "#6BAED6") + 
geom_ribbon(data=camPlot[Scen=="W2"], aes(ymin = q1, ymax = q9,color="2 Weeks"), fill = "#2171B5") + 
geom_line(data=camPlot[Scen=="W2"],aes(y = actual), color = "orange") + 
labs( y = "VHI", x = "Time") + 
scale_colour_manual("Intervals:",
  #  values = c("3 Months"="#EFF3FF",'2 Months'="#BDD7E7","1 Month"= "#6BAED6","2 Weeks"="#2171B5"))+
  values = c("2 Weeks"="#2171B5","1 Month"= "#6BAED6",'2 Months'="#BDD7E7","3 Months"="#EFF3FF"))+
theme(legend.position="bottom", legend.box = "horizontal")+
  ggtitle(paste0(x, ", 5 years predicted Quantile Intervals"))+
guides(color = guide_legend(override.aes = list(fill =rev(RColorBrewer::brewer.pal(4,"Blues"))))) 



})

names(allPlotsIntervals)<-unique(intervalMetrics$CRN)

allPlotsIntervals$Vietnam


save(allPlotsIntervals,file ="allPlotsIntervals.RData")




