
library(caret)
library(data.table)
source("supportFunc.R")

testSize<-52*5

timer0=Sys.time()

SEAmodels<-lapply(scenarios,function(SCEN){
  
  lapply(split.data.frame(SCEN,SCEN$CRN),function(x){
    
    

  TSsea<-x
  training<-TSsea[1:(nrow(TSsea)-testSize)]
  training<-training[,-c("Date","DateVHI","CRN","Year","sin.year","cos.year"),with=F]
  
  testing<-tail(TSsea,testSize)
  testing<-testing[,-c("Date","DateVHI","CRN","Year","sin.year","cos.year"),with=F]
  names(training)
  setdiff(names(training),names(testing))
  setcolorder(training,"VHI")
  setcolorder(testing,"VHI")
  setDF(training)
  setDF(testing)
  
  train_control <- trainControl(method = "timeslice", # for “cross-validation”
                                initialWindow = 52*10, horizon = 52*5,
                                skip = 52-1, fixedWindow = T,
                                verboseIter=TRUE,
                                allowParallel=TRUE,
                                savePredictions="final")
  
  set.seed(223)
  qrf_modelCV<-train(VHI ~ ., data = training,
                     trControl = train_control,
                     tuneLength =20,
                     method = 'ranger',
                     quantreg=TRUE)
  
  
  
  Finn<-myMetrics(model=qrf_modelCV,training = training,testing = testing)
  
  return(Finn)  
  })
  
})

timer1=Sys.time()
timer1-timer0
print("THIS IS DONE!!")

###################################################################
#                                                                 #

#                       RESULTS CHECK                             #

#                                                                 #
###################################################################

#[[[[[[[[Tab with metrics]]]]]]]]
library(purrr)
R2<-unlist(map_depth(SEAmodels,3,"r2"))
RMSPE<-unlist(map_depth(SEAmodels,3,"rmspe"))
MAPE<-unlist(map_depth(SEAmodels,3,"mape"))

MetricsTab<-as.data.table(cbind.data.frame(R2,RMSPE,MAPE), keep.rownames="ID")

MetricsTab[,c("Scen", "Country","Dataset") :=tstrsplit(MetricsTab$ID,"\\.",names=T)]
MetricsTab$ID<-NULL
setcolorder(MetricsTab,c("Scen","Country","Dataset"))
setorderv(MetricsTab,c("Country","Scen","Dataset"),c(1,1,-1))

fwrite(MetricsTab,"MetricsTab5years.csv")
# With Booktabs 
kableExtra::kable(MetricsTab, "latex", booktabs = TRUE)


##horrible results, why?
SEAmodels$M3$Cambodia$testr$r2
SEAmodels$M3$Cambodia$trainr$r2
library(ggplot2)
MetricsTab[Country=="Cambodia"]
fullPlot<-cbind.data.frame(SEAmodels$M3$Cambodia$quants,SEAmodels$M3$Cambodia$testr$testRes,
                           SEAmodels$M3$Cambodia$aux$theTesting$VHI)

names(fullPlot)<-c("q1","q9","preds","actual")
fullPlot$time<-1:(52*5)


p<-ggplot(data=fullPlot)+
  geom_line(aes(y=preds,x=time, color="#5999C7"))
p+geom_line(aes(y =actual ,x =time, color = "#ed7953"))+
  geom_ribbon(aes(y =actual ,x =time,ymin=q1, ymax=q9,color="#D2E3F0"), linetype=2, alpha=0.3,fill ="#D2E3F0")+
  theme_light()+
  scale_color_manual(
    values = c("#5999C7","#D2E3F0", "#ed7953"),
    labels = c("Predicted",'95% Prediction Interval', "Observed"))+
  labs(color = '',y= "VHI", x = "Time (weeks)")+
  theme(legend.position="bottom", legend.box = "horizontal")+
  ggtitle("Cambodia 3 month lag")

ggsave("./Plots/Cambodia_1M.png", width = 20, height = 11, units = "cm")

##############################################################################################
#[[[[[[[[WIDTH Predicted Intervals]]]]]]]]

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

actual$aux.VHI

intervalMetrics<-cbind(hope,preds,actual$aux.VHI)
setnames(intervalMetrics,c("CRN","q1","q9","Scen","preds","actual"))
setcolorder(intervalMetrics,c("Scen","CRN"))
anyNA(intervalMetrics)


intervalMetrics[,width:=q9-q1]
intervalMetrics[,interval_pred_ratio:=width /preds]
intervalMetrics[,mean_interval_width_percentage:=mean(interval_pred_ratio)*100,by=c("CRN","Scen")]
intervalMetrics[,stdev:=sd(interval_pred_ratio),by=c("CRN","Scen")]

library(scoringutils)

intervalGneiting<-as.data.table(interval_score(
  true_values=intervalMetrics$actual,
  lower=intervalMetrics$q1,
  upper=intervalMetrics$q9,
  interval_range=80,
  weigh = TRUE,
  separate_results = T
))

intervalMetrics<-cbind.data.frame(intervalMetrics,intervalGneiting)
setDT(intervalMetrics)
#intervalMetrics[,theMis:=greybox::MIS(holdout=actual,lower = q1,upper = q9,level = 0.90),by=c("CRN","Scen")]
#intervalMetrics[,list(mean(width),mean(mean_interval_width_percentage),mean(stdev)),by=c("CRN","Scen")]



fwrite(intervalMetrics,"intervalMetrics5years.csv")







