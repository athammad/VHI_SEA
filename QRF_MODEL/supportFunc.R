#==========#ALL RESULTS ON TRAINING & TESTING
myMetrics<-function(model,training=NULL,testing=NULL){
  #train
  trainr<-list(
    trainRes<-predict(model,training[,-1]),
    r2=caret::R2(trainRes,training[,1]),
    rmspe=MLmetrics::RMSPE(trainRes ,training[,1]),
    mape=MLmetrics::MAPE(trainRes ,training[,1]))
  
  names(trainr)[1]<-"trainRes"
  #Test
  testr<-list(
    testRes<-predict(model,testing[,-1]),
    r2=caret::R2(testRes,testing[,1]),
    rmspe=MLmetrics::RMSPE(testRes ,testing[,1]),
    mape=MLmetrics::MAPE(testRes ,testing[,1]))
  
  names(testr)[1]<-"testRes"
  #Quantiles
  quants<-list(
    PredsQ = predict(model$finalModel,testing[,-1], 
                     type = "quantiles", quantiles = c(0.1, 0.9))$predictions
  )
  
  aux<-list(
    theModel=model,
    theTraining=training,
    theTesting=testing
  )
  #----------------------------------#
  Mylist<-list(trainr,testr,quants,aux)
  names(Mylist)<-c("trainr","testr","quants","aux")
  return(Mylist)
  
  
}

#check
#respect<-myMetrics(model=qrf_modelCV,training = training,testing = testing)

