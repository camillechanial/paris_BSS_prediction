data_openbikes=read.csv("/Users/camillechanial/Desktop/DATA/data_neighbors/paris/10017-gare-de-lest-saint-laurent.csv")
data_api=read.csv("/Users/camillechanial/Desktop/DATA/data_neighbors_api/10017-gare-de-lest-saint-laurent.csv")
data = rbind(data_openbikes,data_api)
fitControl <- trainControl(method = "repeatedcv", number = 10,repeats = 10) 
Sys.setenv(TZ='GMT')
data$moment=fastPOSIXct(data$moment,tz="GMT")
n=length(data$bikes)
bikes_before=rep(0,n)
timediff=rep(0,n)
data<-cbind(data,bikes_before,timediff)
for(i in (2:n)) {
  data$bikes_before[i]=data$bikes[i-1]
  data$timediff[i]=abs(as.numeric(data$moment[i]-data$moment[i-1]))
}
#data<-data[2:n,]
#data_train<-data[2:floor(8*n/10),]
data_train<-data
#model <- train(bikes ~ heure.normalisee+jour.semaine.normalise+description.grade+temperature.grade+wind.grade+is.weekday+qualitytime_1+qualitytime_2+qualitytime_3+bikes_1+bikes_2+bikes_3, data, na.action=na.omit, method = "gbm", trControl = fitControl,verbose = FALSE)
#model <- train(bikes ~ heure.normalisee+jour.semaine.normalise+description.grade+temperature.grade+wind.grade+is.weekday, data, na.action=na.omit, method = "gbm", trControl = fitControl,verbose = FALSE)
model <- train(bikes ~ heure.normalisee+jour.semaine.normalise+description.grade+temperature.grade+wind.grade+is.weekday+qualitytime_1+qualitytime_2+qualitytime_3+bikes_1+bikes_2+bikes_3+bikes_before+timediff, data_train, na.action=na.omit, method = "gbm", trControl = fitControl,verbose = TRUE)

print(model$results)

##########Sauvegarde du modèle###########
saveRDS(model, "model.rds")



##########Prediction simple#############
k=n
topredict=data[k,]
topredict=topredict[,c("heure.normalisee","jour.semaine.normalise","description.grade","temperature.grade","wind.grade","is.weekday","qualitytime_1","qualitytime_2","qualitytime_3","bikes_1","bikes_2","bikes_3","bikes_before","timediff")]
pred<-predict(model,topredict)
pred
data$bikes[k]

#########Plot en fonction de bikes before#########
k=n-20
j=20
topredict=topredict=data[rep(k, each = 2),]
timediff=rep(0,j)
error=rep(0,j)
index=rep(0,j)
topredict=topredict[,c("heure.normalisee","jour.semaine.normalise","description.grade","temperature.grade","wind.grade","is.weekday","qualitytime_1","qualitytime_2","qualitytime_3","bikes_1","bikes_2","bikes_3","bikes_before","timediff")]
for(i in (0:j)){
  topredict$bikes_before=c(data$bikes_before[k-i],data$bikes_before[k-i])
  topredict$timediff=c(abs(as.numeric(data$moment[k]-data$moment[k-i])),abs(as.numeric(data$moment[k]-data$moment[k-i]))) 
  pred<-predict(model,topredict)
  err=abs(data$bikes[k]-pred[1])
  print("timediff=")
  print(topredict$timediff[1])
  print(err)
  error[i]=err
  timediff[i]=topredict$timediff[1]
  index[i]=i
}
plot(timediff,error)
plot(index,error,"line")
