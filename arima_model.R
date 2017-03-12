library(forecast)

#Clean Env.
rm(list = ls())

#Adding files to filelist
fileList = list.files(pattern="project*")

#Extract Data
dataset = data.frame( "SeqTime" = integer(), "NumReq" = integer(), "Predict" = integer(), stringsAsFactors=FALSE)
for (i in 1:length(fileList)){
  dataframe = read.csv(file = fileList[i], sep = " ", header=F, row.names=1)[,1:3]
  x = sum(dataframe$V3)
  dataset[nrow(dataset) + 1, ] <- c( i, x, 0)
}

#Start time_series prediction
for (j in 505:805){
  start = j-504
  end = j-1
  prediction = forecast(auto.arima(dataset[start:end, 2]), h=1)
  dataset$Predict[j] = prediction$mean[1]
}

#Plot Figures
plot(dataset[505:805,1], dataset[505:805,2], type="l", xlab="Time(hour)", ylab="Number of Request")
lines(dataset[505:805,1], dataset[505:805,3], lty=4, col="red")

#Print the accuracy
print(accuracy(dataset[505:805,2],dataset[505:805,3]))

#plot(dataset[505:805,], type="l", xlab="Time(hour)", ylab="Number of Request")
#prediction = forecast(auto.arima(dataset[1:504,2]), h=1)
#prediction$mean[1]
