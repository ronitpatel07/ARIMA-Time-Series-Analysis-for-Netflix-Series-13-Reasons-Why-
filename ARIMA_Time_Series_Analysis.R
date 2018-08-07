install.packages("forecast")
install.packages("gtrendsR")
install.packages("ggplot2")

library(gtrendsR) #library to import google trend data
library(forecast) #library to make ARIMA forecast
library(ggplot2)  #library for visualization
#Getting google trend data for suicide phrase except used with squad in US 
#from 15th Jan, 2017 to 18th April,2017
suicide_df<- gtrends("suicide - squad", geo = "US", time = "2017-01-15 2017-04-18")
suicide_df<-suicide_df$interest_over_time
suicide_df

#Splitting data into train and test based on date, data before 31st Mrch, 2017
#is used as training data
suicide_train<-suicide_df[1:75, 1:2]
suicide_train
suicide_test<-suicide_df[76:94, 1:2]
test_suicide<-suicide_test

#Visualizing time series data for training and testing dataset
ggplot(suicide_train, aes(x=date, y=hits))+
  geom_line()
ggplot(suicide_test, aes(x=date, y=hits))+
  geom_line()

#Converting dataframe into time-series
train_ts<-ts(suicide_df$hits[1:75], frequency=12, start=c(12,1))
test_ts<-ts(suicide_df$hits[76:94])
train_ts
test_ts

#time series decomposition plot
train_ts.stl<-decompose(train_ts)
ts.plot(train_ts.stl$trend)
ts.plot(train_ts.stl$seasonal)

#Determining if there is need for making first differences to make time-series non-seasonal
ndiffs(train_ts)


#Autocorrelation and partial correlation plots
acf(train_ts, lag.max = 50)
pacf(train_ts, lag.max=50)

#Fitting ARIMA model
fitARIMA <- arima(train_ts, order=c(2,0,1))

#Making prediction
pre<-predict(fitARIMA, n.ahead = 19)
for (i in pre[1]){
  suicide_test[2]<-i
}
suicide_test

actual<-(test_ts)
Final<-data.frame(actual, pred)
names(Final) <- c("test_ts","pre")
View(Final)

#Visualizing prediction
ggplot(suicide_train, aes(x=date, y=hits))+
  geom_line()+
  geom_line(data = suicide_test, aes(x=date, y=hits), colour='red')+
  geom_line(data=test_suicide, aes(x=date, y=hits))

#Checking the average difference between real value and prediction in term of percentage
for (i in pre[1]){
  pred<-as.integer(i)
}
pred
mean_test<-mean(test_ts)
mean_test
mean_pred<-mean(pred)
mean_pred
avg_diff<-((mean_test-mean_pred)/mean_pred)*100
avg_diff

#Checking average difference on April 18th 
Apr_18<-((test_ts[19]-pred[19])/mean_pred)*100
Apr_18

#Checking error
error <- sqrt((test_ts-pred)^2)
error <- sum(error)
error
