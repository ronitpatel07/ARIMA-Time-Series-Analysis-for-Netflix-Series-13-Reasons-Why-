# ARIMA-Time-Series-Analysis-for-Netflix-Series-13-Reasons-Why-

# Task
For this task you will use Google trends data to try to reproduce the results presented by the authors of the Netflix study: https://gizmodo.com/research-finds-disturbing-suicide-search-trends-followi-1797398484

To successfully carry out this task, you’ll need to explore various combinations of Google trends search terms to arrive at a clean time series with the appropriate detail level. You can use inclusion or exclusion of certain terms, see https://support.google.com/trends/answer/4359582?hl=en
Follow the description of the process used by the authors of the study to try to arrive at approximately the same time series. 

Once you have found a reasonable time series, use an ARIMA model to forecast what would have happened if the Netflix series hadn’t aired. Then compare the forecasts with the actuals and see whether you can see the increased interest in suicide that the authors report. 

In your report, in addition to a description of the process you followed to arrive at your test time series, and the results you obtained, you must include the process you follow to choose the parameters of the model (that is, your ACF and PACF plots, how you used them, etc.). 

# Solution
I used R library “gtrendsR” to extract google trend data for the word suicide used with all other words except squad.
I selected geological region as United States and picked dates from 01/15/2017 to 04/18/2017. 
Then, I created data frame containing just two columns data and hits and split it into two part train data which contains data from 01/15/2017 to 03/30/2017 and test data from 03/31/2017 to 04/18/2017. 
Using ggplot, I created time-series plot for training data and testing data by utilizing date and hits columns. After that I converted both training and testing data-frame into time series for seasonality-trend decomposition. 
Then, I checked if the data is non-stationary and determined number of differences needed to make time-series non-seasonal by using ndiffs() function in R. But, there was no such requirements. 
I plotted autocorrelation and partial autocorrelation plot using acf() and pacf() function in R to determine the value for p and q order for ARIMA model. For ACF, the lag series decayed after two lags and in case of PACF, it decayed after one lag.
Hence, I choose p order as 2, q order as 1 and d order 0 as there was no need to implement differences because data was stationary. 
Using, predict function I forecasted time-series from 03/31/2017 to 04/18/2018 on fitted ARIMA model with (2,0,1) p, d, q order. 
I plotted the predicted values against test dataset using ggplot2 visualization which is shown below. I calculated that the average values of test data were 19% higher than average predicted values.
Also, the real value on 18th April, 2017 was 43% higher than forecasted value. 

