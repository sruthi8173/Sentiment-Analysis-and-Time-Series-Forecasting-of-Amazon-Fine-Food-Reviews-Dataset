#Copyright reserved 
#svenka15, nthanik, vchitto 
#BI Capstone project 


#########################################################

#TIME SERIES ANALYSIS

#########################################################
library(data.table)
library(zoo)
library(forecast)
library(xts)
data_time<-fread("preprocessed_reviews.csv")
head(data_time)
data_time[,6]<-(as.POSIXct(unlist(data[,6]),origin="1970/01/01"))
#as.POSIXct(1.563900e+04,origin="1970/01/01")

data_new_time<-data_time[,5:6]
##distinct 
dates_uni<-unique(data_new_time[,2])
dates_uni<-sort(dates_uni[[1]])
year_unique<-year(dates_uni)
mon_unique<-month(dates_uni)
new_rows<-paste(mon_unique,year_unique)
uni_dates<-unique(new_rows)


#hj_final <- data.frame( "time" = as.Date(character()), "Score" = integer(),stringsAsFactors = FALSE)
#########################################################################################

#PREPROCESS TO 12 MONTHS A YEAR

#######################################################################################
rm(score_unique)
score_unique<-0
i=1
kl<-0
for (k in 1999:2012)
{ 
  #hj<-data_new[data_new$Time==dates_uni[k][[1]],][[2]]
  for(j in 1:12)
  { print(i)
    nor<-NaN
    nor<-mean(data_new_time[month(data_new_time$Time)==j & year(data_new_time$Time)==k,][[1]])
    #print(count)
    #count=count+1
    if(is.nan(nor))
    {
      score_unique[i]=NaN
    }
    else
      score_unique[i]=nor
    
    
    kl[i]<-paste(j,k)
    i=i+1
  }
  #print(hj_final[i,1])
  
  
  
}
#binding new data
data_new1_time<-cbind(kl,score_unique)

########################################################################
#ets model
########################################################################
#Stationery and creation of time series 
plot(data_new)

time_series=ts(score_unique,start=1999,frequency=12)
plot(time_series,ylim=c(1,5),ylab="Scores")
plot(BoxCox(time_series,lambda=1))
#Boxcox transformation is not required as the plot obtained is similar to the original time series plot.  
kpss.test(time_series)
#P value is 0.1, hence stationery

acf(time_series,na.action = na.pass)  
pacf(time_series,na.action = na.pass)
#No gradual drop, so stationery 

model1<-ets(time_series)
fore<-forecast(model1,10)

plot(fore)
model1[2]
#AIC value is 178.87
############################################################################
#arima
#########################################################################
model5<-auto.arima(time_series)
plot(forecast(model5,10))
forecast(model5,10)
#AIC is 182.04

model2<-arima(time_series,order=c(0,0,0))
summary(model2)
#aic =207.79
fore<-forecast(model2,10)
plot(fore)
################################################################3
#holt
##################################################################
model3<-holt(time_series,10)
#aic = 226.1606
summary(model3)
fore<-forecast(model3,10)
plot(fore)
#####################################################################3
#ses
#######################################################################
model4<-ses(time_series,10)
#aic= 222.5346
summary(model4)
fore<-forecast(model4,10)
plot(fore)
###############################################################################
#snaive
###############################################################################
model5<-snaive(time_series,10)
summary(model5)
fore<-forecast(model5,10)
plot(fore)
model2[6]

