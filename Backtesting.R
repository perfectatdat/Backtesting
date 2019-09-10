library(devtools)
devtools::install_github("ramamet/nse1minR") ##Doesn't work

#####Convert the time frame from 1 min to 15 min####
library(xts)
nifty_1min<-nifty_1min.copy
str(nifty_1min)
nifty_1min$date<-as.POSIXct(as.character(nifty_1min$date),format = "%Y%m%d")

nifty_1min_ohlc<-nifty_1min[,c("open","high","low","close")]
row.names(nifty_1min_ohlc)<-as.POSIXct(paste(nifty_1min$date,nifty_1min$time),format="%Y-%m-%d %H:%M")

nifty_1min_ohlc<-xts(nifty_1min_ohlc,order.by = as.POSIXct(paste(nifty_1min$date,nifty_1min$time),format="%Y-%m-%d %H:%M"))
nifty_15min_ohlc = to.minutes15(nifty_1min_ohlc)
nifty_5min_ohlc = to.minutes5(nifty_1min_ohlc)
nifty_5min_ohlc = as.data.frame(nifty_5min_ohlc)
library(TTR)
nifty_15min_ohlc$RSI<-RSI(nifty_15min_ohlc$nifty_1min_ohlc.Close,n=14)
######Filtering dates when the RSI was between 30 and 70 ##########
nifty_15min_ohlc<-as.data.frame(nifty_15min_ohlc)
nifty_15min_ohlc$Date<-as.Date(row.names(nifty_15min_ohlc))

IntradayConsolidation<-aggregate(RSI~Date,data=nifty_15min_ohlc, FUN = function(x){if(max(x)< 70 & min(x)>30) TRUE else FALSE})

IntradayConsolidation<-subset(IntradayConsolidation,RSI ==TRUE, select = Date)

View(IntradayConsolidation[which(IntradayConsolidation$RSI==T),])

View(IntradayConsolidation[which(IntradayConsolidation$RSI==T)+1,])

ORB_Dates<-IntradayConsolidation[which(IntradayConsolidation$RSI==T)+1,"Date"]

ORB_Dates<-ORB_Dates[complete.cases(ORB_Dates)]


#####Calculating technicals on the 5min OHLC chart#########
library(TTR)
nifty_5min_ohlc$EMA_5<-EMA(nifty_5min_ohlc$nifty_1min_ohlc.Close,n=5)
nifty_5min_ohlc$EMA_13<-EMA(nifty_5min_ohlc$nifty_1min_ohlc.Close,n=13)
nifty_5min_ohlc$EMA_21<-EMA(nifty_5min_ohlc$nifty_1min_ohlc.Close,n=21)

nifty_5min_ohlc$Date<-as.Date(row.names(nifty_5min_ohlc))
nifty_5min_ohlc$Time<-strftime(row.names(nifty_5min_ohlc), format = "%H:%M")

#####Getting the first 30min High and Low for each day #######
High_30min<-aggregate(nifty_1min_ohlc.High~Date,nifty_5min_ohlc,FUN = function(x){max(x[1:6])})
colnames(High_30min)[2]<-"High_30min"
Low_30min<-aggregate(nifty_1min_ohlc.Low~Date,nifty_5min_ohlc,FUN = function(x){min(x[1:6])})
colnames(Low_30min)[2]<-"Low_30min"

library(plyr)
nifty_5min_ohlc<-join(nifty_5min_ohlc,High_30min,by="Date",type="left")
nifty_5min_ohlc<-join(nifty_5min_ohlc,Low_30min,by="Date",type="left")
##############################################################
# library(dplyr)
# View(nifty_5min_ohlc %>% group_by(Date) %>% select(Date,Time,Close_5min=nifty_1min_ohlc.Close,EMA_5,EMA_13,EMA_21,High_30min, Low_30min)%>% filter(row_number() %in% 7:n())
#      %>% mutate(Position = 0) 
#      %>% mutate(Buy = if_else(EMA_5>High_30min & Position == 0,1,0))
#      %>% mutate(Sell = if_else(EMA_5<Low_30min & Position == 0,1,0))
#      %>% mutate(Position = if_else(Buy ==1 | Sell ==1,1,0))
#      %>% mutate(Sell = if_else(Position==1 & Close_5min < EMA_13,1,0))
#      %>% mutate(Position = 0)
# )

nifty_5min_ohlc$Buy<-0
nifty_5min_ohlc$Sell<-0
nifty_5min_ohlc$BuyPosition<-0
nifty_5min_ohlc$SellPosition<-0


####Renaming the columns properly#########
nifty_5min_ohlc<-nifty_5min_ohlc %>% rename(Open = nifty_1min_ohlc.Open,High=nifty_1min_ohlc.High,Low=nifty_1min_ohlc.Low,Close=nifty_1min_ohlc.Close)

# Net_PL<-rep(0,length(unique(nifty_5min_ohlc$Date)))
#Size of capital####

capital<-30000
num_of_trades<-c()
Net_PL<-rep(0,length(ORB_Dates))
k=1
# for (i in unique(nifty_5min_ohlc$Date)){
for (i in ORB_Dates){
  s = subset(nifty_5min_ohlc,Date == i)
  if((s$High_30min[1]-s$Low_30min[1])<0.02*s$Low_30min[1]){
    if(capital>0){
    for (j in 7:nrow(s)-1){
    # j=nrow(s)
    #####When no open positions are there#####
    if(sum(s$BuyPosition,s$SellPosition) %% 2 == 0) #& s$Close[j] > s$EMA_5[j])
    {
    # if(sum(s$Buy) %% 2 == 0){s$Buy[j] = ifelse(s$EMA_5[j]>s$High_30min[j],1,0)}
    # if(sum(s$Sell) %% 2 ==0){s$Sell[j] = ifelse(s$EMA_5[j]<s$Low_30min[j],1,0)}  
    s$Buy[j] = ifelse(s$Close[j] > s$EMA_5[j] & s$EMA_5[j]>s$High_30min[j],floor(capital/s$Close[j]),0)
    s$Sell[j] = ifelse(s$Close[j] < s$EMA_5[j] & s$EMA_5[j]<s$Low_30min[j],floor(capital/s$Close[j]),0)
    s$BuyPosition[j] = ifelse(s$Buy[j]>=1,1,0)
    s$SellPosition[j] = ifelse(s$Sell[j]>=1,1,0)
    }
    ######Open positions exist###########
    else{
    # if(sum(s$Buy) %% 2 != 0) {s$Sell[j] = ifelse(s$Close[j] < s$EMA_13[j] ,1,0)}
    # if(sum(s$Sell) %% 2 != 0) {s$Buy[j] = ifelse(s$Close[j] > s$EMA_13[j],1,0)}
    if(sum(s$BuyPosition)>sum(s$SellPosition)){s$Sell[j] = ifelse(s$Close[j] < s$EMA_13[j] ,tail(s$Buy[s$Buy!=0],1),0)}
    if(sum(s$SellPosition)>sum(s$BuyPosition)){s$Buy[j] = ifelse(s$Close[j] > s$EMA_13[j],tail(s$Sell[s$Sell!=0],1),0)}
    s$BuyPosition[j] = ifelse(s$Buy[j] >= 1,1,0)
    s$SellPosition[j] = ifelse(s$Sell[j]>=1,1,0)
    }
  }
  ###rows for a day j loop
  #####Squaring off positions at day-end##########
  j=j+1
  if(j==nrow(s) & sum(s$Buy)>sum(s$Sell)){
    s$Sell[j]<-if(length(tail(s$Buy[s$Buy!=0],1))==0) {0} else {tail(s$Buy[s$Buy!=0],1)}
    s$SellPosition[j]<-1
  } else {if(j==nrow(s) & sum(s$Sell)>sum(s$Buy)) {
      s$Buy[j]<-if(length(tail(s$Sell[s$Sell!=0],1))==0) {0} else {tail(s$Sell[s$Sell!=0],1)}
      s$BuyPosition[j]<-1}}
  s$BuyPosition[s$BuyPosition==1]<-"Buy" 
  s$SellPosition[s$SellPosition==1]<-"Sell"
  s$BuySell<-paste(s$BuyPosition,s$SellPosition,sep = "")
  s$BuySell<-gsub("0","",s$BuySell)
  num_of_trades<-c(num_of_trades,length(s$BuySell[s$BuySell!=""]))
  Prices<-s[which(s$BuySell!= ""),c("BuySell","Close")]
  Net_PL[k]<-sum(Prices[which(Prices$BuySell=="Sell"),"Close"]*s[which(s$BuySell=="Sell"),"Sell"]-Prices[which(Prices$BuySell=="Buy"),"Close"]*s[which(s$BuySell=="Buy"),"Buy"])
  capital<-capital + Net_PL[k] ###Updating the net capital at the end of the day
  k =k+1
    }
  }
}### unique date i loop



i=ORB_Dates[25]
j = index(s$Time)[s$Time=="12:14"]

library(ggplot2)
library(plyr)
library(dplyr)
s$Time<-as.POSIXct(strptime(s$Time,format = "%H:%M"))
s %>% ggplot(aes(x = Time)) + 
  geom_point(aes(y = Close)) +
  geom_line(aes(y = EMA_5), color = "red") + 
  geom_line(aes(y = EMA_13), color = "blue") +
  geom_line(aes(y = EMA_21), color = "green") +
  geom_line(aes(y = High_30min), color = "grey") +    
  geom_line(aes(y = Low_30min), color = "grey")
  # geom_text(aes(label=Buy))
  
i<-unique(nifty_5min_ohlc$Date)[2]


#########P&L assessment############
summary(Net_PL)
hist(Net_PL)

sum(Net_PL)###Total Income###
summary(ORB_Dates)

length(Net_PL[Net_PL!=0]) ###Number of days traded###
sum(num_of_trades) ###Total number of trades##
hist(num_of_trades) ###Number of trades per day
library(plm)
index(num_of_trades)[num_of_trades %% 2 !=0]

###Average Income per day###
sum(Net_PL)/length(Net_PL[Net_PL!=0])

###Brokerage cost#####
sum(num_of_trades)*10


