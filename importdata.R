# Fin_db_mgmt_2019
# Reference:
# 1. https://rpubs.com/mohammadshadan/288218
# 2. MANIPULATING TIME SERIES DATA IN R WITH XTS & ZOO, datacamp course slide.
#-----------------------------------------------------------------------------
# Using read.table to import .txt file
rm(list=ls())
etf4<-read.table("ETF4_2000_2018_d.txt")
# you will get error message! This is because Chinese characters that cannot be recognized!
# But this works fine in Mac!!!
# But you can add: fileEncoding = 'UTF-8-BOM' to solve the problem!
etf4<-read.table("ETF4_2000_2018_d.txt", fileEncoding = "UTF-8-BOM")
head(etf4)
# Or you can save the .txt as ansi file fromat using notepad
etf4<-read.table("ETF4_2000_2018_d_ansi.txt", header = T)
# You will get error message in Mac
str(etf4)
# change imoprted data types
etf4<-read.table("ETF4_2000_2018_d_ansi.txt", header = T, stringsAsFactors = T, 
                 colClasses = c("證券代碼"="character"))
str(etf4)
head(etf4)
# Using read.csv to import .csv file
etf4<-read.csv("ETF4_2000_2018_d.csv", colClasses = c("證券代碼"="character"))
str(etf4)
# If you get garbled text, you may try:
etf4.csv<-read.csv("ETF4_2000_2018_d.csv", fileEncoding='big5', 
                   colClasses=c('factor', 'factor', 'factor', 'numeric', 'numeric'))
head(etf4.csv)
str(etf4.csv)
# using read_csv to imoprt data to tibble format
# install.packages("readr")
# library(readr)
ifelse(!require(readr), install.packages('readr'), library(readr))
#
etf4_csv<-read_csv("ETF4_2000_2018_d.csv")
# you will get garbled text!
etf4_csv<-read_csv("ETF4_2000_2018_d.csv", locale = locale(encoding='big5'))
head(etf4_csv)
str(etf4_csv)
# read xls file
ifelse(!require(readxl), install.packages('readxl'), library(readxl))
#
etf4_xls<-read_excel("ETF4_2000_2018_d.xls", 
                     col_types =c("text", "text","text", "numeric","numeric"))
head(etf4_xls)
#----------------------------------------------------------------------------
# Practice 01:
# Try to import '2017Q4_code.csv'
#-----------------------------------------------------------------------------
tw50_2017<-read_csv("2017Q4_code.csv", locale = locale(encoding='big5'))
head(tw50_2017)
#
ifelse(!require(quantmod), install.packages('quantmod'), library(quantmod))
#
code50<-tw50_2017$code
code50.tw <- paste(code50, ".TW", sep="")
code50.tw
#
#tickers<-c("2330.TW", "1101.TW", "1102.TW")
#getSymbols(tickers, from= '2016-01-01', auto.assign = T)
#
data=new.env()
getSymbols(code50.tw, from= '2016-01-01', env = data, auto.assign = T)
names(data)
data$`1101.TW`
# Error: 2311.TW download failed after two attempts. Error message:
# Replace 2311 with 3711
getSymbols("3711.TW", from= '2016-01-01', auto.assign = T)
data$"3711.TW"<-`3711.TW`
# Combine adjusted closing price across all 50 stocks!
str(data)
ls(data)
tw50p<-data.frame()

t<-as.data.frame(as.list(data))
dim(t)
head(t)
head(t[1])

for (i in c("1101.TW", "1102.TW")) {
    tmp<-paste(data)
    tw50p<-cbind(tw50p, Ad(data$"1101.TW"))
}
#--------------------------------------------------
# 錯誤處理：
# Reference: 
# tryCatch()
# https://www.cnblogs.com/weibaar/p/4382397.html
# http://xuyt.blogspot.com/2013/10/rtry-and-catch.html
# try()
# http://www.endmemo.com/program/R/try.php
# inherits()
# http://www.learn-r-the-easy-way.tw/chapters/5
#---------------------------------------------------
# use loop to solve error problem
# i=1
# for(i in 1:length(code50.tw)) {
#   symbol <- code50.tw[i]
#   tryit <- try(getSymbols(symbol,from="2016-01-01", src='yahoo'))
#   # specify the "from" date to desired start date
#   if(inherits(tryit, "try-error")){
#     i <- i+1
#   } else {
#     data <- getSymbols(symbol, from="2016-04-27", src='yahoo')# specify the "from" date to desired start date
#     dataset <- merge(dataset, Cl(get(name[i])))#將所有股票的收盤價 Cl 合併成一個 data frame
#     rm(symbol)
#   }
# }
#------------------------------------------------------------------------------
# 將沒有找到的股票error找出，並將結果輸出為NULL
show_condition <- function(code){
  tryCatch(code, 
           error = function(c){print("error"); return(NULL)},
           warning = function(c){print(paste("Caught warning message:", symboli))},
           message = function(c){symboli}
           )
}
# 將沒有找到的股票error找出，並將結果輸出為NULL
all.data<-c()
TSE_tw50_symbols_yahoo<-c()
#
symboli = "1101.TW"
for (symboli in code50.tw) {
  yx = show_condition(getSymbols(symboli, from = '2016-01-01', index.class = 'Date'))
  Sys.sleep(0.2 + runif(1)/2) # 不要一直抓，讓系統短暫休息
  if(is.null(yx)){
    for (hx in seq(1)){
      yx = show_condition(getSymbols(symboli, from = '2016-01-01', index.class = 'Date'))
      Sys.sleep(0.2 + runif(1)/2) # 不要一直抓，讓系統短暫休息
      print(paste("重試", hx, "次"))
    }
   }
  if(!is.null(yx)){ #無錯誤就繼續抓
    print(c(i, symboli))
    getSymbols(symboli, from = '2016-01-01', index.class = 'Date')
    all.data<-cbind(all.data, get(symboli))
    TSE_tw50_symbols_yahoo<-cbind(TSE_tw50_symbols_yahoo, yx)
    }
}


all.data
dim(all.data)
head(all.data)
tw50<-Cl(all.data)
head(tw50)
tw50<-na.omit(tw50)
tw50
#=============================================================================
# clean data
etf4.c<-etf4_csv[, c(-2, -4)]
etf4.c<-etf4.c[-1,]
colnames(etf4.c)<-c("id", "date", "price")
# use pipe operator 
library(magrittr)
#install.packages("dplyr")
library(dplyr)
etf4.c<-etf4_csv%>%select(c(1,3,5))%>%rename("id" = "證券代碼", "date"= "日期", "price" = "當日均價(元)")
etf4.c
#etf4.c<-etf4_csv%>%select("證券代碼", "日期", "當日均價(元)")
#-----------------------------------------------------------------                
# use dcast to reorder dataframe by date;
#install.packages("reshape2")
library(reshape2)
etf4.reorder = dcast(etf4.c, date~id)
dim(etf4.reorder)
head(etf4.reorder)
str(etf4.reorder)
# convert into date format using as.Date()
etf4.reorder$date<-as.Date(as.character(etf4.reorder$date), "%Y%m%d") 
head(etf4.reorder)
str(etf4.reorder)
# convert character into numeric 
# convert to xts
#install.packages("xts")
library(xts)
etf4.xts<-xts(etf4.reorder[,-1], order.by = etf4.reorder$date)
head(etf4.xts)
tail(etf4.xts)
str(etf4.xts)
saveRDS(etf4.xts, "etf4_xts_all")
#----------------------------------------------
# Handling missingness in your data 
#----------------------------------------------
# Last obs. carried forward
#etf4.xts$`0050`['2018-12-27']<-NA 
#tail(etf4.xts)
etf4.xts<-na.locf(etf4.xts)                
tail(etf4.xts)
# Next obs. carried backward
etf4.xts.fill<-na.locf(etf4.xts, fromLast = TRUE) 
head(etf4.xts.fill)
#-------------------------------------------------
# delete NA values
etf4.xts<-na.omit(etf4.xts)
head(etf4.xts)
# or complete cases
#install.packages("tidyr")
library(tidyr)
etf4.xts1<-etf4.xts[complete.cases(etf4.xts),]
head(etf4.xts1)
#------------------------------------------------------
# lag operator
lag_x <- lag(etf4.xts$`0050`, 1)
head(lag_x)

#-----------------------------------------------------------
# export data
#----------------------------------------------------------
write.csv(etf4.xts1, file = "myetf4.csv")
# date index disappears!!!
# you have to use write.zoo to save .xts file
# write.zoo(etf4.xts, sep = ',', file = "myetf4.csv.1")
saveRDS(etf4.xts, file = "etf4.xts.rds")
etf4.xts2 <- readRDS("etf4.xts.rds")
head(etf4.xts2)
##
etf4.zoo <- read.zoo("myetf4.csv.1", header = TRUE, index.column =1, 
                     sep = ",", format = "%Y-%m-%d")
head(etf4.zoo)
class(etf4.zoo)
etf4.xts3<-as.xts(etf4.zoo)
head(etf4.xts3)
#=============================================
# Querying for data
#=============================================
etf4_2016<-etf4.xts['2016']
etf4_2016_01_06 <- etf4.xts["20160101/20160630"]
head(etf4_2016_01_06)

#------------------------------------------------------------
# Converting Daily Prices to Monthly Returns in the xts world
#------------------------------------------------------------
#install.packages('quantmod')
library(quantmod)
etf4_monthly <- to.monthly(etf4.xts, indexAt = "lastof", OHLC=FALSE)
head(etf4_monthly)
# convert daily prices to weekly returns
etf4_weekly<-to.weekly(etf4.xts, indexAt = "lastof", OHLE = FALSE)
head(etf4_weekly)
dim(etf4_weekly)
# however, you will lose column names
# you can use the following to keep column names
etf4_weekly <- etf4.xts[endpoints(etf4.xts, on="weeks", k=1), ]
head(etf4_weekly)
dim(etf4_weekly)
#
#install.packages('PerformanceAnalytics', 'magrittr')
library(PerformanceAnalytics)
library(magrittr)
etf4_returns_xts <-Return.calculate(etf4_monthly, method = "log") %>%
  na.omit()
head(etf4_returns_xts)
dim(etf4_returns_xts)
# you can also use coredata() to compute returns directly
etf4_ret<-coredata(etf4_monthly[-1,])/coredata(etf4_monthly[-dim(etf4_monthly)[1],])-1
head(etf4_ret)
class(etf4_ret)
#============================================================
# Plot in R
#-------------------------------------------------------------
plot(etf4_returns_xts, xaxt='n')
axis(1, index(etf4_returns_xts), format(index(etf4_returns_xts), "%Y/%m"))
# plot the scatterplot of 0050 and 00646
# convert xts into df using fortify()
library(ggplot2)
#
etf4_ret.df1<-fortify(etf4_returns_xts)
head(etf4_ret.df1)
plot(etf4_ret.df1$`0050`, etf4_ret.df1$`00646`, pch=20,
     col = 'darkred', main = '0050 vs. 00646 monthly returns',
     xlab = '0050', ylab = '00646 S&P500')
#-----------------------------------------------------------
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
# convert xts into data frame which can be used by ggplot
# split date index in xts into year, month and day columns 
# using lubridate package
library(lubridate)
etf4_ret.df2 <- cbind(etf4_ret.df1, month=month(index(etf4_returns_xts)), 
                      year=year(index(etf4_returns_xts)))
#
ggplot(data = etf4_ret.df2) +
  geom_point(mapping = aes(x = etf4_ret.df2$`0050`, y = etf4_ret.df2$`0056`, color = month))
#
ggplot(data = etf4_ret.df2) +
  geom_point(mapping = aes(x = etf4_ret.df2$`0050`, y = etf4_ret.df2$`0056`, size = month))
#
ggplot(data = etf4_ret.df2) +
  geom_point(mapping = aes(x = etf4_ret.df2$`0050`, y = etf4_ret.df2$`0056`, alpha = month))
#
etf4_ret
etf4_ret.tmp<-data.frame(date = index(etf4_returns_xts), etf4_ret)
head(etf4_ret.tmp)
# or you can use the following code
# %>% pipe operator
#etf4_ret.tmp<-data.frame(etf4_returns_xts, date=index(etf4_returns_xts)) 

etf4_ret.tmp02<-etf4_returns_xts %>% 
  data.frame(date=index(.)) %>% 
  remove_rownames() %>% 
  gather(asset, return, -date) # turn data into long format

head(etf4_ret.tmp02)
#
#plot(etf4_ret.tmp02$X0050, etf4_ret.tmp02$X0056)
#
#ggplot(etf4_ret.tmp02) +
#  geom_point(mapping = aes(x = X0050, y = X0056))
#
#---------------------------------------------------
etf4_ret.df<-fortify(etf4_returns_xts, melt=TRUE)
head(etf4_ret.df)
#
p<-ggplot(etf4_ret.df, aes(x = Index, y = Value))+
  geom_line(aes(color = Series), size = 1)
p

p + scale_x_date(date_labels = "%Y/%m")

# histogram distribution
q<-etf4_ret.df %>%
  ggplot(aes(x =Value, fill = Series)) +
  geom_histogram(alpha = 0.45, binwidth = .005) +
  ggtitle("Monthly Returns")
q + facet_wrap(~Series)+ theme_update(plot.title = element_text(hjust = 0.5))


# line distribution
etf4_ret.df %>%
  ggplot(aes(x = Value, colour = Series)) +
  geom_density(alpha = 1) +
  ggtitle("Monthly Returns Density Since 2016") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

# Combine line and histogram together
etf4_ret.df %>%
  ggplot(aes(x = Value)) +
  geom_density(aes(color = Series), alpha = 1) +
  geom_histogram(aes(fill = Series), alpha = 0.45, binwidth = .01) +
  guides(fill = FALSE) +
  facet_wrap(~Series) +
  ggtitle("Monthly Returns Since 2016") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

#---------------------------------------------------------------
library(plotly)
p1<-plot_ly(etf4_ret.tmp, x = ~date, y=~X0050, name = "0050", type = 'scatter', mode = 'lines') %>% 
    add_trace(y=~X0056, name = '0056', mode = 'lines+markers') %>% 
    layout(xaxis = list(title ='year'), yaxis = list(title='monthly returns'))
p1



