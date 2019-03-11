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
#install.packages("readr")
library(readr)
etf4_csv<-read_csv("ETF4_2000_2018_d.csv")
# you will get garbled text!
etf4_csv<-read_csv("ETF4_2000_2018_d.csv", locale = locale(encoding='big5'))
head(etf4_csv)
str(etf4_csv)
# read xls file
#install.packages("readxl")
library(readxl)
etf4_xls<-read_excel("ETF4_2000_2018_d.xls", 
                     col_types =c("text", "text","text", "numeric","numeric"))
head(etf4_xls)
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
write.csv(etf4.xts, file = "myetf4.csv")
# date index disappears!!!
# you have to use write.zoo to save .xts file
write.zoo(etf4.xts, sep = ',', file = "myetf4.csv.1")
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
#
lastweek <- last(etf4_2016, "1 week")
# Print the last 2 observations in lastweek
last(lastweek, 2)
# Extract all but the first two days of lastweek
first(lastweek, "-2 days")




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
#============================================================
# Plot in R
#-------------------------------------------------------------
plot(etf4_returns_xts, xaxt='n')
axis(1, index(etf4_returns_xts), format(index(etf4_returns_xts), "%Y/%m"))
#axis(side=1, at=yahoo2$date[ at ], labels=format(yahoo2$date[at], '%b-%y'))
#plot.xts(etf4_mon_ret, auto.legend = TRUE)
#-----------------------------------------------------------
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
# convert xts into data frame which can be used by ggplot
#etf4_returns.df<-fortify(etf4_returns_xts)
etf4_ret.df<-fortify(etf4_returns_xts, melt=TRUE)
head(etf4_ret.df)
#
p<-ggplot(etf4_ret.df, aes(x = Index, y = Value))+
  geom_line(aes(color = Series), size = 1)

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

# plot the scatterplot of 0050 and 00646
# convert xts into df 
etf4_ret.df1<-fortify(etf4_returns_xts)
plot(etf4_returns.df1$`0050`, etf4_returns.df1$`00646`, pch=20,
     col = 'darkred', main = '0050 vs. 00646 monthly returns',
     xlab = '0050', ylab = '00646 S&P500')


#================================================
# some manipulations on xts 
#================================================
dates <- as.Date("2016-01-01") + 0:40
# Create ts_a
ts_a <- xts(x = 1:41, order.by = dates)
ts_a
# Create ts_b
ts_b <- xts(x = 1:41, order.by = as.POSIXct(dates))
ts_b
ts_a[index(ts_a)]
ts_a[index(ts_b)]
#
tmp_file <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1127/datasets/tmp_file.csv"
# Create dat by reading tmp_file
dat <- read.csv(tmp_file)
head(dat)
str(dat)
# Convert dat into xts
xts(dat, order.by = as.Date(rownames(dat), "%m/%d/%Y"))

# Read tmp_file using read.zoo
dat_zoo <- read.zoo(tmp_file, index.column = 0, sep = ",", format = "%m/%d/%Y")
dat_zoo
str(dat_zoo)
# Convert dat_zoo to xts
dat_xts <- as.xts(dat_zoo)
#----------------------------------------------