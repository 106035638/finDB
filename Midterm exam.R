rm(list=ls())

ifelse(!require(quantmod), install.packages('quantmod'), library(quantmod))
tw20_adj_close<-read.csv("2018Q4_20.csv")


ifelse(!require(readr), install.packages('readr'), library(readr))
tw20.txt<-read.table("tw20.txt")


ifelse(!require(reshape2), install.packages('reshape2'), library(reshape2))
colnames(tw20.txt)<-c("id","","date", "price")
tw20.xts = dcast(tw20.txt, date~id)


tw20.xts$date<-as.Date(as.character(tw20.xts$date), "%Y%m%d") 
library(xts)
tw20.xts.1<-xts(tw20.xts, order.by = tw20.xts$date)

library(quantmod)
tw20.mon.ret <- to.monthly(tw20.xts.1, indexAt = "lastof", OHLC=FALSE)
head(tw20.mon.ret)

library(PerformanceAnalytics)
library(magrittr)
tw20.day.ret <-Return.calculate(tw20.xts.1, method = "log") %>%
  na.omit()
head(tw20.day.ret)
dim(tw20.day.ret)

ifelse(!require(PerformanceAnalytics), install.packages('PerformanceAnalytics'), library(PerformanceAnalytics))
ifelse(!require(magrittr), install.packages('magrittr'), library(magrittr))
