# Reference: http://www.systematicportfolio.com
# Evaluate and analyze Trading Strategies
rm(list=ls())
#
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

load.packages('quantmod')
# data is a time series of price
# signal is a indicator vector for buy and sell
bt.simple <- function(data, signal)
{
  # lag serial
  signal <- lag(signal,1)
  # back fill
  signal <- na.locf(signal, na.rm = FALSE)
  signal[is.na(signal)] = 0
  # calculate close-to-close returns
  # ROC() : Calculate the (rate of) change of a series over n periods.
  ret <- ROC(Cl(data), type="discrete")
  ret[1] = 0
  # compute stats
  bt <- list()
  bt$ret <- ret * signal 
  bt$equity <- cumprod(1 + bt$ret)
  return(bt)
}

# Test for bt.simple functions
# load historical prices from Yahoo Finance
data <- getSymbols('SPY', src = 'yahoo', from = '2000-01-01', to = '2018-12-31', auto.assign = F)
# buy and hold
signal <- rep(1, nrow(data))
buy.hold <- bt.simple(data, signal)
buy.hold$equity<-as.xts(buy.hold$equity)
head(buy.hold$equity)
tail(buy.hold$equity)
buy.hold$ret<-as.xts(buy.hold$ret)
head(buy.hold$ret)
# MA cross (moving average)
# Cl: get closing price
sma <- SMA(Cl(data), 200)
head(sma, 201)
#
signal <- ifelse(Cl(data) > sma, 1, 0) # if price large than moving mean, buy
head(signal, 201)
sma.cross <- bt.simple(data, signal)
names(sma.cross)
tail(sma.cross$equity)

#---------------------------------------------------------------------
# Create a chart showing the strategies perfromance in 2000:2009
dates <- '2000::2018'
buy.hold.equity <- buy.hold$equity[dates] / as.double(buy.hold$equity[dates][1])
sma.cross.equity <- sma.cross$equity[dates] / as.double(sma.cross$equity[dates][1])

# chartSeries() : Charting tool to create standard financial charts given a time series like object
chartSeries(buy.hold.equity, TA = c(addTA(sma.cross.equity, on=1, col='red')), 
            theme ='white', yrange = range(buy.hold.equity, sma.cross.equity) )
#------------------------------------------------------------------------
library(magrittr)
strategy.sma<-merge(buy.hold.equity, sma.cross.equity) %>% 
  set_colnames(c("BH", "SMA"))
head(strategy.sma,30)
tail(strategy.sma)
# plot using ggplot2 
library(ggplot2)
strategy.sma.df<-fortify(strategy.sma, melt=TRUE)
head(strategy.sma.df)
#
p<-ggplot(strategy.sma.df, aes(x = Index, y = Value))+
  geom_line(aes(color = Series), size = 0.5) +
  scale_x_date(date_labels = "%Y/%m")  # +
  #geom_hline(yintercept = c(1.0, 0.6)) # +  
  #geom_text(aes( 0, 0.6, label = 0.6, vjust = -1))

p
#===================================================================
# sample code to implement the above strategies using the backtesting 
# library in the Systematic Investor Toolbox:
#*****************************************************************
# Load historical data
#******************************************************************    
load.packages('quantmod')
tickers <- spl('SPY')

data1 <- new.env() # data is a environment

# bt.prep function merges and aligns all symbols in the data environment
getSymbols(tickers, src = 'yahoo', from = '2000-01-01', to = '2018-12-31', env = data1, auto.assign = T)
bt.prep(data1, align='keep.all')
names(data1)
#prices<-Ad(data$SPY)
#data$prices<-prices
#data$weight<-prices * NA
#data$execution.price <- prices * NA
head(data1$prices)
tail(data1$prices)
#*****************************************************************
# Code Strategies
#*****************************************************************
# bt.run computes the equity curve of strategy specified by data$weight matrix. 
# The data$weight matrix holds weights (signals) to open/close positions
# Buy & Hold 
# Reference: https://github.com/systematicinvestor/SIT/blob/master/R/bt.summary.r
data1$weight[] <- 1
buy.hold <- bt.run.share(data1, clean.signal=F, trade.summary = TRUE)
buy.hold <- bt.run(data1)
# MA Cross
# bt.apply function applies user given function to each symbol in the data environment
prices<-data1$prices
sma.1 <- bt.apply(data1, function(x) { SMA(Cl(x), 200) } ) 
head(sma.1, 201)
data1$weight[] <- NA # update weights matirx
data1$weight[] <- iif(prices >= sma.1, 1, 0)
sma.1.cross <- bt.run(data1, trade.summary=T)   
# 
plotbt.custom.report(sma.1.cross, buy.hold)
#
models<-list("SMA"= sma.1.cross, "BH" = buy.hold)
strategy.performance.snapshoot(sma.1.cross, T) 
strategy.performance.snapshoot(buy.hold, T) 
strategy.performance.snapshoot(models, T) 
#
#etf4.all<-readRDS("etf4_xts_all")
#

#----------------------------------------------------------
# Introduction to plota.layout function
# https://www.r-bloggers.com/introduction-to-plota-library-in-the-systematic-investor-toolbox/
#---------------------------------------------------------
load.packages('quantmod')

# download sample data from Yahoo
data.spy = getSymbols('SPY', from = '2000-01-01', auto.assign = FALSE)
data.ibm = getSymbols('IBM', from = '2000-01-01', auto.assign = FALSE)

y = data.spy['2011:01:01::2011:02:01']
highlight = which(Cl(y) < 127)

png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')

layout(c(1,1,2))
plota(y, type = 'candle', main = 'SPY', plotX = F, x.highlight = highlight)
y = plota.scale.volume(y)
plota(y, type = 'volume', x.highlight = highlight)

dev.off()
# ---------------------------------------------------------
# To create a simple chart of SPY with RSI and Legend:
y = data.spy['2010:01:01::2011:02:01']

layout(c(1,1,2,3))
plota(y, type = 'candle', plotX = F)
plota.legend('SPY', 'blue', y)

y = plota.scale.volume(y)
plota(y, type = 'volume', plotX = F)
plota.legend('Volume', 'blue', Vo(y))

rsi = RSI(Cl(y),2)
plota(rsi, type = 'l', y.highlight = c(c(Inf,80),c(20,-Inf)))
abline(h = 20, col = 'red')
abline(h = 80, col = 'red')
plota.legend('RSI(2)', 'black', rsi)
#--------------------------------------------------------------
# To create a chart with second Y axis:
#
y = data.spy['2010:01:01::2011:02:01']

# to plot second Y axis, free some space on left side
# e.g. set LeftMargin=3
plota(y, type = 'ohlc', LeftMargin=3)

y0 = y;
y = data.ibm['2010:10:15::2011:02:01']
plota2Y(y, ylim = range(OHLC(y)),las=1, col='red', col.axis = 'red')
plota.ohlc(y, col = 'red')
plota.legend('SPY(rhs),IBM(lhs)', 'blue,red', list(y0,y))
#
# To plot daily and monthly on the same plot: 
y = data.spy['2010:01:01::2011:02:01']

plota(y, type = 'candle')
y1 = to.monthly(y)
index(y1) = as.Date(index(y1))
plota.ohlc(y1, col = 'pink')
plota.candle(y)
plota.legend('Daily,Monthly', 'red,pink')

# To plot daily, weekly and monthly:
y = data.spy['2010:01:01::2011']

layout(c(1,2,3))
plota(y, type = 'candle', plotX = F)
plota.legend('Daily', 'blue', y)

plota(y, ylim = range(OHLC(y)), plotX = F)
y1 = to.weekly(y)
index(y1) = as.Date(index(y1))
plota.candle(y1)
plota.legend('Weekly', 'blue', y1)

plota(y, ylim = range(OHLC(y)))
y1 = to.monthly(y)
index(y1) = as.Date(index(y1))
plota.candle(y1)
plota.legend('Monthly', 'blue', y1)

#



