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
head(buy.hold$equity)
tail(buy.hold$equity)
head(buy.hold$ret)
# MA cross (moving average)
# Cl: get closing price
sma <- SMA(Cl(data), 200)
head(sma, 200)
#
signal <- ifelse(Cl(data) > sma, 1, 0) # if price large than moving mean, buy
head(signal, 201)
sma.cross <- bt.simple(data, signal)
# Create a chart showing the strategies perfromance in 2000:2009
dates <- '2000::2018'
buy.hold.equity <- buy.hold$equity[dates] / as.double(buy.hold$equity[dates][1])
sma.cross.equity <- sma.cross$equity[dates] / as.double(sma.cross$equity[dates][1])

# chartSeries() : Charting tool to create standard financial charts given a time series like object
chartSeries(buy.hold.equity, TA = c(addTA(sma.cross.equity, on=1, col='red')), 
            theme ='white', yrange = range(buy.hold.equity, sma.cross.equity) )
#
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
  scale_x_date(date_labels = "%Y/%m") +
  geom_hline(yintercept = c(1.0, 0.6))

p
#===================================================================
# sample code to implement the above strategies using the backtesting 
# library in the Systematic Investor Toolbox:
#*****************************************************************
# Load historical data
#******************************************************************    
load.packages('quantmod')
tickers <- spl('SPY')

data <- new.env() # data is a environment

# bt.prep function merges and aligns all symbols in the data environment
getSymbols(tickers, src = 'yahoo', from = '2000-01-01', to = '2018-12-31', env = data, auto.assign = T)
# bt.prep(data, align='keep.all')
names(data)
prices<-Ad(data$SPY)
data$prices<-prices
data$weight<-prices * NA
data$execution.price <- prices * NA
head(data$prices)
tail(data$prices)
#*****************************************************************
# Code Strategies
#*****************************************************************
# bt.run computes the equity curve of strategy specified by data$weight matrix. 
# The data$weight matrix holds weights (signals) to open/close positions
# Buy & Hold 
data$weight[] <- 1
buy.hold <- bt.run.share(data, clean.signal=F, trade.summary = TRUE)
buy.hold <- bt.run(data)
# MA Cross
# bt.apply function applies user given function to each symbol in the data environment
sma <- bt.apply(data, function(x) { SMA(Cl(x), 200) } ) 
data$weight[] <- NA # update weights matirx
data$weight[] <- iif(prices >= sma, 1, 0)
sma.cross <- bt.run(data, trade.summary=T)   

plotbt.custom.report(sma.cross, buy.hold)



#

etf4.all<-readRDS("etf4_xts_all")



#
tickers = spl('^GSPC')

data1 <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1896-01-01', env = data1, auto.assign = T)
bt.prep(data1, align='keep.all', dates='1896::2011')

#*****************************************************************
# Code Strategies
#****************************************************************** 
prices = data1$prices    

# Buy & Hold	
data1$weight[] = 1
buy.hold = bt.run(data1)
#
b<- data1
bt.run <- function
(
  b,					# environment with symbols time series
  trade.summary = F, 	# flag to create trade summary
  do.lag = 1, 		# lag signal
  do.CarryLastObservationForwardIfNA = TRUE, 
  type = c('weight', 'share'),
  silent = F,
  capital = 100000,
  commission = 0,
  weight = b$weight,
  dates = 1:nrow(b$prices)	
) 
{
  # convert dates to dates.index
  dates.index = dates2index(b$prices, dates) 
  
  # setup
  type = type[1]
  
  # create signal
  weight[] = ifna(weight, NA)
  
  # lag
  if(do.lag > 0)
    weight = mlag(weight, do.lag) # Note k=1 implies a move *forward*  
  
  # backfill
  if(do.CarryLastObservationForwardIfNA)
    weight[] = apply(coredata(weight), 2, ifna.prev)
  
  weight[is.na(weight)] = 0
  
  # find trades
  weight1 = mlag(weight, -1)
  tstart = weight != weight1 & weight1 != 0
  tend = weight != 0 & weight != weight1
  trade = ifna(tstart | tend, FALSE)
  
  # prices
  prices = b$prices
  
  # execution.price logic
  if( sum(trade) > 0 ) {
    execution.price = coredata(b$execution.price)
    prices1 = coredata(b$prices)
    
    prices1[trade] = iif( is.na(execution.price[trade]), prices1[trade], execution.price[trade] )
    prices[] = prices1
  }
  
  # type of backtest
  if( type == 'weight') {
    ret = prices / mlag(prices) - 1
    ret[] = ifna(ret, NA)
    ret[is.na(ret)] = 0			
  } else { # shares, hence provide prices
    ret = prices
  }
  
  #weight = make.xts(weight, b$dates)
  temp = b$weight
  temp[] = weight
  weight = temp
  
  
  # prepare output
  bt = bt.summary(weight, ret, type, b$prices, capital, commission)
  bt$dates.index = dates.index
  bt = bt.run.trim.helper(bt, dates.index)
  
  if( trade.summary ) bt$trade.summary = bt.trade.summary(b, bt)
  
  if( !silent ) {
    # print last signal / weight observation
    cat('Latest weights :\n')
    print(round(100*last(bt$weight),2))
    cat('\n')
    
    cat('Performance summary :\n')
    cat('', spl('CAGR,Best,Worst'), '\n', sep = '\t')  
    cat('', sapply(cbind(bt$cagr, bt$best, bt$worst), function(x) round(100*x,1)), '\n', sep = '\t')  
    cat('\n')    
  }
  
  return(bt)
}

