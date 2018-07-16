#####################
### Tickers ### 
#S&P 500 = SPX
#Russell 2000 = RTY
#Barclays Agg = LBUSTRUU
#Levered Loan = SPBDAL
#r2K Financial Service = RGUSFS  
#r2k Health Care = RGUSHS
#r2k Technology = RGUSTS
#R2K Producer Durables = RGUSPS
#R2K Consumer Discretionary = RGUSDS
#R2K Materials & Proce
#r2k Utilities = RGUSUS
#R2K Energy = RGUSES
#R2K Consumer Staples = RGUSSS
#CPI = CPUPAXFE (must use PX_LAST)
#####################

library(Rblpapi)
library(xts)
library(lubridate)
start <- as.Date("2004-01-01")
end <- today()
tickers <- paste(c("SPX","RTY","LBUSTRUU","SPBDAL","RGUSFS","RGUSHS", "RGUSTS", "RGUSPS", "RGUSDS","RGUSMS", "RGUSUS",
                  "RGUSES", "RGUSSS")," INDEX")
fields <- c("TOT_RETURN_INDEX_GROSS_DVDS","NAME", "PX_LAST")
conn <- blpConnect()
bbgdat.daily <- bdh(tickers,fields[1],start.date = start, end.date = end, options = 
                     c("periodicitySelection" = "DAILY"))
bbg.name = bdp(tickers, fields[2])
data.list <- list()
for(t in tickers) {
  dat <- bbgdat.daily[[t]]
  colnames(dat) = c("Date", "Price")
  px.xts <- xts(dat$Price, dat$Date)
  n <- bbg.name[t, fields[2]]
  colnames(px.xts) <- n
  data.list[[n]] <- px.xts
}  

#get cpi data and add to list above
bbgdat.daily <- bdh("CPUPAXFE Index", fields[3], start.date = start, end.date = end, options = 
                      c("periodicitySelection" = "DAILY"))
bbg.name = bdp("CPUPAXFE Index", "NAME")
x=blpDisconnect(conn)
colnames(bbgdat.daily) <- c("Date", "Price")
data.list[[bbg.name[1,1]]] <- xts(bbgdat.daily$Price, bbgdat.daily$Date)


# Calculate ODCE daily index & add to list above
odceq = read.csv('data/ODCE.csv')
odceq.mat = as.matrix(odceq)
odceq.vec = as.vector(t(odceq.mat[,2:5]))
dates1 = paste0(odceq$Year,"-3-31")
dates2 = paste0(odceq$Year,"-6-30")
dates3 = paste0(odceq$Year,"-9-30")
dates4 = paste0(odceq$Year,"-12-31")
odceq.dates = as.Date(as.vector(rbind(dates1,dates2,dates3,dates4)))
nas = which(is.na(odceq.vec))
if(length(nas)>0) {odceq.dates=odceq.dates[-nas];odceq.vec=odceq.vec[-nas]}
odceq.dates = c(odceq.dates[1]-90,odceq.dates)
mgmt=0
odceq.index = xts(cumprod(c(1,(1+odceq.vec-mgmt))),odceq.dates)
first = odceq.dates[1]
last = odceq.dates[length(odceq.dates)]
days365 = zooreg(rep(0,1+last-first),start=first,end=last)
odce.daily = merge(days365,odceq.index)
odce.daily = na.approx(odce.daily[,2])
data.list[["ODCE"]] <- xts(coredata(odce.daily), index(odce.daily))

