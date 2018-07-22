library('tidyverse')
library('timetk')
library('DBI')
library('RSQLite')
library('Rblpapi')
library('xts')
library('lubridate')

#con <- dbConnect(RSQLite::SQLite(), "P:\\IMD\\2018 Database Project\\asrs_temporary_db.db")
con = dbConnect(RSQLite::SQLite(), "C:\\Users\\scotts\\Documents\\GitHub\\DB_Application\\asrs_temporary.db")

#####################
### Tickers ### 
#Barclays Agg = LBUSTRUU
#CPI = CPUPAXFE (must use PX_LAST)
#Russell 2000 = RTY
#S&P 500 = SPX
#r2K Financial Service = RGUSFS  
#r2k Health Care = RGUSHS
#r2k Technology = RGUSTS
#R2K Producer Durables = RGUSPS
#R2K Consumer Discretionary = RGUSDS
#R2K Materials & Proce
#r2k Utilities = RGUSUS
#R2K Energy = RGUSES
#R2K Consumer Staples = RGUSSS
#####################

lookup_table = read_csv('ticker_symbols.csv')

start <- as.Date("2004-01-01")
end <- today()
tickers <- paste(c("SPX","RTY","LBUSTRUU","SPBDAL","RGUSFS","RGUSHS", "RGUSTS", "RGUSPS", "RGUSDS","RGUSMS", "RGUSUS",
                   "RGUSES", "RGUSSS")," INDEX")
fields <- c("TOT_RETURN_INDEX_GROSS_DVDS","NAME", "PX_LAST")
conn <- blpConnect()


bbgdat.daily <- bdh(tickers,fields[1],start.date = start, end.date = end, options = 
                      c("periodicitySelection" = "DAILY"))
tickers_df = bbgdat.daily %>% 
  map(bind_rows) %>%
  bind_rows(.id = 'symbol') %>%
  rename(price = TOT_RETURN_INDEX_GROSS_DVDS)

bbgdat.daily <- bdh("CPUPAXFE Index", fields[3], start.date = start, end.date = end, options = 
                      c("periodicitySelection" = "DAILY"))
tickers_df = tickers_df %>%
  bind_rows(data.frame(bbgdat.daily) %>% 
              rename(price = PX_LAST) %>%
              mutate(symbol = "CPUPAXFE Index"))


# Calculate ODCE daily index 
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

odce_df = tibble(date = index(odce.daily), odce.daily) %>%
  rename(price = odce.daily) %>%
  mutate(symbol = 'ODCE',
         price = as.numeric(price))


df = tickers_df %>% as_tibble() %>%
  bind_rows(odce_df) %>%
  left_join(lookup_table, by = c('symbol' = 'reference_name')) %>%
  select(date, price, shortname, longname, symbol) %>%
  mutate(date = as.character(date))

dbSendQuery(con, "DELETE FROM benchmark;")
dbWriteTable(con, name='benchmark', value = df, row.names=FALSE, append=TRUE)
dbDisconnect(con)

