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

shortnames = c("SPX","RTY","LBUSTRUU",
               "RGUSFS","RGUSHS", "RGUSTS", 
               "RGUSPS", "RGUSDS","RGUSMS", 
               "RGUSUS","RGUSES", "RGUSSS",
               "CPUPAXFE", "ODCE")

longnames = c("S&P 500 Index", "Russell 2000 Index", "Bloomberg Barclays US Agg Tota", 
              "Russell 2000 Financial Service", "Russell 2000 Health Care Index", "Russell 2000 Technology Index",
              "Russell 2000 Producer Durables", "Russell 2000 Consumer Discreti", "Russell 2000 Materials & Proce",
              "Russell 2000 Utilities Index", "Russell 2000 Energy Index", "Russell 2000 Consumer Staples",
              "US CPI Urban Consumers Less Fo", "ODCE")

lookup_table = tibble(shortname = shortnames, 
                      longname = longnames)


start <- as.Date("2004-01-01")
end <- today()
tickers <- paste(c("SPX","RTY","LBUSTRUU","RGUSFS","RGUSHS", "RGUSTS", "RGUSPS", "RGUSDS","RGUSMS", "RGUSUS",
                   "RGUSES", "RGUSSS")," INDEX")
fields <- c("TOT_RETURN_INDEX_GROSS_DVDS","SECURITY_NAME", "PX_LAST")
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

df_z = do.call('merge', data.list) # merge to wide format
names(df_z) = names(data.list)
df = tk_tbl(df_z) %>% rename(date = index)
df = df[, colSums(is.na(df)) != nrow(df)] # remove rows containing ONLY NA
df = df %>% 
  as_tibble() %>%
  mutate(date = as.character(date)) %>%
  gather(longname, price, -date) %>%
  arrange(longname,date) %>%
  drop_na()

df_final = df %>%
  left_join(lookup_table, by = 'longname')

dbSendQuery(con, "DELETE FROM benchmark;")
dbWriteTable(con, name='benchmark', value = df_final, row.names=FALSE, append=TRUE)
dbDisconnect(con)
#











