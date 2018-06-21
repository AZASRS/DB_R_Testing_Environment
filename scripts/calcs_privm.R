library('tidyverse')


# This file executes things originally produced in privmcalcs-Feb2018.R


all_cashflow_irr = function(){
  x.all=read_csv(file='data/AllCashFlowIRR.csv')
  Bolt = read_csv(file='data/BoltOnCFStockSales.csv')
  CCash = read_csv(file='data/PI Transactions.csv')
  
  # Wrangle Data - convert to proper form
  df_x.all = x.all %>%
    rename(Date = `Transaction Date`,
           ShortName = `Client Grouping 1`,
           Type = `Cash Flow Type`,
           Amount = `Best Available Cash Flow Amt`) %>%
    mutate(Type = gsub("EV", "V", Type))
  
  df_CCash = CCash %>%
    rename(ShortName = `Client Grouping 1`,
           Date = `Transaction Date`) %>%
    mutate(Amount = `Total Distribution Amt` - `Total Contributions`,
           Type = "C") %>%
    select(ShortName, Date, Type, Amount)
  
  df = df_x.all %>%
    bind_rows(Bolt, df_CCash) %>%
    mutate(Date = as.Date(Date, format='%m/%d/%Y'))
  return(df)
}


all_navs = function(){
  # Function replaces: reviewhv.csv
  perf = read_csv(file = 'data/Valuation Performance.csv')
  hv = read_csv(file='data/AllHistoricalMV.csv')
  
  # Wrangle Data - convert to proper form
  df_perf = perf %>%
    select(-`Investment Name`) %>%
    rename(ShortName = `Client Grouping 1`,
           Amount = `Market Value`) %>%
    mutate(Type = "V",
           Date = as.Date(cut(Sys.Date(), "quarter")) - 1)
  ### This is where the "quarter" date is made up from manual data update
  ### This replaces bringing in fundinfo.csv and retrieving a manually updated number
  # as.Date(zoo::as.yearqtr(Sys.Date()) + 1/4) = beginning of quarter
  # as.Date(zoo::as.yearqtr(Sys.Date()) + 1/4, frac = 1) = end of quarter
  # as.Date(as.character(zoo::as.yearqtr(Sys.Date()) - 1/4, frac = 1), format = "%Y Q") = end of quarter ??
  
  df_hv = hv %>%
    mutate(Date = as.Date(Date, format = '%m/%d/%Y'))
  
  ### check required for NA values
  df = bind_rows(df_hv, df_perf)
  return(df)
}


combine_navs_irr = function(){
  x.all = all_cashflow_irr()
  hv = all_navs()
  df = bind_rows(x.all,hv)
  return(df)
}

#a = x.all %>% left_join(fundinfo, by = c('ShortName'='Short'))
#b = hv %>% left_join(fundinfo, by = c('ShortName' = 'Short'))

#106 - 
#join fundinfo and cats, some random fiddling done to make it fit

#124 - 154
#join x and f.c.name, join x.v, join hv (where x.v is subset of x.all)

# 156 - 
# join x and cats

fundinfo = read_csv('data/fundinfo.csv')
cats = read_csv('data/Category.csv')

irr_navs = combine_navs_irr()
a = irr_navs %>%
  filter(Type == 'C')
b = fundinfo %>%
  left_join(cats, by='catshort')
c = a %>%
  left_join(b, by=c('ShortName' = 'Short'))
#



tfundvals = read_csv('data/tfundvals.csv')
p2pirrs = read_csv('data/p2pirrs.csv') #seems created not raw data
portmat = read_csv('data/portmat.csv') #seems created not raw data

all_benchmarks = function(){
  spy = read_csv('data/spx index.csv')
  rty = read_csv('data/rty index.csv')
  vmfx = read_csv('data/vbmfx equity.csv')
  odce = read_csv('data/odce daily index.csv')
  lli = read_csv('data/spbdal index.csv')
  cpi = read_csv('data/CPI X Food Energy.csv')
  r2ksec = read_csv('data/r2k sector indices.csv')
  
  df_benchmarks = bind_rows(spy %>% mutate(symbol = 'SPY'),
                       rty %>% mutate(symbol = 'RTY'),
                       vmfx %>% mutate(symbol = 'VMFX'),
                       lli %>% mutate(symbol = 'LLI'),
                       cpi %>% mutate(symbol = 'CPI') %>% rename(PX_LAST = CPIxFE, date = Date),
                       odce %>% mutate(symbol = 'ODCE') %>% rename(date = X1, PX_LAST = x),
                       r2ksec %>% select(-X1) %>% rename(date = Date) %>% gather(symbol, PX_LAST, -date)) %>%
    select(date, PX_LAST, symbol)
  return(df_benchmarks)
}

irr_navs = combine_navs_irr()
irr_navs = irr_navs %>%
  rename(shortname = ShortName,
         date = Date,
         transaction_type = Type,
         amount = Amount) %>%
  mutate(date = as.character(date)) %>%
  unique() %>%
  na.omit()
benchmarks = all_benchmarks()
benchmarks = benchmarks %>% rename(px_last = PX_LAST) %>%
  mutate(date = as.character(date)) %>%
  unique() %>%
  na.omit()


irr_navs = combine_navs_irr() %>% 
  mutate(Date = as.character(Date)) %>%
  rename(date = Date, shortname = ShortName, transaction_type = Type, amount = Amount) %>%
  group_by(date,shortname,transaction_type) %>%
  summarise(amount = max(amount)) %>%
  unique() %>%
  na.omit()




library('DBI')
library('RSQLite')
con <- dbConnect(RSQLite::SQLite(), "../DB_Application/temporary4.db")
dbWriteTable(con, name='irr', value = irr_navs %>% unique(), row.names=FALSE, append=TRUE)
dbWriteTable(con, name='benchmarks', value = benchmarks %>% unique(), row.names=FALSE, append=TRUE)
