library('tidyverse')

#TODO: all_navs function - no great way to capture date for 'perf' (fundinfo.csv workaround needs fixing)


all_cashflow_irr = function(){
  x.all=read_csv(file='data/AllCashFlowIRR.csv')
  Bolt = read_csv(file='data/BoltOnCFStockSales.csv')
  CCash = read_csv(file='data/PI Transactions.csv')
  
  # Wrangle Data - convert to proper form
  ##### Must be cleaned to enter database
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
  perf = read_csv(file = 'data/Valuation Performance.csv')
  hv = read_csv(file='data/AllHistoricalMV.csv')
  ## fundinfo.csv only being brought in for a date to be pulled (which is updated manually each qtr??)
  fundinfo = read_csv(file='data/fundinfo.csv')
  
  df_perf = perf %>%
    select(-`Investment Name`) %>%
    rename(ShortName = `Client Grouping 1`,
           Amount = `Market Value`) %>%
    mutate(Type = "V",
           Date = as.Date(zoo::as.yearqtr(Sys.Date()) - 1/4, frac = 1))
  ### this is where the "quarter" date is made up from manual data update
  ## Could we use??
  # as.Date(zoo::as.yearqtr(Sys.Date()) + 1/4) = beginning of quarter
  # as.Date(zoo::as.yearqtr(Sys.Date()) + 1/4, frac = 1) = end of quarter
  # as.Date(zoo::as.yearqtr(Sys.Date()) - 1/4, frac = 1) = end of quarter ??
  
  df_hv = hv %>%
    mutate(Date = as.Date(Date, format = '%m/%d/%Y'))
  
  ### check required for NA values
  df = bind_rows(df_hv, df_perf)
  return(df)
}



x.all = all_cashflow_irr()
hv = all_navs()
a = x.all %>% left_join(fundinfo, by = c('ShortName'='Short'))
b = hv %>% left_join(fundinfo, by = c('ShortName' = 'Short'))

#106 - 
#join fundinfo and cats, some random fiddling done to make it fit

#124 - 154
#join x and f.c.name, join x.v, join hv (where x.v is subset of x.all)

# 156 - 
# join x and cats


tfundvals = read_csv('data/tfundvals.csv')
p2pirrs = read_csv('data/p2pirrs.csv') #seems created not raw data
portmat = read_csv('data/portmat.csv') #seems created not raw data



