library('tidyverse')
source(file='scripts/basic financial.r')


# This file executes things originally produced in privmcalcs-Feb2018.R

# Functions beginning with "get_" grab and manipulate data
# Functions beginning with "mix_" combine data sources
# Functions beginning with "populate_" finalize data prep for table insertion

get_fundinfo_data = function(){
  df = read_csv('data/fundinfo.csv')
  return(df)
}


get_valdate = function(){
  #This is a dangerous way to get date
  fundinfo = get_fundinfo_data()
  valdate = as.Date(fundinfo$CSDate, format='%m/%d/%Y')[1] 
  return(valdate)
}


get_unfunded_date = function(){
  #This is a dangerous way to get date
  fundinfo = get_fundinfo_data()
  unfunded.date = as.Date(fundinfo$Unfunded.Date, format='%m/%d/%Y')[1] 
  return(unfunded.date)
}


get_category_data = function(){
  df = read_csv('data/Category.csv')
  return(df)
}


get_all_cash_flow_irr = function(){
  
  ## Replaces x.all up to line 58
  
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
    mutate(Date = gsub("-","/",as.character(Date))) %>%
    mutate(Date = as.Date(Date, format='%m/%d/%Y'))
  
  return(df)
}


get_all_historical_mv = function(){
  # Creates hv from line to line 60
  perf = read_csv(file = 'data/Valuation Performance.csv')
  hv = read_csv(file='data/AllHistoricalMV.csv')
  valdate = get_valdate()
  
  # Wrangle Data - convert to proper form
  df_perf = perf %>%
    select(-`Investment Name`) %>%
    rename(ShortName = `Client Grouping 1`,
           Amount = `Market Value`) %>%
    mutate(Type = "V",
           Date = valdate)
  
  df_hv = hv %>%
    mutate(Date = as.Date(Date, format = '%m/%d/%Y'))
  
  ### check required for NA values
  df = bind_rows(df_hv, df_perf)
  return(df)
}


mix_cash_flow_irr_to_fund_info = function(){
  # Continues x.all up to line 86
  cf_irr = get_all_cash_flow_irr()
  fundinfo = get_fundinfo_data()
  
  df = cf_irr %>%
    left_join(fundinfo, by = c("ShortName" = "Short")) %>%
    select(Date, ShortName, Type, Amount,
           catshort, Vintage, Portfolio, Legacy, Stype, Sector) %>%
    rename(cats = catshort, vints = Vintage)
  return(df)
}


populate_cash_flows_data = function(){
  df = mix_cash_flow_irr_to_fund_info()
  df = df %>%
    filter(Type == 'C') %>%
    select(-Type)
  return(df)
}


populate_values_data = function(){
  df = mix_cash_flow_irr_to_fund_info()
  df = df %>%
    filter(Type == 'V') %>%
    select(-Type)
  return(df)
}


mix_all_historical_mv_to_fund_info = function(){
  # Creates hv to line 82
  hv = get_all_historical_mv()
  fundinfo = get_fundinfo_data()
  
  df = hv %>%
    left_join(fundinfo, by = c("ShortName" = "Short")) %>%
    select(ShortName, Date, Amount, Type,
           catshort, Vintage, Portfolio, Legacy, Stype, Sector) %>%
    rename(cats = catshort, vints = Vintage)
  return(df)
}




## Equivalents ##
valdate = get_valdate()
cats = as.data.frame(get_category_data())
x.all = as.data.frame(mix_cash_flow_irr_to_fund_info())
fundinfo = as.data.frame(get_fundinfo_data())
x = as.data.frame(populate_cash_flows_data())
x.v = as.data.frame(populate_values_data())
hv = as.data.frame(mix_all_historical_mv_to_fund_info())
## Now you can start at line 90
## This works up until the end!!






combine_navs_irr = function(){
  x.all = all_cashflow_irr()
  hv = all_navs()
  df = bind_rows(x.all,hv)
  return(df)
}











#
















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



#library('DBI')
#library('RSQLite')
#con <- dbConnect(RSQLite::SQLite(), "../DB_Application/temporary4.db")
#dbWriteTable(con, name='irr', value = irr_navs %>% unique(), row.names=FALSE, append=TRUE)
#dbWriteTable(con, name='benchmarks', value = benchmarks %>% unique(), row.names=FALSE, append=TRUE)

