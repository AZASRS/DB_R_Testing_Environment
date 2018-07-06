library('tidyverse')
source(file='scripts/basic financial.r')

# This file executes things originally produced in privmcalcs-Feb2018.R

# Functions beginning with "get_" grab and manipulate data
# Functions beginning with "mix_" combine data sources
# Functions beginning with "populate_" finalize data prep for table insertion
# Functions beginning with "utility_" are used for DRY purposes in other functions


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
  dat = read_csv('data/Category.csv')
  df = dat
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
  
  ###TODO: check required for NA values
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


############################################
### Clean up so that data does not need to be looped through

## Equivalents ##
valdate = get_valdate()
cats = get_category_data()
x.all = as.data.frame(mix_cash_flow_irr_to_fund_info())
fundinfo = as.data.frame(get_fundinfo_data()) 
x = as.data.frame(populate_cash_flows_data()) 
x.v = as.data.frame(populate_values_data()) 
pre_hv = as.data.frame(get_all_historical_mv())
hv = as.data.frame(mix_all_historical_mv_to_fund_info()) ### hv was recreated in NEW-privmcalcs as a combo of pre_hv and fundinfo to avoid duplicating data in db
## Now you can start at line 90
## This works up until the end!!

###############################################

#### This section maybe shows that x, x.v, hv = get_all_historical_mv(), fundinfo, cats could be all we need in database
### TODO: should I group by fundinfo$Short and sum() ?? need all grouping variables and values to sum
a = cats %>%
  left_join(fundinfo, by = 'catshort')

b = a %>%
  left_join(x, by = c("Short" = "ShortName"))
b %>% group_by(Short) %>% summarize(n = n()) %>% arrange(desc(n)) ### shows duplication problem

c = a %>%
  left_join(x.v, by = c("Short" = "ShortName"))
c %>% group_by(Short) %>% summarize(n = n()) %>% arrange(desc(n)) ### shows duplication problem

d = bind_rows(b,c) ### may not be necessary because tables will be different

e = d %>%
  left_join(hv, by = c("Short" = "ShortName"))
##############################

utility_cf_and_value_data_function = function(y){
  df = tibble(Date=character(),Amount=integer(),ShortName=character())
  for(i in names(y)){
    a = y[i][[1]]
    if(length(a > 0)){
      b = fortify.zoo(a)
      b$Date = as.character(b$Index)
      b$Amount = b$a
      b$ShortName = i
      b$Index = NULL
      b$a = NULL
      df = bind_rows(df,b)
    }
  }
  return(df)
}


populate_fund_value_data = function(){
  ## This covers through line, searhc for names(y.hv)=namevec
  y_hv = utility_cf_and_value_data_function(y.hv)
  y_v = utility_cf_and_value_data_function(y.v)
  df = bind_rows(y_hv,y_v)
  df = df %>%
    group_by(Date, ShortName) %>%
    summarize(Amount = sum(Amount)) %>%
    tidyr::drop_na()
  return(df)
}


populate_fund_cash_flows_data = function(){
  ## This covers through line, searhc for names(y.hv)=namevec
  df = utility_cf_and_value_data_function(y.cf)
  df = df %>%
    group_by(Date, ShortName) %>%
    summarize(Amount = sum(Amount)) %>%
    tidyr::drop_na()
  return(df)
}


y.v.table = populate_fund_value_data()
y.cf.table = populate_fund_cash_flows_data()











##################################
## cleaning for insert to database
##################################

df_fundinfo = fundinfo
names(df_fundinfo) = tolower(names(df_fundinfo))
df_fundinfo = df_fundinfo %>%
  rename(unfunded_date = unfunded.date,
         yield_amt = yield,
         class_type = class)

df_x.v = x.v
names(df_x.v) = tolower(names(df_x.v))
df_x.v = df_x.v %>%
  select(date, shortname, amount) %>%
  mutate(date = as.character(date)) %>%
  group_by(date,shortname) %>%
  summarize(amount = max(amount))

df_x = x
names(df_x) = tolower(names(df_x))
df_x = df_x %>%
  select(date, shortname, amount) %>%
  mutate(date = as.character(date)) %>%
  group_by(date,shortname) %>%
  summarize(amount = max(amount))

df_cats = cats
names(df_cats) = tolower(names(df_cats))
df_cats = df_cats %>% 
  rename(growth_d = growth.d,
         yield_d = yield.d,
         method_d = method.d)

df_pre_hv = pre_hv
names(df_pre_hv) = tolower(names(df_pre_hv))
df_pre_hv = df_pre_hv %>% 
  select(-type) %>%
  filter(is.na(shortname) == FALSE)


library('DBI')
library('RSQLite')
con <- dbConnect(RSQLite::SQLite(), "../DB_Application/asrs_temporary.db")
dbWriteTable(con, name = 'fundinfo', value = df_fundinfo, row.names = FALSE, append = TRUE)
dbWriteTable(con, name = 'cashflow', value = df_x.v, row.names = FALSE, append = TRUE)
dbWriteTable(con, name = 'nav', value = df_x, row.names = FALSE, append = TRUE)
dbWriteTable(con, name = 'category', value = df_cats, row.names = FALSE, append = TRUE)
dbWriteTable(con, name = 'historical_mv', value = df_pre_hv, row.names = FALSE, append = TRUE)
