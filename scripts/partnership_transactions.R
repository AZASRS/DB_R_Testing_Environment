library('tidyverse')


partnership_transactions = function(){
  
  Fund.raw = read_delim("data/201805 - Partnership_Investment_Transaction_Download.csv", delim = "|")
  
  df_group_fees = Fund.raw %>% 
    transmute(`Asset Class` = `FUND_SHORT_NAME`,
              `Fund Name` = `INVESTMENT_NAME`,
              Date = `CONTRIB_DISTRIB_DATE`,
              `Carry in Commit` = `BASE_FEES_IN_COMMIT`,
              `Mgmt Fees in Commit` = `BASE_MF_IN_COMMIT`,
              `Carry outside Commit` = `BASE_FEES_OUT_COMMIT`,
              `Mgmt Fees outside Commit` = `BASE_MF_OUT_COMMIT`) %>%
    mutate(Date = as.Date(Date, format='%d %b %Y')) %>%
    transmute(Date = Date,
              `Asset Class` = gsub("ASRS - ", "", `Asset Class`),
              `Total Mgmt Fees` = `Mgmt Fees in Commit` + `Mgmt Fees outside Commit`,
              `Total Carry` = `Carry in Commit` + `Carry outside Commit`)
  return(df_group_fees)
}


partnership_transactions_report = function(df, DO_NOT_INCLUDE_BEFORE_DATE = "2018-01-01"){
  DO_NOT_INCLUDE_BEFORE_DATE = as.Date(DO_NOT_INCLUDE_BEFORE_DATE)
  df = dat %>%
    filter(Date >= DO_NOT_INCLUDE_BEFORE_DATE) %>%
    select(-Date) %>%
    group_by(`Asset Class`) %>%
    summarize_all(sum) %>%
    replace_na(list(`Total Mgmt Fees` = 0, 
               `Total Carry` = 0)) %>%
    bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total All")))
}

dat = partnership_transactions()
report = partnership_transactions_report(df = dat)

pt = dat %>% 
  rename(date = Date,
         asset_class = `Asset Class`,
         total_mgmt_fees = `Total Mgmt Fees`,
         total_carry = `Total Carry`) %>%
  mutate(date = as.character(date)) %>%
  group_by(date, asset_class) %>%
  summarise(total_mgmt_fees = max(total_mgmt_fees),
            total_carry = max(total_carry)) %>%
  unique() %>%
  na.omit() 

fi = fundinfo
colnames(fi) = tolower(colnames(fi))
fi = fi %>%
  rename(unfunded_date = unfunded.date,
         yield_amt = yield,
         class_type = class)
  

library('DBI')
library('RSQLite')
con <- dbConnect(RSQLite::SQLite(), "../DB_Application/temporary4.db")
dbWriteTable(con, name='partnership_transactions', value = pt %>% unique(), row.names=FALSE, append=TRUE)
dbWriteTable(con, name='fundinfo', value = fi %>% unique(), row.names=FALSE, append=TRUE)
