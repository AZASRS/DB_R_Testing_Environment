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
    filter(Date >= DO_NOT_INCLUDE_BEFORE_DATE) %>%
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
