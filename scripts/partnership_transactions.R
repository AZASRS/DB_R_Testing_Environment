library('tidyverse')


partnership_transactions = function(DO_NOT_INCLUDE_BEFORE_DATE = "2018-01-01"){
  
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
    transmute(`Asset.Class` = gsub("ASRS - ", "", `Asset Class`),
              `Total.Mgmt Fees` = `Mgmt Fees in Commit` + `Mgmt Fees outside Commit`,
              `Total.Carry` = `Carry.in.Commit` + `Carry outside Commit`)
  return(df_group_fees)
}


