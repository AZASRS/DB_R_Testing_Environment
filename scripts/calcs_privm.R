library('tidyverse')


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

