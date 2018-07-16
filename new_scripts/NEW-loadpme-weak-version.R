library('tidyverse')
load('data/pmedata.rdata')


pme.df_fund = pme.df %>% 
  mutate(fund_name = row.names(pme.df)) %>%
   as_tibble()

data_select = function(pme_df, column_name_ends_with){
  df = pme_df %>% 
    select(fund_name, ends_with(column_name_ends_with)) %>%
    drop_na()
  if(column_name_ends_with == ' PME' | column_name_ends_with == ' Dollar Matched IRR'){
    colnames(df) = gsub(column_name_ends_with, '', colnames(df))  
  }
  return(df)
}

# Fact / Metrics tables
pme = data_select(pme.df_fund, ' PME') %>% gather(comp_name, pme, -fund_name)
d_m_irr = data_select(pme.df_fund, ' Dollar Matched IRR') %>% gather(comp_name, dollar_matched_irr, -fund_name)
fund_irr = data_select(pme.df_fund, 'Fund IRR') %>% rename(fund_irr = `Fund IRR`)
fund_irr_2 = data_select(pme.df_fund, 'Fund IRR 2') %>% rename(`fund_irr_2` = `Fund IRR 2`)
fund_tvpi = data_select(pme.df_fund, 'Fund TVPI') %>% rename(`fund_tvpi` = `Fund TVPI`)
fund_tvpi_2 = data_select(pme.df_fund, 'Fund TVPI 2') %>% rename(`fund_tvpi_2` = `Fund TVPI 2`)
fund_unrealized_pct = data_select(pme.df_fund, 'Unrealized Percent') %>% rename(`unrealized_percent` = `Unrealized Percent`)
fund_unrealized_pct_2 = data_select(pme.df_fund, 'Unrealized  Percent 2') %>% rename(`unrealized_percent_2` = `Unrealized  Percent 2`)
cash_adj_nav = data_select(pme.df_fund, 'Cash Adj NAV') %>% rename(`cash_adjusted_nav` = `Cash Adj NAV`)
val = data_select(pme.df_fund, 'val') %>% rename(`value` = `val`)

# Dimension table
pme_dims = pme.df_fund  %>% 
  select(fund_name, cat, vint, unf, commit, name, Date.2, Legacy, Portfolio,
         isvint, istotal, vintsum, iscat, catsum, isfund, drawn, distributed,
         dpi, appr, consultant, sponsorsum) %>%
  rename(date_2 = Date.2)



