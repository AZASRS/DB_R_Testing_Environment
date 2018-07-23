library('tidyverse')
load('data/pmedata.rdata')

library('DBI')
library('RSQLite')

#con <- dbConnect(RSQLite::SQLite(), "P:\\IMD\\2018 Database Project\\asrs_temporary_db.db")
con = dbConnect(RSQLite::SQLite(), "C:\\Users\\scotts\\Documents\\GitHub\\DB_Application\\asrs_temporary.db")


pme.df_fund = pme.df %>% 
  mutate(fund_name = row.names(pme.df)) %>%
  as_tibble()

data_select = function(pme_df, column_name_ends_with){
  df = pme_df %>% 
    select(fund_name, ends_with(column_name_ends_with)) %>%
    drop_na()
  if(column_name_ends_with == ' PME' | column_name_ends_with == ' PME 2' | column_name_ends_with == ' Dollar Matched IRR' | column_name_ends_with == ' Dollar Matched IRR 2'){
    colnames(df) = gsub(column_name_ends_with, '', colnames(df))  
  }
  return(df)
}

# Fact / Metrics tables
pme_1 = data_select(pme.df_fund, ' PME') %>% gather(comp_name, pme, -fund_name) %>% mutate(period = 'Historical')
pme_2 = data_select(pme.df_fund, ' PME 2') %>% gather(comp_name, pme, -fund_name) %>% mutate(period = 'Most Recent')
pme = bind_rows(pme_1, pme_2)

d_m_irr_1 = data_select(pme.df_fund, ' Dollar Matched IRR') %>% gather(comp_name, dollar_matched_irr, -fund_name) %>% mutate(period = 'Historical')
d_m_irr_2 = data_select(pme.df_fund, ' Dollar Matched IRR 2') %>% gather(comp_name, dollar_matched_irr, -fund_name) %>% mutate(period = 'Most Recent')
d_m_irr = bind_rows(d_m_irr_1, d_m_irr_2)

fund_irr_1 = data_select(pme.df_fund, 'Fund IRR') %>% rename(fund_irr = `Fund IRR`) %>% mutate(period = 'Historical')
fund_irr_2 = data_select(pme.df_fund, 'Fund IRR 2') %>% rename(`fund_irr` = `Fund IRR 2`) %>% mutate(period = 'Most Recent')
fund_irr = bind_rows(fund_irr_1, fund_irr_2)

fund_tvpi_1 = data_select(pme.df_fund, 'Fund TVPI') %>% rename(`fund_tvpi` = `Fund TVPI`) %>% mutate(period = 'Historical')
fund_tvpi_2 = data_select(pme.df_fund, 'Fund TVPI 2') %>% rename(`fund_tvpi` = `Fund TVPI 2`) %>% mutate(period = 'Most Recent')
fund_tvpi = bind_rows(fund_tvpi_1, fund_tvpi_2)

unrealized_pct_1 = data_select(pme.df_fund, 'Unrealized Percent') %>% rename(`unrealized_percent` = `Unrealized Percent`) %>% mutate(period = 'Historical')
unrealized_pct_2 = data_select(pme.df_fund, 'Unrealized  Percent 2') %>% rename(`unrealized_percent` = `Unrealized  Percent 2`) %>% mutate(period = 'Most Recent')
unrealized_pct = bind_rows(unrealized_pct_1, unrealized_pct_2)

cash_adj_nav = data_select(pme.df_fund, 'Cash Adj NAV') %>% rename(`cash_adjusted_nav` = `Cash Adj NAV`)
val = data_select(pme.df_fund, 'val') %>% rename(`value` = `val`)
drawn = data_select(pme.df_fund, 'drawn')
distributed = data_select(pme.df_fund, 'distributed')
appr = data_select(pme.df_fund, 'appr')
dpi = data_select(pme.df_fund, 'dpi')

# Dimension table
pme_lookup = pme.df_fund  %>% 
  select(fund_name, cat, vint, unf, commit, name, Date.2, Legacy, Portfolio,
         isvint, istotal, vintsum, iscat, catsum, isfund,
         consultant, sponsorsum) %>%
  rename(date_2 = Date.2,
         legacy = Legacy,
         portfolio = Portfolio,
         is_vint = isvint,
         is_total = istotal,
         vint_sum = vintsum,
         is_cat = iscat,
         cat_sum = catsum,
         is_fund = isfund,
         sponsor_sum = sponsorsum)



# Truncate tables before inserting
dbSendQuery(con, "DELETE FROM pme;")
dbWriteTable(con, name = 'pme', value = pme, row.names = FALSE, append = TRUE)

dbSendQuery(con, "DELETE FROM dollar_matched_irr;")
dbWriteTable(con, name = 'dollar_matched_irr', value = d_m_irr, row.names = FALSE, append = TRUE)

dbSendQuery(con, "DELETE FROM fund_irr;")
dbWriteTable(con, name = 'fund_irr', value = fund_irr, row.names = FALSE, append = TRUE)

dbSendQuery(con, "DELETE FROM fund_tvpi;")
dbWriteTable(con, name = 'fund_tvpi', value = fund_tvpi, row.names = FALSE, append = TRUE)

dbSendQuery(con, "DELETE FROM unrealized_percent;")
dbWriteTable(con, name = 'unrealized_percent', value = unrealized_pct, row.names = FALSE, append = TRUE)

dbSendQuery(con, "DELETE FROM cash_adjusted_nav;")
dbWriteTable(con, name = 'cash_adjusted_nav', value = cash_adj_nav, row.names = FALSE, append = TRUE)

dbSendQuery(con, "DELETE FROM value;")
dbWriteTable(con, name = 'value', value = val, row.names = FALSE, append = TRUE)

dbSendQuery(con, "DELETE FROM drawn;")
dbWriteTable(con, name = 'drawn', value = drawn, row.names = FALSE, append = TRUE)

dbSendQuery(con, "DELETE FROM distributed;")
dbWriteTable(con, name = 'distributed', value = distributed, row.names = FALSE, append = TRUE)

dbSendQuery(con, "DELETE FROM appr;")
dbWriteTable(con, name = 'appr', value = appr, row.names = FALSE, append = TRUE)

dbSendQuery(con, "DELETE FROM dpi;")
dbWriteTable(con, name = 'dpi', value = dpi, row.names = FALSE, append = TRUE)

dbSendQuery(con, "DELETE FROM pme_lookup;")
dbWriteTable(con, name = 'pme_lookup', value = pme_lookup, row.names = FALSE, append = TRUE)

dbDisconnect(con)
