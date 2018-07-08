library('tidyverse')
library('DBI')
library('RSQLite')
con <- dbConnect(RSQLite::SQLite(), "P:\\IMD\\2018 Database Project\\asrs_temporary_db.db")
#con = dbConnect(RSQLite::SQLite(), "C:\\Users\\scotts\\Documents\\GitHub\\DB_Application\\asrs_temporary.db")
### TODO: Address adding 250 to the LLI benchmark (and log), fixed8 is modified ODCE
### TODO: address the days365 and basis point additions

spy = read_csv('data/spx index.csv')
rty = read_csv('data/rty index.csv')
vmfx = read_csv('data/vbmfx equity.csv')
odce = read_csv('data/odce daily index.csv')
lli = read_csv('data/spbdal index.csv')
cpi = read_csv('data/CPI X Food Energy.csv')
r2ksec = read_csv('data/r2k sector indices.csv')

long_names = c("S&P 500",
               "Russell 2K",
               "Bonds",
               'Lev Loan+250',
               'CPIxFE+350', 
               'ODCE',
               'R2K Fin Svc', 	
               'R2K Health Care', 
               'R2K Tech', 
               'R2K Durables',
               'R2K Consumer Disc',	
               'R2K Materials',	
               'R2K Utilities',	
               'R2K Energy', 
               'R2K Staples')

short_names = c('SPY',
                'RTY',
                'VMFX',
                'LLI',
                'CPI',
                'ODCE',
                "Russ2000Financial Svc",
                "Russ2000Health Care",
                "Russ2000Technology",
                "Russ2000Producer Dur",
                "Russ2000Consumer Disc",
                "Russ2000Matl & Proc",
                "Russ2000Utilities",
                "Russ2000Energy",
                "Russ2000Consumer Stap")

lookup_table = tibble(short_name = short_names, 
                      long_name = long_names)

benchmarks = bind_rows(spy %>% mutate(symbol = 'SPY'),
                       rty %>% mutate(symbol = 'RTY'),
                       vmfx %>% mutate(symbol = 'VMFX'),
                       lli %>% mutate(symbol = 'LLI'),
                       cpi %>% mutate(symbol = 'CPI') %>% rename(PX_LAST = CPIxFE, date = Date),
                       odce %>% mutate(symbol = 'ODCE') %>% rename(date = X1, PX_LAST = x),
                       r2ksec %>% select(-X1) %>% rename(date = Date) %>% gather(symbol, PX_LAST, -date)) %>% 
  select(date, PX_LAST, symbol) %>%
  rename(price = PX_LAST, shortname = symbol)

benchmarks_df = benchmarks %>%
  left_join(lookup_table, by = c('shortname' = 'short_name')) %>%
  mutate(date = as.character(date)) %>%
  rename(longname = long_name) %>%
  unique()


dbSendQuery(con, "DELETE FROM benchmark;")
dbWriteTable(con, name='benchmark', value = benchmarks_df, row.names=FALSE, append=TRUE)
dbDisconnect(con)
