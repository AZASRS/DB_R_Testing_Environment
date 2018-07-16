library('AZASRS')
library('tidyverse')

fundinfo = pull_fundinfo()
x.v = pull_cashflow()
x = pull_nav()
cats = pull_categories()
mv = pull_historicalmv()

hv = mv %>%
  left_join(fundinfo, by = c('shortname' = 'short')) %>%
  select(shortname, date, amount,
         catshort, vintage, portfolio, legacy, stype, sector)










####
a = cats %>%
  left_join(fundinfo, by = 'catshort')

b = a %>%
  left_join(x, by = c("short" = "shortname"))
b %>% group_by(short) %>% summarize(n = n()) %>% arrange(desc(n)) ### shows duplication problem

d = a %>%
  left_join(x.v, by = c("short" = "shortname"))
d %>% group_by(short) %>% summarize(n = n()) %>% arrange(desc(n)) ### shows duplication problem

e = bind_rows(b %>% mutate(type="C"),d %>% mutate(type="V")) ### may not be necessary because tables will be different

f = e %>%
  left_join(hv, by = c("short" = "shortname"))

f %>%
  group_by(portfolio, vintage.x, date.x, type) %>%
  summarize(amount.x = max(amount.x))

