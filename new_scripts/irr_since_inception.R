library(tidyverse)
library(zoo)
library(lubridate)

#### Example: ####################

# irr_df = irr_since_inception(shortname = 'Total PE', benchname = '^RUT', portname = 'PE')
# p = ggplot(irr_df, aes(x = date, y = irr * 100, col = name))
# p + geom_line()

##################################


irr_since_inception = function(shortname, benchname, portname){
  #### portfolionav.csv only adds in a few extra historical values... 
  #### TODO: ok, can we get these into the database somehow??
  more.values=read.csv('data/portfolionav.csv')
  more.values$Date=as.Date(more.values$Date,format="%m/%d/%Y")
  
  fundinfo = AZASRS::pull_fundinfo()
  valdate = AZASRS::get_valdate()
  pev = AZASRS::get_filtered_nav(shortname, convert_365 = FALSE, return_zoo = TRUE)
  indx = AZASRS::get_filtered_benchmark(benchname, convert_365 = TRUE)
  pecf = AZASRS::get_filtered_cashflow(shortname, convert_365 = TRUE) %>% na.trim()
  
  pev.more=more.values[which(more.values$Portfolio==portname),]
  pev.more=pev.more[which(!(pev.more$Date%in%time(pev))),]
  pev.more=zoo(pev.more$Amount,pev.more$Date)
  pev=c(pev.more,pev)
  #extract the dates for which we have values 
  dates=time(pev) 
  # 
  #now calculate the IRR for each end date 
  #initiate empty to hold the results 
  peirr=vector() 
  r2kirr=vector() 
  r2kwealth=vector()
  # 
  ### equivalents exist already
  for (i in 1:length(dates)) {   
    #extract the cash flow through the end date   
    cfi = pecf[time(pecf)<=dates[i]]   
    #add the value to the cash flow   
    cfi = asrsMethods::mergesum.z(cfi,pev[i])   
    #extract the index values for the same dates   
    r2ki = indx[time(cfi)]   
    #call pestats to calculate performance   
    #arguments are the cash flow as a zoo object and the index values for the same dates   
    ansi = asrsMethods::pestats(cfi,r2ki)   
    peirr[i] = ansi$irr   
    r2kirr[i] = ansi$ind.irr
    r2kwealth[i] = pev[i] - ansi$pme.wealthdiff
  } 
  #convert the irrs to zoo objects with the same dates as the values 
  peirr = zoo(peirr,dates) 
  r2kirr = zoo(r2kirr,dates) 
  r2kwealth = zoo(r2kwealth,dates)
  
  irr_df = tibble(date = index(peirr), peirr) %>% 
    rename(irr = peirr) %>% 
    mutate(name = shortname) %>%
    bind_rows(
      tibble(date = index(r2kirr), r2kirr) %>% 
        rename(irr = r2kirr) %>% 
        mutate(name = benchname)
    )
  
  ## TODO: figure out how to return both irr & wealth data to avoid redundant calculations
  #  r2k_wealth = tibble(date = index(r2kwealth), r2kwealth)
  
  return(irr_df)
}


