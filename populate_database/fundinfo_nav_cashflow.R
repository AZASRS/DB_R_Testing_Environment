library('timetk')
library('zoo')
library('tseries')
library('tidyverse')
source(file='scripts/basic financial.r')

library('DBI')
library('RSQLite')

#con <- dbConnect(RSQLite::SQLite(), "P:\\IMD\\2018 Database Project\\asrs_temporary_db.db")
con = dbConnect(RSQLite::SQLite(), "C:\\Users\\scotts\\Documents\\GitHub\\DB_Application\\asrs_temporary.db")

## PURPOSE ######################################################################
# This file loads the database with data initially sourced in privmcalcs-Feb2018.R
# Changes from file: cashflows and values are separated into two different tables
# File truncates table before inserting data - this is to remove duplication until
# there is a strong schema and production database
# File currently populates tables:
# - fundinfo
# - nav
# - cashflow
## END PURPOSE ###################################################################

## TODO: remove "Total" calculations from y.v/hv/cf ... they will be calculated by portfolio, not stored in database


# Functions beginning with "get_" grab and manipulate data
# Functions beginning with "mix_" combine data sources
# Functions beginning with "populate_" finalize data prep for table insertion

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
  df = read_csv('data/Category.csv')
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
  
  ### check required for NA values
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




## Equivalents ##
valdate = get_valdate()
cats = as.data.frame(get_category_data())
x.all = as.data.frame(mix_cash_flow_irr_to_fund_info())
fundinfo = as.data.frame(get_fundinfo_data())
x = as.data.frame(populate_cash_flows_data())
x.v = as.data.frame(populate_values_data())
hv = as.data.frame(mix_all_historical_mv_to_fund_info())


#### Taken directly from privmcalcs
#initialize empty lists
y.cf=list()
y.v=list()
y.hv=list()

# inititalize empty vectors
unf=vector()
commit=vector()
lastcfdate=max(x$Date)
namevec=vector()
longname=vector()
catvec=vector()
vintvec=vector()
portvec=vector()
legvec=vector()
specialist=vector()
sector=vector()
sponsorsum=vector()

# loop through categories
for (i in 1:length(cats$catshort)) {
  # which funds are in the current category
  f.c=which((as.character(cats$catshort[i]))==fundinfo$catshort)
  # if none go to the next category
  if (0==length(f.c)) next
  # extract some data from fundinfo for the funds in the current category
  f.c.name=as.character(fundinfo$Short[f.c])
  f.c.legacy=as.character(fundinfo$Legacy[f.c])
  f.c.longname=as.character(fundinfo$Csname[f.c])
  f.c.closed=as.logical(fundinfo$Closed[f.c])
  # add to the extracted data subtotal information for the category
  unf=c(unf,fundinfo$Unfunded[f.c],sum(fundinfo$Unfunded[f.c]))
  commit=c(commit,fundinfo$Commit[f.c],sum(fundinfo$Commit[f.c]))
  namevec=c(namevec,f.c.name,as.character(cats$catshort[i]))
  legvec=c(legvec,f.c.legacy,NA)
  longname=c(longname,f.c.longname,as.character(cats$catlong[i]))
  catvec=c(catvec,rep(as.character(cats$catshort[i]),1+length(f.c.name)))
  portvec=c(portvec,rep(as.character(cats$Portfolio[i]),1+length(f.c.name)))
  # flag as not part of a sponsor summary
  sponsorsum=c(sponsorsum,rep(FALSE,1+length(f.c.name)))
  # initiate valsub as zero
  valsub=zoo(0,valdate)
  # now we are looping for each fund in the category
  for (j in 1:length(f.c.name)) {
    
    # an index in to x for the jth fund
    ind.j=which(x$ShortName==f.c.name[j])
    #extract cash flows and a 0 to the end
    cf.j=c(x$Amount[ind.j],0)
    #extract the dates and add valdate to the end
    date.j=c(x$Date[ind.j],valdate)
    # aggregate the cash flows by date (sometimes there is more than one cash flow on a given date)
    cf.j=aggregate(cf.j,list(date.j),sum)
    # convert to a zoo object
    cf.z=zoo(cf.j[,2],cf.j[,1])
    # add to the list
    y.cf=c(y.cf,list(cf.z))
    
    #get value and add to vector
    # index to value
    ind.j=lastinvec(which(x.v$ShortName==f.c.name[j]))
    # grab the value
    val.j=x.v$Amount[ind.j]
    # if missing, use NA unless it is a closed investment in which case it is zero
    if(length(val.j)==0) {
      val.j=NA
      if(f.c.closed[j]) val.j=0
    }
    # if not NA, add the value to the category subtotal
    if(!is.na(val.j)) valsub=valsub+val.j
    # convert to zoo object
    val.z=zoo(val.j,valdate)
    # find the historic values
    hvalind=which(hv$ShortName==f.c.name[j])
    hval.j=hv$Amount[hvalind]
    hvaldate=hv$Date[hvalind]
    if(length(hval.j)!=length(unique(hvaldate))) warning(paste(f.c.name[j]),' has duplicate hv entries on same day, line 103')
    # turn them in to a zoo object
    hval.z=zoo(hval.j,hvaldate)
    # add v and hv to the lists
    y.v=c(y.v,list(val.z))
    y.hv=c(y.hv,list(hval.z))
    
    #add vintage to vintvec
    vint.j=fundinfo$Vintage[fundinfo$Short==f.c.name[j]]
    p.j=lastinvec(portvec)
    vintvec=c(vintvec,paste(p.j," V",vint.j,sep=""))
    
  }
  
  # end the j loop through funds in a category
  
  #calculate the subtotal for the category and add it to the list
  # extract the cash flow items from x for this category
  ind.i=which(x$cats==as.character(cats$catshort[i]))
  cf.i=c(x$Amount[ind.i],0)
  # extract the dates for this category
  date.i=c(x$Date[ind.i],valdate)
  # aggregate by date
  cf.i=aggregate(cf.i,list(date.i),sum)
  # make a zoo object
  cf.z=zoo(cf.i[,2],cf.i[,1])
  # add to the list
  y.cf=c(y.cf,list(cf.z))
  
  #add subtotal value to the list
  y.v=c(y.v,list(valsub))
  # an index for historic values before valdate
  hvalind=which(hv$cats==as.character(cats$catshort[i])&hv$Date<=valdate)
  # an index for historic values after valdate
  hvalind.cur=which(hv$cats==as.character(cats$catshort[i])&hv$Date>valdate)
  # create a "current" value for the category only if every fund has reported 
  # for the quarter after valdate
  if(length(hvalind.cur)==length(f.c)&
     1==length(unique(hv$Date[hvalind.cur]))) hvalind=c(hvalind,hvalind.cur)
  if (length(hvalind)==0) {
    y.hv=c(y.hv,list(c(NA))) } else {
      hval.sub=hv$Amount[hvalind]
      # create a zoo object for the category historic values
      hvaldate.sub=hv$Date[hvalind]
      hval.sub=aggregate(hval.sub,list(hvaldate.sub),sum)
      hval.sub.z=zoo(hval.sub[,2],hval.sub[,1])
      # add the category historic values to the list
      y.hv=c(y.hv,list(hval.sub.z))
    }
  # the category vintage is NA
  vintvec=c(vintvec,NA)
  # If this is the last category in a portfolio, do a portfolio total
  dotot=FALSE
  if(i==length(cats$catshort)) {dotot=TRUE} else
    if (cats$Portfolio[i+1]!=cats$Portfolio[i]) {dotot=TRUE}
  if (dotot) {
    
    #produce needed subsets from the x data.frame
    portfolio=lastinvec(portvec)
    # get the stuff for the portfolio
    xsubset=subset(x,x$Portfolio==portfolio)
    # split the stuff between K and PreK
    xsubset.prek=subset(xsubset,xsubset$Legacy=='PreK')
    xsubset.k=subset(xsubset,xsubset$Legacy=='K')
    # split the stuff between G and S
    #adding Eric's slice and dice categories for PE Only
    xsubset.sty.g = subset(xsubset, xsubset$Stype=='G')
    xsubset.sty.s = subset(xsubset, xsubset$Stype=='S')
    
    # produce same subsets from the fundinfo data.frame
    fsubset=subset(fundinfo,fundinfo$Portfolio==portfolio)
    fsubset.prek=subset(fsubset,fsubset$Legacy=='PreK')
    fsubset.k=subset(fsubset,fsubset$Legacy=='K')
    #adding Eric's slice and dice categories for PE Only
    fsubset.sty.g = subset(fsubset, fsubset$Stype=='G')
    fsubset.sty.s = subset(fsubset, fsubset$Stype=='S')
    
    
    # produce same subsets from the x.v data.frame
    xvsubset=subset(x.v,x.v$Portfolio==portfolio)
    xvsubset.prek=subset(xvsubset,xvsubset$Legacy=='PreK')
    xvsubset.k=subset(xvsubset,xvsubset$Legacy=='K')
    #adding Eric's slice and dice categories for PE Only
    xvsubset.sty.g = subset(xvsubset, xvsubset$Stype=='G')
    xvsubset.sty.s = subset(xvsubset, xvsubset$Stype=='S')
    
    # produce same subset from the hv data.frame
    hvsubset=subset(hv,hv$Portfolio==portfolio&hv$Date<=valdate)
    hvsubset.prek=subset(hvsubset,hvsubset$Legacy=='PreK')
    hvsubset.k=subset(hvsubset,hvsubset$Legacy=='K')
    #adding Eric's slice and dice categories for PE Only
    hvsubset.sty.g = subset(hvsubset, hvsubset$Stype=='G')
    hvsubset.sty.s = subset(hvsubset, hvsubset$Stype=='S')
    
    
    # portfolio total
    # total cash flow for the portfolio
    x.t=aggregate(xsubset$Amount,by=list(xsubset$Date),sum)
    x.z=zoo(x.t[,2],x.t[,1])
    y.cf=c(y.cf,list(x.z))
    # total valdate value
    vtz=zoo(sum(xvsubset$Amount[xvsubset$Date==valdate]),valdate)
    y.v=c(y.v,list(vtz))
    # total value for dates other than valdate
    if(nrow(hvsubset)>0) {
      hval.sub=aggregate(hvsubset$Amount,list(hvsubset$Date),sum)
      hval.sub.z=zoo(hval.sub[,2],hval.sub[,1])
      y.hv=c(y.hv,list(hval.sub.z))} else {
        y.hv=c(y.hv,NA)
      }
    # add totals to the vectors
    unf=c(unf,sum(fsubset$Unfunded))
    commit=c(commit,sum(fsubset$Commit))
    namevec=c(namevec,paste("Total",portfolio))
    longname=c(longname,paste("Total",portfolio))
    catvec=c(catvec,NA)
    vintvec=c(vintvec,NA)
    portvec=c(portvec,lastinvec(portvec))
    sponsorsum=c(sponsorsum,FALSE)
    legvec=c(legvec,NA)
    
    
    # cash flows and values for the legacy portfolio
    if (length(xsubset.prek$Amount)>0) {
      x.t=aggregate(xsubset.prek$Amount,by=list(xsubset.prek$Date),sum)
      x.z=zoo(x.t[,2],x.t[,1])
      y.cf=c(y.cf,list(x.z))
      vtz=zoo(sum(xvsubset.prek$Amount[xvsubset.prek$Date==valdate]),valdate)
      y.v=c(y.v,list(vtz))
      hval.sub=aggregate(hvsubset.prek$Amount,list(hvsubset.prek$Date),sum)
      hval.sub.z=zoo(hval.sub[,2],hval.sub[,1])
      y.hv=c(y.hv,list(hval.sub.z))
      unf=c(unf,sum(fsubset.prek$Unfunded))
      commit=c(commit,sum(fsubset.prek$Commit))
      namevec=c(namevec,paste("Total",portfolio,"Legacy Portfolio"))
      longname=c(longname,paste("Total",portfolio,"Legacy Portfolio"))
      catvec=c(catvec,NA)
      vintvec=c(vintvec,NA)
      portvec=c(portvec,lastinvec(portvec))
      sponsorsum=c(sponsorsum,FALSE)
      legvec=c(legvec,"PreK")
    }
    
    #cash flows and values for the karl era portfolio
    if(length(xsubset.k$Amount)>0) {
      x.t=aggregate(xsubset.k$Amount,by=list(xsubset.k$Date),sum)
      x.z=zoo(x.t[,2],x.t[,1])
      y.cf=c(y.cf,list(x.z))
      vtz=zoo(sum(xvsubset.k$Amount[xvsubset.k$Date==valdate]),valdate)
      y.v=c(y.v,list(vtz))
      if (nrow(hvsubset.k)>0) {
        hval.sub=aggregate(hvsubset.k$Amount,list(hvsubset.k$Date),sum)
        hval.sub.z=zoo(hval.sub[,2],hval.sub[,1])
        y.hv=c(y.hv,list(hval.sub.z)) } else {
          y.hv=c(y.hv,NA)  
        }
      unf=c(unf,sum(fsubset.k$Unfunded))
      commit=c(commit,sum(fsubset.k$Commit))
      namevec=c(namevec,paste("Total",portfolio,"Current Portfolio"))
      longname=c(longname,paste("Total",portfolio,"Current Portfolio"))
      catvec=c(catvec,NA)
      vintvec=c(vintvec,NA)
      portvec=c(portvec,lastinvec(portvec))
      sponsorsum=c(sponsorsum,FALSE)
      legvec=c(legvec,"K")
    }
    
    #cash flows and values for the Eric Generalist portfolio
    if(length(xsubset.sty.g$Amount)>0) {
      x.t=aggregate(xsubset.sty.g$Amount,by=list(xsubset.sty.g$Date),sum)
      x.z=zoo(x.t[,2],x.t[,1])
      y.cf=c(y.cf,list(x.z))
      vtz=zoo(sum(xvsubset.sty.g$Amount[xvsubset.sty.g$Date==valdate]),valdate)
      y.v=c(y.v,list(vtz))
      if (nrow(hvsubset.sty.g)>0) {
        hval.sub=aggregate(hvsubset.sty.g$Amount,list(hvsubset.sty.g$Date),sum)
        hval.sub.z=zoo(hval.sub[,2],hval.sub[,1])
        y.hv=c(y.hv,list(hval.sub.z)) } else {
          y.hv=c(y.hv,NA)  
        }
      unf=c(unf,sum(fsubset.sty.g$Unfunded))
      commit=c(commit,sum(fsubset.sty.g$Commit))
      namevec=c(namevec,paste("Total",portfolio,"Generalist Portfolio"))
      longname=c(longname,paste("Total",portfolio,"Generalist Portfolio"))
      catvec=c(catvec,NA)
      vintvec=c(vintvec,NA)
      legvec = c(legvec, NA)
      portvec=c(portvec,lastinvec(portvec))
      sponsorsum=c(sponsorsum,FALSE)
      specialist=c(specialist,"G")
    }
    
    #cash flows and values for the Eric Specialist portfolio
    if(length(xsubset.sty.s$Amount)>0) {
      x.t=aggregate(xsubset.sty.s$Amount,by=list(xsubset.sty.s$Date),sum)
      x.z=zoo(x.t[,2],x.t[,1])
      y.cf=c(y.cf,list(x.z))
      vtz=zoo(sum(xvsubset.sty.s$Amount[xvsubset.sty.s$Date==valdate]),valdate)
      y.v=c(y.v,list(vtz))
      if (nrow(hvsubset.sty.s)>0) {
        hval.sub=aggregate(hvsubset.sty.s$Amount,list(hvsubset.sty.s$Date),sum)
        hval.sub.z=zoo(hval.sub[,2],hval.sub[,1])
        y.hv=c(y.hv,list(hval.sub.z)) } else {
          y.hv=c(y.hv,NA)  
        }
      unf=c(unf,sum(fsubset.sty.s$Unfunded))
      commit=c(commit,sum(fsubset.sty.s$Commit))
      namevec=c(namevec,paste("Total",portfolio,"Specialist Portfolio"))
      longname=c(longname,paste("Total",portfolio,"Specialist Portfolio"))
      catvec=c(catvec,NA)
      vintvec=c(vintvec,NA)
      legvec = c(legvec, NA)
      portvec=c(portvec,lastinvec(portvec))
      sponsorsum=c(sponsorsum,FALSE)
      specialist=c(specialist,"S")
    }
  }
}  

# end of loop through categories

#now we are going to add cash flows and values for vintage years 
portfolios=as.character(unique(fundinfo$Portfolio))

# loop through by portfolio
for(h in 1:length(portfolios)) {
  portfolio=portfolios[h]
  fsubset=subset(fundinfo,fundinfo$Portfolio==portfolio)
  xsubset=subset(x,x$Portfolio==portfolio)
  xvsubset=subset(x.v,x.v$Portfolio==portfolio)
  hvsubset=subset(hv,hv$Portfolio==portfolio)
  vintages=sort(unique(fsubset$Vintage))
  # now loop for each vintage in the portfolio
  for (i in 1:length(vintages)) {
    vintname=paste0(portfolio," V",vintages[i],sep="")
    vintlongname=paste(portfolio,"Vintage",vintages[i])
    
    # calculate vintage cash flow and add to list
    vint.ind=which(vintages[i]==xsubset$vints)
    vint.cf=c(xsubset$Amount[vint.ind],0)
    vint.date=c(xsubset$Date[vint.ind],valdate)
    vint.cf=aggregate(vint.cf,by=list(vint.date),sum)
    vint.z=zoo(vint.cf[,2],vint.cf[,1])
    y.cf=c(y.cf,list(vint.z))
    
    # add vintage name to name list  
    namevec=c(namevec,vintname)
    longname=c(longname,vintlongname)
    catvec=c(catvec,NA)
    vintvec=c(vintvec,vintname)
    legvec=(c(legvec,NA))
    portvec=c(portvec,portfolio)
    sponsorsum=c(sponsorsum,FALSE)
    
    # add vintage value to vector
    vint.ind=which((vintages[i]==xvsubset$vints)&(valdate==xvsubset$Date))
    vintval.z=zoo(sum(xvsubset$Amount[vint.ind]),valdate)
    y.v=c(y.v,list(vintval.z))
    hv.ind=which((vintages[i]==hvsubset$vints)&(valdate!=hvsubset$Date))
    if (length(hv.ind)==0) {
      y.hv=c(y.hv,list(c(NA)))
    } else {
      vint.hv=hvsubset$Amount[hv.ind]
      vint.hvdate=hvsubset$Date[hv.ind]
      vint.hv=aggregate(vint.hv,by=list(vint.hvdate),sum)
      vint.hv.z=zoo(vint.hv[,2],vint.hv[,1])
      y.hv=c(y.hv,list(vint.hv.z))
    }
    
    # add unfunded by vintage
    unf=c(unf,sum(fsubset$Unfunded[fsubset$Vintage==vintages[i]]))
    commit=c(commit,sum(fsubset$Commit[fsubset$Vintage==vintages[i]]))
  }
  #end loop for each vintage in a portfolio
}
#end loop for each portfolio


#now add cash flows and values RCLCO underwritten deals
namevec=c(namevec,"RCLCO")
longname=c(longname,"RCLCO Underwriting")
catvec=c(catvec,NA)
vintvec=c(vintvec,"RCLCO")
legvec=(c(legvec,NA))
portvec=c(portvec,"RE")
sponsorsum=c(sponsorsum,TRUE)
RCLCONames=fundinfo[(fundinfo$Consultant=="RCLCO"),"Short"]
RCLCOInd=match(RCLCONames[RCLCONames %in% namevec],namevec)
RCLCOList=y.cf[RCLCOInd]
RCLCOVals=y.v[RCLCOInd]
RCLCOTot=do.call(zoosum,RCLCOList)
y.cf=c(y.cf,list(RCLCOTot))
y.v=c(y.v,list(do.call(zoosum,RCLCOVals)))
y.hv=c(y.hv,list(c(NA)))
unf=c(unf,sum(unf[RCLCOInd]))
commit=c(commit,sum(commit[RCLCOInd]))
consultantvec=rep(NA,length(commit))
consultantvec[RCLCOInd]="RCLCO"
consultantvec[length(consultantvec)]="RCLCO"

#Add manager summaries for real estate
sponsors.re=fundinfo$Sponsor[fundinfo$Portfolio=="RE"]
sponsors.re=sponsors.re[!is.na(sponsors.re)]
sponsors.re=as.character(sort(unique(sponsors.re)))
sponsors.re=sponsors.re[!sponsors.re==""]
for (spons in sponsors.re) {
  namevec=c(namevec,paste0(spons,".sum"))
  longname=c(longname,paste(spons,"Summary"))
  catvec=c(catvec,"NA")
  vintvec=c(vintvec,"RE Mgr Summary")
  legvec=(c(legvec,NA))
  portvec=c(portvec,"RE")
  sponsorsum=c(sponsorsum,TRUE)
  RCLCONames=as.character(fundinfo[(fundinfo$Sponsor==spons &
                                      fundinfo$Portfolio=="RE"),"Short"])
  RCLCONames=c(RCLCONames, 
               as.character(fundinfo[(fundinfo$Sponsor==spons &
                                        fundinfo$Portfolio=="POPP" &
                                        fundinfo$ICPCat=="R"),"Short"]))
  RCLCOInd=match(RCLCONames[RCLCONames %in% namevec],namevec)
  RCLCOList=y.cf[RCLCOInd]
  RCLCOVals=y.v[RCLCOInd]
  RCLCOTot=do.call(zoosum,RCLCOList)
  y.cf=c(y.cf,list(RCLCOTot))
  y.v=c(y.v,list(do.call(zoosum,RCLCOVals)))
  y.hv=c(y.hv,list(c(NA)))
  unf=c(unf,sum(unf[RCLCOInd]))
  commit=c(commit,sum(commit[RCLCOInd]))
  consultantvec=c(consultantvec,NA)
  
}


#add names to y.cf, y.v and y.hv
names(y.cf)=namevec
names(y.v)=namevec
names(y.hv)=namevec



# we are now done building the lists of cash flows and values


# now we are going to grab index data
# now held in a series of csv files
# when KP wrote this code it got the data from BBG
# need to investigate code that creates the csv files
#

#setup dates
#add missing days to benchmark
#fill NAs with by linear interpolation with neighboring values
first=-5+min(x.all$Date)
last=max(x.all$Date)

days365=zooreg(rep(0,1+last-first),start=first,end=last)

#add SPY to benchmark
SPY=read.csv('data/spx index.csv')
SPYd=as.Date(SPY[,2],format='%Y-%m-%d')
SPY.z=zoo(SPY[,3],SPYd)
SPY.z=na.approx(merge(days365,SPY.z)[,2],na.rm=FALSE)

#add ^RUT to benchmark
RUT=read.csv('data/rty index.csv')
RUTd=as.Date(RUT[,2])#,format='%Y-%m-%d')
RUT.z=zoo(RUT[,3],RUTd)
RUT.z=na.approx(merge(days365,RUT.z)[,2],na.rm=FALSE)

#add VBMFX (bonds) to benchmark
VBMFX=read.csv('data/vbmfx equity.csv')
VBMFXd=as.Date(VBMFX[,2])#,format='%Y-%m-%d')
VBMFX.z=zoo(VBMFX[,3],VBMFXd)
VBMFX.z=na.approx(merge(days365,VBMFX.z)[,2],na.rm=FALSE)

#add ODCE to the benchmark
ODCE=read.csv('data/odce daily index.csv')
ODCE[,1]=as.Date(ODCE[,1])#,format='%m/%d/%Y')
ODCE.z=zoo(ODCE[,2],ODCE[,1])
ODCE.z=na.approx(merge(days365,ODCE.z)[,2],na.rm=FALSE)

#add Fixed 8 to the benchmark
fixed8=exp(cumsum(rep(log(1.08)/365,length(ODCE.z))))
fixed8.z=zoo(fixed8,time(ODCE.z))
#bench.lst[[i+4]]=fixed8.z

####################################################
#This Benchmark adds basis points to the index
#add levered loans to benchmark
lli=read.csv('data/spbdal index.csv')
llid=as.Date(lli[,2],format='%Y-%m-%d')
lli.z=zoo(lli[,3],llid)
lli.z=na.approx(merge(days365,lli.z)[,2],na.rm=FALSE)

#add 250 to lli benchmark
lli.notna=which(!is.na(lli.z))
lli.cc=as.numeric(log(lli.z[lli.notna]))
lli.z[lli.notna]=(exp(c(0,(cumsum(diff(lli.cc)+log(1.025)/365)))))


#################################################
#This Benchmark adds basis points to the index
#add cpi to list
cpi.df=read.csv('data/CPI X Food Energy.csv')
cpid=as.Date(cpi.df[,2],format="%Y-%m-%d")
cpicc=log(as.numeric(cpi.df[,3]))

#add 350
cpi.350=exp(c(0,(cumsum(diff(cpicc)+log(1.035)/12))))
cpi.z=zoo(cpi.350,cpid)
if(time(lastinvec(cpi.z))<last) cpi.z=mergesum.z(cpi.z,zoo(as.numeric(lastinvec(cpi.z)),last))
cpi.z=na.approx(merge(days365,cpi.z)[,2],na.rm=FALSE)


##############################################
# brings in matrix of R2K sector indices
#add R2K sector indices to list
r2ksec.df = read.csv('data/r2k sector indices.csv')
r2ksecd=as.Date(r2ksec.df[,2],format="%Y-%m-%d")

#r2k fin services
# RGUSFS "R2K Fin Svc"
r2kfin.z=zoo(r2ksec.df[,3],r2ksecd)
r2kfin.z=na.approx(merge(days365,r2kfin.z)[,2],na.rm=FALSE)

#r2k health care
# RGUSHS "R2K Health Care"
r2khc.z=zoo(r2ksec.df[,4],r2ksecd)
r2khc.z=na.approx(merge(days365,r2khc.z)[,2],na.rm=FALSE)

#r2k technology
# RGUSTS "R2K Tech"
r2ktech.z=zoo(r2ksec.df[,5],r2ksecd)
r2ktech.z=na.approx(merge(days365,r2ktech.z)[,2],na.rm=FALSE)

#r2k producer durables
# RGUSPS "R2K Durables"
r2kprod.z=zoo(r2ksec.df[,6],r2ksecd)
r2kprod.z=na.approx(merge(days365,r2kprod.z)[,2],na.rm=FALSE)

#r2k consumer disc
# RGUSDS "R2K Consumer Disc" 
r2kcd.z=zoo(r2ksec.df[,7],r2ksecd)
r2kcd.z=na.approx(merge(days365,r2kcd.z)[,2],na.rm=FALSE)

#r2k material
# RGUSMS "R2K Materials"
r2kmat.z=zoo(r2ksec.df[,8],r2ksecd)
r2kmat.z=na.approx(merge(days365,r2kmat.z)[,2],na.rm=FALSE)

#r2k utilities
# RGUSUS "R2K Utilities"
r2kutil.z=zoo(r2ksec.df[,9],r2ksecd)
r2kutil.z=na.approx(merge(days365,r2kutil.z)[,2],na.rm=FALSE)

#r2k energy
# RGUSES "R2K Energy"
r2ken.z=zoo(r2ksec.df[,10],r2ksecd)
r2ken.z=na.approx(merge(days365,r2ken.z)[,2],na.rm=FALSE)

#r2k consumer staples
# RGUSSS  "R2K Staples"
r2kstaple.z=zoo(r2ksec.df[,11],r2ksecd)
r2kstaple.z=na.approx(merge(days365,r2kstaple.z)[,2],na.rm=FALSE)

# put the benchmarks in to a list
bench.lst = list(SPY.z, RUT.z, VBMFX.z, ODCE.z, fixed8.z, lli.z, cpi.z, r2kfin.z, r2khc.z, r2ktech.z, r2kprod.z, r2kcd.z, r2kmat.z, r2kutil.z, r2ken.z, r2kstaple.z)
#add the names to the list
names(bench.lst)=ticker.vec=c('SPY','^RUT','VBMFX','ODCE','Fixed8','LevLoan.250','CPIxFE.350','RGUSFS','RGUSHS', 'RGUSTS', 'RGUSPS', 'RGUSDS','RGUSMS', 'RGUSUS',
                              'RGUSES', 'RGUSSS')
benchnames=c("S&P 500","Russell 2K","Bonds",'ODCE','Fixed 8','Lev Loan+250','CPIxFE+350', 'R2K Fin Svc', 	'R2K Health Care', 'R2K Tech', 'R2K Durables',
             'R2K Consumer Disc',	'R2K Materials',	'R2K Utilities',	'R2K Energy', 'R2K Staples')

benchmarks_z = do.call('merge', bench.lst)
benchmarks_df = tk_tbl(benchmarks_z) %>% rename(date = index)
benchmarks_df = benchmarks_df[, colSums(is.na(benchmarks_df)) != nrow(benchmarks_df)] # remove rows containing ONLY NA
benchmarks_df = benchmarks_df %>% 
  mutate(date = as.character(date)) %>%
  gather(shortname, price, -date) %>%
  arrange(shortname,date)


# Convert list of zoo objects to dataframes
y.cf_z = do.call('merge', y.cf) # merge to wide format
y.cf_df = tk_tbl(y.cf_z) %>% rename(date = index)
y.cf_df = y.cf_df[, colSums(is.na(y.cf_df)) != nrow(y.cf_df)] # remove rows containing ONLY NA
y.cf_df = y.cf_df %>% 
  as_tibble() %>%
  mutate(date = as.character(date)) %>%
  gather(shortname, amount, -date) %>%
  arrange(shortname,date)

y.v_z = do.call('merge', y.v) # merge to wide format
y.v_df = tk_tbl(y.v_z) %>% rename(date = index)
y.v_df = y.v_df[, colSums(is.na(y.v_df)) != nrow(y.v_df)] # remove rows containing ONLY NA
y.v_df = y.v_df %>% 
  as_tibble() %>%
  mutate(date = as.character(date)) %>%
  gather(shortname, amount, -date) %>%
  arrange(shortname,date)

y.hv_z = do.call('merge', y.hv) # merge to wide format
y.hv_df = tk_tbl(y.hv_z) %>% rename(date = index)
y.hv_df = y.hv_df[, colSums(is.na(y.hv_df)) != nrow(y.hv_df)] # remove rows containing ONLY NA
y.hv_df = y.hv_df %>% 
  as_tibble() %>%
  mutate(date = as.character(date)) %>%
  gather(shortname, amount, -date) %>%
  arrange(shortname,date)

# Join HV and V 
y_df = bind_rows(y.v_df, y.hv_df)

# Check for duplication
if(nrow(y.cf_df) - nrow(y.cf_df %>% unique()) != 0){warning('Duplication exists in data y.cf')}
if(nrow(y.v_df) - nrow(y.v_df %>% unique()) != 0){warning('Duplication exists in data y.v')}
if(nrow(y.hv_df) - nrow(y.hv_df %>% unique()) != 0){warning('Duplication exists in data y.hv')}
if(nrow(y_df) - nrow(y_df %>% unique()) != 0){warning('Duplication exists in data y_df (overlap between y.v and y.hv)')}

##########################
#TODO: BREAK loading database, must choose any duplicate value before
#---must be investigated by hand
#---break for both cashflows and values
#---if the duplicate values are the same, it's okay, if different, break
#############################

###TODO: Check these... MAKING ASSUMPTIONS ###
y_df = y_df %>%
  group_by(date, shortname) %>%
  summarize(amount = max(amount))
y.cf_df = y.cf_df %>%
  group_by(date, shortname) %>%
  summarize(amount = max(amount))

# Clean fundinfo for database
fundinfo_df = fundinfo
names(fundinfo_df) = tolower(names(fundinfo_df))
fundinfo_df = fundinfo_df %>%
  rename(unfunded_date = unfunded.date,
         yield_amt = yield,
         class_type = class)

# Truncate tables before inserting
dbSendQuery(con, "DELETE FROM fundinfo;")
dbWriteTable(con, name = 'fundinfo', value = fundinfo_df, row.names = FALSE, append = TRUE)

dbSendQuery(con, "DELETE FROM nav;")
dbWriteTable(con, name = 'nav', value = y_df, row.names = FALSE, append = TRUE)

dbSendQuery(con, "DELETE FROM cashflow;")
dbWriteTable(con, name = 'cashflow', value = y.cf_df, row.names = FALSE, append = TRUE)

dbSendQuery(con, "DELETE FROM benchmark;")
dbWriteTable(con, name='benchmark', value = benchmarks_df, row.names=FALSE, append=TRUE)

dbDisconnect(con)
