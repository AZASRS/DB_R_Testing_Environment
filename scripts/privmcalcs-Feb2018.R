library(zoo)
library(tseries)
#require(quantmod)
require(lubridate)
#require(Rbbg)
require(tidyr)

#load the ASRS financial methods (replace with library(asrsMethods))
source(file='scripts/basic financial.r')

#read in cash flows and valuation from most recent complete performance period
x.all=read.csv(file='data/AllCashFlowIRR.csv')
#rename column files from download    
colnames(x.all)[1] <- "Date"
colnames(x.all)[2] <- "ShortName"
colnames(x.all)[3] <- "Type"
colnames(x.all)[4] <- "Amount"

#change "EV" to "V" in the x.all frame
x.all$Type <- gsub("EV", "V", x.all$Type)
#add some data needed related to the disposition of TPB specialty lending
Bolt = read.csv(file='data/BoltOnCFStockSales.csv')
# KP doesn't know what this is
CCash = read.csv(file='data/PI Transactions.csv')
#rename column files from download    
colnames(CCash)[1] <- "ShortName"
colnames(CCash)[2] <- "Date"
colnames(CCash)[3] <- "Contributions"
colnames(CCash)[4] <- "Distributions"

#math
# makes distributions positive and contributions negative and nets them when on the same day
CCash$Amount <- (CCash$Distributions - CCash$Contributions)

#remove unneeded columns
CCash$Contributions <- NULL
CCash$Distributions <- NULL

#add column with type "C"
CCash$Type = "C"
x.all <- rbind(x.all, Bolt, CCash)

#read in fund info
fundinfo = read.csv(file='data/fundinfo.csv', stringsAsFactors = FALSE)

#read and combine NAVs
# the hv frame contains the values other than the most recent official performance
hv=read.csv(file='data/AllHistoricalMV.csv', stringsAsFactors=FALSE)
# read in the values that have come in after the last complete performance period
perf = read.csv(file = 'data/Valuation Performance.csv', stringsAsFactors = FALSE)
perf$Investment.Name <- NULL
perf$Type = "V"
colnames(perf)[1] <- "ShortName"
colnames(perf)[2] <- "Amount" 
# valdate is the most recent complete performance period, a quarter ending date
valdate=as.Date(fundinfo[,"CSDate"],format='%m/%d/%Y')[1]
# nextq is the last day of the next quarter after valdate
nextq=valdate+1+months(3)-1
perf$Date=nextq
hv$Date = as.Date(hv$Date,format='%m/%d/%Y')
# add the perf data to hv
hv <- rbind(perf, hv)
write.csv(hv, file='reviewhv.csv')
if(any(is.na(x.all))) warning("NA's in x.all data")
if(any(is.na(hv))) warning("NA's in hv data")

#convert date to date format
x.all$Date=gsub("-","/",as.character(x.all$Date))
x.all$Date=as.Date(x.all$Date,format='%m/%d/%Y')
x.all$Amount=as.numeric(x.all$Amount)
hv$Date=as.Date(hv$Date,format='%m/%d/%Y')
hv$Amount=as.numeric(hv$Amount)

#read in subtotal definitions
cats=read.csv(file='data/Category.csv')
#fundinfo=read.csv(file='fundinfo.csv')
unfunded.date = as.Date(fundinfo$Unfunded.Date,format='%m/%d/%Y')[1]
#valdate=as.Date(fundinfo$CSDate,format='%m/%d/%Y')[1]

#add factors to x.all and hv for category, vintage, portfolio, legacy, Stype, Sector 
x.ind=match(x.all$ShortName,fundinfo$Short)
x.all$cats=fundinfo$catshort[x.ind]
x.all$vints=fundinfo$Vintage[x.ind]
x.all$Portfolio=fundinfo$Portfolio[x.ind]
x.all$Legacy=fundinfo$Legacy[x.ind]
x.all$Stype=fundinfo$Stype[x.ind]
x.all$Sector=fundinfo$Sector[x.ind]
hv.ind=match(hv$ShortName,fundinfo$Short)
hv$cats=fundinfo$catshort[hv.ind]
hv$vints=fundinfo$Vintage[hv.ind]
hv$Portfolio=fundinfo$Portfolio[hv.ind]
hv$Legacy=fundinfo$Legacy[hv.ind]
hv$Stype=fundinfo$Stype[hv.ind]
hv$Sector=fundinfo$Sector[hv.ind]

#separate cash flows from values
x=subset(x.all,x.all$Type=="C")
x.v=subset(x.all,x.all$Type=="V")

# drop names with no activity
fundinfo=fundinfo[sort(unique(x.ind)),]

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

#for each fund and composite calculate performance statistics relative to every benchmark

# initialize vectors for each performance statistic
pme=vector()
IRR=vector()
IRR.l=vector()
TVPI=vector()
TVPI.l=vector()
Unrealized=vector()
Unrealized.l=vector()
Date2=vector()
cval=vector()
drawn=vector()
distributed=vector()
dpi=vector()
for (i in 1:length(y.cf)) {
  # display a value to track progress
  cat(paste0(i,"."))
  # get the cash flow
  fundcf=(y.cf[[i]])
  # get the value
  fundval=(y.v[[i]])
  haveval=TRUE
  if(is.na(fundval)) haveval=FALSE
  #get a historic value if one after valdate is available
  fundval.l=(y.hv[[i]])
  if(length(fundval.l)==0) {fundval.l=NA; haveval.l=FALSE} else {
    haveval.l=TRUE
    fundval.l=lastinvec(fundval.l)
    if(index(fundval.l)<=valdate) {fundval.l=NA;haveval.l=FALSE}
  }
  valdate.l=NA
  fundcf.l=NA
  x.pos.l=NA
  x.neg.l=NA
  #set up cash flow and value data to calculate performance for quarter after valdate, if available
  if(haveval.l) {
    valdate.l=index(fundval.l)
    fundcf.l=fundcf[which(index(fundcf)<=valdate.l)]
    x.pos.l=fundcf.l
    x.pos.l[x.pos.l<0]=0
    x.neg.l=fundcf.l
    x.neg.l[x.neg.l>0]=0
    x.pos.l=mergesum.z(x.pos.l,fundval.l)
  }
  #set up cash flow and value data to calculate performance for valdate quarter
  fundcf.v=fundcf[which(index(fundcf)<=valdate)]
  x.pos=fundcf.v
  x.pos[x.pos<0]=0
  x.neg=fundcf.v
  x.neg[x.neg>0]=0
  x.pos=mergesum.z(x.pos,fundval)
  if (haveval.l) {cval[i]=
    coredata(fundval.l-sum(fundcf[time(fundcf)>valdate.l]))
  }
  if (haveval & !haveval.l) {cval[i]=
    coredata(fundval-sum(fundcf[time(fundcf)>valdate]))
  }
  if (!haveval & !haveval.l) {cval[i]=-sum(fundcf)}
  #calculate drawn distributed and dpi for this investment
  drawn[i]=-sum(fundcf[fundcf<0])
  distributed[i]=sum(fundcf[fundcf>0])
  dpi[i]=100*distributed[i]/drawn[i]
  #setup vectors to calculate the irrs and pmes for this investment
  x.pme=vector()
  x.pme.l=vector()
  x.irr=vector()
  x.irr.l=vector()
  # loop through for every benchmark and calculate performance for available time periods
  for (j in 1:length(ticker.vec)) {
    pme.temp=NA
    # for valdate period if available
    if(haveval) {
      # calcs pme directly instead of using pestats
      ind.pos=match(index(x.pos),index(bench.lst[[j]]))
      ind.neg=match(index(x.neg),index(bench.lst[[j]]))
      mult.pos=(as.numeric(lastinvec(bench.lst[[j]][ind.pos])))/as.numeric(bench.lst[[j]][ind.pos])
      mult.neg=(as.numeric(lastinvec(bench.lst[[j]][ind.neg])))/as.numeric(bench.lst[[j]][ind.neg])
      x.pos.t=sum(mult.pos*x.pos)
      x.neg.t=sum(mult.neg*x.neg)
      if((abs(x.neg.t)<.01)|any(is.na(x.neg.t))) {
        pme.temp=NA} else {
          pme.temp=-x.pos.t/x.neg.t
        }
    }
    pme.temp.l=NA
    #for next quarter if available
    if(haveval.l) {
      ind.pos.l=match(index(x.pos.l),index(bench.lst[[j]]))
      ind.neg.l=match(index(x.neg.l),index(bench.lst[[j]]))
      mult.pos.l=(as.numeric(lastinvec(bench.lst[[j]][ind.pos.l])))/as.numeric(bench.lst[[j]][ind.pos.l])
      mult.neg.l=(as.numeric(lastinvec(bench.lst[[j]][ind.neg.l])))/as.numeric(bench.lst[[j]][ind.neg.l])
      x.pos.t.l=sum(mult.pos.l*x.pos.l)
      x.neg.t.l=sum(mult.neg.l*x.neg.l)
      if((abs(x.neg.t.l)<.01)|any(is.na(x.neg.t.l))) {
        pme.temp.l=NA} else {
          pme.temp.l=-x.pos.t.l/x.neg.t.l
        }
    }
    x.pme=c(x.pme,pme.temp)
    x.pme.l=c(x.pme.l,pme.temp.l)
    pme=c(pme,x.pme[j],x.pme.l[j])
    #irr for investment in benchmark
    # uses pestats (which calculates the PME calculated above all over again)
    if (!haveval) {
      x.irr=c(x.irr,NA) } else {
        ans.pestats=pestats(mergesum.z(x.neg,x.pos),bench.lst[[j]][ind.pos])
        x.irr=c(x.irr,100*ans.pestats$ind.irr)
        ans.pestats=NA
      }  
    if(!haveval.l) {
      x.irr.l=c(x.irr.l,NA)
    } else {
      ans.pestats=pestats(mergesum.z(x.neg.l,x.pos.l),bench.lst[[j]][ind.pos.l])
      x.irr.l=c(x.irr.l,100*ans.pestats$ind.irr)
      ans.pestats=NA
    }
    pme=c(pme,x.irr[j],x.irr.l[j])
  }
  # end of loop through benchmarks
  
  # add next quarter results to vectors
  if(!haveval.l) {
    IRR.l=c(IRR.l,NA)
    TVPI.l=c(TVPI.l,NA)
    Unrealized.l=c(Unrealized.l,NA)
  } else {
    IRR.l=c(IRR.l,100*irr.z(mergesum.z(x.pos.l,x.neg.l),gips=TRUE))
    TVPI.l=c(TVPI.l,tvpi(mergesum.z(x.pos.l,x.neg.l)))
    Unrealized.l=c(Unrealized.l,100*fundval.l/sum(x.pos.l))
  }
  # add valdate period results to vectors
  if(haveval) {
    IRR=c(IRR,100*irr.z(mergesum.z(x.pos,x.neg),gips=TRUE))
    TVPI=c(TVPI,tvpi(mergesum.z(x.pos,x.neg)))
    Unrealized=c(Unrealized,100*fundval/sum(x.pos))
  } else {
    IRR=c(IRR,NA)
    TVPI=c(TVPI,NA)
    Unrealized=c(Unrealized,NA)
  }
  Date2=c(Date2,as.character(valdate.l))
  
}
# end loop through all the portfolios

# build a matrix of the results of all these calculations
pme.mat=matrix(pme,ncol=length(y.cf))
pme.mat=rbind(pme.mat,IRR,IRR.l,TVPI,TVPI.l,Unrealized,Unrealized.l,cval)
benchnamemat=rbind(paste(benchnames,"PME"),
                   paste(benchnames,"PME 2"),
                   paste(benchnames,"Dollar Matched IRR"),
                   paste(benchnames,"Dollar Matched IRR 2"))
rownames(pme.mat)=c(as.vector(benchnamemat),
                    "Fund IRR","Fund IRR 2",
                    "Fund TVPI","Fund TVPI 2",
                    "Unrealized Percent","Unrealized  Percent 2","Cash Adj NAV")
colnames(pme.mat)=longname 
#flip it
pme.mat=t(pme.mat)
#wrte as csv
write.csv(pme.mat,file='pmedat3.csv')

#add categories and vintages and more stuff
pme.df=as.data.frame(pme.mat)
pme.df$cat=catvec
pme.df$vint=vintvec
pme.df$val=as.numeric(as.vector(y.v))
pme.df$unf=unf
pme.df$commit=commit
pme.df$name=namevec
pme.df$Date.2=Date2
pme.df$Legacy=legvec
pme.df$Portfolio=portvec
pme.df$isvint=pme.df$name==pme.df$vint
pme.df$isvint[is.na(pme.df$isvint)]=FALSE
pme.df$istotal=FALSE
pme.df$istotal[grep("Total",pme.df$name)]=TRUE
pme.df$vintsum=pme.df$isvint|pme.df$istotal
pme.df$iscat=pme.df$name==pme.df$cat
pme.df$iscat[is.na(pme.df$iscat)]=FALSE
pme.df$catsum=pme.df$iscat|pme.df$istotal
pme.df$isfund=!(pme.df$catsum|pme.df$vintsum)
pme.df$drawn=drawn
pme.df$distributed=distributed
pme.df$dpi=dpi
pme.df$appr=cval-(drawn-distributed)
pme.df$consultant=consultantvec
pme.df$sponsorsum=sponsorsum
# correct some subtotals for categories and totals
for (i in which(pme.df[,'iscat'])) {
  pme.sub=pme.df[(!pme.df[,'sponsorsum'])&pme.df[,'isfund']&(pme.df[,'cat']==pme.df[,'cat'][i]),]
  pme.df[,'Cash Adj NAV'][i]=sum(pme.sub[ ,'Cash Adj NAV'])
  pme.df[,'drawn'][i]=sum(pme.sub[,'drawn'])
  pme.df[,'distributed'][i]=sum(pme.sub[,'distributed'])
  pme.df[,'appr'][i]=sum(pme.sub[,'appr'])
}
for (i in which(pme.df[,'istotal'])) {
  pme.sub=pme.df[(!pme.df[ ,'sponsorsum']) & pme.df[,'isfund'] & (pme.df[,'Portfolio']==pme.df[,'Portfolio'][i]),]
  if(!is.na(pme.df[,'Legacy'][i])) pme.sub=pme.sub[pme.sub[,'Legacy']==pme.df[,'Legacy'][i],]
  pme.df[,'Cash Adj NAV'][i]=sum(pme.sub[,'Cash Adj NAV'])
  pme.df[,'drawn'][i]=sum(pme.sub[,'drawn'])
  pme.df[,'distributed'][i]=sum(pme.sub[,'distributed'])
  pme.df[,'appr'][i]=sum(pme.sub[,'appr'])
}
#save another csv
write.csv(pme.df,file="pmedf.csv")
# save the rdata image
save.image(file="pmedata.rdata")
