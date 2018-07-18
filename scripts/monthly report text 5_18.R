#monthly report
## @knitr setup
setwd("P:/IMD/Karl/R projects/private investment performance")
load('pmedata.rdata')
source('../basic financial.r')
require(xtable)
require(zoo)
require(lubridate)
# get values for valdate and lastcfdate
valdf=read.csv('tfundvals.csv')
valdf$dates=as.Date(valdf$dates, format='%m/%d/%Y')
tfund.valdate=valdf$vals[which(valdf$dates==valdate)]
tfund.cfdate=valdf$vals[which(valdf$dates==lastcfdate)]
if (is.null(tfund.valdate)) print(paste(
  'need total fund value in tfundvals.csv for', valdate))
if (is.null(tfund.cfdate)) print(paste(
  'need total fund value in tfundvals.csv for', lastcfdate))
twrs=read.csv(file='twrs.csv')
twrs$Twrdate=as.Date(twrs$Twrdate,format='%m/%d/%Y')
twrs=subset(twrs,Twrdate==valdate)
p2pirrs=read.csv(file='p2pirrs.csv') 

portmat=read.csv("portmat.csv")


#if working on one portfolio and running portfolios individually (or code line individually) or diagnosing report formatting
# skip first line of the function and run the rowpm and beyond.   need to initialize port by assigning argument for 
# portfolio you want to run - e.g. port <- "PE"

print.portfolio=function(port) {
rowpm=which(portmat$portshort==port)
port.longname=as.character(portmat$portlongname[rowpm])
pme.x=subset(pme.df,Portfolio==port)
pme.x=pme.x[-grep("Vintage",rownames(pme.x)),]
pme.x$longname=rownames(pme.x)

#summary page
cf.port=y.cf[[paste("Total",port)]]
val.valdate=pme.x[paste("Total",port),"val"]
val.cfdate=sum(pme.x[ ,"Cash Adj NAV"][pme.x$isfund&(!pme.x$sponsorsum)])
draw.valdate=-sum(cf.port[(time(cf.port)<=valdate)&cf.port<0])
draw.sincevaldate=-sum(cf.port[(time(cf.port)>valdate)&cf.port<0])
dist.valdate=-sum(cf.port[(time(cf.port)<=valdate)&cf.port>0])
dist.sincevaldate=-sum(cf.port[(time(cf.port)>valdate)&cf.port>0])
deltaval.valdate=-dist.valdate+val.valdate-draw.valdate
deltaval.sincevaldate=-dist.sincevaldate+val.cfdate-draw.sincevaldate-val.valdate
valpct.valdate=100*val.valdate/(tfund.valdate*1000000)
valpct.cfdate=100*val.cfdate/(tfund.cfdate*1000000)
# portirr.valdate=pme.x[paste("Total",port),"Fund.IRR"]
# portirr.cfdate=100*irr.z(mergesum.z(cf.port,zoo(val.cfdate,lastcfdate)))
portunf=pme.x[paste("Total",port),"unf"]
portexp=val.cfdate+(portunf*1000000)+sum(cf.port[(time(cf.port)>unfunded.date)&cf.port<0])
portexppct=100*portexp/(tfund.cfdate*1000000)
roll12draw=sum(cf.port[(time(cf.port)>(lastcfdate-years(1)))&cf.port<0])
roll12dist=sum(cf.port[(time(cf.port)>(lastcfdate-years(1)))&cf.port>0])
roll12net=roll12dist+roll12draw
titleframe=data.frame(titles=c("Arizona State Retirement System",paste(port.longname,"Portfolio"),
           paste("Information compiled through",format(lastcfdate,"%m/%d/%Y"))))
title.x=xtable(titleframe)
align(title.x)='lc'
print(title.x,hline.after=NULL,include.rownames=FALSE,include.colnames=FALSE)
column1=c(rep("    ",2),formatwblank(draw.valdate/1000000),
          formatwblank(dist.valdate/1000000),
          formatwblank(deltaval.valdate/1000000),
          formatwblank(val.valdate/1000000),"    ",
          formatwblank(valpct.valdate,2,'%'),
          rep("    ",9)          
          )
column2=c("   ",formatwblank(val.valdate/1000000),
          formatwblank(draw.sincevaldate/1000000),
          formatwblank(dist.sincevaldate/1000000),
          formatwblank(deltaval.sincevaldate/1000000),
          formatwblank(val.cfdate/1000000),"    ",
          formatwblank(valpct.cfdate,2,'%'),"    ",
          formatwblank(portunf),
          formatwblank(portexp/1000000),
          formatwblank(portexppct,2,'%'),"    ","    ",
          formatwblank(roll12draw/1000000),
          formatwblank(roll12dist/1000000),
          formatwblank(roll12net/1000000)
          )
policy.txt=paste0("    Policy weight equals ",portmat[rowpm,"pol.low"],'%')
if (portmat[rowpm,"pol.high"]>portmat[rowpm,"pol.low"]) policy.txt=paste0(policy.txt,' to ',
                                                                     portmat[rowpm,"pol.high"],'%')
textcol=c("   ","Beginning NAV","Contributions","Distributions",
                     "Net Increase/(Decrease)","Ending NAV",
                     "    ","Percent of Total Fund","    ",
                     paste("Unfunded Commitments as of",format(unfunded.date,"%m/%d/%Y")),
                     "Estimated exposure of ending NAV and net unfunded",
                     "Estimated exposure (NAV+unfunded) as % of TF_NAV",
                     policy.txt,
                      "    ",
                     "Rolling 12 month draws","Rolling 12 month distribution",
                     "Rolling 12 month net cash flow")
printmat=data.frame(textcol,column1,column2)
colnames(printmat)=c(" ",
                     paste("Reported by SS-AIS as of",format(valdate,"%m/%d/%Y")),
                     paste("Estimated as of",format(lastcfdate,"%m/%d/%Y")))
printmat.x=xtable(printmat)
align(printmat.x)='l|r|ZZ|'
print(printmat.x,tabular.environment='tabularx',
      width='5.5in',scalebox=.9,include.rownames=FALSE,
      hline.after=c(-1,0,6,9,13,length(column1)))                   
                     
           

#return table

row.re=which(twrs$Asset==port) 
row.odce=which(twrs$Asset==as.character(portmat[rowpm,'b1'])) 
if(port=='PE') row.odce=c(row.odce,row.odce+1)
if(port=='POPP') row.odce=row.odce[1]
if(port=='OPP') row.odce=row.odce[2]
twrs.re=twrs[c(row.re,row.odce),2:6] 
row.re=which(p2pirrs$X==port.longname) 
row.odce=which(p2pirrs$X==as.character(portmat[rowpm,'b2'])) 
if(port=='PE') row.odce=c(row.odce,row.odce+1)
if(port=='POPP') row.odce=row.odce[1]
if(port=='OPP') row.odce=row.odce[2]
p2pirrs.re=p2pirrs[c(row.re,row.odce),2:6] 
countrows=sum(length(row.re),length(row.odce))
p2pirrs.re=matrix(paste0(format(as.vector(as.matrix(p2pirrs.re))),'%'),ncol=5,nrow=countrows)
twrs.re=matrix(paste0(format(as.vector(as.matrix(twrs.re))),'%'),ncol=5,nrow=countrows) 
rn1=c(paste(port.longname,"IRR"),
      paste(portmat[rowpm,'b2'],"IRR")) 
if(port=='PE') rn1=c(rn1,'Burgiss IRR')
rn2=c(paste(port.longname,"TWR"),
      paste(portmat[rowpm,'b2'],"TWR"))
if(port=='PE') rn2=c(rn2,'Burgiss TWR')
rownames(p2pirrs.re)=rn1
rownames(twrs.re)= rn2
colnames(twrs.re)=colnames(p2pirrs.re)=c(
  "One Quarter", "One Year","Three Years","Five Years",
  paste0("Inception (",portmat[rowpm,"inception"],")"))
twrs.re.x=xtable(rbind(p2pirrs.re),
                 caption=paste("Summary of IRRs as of",format(valdate,'%m/%d/%Y')))
align(twrs.re.x)='|rZZZZZ|'
print(twrs.re.x,tabular.environment='tabularx',width='6.5in',
      scalebox=.8,caption.placement="top",hline.after=c(-1,0,nrow(twrs.re.x)))

#put a blank row after each non-fund (a subtotal)
blanksgo=vector()
nb=which(!pme.x$isfund)
if(length(nb)>=2) {
nbbot=c(1,1+nb[-length(nb)])
nbtop=nb
nbnbot=nbbot+(0:(-1+length(nb)))
nbntop=nbtop+(0:(-1+length(nb)))
}
blanksgo=nbnbot[-1]-1
pme.xn=pme.x
for (i in 1:length(nb)) {
  pme.xn[nbnbot[i]:nbntop[i],]=pme.x[nbbot[i]:nbtop[i],]
  if(i!=length(nb)) pme.xn[blanksgo[i],]=rep(NA,ncol(pme.xn))
}
pme.xn$longname[blanksgo]=''
rownames(pme.xn)=NULL
blanksgo=c(blanksgo,nrow(pme.xn))



## Adjust data frames for portfolio PME's

if (port == "PE") pme.xn2=pme.xn[,c("longname", "vint", "commit", "drawn", "distributed",
               "unf", "appr", "Cash Adj NAV", "dpi", "Fund TVPI", "Fund IRR", "Russell 2K PME", "Fund IRR 2")]

if (port == "RE") pme.xn2=pme.xn[,c("longname","vint","commit","drawn","distributed",
                                    "unf","appr","Cash Adj NAV","dpi","Fund TVPI","Fund IRR", "ODCE PME", "Fund IRR 2")]

if (port == "POPP") pme.xn2=pme.xn[,c("longname", "vint", "commit", "drawn", "distributed",
                                    "unf", "appr", "Cash Adj NAV", "dpi", "Fund TVPI", "Fund IRR","Fixed 8 PME", "Fund IRR 2")]

if (port == "PD") pme.xn2=pme.xn[,c("longname","vint","commit","drawn","distributed",
                                    "unf","appr","Cash Adj NAV","dpi","Fund TVPI", "Fund IRR", "Lev Loan+250 PME", "Fund IRR 2")]

if (port == "OPP") pme.xn2=pme.xn[,c("longname","vint","commit","drawn","distributed",
                                    "unf","appr","Cash Adj NAV","dpi","Fund TVPI", "Fund IRR", "Fixed 8 PME", "Fund IRR 2")]

if (port == "FARM") pme.xn2=pme.xn[,c("longname","vint","commit","drawn","distributed",
                                     "unf","appr","Cash Adj NAV","dpi","Fund TVPI", "Fund IRR", "CPIxFE+350 PME", "Fund IRR 2")]

#format numbers

pme.xn2$vint=gsub(paste(port,"V"),'',pme.xn2$vint)
pme.xn2$commit=formatwblank(pme.xn2$commit)
pme.xn2$unf=formatwblank(pme.xn2$unf)
pme.xn2$drawn=formatwblank(pme.xn2$drawn/1000000)
pme.xn2$distributed=formatwblank(pme.xn2$distributed/1000000)
pme.xn2$appr=formatwblank(pme.xn2$appr/1000000)
pme.xn2[ ,"Cash Adj NAV"]=formatwblank(pme.xn2[,'Cash Adj NAV']/1000000)
pme.xn2$dpi=formatwblank(pme.xn2$dpi/100,2,' x')
pme.xn2[ ,"Fund TVPI"]=formatwblank(pme.xn2[ , "Fund TVPI"],2,' x')
pme.xn2[ ,"Fund IRR"]=formatwblank(pme.xn2[ , "Fund IRR"],2,'%')
#test
if (port == "PE") pme.xn2[ ,"Russell 2K PME"] = formatwblank(pme.xn2[ ,"Russell 2K PME"],2)
if (port == "RE") pme.xn2[ ,"ODCE PME"] = formatwblank(pme.xn2[ ,"ODCE PME"],2)
if (port == "POPP") pme.xn2[ ,"Fixed 8 PME"] = formatwblank(pme.xn2[ ,"Fixed 8 PME"],2)
if (port == "PD") pme.xn2[ ,"Lev Loan+250 PME"] = formatwblank(pme.xn2[ ,"Lev Loan+250 PME"],2)
if (port == "OPP") pme.xn2[ ,"Fixed 8 PME"] = formatwblank(pme.xn2[ ,"Fixed 8 PME"],2)
if (port == "FARM") pme.xn2[ ,"CPIxFE+350 PME"] = formatwblank(pme.xn2[ ,"CPIxFE+350 PME"],2)

# end test


#pme.xn2[ ,"Russell 2K PME"] = formatwblank(pme.xn2[ ,"Russell 2K PME"],2)
pme.xn2[,"Fund IRR 2"]=formatwblank(pme.xn2[,"Fund IRR 2"],2,'%')
colnames(pme.xn2)=holdname=c("Fund Name","Vintage Year","Committed",
                    paste("Called through",format(lastcfdate,"%m/%d/%Y")),
                    paste("Distributed through",format(lastcfdate,"%m/%d/%Y")),
                    paste("Unfunded as of",format(unfunded.date,"%m/%d/%Y")),
                    "Est. Appreciation","Cash Adjusted Net Asset Value","DPI","TVPI Multiple",
                    paste("IRR as of",format(valdate,"%m/%d/%Y")), paste("PME", format(valdate, "%m/%d/%Y")),
                    "Most Recent IRR")
#print the results
nline=30 #lines per page
nbreak=floor(nrow(pme.xn2)/nline)
if(nbreak==0) breakvec=nrow(pme.xn2)
if(nbreak>0) {
  rowgroups=blanksgo
  breakvec=vector()
  for (z in (1:nbreak)) {
    if (any(rowgroups<nline)) {
    breakvec[z]=max(rowgroups[rowgroups<nline])} else {
    breakvec[z]=nline }
    rowgroups=rowgroups-breakvec[z]
    rowgroups=rowgroups[rowgroups>0]  
    }
  }
breakvec=cumsum(breakvec)
startrow=c(1,breakvec+1)
breakvec=c(breakvec,nrow(pme.xn2))
for (z in 1:(nbreak+1)) {
printrange=startrow[z]:breakvec[z]
hlinevec=(blanksgo[which(blanksgo%in%printrange)])-(printrange[1]-1)
captiontxt=port.longname
if(nbreak>0) captiontxt=paste(captiontxt,z,"of",nbreak+1)
pme.xt=xtable(pme.xn2[printrange,],caption=captiontxt)
align(pme.xt)='l|lYZ|ZZZ|ZZ|ZZ|ZZZ|'
print(pme.xt,tabular.environment='tabularx',floating.environment='sidewaystable',
     width='14in',scalebox=.65,include.rownames=FALSE,hline.after=c(-1,0,hlinevec))
}
cat('\\clearpage')
}
## @knitr print.reports
port.names=as.character(unique(pme.df$Portfolio))
nullvec=sapply(port.names,print.portfolio)
# ##print.re
# print.portfolio("RE")
# ## print.pe
# print.portfolio("PE")
# ## print.popp
# print.portfolio("POPP")
# ## print.opp
# print.portfolio("OPP")
# ## print.pd
# print.portfolio("PD")
# 
