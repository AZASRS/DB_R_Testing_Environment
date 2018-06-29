library(zoo)
library(lubridate)
setwd("P:/IMD/Karl/R projects/pacing pe 2017")
load(file="../private investment performance/pmedata.rdata")
source(file="../basic financial.r")
fundinfo=read.csv(file="../private investment performance/fundinfo.csv")
fundinfo2=read.csv(file="../private investment performance/fundinfo_pacing.csv")
fundinfo=rbind(fundinfo,fundinfo2)
cats=read.csv(file="../private investment performance/category.csv")
fut.text="*Future*"
#
#pick a portfolio to run
#one or the other -- can't both be true
#
isrealestate=FALSE
isprivateequity=TRUE
if (isrealestate & isprivateequity) stop("can't be both real estate and private equity")
if (!(isrealestate | isprivateequity)) stop("must be real estate XOR private equity")
#
#real estate setup
#
if(isrealestate) {
  portfolio="RE"
  fi.re=subset(fundinfo,fundinfo$catshort %in% c("AZ","CorePr","HgRP","EnRP"))
  fi.re$Unfunded.Date=as.Date(fi.re$Unfunded.Date,format="%m/%d/%Y")
  fi.re$Unfunded.Date[is.na(fi.re$Unfunded.Date)]=as.character(as.Date(fundinfo$Unfunded.Date[1],format="%m/%d/%Y"))
  fi.re$TermEnd=as.character(as.Date(fi.re$TermEnd,format="%m/%d/%Y"))
  fi.re$InvestEnd=as.character(as.Date(fi.re$InvestEnd,format="%m/%d/%Y"))
  distdelay=-4
  bow=1.7
  notcalled=0.05
  call.pattern=c(rep(.05,12),rep(.4/12,12))
  #
  #future funds
  #
  nfuturefunds=8
  if(nfuturefunds>0) {
    nubsize=75
    size.grow=0
    size=grow(nubsize,nfuturefunds,size.grow)
    #size=c(150,125,160,170,170)
    Unfunded.f=Commit.f=size
    Start.f=as.yearqtr(as.Date("2020-7-1")+years(0:(nfuturefunds-1)))#today())+(.5*1:nfuturefunds)
    Unfunded.Date.f=-1+as.Date(Start.f)
    InvestEnd.f=Unfunded.Date.f+years(5)
    TermEnd.f=Unfunded.Date.f+years(10)
    #note if any new fields are added to fundinfo file, they need to be reflected here
    future.df=data.frame(Portfolio=rep("RE",nfuturefunds),
                         Short=paste0(fut.text,1:nfuturefunds),
                         Csname=paste("RE Opp Fund",Start.f),
                         catshort=rep("HgRP",nfuturefunds),
                         catlong=NA,
                         Vintage=year(Start.f),
                         Unfunded.Date=as.character(Unfunded.Date.f),
                         Commit=size,
                         Unfunded=size,
                         Benchmark=NA,
                         Legacy=NA,
                         CSDate=NA,
                         Closed=NA,
                         InvestEnd=as.character(InvestEnd.f),
                         TermEnd=as.character(TermEnd.f),
                         Extension=NA,
                         ExtTime=NA,
                         ExtUsed=NA,
                         Grow=NA,
                         Yield=NA,
                         Method=rep("",nfuturefunds),
                         ICPCat=rep("",nfuturefunds),
                         Consultant=rep("",nfuturefunds),
                         Sponsor=rep("",nfuturefunds),
                         AdvBoard=rep(FALSE,nfuturefunds),
                         FundSizeM=rep(0,nfuturefunds),
                         Ticker="") 
    fi.re=rbind(fi.re,future.df) 
  }
}
#
#private equity setup
#
act.funds <- dim(subset(fundinfo,fundinfo$Portfolio==portfolio))[1]
if (isprivateequity) {
  portfolio="PE"
  fi.re=subset(fundinfo,fundinfo$Portfolio==portfolio)
  fi.re$Unfunded.Date=as.character(as.Date(fundinfo$Unfunded.Date[1],format="%m/%d/%Y"))
  fi.re$TermEnd=as.character(as.Date(fi.re$TermEnd,format="%m/%d/%Y"))
  fi.re$InvestEnd=as.character(as.Date(fi.re$InvestEnd,format="%m/%d/%Y"))
  distdelay=-3
  bow=1.6
  notcalled=.10
  call.pattern=c(.02,.04,rep(.58/10,10),rep(.36/8,8))
  #
  #future funds
  #
  nfuturefunds=50
  if(nfuturefunds>0) {
  nubsize=(712)/4
  size.grow=.05/4
  size=grow(nubsize,nfuturefunds,size.grow)
  #hard wire first two quarters for end of 2014 
  #comment out the following two lines if you do not want this hard wire
  #size[1]=0
  #size[2]=0
  #end of hard wiring code
  Unfunded.f=Commit.f=size
  Start.f=as.yearqtr(today())+(.25*1:nfuturefunds)
  Unfunded.Date.f=-1+as.Date(Start.f)
  InvestEnd.f=Unfunded.Date.f+years(5)
  TermEnd.f=Unfunded.Date.f+years(15)
  #note if any new fields are added to fundinfo file, they need to be reflected here
  future.df=data.frame(Portfolio=rep("PE",nfuturefunds),
                       Short=paste0(fut.text,1:nfuturefunds),
                       Csname=paste("Buyout",Start.f),
                       catshort=rep("MedBo",nfuturefunds),
                       catlong=NA,
                       Vintage=year(Start.f),
                       Unfunded.Date=as.character(Unfunded.Date.f),
                       Commit=size,
                       Unfunded=size,
                       Benchmark=NA,
                       Legacy=NA,
                       CSDate=NA,
                       Closed=NA,
                       InvestEnd=as.character(InvestEnd.f),
                       TermEnd=as.character(TermEnd.f),
                       Extension=NA,
                       ExtTime=NA,
                       ExtUsed=NA,
                       Grow=NA,
                       Yield=NA,
                       Method=rep("",nfuturefunds),
                       ICPCat=rep("",nfuturefunds),
                       Consultant=rep("",nfuturefunds),
                       Sponsor=rep("",nfuturefunds),
                       AdvBoard=rep(FALSE,nfuturefunds),
                       FundSizeM=rep(0,nfuturefunds),
                       Ticker=NA) 
  fi.re=rbind(fi.re,future.df) 
  }
}
#
#set up date variables
#
vint.re=fi.re$Vintage
start.re=as.Date(as.yearqtr(vint.re+.5))  
ei.re=as.Date(fi.re$InvestEnd)
end.re=as.Date(fi.re$TermEnd)
for (i in 1:length(vint.re)) {
  if (is.na(ei.re[i])) ei.re[i]=start.re[i]+years(5)
  m.d=fi.re$Method[i]
  if (m.d==""|is.na(m.d)) m.d=cats[cats$catshort==fi.re[i,"catshort"],"method.d"]
  if(is.na(end.re[i])) {
    if(m.d=="P") {
       end.re[i]=as.Date(as.yearqtr(today()+years(15)))} else { 
       end.re[i]=ei.re[i]+years(5)
     }
    
  }
  if(((today()-start.re[i])/365)>3) {
    if(pme.df[pme.df$name==fi.re$Short[i],"Fund.IRR"]<6) end.re[i]=end.re[i]+years(1)
    if(pme.df[pme.df$name==fi.re$Short[i],"Fund.IRR"]<9) end.re[i]=end.re[i]+years(1)
  }
}
#
#calculate capital calls
#
call.re=list()
call.name=vector()
tofund=(pmax(0,(fi.re$Unfunded-(notcalled*fi.re$Commit))))*-1000000

for (i in 1:length(vint.re)) {
  unf.date=as.Date(fi.re$Unfunded.Date[i])
  if (tofund[i]==0) next
  if((ei.re[i]+years(1))<today()) next
  #adjust tofund by the amount of capital calls since the unfunded.date
  cf.n=which(names(y.cf)==fi.re$Short[i])
  if(length(cf.n)==0) {
    cf.i=zoo(0,unf.date)} else {
    cf.i=y.cf[[cf.n]]
    }
  tofund[i]=min(0,tofund[i]-min(0,sum(as.numeric(cf.i[time(cf.i)>unf.date]))))
  #number of quarterly contributions
  ncall=min(length(call.pattern),max(2,floor(as.numeric((ei.re[i]+years(1)-today())/91))))
  #set quarterly contributions
  call.pattern.i=call.pattern[(1+length(call.pattern)-ncall):length(call.pattern)]
  call.pattern.i=call.pattern.i*(1/sum(call.pattern.i))
  call.i=call.pattern.i*tofund[i]
  call.i=zoo(call.i,max(today(),unf.date)+16+(91*(0:(ncall-1))))
  call.re=c(call.re,list(call.i))
  call.name=c(call.name,as.character(fi.re$Short[i]))
}
call.mat=mergelist.z(call.re)
call.tot=zoo(rowSums(call.mat),index(call.mat))
idx.fut=grep(fut.text,call.name)
call.fut=zoo(rowSums(call.mat[,idx.fut]),index(call.mat))
idx.his=which(!(1:length(call.name)%in%idx.fut))
call.his=zoo(rowSums(call.mat[,idx.his]),index(call.mat))
call.re=c(call.re,list(call.tot),list(call.his),list(call.fut))
names(call.re)=c(call.name,paste("Total",portfolio),paste("Historic",portfolio),paste("Future",portfolio))
#
#calculate distributions and NAV
#
dist.re=list()
unf.re=list()
nav.re=list()
for (i in 1:length(vint.re)) {
  unf.date=as.Date(fi.re$Unfunded.Date[i])
  #determine the applicable distribution method
  m.d=fi.re$Method[i]
  if (m.d==""|is.na(m.d)) m.d=cats[cats$catshort==fi.re[i,"catshort"],"method.d"]
  #determine the applicable growth and yield factors
  g.d=fi.re$Grow[i]
  if (is.na(g.d)) g.d=cats[cats$catshort==fi.re[i,"catshort"],"growth.d"]
  y.d=fi.re$Yield[i]
  if (is.na(y.d)) y.d=cats[cats$catshort==fi.re[i,"catshort"],"yield.d"]
  #get the latest NAV
  name.i=fi.re$Short[i]
  nav.n=which(names(y.v)==name.i)
  if(length(nav.n)==0) {
    nav.v=zoo(0,as.Date(as.yearqtr(unf.date))-1)
    nav.hv=NULL } else {
    nav.v=y.v[[nav.n]]
    nav.hv=y.hv[[nav.n]]
    }
  nav.pred=nav.d=vector()
  if (length(nav.hv)==0) {
    nav.d=nav.v
    nav.pred=NULL} else {
  if (time(nav.v)>=(max(time(nav.hv)))) {
    nav.d=nav.v 
    nav.pred=NULL} else {
    nav.d=nav.hv[max(time(nav.hv))]
    nav.pred=nav.v
  }}
  if (is.na(nav.d)) nav.d=zoo(0,as.Date(as.yearqtr(unf.date))-1)
  #determine a cash flow, known and projected, that will be added to NAV
  cf.n=which(name.i==names(call.re))
  if(length(nav.n)==0) {
    cf.i=zoo(0,unf.date) } else {
    cf.i=y.cf[[nav.n]]
    }
  cf.p=zoo(0,time(nav.d))
  if (length(cf.n)==1) cf.p=call.re[[cf.n]]
  call.d=cf.p
  intcall=min(0,sum(cf.i[time(cf.i)>unf.date]))
  if(intcall<0) call.d=c(zoo(intcall,-45+today()),call.d)
  cf.d=mergesum.z(cf.i,cf.p)  
  #calculate the NAVs and distributions, quarter by quarter
  #how many quarters?
  n.q=0
  n.q=max(4,floor(as.numeric(end.re[i]-time(nav.d))/90))
  dates.d=-1+((time(nav.d)+1)+months(3*(1:n.q)))
  startdate=time(nav.d)#-1+(time(nav.d)+1+months(3))
  lastval=as.numeric(nav.d)
  lastunf=fi.re$Unfunded[i]*1000000
  unf.d=yield.d=dist.d=vals.d=rep(0,length(dates.d))
  sldivisor=dates.d>max(today()+16,(ei.re[i]+years(distdelay)))
  ndis=sum(sldivisor)
  notdis=sum(!sldivisor)
  sldivisor=c(rep(0,notdis),ndis:1)
  if (m.d=="T"){
    offset=max(0,(year(nav.d)-fi.re$Vintage[i])*4)
    sldivisor=c(rep(0,notdis),(offset+notdis+(1:ndis)))
    fundlength=offset+length(sldivisor)
    sldivisor=4/((sldivisor/fundlength)^bow)
    sldivisor[length(sldivisor)-(3:0)]=4:1
  }
  for (j in 1:length(dates.d)) {
    if (j>1) {
      startdate=dates.d[j-1]
      lastval=vals.d[j-1]
      lastunf=unf.d[j-1]
    }
    enddate=dates.d[j]
    cf.j=as.numeric(cf.d[(time(cf.d)>startdate)&(time(cf.d)<=enddate)])
    call.j=as.numeric(call.d[(time(call.d)>startdate)&(time(call.d)<=enddate)])
    if(dates.d[j]>today()) yield.d[j]=lastval*y.d/4
    lastval=lastval*(1+(g.d/4))
    if (y.d>0) {
      vals.d[j]=lastval-sum(cf.j[cf.j<0])
      unf.d[j]=max(0,lastunf+sum(call.j))
      } else {
      vals.d[j]=lastval-sum(cf.j)
      unf.d[j]=max(0,lastunf+sum(call.j))
    }
    if (dates.d[j]>(ei.re[i]+years(2))) unf.d[j]=min(unf.d[j],.1*1000000*fi.re$Commit[i])
    if (sldivisor[j]>0 & (m.d!="P") & (dates.d[j]>today())) dist.d[j]=vals.d[j]/sldivisor[j]
    if (m.d=="P"&j==length(dates.d)) dist.d[j]=vals.d[j]
    vals.d[j]=vals.d[j]-dist.d[j]
    }
  vals.d=zoo(vals.d,dates.d)
  if(!is.null(nav.pred)) vals.d=mergesum.z(vals.d,nav.d)
  dist.d=zoo(dist.d+yield.d,dates.d)
  unf.d=zoo(unf.d,dates.d)
  nav.re=c(nav.re,list(vals.d))
  dist.re=c(dist.re,list(dist.d))
  unf.re=c(unf.re,list(unf.d))
}
idx.fut=grep(fut.text,as.character(fi.re$Short))
idx.his=which(!(1:length(fi.re$Short)%in%idx.fut))

nav.mat=(mergelist.z(nav.re))
nav.tot=zoo(rowSums(nav.mat),index(nav.mat))
nav.fut=zoo(rowSums(nav.mat[,idx.fut]),index(nav.mat))
nav.his=zoo(rowSums(nav.mat[,idx.his]),index(nav.mat))
nav.re=c(nav.re,list(nav.tot),list(nav.his),list(nav.fut))
names(nav.re)=c(as.character(fi.re$Short)
                ,paste("Total",portfolio),paste("Historic",portfolio),paste("Future",portfolio))

dist.mat=(mergelist.z(dist.re))
dist.tot=zoo(rowSums(dist.mat),index(dist.mat))
dist.fut=zoo(rowSums(dist.mat[,idx.fut]),index(dist.mat))
dist.his=zoo(rowSums(dist.mat[,idx.his]),index(dist.mat))
dist.re=c(dist.re,list(dist.tot),list(dist.his),list(dist.fut))
names(dist.re)=c(as.character(fi.re$Short)
                ,paste("Total",portfolio),paste("Historic",portfolio),paste("Future",portfolio))

unf.mat=(mergelist.z(unf.re))
unf.tot=zoo(rowSums(unf.mat),index(unf.mat))
unf.fut=zoo(rowSums(unf.mat[,idx.fut]),index(unf.mat))
unf.his=zoo(rowSums(unf.mat[,idx.his]),index(unf.mat))
unf.re=c(unf.re,list(unf.tot),list(unf.his),list(unf.fut))
names(unf.re)=c(as.character(fi.re$Short)
                 ,paste("Total",portfolio),paste("Historic",portfolio),paste("Future",portfolio))

call.df=mergelist.z(call.re)
call.df=aggregate(call.df,as.yearqtr,sum)
rownames(call.df)=as.character(time(call.df))
write.csv(call.df,file=paste0(portfolio,"call.csv"))
nav.df=mergelist.z(nav.re)
nav.df=aggregate(nav.df,as.yearqtr,lastinvec)
rownames(nav.df)=as.character(time(nav.df))
write.csv(nav.df,file=paste0(portfolio,"nav.csv"))
dist.df=mergelist.z(dist.re)
dist.df=aggregate(dist.df,as.yearqtr,sum)
rownames(dist.df)=as.character(time(dist.df))
write.csv(dist.df,file=paste0(portfolio,"dist.csv"))
unf.df=mergelist.z(unf.re)
unf.df=aggregate(unf.df,as.yearqtr,sum)
rownames(unf.df)=as.character(time(unf.df))
write.csv(unf.df,file=paste0(portfolio,"unf.csv"))
save.image(file=paste0(portfolio,"pacing.rdata"))

