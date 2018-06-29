#Quarterly Performance Report

## @knitr setup
#Chunk Start <<setup, echo=FALSE, warning=FALSE, message=FALSE>>=
library(zoo) 
library(tseries) 
library(ggplot2) 
library(RColorBrewer) 
library(xtable)
setwd("P:/IMD/Karl/R projects/private investment performance") 
load(file="pmedata.rdata")
source(file='../basic financial.r') 
pme.hold=pme.df

#PE Quarter Review

## @knitr PEQtrRev
#Chunk start <<PEQtrRev, echo=FALSE, warning=FALSE, message=FALSE, results='asis'>>=
require(xtable)
twrs=read.csv(file='twrs.csv') 
twrs$Twrdate=as.Date(twrs$Twrdate,format="%m/%d/%Y") 
twrs=subset(twrs,twrs$Twrdate==valdate)
p2pirrs=read.csv(file='p2pirrs.csv') 
row.re=which(twrs$Asset=="PE") 
row.odce=which(twrs$Asset=="R2K") 
row.burgiss=which(twrs$Asset=="Burgiss")
twrs.re=twrs[c(row.re,row.odce,row.burgiss),2:6] 
row.re=which(p2pirrs$X=="Private Equity") 
row.odce=which(p2pirrs$X=="Russell 2000") 
row.burgiss=which(p2pirrs$X=="Burgiss")
p2pirrs.re=p2pirrs[c(row.re,row.odce,row.burgiss),2:6] 
p2pirrs.re=matrix(paste0(format(as.vector(as.matrix(p2pirrs.re))),'%'),ncol=5,nrow=3)
twrs.re=matrix(paste0(format(as.vector(as.matrix(twrs.re))),'%'),ncol=5,nrow=3) 
rownames(p2pirrs.re)=c("Private Equity IRR","Russell 2000 IRR","Burgiss IRR") 
rownames(twrs.re)=c("Private Equity TWR","Russell 2000 TWR","Burgiss TWR") 
colnames(twrs.re)=colnames(p2pirrs.re)=c("One Quarter", "One Year","Three Years","Five Years","Inception")
twrs.re.x=xtable(p2pirrs.re)
align(twrs.re.x)=rep('r',6)
print(twrs.re.x,floating=FALSE,scalebox=.85)

#RE Quarter Review

## @knitr REQtrRev
#Chunk Start <<REQtrRev, echo=FALSE, warning=FALSE, message=FALSE, results='asis'>>=
twrs$Twrdate=as.Date(twrs$Twrdate,format="%m/%d/%Y") 
twrs=subset(twrs,twrs$Twrdate==valdate)
row.re=which(twrs$Asset=="RE") 
row.odce=which(twrs$Asset=="ODCE") 
twrs.re=twrs[c(row.re,row.odce),2:6] 
row.re=which(p2pirrs$X=="Private Real Estate") 
row.odce=which(p2pirrs$X=="ODCE Net") 
p2pirrs.re=p2pirrs[c(row.re,row.odce),2:6] 
p2pirrs.re=matrix(paste0(format(as.vector(as.matrix(p2pirrs.re))),'%'),ncol=5,nrow=2)
twrs.re=matrix(paste0(format(as.vector(as.matrix(twrs.re))),'%'),ncol=5,nrow=2) 
rownames(p2pirrs.re)=c("Real Estate IRR","ODCE IRR") 
rownames(twrs.re)=c("Real Estate TWR","ODCE TWR") 
colnames(twrs.re)=colnames(p2pirrs.re)=c("One Quarter", "One Year","Three Years","Five Years","Inception")
twrs.re.x=xtable(p2pirrs.re)
align(twrs.re.x)=rep('r',6)
print(twrs.re.x,floating=FALSE,scalebox=.9)

#Farmland and Infra Quarter Review

## @knitr FarmInfra
#Chuck Start<<FarmInfra, echo=FALSE, warning=FALSE, message=FALSE, results='asis'>>=
twrs$Twrdate=as.Date(twrs$Twrdate,format="%m/%d/%Y") 
twrs=subset(twrs,twrs$Twrdate==valdate)
row.re=which(twrs$Asset=="FARM") 
row.odce=which(twrs$Asset=="CPIxFE 350") 
twrs.re=twrs[c(row.re,row.odce),2:6] 
row.re=which(p2pirrs$X=="Farmland") 
row.odce=which(p2pirrs$X=="Core CPI+350")
p2pirrs.re=p2pirrs[c(row.re,row.odce),2:6] 
p2pirrs.re=matrix(paste0(format(as.vector(as.matrix(p2pirrs.re))),'%'),ncol=5,nrow=2)
twrs.re=matrix(paste0(format(as.vector(as.matrix(twrs.re))),'%'),ncol=5,nrow=2) 
rownames(p2pirrs.re)=c("Farmland IRR","CPIxFE+350 IRR") 
rownames(twrs.re)=c("Farmland TWR","CPIxFE+350 TWR") 
colnames(twrs.re)=colnames(p2pirrs.re)=c("One Quarter", "One Year","Three Years","Five Years","Inception")
twrs.re.x=xtable(p2pirrs.re)
align(twrs.re.x)=rep('r',6)
print(twrs.re.x,floating=FALSE,scalebox=.9)

#Private Opportunistic Quarter Review

## @knitr POPPRev
#Chunk Start <<POPPRev, echo=FALSE, warning=FALSE, message=FALSE, results='asis'>>=
twrs$Twrdate=as.Date(twrs$Twrdate,format="%m/%d/%Y") 
twrs=subset(twrs,twrs$Twrdate==valdate)
row.re=which(twrs$Asset=="POPP") 
row.odce=row.re+1 
twrs.re=twrs[c(row.re,row.odce),2:6] 
row.re=which(p2pirrs$X=="Private Opportunistic Equity") 
row.odce=row.re+1
p2pirrs.re=p2pirrs[c(row.re,row.odce),2:6] 
p2pirrs.re=matrix(paste0(format(as.vector(as.matrix(p2pirrs.re))),'%'),ncol=5,nrow=2)
twrs.re=matrix(paste0(format(as.vector(as.matrix(twrs.re))),'%'),ncol=5,nrow=2) 
rownames(p2pirrs.re)=c("Private Opportunistic IRR","Absolute 8 IRR") 
rownames(twrs.re)=c("Private Opportunistic TWR","Absolute 8 TWR") 
colnames(twrs.re)=colnames(p2pirrs.re)=c("One Quarter", "One Year","Three Years","Five Years","Inception")
twrs.re.x=xtable(p2pirrs.re)
align(twrs.re.x)=rep('r',6)
print(twrs.re.x,floating=FALSE,scalebox=.9)

#Private Debt Quarter Review

## @knitr PDRev
#Chunk Start <<PDRev, echo=FALSE, warning=FALSE, message=FALSE, results='asis'>>=
twrs$Twrdate=as.Date(twrs$Twrdate,format="%m/%d/%Y") 
twrs=subset(twrs,twrs$Twrdate==valdate)
row.re=which(twrs$Asset=="PD") 
row.odce=row.re+1 
twrs.re=twrs[c(row.re,row.odce),2:6] 
row.re=which(p2pirrs$X=="Private Debt") 
row.odce=row.re+1
p2pirrs.re=p2pirrs[c(row.re,row.odce),2:6] 
p2pirrs.re=matrix(paste0(format(as.vector(as.matrix(p2pirrs.re))),'%'),ncol=5,nrow=2)
twrs.re=matrix(paste0(format(as.vector(as.matrix(twrs.re))),'%'),ncol=5,nrow=2) 
rownames(p2pirrs.re)=c("Private Debt IRR","Lev Loan+250 IRR") 
rownames(twrs.re)=c("Private Debt TWR","Lev Loan+250 TWR") 
colnames(twrs.re)=colnames(p2pirrs.re)=c("One Quarter", "One Year","Three Years","Five Years","Inception")
twrs.re.x=xtable(p2pirrs.re)
align(twrs.re.x)=rep('r',6)
print(twrs.re.x,floating=FALSE,scalebox=.9)

#Opportunistic Debt Quarter Review

## @knitr OPPDebtRev
#Chunk Start <<opp2, echo=FALSE, warning=FALSE, message=FALSE, results='asis'>>=
twrs$Twrdate=as.Date(twrs$Twrdate,format="%m/%d/%Y") 
twrs=subset(twrs,twrs$Twrdate==valdate)
row.re=which(twrs$Asset=="OPP") 
row.odce=row.re+1 
twrs.re=twrs[c(row.re,row.odce),2:6] 
row.re=which(p2pirrs$X=="Opportunistic Fixed Income") 
row.odce=row.re+1
p2pirrs.re=p2pirrs[c(row.re,row.odce),2:6] 
p2pirrs.re=matrix(paste0(format(as.vector(as.matrix(p2pirrs.re))),'%'),ncol=5,nrow=2)
twrs.re=matrix(paste0(format(as.vector(as.matrix(twrs.re))),'%'),ncol=5,nrow=2) 
rownames(p2pirrs.re)=c("Opportunistic Fixed Income IRR","Absolute 8 IRR") 
rownames(twrs.re)=c("Opportunistic Fixed Income TWR","Absolute 8 TWR") 
colnames(twrs.re)=colnames(p2pirrs.re)=c("One Quarter", "One Year","Three Years","Five Years","Inception")
twrs.re.x=xtable(p2pirrs.re)
align(twrs.re.x)=rep('r',6)
print(twrs.re.x,floating=FALSE,scalebox=.8)

#Private Equity - Category Names

## @knitr pecats
#Chunk Start <<pecats,echo=FALSE, results='asis'>>=
portfolio='PE'
cats.sub=subset(cats,cats$Portfolio==portfolio)[,1:3]
cat.table=xtable(cats.sub,caption='Private equity category abbreviations')
print(cat.table,tabular.environment='longtable',floating=FALSE)

# Private Equity - Fund Name Abbreviations

## @knitr peabbr
#Chunk Start <<peabbr,echo=FALSE,results='asis'>>=
funds.sub=subset(fundinfo,fundinfo$Portfolio==portfolio)
funds.sub=cbind(as.character(funds.sub$Short),as.character(funds.sub$Csname))
colnames(funds.sub)=c('Abbreviation','Long Name')
funds.table=xtable(funds.sub,caption='Private equity fund name abbreviations')
print(funds.table,tabular.environment='longtable',floating=FALSE)

## Private Equity - Charts

## @knitr pecharts
#Chunk Start <<pecharts,echo=FALSE, out.height="8in", out.width="6in">>=
pme.df=subset(pme.hold,pme.hold$Portfolio==portfolio)
vintages=unique(pme.df$vint) 
vintages=vintages[!is.na(vintages)] 
pmepal=(brewer.pal(n=10,name="Spectral"))[c(1,3,8,9,10)] 
#drop clear channel -- small outlier 
clrchnind=which(pme.df$name=="ClrChn") 
if (length(clrchnind)==1) pme.df=pme.df[-clrchnind,] 
#remove NAs 
naind=which((is.na(pme.df$`Fund IRR`))|(is.na(pme.df$`Fund TVPI`))) 
if (length(naind)>0) pme.df=pme.df[-naind,] 
palette(pmepal) 
#scatterplots by vintage 
#get rid 2017 
exclude=NULL
exclude=c(exclude,grep("2017",vintages))
if(length(exclude)>0)  vintages=vintages[-exclude] 
vintages=sort(vintages)
for (i in 0:(length(vintages))) {   
  par(mfrow=c(1,1))   
  if(i==0) vindx=which(pme.df$vintsum)   
  if (i>0) vindx=which(pme.df$vint==vintages[i])   
  if (i==0) titletext=paste(portfolio,"Comparison of all vintages")   
  if(i>0) titletext=rownames(pme.df)[lastinvec(vindx)]   
  if(length(vindx)==2) vindx=vindx[1]   
  max.y=max(pme.df$`Fund IRR`[vindx])   
  min.y=min(pme.df$`Fund IRR`[vindx])   
  yinc=(max(.05,(max.y-min.y)))/40   
  max.y=-.005+(.1*(floor(max.y*10)))   
  min.x=min(pme.df$`Fund TVPI`[vindx]) 
  pme.R2k=cut(pme.df$`Russell 2K PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)
  if(portfolio=="RE") pme.R2k=cut(pme.df$`ODCE PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE) 
  scatplot=ggplot(pme.df[vindx,],aes(x=`Fund TVPI`,y=`Fund IRR`))+
    geom_text(label=pme.df$name[vindx],size=1.5,colour=pme.R2k)+
    xlab("TVPI")+
    ylab("IRR")+
    annotate("text",y=max.y,x=min.x,label="PME is > 1.2",size=1.5,colour=pmepal[5],hjust=0)+
    annotate("text",y=max.y-yinc,x=min.x,label="PME is 1.05 to 1.2",size=1.5,colour=pmepal[4],hjust=0)+
    annotate("text",y=max.y-(yinc*2),x=min.x,label="PME is .95 to 1.05",size=1.5,colour=pmepal[3],hjust=0)+
    annotate("text",y=max.y-(yinc*3),x=min.x,label="PME is .8 to .95",size=1.5,colour=pmepal[2],hjust=0)+
    annotate("text",y=max.y-(yinc*4),x=min.x,label="PME is < .8",size=1.5,colour=pmepal[1],hjust=0)+
    ggtitle(titletext)
  print(scatplot)
}
scat.cats=cats$catshort[cats$Portfolio==portfolio] 
scat.cats.long=cats$catlong[cats$Portfolio==portfolio] 
for (i in 0:length(scat.cats)) {   
  par(mfrow=c(1,1))   
  if(i==0) vindx=which(pme.df$catsum)   
  if(i>0) vindx=which(pme.df$cat==scat.cats[i])   
  if(i==0) titletext=paste(portfolio,"Comparison of Strategies")   
  if(i>0) titletext=scat.cats.long[i]   
  if(length(vindx)==2) vindx=vindx[1]   
  max.y=max(pme.df$`Fund IRR`[vindx])   
  min.x=min(pme.df$`Fund TVPI`[vindx])   
  min.y=min(pme.df$`Fund IRR`[vindx])   
  yinc=(max(.05,(max.y-min.y)))/40   
  max.y=-.005+(.1*(floor(max.y*10)))   
  pme.R2k=cut(pme.df$`Russell 2K PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)   
  if(portfolio=="RE") pme.R2k=cut(pme.df$ODCE.PME[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)   
  scatplot=ggplot(pme.df[vindx,],aes(x=`Fund TVPI`,y=`Fund IRR`))+
    geom_text(label=pme.df$name[vindx],size=1.5,colour=pme.R2k)+
    xlab("TVPI")+
    ylab("IRR")+
    annotate("text",y=max.y,x=min.x,label="PME is > 1.2",size=1.5,colour=pmepal[5],hjust=0)+
    annotate("text",y=max.y-yinc,x=min.x,label="PME is 1.05 to 1.2",size=1.5,colour=pmepal[4],hjust=0)+
    annotate("text",y=max.y-(yinc*2),x=min.x,label="PME is .95 to 1.05",size=1.5,colour=pmepal[3],hjust=0)+
    annotate("text",y=max.y-(yinc*3),x=min.x,label="PME is .8 to .95",size=1.5,colour=pmepal[2],hjust=0)+
    annotate("text",y=max.y-(yinc*4),x=min.x,label="PME is < .8",size=1.5,colour=pmepal[1],hjust=0)+
    ggtitle(titletext)
  print(scatplot)
}

#Private Equity Peformance Table

## @knitr petab
#Chunk Start <<petab,echo=FALSE, results='asis'>>=
pme.sub=pme.df[,c("name","Russell 2K PME","Fund TVPI","Fund IRR","Russell 2K Dollar Matched IRR")] 
pme.sub$outp=pme.sub[,4]-pme.sub[,5] 
pme.sub[,2:6]=round(pme.sub[,2:6],2) 
colnames(pme.sub)=c('Fund','R2k PME','TVPI','IRR','R2K IRR','Outperformance') 
rownames(pme.sub)=NULL 
pme.tab=xtable(pme.sub,caption='Private equity performance compared to Russell 2000')
print(pme.tab,tabular.environment='longtable',floating=FALSE)


# REAL ESTATE SECTION

# Real Estate Category Names

## @knitr recats
# Chunk Setup <<recats,echo=FALSE, results='asis'>>=
portfolio='RE'
cats.sub=subset(cats,cats$Portfolio==portfolio)[,1:3]
cat.table=xtable(cats.sub,caption='Real Estate category abbreviations')
print(cat.table,tabular.environment='longtable',floating=FALSE)


# Real Estate Table of Fund Names

## @knitr reabbr
# Chunk Setup <<reabbr,echo=FALSE,results='asis'>>=
funds.sub=subset(fundinfo,fundinfo$Portfolio==portfolio)
funds.sub=cbind(as.character(funds.sub$Short),as.character(funds.sub$Csname))
colnames(funds.sub)=c('Abbreviation','Long Name')
funds.table=xtable(funds.sub,caption='Real Estate fund name abbreviations')
print(funds.table,tabular.environment='longtable',floating=FALSE)


# Real Estate Charts

## @knitr recharts
# Chunk Start<<recharts,echo=FALSE, warning=FALSE, out.height="8in", out.width="6in">>=
pme.df=subset(pme.hold,pme.hold$Portfolio==portfolio)
vintages=unique(pme.df$vint) 
pme.df=subset(pme.hold,pme.hold$Portfolio==portfolio|pme.hold$Portfolio=="POPP")
vintages=vintages[!is.na(vintages)] 
pmepal=(brewer.pal(n=10,name="Spectral"))[c(1,3,8,9,10)] 
#drop clear channel -- small outlier 
clrchnind=which(pme.df$name=="ClrChn") 
if (length(clrchnind)==1) pme.df=pme.df[-clrchnind,] 
#remove NAs 
naind=which((is.na(pme.df$`Fund IRR`))|(is.na(pme.df$`Fund TVPI`))) 
if (length(naind)>0) pme.df=pme.df[-naind,] 
palette(pmepal) 
#scatterplots by vintage 
#get rid 2017 
exclude=NULL
exclude=c(exclude,grep("2017",vintages))
if(length(exclude)>0)  vintages=vintages[-exclude] 
vintages=sort(vintages)
for (i in 0:(length(vintages))) {   
  par(mfrow=c(1,1))   
  if(i==0) vindx=which(pme.df$vintsum&pme.df$Portfolio==portfolio)   
  if (i>0) vindx=which(pme.df$vint==vintages[i])
  if (i>0)  if(vintages[i]=="RCLCO") vindx=which(pme.df$consultant=="RCLCO")  
  if (i==0) titletext=paste(portfolio,"Comparison of all vintages")   
  if(i>0) titletext=rownames(pme.df)[lastinvec(vindx)] 
  if(i>0) if(vintages[i]=="RE Mgr Summary") titletext="Real Estate Manager Summary"
  if(length(vindx)==2) vindx=vindx[1]   
  max.y=max(pme.df$`Fund IRR`[vindx])   
  min.y=min(pme.df$`Fund IRR`[vindx])
  #if (titletext=="Real Estate Manager Summary") min.y=-25  
  yinc=(max(.05,(max.y-min.y)))/40   
  max.y=-.005+(.1*(floor(max.y*10)))   
  min.x=min(pme.df$`Fund TVPI`[vindx]) 
  pme.R2k=cut(pme.df$`Russell 2K PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)
  if(portfolio=="RE") pme.R2k=cut(pme.df$`ODCE PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE) 
  scatplot=ggplot(pme.df[vindx,],aes(x=`Fund TVPI`,y=`Fund IRR`))+
    geom_text(label=pme.df$name[vindx],size=1.5,colour=pme.R2k)+
    xlab("TVPI")+
    ylab("IRR")+
    annotate("text",y=max.y,x=min.x,label="PME is > 1.2",size=1.5,colour=pmepal[5],hjust=0)+
    annotate("text",y=max.y-yinc,x=min.x,label="PME is 1.05 to 1.2",size=1.5,colour=pmepal[4],hjust=0)+
    annotate("text",y=max.y-(yinc*2),x=min.x,label="PME is .95 to 1.05",size=1.5,colour=pmepal[3],hjust=0)+
    annotate("text",y=max.y-(yinc*3),x=min.x,label="PME is .8 to .95",size=1.5,colour=pmepal[2],hjust=0)+
    annotate("text",y=max.y-(yinc*4),x=min.x,label="PME is < .8",size=1.5,colour=pmepal[1],hjust=0)+
    ggtitle(titletext)
  print(scatplot)
}
scat.cats=cats$catshort[cats$Portfolio==portfolio] 
scat.cats.long=cats$catlong[cats$Portfolio==portfolio] 
for (i in 0:length(scat.cats)) {   
  par(mfrow=c(1,1))   
  if(i==0) vindx=which(pme.df$catsum&pme.df$Portfolio==portfolio)   
  if(i>0) vindx=which(pme.df$cat==scat.cats[i])   
  if(i==0) titletext=paste(portfolio,"Comparison of Strategies")   
  if(i>0) titletext=scat.cats.long[i]   
  if(length(vindx)==2) vindx=vindx[1]   
  max.y=max(pme.df$`Fund IRR`[vindx])   
  min.x=min(pme.df$`Fund TVPI`[vindx])   
  min.y=min(pme.df$`Fund IRR`[vindx])   
  yinc=(max(.05,(max.y-min.y)))/40   
  max.y=-.005+(.1*(floor(max.y*10)))   
  pme.R2k=cut(pme.df$`Russell 2K PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)   
  if(portfolio=="RE") pme.R2k=cut(pme.df$`ODCE PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)   
  scatplot=ggplot(pme.df[vindx,],aes(x=`Fund TVPI`,y=`Fund IRR`))+
    geom_text(label=pme.df$name[vindx],size=1.5,colour=pme.R2k)+
    xlab("TVPI")+
    ylab("IRR")+
    annotate("text",y=max.y-yinc,x=min.x,label="PME is > 1.2",size=1.5,colour=pmepal[5],hjust=0)+
    annotate("text",y=max.y-(yinc*2),x=min.x,label="PME is 1.05 to 1.2",size=1.5,colour=pmepal[4],hjust=0)+
    annotate("text",y=max.y-(yinc*3),x=min.x,label="PME is .95 to 1.05",size=1.5,colour=pmepal[3],hjust=0)+
    annotate("text",y=max.y-(yinc*4),x=min.x,label="PME is .8 to .95",size=1.5,colour=pmepal[2],hjust=0)+
    annotate("text",y=max.y-(yinc*5),x=min.x,label="PME is < .8",size=1.5,colour=pmepal[1],hjust=0)+
    ggtitle(titletext)
  print(scatplot)
}

# Real Estate Table of Performance

## @knitr retab
# Chunk Setup <<retab,echo=FALSE, results='asis'>>=
pme.df=subset(pme.hold,pme.hold$Portfolio==portfolio)
pme.sub=pme.df[,c("name","ODCE PME","Fund TVPI","Fund IRR","ODCE Dollar Matched IRR")] 
pme.sub$outp=pme.sub[,4]-pme.sub[,5] 
pme.sub[,2:6]=round(pme.sub[,2:6],2) 
colnames(pme.sub)=c('Fund','ODCE PME','TVPI','IRR','ODCE IRR ','Outperformance') 
rownames(pme.sub)=NULL 
pme.tab=xtable(pme.sub,caption='Real Estate performance compared to ODCE')
print(pme.tab,tabular.environment='longtable',floating=FALSE)

#FARMLAND AND INFRASTRUCTURE

# Farmland Categories
## @knitr farmcats
#Chunk Setup <<farmcats,echo=FALSE, results='asis'>>=
portfolio='FARM'
cats.sub=subset(cats,cats$Portfolio==portfolio)[,1:3]
cat.table=xtable(cats.sub,caption='Farmland category abbreviations')
print(cat.table,tabular.environment='longtable',floating=FALSE)

# Farmland Fund Names Table

## @knitr farmabbr
# Chunk Setup <<farmabbr,echo=FALSE,results='asis'>>=
funds.sub=subset(fundinfo,fundinfo$Portfolio==portfolio)
funds.sub=cbind(as.character(funds.sub$Short),as.character(funds.sub$Csname))
colnames(funds.sub)=c('Abbreviation','Long Name')
funds.table=xtable(funds.sub,caption='Farmland fund name abbreviations')
print(funds.table,tabular.environment='longtable',floating=FALSE)

# Farmland Charts

## @knitr farmcharts
# Chunk Setup <<farmcharts,echo=FALSE, out.height="8in", out.width="6in">>=
pme.df=subset(pme.hold,pme.hold$Portfolio==portfolio)
vintages=unique(pme.df$vint) 
vintages=vintages[!is.na(vintages)] 
pmepal=(brewer.pal(n=10,name="Spectral"))[c(1,3,8,9,10)] 
#drop clear channel -- small outlier 
clrchnind=which(pme.df$name=="ClrChn") 
if (length(clrchnind)==1) pme.df=pme.df[-clrchnind,] 
#remove NAs 
naind=which((is.na(pme.df$`Fund IRR`))|(is.na(pme.df$`Fund TVPI`))) 
if (length(naind)>0) pme.df=pme.df[-naind,] 
palette(pmepal) 
#scatterplots by vintage 
#get rid 2017 
exclude=NULL 
exclude=c(exclude,grep("2017",vintages))
if(length(exclude)>0)  vintages=vintages[-exclude] 
vintages=sort(vintages)
scat.cats=cats$catshort[cats$Portfolio==portfolio] 
scat.cats.long=cats$catlong[cats$Portfolio==portfolio] 
for (i in 1:length(scat.cats)) {   
  par(mfrow=c(1,1))   
  if(i==0) vindx=which(pme.df$catsum)   
  if(i>0) vindx=which(pme.df$cat==scat.cats[i])   
  if(i==0) titletext=paste(portfolio,"Comparison of Strategies")   
  if(i>0) titletext=scat.cats.long[i]   
  if(length(vindx)==2) vindx=vindx[1]   
  max.y=max(pme.df$`Fund IRR`[vindx])   
  min.x=min(pme.df$`Fund TVPI`[vindx])   
  min.y=min(pme.df$`Fund IRR`[vindx])   
  yinc=(max(.05,(max.y-min.y)))/40   
  max.y=-.005+(.1*(floor(max.y*10)))   
  pme.R2k=cut(pme.df$`Russell 2K PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)   
  if(portfolio=="RE") pme.R2k=cut(pme.df$`ODCE PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)
  if(portfolio=="POPP") pme.R2k=cut(pme.df$`Fixed 8 PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE) 
  if(portfolio=="PD") pme.R2k=cut(pme.df$`Lev Loan 250 PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE) 
  if(portfolio=="FARM") pme.R2k=cut(pme.df$`CPIxFE+350 PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)
  scatplot=ggplot(pme.df[vindx,],aes(x=`Fund TVPI`,y=`Fund IRR`))+
    geom_text(label=pme.df$name[vindx],size=1.5,colour=pme.R2k)+
    xlab("TVPI")+
    ylab("IRR")+
    annotate("text",y=max.y,x=min.x,label="PME is > 1.2",size=1.5,colour=pmepal[5],hjust=0)+
    annotate("text",y=max.y-yinc,x=min.x,label="PME is 1.05 to 1.2",size=1.5,colour=pmepal[4],hjust=0)+
    annotate("text",y=max.y-(yinc*2),x=min.x,label="PME is .95 to 1.05",size=1.5,colour=pmepal[3],hjust=0)+
    annotate("text",y=max.y-(yinc*3),x=min.x,label="PME is .8 to .95",size=1.5,colour=pmepal[2],hjust=0)+
    annotate("text",y=max.y-(yinc*4),x=min.x,label="PME is < .8",size=1.5,colour=pmepal[1],hjust=0)+
    ggtitle(titletext)
  print(scatplot)
}


# Farmland Performance Numbers

## @knitr farmtab
#Chunk Setup <<farmtab,echo=FALSE, results='asis'>>=
pme.sub=pme.df[,c("name","CPIxFE+350 PME","Fund TVPI","Fund IRR","CPIxFE+350 Dollar Matched IRR")] 
pme.sub$outp=pme.sub[,4]-pme.sub[,5] 
pme.sub[,2:6]=round(pme.sub[,2:6],2) 
colnames(pme.sub)=c('Fund','CPIxFE+350 PME','TVPI','IRR','CPIxFE IRR+350','Outperformance') 
rownames(pme.sub)=NULL 
pme.tab=xtable(pme.sub,caption='Farmland performance compared to CPIxFE+350bp')
print(pme.tab,tabular.environment='longtable',floating=FALSE)


#PRIVATE OPPORTUNISTIC

# Private Opportunistic Categories

## @knitr oppcats
# Chunk Setup <<oppcats,echo=FALSE, results='asis'>>=
portfolio='POPP'
cats.sub=subset(cats,cats$Portfolio==portfolio)[,1:3]
cat.table=xtable(cats.sub,caption='Private opportunistic category abbreviations')
print(cat.table,tabular.environment='longtable',floating=FALSE)


# POPP Table

## @knitr oppabbr
# Chunk Setup <<oppabbr,echo=FALSE,results='asis'>>=
funds.sub=subset(fundinfo,fundinfo$Portfolio==portfolio)
funds.sub=cbind(as.character(funds.sub$Short),as.character(funds.sub$Csname))
colnames(funds.sub)=c('Abbreviation','Long Name')
funds.table=xtable(funds.sub,caption='Private opportunistic fund name abbreviations')
print(funds.table,tabular.environment='longtable',floating=FALSE)

# POPP Chart

## @knitr oppcharts
# Chunk Setup <<oppcharts,echo=FALSE, out.height="8in", out.width="6in">>=
pme.df=subset(pme.hold,pme.hold$Portfolio==portfolio)
vintages=unique(pme.df$vint) 
vintages=vintages[!is.na(vintages)] 
pmepal=(brewer.pal(n=10,name="Spectral"))[c(1,3,8,9,10)] 
#drop clear channel -- small outlier 
clrchnind=which(pme.df$name=="ClrChn") 
if (length(clrchnind)==1) pme.df=pme.df[-clrchnind,] 
#remove NAs 
naind=which((is.na(pme.df$`Fund IRR`))|(is.na(pme.df$`Fund TVPI`))) 
if (length(naind)>0) pme.df=pme.df[-naind,] 
palette(pmepal) 
#scatterplots by vintage 
#get rid 2017 
exclude=NULL 
exclude=c(exclude,grep("2017",vintages))
if(length(exclude)>0)  vintages=vintages[-exclude] 
vintages=sort(vintages)
scat.cats=cats$catshort[cats$Portfolio==portfolio] 
scat.cats.long=cats$catlong[cats$Portfolio==portfolio] 
for (i in 1:length(scat.cats)) {   
  par(mfrow=c(1,1))   
  if(i==0) vindx=which(pme.df$catsum)   
  if(i>0) vindx=which(pme.df$cat==scat.cats[i])   
  if(i==0) titletext=paste(portfolio,"Comparison of Strategies")   
  if(i>0) titletext=scat.cats.long[i]   
  if(length(vindx)==2) vindx=vindx[1]   
  max.y=max(pme.df$`Fund IRR`[vindx])   
  min.x=min(pme.df$`Fund TVPI`[vindx])   
  min.y=min(pme.df$`Fund IRR`[vindx])   
  yinc=(max(.05,(max.y-min.y)))/40   
  max.y=-.005+(.1*(floor(max.y*10)))   
  pme.R2k=cut(pme.df$`Russell 2K PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)   
  if(portfolio=="RE") pme.R2k=cut(pme.df$`ODCE PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)
  if(portfolio=="POPP") pme.R2k=cut(pme.df$`Fixed 8 PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE) 
  scatplot=ggplot(pme.df[vindx,],aes(x=`Fund TVPI`,y=`Fund IRR`))+
    geom_text(label=pme.df$name[vindx],size=1.5,colour=pme.R2k)+
    xlab("TVPI")+
    ylab("IRR")+
    annotate("text",y=max.y,x=min.x,label="PME is > 1.2",size=1.5,colour=pmepal[5],hjust=0)+
    annotate("text",y=max.y-yinc,x=min.x,label="PME is 1.05 to 1.2",size=1.5,colour=pmepal[4],hjust=0)+
    annotate("text",y=max.y-(yinc*2),x=min.x,label="PME is .95 to 1.05",size=1.5,colour=pmepal[3],hjust=0)+
    annotate("text",y=max.y-(yinc*3),x=min.x,label="PME is .8 to .95",size=1.5,colour=pmepal[2],hjust=0)+
    annotate("text",y=max.y-(yinc*4),x=min.x,label="PME is < .8",size=1.5,colour=pmepal[1],hjust=0)+
    ggtitle(titletext)
  print(scatplot)
}


#POPP Performance Table

## @knitr popptab
#chunk setup <<popptab,echo=FALSE, results='asis'>>=
pme.sub=pme.df[,c("name","Fixed 8 PME","Fund TVPI","Fund IRR","Fixed 8 Dollar Matched IRR")] 
pme.sub$outp=pme.sub[,4]-pme.sub[,5] 
pme.sub[,2:6]=round(pme.sub[,2:6],2) 
colnames(pme.sub)=c('Fund','Fixed 8 PME','TVPI','IRR','Fixed 8 IRR','Outperformance') 
rownames(pme.sub)=NULL 
pme.tab=xtable(pme.sub,caption='Private opportunistic performance compared to Fixed 8\\%')
print(pme.tab,tabular.environment='longtable',floating=FALSE)


# PRIVATE DEBT 

# Private Debt Categories

## @knitr pdcats
#chunk setup <<pdcats,echo=FALSE, results='asis'>>=
portfolio='PD'
cats.sub=subset(cats,cats$Portfolio==portfolio)[,1:3]
cat.table=xtable(cats.sub,caption='Private debt category abbreviations')
print(cat.table,tabular.environment='longtable',floating=FALSE)

# Private Debt Abbreviations

## @knitr pdabbr
#Chunk Setup <<pdabbr,echo=FALSE,results='asis'>>=
funds.sub=subset(fundinfo,fundinfo$Portfolio==portfolio)
funds.sub=cbind(as.character(funds.sub$Short),as.character(funds.sub$Csname))
colnames(funds.sub)=c('Abbreviation','Long Name')
funds.table=xtable(funds.sub,caption='Private debt fund name abbreviations')
print(funds.table,tabular.environment='longtable',floating=FALSE)

# Private Debt Charts

## @knitr pdcharts
# Chunk Setup <<pdcharts,echo=FALSE, out.height="8in", out.width="6in">>=
pme.df=subset(pme.hold,pme.hold$Portfolio==portfolio)
vintages=unique(pme.df$vint) 
vintages=vintages[!is.na(vintages)] 
pmepal=(brewer.pal(n=10,name="Spectral"))[c(1,3,8,9,10)] 
#drop clear channel -- small outlier 
clrchnind=which(pme.df$name=="ClrChn") 
if (length(clrchnind)==1) pme.df=pme.df[-clrchnind,] 
#remove NAs 
naind=which((is.na(pme.df$`Fund IRR`))|(is.na(pme.df$`Fund TVPI`))) 
if (length(naind)>0) pme.df=pme.df[-naind,] 
palette(pmepal) 
#scatterplots by vintage 
#get rid 2017 
exclude=NULL
exclude=c(exclude,grep("2017",vintages))
if(length(exclude)>0)  vintages=vintages[-exclude] 
vintages=sort(vintages)
scat.cats=cats$catshort[cats$Portfolio==portfolio] 
scat.cats.long=cats$catlong[cats$Portfolio==portfolio] 
for (i in 1:length(scat.cats)) {   
  par(mfrow=c(1,1))   
  if(i==0) vindx=which(pme.df$catsum)   
  if(i>0) vindx=which(pme.df$cat==scat.cats[i])   
  if(i==0) titletext=paste(portfolio,"Comparison of Strategies")   
  if(i>0) titletext=scat.cats.long[i]   
  if(length(vindx)==2) vindx=vindx[1]   
  max.y=max(pme.df$`Fund IRR`[vindx])   
  min.x=min(pme.df$`Fund TVPI`[vindx])   
  min.y=min(pme.df$`Fund IRR`[vindx])   
  yinc=(max(.05,(max.y-min.y)))/40   
  max.y=-.005+(.1*(floor(max.y*10)))   
  pme.R2k=cut(pme.df$`Russell 2K PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)   
  if(portfolio=="RE") pme.R2k=cut(pme.df$`ODCE PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)
  if(portfolio=="POPP") pme.R2k=cut(pme.df$`Fixed 8 PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE) 
  if(portfolio=="PD") pme.R2k=cut(pme.df$`Lev Loan+250 PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE) 
  scatplot=ggplot(pme.df[vindx,],aes(x=`Fund TVPI`,y=`Fund IRR`))+
    geom_text(label=pme.df$name[vindx],size=1.5,colour=pme.R2k)+
    xlab("TVPI")+
    ylab("IRR")+
    annotate("text",y=max.y,x=min.x,label="PME is > 1.2",size=1.5,colour=pmepal[5],hjust=0)+
    annotate("text",y=max.y-yinc,x=min.x,label="PME is 1.05 to 1.2",size=1.5,colour=pmepal[4],hjust=0)+
    annotate("text",y=max.y-(yinc*2),x=min.x,label="PME is .95 to 1.05",size=1.5,colour=pmepal[3],hjust=0)+
    annotate("text",y=max.y-(yinc*3),x=min.x,label="PME is .8 to .95",size=1.5,colour=pmepal[2],hjust=0)+
    annotate("text",y=max.y-(yinc*4),x=min.x,label="PME is < .8",size=1.5,colour=pmepal[1],hjust=0)+
    ggtitle(titletext)
  print(scatplot)
}

# Private Debt Performance Table

## @knitr pdtab
#chunk setup<<pdtab,echo=FALSE, results='asis'>>=
pme.sub=pme.df[,c("name","Lev Loan+250 PME","Fund TVPI","Fund IRR","Lev Loan+250 Dollar Matched IRR")] 
pme.sub$outp=pme.sub[,4]-pme.sub[,5] 
pme.sub[,2:6]=round(pme.sub[,2:6],2) 
colnames(pme.sub)=c('Fund','Lev Loan+250 PME','TVPI','IRR','Lev Loan+250 Dollar Matched IRR','Outperformance') 
rownames(pme.sub)=NULL 
pme.tab=xtable(pme.sub,caption='Private debt performance compared to Levered Loans+250bp')
print(pme.tab,tabular.environment='longtable',floating=FALSE) 


# OPPORTUNISTIC FIXED INCOME

# OPP Categories

## @knitr oppficats
#chunk setup <<oppficats,echo=FALSE, results='asis'>>=
portfolio='OPP'
cats.sub=subset(cats,cats$Portfolio==portfolio)[,1:3]
cat.table=xtable(cats.sub,caption='Opportunistic Fixed Income category abbreviations')
print(cat.table,tabular.environment='longtable',floating=FALSE)


#OPP Fund Abbreviations

## @knitr
# chunk setup <<oppfiabbr,echo=FALSE,results='asis'>>=
funds.sub=subset(fundinfo,fundinfo$Portfolio==portfolio)
funds.sub=cbind(as.character(funds.sub$Short),as.character(funds.sub$Csname))
colnames(funds.sub)=c('Abbreviation','Long Name')
funds.table=xtable(funds.sub,caption='Private debt fund name abbreviations')
print(funds.table,tabular.environment='longtable',floating=FALSE)

# OPP Charts

## @knitr oppficharts
# chunk setup <<oppficharts,echo=FALSE, out.height="8in", out.width="6in">>=
pme.df=subset(pme.hold,pme.hold$Portfolio=="OPP")
vintages=unique(pme.df$vint) 
vintages=vintages[!is.na(vintages)] 
pmepal=(brewer.pal(n=10,name="Spectral"))[c(1,3,8,9,10)] 
#drop clear channel -- small outlier 
clrchnind=which(pme.df$name=="ClrChn") 
if (length(clrchnind)==1) pme.df=pme.df[-clrchnind,] 
#remove NAs 
naind=which((is.na(pme.df$`Fund IRR`))|(is.na(pme.df$`Fund TVPI`))) 
if (length(naind)>0) pme.df=pme.df[-naind,] 
palette(pmepal) 
#scatterplots by vintage 
#get rid 2017 
exclude=NULL
exclude=c(exclude,grep("2017",vintages))
if(length(exclude)>0)  vintages=vintages[-exclude] 
vintages=sort(vintages)
scat.cats=cats$catshort[cats$Portfolio=="OPP"] 
scat.cats.long=cats$catlong[cats$Portfolio=="OPP"] 
for (i in 1:length(scat.cats)) {   
  par(mfrow=c(1,1))   
  if(i==0) vindx=which(pme.df$catsum)   
  if(i>0) vindx=which(pme.df$cat==scat.cats[i])   
  if(i==0) titletext=paste(portfolio,"Comparison of Strategies")   
  if(i>0) titletext=scat.cats.long[i]   
  if(length(vindx)==2) vindx=vindx[1]   
  max.y=max(pme.df$`Fund IRR`[vindx])   
  min.x=min(pme.df$`Fund TVPI`[vindx])   
  min.y=min(pme.df$`Fund IRR`[vindx])   
  yinc=(max(.05,(max.y-min.y)))/40   
  max.y=-.005+(.1*(floor(max.y*10)))   
  pme.R2k=cut(pme.df$`Russell 2K PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)   
  if(portfolio=="RE") pme.R2k=cut(pme.df$`ODCE PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE)
  if(portfolio=="POPP"|portfolio=="OPP") pme.R2k=cut(pme.df$`Fixed 8 PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE) 
  if(portfolio=="PD") pme.R2k=cut(pme.df$`Lev Loan+250 PME`[vindx],c(0,.8,.95,1.05,1.2,100),labels=FALSE) 
  scatplot=ggplot(pme.df[vindx,],aes(x=`Fund TVPI`,y=`Fund IRR`))+
    geom_text(label=pme.df$name[vindx],size=1.5,colour=pme.R2k)+
    xlab("TVPI")+
    ylab("IRR")+
    annotate("text",y=max.y,x=min.x,label="PME is > 1.2",size=1.5,colour=pmepal[5],hjust=0)+
    annotate("text",y=max.y-yinc,x=min.x,label="PME is 1.05 to 1.2",size=1.5,colour=pmepal[4],hjust=0)+
    annotate("text",y=max.y-(yinc*2),x=min.x,label="PME is .95 to 1.05",size=1.5,colour=pmepal[3],hjust=0)+
    annotate("text",y=max.y-(yinc*3),x=min.x,label="PME is .8 to .95",size=1.5,colour=pmepal[2],hjust=0)+
    annotate("text",y=max.y-(yinc*4),x=min.x,label="PME is < .8",size=1.5,colour=pmepal[1],hjust=0)+
    ggtitle(titletext)
  print(scatplot)
}


#Opportunistic Table

## @knitr oppfitab
#Chuck Start <<oppfitab,echo=FALSE, results='asis'>>=
pme.sub=pme.df[,c("name","Fixed 8 PME","Fund TVPI","Fund IRR","Fixed 8 Dollar Matched IRR")] 
pme.sub$outp=pme.sub[,4]-pme.sub[,5] 
pme.sub[,2:6]=round(pme.sub[,2:6],2) 
colnames(pme.sub)=c('Fund','Absolute 8 PME','TVPI','IRR','Absolute 8','Outperformance') 
rownames(pme.sub)=NULL 
pme.tab=xtable(pme.sub,caption='Opportunistic Fixed Income performance compared to Absolute 8')
print(pme.tab,tabular.environment='longtable',floating=FALSE)

# end

  


