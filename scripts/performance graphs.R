library(zoo)
library(lubridate)
setwd("P:/IMD/Karl/R projects/private investment performance")
load('pmedata.rdata')
source('../basic financial.r',echo=FALSE)
#
# Private Equity
#
onefile=FALSE
cstwr.all=read.csv('twrs.csv')
cstwr.all$Twrdate=as.Date(cstwr.all$Twrdate,format="%m/%d/%Y")
cstwr.all=cstwr.all[which(cstwr.all$Twrdate==valdate),]
more.values=read.csv('portfolionav.csv')
more.values$Date=as.Date(more.values$Date,format="%m/%d/%Y")
burgiss=read.csv('burgiss.csv')
burgiss.z=zoo(burgiss$Value,as.Date(burgiss$Date,format="%m/%d/%Y"))
firstburgiss=time(burgiss.z)[1]
burgiss.z=as.numeric(na.approx(as.ts(burgiss.z)))
burgiss.z=zooreg(burgiss.z,start=firstburgiss)
if (onefile) pdf(file='performancegraphs.pdf',width=11,height=8) else
                 pdf(file='pegraphs.pdf',width=11,height=8)
par(mfrow=c(2,2))
portfolio='Total PE'
peind=which(names(y.cf)=='Total PE')
indind=which(names(bench.lst)=='^RUT')
indx=bench.lst[[indind]]
pecf=y.cf[[peind]]
pev=c(y.hv[[peind]],y.v[[peind]]) 
pev.more=more.values[which(more.values$Portfolio=='PE'),]
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
for (i in 1:length(dates)) {   
  #extract the cash flow through the end date   
  cfi=pecf[time(pecf)<=dates[i]]   
  #add the value to the cash flow   
  cfi=mergesum.z(cfi,pev[i])   
  #extract the index values for the same dates   
  r2ki=indx[time(cfi)]   
  #call pestats to calculate performance   
  #arguments are the cash flow as a zoo object and the index values for the same dates   
  ansi=pestats(cfi,r2ki)   
  peirr[i]=ansi$irr   
  r2kirr[i]=ansi$ind.irr
  r2kwealth[i]=pev[i]-ansi$pme.wealthdiff
} 
#convert the irrs to zoo objects with the same dates as the values 
peirr=zoo(peirr,dates) 
r2kirr=zoo(r2kirr,dates) 
r2kwealth=zoo(r2kwealth,dates)
plot(peirr*100,col='blue',
     main='Private Equity IRRs compared to Russell 2000\nInception through indicated date',
     ylim=100*range(c(as.numeric(peirr),as.numeric(r2kirr))),xlim=range(time(pecf)),
     ylab='IRR',xlab='')
lines(r2kirr*100,col='red') 
legend('topleft',fill=c('blue','red'),legend=c('Private Equity','Russell 2000'),cex=.7) 
abline(h=c(-20,-10,0,10,20),lty=3)
indx.t=indx[time(indx)>=min(time(pecf))&time(indx)<=max(time(pecf))]
plot(indx.t/as.numeric(indx.t[1]),col='blue',ylab='Growth of a $',xlab='',main='Russell 2000')
abline(h=1,lty=3)
ylim=range(c(range(cumsum(-pecf)),range(pev)),range(r2kwealth))/1000000
ylim[1]=min(0,ylim[1])
plot(cumsum(-pecf)/1000000,col='blue',ylim=ylim,
     main='Private Equity Cumulative Net Capital Contributed\nCompared to Value',
     xlab='',ylab='$ Millions')
#points(r2kwealth/1000000,col='green',pch=3)
points(pev/1000000,col='red',pch=20)
#legend('topleft',fill=c('blue','red','green'),legend=c('Cumulative Net Cash Flow','Net Asset Value','Value R2K'),cex=.7)
legend('topleft',fill=c('blue','red'),legend=c('Cumulative Net Cash Flow','Net Asset Value'),cex=.7)
pecf.qtrdraw=aggregate(pecf[pecf<0],as.yearqtr,sum)/1000000
pecf.qtrdist=aggregate(pecf[pecf>0],as.yearqtr,sum)/1000000
pecf.qtrnet=aggregate(pecf,as.yearqtr,sum)/1000000
ylim=range(c(range(pecf.qtrdraw),range(pecf.qtrdist)))
plot(pecf.qtrnet,ylim=ylim,col='green',ylab='$ millions',xlab='',
     main='Private Equity\nQuarterly Draws and Distributions')
lines(pecf.qtrdraw,col='red')
lines(pecf.qtrdist,col='blue')
abline(h=0,lty=3)
legend('topleft',fill=c('blue','green','red'),legend=c('Distribution','Net',"Draw"),cex=.7)
if(!onefile) dev.off()
#now calculate the one quarter, one year, three year and inception irr's
if(!onefile) pdf(file='pereturns.pdf',width=11,height=8)
# par(mfrow=c(2,2))
# wdiff=(pev-r2kwealth)/1000000
# plot(wdiff,col='blue',type='l',
#      main='Wealth Difference from investing in PE\ncompared to benchmark'
#      ,xlab='',ylab='$ Millions')
# abline(h=lastinvec(wdiff),lty='dotted')
# wdiff=100*(pev-r2kwealth)/pev
# plot(wdiff,col='blue',type='l',
#      main="Wealth Difference from investing in PE\ncompared to benchmark"
#      ,xlab='',ylab='Percent')
# abline(h=lastinvec(wdiff),lty='dotted')
par(mfrow=c(1,2))
pe.returns=matrix(NA,nrow=3,ncol=5)
rownames(pe.returns)=c("Private Equity","Russell 2000","Burgiss")
colnames(pe.returns)=c("One Quarter","One Year","Three Years","Five Years","Inception")
end.date=time(tail(pev,1))
onequarter.date=as.Date(as.yearqtr(end.date))-1
oneyear.date=end.date-years(1)
threeyear.date=end.date-years(3)
fiveyear.date=end.date-years(5)
if(length(pev[onequarter.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>onequarter.date]
  cfi=mergesum.z(cfi,-pev[onequarter.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,1]=ansi$irr
  pe.returns[2,1]=ansi$ind.irr
  r2ki=burgiss.z[time(cfi)]
  ansi=pestats(cfi,r2ki)
  pe.returns[3,1]=ansi$ind.irr
}
if(length(pev[oneyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>oneyear.date]
  cfi=mergesum.z(cfi,-pev[oneyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,2]=ansi$irr
  pe.returns[2,2]=ansi$ind.irr
  r2ki=burgiss.z[time(cfi)]
  ansi=pestats(cfi,r2ki)
  pe.returns[3,2]=ansi$ind.irr
}
if(length(pev[threeyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>threeyear.date]
  cfi=mergesum.z(cfi,-pev[threeyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,3]=ansi$irr
  pe.returns[2,3]=ansi$ind.irr
  r2ki=burgiss.z[time(cfi)]
  ansi=pestats(cfi,r2ki)
  pe.returns[3,3]=ansi$ind.irr
}
if(length(pev[fiveyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>fiveyear.date]
  cfi=mergesum.z(cfi,-pev[fiveyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,4]=ansi$irr
  pe.returns[2,4]=ansi$ind.irr
  r2ki=burgiss.z[time(cfi)]
  ansi=pestats(cfi,r2ki)
  pe.returns[3,4]=ansi$ind.irr
}
cfi=pecf[time(pecf)<=end.date]
cfi=mergesum.z(cfi,pev[end.date])
r2ki=indx[time(cfi)]
ansi=pestats(cfi,r2ki)   
pe.returns[1,5]=ansi$irr
pe.returns[2,5]=ansi$ind.irr
r2ki=burgiss.z[time(cfi)]
ansi=pestats(cfi,r2ki)
pe.returns[3,5]=ansi$ind.irr
pe.returns=round(100*pe.returns,2)
pe.twrind=which(cstwr.all$Asset=="PE")
pe.twrind=c(pe.twrind,pe.twrind+c(1,2))
cstwr=as.matrix(cstwr.all[pe.twrind,c(2,3,4,5,6)])
rangevec=as.vector(rbind(pe.returns,cstwr))
ylim=range(rangevec[!is.na(rangevec)])
if(ylim[1]>0) ylim[1]=0
color=gray(c(.5,.7,.9))
bp=barplot(pe.returns,beside=TRUE,ylab="Dollar Matched IRR",
       main=paste0("Private Equity IRRs\n",end.date),ylim=ylim,
       col=color,legend.text=TRUE,args.legend=list(cex=.7))
#text(bp,0,as.vector(pe.returns),pos=3,cex=.8,col='blue')
rownames(cstwr)=rownames(pe.returns)
colnames(cstwr)=colnames(pe.returns)
bp=barplot(cstwr,beside=TRUE,ylab="Time Weighted Returns",
       main=paste0("Private Equity TWRs\n",end.date),ylim=ylim,
       col=color,legend.text=TRUE,args.legend=list(cex=.7))
#text(bp,0,as.vector(cstwr),pos=3,cex=.8,col='blue')
p2pirrs=pe.returns
if(!onefile) dev.off()
#
#Real estate
#
if(!onefile) pdf(file='regraphs.pdf',width=11,height=8)
par(mfrow=c(2,2))
portfolio='Total RE'
peind=which(names(y.cf)=='Total RE')
indind=which(names(bench.lst)=='ODCE')
indx=bench.lst[[indind]]
pecf=y.cf[[peind]]
pev=c(y.hv[[peind]],y.v[[peind]]) 
pev.more=more.values[which(more.values$Portfolio=='RE'),]
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
# 
for (i in 1:length(dates)) {   
  #extract the cash flow through the end date   
  cfi=pecf[time(pecf)<=dates[i]]   
  #add the value to the cash flow   
  cfi=mergesum.z(cfi,pev[i])   
  #extract the index values for the same dates   
  r2ki=indx[time(cfi)]   
  #call pestats to calculate performance   
  #arguments are the cash flow as a zoo object and the index values for the same dates   
  ansi=pestats(cfi,r2ki)   
  peirr[i]=ansi$irr   
  r2kirr[i]=ansi$ind.irr 
} 
#convert the irrs to zoo objects with the same dates as the values 
replotstart=as.Date("2004-12-31")
peirr=zoo(peirr,dates) 
#peirrplot=peirr[time(peirr)>=replotstart]
r2kirr=zoo(r2kirr,dates)
#r2kirrplot=r2kirr[time(r2kirr)>=replotstart]
xlim=c(replotstart,max(time(pecf)))
plot(peirr*100,col='blue',
     main='Real Estate IRRs compared to ODCE\nInception through indicated date',
     ylim=100*range(c(as.numeric(peirr),as.numeric(r2kirr))),
     xlim=xlim,
     ylab='IRR',xlab='')
lines(r2kirr*100,col='red') 
legend('topright',fill=c('blue','red'),legend=c('Real Estate','ODCE'),cex=.7) 
abline(h=c(-20,-10,0,10,20),lty=3)
indx.t=indx[time(indx)>=replotstart&time(indx)<=max(time(pecf))]
plot(indx.t/as.numeric(indx.t[1]),col='blue',ylab='Growth of a $',xlab='',main='ODCE')
abline(h=1,lty=3)
ylim=range(c(range(cumsum(-pecf)),range(pev)))/1000000
plot(cumsum(-pecf)/1000000,col='blue',ylim=ylim,xlim=xlim,
     main='Real Estate Cumulative Net Capital Contributed\nCompared to Value'
     ,xlab='',ylab='$ Millions')
points(pev/1000000,col='red',pch=20)
legend('topleft',fill=c('blue','red'),legend=c('Cumulative Net Cash Flow','Net Asset Value'),cex=.7)
pecf.qtrdraw=aggregate(pecf[pecf<0&time(pecf)>replotstart],as.yearqtr,sum)/1000000
pecf.qtrdist=aggregate(pecf[pecf>0&time(pecf)>replotstart],as.yearqtr,sum)/1000000
pecf.qtrnet=aggregate(pecf[time(pecf)>replotstart],as.yearqtr,sum)/1000000
ylim=range(c(range(pecf.qtrdraw),range(pecf.qtrdist)))
plot(pecf.qtrnet,ylim=ylim,
     col='green',ylab='$ millions',xlab='',
     main='Real Estate\nQuarterly Draws and Distributions')
lines(pecf.qtrdraw,col='red')
lines(pecf.qtrdist,col='blue')
abline(h=0,lty=3)
legend('topleft',fill=c('blue','green','red'),legend=c('Distribution','Net',"Draw"),cex=.7)
if(!onefile) dev.off()
#now calculate the one quarter, one year, three year and inception irr's
if(!onefile) pdf(file='rereturns.pdf',width=11,height=8)
par(mfrow=c(1,2))

pe.returns=matrix(NA,nrow=2,ncol=5)
rownames(pe.returns)=c("Private Real Estate","ODCE Net")
colnames(pe.returns)=c("One Quarter","One Year","Three Years","Five Years","Inception")
end.date=time(tail(pev,1))
onequarter.date=as.Date(as.yearqtr(end.date))-1
oneyear.date=end.date-years(1)
threeyear.date=end.date-years(3)
fiveyear.date=end.date-years(5)
if(length(pev[onequarter.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>onequarter.date]
  cfi=mergesum.z(cfi,-pev[onequarter.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,1]=ansi$irr
  pe.returns[2,1]=ansi$ind.irr
}
if(length(pev[oneyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>oneyear.date]
  cfi=mergesum.z(cfi,-pev[oneyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,2]=ansi$irr
  pe.returns[2,2]=ansi$ind.irr
}
if(length(pev[threeyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>threeyear.date]
  cfi=mergesum.z(cfi,-pev[threeyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,3]=ansi$irr
  pe.returns[2,3]=ansi$ind.irr
}
if(length(pev[fiveyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>fiveyear.date]
  cfi=mergesum.z(cfi,-pev[fiveyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,4]=ansi$irr
  pe.returns[2,4]=ansi$ind.irr
}
cfi=pecf[time(pecf)<=end.date]
cfi=mergesum.z(cfi,pev[end.date])
r2ki=indx[time(cfi)]
ansi=pestats(cfi,r2ki)   
pe.returns[1,5]=ansi$irr
pe.returns[2,5]=ansi$ind.irr
pe.returns=round(100*pe.returns,2)
pe.twrind=which(cstwr.all$Asset=="RE")
pe.twrind=c(pe.twrind,pe.twrind+1)
cstwr=as.matrix(cstwr.all[pe.twrind,c(2,3,4,5,6)])
rangevec=as.vector(rbind(pe.returns,cstwr))
ylim=range(rangevec[!is.na(rangevec)])
if(ylim[1]>0) ylim[1]=0
color=gray(c(.7,.9))
bp=barplot(pe.returns,beside=TRUE,ylab="Dollar Matched IRR",
           main=paste0("Real Estate IRRs\n",end.date),ylim=ylim,
           col=color,legend.text=TRUE,args.legend=list(cex=.7))
text(bp,0,as.vector(pe.returns),pos=3,cex=.8,col='blue')
rownames(cstwr)=rownames(pe.returns)
colnames(cstwr)=colnames(pe.returns)
bp=barplot(cstwr,beside=TRUE,ylab="Time Weighted Returns",
           main=paste0("Real Estate TWRs\n",end.date),ylim=ylim,
           col=color,legend.text=TRUE,args.legend=list(cex=.7))
text(bp,0,as.vector(cstwr),pos=3,cex=.8,col='blue')
p2pirrs=rbind(p2pirrs,pe.returns)
if(!onefile) dev.off()
#
#Private Opportunistic
#
if(!onefile) pdf(file='oppgraphs.pdf',width=11,height=8)
par(mfrow=c(2,2))
portfolio='Total POPP'
peind=which(names(y.cf)==portfolio)
indind=which(names(bench.lst)=='Fixed8')
indx=bench.lst[[indind]]
pecf=y.cf[[peind]]
pev=c(y.hv[[peind]],y.v[[peind]]) 
#extract the dates for which we have values 
dates=time(pev) 
# 
#now calculate the IRR for each end date 
#initiate empty to hold the results 
peirr=vector() 
r2kirr=vector() 
# 
for (i in 1:length(dates)) {   
  #extract the cash flow through the end date   
  cfi=pecf[time(pecf)<=dates[i]]   
  #add the value to the cash flow   
  cfi=mergesum.z(cfi,pev[i])   
  #extract the index values for the same dates   
  r2ki=indx[time(cfi)]   
  #call pestats to calculate performance   
  #arguments are the cash flow as a zoo object and the index values for the same dates   
  ansi=pestats(cfi,r2ki)   
  peirr[i]=ansi$irr   
  r2kirr[i]=ansi$ind.irr 
} 
#convert the irrs to zoo objects with the same dates as the values 
peirr=zoo(peirr,dates) 
r2kirr=zoo(r2kirr,dates) 
plot(peirr*100,col='blue',
     main='Private Opportunistic IRRs compared to Absolute 8%\nInception through indicated date',
     ylim=100*range(c(as.numeric(peirr),as.numeric(r2kirr))),xlim=range(time(pecf)),
     ylab='IRR',xlab='')
lines(r2kirr*100,col='red') 
legend('topleft',fill=c('blue','red'),legend=c('Private Opportunistic','Absolute 8% Return'),cex=.7) 
abline(h=c(-20,-10,0,10,20),lty=3)
indx.t=indx[time(indx)>=min(time(pecf))&time(indx)<=max(time(pecf))]
plot(indx.t/as.numeric(indx.t[1]),col='blue',ylab='Growth of a $',xlab='',main='Absolute 8% Return')
abline(h=1,lty=3)
ylim=range(c(range(cumsum(-pecf)),range(pev)))/1000000
ylim[1]=min(0,ylim[1])
plot(cumsum(-pecf)/1000000,col='blue',ylim=ylim,
     main='Private Opportunistic Cum. Net Capital Contributed\nCompared to Value',
     xlab='',ylab='$ Millions')
points(pev/1000000,col='red',pch=20)
legend('topleft',fill=c('blue','red'),legend=c('Cumulative Net Cash Flow','Net Asset Value'),cex=.7)
pecf.qtrdraw=aggregate(pecf[pecf<0],as.yearqtr,sum)/1000000
pecf.qtrdist=aggregate(pecf[pecf>0],as.yearqtr,sum)/1000000
pecf.qtrnet=aggregate(pecf,as.yearqtr,sum)/1000000
ylim=range(c(range(pecf.qtrdraw),range(pecf.qtrdist)))
plot(pecf.qtrnet,ylim=ylim,col='green',ylab='$ millions',xlab='',
     main='Private Opportunistic\nQuarterly Draws and Distributions')
lines(pecf.qtrdraw,col='red')
lines(pecf.qtrdist,col='blue')
abline(h=0,lty=3)
legend('bottomright',fill=c('blue','green','red'),legend=c('Distribution','Net',"Draw"),cex=.7)
if(!onefile) dev.off()
if(!onefile) pdf(file='oppreturns.pdf',width=11,height=8)
par(mfrow=c(1,2))

pe.returns=matrix(NA,nrow=2,ncol=5)
rownames(pe.returns)=c("Private Opportunistic Equity","Absolute Eight")
colnames(pe.returns)=c("One Quarter","One Year","Three Years","Five Years","Inception")
end.date=time(tail(pev,1))
onequarter.date=as.Date(as.yearqtr(end.date))-1
oneyear.date=end.date-years(1)
threeyear.date=end.date-years(3)
fiveyear.date=end.date-years(5)
if(length(pev[onequarter.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>onequarter.date]
  cfi=mergesum.z(cfi,-pev[onequarter.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,1]=ansi$irr
  pe.returns[2,1]=ansi$ind.irr
}
if(length(pev[oneyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>oneyear.date]
  cfi=mergesum.z(cfi,-pev[oneyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,2]=ansi$irr
  pe.returns[2,2]=ansi$ind.irr
}
if(length(pev[threeyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>threeyear.date]
  cfi=mergesum.z(cfi,-pev[threeyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,3]=ansi$irr
  pe.returns[2,3]=ansi$ind.irr
}
if(length(pev[fiveyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>fiveyear.date]
  cfi=mergesum.z(cfi,-pev[fiveyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,4]=ansi$irr
  pe.returns[2,4]=ansi$ind.irr
}
cfi=pecf[time(pecf)<=end.date]
cfi=mergesum.z(cfi,pev[end.date])
r2ki=indx[time(cfi)]
ansi=pestats(cfi,r2ki)   
pe.returns[1,5]=ansi$irr
pe.returns[2,5]=ansi$ind.irr
pe.returns=round(100*pe.returns,2)
pe.twrind=which(cstwr.all$Asset=="POPP")
pe.twrind=c(pe.twrind,pe.twrind+1)
cstwr=as.matrix(cstwr.all[pe.twrind,c(2,3,4,5,6)])
rangevec=as.vector(rbind(pe.returns,cstwr))
ylim=range(rangevec[!is.na(rangevec)])
if(ylim[1]>0) ylim[1]=0
color=gray(c(.7,.9))
bp=barplot(pe.returns,beside=TRUE,ylab="Dollar Matched IRR",
           main=paste0("Private Opportunistic\n",end.date),ylim=ylim,
           col=color,legend.text=TRUE,args.legend=list(x="topleft",cex=.7))
text(bp,0,as.vector(pe.returns),pos=3,cex=.8,col='blue')
rownames(cstwr)=rownames(pe.returns)
colnames(cstwr)=colnames(pe.returns)
bp=barplot(cstwr,beside=TRUE,ylab="Time Weighted Returns",
           main=paste0("Private Opportunistic TWRs\n",end.date),ylim=ylim,
           col=color,legend.text=TRUE,args.legend=list(cex=.7))
text(bp,0,as.vector(cstwr),pos=3,cex=.8,col='blue')
p2pirrs=rbind(p2pirrs,pe.returns)
if(!onefile) dev.off()
#
# Opportunistic Fixed Income
#
if(!onefile) pdf(file='oppfigraphs.pdf',width=11,height=8)
par(mfrow=c(2,2))
portfolio='Total OPP'
peind=which(names(y.cf)==portfolio)
indind=which(names(bench.lst)=='Fixed8')
indx=bench.lst[[indind]]
pecf=y.cf[[peind]]
pev=c(y.hv[[peind]],y.v[[peind]]) 
#extract the dates for which we have values 
dates=time(pev) 
# 
#now calculate the IRR for each end date 
#initiate empty to hold the results 
peirr=vector() 
r2kirr=vector() 
# 
for (i in 1:length(dates)) {   
  #extract the cash flow through the end date   
  cfi=pecf[time(pecf)<=dates[i]]   
  #add the value to the cash flow   
  cfi=mergesum.z(cfi,pev[i])   
  #extract the index values for the same dates   
  r2ki=indx[time(cfi)]   
  #call pestats to calculate performance   
  #arguments are the cash flow as a zoo object and the index values for the same dates   
  ansi=pestats(cfi,r2ki)   
  peirr[i]=ansi$irr   
  r2kirr[i]=ansi$ind.irr 
} 
#convert the irrs to zoo objects with the same dates as the values 
peirr=zoo(peirr,dates) 
r2kirr=zoo(r2kirr,dates) 
plot(peirr*100,col='blue',
     main='Opportunistic Fixed Income IRRs\ncompared to Absolute 8%',
     ylim=100*range(c(as.numeric(peirr),as.numeric(r2kirr))),
     xlim=range(time(pecf)),
     ylab='Inception to Date IRR',xlab='')
lines(r2kirr*100,col='red') 
legend('bottomright',fill=c('blue','red'),legend=c('Opportunistic Fixed Income','Absolute 8'),cex=.7) 
abline(h=c(-20,-10,0,10,20),lty=3)
indx.t=indx[time(indx)>=min(time(pecf))&time(indx)<=max(time(pecf))]
plot(indx.t/as.numeric(indx.t[1]),col='blue',ylab='Growth of a $',xlab='',main='Absolute 8')
abline(h=1,lty=3)
ylim=range(c(range(cumsum(-pecf)),range(pev)))/1000000
ylim[1]=min(0,ylim[1])
plot(cumsum(-pecf)/1000000,col='blue',ylim=ylim,
     main='Opp. Fixed Income Cum. Net Capital Contributed\nCompared to Value',
     xlab='',ylab='$ Millions')
points(pev/1000000,col='red',pch=20)
legend('topleft',fill=c('blue','red'),legend=c('Cumulative Net Cash Flow','Net Asset Value'),cex=.7)
pecf.qtrdraw=aggregate(pecf[pecf<0],as.yearqtr,sum)/1000000
pecf.qtrdist=aggregate(pecf[pecf>0],as.yearqtr,sum)/1000000
pecf.qtrnet=aggregate(pecf,as.yearqtr,sum)/1000000
ylim=range(c(range(pecf.qtrdraw),range(pecf.qtrdist)))
plot(pecf.qtrnet,ylim=ylim,col='green',ylab='$ millions',xlab='',
     main='Opportunistic Fixed Income\nQuarterly Draws and Distributions')
lines(pecf.qtrdraw,col='red')
lines(pecf.qtrdist,col='blue')
abline(h=0,lty=3)
legend('bottomright',fill=c('blue','green','red'),legend=c('Distribution','Net',"Draw"),cex=.7)
if(!onefile) dev.off()
if(!onefile) pdf(file='oppfireturns.pdf',width=11,height=8)
par(mfrow=c(1,2))

pe.returns=matrix(NA,nrow=2,ncol=5)
rownames(pe.returns)=c("Opportunistic Fixed Income","Absolute Eight")
colnames(pe.returns)=c("One Quarter","One Year","Three Years","Five Years","Inception")
end.date=time(tail(pev,1))
onequarter.date=as.Date(as.yearqtr(end.date))-1
oneyear.date=end.date-years(1)
threeyear.date=end.date-years(3)
fiveyear.date=end.date-years(5)
if(length(pev[onequarter.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>onequarter.date]
  cfi=mergesum.z(cfi,-pev[onequarter.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,1]=ansi$irr
  pe.returns[2,1]=ansi$ind.irr
}
if(length(pev[oneyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>oneyear.date]
  cfi=mergesum.z(cfi,-pev[oneyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,2]=ansi$irr
  pe.returns[2,2]=ansi$ind.irr
}
if(length(pev[threeyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>threeyear.date]
  cfi=mergesum.z(cfi,-pev[threeyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,3]=ansi$irr
  pe.returns[2,3]=ansi$ind.irr
}
if(length(pev[fiveyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>fiveyear.date]
  cfi=mergesum.z(cfi,-pev[fiveyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,4]=ansi$irr
  pe.returns[2,4]=ansi$ind.irr
}
cfi=pecf[time(pecf)<=end.date]
cfi=mergesum.z(cfi,pev[end.date])
r2ki=indx[time(cfi)]
ansi=pestats(cfi,r2ki)   
pe.returns[1,5]=ansi$irr
pe.returns[2,5]=ansi$ind.irr
pe.returns=round(100*pe.returns,2)
pe.twrind=which(cstwr.all$Asset=="OPP")
pe.twrind=c(pe.twrind,pe.twrind+1)
cstwr=as.matrix(cstwr.all[pe.twrind,c(2,3,4,5,6)])
rangevec=as.vector(rbind(pe.returns,cstwr))
ylim=c(0,15)#range(rangevec[!is.na(rangevec)])
if(ylim[1]>0) ylim[1]=0
color=gray(c(.7,.9))
bp=barplot(pe.returns,beside=TRUE,ylab="Dollar Matched IRR",
           main=paste0("Opportunistic Fixed Income IRRs\n",end.date),ylim=ylim,
           col=color,legend.text=TRUE,args.legend=list(x="topleft",cex=.7))
text(bp,0,as.vector(pe.returns),pos=3,cex=.8,col='blue')
rownames(cstwr)=rownames(pe.returns)
colnames(cstwr)=colnames(pe.returns)
bp=barplot(cstwr,beside=TRUE,ylab="Time Weighted Returns",
           main=paste0("Opportunistic Fixed Income TWRs\n",end.date),ylim=ylim,
           col=color,legend.text=TRUE,args.legend=list(x="topleft",cex=.7))
text(bp,0,as.vector(cstwr),pos=3,cex=.8,col='blue')
p2pirrs=rbind(p2pirrs,pe.returns)
if(!onefile) dev.off()
#
# private debt
#
if(!onefile) pdf(file='pdgraphs.pdf',width=11,height=8)
par(mfrow=c(2,2))
portfolio='Total PD'
peind=which(names(y.cf)==portfolio)
indind=which(names(bench.lst)=='LevLoan.250')
indx=bench.lst[[indind]]
pecf=y.cf[[peind]]
pev=c(y.hv[[peind]],y.v[[peind]]) 
#extract the dates for which we have values 
dates=time(pev) 
# 
#now calculate the IRR for each end date 
#initiate empty to hold the results 
peirr=vector() 
r2kirr=vector() 
# 
for (i in 1:length(dates)) {   
  #extract the cash flow through the end date   
  cfi=pecf[time(pecf)<=dates[i]]   
  #add the value to the cash flow   
  cfi=mergesum.z(cfi,pev[i])   
  #extract the index values for the same dates   
  r2ki=indx[time(cfi)]   
  #call pestats to calculate performance   
  #arguments are the cash flow as a zoo object and the index values for the same dates   
  ansi=pestats(cfi,r2ki)   
  peirr[i]=ansi$irr   
  r2kirr[i]=ansi$ind.irr 
} 
#convert the irrs to zoo objects with the same dates as the values 
peirr=zoo(peirr,dates) 
r2kirr=zoo(r2kirr,dates) 
plot(peirr*100,col='blue',
     main='Private Debt IRRs\ncompared to Lev Loan+250',
     ylim=100*range(c(as.numeric(peirr),as.numeric(r2kirr))),xlim=range(time(pecf)),
     ylab='Inception to Date IRR',xlab='')
lines(r2kirr*100,col='red') 
legend('topleft',fill=c('blue','red'),legend=c('Private Debt','Lev. Loan+250'),cex=.7) 
abline(h=c(-20,-10,0,10,20),lty=3)
indx.t=indx[time(indx)>=min(time(pecf))&time(indx)<=max(time(pecf))]
plot(indx.t/as.numeric(indx.t[1]),col='blue',ylab='Growth of a $',xlab='',main='Lev Loan+250')
abline(h=1,lty=3)
ylim=range(c(range(cumsum(-pecf)),range(pev)))/1000000
ylim[1]=min(0,ylim[1])
plot(cumsum(-pecf)/1000000,col='blue',ylim=ylim,
     main='Private Debt Cumulative Net Capital Contributed\nCompared to Value',
     xlab='',ylab='$ Millions')
points(pev/1000000,col='red',pch=20)
legend('topleft',fill=c('blue','red'),legend=c('Cumulative Net Cash Flow','Net Asset Value'),cex=.7)
pecf.qtrdraw=aggregate(pecf[pecf<0],as.yearqtr,sum)/1000000
pecf.qtrdist=aggregate(pecf[pecf>0],as.yearqtr,sum)/1000000
pecf.qtrnet=aggregate(pecf,as.yearqtr,sum)/1000000
ylim=range(c(range(pecf.qtrdraw),range(pecf.qtrdist)))
plot(pecf.qtrnet,ylim=ylim,col='green',ylab='$ millions',xlab='',
     main='Private Debt\nQuarterly Draws and Distributions')
lines(pecf.qtrdraw,col='red')
lines(pecf.qtrdist,col='blue')
abline(h=0,lty=3)
legend('bottomright',fill=c('blue','green','red'),legend=c('Distribution','Net',"Draw"),cex=.7)
if(!onefile) dev.off()
if(!onefile) pdf(file='pdreturns.pdf',width=11,height=8)
par(mfrow=c(1,2))

pe.returns=matrix(NA,nrow=2,ncol=5)
rownames(pe.returns)=c("Private Debt","Lev Loan+250")
colnames(pe.returns)=c("One Quarter","One Year","Three Years","Five Years","Inception")
end.date=time(tail(pev,1))
onequarter.date=as.Date(as.yearqtr(end.date))-1
oneyear.date=end.date-years(1)
threeyear.date=end.date-years(3)
fiveyear.date=end.date-years(5)
if(length(pev[onequarter.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>onequarter.date]
  cfi=mergesum.z(cfi,-pev[onequarter.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,1]=ansi$irr
  pe.returns[2,1]=ansi$ind.irr
}
if(length(pev[oneyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>oneyear.date]
  cfi=mergesum.z(cfi,-pev[oneyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,2]=ansi$irr
  pe.returns[2,2]=ansi$ind.irr
}
if(length(pev[threeyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>threeyear.date]
  cfi=mergesum.z(cfi,-pev[threeyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,3]=ansi$irr
  pe.returns[2,3]=ansi$ind.irr
}
if(length(pev[fiveyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>fiveyear.date]
  cfi=mergesum.z(cfi,-pev[fiveyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,4]=ansi$irr
  pe.returns[2,4]=ansi$ind.irr
}
cfi=pecf[time(pecf)<=end.date]
cfi=mergesum.z(cfi,pev[end.date])
r2ki=indx[time(cfi)]
ansi=pestats(cfi,r2ki)   
pe.returns[1,5]=ansi$irr
pe.returns[2,5]=ansi$ind.irr
pe.returns=round(100*pe.returns,2)
pe.twrind=which(cstwr.all$Asset=="PD")
pe.twrind=c(pe.twrind,pe.twrind+1)
cstwr=as.matrix(cstwr.all[pe.twrind,c(2,3,4,5,6)])
rangevec=as.vector(rbind(pe.returns,cstwr))
ylim=range(rangevec[!is.na(rangevec)])
if(ylim[1]>0) ylim[1]=0
color=gray(c(.7,.9))
bp=barplot(pe.returns,beside=TRUE,ylab="Dollar Matched IRR",
           main=paste0("Private Debt IRRs\n",end.date),ylim=ylim,
           col=color,legend.text=TRUE,args.legend=list(x="topleft",cex=.7))
text(bp,0,as.vector(pe.returns),pos=3,cex=.8,col='blue')
rownames(cstwr)=rownames(pe.returns)
colnames(cstwr)=colnames(pe.returns)
bp=barplot(cstwr,beside=TRUE,ylab="Time Weighted Returns",
           main=paste0("Private Debt TWRs\n",end.date),ylim=ylim,
           col=color,legend.text=TRUE,args.legend=list(x="topleft",cex=.7))
text(bp,0,as.vector(cstwr),pos=3,cex=.8,col='blue')
p2pirrs=rbind(p2pirrs,pe.returns)
dev.off()
##
## end private debt
##
#
#   Farm
#
if(!onefile) pdf(file='farmgraphs.pdf',width=11,height=8)
par(mfrow=c(2,2))
portfolio='Total FARM'
peind=which(names(y.cf)==portfolio)
indind=which(names(bench.lst)=='CPIxFE.350')
indx=bench.lst[[indind]]
pecf=y.cf[[peind]]
if(!is.na(y.hv[[peind]][1])) {
  pev=c(y.hv[[peind]],y.v[[peind]]) 
  } else {
    pev=y.v[[peind]]
  }
#extract the dates for which we have values 
dates=time(pev) 
# 
#now calculate the IRR for each end date 
#initiate empty to hold the results 
peirr=vector() 
r2kirr=vector() 
# 
for (i in 1:length(dates)) {   
  #extract the cash flow through the end date   
  cfi=pecf[time(pecf)<=dates[i]]   
  #add the value to the cash flow   
  cfi=mergesum.z(cfi,pev[i])   
  #extract the index values for the same dates   
  r2ki=indx[time(cfi)]   
  #call pestats to calculate performance   
  #arguments are the cash flow as a zoo object and the index values for the same dates   
  ansi=pestats(cfi,r2ki)   
  peirr[i]=ansi$irr   
  r2kirr[i]=ansi$ind.irr 
} 
#convert the irrs to zoo objects with the same dates as the values 
peirr=zoo(peirr,dates) 
r2kirr=zoo(r2kirr,dates) 
plot(peirr*100,col='blue',
     main='Farm and Infrastucture IRRs compared to CPI+350\nInception through indicated date',
     ylim=100*range(c(as.numeric(peirr),as.numeric(r2kirr))),xlim=range(time(pecf)),
     ylab='IRR',xlab='')
lines(r2kirr*100,col='red') 
legend('topleft',fill=c('blue','red'),legend=c('Farm Infrastructure','Core CPI+350'),cex=.7) 
abline(h=c(-20,-10,0,10,20),lty=3)
indx.t=indx[time(indx)>=min(time(pecf))&time(indx)<=max(time(pecf))]
plot(indx.t/as.numeric(indx.t[1]),col='blue',ylab='Growth of a $',xlab='',main='Core CPI+350')
abline(h=1,lty=3)
ylim=range(c(range(cumsum(-pecf)),range(pev)))/1000000
ylim[1]=min(0,ylim[1])
plot(cumsum(-pecf)/1000000,col='blue',ylim=ylim,
     main='Farm Infra Cum. Net Capital Contributed\nCompared to Value',
     xlab='',ylab='$ Millions')
points(pev/1000000,col='red',pch=20)
legend('bottomleft',fill=c('blue','red'),legend=c('Cumulative Net Cash Flow','Net Asset Value'),cex=.7)
pecf.qtrdraw=aggregate(pecf[pecf<0],as.yearqtr,sum)/1000000
if(length(pecf[pecf>0])>0) {
  pecf.qtrdist=aggregate(pecf[pecf>0],as.yearqtr,sum)/1000000
} else {
  pecf.qtrdist=NULL
}
pecf.qtrnet=aggregate(pecf,as.yearqtr,sum)/1000000
ylim=range(c(as.numeric(pecf.qtrdraw),as.numeric(pecf.qtrdist)))
plot(pecf.qtrnet,ylim=ylim,col='green',ylab='$ millions',xlab='',
     main='Farm Infra\nQuarterly Draws and Distributions')
lines(pecf.qtrdraw,col='red')
if(!is.null(pecf.qtrdist)) lines(pecf.qtrdist,col='blue')
abline(h=0,lty=3)
legend('bottomright',fill=c('blue','green','red'),legend=c('Distribution','Net',"Draw"),cex=.7)
if(!onefile) dev.off()
if(!onefile) pdf(file='farmreturns.pdf',width=11,height=8)
par(mfrow=c(1,2))

pe.returns=matrix(NA,nrow=2,ncol=5)
rownames(pe.returns)=c("Farmland","Core CPI+350")
colnames(pe.returns)=c("One Quarter","One Year","Three Years","Five Years","Inception")
end.date=time(tail(pev,1))
onequarter.date=as.Date(as.yearqtr(end.date))-1
oneyear.date=end.date-years(1)
threeyear.date=end.date-years(3)
fiveyear.date=end.date-years(5)
if(length(pev[onequarter.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>onequarter.date]
  cfi=mergesum.z(cfi,-pev[onequarter.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,1]=ansi$irr
  pe.returns[2,1]=ansi$ind.irr
}
if(length(pev[oneyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>oneyear.date]
  cfi=mergesum.z(cfi,-pev[oneyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,2]=ansi$irr
  pe.returns[2,2]=ansi$ind.irr
}
if(length(pev[threeyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>threeyear.date]
  cfi=mergesum.z(cfi,-pev[threeyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,3]=ansi$irr
  pe.returns[2,3]=ansi$ind.irr
}
if(length(pev[fiveyear.date])==1) {
  cfi=pecf[time(pecf)<=end.date&time(pecf)>fiveyear.date]
  cfi=mergesum.z(cfi,-pev[fiveyear.date],pev[end.date])
  r2ki=indx[time(cfi)]
  ansi=pestats(cfi,r2ki)   
  pe.returns[1,4]=ansi$irr
  pe.returns[2,4]=ansi$ind.irr
}
cfi=pecf[time(pecf)<=end.date]
cfi=mergesum.z(cfi,pev[end.date])
r2ki=indx[time(cfi)]
ansi=pestats(cfi,r2ki)   
pe.returns[1,5]=ansi$irr
pe.returns[2,5]=ansi$ind.irr
pe.returns=round(100*pe.returns,2)
pe.twrind=which(cstwr.all$Asset=="FARM")
pe.twrind=c(pe.twrind,pe.twrind+1)
cstwr=as.matrix(cstwr.all[pe.twrind,c(2,3,4,5,6)])
rangevec=as.vector(rbind(pe.returns,cstwr))
ylim=range(rangevec[!is.na(rangevec)])
if(ylim[1]>0) ylim[1]=0
color=gray(c(.7,.9))
bp=barplot(pe.returns,beside=TRUE,ylab="Dollar Matched IRR",
           main=paste0("FARM INFRA\n",end.date),ylim=ylim,
           col=color,legend.text=TRUE,args.legend=list(x="topleft",cex=.7))
text(bp,0,as.vector(pe.returns),pos=3,cex=.8,col='blue')
rownames(cstwr)=rownames(pe.returns)
colnames(cstwr)=colnames(pe.returns)
bp=barplot(cstwr,beside=TRUE,ylab="Time Weighted Returns",
           main=paste0("FARM INFRA TWRs\n",end.date),ylim=ylim,
           col=color,legend.text=TRUE,args.legend=list(cex=.7))
text(bp,0,as.vector(cstwr),pos=3,cex=.8,col='blue')
p2pirrs=rbind(p2pirrs,pe.returns)
if(!onefile) dev.off()
#
#  you didn't buy the farm - yay - time to celebrate!

write.csv(p2pirrs,file='p2pirrs.csv')
