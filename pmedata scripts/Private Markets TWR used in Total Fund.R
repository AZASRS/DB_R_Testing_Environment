require(zoo)
require(tidyr)
require(lubridate)
### Load custom functions and Priv Markets latest Data
load('P:/IMD/Karl/R Projects/private investment performance/pmedata.rdata')
source('P:/IMD/Karl/R Projects/basic financial.r',echo=FALSE)


#calculate TWR Monthly TWR

#create date windows
beg.nav = as.Date("2017-09-30") #previous valdate in Private Markets
valudate = as.Date("2017-12-31") #latest valdate in Private Markets
m1 = as.Date("2018-01-31") #First month of current quarter
m2 = as.Date("2018-02-28") #Second month of current quarter
m3 = as.Date("2018-03-31") #Last month of current quarter
#create the total privates values for beginning/ ending + cash flows
total.cats <- c("Total PE", "Total RE", "Total POPP", "Total OPP", "Total PD","Total FARM")
beg.navs <- list()
end.navs <- list()
m3.cfs <- list()
m2.cfs <- list()
m1.cfs <- list()
monthly.twr <- list()
for(i in total.cats) {
  #get cash flows and navs
  cv <- y.v[[i]]
  cf <- y.cf[[i]]*-1
  hv <- y.hv[[i]]
  val <- mergesum.z(cv, hv)
  #calculate month 1
  cf.q <- subset(cf, index(cf) > beg.nav & index(cf) <= valudate)
  beg.adj.nav <- val[beg.nav] + sum(cf.q)
  cf.m1 <- subset(cf, index(cf) > valudate & index(cf) <= m1)
  mv.adj.end <- val[beg.nav] + sum(cf.q) + sum(cf.m1)
  #m1.twr <- coredata(mv.adj.end)/coredata(beg.adj.nav) -1
  #calculate the weighted cash flow during the month for twr
  n = as.numeric(m1 - valudate)
  cf.adj.v = vector()
  for(j in 1:length(cf.m1)) {
    cf.m = cf.m1[j]*(as.numeric(index(cf.m1[j]) - valudate)/n)
    cf.adj.v[j] = coredata(cf.m)
  }
  cf.adj.m1 <- sum(cf.adj.v)
  m1.twr <- (coredata(mv.adj.end) - coredata(beg.adj.nav) - sum(cf.m1))/
    (coredata(beg.adj.nav) + cf.adj.m1)
  if(is.na(m1.twr)=="TRUE") {m1.twr = coredata(mv.adj.end)/coredata(beg.adj.nav)-1}
  #calculate month 2
  cf.m2 <- subset(cf, index(cf) > m1 & index(cf) <= m2)
  beg.nav.m2 <- mv.adj.end
  mv.end.m2 <- beg.nav.m2 + sum(cf.m2)
  #calculate the weighted cash flow during the month for twr
  n = as.numeric(m2 - m1)
  cf.adj.v.m2 = vector()
  for(j in 1:length(cf.m2)) {
    cf.m.a = cf.m2[j]*(as.numeric(index(cf.m2[j]) - m1)/n)
    cf.adj.v.m2[j] = coredata(cf.m.a)
  }
  cf.adj.m2 <- sum(cf.adj.v.m2)
  m2.twr <- (coredata(mv.end.m2) - coredata(beg.nav.m2) - sum(cf.m2))/
    (coredata(beg.nav.m2) + cf.adj.m2)
  if(is.na(m2.twr)=="TRUE") {m2.twr = coredata(mv.end.m2)/coredata(beg.nav.m2)-1}
  ## KW: Note - ending value s/b at Valudate - 3/31/17 NAV's plus C/F for April, May and June  
  #calculate month 3
  beg.nav.m3 <- val[beg.nav] + sum(subset(cf, index(cf) > beg.nav & index(cf) <= m2))
  cf.cq <- subset(cf, index(cf) > valudate & index(cf) <= m3)
  mv.end.m3 <- val[valudate] + sum(cf.cq)
  #calculate the weighted cash flow during the month for twr
  cf.m3 <- subset(cf, index(cf) > m2 & index(cf) <= m3)
  n = as.numeric(m3 - m2)
  cf.adj.v.m3 = vector()
  for(j in 1:length(cf.m3)) {
    cf.m.b = cf.m3[j]*(as.numeric(index(cf.m3[j]) - m2)/n)
    cf.adj.v.m3[j] = coredata(cf.m.b)
  }
  cf.adj.m3 <- sum(cf.adj.v.m3)
  m3.twr <- (coredata(mv.end.m3) - coredata(beg.nav.m3) - sum(cf.m3))/
    (coredata(beg.nav.m3) + cf.adj.m3)
  if(is.na(m3.twr)=='TRUE') {m3.twr <- coredata(mv.end.m3)/coredata(beg.nav.m3)-1}
  monthly.twr[[i]] <- data.frame("Portfolio" = i, "Month 1" = m1.twr, "Month 2" = m2.twr,
                            "Month 3"= m3.twr)
  beg.navs[[i]] <- data.frame("Portfolio"=i, "Month 1" = beg.adj.nav, "Month 2" = beg.nav.m2,
                              "Month 3"= beg.nav.m3, row.names = NULL)
  end.navs[[i]] <- data.frame("Portfolio"=i, "Month 1" = mv.adj.end, "Month 2" = mv.end.m2,
                              "Month 3"= mv.end.m3, row.names = NULL)
  m3.cfs[[i]] <- cf.adj.m3
  m2.cfs[[i]] <- cf.adj.m2
  m1.cfs[[i]] <- cf.adj.m1
}

twrs = do.call(rbind, monthly.twr)
twrs$`Qtr Return` = ((1+twrs$Month.1)*(1+twrs$Month.2)*(1+twrs$Month.3))-1
twrs$`Adj CFs` = do.call(rbind, m3.cfs)
twrs$`Adj CFs` = na.fill(twrs$`Adj CFs`, 0)
beg.nav.df = do.call(rbind, end.navs)
twrs$`Beg NAV` = beg.nav.df$Month.3
twrs$`Adj NAV` = c(twrs$`Beg NAV`+ twrs$`Adj CFs`)
end.nav = do.call(rbind, end.navs)
twrs$`End NAV` = end.nav$Month.3
cfs.m1 = do.call(rbind, m1.cfs)
cfs.m2 = do.call(rbind, m2.cfs)
cfs.m3 = do.call(rbind, m3.cfs)
cfs.adj = cbind(cfs.m1, cfs.m2, cfs.m3)
colnames(cfs.adj) = c("Month 1", "Month 2","Month 3")
rownames(twrs) = NULL
write.csv(twrs, 'twr privates upd.csv')
write.csv(beg.nav.df, 'beg.nav.month.csv')
write.csv(end.nav, 'end.nav.month.csv')
write.csv(cfs.adj, 'cfs by month.csv')


##### Get Public Data from rNEPC for relavent dates

##### do not use code below #######
pub.dat = read.csv("SS Daily Perf 2Q17.csv", stringsAsFactors = FALSE, fill = FALSE)
pub.dat$Effective.Date = as.Date(pub.dat$Effective.Date, format = "%m/%d/%Y")
#pub.dat$`Return plus 1` = pub.dat$Fund/100+1
#pub.dat$`Return plus 1` = na.fill(pub.dat$`Return plus 1`, 1)
ret.dat = subset(pub.dat, select = c("Effective.Date","Name","Daily.Return"))
#bm.dat = subset(pub.dat, select = c("Effective.Date","Name","Benchmark"))
#bm.dat$Benchmark = bm.dat$Benchmark/100+1
#bm.dat$Benchmark = na.fill(bm.dat$Benchmark, 1)
comps <- unique(pub.dat$Name)
ret.vec.m1 = vector()
ret.vec.m2 = vector()
ret.vec.m3 = vector()
for(c in comps){
  ret=subset(ret.dat, ret.dat$Name == c)
  ret.m1= subset(ret, ret$Effective.Date > valudate & ret$Effective.Date <= m1)
  m1.ret = cumprod(ret.m1$`Return plus 1`)[nrow(ret.m1)]-1
  ret.vec.m1[[c]] = m1.ret
  ret.m2= subset(ret, ret$Effective.Date > m1 & ret$Effective.Date <= m2)
  m2.ret = cumprod(ret.m2$`Return plus 1`)[nrow(ret.m2)]-1
  ret.vec.m2[[c]] = m2.ret
  ret.m3= subset(ret, ret$Effective.Date > m2 & ret$Effective.Date <= m3)
  m3.ret = cumprod(ret.m3$`Return plus 1`)[nrow(ret.m3)]-1
  ret.vec.m3[[c]] = m3.ret
}
returns.df = data.frame(cbind(ret.vec.m1, ret.vec.m2, ret.vec.m3))
colnames(returns.df) = c("April", "May", "June")
returns.df$Qtr = (1+returns.df[,1])*(1+returns.df[,2])*(1+returns.df[,3])-1
bm.vec.m1 = vector()
bm.vec.m2 = vector()
bm.vec.m3 = vector()
for(c in comps){
  ret=subset(bm.dat, ret.dat$Name == c)
  ret.m1= subset(ret, ret$Effective.Date > valudate & ret$Effective.Date <= m1)
  m1.ret = cumprod(ret.m1$Benchmark)[nrow(ret.m1)]-1
  bm.vec.m1[[c]] = m1.ret
  ret.m2= subset(ret, ret$Effective.Date > m1 & ret$Effective.Date <= m2)
  m2.ret = cumprod(ret.m2$Benchmark)[nrow(ret.m2)]-1
  bm.vec.m2[[c]] = m2.ret
  ret.m3= subset(ret, ret$Effective.Date > m2 & ret$Effective.Date <= m3)
  m3.ret = cumprod(ret.m3$Benchmark)[nrow(ret.m3)]-1
  bm.vec.m3[[c]] = m3.ret
}
bm.df = as.data.frame(cbind(bm.vec.m1, bm.vec.m2, bm.vec.m3))
colnames(bm.df) = c("April", "May", "June")
bm.df$Qtr = (1+bm.df[,1])*(1+bm.df[,2])*(1+bm.df[,3])-1




#calculate public returns for June
pub.dat = read.csv("dailypref.csv", stringsAsFactors = FALSE)
pub.dat$Effective.Date = as.Date(pub.dat$Effective.Date, format = "%m/%d/%Y")
pub.dat$X1.Day = pub.dat$X1.Day/100+1
ret = subset(pub.dat, pub.dat$Name=="BRIDGEWATER ASSOCIATES GLBL TAA" ,select = c("Effective.Date","Name","X1.Day"))
ret=(cumprod(ret$X1.Day)-1)[nrow(ret)]
#pub.ret = aggregate(pub.dat$X1.Day ~ pub.dat$Name, prod)
cfs = subset(pub.dat, select = c("Name",'Effective.Date',"Net.Cash.Flow"))
start = as.Date("2017-05-31")
n = as.numeric(as.Date("2017-06-30")- start)
cfs$`Adj CF` = cfs$Net.Cash.Flow*((as.numeric(cfs$Effective.Date - start)/n))
adj.cfs = aggregate(cfs$`Adj CF`~cfs$Name,cfs, sum)
adj.cfs = adj.cfs[which(adj.cfs$`cfs$Name`=="PUBLIC MARKET"),]
beg.mvs = subset(pub.dat, pub.dat$Effective.Date=="2017-06-01", select = c("Name", "Beginning.Mkt.Value", "Effective.Date"))
beg.mvs = beg.mvs[which(beg.mvs$Name=="PUBLIC MARKET"),]

#calculate public returns for 2Q17
pub.dat = read.csv("daily performance 2Q17.csv", stringsAsFactors = FALSE)
pub.dat$Effective.Date = as.Date(pub.dat$Effective.Date, format = "%m/%d/%Y")
pub.dat$`Return plus 1` = pub.dat$Fund/100+1
pub.dat$`Return plus 1` = na.fill(pub.dat$`Return plus 1`, 1)
ret.dat = subset(pub.dat, select = c("Effective.Date","Name","Return plus 1"))
bm.dat = subset(pub.dat, select = c("Effective.Date","Name","Benchmark"))
bm.dat$Benchmark = bm.dat$Benchmark/100+1
bm.dat$Benchmark = na.fill(bm.dat$Benchmark, 1)
comps <- unique(pub.dat$Name)
ret.vec.m1 = vector()
ret.vec.m2 = vector()
ret.vec.m3 = vector()
for(c in comps){
  ret=subset(ret.dat, ret.dat$Name == c)
  ret.m1= subset(ret, ret$Effective.Date > valudate & ret$Effective.Date <= m1)
  m1.ret = cumprod(ret.m1$`Return plus 1`)[nrow(ret.m1)]-1
  ret.vec.m1[[c]] = m1.ret
  ret.m2= subset(ret, ret$Effective.Date > m1 & ret$Effective.Date <= m2)
  m2.ret = cumprod(ret.m2$`Return plus 1`)[nrow(ret.m2)]-1
  ret.vec.m2[[c]] = m2.ret
  ret.m3= subset(ret, ret$Effective.Date > m2 & ret$Effective.Date <= m3)
  m3.ret = cumprod(ret.m3$`Return plus 1`)[nrow(ret.m3)]-1
  ret.vec.m3[[c]] = m3.ret
}
returns.df = data.frame(cbind(ret.vec.m1, ret.vec.m2, ret.vec.m3))
colnames(returns.df) = c("April", "May", "June")
returns.df$Qtr = (1+returns.df[,1])*(1+returns.df[,2])*(1+returns.df[,3])-1
bm.vec.m1 = vector()
bm.vec.m2 = vector()
bm.vec.m3 = vector()
for(c in comps){
  ret=subset(bm.dat, ret.dat$Name == c)
  ret.m1= subset(ret, ret$Effective.Date > valudate & ret$Effective.Date <= m1)
  m1.ret = cumprod(ret.m1$Benchmark)[nrow(ret.m1)]-1
  bm.vec.m1[[c]] = m1.ret
  ret.m2= subset(ret, ret$Effective.Date > m1 & ret$Effective.Date <= m2)
  m2.ret = cumprod(ret.m2$Benchmark)[nrow(ret.m2)]-1
  bm.vec.m2[[c]] = m2.ret
  ret.m3= subset(ret, ret$Effective.Date > m2 & ret$Effective.Date <= m3)
  m3.ret = cumprod(ret.m3$Benchmark)[nrow(ret.m3)]-1
  bm.vec.m3[[c]] = m3.ret
}
bm.df = as.data.frame(cbind(bm.vec.m1, bm.vec.m2, bm.vec.m3))
colnames(bm.df) = c("April", "May", "June")
bm.df$Qtr = (1+bm.df[,1])*(1+bm.df[,2])*(1+bm.df[,3])-1

#calculate internal vs external performance
port.dat = read.csv("Internal and External mgrs.csv", stringsAsFactors = FALSE)
port.dat$Effective.Date = as.Date(port.dat$Effective.Date, format = "%m/%d/%Y")
port.dat = subset(port.dat, !port.dat$Cat == "")
new.dat = na.omit(port.dat)
new.dat$Adj.CF = new.dat$Beginning.Mkt.Value + new.dat$Net.Cash.Flow
cats = unique(new.dat$Cat)
comp.end = list()
for(c in cats){}
ports = subset(new.dat, new.dat$Cat == c, select = c("Effective.Date","Name",
                                                     "Benchmark.Name","Adj.CF",
                                                     "Fund","Benchmark"))
ret = spread(ports[,c("Effective.Date","Name","Fund")], Name, Fund)
ret.z = zoo(ret[,-1], ret[,1])
ret.z = na.fill(ret.z, 0)
mv = spread(ports[,c("Effective.Date","Name","Adj.CF")], Name, Adj.CF)
mv.z = zoo(mv[,-1], mv[,1])
mv.z = na.fill(mv.z, 0)
total = rowSums(mv.z)
weights = mv.z/total
bm = spread(ports[,c("Effective.Date","Benchmark.Name","Benchmark")], Benchmark.Name, Benchmark)
bm.z = zoo(bm[,-1], bm[,1])
bm.z = na.fill(bm.z, 0)
comp.ret = rowSums(weights * ret.z)
comp.ret = prod(1+comp.ret/100)-1
comp.bm = rowSums(weights * bm.z)
comp.bm = prod(1+comp.bm/100)-1
fy.ret = rbind(comp.ret, comp.bm, (comp.ret - comp.bm))

