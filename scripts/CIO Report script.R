## @knitr setup
require(zoo)
require(xts)
require(tidyr)
require(ggplot2)
require(lubridate)
require(reshape2)
require(scales)
require(PerformanceAnalytics)
require(knitr)
require(gridExtra)
require(kableExtra)
require(xtable)
require(dplyr)
require(tidyr)
require(magrittr)
require(ggpubr)
require(grid)

source('P:/IMD/Karl/R projects/basic financial.r',echo=FALSE)
# source('P:/IMD/Karl/R projects/cbind.na.r',echo=FALSE)
# source('P:/IMD/Karl/R projects/rbind.na.r',echo=FALSE)
tomillions = function(x) x/1000000
label.d = "as of 3/31/18"
label.pms = "as Reported 3/31/18"

#Because of data issues, fix the start AND end date for the attribution analysis
end.date <- as.Date("2018-03-31")

#Ten year window
start.date <- as.Date("2008-03-31")
xts.start <- as.Date("2008-04-30")
xts.range <- paste0(xts.start, "/", end.date)

## @knitr tf.dva
#nepc.dat is the raw NEPC data file
nepc.dat <- read.csv("P:/IMD/Karl/R Projects/Public Performance/Data/CSVs/rNEPC.csv", stringsAsFactors = FALSE) %>%
  mutate(ID = ShortName) %>%
  mutate(Date = as.Date(AsOf, format = "%m/%d/%Y")) %>%
  mutate(NetReturn = NetReturn/100) %>%
  filter(between(Date, start.date, end.date) & Period == "Monthly")

#NEPC data does not include the EMD bench so it must be added from a seperate file
emd.bench <- read.csv("P:/IMD/Karl/R projects/Public Performance/Data/CSVs/EMD.bench.csv", stringsAsFactors = FALSE) %>%
  .[,1:2] %>%
  set_colnames(c("Date","EMD.BM")) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(EMD.BM = EMD.BM/100)

#nepc.map is the mapping file for nepc.dat
nepc.map <- read.csv("P:/IMD/Karl/R Projects/Public Performance/Data/Mapping/NEPC.map.csv", stringsAsFactors = FALSE)

#SAA Interim Policy Weights with MA adjustment
saa <- read.csv("P:/IMD/JohnD/Attribution/Broad SAA BM Adj 2.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y"))

#Total Fund Loop
comp.id <- nepc.map[which(nepc.map$Attribution == "Y"), 'ID']
#Total Fund Benchmark - needed for loop
b.tf <- subset(nepc.dat, nepc.dat$ID == "RAY0001", select = c('Date','NetReturn'))
b.tf.xts <- xts(b.tf[ ,-1], b.tf[ ,1])
b.tf.xts <- b.tf.xts[xts.range,]
b.tf.xts <- b.tf.xts[!duplicated(index(b.tf.xts)), ]

#create empty lists
r.list = list()   
b.list = list()

mv.list = list()
dva.list = list()
sel.list = list()

for(c in comp.id) {
  #get return, name, & inception date
  name <- nepc.map[which(nepc.map$ID == c), 'Short.Name']
  r <- subset(nepc.dat, nepc.dat$ID == c, select = c('Date','NetReturn'))
  r.xts <- xts(r[,-1], r[,1])
  r.xts <- r.xts[!is.na(r.xts),]
  r.xts <- r.xts[!duplicated(index(r.xts)), ]
  r.xts <- r.xts[xts.range,]
  i.d <- as.character(time(r.xts)[1])
  
  #benchmarks for each asset class and convert Private Equity & Private Debt to quarterly
  b.id <- nepc.map[which(nepc.map$ID == c), 'BM.ID']
  b <- subset(nepc.dat, nepc.dat$ID == b.id, select = c('Date','NetReturn'))
  b.xts <- xts(b[ ,-1], b[ ,1])
  b.xts <- b.xts[paste0(i.d,"/"), ]
  b.xts = b.xts[xts.range, ]
  
  #get market values
  mv <- subset(nepc.dat, nepc.dat$ID == c, select = c('Date','MthEndMV'))
  mv.xts <- xts(mv[,-1], mv[,1])
  mv.xts <- mv.xts[!duplicated(index(mv.xts)), ]
  mv.xts <- lag.xts(mv.xts, 1)
  mv.xts <- mv.xts[xts.range,]
  
  #build cash composite
  if(c == "RAY0118") {
    #get data for non-assetized cash
    r.na <- subset(nepc.dat, nepc.dat$ID == "ASRSCASH", select = c('Date','NetReturn'))
    r.na <- xts(r.na[,-1], r.na[,1])
    r.na <- r.na[!is.na(r.na),]
    r.na <- r.na[!duplicated(index(r.na)), ]
    r.na <- r.na[xts.range,]
    b.na.id <- nepc.map[which(nepc.map$ID == "ASRSCASH"), 'BM.ID']
    b.na <- subset(nepc.dat, nepc.dat$ID == b.na.id, select = c('Date','NetReturn'))
    b.na <- xts(b.na[ ,-1], b.na[ ,1])
    b.na = b.na[xts.range, ]
    mv.na <- subset(nepc.dat, nepc.dat$ID == "ASRSCASH", select = c('Date','MthEndMV'))
    mv.na <- xts(mv.na[,-1], mv.na[,1])
    mv.na <- mv.na[!duplicated(index(mv.na)), ]
    mv.na <- lag.xts(mv.na, 1)
    mv.na <- mv.na[xts.range,]
    #calculate weighted average composite for cash
    comb.mv <- merge(mv.xts, mv.na)
    comb.mv$Total = rowSums(comb.mv, na.rm = TRUE)
    comb.mv = na.fill(comb.mv, 0)
    mv.xts <- comb.mv$Total
    ret <- merge(r.xts, r.na)
    ret = na.fill(ret, 0)
    r.xts <- ret[,1]*(comb.mv[,1]/comb.mv[,3])+ret[,2]*(comb.mv[,2]/comb.mv[,3])
    bm <- merge(b.xts, b.na)
    bm = na.fill(bm, 0)
    b.xts <- bm[,1]*(comb.mv[,1]/comb.mv[,3])+bm[,2]*(comb.mv[,2]/comb.mv[,3])
  }
  r.list[[name]] <- r.xts
  b.list[[name]] <- b.xts
  mv.list[[name]] <- mv.xts
  
  #calculate selection effect
  sel.list[[name]] <- (r.xts - b.xts) * mv.xts
  
  #calculate total DVA
  dva.xts <- (r.xts - b.tf.xts) * mv.xts
  dva.list[[name]] <- dva.xts
}

#Portfolio Returns
r.ac <- do.call(merge, r.list)
r.ac <- na.fill(r.ac, 0)
colnames(r.ac) <- names(r.list)

#Benchmark Returns
b.ac <- do.call(merge, b.list)
b.ac = na.fill(b.ac, 0)
colnames(b.ac) <- names(b.list)

#Market Values
ac.mvs <- do.call(merge, mv.list)
ac.mvs = na.fill(ac.mvs, 0)
colnames(ac.mvs) <- names(mv.list)

#total fund DVA
ac.dva <- do.call(merge, dva.list)
ac.dva <- na.fill(ac.dva, 0)
ac.dva <- apply.quarterly(ac.dva, FUN = colSums)
colnames(ac.dva) <- names(dva.list)

#TF Return and BM
tf.data <- merge(r.ac[,1], b.ac[ ,1], ac.mvs[,1])
colnames(tf.data) <- c("Total Fund","SAA Benchmark", "BegMV")

#Total Fund Multiplier
tf.index <- as.vector(cumprod(1 + apply.quarterly(b.ac[, 1], FUN = Return.cumulative)))
tf.multiplier <- tf.index[length(tf.index)] / tf.index

#calculate active weights versus GTAA adjusted SAA
saa.gtaa <- xts(saa[ ,-1], saa[ ,1])
saa.gtaa <- saa.gtaa[xts.range, ]
saa.gtaa <- cbind("Total Fund"= rowSums(saa.gtaa), saa.gtaa)

#active exposure
aw.percent <- ac.mvs/rowSums(ac.mvs[,-1]) - saa.gtaa
aw.dollar <- as.vector(ac.mvs[,1]) * aw.percent

#Allocation effect
allocation <- aw.dollar * (b.ac - as.vector(b.ac[,1]))
allocation <- apply.quarterly(allocation, FUN = colSums)

#Selection effect
selection <- do.call(merge, sel.list)
selection <- na.fill(selection, 0)
colnames(selection) = names(sel.list)
selection <- apply.quarterly(selection, FUN = colSums)

#Other
other <- ac.dva[,1] - rowSums(allocation[,-1]) - rowSums(selection[,-1])
end.mth <- dim(tf.data)[1]


tf.return.long <- data.frame(
  t(Return.annualized(tf.data[(end.mth-11):end.mth, 1:2])),
  t(Return.annualized(tf.data[(end.mth-35):end.mth, 1:2])),
  t(Return.annualized(tf.data[(end.mth-59):end.mth, 1:2])),
  t(Return.annualized(tf.data[(end.mth-119):end.mth, 1:2]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  mutate(Portfolio = rownames(.)) %>%
  gather(Period, Return, - Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = c("Total Fund", "SAA Benchmark")))

tf.return.plot <- ggplot(tf.return.long, aes(x = Period, y = Return, fill = Portfolio)) +
  geom_bar(stat = "identity", position = "Dodge") + 
  geom_text(aes(label = round(Return*100, 1)), position = position_dodge(width=0.9), vjust=-0.25)+
  xlab("") + scale_y_continuous(name = "Annualized Return", labels = scales::percent)+
  ggtitle("Total Fund and SAA Benchmark",
          subtitle = paste("Trailing Period Returns",label.d)) +
  scale_fill_manual(values = IMD.palette(), name = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"), legend.position = 'bottom',
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))

dva.summary <- merge(ac.dva[, 1], rowSums(allocation[,-1]), rowSums(selection[,-1]), other[,1]) %>%
  set_colnames(c("Total Fund DVA", "Allocation", "Selection", "Other")) %>%
  multiply_by(tf.multiplier) %>%
  tomillions(.)

end.qtr <- dim(dva.summary)[1]

dva.summary.long <- data.frame(
  colSums(coredata(dva.summary[(end.qtr- 3):end.qtr, -1])),
  colSums(coredata(dva.summary[(end.qtr-11):end.qtr, -1])),
  colSums(coredata(dva.summary[(end.qtr-19):end.qtr, -1])),
  colSums(coredata(dva.summary[(end.qtr-39):end.qtr, -1]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  mutate(Effect = rownames(.)) %>%
  gather(Period, DVA, -Effect) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year"))) %>%
  mutate(Effect = factor(Effect, levels = c("Allocation", "Selection", "Other")))

tf.dva.plot <- ggplot(dva.summary.long, aes(x = Period, y = DVA, fill = Effect)) + geom_bar(stat = "identity") +
  ggtitle("Total Fund Dollar Value Add", 
          subtitle = paste("Relative to SAA Benchmark",label.d)) +
  ylab("in Millions") + xlab("") + scale_y_continuous(labels = scales::dollar) +
  scale_fill_manual(values=IMD.palette(), name = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"), legend.position = 'bottom',
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))

grid.arrange(tf.return.plot, tf.dva.plot, ncol = 2)

## @knitr tf.rolling
r.tf.all <- read.csv("P:/IMD/Karl/R Projects/Public Performance/Data/CSVs/rNEPC.csv", stringsAsFactors = FALSE) %>%
  mutate(AsOf = as.Date(AsOf, format = "%m/%d/%Y")) %>%
  mutate(NetReturn = NetReturn/100) %>%
  filter(Period == "Monthly", ShortName == "RAY0032", AsOf <= end.date) %>%
  select(c("AsOf", "NetReturn"))

r.tf.all <- xts(r.tf.all[,-1], r.tf.all[,1])
r.tf.qtr <- apply.quarterly(r.tf.all, FUN = Return.cumulative)

r.1.roll <- rollapply(r.tf.qtr, width = 4, FUN = "Return.annualized", scale = 4)
r.3.roll <- rollapply(r.tf.qtr, width = 12, FUN = "Return.annualized", scale = 4)
r.10.roll <- rollapply(r.tf.qtr, width = 40, FUN = "Return.annualized", scale = 4)
r.20.roll <- rollapply(r.tf.qtr, width = 80, FUN = "Return.annualized", scale = 4)

roll.data <- merge(r.1.roll, r.10.roll, r.20.roll)
colnames(roll.data) <- c("One Year", "Ten Year", "Twenty Year")

roll.plot <- ggplot(data = fortify.zoo(roll.data, melt = TRUE), 
                    aes(x = Index, y = Value, stat = Series, colour = Series)) +
  geom_line(size = 1) +
  xlab("") + scale_y_continuous(name = "Annualized Return", labels = scales::percent) +
  ggtitle("Total Fund Rolling Returns",
          subtitle = paste("Trailing Period Returns", label.d)) +
  scale_color_manual(values = IMD.palette(), name = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"),
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))

print(roll.plot)

## @knitr tf.allocation
end.qtr <- dim(allocation)[1]

allocation.summary <- tomillions(allocation * tf.multiplier)

allocation.long <- data.frame(
  colSums(coredata(allocation.summary[(end.qtr -  3):end.qtr, ])),
  colSums(coredata(allocation.summary[(end.qtr - 11):end.qtr, ])),
  colSums(coredata(allocation.summary[(end.qtr - 19):end.qtr, ])),
  colSums(coredata(allocation.summary[(end.qtr - 39):end.qtr, ]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  rbind(., "Farmland & Infrastructure" = .["Farmland",] + .["Infrastructure",]) %>%
  .[!rownames(.) %in% c("Total Fund", "Farmland", "Infrastructure"),] %>%
  mutate(Portfolio = rownames(.)) %>%
  gather(Period, DVA, -Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = unique(.$Portfolio)))

allocation.plot = ggplot(allocation.long, aes(x = Period, y = DVA, fill = Portfolio)) +
  geom_bar(stat='identity', position=position_dodge()) + 
  xlab("") + scale_y_continuous(name = "in Millions", labels = scales::dollar) +
  ggtitle("Allocation Effect by Asset Class",
          subtitle = paste("Relative to SAA Benchmark", label.d)) +
  scale_fill_manual(values = IMD.palette())+ labs(fill = "") + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"),
        legend.position= "none")

range.1 <- paste0(end.date - years(1) + days(1), "/", end.date)
range.3 <- paste0(end.date - years(3) + days(1), "/", end.date)
range.5 <- paste0(end.date - years(5) + days(1), "/", end.date)
range.10 <- paste0(end.date - years(10) + days(1), "/", end.date)

active.weights <- data.frame(
  colMeans(aw.percent[range.1, -1]),
  colMeans(aw.percent[range.3, -1]),
  colMeans(aw.percent[range.5, -1]),
  colMeans(aw.percent[range.10, -1])
) %>%
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  rbind(., "Farmland & Infrastructure" = .["Farmland",] + .["Infrastructure",]) %>%
  .[!rownames(.) %in% c("Total Fund", "Farmland", "Infrastructure"),] %>%   
  round(digits = 3) %>%
  mutate(Portfolio = rownames(.)) %>%
  gather(Period, DVA, -Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = unique(.$Portfolio)))

aw.plot = ggplot(active.weights, aes(x = Period, y = DVA, fill = Portfolio)) +
  geom_bar(stat='identity', position=position_dodge()) + 
  xlab("") + scale_y_continuous(name = "Active Weight", labels = scales::percent) +
  ggtitle("Asset Class Average Active Weights",
          subtitle = paste("Relative to SAAP",label.d)) +
  scale_fill_manual(values = IMD.palette())+ labs(fill = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"),
        legend.background = element_rect(fill="gray92", size=2.5, linetype="dotted"),
        legend.position="bottom", legend.box.spacing = unit(0, "cm"),
        legend.text = element_text(size = 5), legend.key.size = unit(.25, "cm"))

ggarrange(allocation.plot, aw.plot, nrow = 1, ncol = 2, common.legend = T, legend = "bottom")

## @knitr tf.selection
selection.summary <- tomillions(selection * tf.multiplier)

selection.long <- data.frame(
  colSums(coredata(selection.summary[(end.qtr -  3):end.qtr, ])),
  colSums(coredata(selection.summary[(end.qtr - 11):end.qtr, ])),
  colSums(coredata(selection.summary[(end.qtr - 19):end.qtr, ])),
  colSums(coredata(selection.summary[(end.qtr - 39):end.qtr, ]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  rbind(., "Farmland & Infrastructure" = .["Farmland",] + .["Infrastructure",]) %>%
  .[!rownames(.) %in% c("Total Fund", "Farmland", "Infrastructure"),] %>%
  mutate(Portfolio = rownames(.)) %>%
  gather(Period, DVA, -Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = unique(.$Portfolio)))

#For later Tests:
tf.eq.selection <- filter(selection.long, Portfolio == "Public Equity")
tf.fi.selection <- filter(selection.long, Portfolio == "Public Fixed Income")

selection.plot = ggplot(selection.long, aes(x = Period, y = DVA, fill = Portfolio)) +
  geom_bar(stat='identity', position=position_dodge()) + 
  xlab("") + scale_y_continuous(name = "in Millions", labels = scales::dollar) +
  ggtitle("Selection Effect by Asset Class",
          subtitle = paste("Relative to SAA Benchmark", label.d)) +
  scale_fill_manual(values = IMD.palette())+ labs(fill = "") + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"),
        legend.position = "bottom", legend.box.spacing = unit(0, "cm"),
        legend.text = element_text(size = 8), legend.key.size = unit(.25, "cm"))

print(selection.plot)

## @knitr pub.eq.dva
saa.equity <- read.csv("P:/IMD/JohnD/Attribution/csv/saa.equity.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

equity.id <- c("RAY0003", "RAY0005", "RAY0006", "RAY0007", "RAY0039","RAY0038", "RAY0010", "RAYE9", "RAY00041")

#Total Equity Benchmark
b.eq <- subset(nepc.dat, nepc.dat$ID == "ASRSTEBM", select = c('Date','NetReturn'))
b.eq.xts <- xts(b.eq[ ,-1], b.eq[ ,1])
b.eq.xts <- b.eq.xts[xts.range,]

#create empty lists
r.eq.list = list()   
b.eq.list = list()
mv.eq.list = list()
dva.eq.list = list()
sel.eq.list = list()

for(c in equity.id) {
  #get return, name, & inception date
  name <- nepc.map[which(nepc.map$ID == c), 'Short.Name']
  r <- subset(nepc.dat, nepc.dat$ID == c, select = c('Date','NetReturn'))
  r.xts <- xts(r[,-1], r[,1])
  r.xts <- r.xts[!is.na(r.xts),]
  r.xts <- r.xts[!duplicated(index(r.xts)), ]
  r.xts <- r.xts[xts.range,]
  i.d <- as.character(time(r.xts)[1])
  r.eq.list[[name]] <- r.xts
  
  #benchmarks for each asset class and convert Private Equity & Private Debt to quarterly
  b.id <- nepc.map[which(nepc.map$ID == c), 'BM.ID']
  b <- subset(nepc.dat, nepc.dat$ID == b.id, select = c('Date','NetReturn'))
  b.xts <- xts(b[ ,-1], b[ ,1])
  b.xts <- b.xts[paste0(i.d,"/"), ]
  b.xts = b.xts[xts.range, ]
  b.eq.list[[name]] <- b.xts
  
  #get market values
  mv = subset(nepc.dat, nepc.dat$ID == c, select = c('Date','MthEndMV'))
  mv.xts <- xts(mv[ ,-1], mv[ ,1])
  mv.xts <- lag.xts(mv.xts, 1)
  mv.xts <- mv.xts[!duplicated(index(mv.xts)), ]
  mv.xts <- mv.xts[paste0(i.d,"/"), ]
  mv.xts <- mv.xts[xts.range, ]
  mv.eq.list[[name]] <- mv.xts
  
  #calculate selection effect
  sel.eq.list[[name]] <- (r.xts - b.xts) * mv.xts
  
  #calculate total DVA
  dva.xts <- (r.xts - b.eq.xts) * mv.xts
  dva.eq.list[[name]] <- dva.xts
}

#Portfolio Returns
r.eq <- do.call(merge, r.eq.list)
r.eq <- na.fill(r.eq, 0)
colnames(r.eq) <- names(r.eq.list)

#Benchmark Returns
b.eq <- do.call(merge, b.eq.list)
#b.eq = na.fill(b.eq, 0)
colnames(b.eq) <- names(b.eq.list)
#Test:   identical(b.eq.xts, b.eq.list[[1]])

#Market Values
eq.mvs <- do.call(merge, mv.eq.list)
eq.mvs = na.fill(eq.mvs, 0)
colnames(eq.mvs) <- names(mv.eq.list)

#Equity DVA
eq.dva <- do.call(merge, dva.eq.list)
eq.dva <- na.fill(eq.dva, 0)
eq.dva <- apply.quarterly(eq.dva, FUN = colSums)
colnames(eq.dva) <- names(dva.eq.list)
#Test: sum(do.call(merge, dva.eq.list) - eq.mvs * (r.eq - as.vector(b.eq.xts)), na.rm = T) == 0    

#TF Return and BM
eq.data <- merge(r.eq[,1], b.eq[ ,1], eq.mvs[,1])
colnames(eq.data) <- c("Public Equity","Composite Benchmark", "BegMV")

#calculate active weights versus GTAA adjusted SAA
saa.eq <- xts(saa.equity[ ,-1], saa.equity[ ,1])
saa.eq <- saa.eq[xts.range, -9]
#Test:  sum(rowSums(saa.eq[,-1])) == dim(saa.eq)[1]

#active exposure
aw.eq.percent <- eq.mvs/rowSums(eq.mvs[,-1]) - saa.eq
aw.eq.dollar <- as.vector(eq.mvs[,1]) * aw.eq.percent
#Test:  sum(rowSums(trunc(aw.eq.percent[,-1], digits = 8))) == 0

#Allocation effect
eq.allocation <- aw.eq.dollar * (b.eq - as.vector(b.eq[,1]))
eq.allocation <- na.fill(eq.allocation, 0)
eq.allocation <- apply.quarterly(eq.allocation, FUN = colSums)
#Test:  sum(eq.allocation[,1]) == 0

#Selection effect
eq.selection <- do.call(merge, sel.eq.list)
eq.selection <- na.fill(eq.selection, 0)
eq.selection <- apply.quarterly(eq.selection, FUN = colSums)
colnames(eq.selection) = names(sel.eq.list)
#Test:  identical(eq.dva[,1], eq.selection[,1])

#Other
eq.other = eq.dva[,1] - rowSums(eq.allocation[,-1]) - rowSums(eq.selection[,-1])
end.mth <- dim(eq.data)[1]

eq.return.long <- data.frame(
  t(Return.annualized(eq.data[(end.mth-11):end.mth, 1:2])),
  t(Return.annualized(eq.data[(end.mth-35):end.mth, 1:2])),
  t(Return.annualized(eq.data[(end.mth-59):end.mth, 1:2])),
  t(Return.annualized(eq.data[(end.mth-119):end.mth, 1:2]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  mutate(Portfolio = rownames(.)) %>%
  gather(Period, Return, - Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = c("Public Equity", "Composite Benchmark")))

eq.return.plot <- ggplot(eq.return.long, aes(x = Period, y = Return, fill = Portfolio)) +
  geom_bar(stat = "identity", position = "Dodge") + 
  geom_text(aes(label = round(Return*100, 1)), position = position_dodge(width=0.9), vjust=-0.25)+
  xlab("") + scale_y_continuous(name = "Annualized Return", labels = scales::percent) +
  ggtitle("Public Equity and Composite Benchmark",
          subtitle = paste("Trailing Period Returns", label.d)) +
  scale_fill_manual(values = IMD.palette(), name = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"), legend.position = "bottom",
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))

eq.dva.summary <- merge(eq.dva[, 1], rowSums(eq.allocation[,-1]), rowSums(eq.selection[,-1]), eq.other[,1]) %>%
  set_colnames(c("Public Equity DVA", "Allocation", "Selection", "Other")) %>%
  multiply_by(tf.multiplier) %>%
  tomillions(.)
#Test:   sum(eq.dva.summary[,1]) == sum(selection.summary[,2])

eq.dva.summary.long <- data.frame(
  colSums(coredata(eq.dva.summary[(end.qtr- 3):end.qtr, -1])),
  colSums(coredata(eq.dva.summary[(end.qtr-11):end.qtr, -1])),
  colSums(coredata(eq.dva.summary[(end.qtr-19):end.qtr, -1]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year")) %>%
  mutate(Effect = rownames(.)) %>%
  gather(Period, DVA, -Effect) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year"))) %>%
  mutate(Effect = factor(Effect, levels = c("Allocation", "Selection", "Other")))

eq.dva.plot <- ggplot(eq.dva.summary.long, aes(x = Period, y = DVA, fill = Effect)) +
  geom_bar(stat = "identity") +
  ggtitle("Public Equity Dollar Value Add", 
          subtitle = paste("Relative to Composite Benchmark", label.d)) +
  ylab("in Millions") + xlab("") + scale_y_continuous(labels = scales::dollar) +
  scale_fill_manual(values=IMD.palette()) + 
  guides(fill = guide_legend(title = "Effect:")) +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10), legend.title = element_blank(),
        axis.title.y = element_text(size = 10, face = "italic"), legend.position = 'bottom',
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))

grid.arrange(eq.return.plot, eq.dva.plot, ncol = 2)

## @knitr eq.allocation
end.qtr <- dim(allocation)[1]

eq.allocation.summary <- tomillions(eq.allocation[,-1] * tf.multiplier)

eq.allocation.long <- data.frame(
  colSums(coredata(eq.allocation.summary[(end.qtr -  3):end.qtr, ])),
  colSums(coredata(eq.allocation.summary[(end.qtr - 11):end.qtr, ])),
  colSums(coredata(eq.allocation.summary[(end.qtr - 19):end.qtr, ]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year")) %>%
  mutate(Portfolio = rownames(.)) %>%
  gather(Period, DVA, -Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = unique(.$Portfolio)))

eq.allocation.plot = ggplot(eq.allocation.long, aes(x = Period, y = DVA, fill = Portfolio)) +
  geom_bar(stat='identity', position=position_dodge()) + 
  xlab("") + scale_y_continuous(name = "in Millions", labels = scales::dollar) +
  ggtitle("Public Equity Allocation Effect by Sub Asset Class",
          subtitle = paste("Relative to Composite Benchmark", label.d)) +
  scale_fill_manual(values = IMD.palette())+ labs(fill = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"), legend.position = 'none',
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))

eq.active.weights <- data.frame(
  colMeans(aw.eq.percent[range.1, -1]),
  colMeans(aw.eq.percent[range.3, -1]),
  colMeans(aw.eq.percent[range.5, -1])
) %>%
  set_colnames(c("One Year","Three Year","Five Year")) %>%
  round(digits = 3) %>%
  mutate(Portfolio = rownames(.)) %>%
  gather(Period, DVA, -Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = unique(.$Portfolio)))

eq.aw.plot = ggplot(eq.active.weights, aes(x = Period, y = DVA, fill = Portfolio)) +
  geom_bar(stat='identity', position=position_dodge()) + 
  xlab("") + scale_y_continuous(name = "Active Weight (%)", labels = scales::percent) +
  ggtitle("Public Equity Average Monthly Active Weight",
          subtitle = paste("Relative to Composite Benchmark", label.d)) +
  scale_fill_manual(values = IMD.palette())+ labs(fill = "Asset Class") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"), 
        legend.background = element_rect(fill="gray92", size=3, linetype="dotted"),        
        legend.position="bottom", legend.box.spacing = unit(0, "cm"),
        legend.text = element_text(size = 5), legend.key.size = unit(.25, "cm"))

ggarrange(eq.allocation.plot, eq.aw.plot, nrow = 1, ncol = 2, common.legend = T, legend = "bottom")

## @knitr eq.selection
eq.selection.summary <- tomillions(eq.selection[,-1] * tf.multiplier)

eq.selection.long <- data.frame(
  colSums(coredata(eq.selection.summary[(end.qtr -  3):end.qtr, ])),
  colSums(coredata(eq.selection.summary[(end.qtr - 11):end.qtr, ])),
  colSums(coredata(eq.selection.summary[(end.qtr - 19):end.qtr, ]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year")) %>%
  mutate(Portfolio = c("US Large", "US Mid", "US Small", "Intl Dev LC","Intl Dev SC", "EM", "Risk Factors","Pub Opp")) %>%
  gather(Period, DVA, -Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = unique(.$Portfolio)))

eq.selection.plot = ggplot(eq.selection.long, aes(x = Period, y = DVA, fill = Portfolio)) +
  geom_bar(stat='identity', position=position_dodge()) + 
  xlab("") + scale_y_continuous(name = "in Millions", labels = scales::dollar) +
  ggtitle("Public Equity Selection Effect",
          subtitle = paste("Relative to Composite Benchmark", label.d)) +
  scale_fill_manual(values = IMD.palette())+ labs(fill = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9),
        axis.title.y = element_text(size = 10, face = "italic"),
        legend.title = element_blank(), legend.position = 'bottom')

print(eq.selection.plot)

## @knitr eq.trailing
peq.trailing <- list()
for(i in names(r.eq.list[-1])) {
  if(i == "Risk Factors") next
  ret <- merge(r.eq.list[[i]], b.eq.list[[i]])
  if(dim(ret)[1] <= 12) next
  return.1 <- t(Return.annualized(ret[((dim(ret)[1]-(12-1)):dim(ret)[1]), ], scale = 12))
  if(dim(ret)[1] >=36) {
    return.3 <- t(Return.annualized(ret[((dim(ret)[1]-(36-1)):dim(ret)[1]), ], scale = 12))}
  else {return.3 <- NA}
  if(dim(ret)[1] >=60){
    return.5 <- t(Return.annualized(ret[((dim(ret)[1]-(60-1)):dim(ret)[1]), ], scale = 12))}
  else {return.5 <- NA}
  if(dim(ret)[1] >= 120)  {
    return.10 <- t(Return.annualized(ret[((dim(ret)[1]-(120-1)):dim(ret)[1]), ], scale = 12))}
  else {return.10 <- NA}
  tr.ret <- cbind(return.1, return.3, return.5, return.10)
  desc <- c("One Year", "Three Year", "Five Year", "Ten Year")
  excess = as.data.frame(t(tr.ret))
  Excess = (excess[, 1] - excess[,2])
  tr.ret = data.frame(rbind(tr.ret, Excess))
  colnames(tr.ret) = desc
  tr.ret$Composite = c(i, "Benchmark", "Excess")
  peq.trailing[[i]] = tr.ret
}
all.peq = do.call(rbind, peq.trailing)
rownames(all.peq) = NULL
all.peq = subset(all.peq, select = c("Composite", desc))
all.peq$`One Year` = paste0(round(all.peq$`One Year`*100,2),"%")
all.peq$`Three Year` = paste0(round(all.peq$`Three Year`*100,2),"%")
all.peq$`Five Year` = paste0(round(all.peq$`Five Year`*100,2),"%")
all.peq$`Ten Year` = paste0(round(all.peq$`Ten Year`*100,2),"%")
hlines = c(-1,seq(0,nrow(all.peq), by=(+3)))
print(xtable(all.peq, align = rep("r",6), digits = 2), hline.after = hlines,
      scalebox=.4, include.rownames = FALSE)

## @knitr fi.dva
saa.fixed <- read.csv("P:/IMD/JohnD/Attribution/csv/saa.fixed.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

fixed.id <- c("RAY0011", "RAY0013", "RAY0035", "RAYEMD")

#Total Fixed Income Benchmark
b.fi <- subset(nepc.dat, nepc.dat$ID == "RAYBB6147", select = c('Date','NetReturn'))
b.fi.xts <- xts(b.fi[ ,-1], b.fi[ ,1])
b.fi.xts <- b.fi.xts[xts.range,]

#create empty lists
r.fi.list = list()   
b.fi.list = list()
mv.fi.list = list()
dva.fi.list = list()
sel.fi.list = list()

for(c in fixed.id) {
  #get return, name, & inception date
  name <- nepc.map[which(nepc.map$ID == c), 'Short.Name']
  r <- subset(nepc.dat, nepc.dat$ID == c, select = c('Date','NetReturn'))
  r.xts <- xts(r[,-1], r[,1])
  r.xts <- r.xts[!is.na(r.xts),]
  r.xts <- r.xts[!duplicated(index(r.xts)), ]
  r.xts <- r.xts[xts.range,]
  i.d <- as.character(time(r.xts)[1])
  r.fi.list[[name]] <- r.xts
  
  #benchmarks for each asset class and convert Private Equity & Private Debt to quarterly
  b.id <- nepc.map[which(nepc.map$ID == c), 'BM.ID']
  b <- subset(nepc.dat, nepc.dat$ID == b.id, select = c('Date','NetReturn'))
  if (c == "RAYEMD"){b <- emd.bench}
  b.xts <- xts(b[ ,-1], b[ ,1])
  b.xts <- b.xts[paste0(i.d,"/"), ]
  b.xts = b.xts[xts.range, ]
  b.fi.list[[name]] <- b.xts
  
  #get market values
  mv = subset(nepc.dat, nepc.dat$ID == c, select = c('Date','MthEndMV'))
  mv.xts <- xts(mv[,-1], mv[,1])
  mv.xts <- mv.xts[!duplicated(index(mv.xts)), ]
  mv.xts <- lag.xts(mv.xts, 1)
  mv.xts <- mv.xts[xts.range,]
  mv.fi.list[[name]] <- mv.xts
  
  #calculate selection effect
  sel.fi.list[[name]] <- (r.xts - b.xts) * mv.xts
  
  #calculate total DVA
  dva.xts <- (r.xts - b.fi.xts) * mv.xts
  dva.fi.list[[name]] <- dva.xts
}

#Portfolio Returns
r.fi <- do.call(merge, r.fi.list)
r.fi <- na.fill(r.fi, 0)
colnames(r.fi) <- names(r.fi.list)

#Benchmark Returns
b.fi <- do.call(merge, b.fi.list)
b.fi = na.fill(b.fi, 0)
colnames(b.fi) <- names(b.fi.list)
#Test:   identical(b.fi.xts, b.fi.list[[1]])

#Market Values
fi.mvs <- do.call(merge, mv.fi.list)
fi.mvs = na.fill(fi.mvs, 0)
colnames(fi.mvs) <- names(mv.fi.list)

#Fixed DVA
fi.dva <- do.call(merge, dva.fi.list)
fi.dva <- na.fill(fi.dva, 0)
fi.dva <- apply.quarterly(fi.dva, FUN = colSums)
colnames(fi.dva) <- names(dva.fi.list)
#Test:   sum(do.call(merge, dva.fi.list) - fi.mvs * (r.fi - as.vector(b.fi.xts)), na.rm = T) == 0 
#Test:   sum(selection[,3]/fi.dva[,1]) == dim(fi.dva)[1]

#TF Return and BM
fi.data <- merge(r.fi[,1], b.fi[ ,1], fi.mvs[,1])
colnames(fi.data) <- c("Public Fixed Income","Composite Benchmark", "BegMV")

#calculate active weights versus GTAA adjusted SAA
saa.fi <- xts(saa.fixed[ ,-1], saa.fixed[ ,1])
saa.fi <- saa.fi[xts.range, ]
#Test:  sum(saa.fi[,1] == 1) == dim(saa.fi)[1]

#active exposure
aw.fi.percent <- fi.mvs/rowSums(fi.mvs[,-1]) - saa.fi
aw.fi.dollar <- as.vector(fi.mvs[,1]) * aw.fi.percent
#Test:  sum(rowSums(trunc(aw.fi.percent[,-1], digits = 8))) == 0

#Allocation effect
fi.allocation <- aw.fi.dollar * (b.fi - as.vector(b.fi[,1]))
fi.allocation <- apply.quarterly(fi.allocation, FUN = colSums)
#Test:  sum(fi.allocation[,1]) == 0

#Selection effect
fi.selection <- do.call(merge, sel.fi.list)
fi.selection <- na.fill(fi.selection, 0)
colnames(fi.selection) = names(sel.fi.list)
fi.selection <- apply.quarterly(fi.selection, FUN = colSums)
#Test:  identical(fi.dva[,1], fi.selection[,1])

#Other
fi.other <- fi.dva[,1] - rowSums(fi.allocation[,-1]) - rowSums(fi.selection[,-1])

end.mth <- dim(fi.data)[1]

fi.return.long <- data.frame(
  t(Return.annualized(fi.data[(end.mth-11):end.mth, 1:2])),
  t(Return.annualized(fi.data[(end.mth-35):end.mth, 1:2])),
  t(Return.annualized(fi.data[(end.mth-59):end.mth, 1:2])),
  t(Return.annualized(fi.data[(end.mth-119):end.mth, 1:2]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  mutate(Portfolio = rownames(.)) %>%
  gather(Period, Return, - Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = c("Public Fixed Income", "Composite Benchmark")))

fi.return.plot <- ggplot(fi.return.long, aes(x = Period, y = Return, fill = Portfolio)) +
  geom_bar(stat = "identity", position = "Dodge") + 
  geom_text(aes(label = round(Return*100, 1)), position = position_dodge(width=0.9), vjust=-0.25)+
  xlab("") + scale_y_continuous(name = "Annualized Return", labels = scales::percent) +
  ggtitle("Public Fixed Income and Composite Benchmark",
          subtitle = paste("Trailing Period Returns", label.d)) +
  scale_fill_manual(values = IMD.palette(), name = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"), legend.position = 'bottom',
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))

fi.dva.summary <- merge(fi.dva[, 1], rowSums(fi.allocation[,-1]), rowSums(fi.selection[,-1]), fi.other[,1]) %>%
  set_colnames(c("Public Fixed DVA", "Allocation", "Selection", "Other")) %>%
  multiply_by(tf.multiplier) %>%
  tomillions(.)
#Test:   sum(fi.dva.summary[,1])==sum(selection.summary[,3])

fi.dva.summary.long <- data.frame(
  colSums(coredata(fi.dva.summary[(end.qtr- 3):end.qtr, -1])),
  colSums(coredata(fi.dva.summary[(end.qtr-11):end.qtr, -1])),
  colSums(coredata(fi.dva.summary[(end.qtr-19):end.qtr, -1])),
  colSums(coredata(fi.dva.summary[(end.qtr-39):end.qtr, -1]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  mutate(Effect = rownames(.)) %>%
  gather(Period, DVA, -Effect) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year"))) %>%
  mutate(Effect = factor(Effect, levels = c("Allocation", "Selection", "Other")))

fi.dva.plot <- ggplot(fi.dva.summary.long, aes(x = Period, y = DVA, fill = Effect)) +
  geom_bar(stat = "identity") +
  ggtitle("Public Fixed Income Dollar Value Add", 
          subtitle = paste("Relative to Composite Benchmark", label.d)) +
  ylab("in Millions") + xlab("") + scale_y_continuous(labels = scales::dollar) +
  scale_fill_manual(values=IMD.palette()) + 
  guides(fill = guide_legend(title = "Effect:")) +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"), legend.position = 'bottom',
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))

grid.arrange(fi.return.plot, fi.dva.plot, ncol = 2)

## @knitr fi.allocation
end.qtr <- dim(allocation)[1]

fi.allocation.summary <- tomillions(fi.allocation[,-1] * tf.multiplier)

fi.allocation.long <- data.frame(
  colSums(coredata(fi.allocation.summary[(end.qtr -  3):end.qtr, ])),
  colSums(coredata(fi.allocation.summary[(end.qtr - 11):end.qtr, ])),
  colSums(coredata(fi.allocation.summary[(end.qtr - 19):end.qtr, ])),
  colSums(coredata(fi.allocation.summary[(end.qtr - 39):end.qtr, ]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  mutate(Portfolio = rownames(.)) %>%
  gather(Period, DVA, -Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = unique(.$Portfolio)))

fi.allocation.plot = ggplot(fi.allocation.long, aes(x = Period, y = DVA, fill = Portfolio)) +
  geom_bar(stat='identity', position=position_dodge()) + 
  xlab("") + scale_y_continuous(name = "in Millions", labels = scales::dollar) +
  ggtitle("Fixed Income Allocation Effect by Sub Asset Class",
          subtitle = paste("Relative to Composite Benchmark", label.d)) +
  scale_fill_manual(values = IMD.palette())+ labs(fill = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"), legend.position = 'none',
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))

fi.active.weights <- data.frame(
  colMeans(aw.fi.percent[range.1, -1]),
  colMeans(aw.fi.percent[range.3, -1]),
  colMeans(aw.fi.percent[range.5, -1]),
  colMeans(aw.fi.percent[range.10, -1])
) %>%
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  round(digits = 3) %>%
  mutate(Portfolio = rownames(.)) %>%
  gather(Period, DVA, -Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = unique(.$Portfolio)))

fi.aw.plot = ggplot(fi.active.weights, aes(x = Period, y = DVA, fill = Portfolio)) +
  geom_bar(stat='identity', position=position_dodge()) + 
  xlab("") + scale_y_continuous(name = "Active Weight", labels = scales::percent) +
  ggtitle("Fixed Income Average Monthly Active Weights",
          subtitle = paste("Relative to Composite Benchmark", label.d)) +
  scale_fill_manual(values = IMD.palette())+ labs(fill = "Asset Class") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"), 
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"),
        legend.position="bottom", legend.box.spacing = unit(0, "cm"),
        legend.text = element_text(size = 5), legend.key.size = unit(.25, "cm"))

ggarrange(fi.allocation.plot, fi.aw.plot, nrow = 1, ncol = 2, common.legend = T, legend = "bottom")

## @knitr fi.selection
fi.selection.summary <- tomillions(fi.selection[,-1] * tf.multiplier)

fi.selection.long <- data.frame(
  colSums(coredata(fi.selection.summary[(end.qtr -  3):end.qtr, ])),
  colSums(coredata(fi.selection.summary[(end.qtr - 11):end.qtr, ])),
  colSums(coredata(fi.selection.summary[(end.qtr - 19):end.qtr, ])),
  colSums(coredata(fi.selection.summary[(end.qtr - 39):end.qtr, ]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  mutate(Portfolio = c("Core", "High Yield", "EM Debt")) %>%
  gather(Period, DVA, -Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = unique(.$Portfolio)))

fi.selection.plot = ggplot(fi.selection.long, aes(x = Period, y = DVA, fill = Portfolio)) +
  geom_bar(stat='identity', position=position_dodge()) + 
  xlab("") + scale_y_continuous(name = "in Millions", labels = scales::dollar) +
  ggtitle("Fixed Income Selection Effect",
          subtitle = "Relative to Composite Benchmark") +
  scale_fill_manual(values = IMD.palette())+ labs(fill = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"), legend.title = element_blank(),
        legend.position = 'bottom', legend.text = element_text(size = 9))

print(fi.selection.plot)

## @knitr fi.trailing
pfi.trailing <- list()
for(i in names(r.fi.list[-1])) {
  if(i == "Emerging Market Debt") next
  ret <- merge(r.fi.list[[i]], b.fi.list[[i]])
  if(dim(ret)[1] <= 12) next
  return.1 <- t(Return.annualized(ret[((dim(ret)[1]-(12-1)):dim(ret)[1]), ], scale = 12))
  if(dim(ret)[1] >=36) {
    return.3 <- t(Return.annualized(ret[((dim(ret)[1]-(36-1)):dim(ret)[1]), ], scale = 12))}
  else {return.3 <- NA}
  if(dim(ret)[1] >=60){
    return.5 <- t(Return.annualized(ret[((dim(ret)[1]-(60-1)):dim(ret)[1]), ], scale = 12))}
  else {return.5 <- NA}
  if(dim(ret)[1] >= 120)  {
    return.10 <- t(Return.annualized(ret[((dim(ret)[1]-(120-1)):dim(ret)[1]), ], scale = 12))}
  else {return.10 <- NA}
  tr.ret <- cbind(return.1, return.3, return.5, return.10)
  desc <- c("One Year", "Three Year", "Five Year", "Ten Year")
  excess = as.data.frame(t(tr.ret))
  Excess = (excess[, 1] - excess[,2])
  tr.ret = data.frame(rbind(tr.ret, Excess))
  colnames(tr.ret) = desc
  tr.ret$Composite = c(i, "Benchmark", "Excess")
  pfi.trailing[[i]] = tr.ret
}
all.pfi = do.call(rbind, pfi.trailing)
rownames(all.peq) = NULL
all.pfi = subset(all.pfi, select = c("Composite", desc))
all.pfi$`One Year` = paste0(round(all.pfi$`One Year`*100,2),"%")
all.pfi$`Three Year` = paste0(round(all.pfi$`Three Year`*100,2),"%")
all.pfi$`Five Year` = paste0(round(all.pfi$`Five Year`*100,2),"%")
all.pfi$`Ten Year` = paste0(round(all.pfi$`Ten Year`*100,2),"%")
hlines = c(-1,seq(0,nrow(all.pfi), by=(+3)))
print(xtable(all.pfi, align = rep("r",6), digits = 2), hline.after = hlines,
      scalebox=.4, include.rownames = FALSE)

## @knitr int.mngd
i.funds <- nepc.map[which(nepc.map$Internal == 'Y'), "ID"]
i.r.list <- list()
i.b.list <- list()
i.mv.list <- list()
for(i in i.funds) {
  #get return, name, & inception date
  open <- nepc.map[which(nepc.map$ID == i), "Open"]
  if(open == "N") {
    d.date <- as.Date(nepc.map[which(nepc.map$ID == i), "Defunding"],format="%m/%d/%Y")
  }
  name <- nepc.map[which(nepc.map$ID == i), 'Short.Name']
  r <- subset(nepc.dat, nepc.dat$ID == i, select = c('Date','NetReturn'))
  r.xts <- xts(r[,-1], r[,1])
  r.xts <- r.xts[!is.na(r.xts),]
  r.xts <- r.xts[!duplicated(index(r.xts)), ]
  if(open == "N") {r.xts <- r.xts[paste0("/",d.date), ] }
  r.xts <- r.xts[xts.range, ]
  #get market values
  mv = subset(nepc.dat, nepc.dat$ID == i, select = c('Date','MthEndMV'))
  mv.xts <- xts(mv[,-1], mv[,1])
  mv.xts <- mv.xts[!duplicated(index(mv.xts)), ]
  mv.xts <- lag.xts(mv.xts, 1)
  mv.xts <- mv.xts[xts.range,]
  i.mv.list[[name]] <- mv.xts
  i.d <- as.character(time(mv.xts)[1])
  if(i == "RAYE11") {i.d = "2017-08-31"
    r.xts <- r.xts[paste0(i.d, "/"), ]
  }
  i.r.list[[name]] <- r.xts
  b.id <- nepc.map[which(nepc.map$ID == i), 'BM.ID']
  if(i == "RAYE11") {b.id <- "ASRSCIBETA"}
  b <- subset(nepc.dat, nepc.dat$ID == b.id, select = c('Date','NetReturn'))
  b.xts <- xts(b[ ,-1], b[ ,1])
  b.xts <- b.xts[paste0(i.d, "/"), ]
  if(open == "N") {b.xts <- b.xts[paste0("/",d.date), ]}
  b.xts = b.xts[xts.range, ]
  i.b.list[[name]] <- b.xts
}
mv.int <- do.call(merge, i.mv.list)
mv.int = na.fill(mv.int, 0)
i.w <- mv.int/rowSums(mv.int, na.rm = TRUE) #calculate weights
r.int <- do.call(merge, i.r.list)
r.int = na.fill(r.int, 0)
int.comp <- xts(rowSums(i.w * r.int), index(r.int)) #calculated weighted avg return
b.int <- do.call(merge, i.b.list)
b.int = na.fill(b.int, 0)
b.comp <- xts(rowSums(i.w * b.int), index(b.int))
im.dva <- (int.comp - b.comp) * rowSums(mv.int) %>%
  multiply_by(tf.multiplier) %>%
  tomillions(.)
colnames(im.dva) <- "Internally Managed Portfolios"
  
im.data <- merge(int.comp, b.comp)
colnames(im.data) = c("Internally Managed Composite", "Composite Benchmark")
end.mth <- dim(im.data)[1]

im.return.long <- data.frame(
  t(Return.annualized(im.data[(end.mth-11):end.mth, ])),
  t(Return.annualized(im.data[(end.mth-35):end.mth, ])),
  t(Return.annualized(im.data[(end.mth-59):end.mth, ])),
  t(Return.annualized(im.data[(end.mth-119):end.mth, ]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  mutate(Portfolio = rownames(.)) %>%
  gather(Period, Return, - Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = c("Internally Managed Composite", "Composite Benchmark")))

im.return.plot <- ggplot(im.return.long, aes(x = Period, y = Return, fill = Portfolio)) +
  geom_bar(stat = "identity", position = "Dodge") + 
  geom_text(aes(label = round(Return*100, 1)), position = position_dodge(width=0.9), vjust=-0.25)+
  xlab("") + scale_y_continuous(name = "Annualized Return", labels = scales::percent) +
  ggtitle("Internally Managed Portfolios and Composite Benchmark",
          subtitle = paste("Trailing Period Returns", label.d)) +
  scale_fill_manual(values = IMD.palette(), name = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"), legend.position = 'bottom',
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))


im.dva.long <- data.frame(
  colSums(coredata(im.dva[(end.mth - 11):end.mth, ])),
  colSums(coredata(im.dva[(end.mth - 35):end.mth, ])),
  colSums(coredata(im.dva[(end.mth - 59):end.mth, ])),
  colSums(coredata(im.dva[(end.mth - 119):end.mth, ]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  mutate(Effect = rownames(.)) %>%
  gather(Period, DVA, -Effect) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year")))

im.dva.plot <- ggplot(im.dva.long, aes(x = Period, y = DVA, fill = Effect)) +
  geom_bar(stat = "identity") +
  ggtitle("Internally Managed Portfolios Dollar Value Add", 
          subtitle = paste("Relative to Composite Benchmark", label.d)) +
  ylab("in Millions") + xlab("") + scale_y_continuous(labels = scales::dollar) +
  scale_fill_manual(values=IMD.palette()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10), legend.title = element_blank(),
        axis.title.y = element_text(size = 10, face = "italic"), legend.position = 'none')

grid.arrange(im.return.plot, im.dva.plot, ncol = 2)

## @knitr privates.ret
#build public composite and write.csv
pub.name <- c("Public Equity","Public Fixed Income", "Inflation Linked", "Cash", "Multi-Asset Class")
pub.mvs <- do.call(merge, mv.list[pub.name])
pub.mvs = na.fill(pub.mvs, 0)
pub.weight <- pub.mvs/rowSums(pub.mvs)
pub.ret <- do.call(merge, r.list[pub.name])
pub.ret = na.fill(pub.ret, 0)
pub.comp.r <- rowSums(pub.weight * pub.ret)
pub.bm <- do.call(merge, b.list[pub.name])
pub.bm = na.fill(pub.bm, 0)
pub.comp.b <- rowSums(pub.weight * pub.bm)
pub.comp <- data.frame("Date" = index(pub.mvs), "Return" = pub.comp.r, "Benchmark" = pub.comp.b, "MVS" = rowSums(pub.mvs))
write.csv(pub.comp,  "P:/IMD/Karl/R Projects/private markets dollar value add/all public.csv")

#private returns
priv.ret = read.csv("P:/IMD/Karl/R Projects/Total Fund Value Add/Data/private returns.csv")
priv.ret = priv.ret[,-1]
total.priv = subset(priv.ret, priv.ret$Portfolio=="Total Private Markets")
total.priv = total.priv[ ,-4]
total.priv$Excess = total.priv$Portfolio_Return - total.priv$Benchmark_Return
excess <- data.frame("Portfolio"=priv.ret$Portfolio, 
                     "Excess"=(priv.ret$Portfolio_Return - priv.ret$Benchmark_Return),
                     "Period" = priv.ret$Period)
excess$Period = factor(excess$Period, levels = c("One Year", "Three Year", "Five Year",
                                                 "Ten Year", "Inception"))
priv.ret.l <- gather(priv.ret, Return_Category, Return, -Period, -Portfolio)
priv.ret.l$Return_Category=factor(priv.ret.l$Return_Category,
                                  levels=c("Portfolio_Return","Benchmark_Return"))
priv.ret.l$Period = factor(priv.ret.l$Period, levels = c("One Year", "Three Year", "Five Year",
                                                         "Ten Year", "Inception"))
ports <- unique(priv.ret.l$Portfolio)
ret.t = list()
for(p in ports) {
  if(p == "Total Private Markets") next
  ret <- subset(priv.ret.l, priv.ret.l$Portfolio == p)
  ret.w <- spread(ret[ ,-2], Period, Return)
  e = subset(excess, excess$Portfolio == p)
  excess.w <- spread(e[,-1], Period, Excess)
  new.ret <- rbind(ret.w[,-1], excess.w)
  Category = c(p, "Benchmark", "Excess")
  ret.t[[p]] = data.frame(cbind(Category, new.ret), row.names = NULL)
}
total.pm = subset(priv.ret.l, priv.ret.l$Portfolio == "Total Private Markets")
ret.w <- spread(total.pm[ ,-2], Period, Return)
e = subset(excess, excess$Portfolio == "Total Private Markets")
excess.w <- spread(e[,-1], Period, Excess)
new.ret <- rbind(ret.w[,-1], excess.w)
rownames(new.ret) = c("Total Private Markets", "Benchmark", "Excess")
#total.pm = data.frame(cbind(Category, new.ret), row.names = NULL)
pm.comp = do.call(rbind, ret.t)
rownames(pm.comp) = c("Private Equity", "PE Benchmark", "PE Excess", "Real Estate Current", 
                      "RE Current Benchmark", "RE Current Excess", "Real Estate Legacy",
                      "RE Legacy Benchmark", "RE Legacy Excess", "Opportunistic Debt",
                      "Opp Debt Benchmark","Opp Debt Excess","Private Debt","PD Benchmark",
                      "PD Excess","Farmland & Infrastructure", "Farmland Benchmark","Farmland Excess")
pm.comp$Ten.Year = pm.comp$Inception
pm.comp = pm.comp[,-5]
final.pm <- new.ret
colnames(final.pm) = c("One Year", "Three Year", "Five Year", "Inception")
final.pm$`One Year` = paste0(round(final.pm$`One Year`*100,2),"%")
final.pm$`Three Year` = paste0(round(final.pm$`Three Year`*100,2),"%")
final.pm$`Five Year` = paste0(round(final.pm$`Five Year`*100,2),"%")
final.pm$Inception = paste0(round(final.pm$Inception*100,2),"%")
hlines = c(-1,seq(0,nrow(final.pm), by=(+3)))
#rownames(all)[c(1,4,7)] <- paste("BOLD", rownames(all)[c(1,4,7)])
#bold.somerows <- function(x) gsub('BOLD(.*)', paste('\\\\textbf{\\1','}'),x)
print(xtable(final.pm, align = rep("r",5), digits = 2), hline.after = hlines, 
      scalebox=.4)


## @knitr private.mvs
priv.names <- c("Real Estate", "Farmland", "Infrastructure", "Private Debt", "Opportunistic Debt", "Private Equity", "Private Opportunistic Equity")
priv.mvs <- do.call(merge, mv.list[priv.names])
colnames(priv.mvs) <- priv.names
priv.mvs = na.fill(priv.mvs, 0)
priv.mvs$combo <- priv.mvs$Farmland + priv.mvs$Infrastructure
priv.mvs$Farmland <- NULL
priv.mvs$Infrastructure <- NULL
colnames(priv.mvs)[6] = "Farmland and Infrastructure"

#turn data into quarter end data
quarter.end <- endpoints(priv.mvs, on = "quarters")
priv.qtr <- priv.mvs[quarter.end, ]
#priv.qtr = na.fill(priv.qtr, 0)
#priv.qtr$'Farmland and Infrastructure' = priv.qtr$'Farmland' + priv.qtr$'Infrastructure'
#priv.qtr = priv.qtr[,-(1:2)]
priv.qtr = data.frame("Date"=time(priv.qtr), coredata(priv.qtr))
pn = priv.names[-(2:3)]
pn = c(pn, "Farmland and Infrastructure")
colnames(priv.qtr)[-1] = pn
priv.qtr.tidy <- gather(priv.qtr, Cat, MV, -Date)
priv.qtr.tidy$MV = tomillions(priv.qtr.tidy$MV)
priv.qtr.tidy$Cat = factor(priv.qtr.tidy$Cat, levels = c("Private Equity", "Real Estate", 
                                                         "Opportunistic Debt","Private Debt",
                                                         "Private Opportunistic Equity",
                                                         "Farmland and Infrastructure"))
priv.mvs <- ggplot()+
  geom_bar(data = priv.qtr.tidy,aes(x = Date, y = MV, fill = Cat), stat="identity")+
  ggtitle(paste("Private Market Values", label.d)) + 
  scale_fill_manual(values = IMD.palette()[-1]) + xlab("") + ylab("in Millions")+
  scale_y_continuous(labels = scales::dollar)+
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = 'bold'), legend.position = "bottom",
        legend.text = element_text(size = 6), legend.key.size = unit(.25, "cm"), 
        legend.title = element_blank(), axis.title.y = element_text(size = 6, face = 'italic'))
print(priv.mvs)


## @knitr private.dva
namedva = c("One Year","Three Year","Five Year","Ten Year")
namedva = factor(namedva,levels=namedva) #this lets you make make namedva the x axis and keeps it in the right order
lookback=read.csv("P:/IMD/Karl/R Projects/Total Fund Value Add/Data/lookback private add value.csv")
lookback$Date=as.Date(lookback$Date)
valudate = lookback$Date[length(lookback$Date)]
lb = lookback[ ,-1]
valudate = lb$Date[length(lb$Date)]
pm.port <- colnames(lb[,-1])
dva.pm <- list()
for(p in pm.port) {
  dvavals=vector()
  dvavals[1] = lookback[lookback$Date==(valudate-years(1)), p]
  dvavals[2] = lookback[lookback$Date==(valudate-years(3)), p]
  dvavals[3] = lookback[lookback$Date==(valudate-years(5)), p]
  rownumber = which(lookback$Date==(valudate-years(10)))
  if (length(rownumber)==0) rownumber=1
  dvavals[4] = lookback[rownumber, p]
  dva.pm[[p]] = data.frame(Period=namedva, Dollar_Value_Add=dvavals, Portfolio=rep(p ,4))
}
#names(dva.pm) = c("Private Equity", "Real Estate Current", "Real Estate Legacy",
#                 "Opp Debt", "Private Debt", "Farmland & Infrastructure", "Total Private")
dvadf.pm <- do.call(rbind, dva.pm)
dvadf.pm$Period = gsub("Ten Year", "Inception", dvadf.pm$Period)
dvadf.pm$Period = factor(dvadf.pm$Period, levels=c("One Year", "Three Year", "Five Year", "Inception"))
dvadf.pm$Portfolio = gsub("Total.","",dvadf.pm$Portfolio)
dvadf.pm$Portfolio = gsub(".Portfolio","",dvadf.pm$Portfolio)
dvadf.pm$Portfolio = gsub("OPP","Opportunistic Debt",dvadf.pm$Portfolio)
dvadf.pm$Portfolio = gsub("PD","Private Debt",dvadf.pm$Portfolio)
dvadf.pm$Portfolio = gsub("RE","Real Estate",dvadf.pm$Portfolio)
dvadf.pm$Portfolio = gsub("PE","Private Equity",dvadf.pm$Portfolio)
dvadf.pm$Portfolio = gsub("Privates","Total Privates",dvadf.pm$Portfolio)
dvadf.pm$Dollar_Value_Add = tomillions(dvadf.pm$Dollar_Value_Add)
#rownames(dvadf.pm) = NULL
dvadf.pm$Portfolio = factor(dvadf.pm$Portfolio, levels = c("Total Privates", "Private Equity",
                                                           "Real Estate.Current","Opportunistic Debt",
                                                           "Private Debt", "FARM","Real Estate.Legacy"))
dva.pm <- ggplot(dvadf.pm, aes(x=Period,y=Dollar_Value_Add, fill=Portfolio))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Private Markets Dollar Value Add Relative to Composite Benchmarks")+
  ylab("in Millions")+scale_y_continuous(labels = scales::dollar)+
  scale_fill_manual(values=IMD.palette())+ ylab("in Millions")+
  xlab("")+
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = 'bold'), legend.title = element_blank(),
        legend.text = element_text(size = 6), legend.key.size = unit(.25, "cm"), 
        axis.title.y = element_text(size = 6),
        axis.text.x = element_text(size = 6, face = "bold"), axis.text.y = element_text(size = 6, face = "bold"))

print(dva.pm)

## @knitr private.ret.comp
pm.ac = pm.comp[ ,-1]
colnames(pm.ac) = c("One Year", "Three Year", "Five Year", "Inception")
pm.ac$`One Year` = paste0(round(pm.ac$`One Year`*100,2),"%")
pm.ac$`Three Year` = paste0(round(pm.ac$`Three Year`*100,2),"%")
pm.ac$`Five Year` = paste0(round(pm.ac$`Five Year`*100,2),"%")
pm.ac$`Inception` = paste0(round(pm.ac$`Inception`*100,2),"%")
hlines = c(-1,seq(0,nrow(pm.ac), by=(+3)))
print(xtable(pm.ac, align = rep("r", 5)), hline.after = hlines, scalebox=.6)

