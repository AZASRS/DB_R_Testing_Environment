setwd=("P:/IMD/IMD Vista/Administrative/IMD Budget/FY18")
require(lubridate) #date package for transforming dates to something usable
require(plyr) #transforming data to something usable
require(dplyr)  #transforming data to something usable
require(zoo)  #date package for transforming dates to something usable

# read fund info file
Fund.raw <- read.csv("P:/IMD/IMD Vista/Administrative/IMD Budget/FY18/201805/201805 - Partnership_Investment_Transaction_Download.csv", header = TRUE, sep = "|")

Group.Fees <- data.frame(Fund.raw$FUND_SHORT_NAME, Fund.raw$INVESTMENT_NAME, Fund.raw$CONTRIB_DISTRIB_DATE, Fund.raw$BASE_FEES_IN_COMMIT, Fund.raw$BASE_MF_IN_COMMIT, Fund.raw$BASE_FEES_OUT_COMMIT, Fund.raw$BASE_MF_OUT_COMMIT)

colnames(Group.Fees)[1] <- "Asset Class"
colnames(Group.Fees)[2] <- "Fund Name"
colnames(Group.Fees)[3] <- "Date"
colnames(Group.Fees)[4] <- "Carry in Commit"
colnames(Group.Fees)[5] <- "Mgmt Fees in Commit"
colnames(Group.Fees)[6] <- "Carry outside Commit"
colnames(Group.Fees)[7] <- "Mgmt Fees outside Commit"

write.csv(Group.Fees, file="P:/IMD/IMD Vista/Administrative/IMD Budget/FY18/201805/201805 - PrivateMktsTrans.csv", row.names=FALSE)

#You will need to go to the csv file and open it.
#The data column will need to be formatted to Short Date
#The numerical columns will need to be changed from the currency format to a number format (no commas)
#Then resave the csv and then run the rest of the script.

Re.Group <- read.csv("P:/IMD/IMD Vista/Administrative/IMD Budget/FY18/201805/201805 - PrivateMktsTrans.csv", stringsAsFactors=FALSE)
#Re.Group$Date <- ymd(Re.Group$Date)

#change date to not include unwanted periods
date.sub <- Re.Group[Re.Group$Date < "2018-01-01",]



#additional formatting
Re.Group$Mgmt.Fees.in.Commit <- as.numeric(Re.Group$Mgmt.Fees.in.Commit)
Re.Group$Mgmt.Fees.outside.Commit <- as.numeric(Re.Group$Mgmt.Fees.outside.Commit)
Re.Group$Carry.in.Commit <- as.numeric(Re.Group$Carry.in.Commit)
Re.Group$Carry.outside.Commit <- as.numeric(Re.Group$Carry.outside.Commit)



#math
Re.Group$Total.Mgmt.Fees <- (Re.Group$Mgmt.Fees.in.Commit + Re.Group$Mgmt.Fees.outside.Commit)
Re.Group$Total.Carry <- (Re.Group$Carry.in.Commit + Re.Group$Carry.outside.Commit)

#formatting
Re.Group$Asset.Class <- gsub("ASRS - ", "", Re.Group$Asset.Class)
Re.Group$Fund.Name <- NULL
Re.Group$Date <- NULL
Re.Group$Carry.in.Commit <- NULL
Re.Group$Mgmt.Fees.in.Commit <- NULL
Re.Group$Carry.outside.Commit <- NULL
Re.Group$Mgmt.Fees.outside.Commit <- NULL

AC.index <- unique(Re.Group$Asset.Class)
AC.index <- as.data.frame(AC.index)

TFarm=subset(Re.Group,Re.Group$Asset.Class=="Farmland and Timber")
TInfra=subset(Re.Group,Re.Group$Asset.Class=="Infrastructure")
TOpD=subset(Re.Group,Re.Group$Asset.Class=="Opportunistic Debt")
TOpE=subset(Re.Group,Re.Group$Asset.Class=="Opportunistic Equity")
TORE=subset(Re.Group,Re.Group$Asset.Class=="Owned Real Estate")
TPd=subset(Re.Group,Re.Group$Asset.Class=="Private Debt")
TPe=subset(Re.Group,Re.Group$Asset.Class=="Private Equity")
TRe=subset(Re.Group,Re.Group$Asset.Class=="Real Estate")

All.combine = rbind(Re.Group, c("Total All",apply(Re.Group[,-1], 2, sum, na.rm=TRUE))) 
Farm.combine = rbind(TFarm, c("Total Farmland and Timber",apply(TFarm[,-1], 2, sum, na.rm=FALSE)))
#colnames(Farm.combine)[1] <- "Asset.Class"
#colnames(Farm.combine)[2] <- "Total.Mgmt.Fees"
#colnames(Farm.combine)[3] <- "Total.Carry"
Infra.combine = rbind(TInfra, c("Total Infrastructure",apply(TInfra[,-1], 2, sum, na.rm=TRUE))) 
OpD.combine = rbind(TOpD, c("Total Opp Debt",apply(TOpD[,-1], 2, sum, na.rm=TRUE))) 
OpE.combine = rbind(TOpE, c("Total Opp Equity",apply(TOpE[,-1], 2, sum, na.rm=TRUE))) 
ORE.combine = rbind(TORE, c("Total Owned Real Estate",apply(TORE[,-1], 2, sum, na.rm=TRUE))) 
Pd.combine = rbind(TPd, c("Total Private Debt",apply(TPd[,-1], 2, sum, na.rm=TRUE))) 
Pe.combine = rbind(TPe, c("Total Private Equity",apply(TPe[,-1], 2, sum, na.rm=TRUE))) 
Re.combine = rbind(TRe, c("Total Real Estate",apply(TRe[,-1], 2, sum, na.rm=TRUE)))

#add to Report
Report1 <- filter(All.combine, Asset.Class == "Total All")
Report2 <- filter(Farm.combine, Asset.Class == "Total Farmland and Timber")
Report3 <- filter(Infra.combine, Asset.Class == "Total Infrastructure")
Report4 <- filter(OpD.combine, Asset.Class == "Total Opp Debt")
Report5 <- filter(OpE.combine, Asset.Class == "Total Opp Equity") 
Report6 <- filter(ORE.combine, Asset.Class == "Total Owned Real Estate")
Report7 <- filter(Pd.combine, Asset.Class == "Total Private Debt")
Report8 <- filter(Pe.combine, Asset.Class == "Total Private Equity")
Report9 <- filter(Re.combine, Asset.Class == "Total Real Estate")

Report <- rbind(Report2, Report3, Report4, Report5, Report6, Report7, Report8, Report9, Report1)

print(Report)
write.csv(Report, file="P:/IMD/IMD Vista/Administrative/IMD Budget/FY18/201805/201805 - BudgetRep_PrivMFees.csv", row.names=FALSE)

#write.csv(Pd.combine, file="P:/IMD/Karl/R projects/private investment performance/FeeCheck_privatedebt.csv", row.names=FALSE)
#write.csv(Pe.combine, file="P:/IMD/Karl/R projects/private investment performance/FeeCheck_privateequity.csv", row.names=FALSE)
#write.csv(Re.combine, file="P:/IMD/Karl/R projects/private investment performance/FeeCheck_realestate.csv", row.names=FALSE)
#write.csv(All.combine, file="P:/IMD/Karl/R projects/private investment performance/FeeCheck_everything.csv", row.names=FALSE)


#built extra check for farmland - does not have activity it will error out report.   This could happen to infrastructure, owned real estate at some point too.


