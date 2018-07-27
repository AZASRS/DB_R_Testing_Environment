setwd=('P:/IMD/Karl/R projects/private investment performance')
require(lubridate)
require(plyr)

#read file downloaded from AIS-SS
all.working <- read.csv("P:/IMD/Karl/R projects/private investment performance/Dynamic Performance.csv", stringsAsFactors=FALSE)
FundInfo <- read.csv("P:/IMD/Karl/R projects/private investment performance/fundinfo.csv", stringsAsFactors=FALSE)
all.working=all.working[all.working[,1]%in%FundInfo$Short,]

#math
all.working$Commitments <- (all.working$Commitments/1000000)
all.working$Commit <- round(all.working$Commitments, digits = 0)
all.working$Remaining.Commitment <- (all.working$Remaining.Commitment/1000000)
all.working$Unfunded <- round(all.working$Remaining.Commitment, digits = 0)

#Eliminate columns no longer needed
all.working$Commitments <- NULL
all.working$Remaining.Commitment <- NULL

#rename columns
colnames(all.working)[1] <- "Short"

#remove/reorder to append to fundinfo file
FundInfo$Unfunded <- all.working$Unfunded[match(FundInfo$Short, all.working$Short)]
#FundInfo$Commit <- all.working$Commit[match(FundInfo$Commit, all.working$Commit)]
write.csv(FundInfo, file="P:/IMD/Karl/R projects/private investment performance/fundinfo.csv", row.names = FALSE)

# add line to remove NA's in data in FundInfo.
# will also either need to change the rows where the header replaces the space with a period.

#change date in Unfunded.Date in fundinfo.csv

#End



