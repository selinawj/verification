##GG PLOTS
library(ggplot2)

#dataframe of how many each domain is present
domainTable = data.frame(table(prepost$domain))
colnames(domainTable) = c('domain', 'count')

#ggplot of total number of each domain type by descending order
domainPlot = ggplot(domainTable, aes(x = reorder(domain, -count), y = count)) + geom_bar(stat = "identity")
domainPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#dataframe of how many scammers is present in each domain
domainScammersTable = data.frame(table(prepost$domain, prepost$status))
colnames(domainScammersTable) = c('domain', 'status', 'count')

#ggplot of domain scammers
domainScammersPlot = ggplot(domainScammersTable, aes(x = reorder(domain, -count), y = count, fill = status)) + geom_bar(stat = "identity")
domainScammersPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Scammers based on Domain", y = "Count", x = "Domain", fill = "Status")

newDomainTable = data.frame(matrix(nrow=12 ,ncol=3))
colnames(newDomainTable) = c('domain', 'status', 'count')
newDomainTable[c(1,2),1] = "outlook.com"
newDomainTable[c(3,4),1] = "aol.com"
newDomainTable[c(5,6),1] = "hotmail.com"
newDomainTable[c(7,8),1] = "yahoo.com"
newDomainTable[c(9,10),1] = "gmail.com"
newDomainTable[c(11,12),1] = "others"
newDomainTable[c(1,3,5,7,9,11),2] = "scammer"
newDomainTable[c(2,4,6,8,10,12),2] = "non-scammer"

row = 1
domainCol=1
statusCol=2
countCol=3
otherScam=0
otherNonScam=0
while (row <= nrow(domainScammersTable)){
  if (domainScammersTable[row,domainCol]=="outlook.com" & domainScammersTable[row,statusCol]=="scammer"){
    newDomainTable[1,3] = domainScammersTable[row,countCol]
  } else if (domainScammersTable[row,domainCol]=="outlook.com" & domainScammersTable[row,statusCol]=="non-scammer"){
    newDomainTable[2,3] = domainScammersTable[row,countCol]
  } else if (domainScammersTable[row,domainCol]=="aol.com" & domainScammersTable[row,statusCol]=="scammer"){
    newDomainTable[3,3] = domainScammersTable[row,countCol]
  } else if (domainScammersTable[row,domainCol]=="aol.com" & domainScammersTable[row,statusCol]=="non-scammer"){
    newDomainTable[4,3] = domainScammersTable[row,countCol]
  } else if (domainScammersTable[row,domainCol]=="hotmail.com" & domainScammersTable[row,statusCol]=="scammer"){
    newDomainTable[5,3] = domainScammersTable[row,countCol]
  } else if (domainScammersTable[row,domainCol]=="hotmail.com" & domainScammersTable[row,statusCol]=="non-scammer"){
    newDomainTable[6,3] = domainScammersTable[row,countCol]
  } else if (domainScammersTable[row,domainCol]=="yahoo.com" & domainScammersTable[row,statusCol]=="scammer"){
    newDomainTable[7,3] = domainScammersTable[row,countCol]
  } else if (domainScammersTable[row,domainCol]=="yahoo.com" & domainScammersTable[row,statusCol]=="non-scammer"){
    newDomainTable[8,3] = domainScammersTable[row,countCol]
  } else if (domainScammersTable[row,domainCol]=="gmail.com" & domainScammersTable[row,statusCol]=="scammer"){
    newDomainTable[9,3] = domainScammersTable[row,countCol]
  } else if (domainScammersTable[row,domainCol]=="gmail.com" & domainScammersTable[row,statusCol]=="non-scammer"){
    newDomainTable[10,3] = domainScammersTable[row,countCol]
  } else if (domainScammersTable[row,domainCol]!="outlook.com" & domainScammersTable[row,domainCol]!="aol.com" & domainScammersTable[row,domainCol]!="hotmail.com" & domainScammersTable[row,domainCol]!="yahoo.com" & domainScammersTable[row,domainCol]!="gmail.com" & domainScammersTable[row,statusCol]=="scammer"){
    otherScam = otherScam + domainScammersTable[row,countCol]
    newDomainTable[11,3] = otherScam
  } else if (domainScammersTable[row,domainCol]!="outlook.com" & domainScammersTable[row,domainCol]!="aol.com" & domainScammersTable[row,domainCol]!="hotmail.com" & domainScammersTable[row,domainCol]!="yahoo.com" & domainScammersTable[row,domainCol]!="gmail.com" & domainScammersTable[row,statusCol]=="non-scammer"){
    otherNonScam = otherNonScam + domainScammersTable[row,countCol]
    newDomainTable[12,3] = otherNonScam
  }
  row = row + 1
}

#ggplot of domain scammers
newDomainPlot = ggplot(newDomainTable, aes(x = reorder(domain, -count), y = count, fill = status)) + geom_bar(stat = "identity") + geom_text(aes(x = domain, y = count, label = count, group = status), position = position_dodge(width = 1), vjust = -0.5)
newDomainPlot + labs(title = "Scammers based on Domain", y = "Count", x = "Domain", fill = "Status")

library(reshape2)

#final table
finalDomainTable = dcast(newDomainTable, newDomainTable$domain~newDomainTable$status)
colnames(finalDomainTable) = c("domain", "nonscammers", "scammers")
finalDomainTable[c("total", "percentage")] = NA
finalDomainTable$total = with(finalDomainTable, scammers+nonscammers)
finalDomainTable$percentage = with(finalDomainTable, scammers/total*100)
finalDomainTable$percentage = with(finalDomainTable, scammers/total*100)