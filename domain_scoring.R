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

#combine other email domains
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

##FUNCTIONS

#prints out the % of scammers with the domain x (x is domain in string)
calDomainScammer <- function(x){
  row = 1
  counter = 0
  domainCol = 3
  verifyCol = 14
  total = 0
  while (row <= nrow(prepost)){
    domain = prepost[row, domainCol]
    verify = prepost[row, verifyCol]
    if (grepl(x, domain)){
      total = total + 1
      if (verify == "scammer"){
        counter = counter + 1
      }
    }
    row = row + 1
  }
  paste(counter/total*100)
}

##SCORING MECHANISM - NOT DONE

domainScammers['score'] = ""

#calculates score to be allocated for domain based on % scammers
row = 1
score = 0
percentageCol = 4
scoreCol = 5
while (row <= nrow(domainScammers)){
  percentage = as.numeric(domainScammers[row, percentageCol])
  score = percentage/100*5
  domainScammers[row,scoreCol] = round(score)
  row = row + 1
}

#assign score to domain scammers
merged = merge(prepost, domainScammers, by.x="domain", by.y="domain")

#create a results column
merged["results"] <- NA
#create a reason column
merged['reason'] <- NA
row = 1
score_col = 18
results_col = 19
reason_col = 20
while (row <= nrow(merged)) {
  if (merged[row,score_col] >= 4){ #normalized: mean(domainScammers$score) = 3.846154
    merged[row,results_col] = "scammer"
    merged[row,reason_col] = "scammer domain" #only reflects when score exceeds 4
  } else {
    merged[row,results_col] = "non-scammer"
  }
  row = row + 1
}

library(RTextTools)

#generate confusion matrix
confusion_matrix = table(merged$status, merged$result)
confusion_matrix
#              non-scammer scammer
#non-scammer         378       0
#scammer             132       8

recall_accuracy(merged$status, merged$results)
#moderately accurate: weighted recall = 0.7451737

#removing unnecessary cols
merged = merged[,-c(15,16,17)]

#rearrange cols
merged = merged[,c("domain", "account_id", "username", "poster", "license", "carrier", "phone_num", "latitude", "longitude", "postingLatitude", "postingLongitude", "citystate", "price", "status", "results", "score", "reason")]