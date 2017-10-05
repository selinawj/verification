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

#count how many scammers is present in each domain
domainScammers = data.frame(table(prepost$domain, prepost$status=='scammer'))
colnames(domainScammers) = c('domain', 'scammer_status', 'count')

#merge table
domainScammersTrue = domainScammers[domainScammers$scammer_status!="FALSE",]
domainScammersFalse = domainScammers[domainScammers$scammer_status!="TRUE",]
finalDomainTable = merge(domainScammersTrue, domainScammersFalse, by.x="domain", by.y="domain")
finalDomainTable = finalDomainTable[,-c(2,4)]
finalDomainTable[c("total", "percentage")] = NA
colnames(finalDomainTable) = c("domain", "scammers", "nonscammers", "total", "scammersPercentage")
finalDomainTable$total = with(finalDomainTable, scammers+nonscammers)
finalDomainTable$scammersPercentage = with(finalDomainTable, scammers/total*100)

#remove False rows and 0 Count
domainScammers = domainScammers[domainScammers$scammer_status!="FALSE" & domainScammers$count!="0",]

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

##SCORING MECHANISM

domainScammers['percentage'] = ""
row = 1
domainCol = 1
percentageCol = 4
while (row <= nrow(domainScammers)){
  domainScammers[row, percentageCol] = calDomainScammer(domainScammers[row, domainCol])
  row = row + 1
}

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