##GG PLOTS
library(ggplot2)

#dataframe of how many each tld is present (2 of them NAs)
tldTable = data.frame(table(prepost$tld))
colnames(tldTable) = c('tld', 'count')

#ggplot of total number of each tld type by descending order
tldPlot = ggplot(tldTable, aes(x = reorder(tld, -count), y = count)) + geom_bar(stat = "identity")
tldPlot

#dataframe of how many scammers is present in each domain
tldScammersTable = data.frame(table(prepost$tld, prepost$status))
colnames(tldScammersTable) = c('tld', 'status', 'count')

#ggplot of TLD scammers
tldScammersPlot = ggplot(tldScammersTable, aes(x = reorder(tld, -count), y = count, fill = status)) + geom_bar(stat = "identity")
tldScammersPlot + labs(title = "Scammers based on TLD", y = "Count", x = "TLD", fill = "Status")

#count how many scammers is present in each tld
tldScammers = data.frame(table(prepost$tld, prepost$status=='scammer'))
colnames(tldScammers) = c('tld', 'scammer_status', 'count')

#merge table
tldScammersTrue = tldScammers[tldScammers$scammer_status!="FALSE",]
tldScammersFalse = tldScammers[tldScammers$scammer_status!="TRUE",]
finalTldTable = merge(tldScammersTrue, tldScammersFalse, by.x="tld", by.y="tld")
finalTldTable = finalTldTable[,-c(2,4)]
finalTldTable[c("total", "percentage")] = NA
colnames(finalTldTable) = c("tld", "scammers", "nonscammers", "total", "scammersPercentage")
finalTldTable$total = with(finalTldTable, scammers+nonscammers)
finalTldTable$scammersPercentage = with(finalTldTable, scammers/total*100)

#remove False rows and 0 Count
tldScammers = tldScammers[tldScammers$scammer_status!="FALSE" & tldScammers$count!="0",]

##FUNCTIONS

#prints out the % of scammers with the domain x (x is domain in string)
calTldScammer <- function(x){
  row = 1
  counter = 0
  tldCol = 56
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

tldScammers['percentage'] = ""
row = 1
tldCol = 1
percentageCol = 4
while (row <= nrow(tldScammers)){
  tldScammers[row, percentageCol] = calTldScammer(tldScammers[row, tldCol])
  row = row + 1
}

tldScammers['score'] = ""

#calculates score to be allocated for domain based on % scammers
row = 1
score = 0
percentageCol = 4
scoreCol = 5
while (row <= nrow(tldScammers)){
  percentage = as.numeric(tldScammers[row, percentageCol])
  score = percentage/100*5
  tldScammers[row,scoreCol] = round(score)
  row = row + 1
}

#assign score to domain scammers
merged = merge(prepost, tldScammers, by.x="tld", by.y="tld")

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
merged = merged[,c("tld", "account_id", "username", "poster", "license", "carrier", "phone_num", "latitude", "longitude", "postingLatitude", "postingLongitude", "citystate", "price", "status", "results", "score", "reason")]