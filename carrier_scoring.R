##GG PLOTS
library(ggplot2)

#uniformize from factors to characters format
i <- sapply(prepost, is.factor)
prepost[i] <- lapply(prepost[i], as.character)

#dataframe of how many each phone numbers are present
carrierTable = data.frame(table(prepost$carrier))
colnames(carrierTable) = c('type','count')

#uniformize from factors to characters format
i <- sapply(carrierTable, is.factor)
carrierTable[i] <- lapply(carrierTable[i], as.character)
carrierTable[1,1] = "NA"

#ggplot of total number of each carrier type
carrierPlot = ggplot(carrierTable, aes(x = reorder(type, -count), y = count)) + geom_bar(stat = "identity") + geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5)
carrierPlot + labs(title = "Number of each type of carrier", y = "Count", x = "Carrier")

#count how many scammers is present in each carrier
carrierScammers = data.frame(table(prepost$carrier, prepost$status=='scammer'))
colnames(carrierScammers) = c('carrier', 'status', 'count')

#uniformize from factors to characters format
i <- sapply(carrierScammers, is.factor)
carrierScammers[i] <- lapply(carrierScammers[i], as.character)
carrierScammers[1,1] = "NA"
carrierScammers[5,1] = "NA"

carrierScammers$status[carrierScammers$status == "FALSE"] <- "non-scammers"
carrierScammers$status[carrierScammers$status == "TRUE"] <- "scammers"

#plot of how many scammers in each phone category
carrierScammersPlot = ggplot(carrierScammers) + geom_bar(aes(x = reorder(carrier, -count), y = count, fill = status, group = status), stat = "identity", position = 'dodge') + geom_text(aes(x = reorder(carrier, -count), y = count, label = count, group = status), position = position_dodge(width = 1), vjust = -0.5)
carrierScammersPlot + labs(title = "Scammers based on Carrier Type", y = "Count", x = "Carrier Type", fill = "Status")

#plot of how many posters in each status
scammersPlot = ggplot(carrierScammers, aes(x = reorder(status, -count), y = count, fill = carrier)) + geom_bar(stat = "identity", position = 'dodge') + geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5)
scammersPlot + labs(title = "Num of Carrier Type within each scammer status", y = "Count", x = "Status", fill = "Carrier")

#merge table
carrierScammersTrue = carrierScammers[carrierScammers$status!="non-scammers",]
carrierScammersFalse = carrierScammers[carrierScammers$status!="scammers",]
finalCarrierTable = merge(carrierScammersTrue, carrierScammersFalse, by.x="carrier", by.y="carrier")
finalCarrierTable =finalCarrierTable[,-c(2,4)]
finalCarrierTable[c("total", "percentage")] = NA
colnames(finalCarrierTable) = c("carrier", "scammers", "nonscammers", "total", "scammersPercentage")
finalCarrierTable$total = with(finalCarrierTable, scammers+nonscammers)
finalCarrierTable$scammersPercentage = with(finalCarrierTable, scammers/total*100)

##FUNCTIONS

#prints out the % of NA carrier scammers
calNaCarrierScammer <- function(){
  row = 1
  counter = 0
  carrierCol = 6
  verifyCol = 14
  naCounter = 0
  while (row <= nrow(prepost)){
    carrier = prepost[row, carrierCol]
    verify = prepost[row, verifyCol]
    if (carrier == ""){ #NA
      if (verify == "scammer"){
        naCounter = naCounter + 1
      }
    }
    row = row + 1
  }
  paste(naCounter/sum(prepost$status=="scammer")*100)
}

#prints out the % of mobile carrier scammers
calMobileScammer <- function(){
  row = 1
  counter = 0
  carrierCol = 6
  verifyCol = 14
  mobileCounter = 0
  while (row <= nrow(prepost)){
    carrier = prepost[row, carrierCol]
    verify = prepost[row, verifyCol]
    if (carrier == "mobile"){
      if (verify == "scammer"){
        mobileCounter = mobileCounter + 1
      }
    }
    row = row + 1
  }
  paste(mobileCounter/sum(prepost$status=="scammer")*100)
}

#prints out the % of voip carrier scammers
calVoipScammer <- function(){
  row = 1
  counter = 0
  carrierCol = 6
  verifyCol = 14
  voipCounter = 0
  while (row <= nrow(prepost)){
    carrier = prepost[row, carrierCol]
    verify = prepost[row, verifyCol]
    if (carrier == "voip"){
      if (verify == "scammer"){
        voipCounter = voipCounter + 1
      }
    }
    row = row + 1
  }
  paste(voipCounter/sum(prepost$status=="scammer")*100)
}

#prints out the % of landline carrier scammers
calLandlineScammer <- function(){
  row = 1
  counter = 0
  carrierCol = 6
  verifyCol = 14
  landlineCounter = 0
  while (row <= nrow(prepost)){
    carrier = prepost[row, carrierCol]
    verify = prepost[row, verifyCol]
    if (carrier == "landline"){
      if (verify == "scammer"){
        landlineCounter = landlineCounter + 1
      }
    }
    row = row + 1
  }
  paste(landlineCounter/sum(prepost$status=="scammer")*100)
}

##SCORING MECHANISM

carrierScammers['percentage'] = ""
carrierScammers[5,4] = calNaCarrierScammer()
carrierScammers[6,4] = calLandlineScammer()
carrierScammers[7,4] = calMobileScammer()
carrierScammers[8,4] = calVoipScammer()
carrierScammers['score'] = ""

#calculates score to be allocated for phone type based on % scammers
row = 5
score = 0
percentageCol = 4
scoreCol = 5
while (row <= nrow(carrierScammers)){
  percentage = as.numeric(carrierScammers[row, percentageCol])
  score = percentage/100*5
  carrierScammers[row,scoreCol] = round(score)
  row = row + 1
}

carrierScammers[1,1] = ""
carrierScammers[5,1] = ""

#remove non-scammers rows
carrierScammers = carrierScammers[-c(1,2,3,4),]

#update score & reason for carrier scammers
row = 1
carrierCol=6
statusCol=13
scoreCol=15
reasonCol=16
while (row <= nrow(merged)){
  if (merged[row, carrierCol] == ""){ #empty
      merged[row,scoreCol] = as.numeric(merged[row,scoreCol]) + as.numeric(carrierScammers[1,5])
      if (is.na(merged[row,reasonCol])){ #update reason
        merged[row,reasonCol] = "NA carrier"
      } else if (!is.na(merged[row,reasonCol])){
        merged[row,reasonCol] = paste(merged[row,reasonCol], ";", "NA carrier")
      }
  } else if (merged[row,carrierCol] == "landline"){
      merged[row,scoreCol] = as.numeric(merged[row,scoreCol]) + as.numeric(carrierScammers[2,5])
      if (is.na(merged[row,reasonCol])){ #update reason
        merged[row,reasonCol] = "landline"
      } else if (!is.na(merged[row,reasonCol])){
        merged[row,reasonCol] = paste(merged[row,reasonCol], ";", "landline")
      }
  } else if (merged[row,carrierCol] == "mobile"){
      merged[row,scoreCol] = as.numeric(merged[row,scoreCol]) + as.numeric(carrierScammers[3,5])
      if (is.na(merged[row,reasonCol])){ #update reason
        merged[row,reasonCol] = "mobile scammer"
      } else if (!is.na(merged[row,reasonCol])){
        merged[row,reasonCol] = paste(merged[row,reasonCol], ";", "mobile scammer")
      }
  } else if (merged[row,carrierCol] == "voip"){
      merged[row,scoreCol] = as.numeric(merged[row,scoreCol]) + as.numeric(carrierScammers[4,5])
      if (is.na(merged[row,reasonCol])){ #update reason
        merged[row,reasonCol] = "voip"
      } else if (!is.na(merged[row,reasonCol])){
        merged[row,reasonCol] = paste(merged[row,reasonCol], ";", "voip")
      }
  }
  row = row + 1
}

#update results
row = 1
results_col = 14
score_col = 15
while (row <= nrow(merged)){
  if (merged[row,score_col] >= 9.75){ #normalized: mean(as.numeric(carrierScammers$score)) = 1.25
    merged[row,results_col] = "scammer"
  } else {
    merged[row,results_col] = "non-scammer"
  }
  row = row + 1
}

library(RTextTools)

#generate confusion matrix
confusion_matrix = table(merged$status, merged$results)
confusion_matrix
#             non-scammer scammer
#non-scammer           1     377
#scammer               5     135

recall_accuracy(merged$status, merged$results)
#increase accuracy: 0.8494208