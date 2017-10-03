##GG PLOTS
library(ggplot2)

#dataframe of how many each phone numbers are present
carrierTable = data.frame(table(prepost$carrier))
colnames(carrierTable) = c('type','count')

#uniformize from factors to characters format
i <- sapply(carrierTable, is.factor)
carrierTable[i] <- lapply(carrierTable[i], as.character)
carrierTable[1,1] = "NA"

#ggplot of total number of each carrier type
carrierPlot = ggplot(carrierTable, aes(x = reorder(type, -count), y = count)) + geom_bar(stat = "identity") + geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5)
carrierPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Number of each type of carrier", y = "Count", x = "Carrier")

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
carrierScammersPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Scammers based on Carrier Type", y = "Count", x = "Carrier Type", fill = "Status")

#plot of how many posters in each status
scammersPlot = ggplot(carrierScammers, aes(x = reorder(status, -count), y = count, fill = carrier)) + geom_bar(stat = "identity", position = 'dodge') + geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5)
scammersPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Num of Carrier Type within each scammer status", y = "Count", x = "Status", fill = "Carrier")

##FUNCTIONS

#prints out the % of NA carrier scammers
calNaCarrierScammer <- function(){
  row = 1
  counter = 0
  carrierCol = 6
  verifyCol = 11
  naCounter = 0
  naTotal = 0
  while (row <= nrow(prepost)){
    carrier = prepost[row, carrierCol]
    verify = prepost[row, verifyCol]
    if (carrier == ""){ #NA
      naTotal = naTotal + 1
      print (naTotal)
      if (verify == "scammer"){
        naCounter = naCounter + 1
      }
    }
    row = row + 1
  }
  paste(naCounter/naTotal*100)
}

#prints out the % of mobile carrier scammers
calMobileScammer <- function(){
  row = 1
  counter = 0
  carrierCol = 6
  verifyCol = 11
  mobileCounter = 0
  mobileTotal = 0
  while (row <= nrow(prepost)){
    carrier = prepost[row, carrierCol]
    verify = prepost[row, verifyCol]
    if (carrier == "mobile"){
      mobileTotal = mobileTotal + 1
      if (verify == "scammer"){
        mobileCounter = mobileCounter + 1
      }
    }
    row = row + 1
  }
  paste(mobileCounter/mobileTotal*100)
}

#prints out the % of voip carrier scammers
calVoipScammer <- function(){
  row = 1
  counter = 0
  carrierCol = 6
  verifyCol = 11
  voipCounter = 0
  voipTotal = 0
  while (row <= nrow(prepost)){
    carrier = prepost[row, carrierCol]
    verify = prepost[row, verifyCol]
    if (carrier == "voip"){
      voipTotal = voipTotal + 1
      if (verify == "scammer"){
        voipCounter = voipCounter + 1
      }
    }
    row = row + 1
  }
  paste(voipCounter/voipTotal*100)
}

#prints out the % of landline carrier scammers
calLandlineScammer <- function(){
  row = 1
  counter = 0
  carrierCol = 6
  verifyCol = 11
  landlineCounter = 0
  landlineTotal = 0
  while (row <= nrow(prepost)){
    carrier = prepost[row, carrierCol]
    verify = prepost[row, verifyCol]
    if (carrier == "landline"){
      landlineTotal = landlineTotal + 1
      if (verify == "scammer"){
        landlineCounter = landlineCounter + 1
      }
    }
    row = row + 1
  }
  paste(landlineCounter/landlineTotal*100)
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

#assign score to domain scammers
carrierScammers = carrierScammers[-c(1,2,3,4),]

#update score & reason for carrier scammers
row = 1
carrierCol=6
statusCol=11
scoreCol=12
reasonCol=14
while (row <= nrow(merged)){
  if (merged[row, carrierCol] == ""){ #empty
    if (merged[row, statusCol] == "scammer"){
      merged[row,scoreCol] = as.numeric(merged[row,scoreCol]) + as.numeric(carrierScammers[5,5])
      if (is.na(merged[row,reasonCol])){ #update reason
        merged[row,reasonCol] = "NA carrier"
      } else if (!is.na(merged[row,reasonCol])){
        merged[row,reasonCol] = paste(merged[row,reasonCol], ";", "NA carrier")
      }
    }
  } else if (merged[row,carrierCol] == "landline"){
    if (merged[row, statusCol] == "scammer"){
      merged[row,scoreCol] = as.numeric(merged[row,scoreCol]) + as.numeric(carrierScammers[6,5])
      if (is.na(merged[row,reasonCol])){ #update reason
        merged[row,reasonCol] = "landline"
      } else if (!is.na(merged[row,reasonCol])){
        merged[row,reasonCol] = paste(merged[row,reasonCol], ";", "landline")
      }
    }
  } else if (merged[row,carrierCol] == "mobile"){
    if (merged[row, statusCol] == "scammer"){
      merged[row,scoreCol] = as.numeric(merged[row,scoreCol]) + as.numeric(carrierScammers[7,5])
      if (is.na(merged[row,reasonCol])){ #update reason
        merged[row,reasonCol] = "mobile scammer"
      } else if (!is.na(merged[row,reasonCol])){
        merged[row,reasonCol] = paste(merged[row,reasonCol], ";", "mobile scammer")
      }
    }
  } else if (merged[row,carrierCol] == "voip"){
    if (merged[row, statusCol] == "scammer"){
      merged[row,scoreCol] = as.numeric(merged[row,scoreCol]) + as.numeric(carrierScammers[8,5])
      if (is.na(merged[row,reasonCol])){ #update reason
        merged[row,reasonCol] = "voip"
      } else if (!is.na(merged[row,reasonCol])){
        merged[row,reasonCol] = paste(merged[row,reasonCol], ";", "voip")
      }
    }
  }
  row = row + 1
}

#update results
row = 1
results_col = 13
score_col = 12
while (row <= nrow(merged)){
  if (merged[row,score_col] >= 2){ #normalized: mean(as.numeric(carrierScammers$score)) = 2.25
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
#              non-scammer scammer
#non-scammer         300      78
#scammer               0     140

recall_accuracy(merged$status, merged$results)
#increase accuracy: 0.8494208