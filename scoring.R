#SCORING BASED ON PHONE, AGENT LICENSE, CARRIER, LOCATION & PRICE

prepost['score']=0
prepost['reason']=NA
prepost['result']=""

##PHONE SCORING

finalPhoneScammersTable['score'] = 0

#calculates score to be allocated for phone type based on % scammers
row = 1
score = 0
percentageCol = 5
scoreCol = 6
while (row <= 2){
  percentage = as.numeric(finalPhoneScammersTable[row, percentageCol])
  score = percentage/100*5
  finalPhoneScammersTable[row,scoreCol] = round(score)
  row = row + 1
}

prepost['hasPhone'] = ""
prepost$hasPhone[!is.na(prepost$phone_num)]="phone"
prepost$hasPhone[is.na(prepost$phone_num)]="naPhone"

#update score & reason for phone scammers
prepost$score[prepost$hasPhone=="naPhone"]=finalPhoneScammersTable[1,6]
prepost$reason[prepost$hasPhone=="naPhone"]="Missing phone no."

#update results
prepost$result[prepost$score == "3"]="scammer"
prepost$result[prepost$score == "0"]="non-scammer"

library(RTextTools)

#generate confusion matrix
confusion_matrix = table(prepost$status, prepost$result)
confusion_matrix
#              non-scammer scammer
#non-scammer         465       7
#scammer             124      16

recall_accuracy(prepost$status, prepost$result)
#0.7859477

#CARRIER

finalCarrierTable['score'] = 0

#calculates score to be allocated for phone type based on % scammers
row = 1
score = 0
percentageCol = 5
scoreCol = 6
while (row <= nrow(finalCarrierTable)){
  percentage = as.numeric(finalCarrierTable[row, percentageCol])
  score = percentage/100*5
  finalCarrierTable[row,scoreCol] = round(score)
  row = row + 1
}

finalCarrierTable$carrier[finalCarrierTable$carrier=="NA"]=""

#update score & reason for carrier scammers
row = 1
carrierCol=6
statusCol=14
scoreCol=16
reasonCol=17
while (row <= nrow(prepost)){
  if (prepost[row, carrierCol] == ""){ #empty
    prepost[row,scoreCol] = as.numeric(prepost[row,scoreCol]) + as.numeric(finalCarrierTable[3,6])
    if (is.na(prepost[row,reasonCol])){ #update reason
      prepost[row,reasonCol] = "NA carrier"
    } else if (!is.na(prepost[row,reasonCol])){
      prepost[row,reasonCol] = paste(prepost[row,reasonCol], ";", "NA carrier")
    }
  } else if (prepost[row,carrierCol] == "voip"){
    prepost[row,scoreCol] = as.numeric(prepost[row,scoreCol]) + as.numeric(finalCarrierTable[4,6])
    if (is.na(prepost[row,reasonCol])){ #update reason
      prepost[row,reasonCol] = "voip"
    } else if (!is.na(prepost[row,reasonCol])){
      prepost[row,reasonCol] = paste(prepost[row,reasonCol], ";", "voip")
    }
  }
  row = row + 1
}

#update results
prepost$result[prepost$score >= "3"]="scammer"
prepost$result[prepost$score == "0"]="non-scammer"

#generate confusion matrix
confusion_matrix = table(prepost$status, prepost$result)
confusion_matrix
#              non-scammer scammer
#non-scammer         442      30
#scammer              77      63

recall_accuracy(prepost$status, prepost$result)
#increased accuracy: 0.8251634

#LICENSE SCORING

finalLicenseTable['score'] = 0

#calculates score to be allocated for license based on % scammers
row = 1
score = 0
percentageCol = 5
scoreCol = 6
while (row <= nrow(finalLicenseTable)){
  percentage = as.numeric(finalLicenseTable[row, percentageCol])
  score = percentage/100*5
  finalLicenseTable[row,scoreCol] = round(score)
  row = row + 1
}

#update score & reason for license scammers
row = 1
posterCol=4
licenseCol=5
scoreCol=16
reasonCol=17
while (row <= nrow(prepost)){
  if (prepost[row, posterCol] == "agent"){
    if (prepost[row, licenseCol] == ""){
      prepost[row,scoreCol] = as.numeric(prepost[row,scoreCol]) + as.numeric(finalLicenseTable[2,6])
      if (is.na(prepost[row,reasonCol])){ #update reason
        prepost[row,reasonCol] = "Missing license no."
      } else if (!is.na(prepost[row,reasonCol])){
        prepost[row,reasonCol] = paste(prepost[row,reasonCol], ";", "missing license no.")
      }
    }
  }
  row = row + 1
}

#update results
prepost$result[prepost$score >= "3"]="scammer"
prepost$result[prepost$score == "0"]="non-scammer"

#generate confusion matrix
confusion_matrix = table(prepost$status, prepost$result)
confusion_matrix
#              non-scammer scammer
#non-scammer         432      40
#scammer              67      73

recall_accuracy(prepost$status, prepost$result)
#0.8251634

#DISTANCE SCORING

locationQuant['score'] = 0

#calculates score to be allocated for location based on % scammers
row = 1
score = 0
percentageCol = 5
scoreCol = 6
while (row <= nrow(locationQuant)){
  percentage = as.numeric(locationQuant[row, percentageCol])
  score = percentage/100*5
  locationQuant[row,scoreCol] = round(score)
  row = row + 1
}

require(data.table)
setDT(prepost)[setDT(locationTable), bucket := i.bucket, on=c("account_id")]

#update score & reason for location scammers
row = 1
scoreCol=16
reasonCol=17
while (row <= nrow(prepost)){
  if (prepost$bucket[row] == "4"){
      prepost$score[row] = as.numeric(prepost$score[row]) + as.numeric(locationQuant[4,6])
      if (is.na(prepost$reason[row])){ #update reason
        prepost$reason[row] = "Location in 4th Quantile"
      } else if (!is.na(prepost$reason[row])){
        prepost$reason[row] = paste(prepost$reason[row], ";", "location in 4th Quantile")
      }
  }
  row = row + 1
}

#update results
prepost$result[prepost$score >= "2"]="scammer"
prepost$result[prepost$score == "0"]="non-scammer"

#generate confusion matrix
confusion_matrix = table(prepost$status, prepost$result)
confusion_matrix
#              non-scammer scammer
#non-scammer         347     125
#scammer              37     103

recall_accuracy(prepost$status, prepost$result)
#0.7352941

#PRICE SCORING

priceQuant['score'] = 0

#calculates score to be allocated for price based on % scammers
row = 1
score = 0
percentageCol = 5
scoreCol = 6
while (row <= nrow(priceQuant)){
  percentage = as.numeric(priceQuant[row, percentageCol])
  score = percentage/100*5
  priceQuant[row,scoreCol] = round(score)
  row = row + 1
}

setDT(prepost)[setDT(priceTable), priceBucket := i.priceBucket, on=c("account_id")]

#update score & reason for location scammers
row = 1
scoreCol=16
reasonCol=17
while (row <= nrow(prepost)){
  if (prepost$priceBucket[row] == "1"){
    prepost$score[row] = as.numeric(prepost$score[row]) + as.numeric(priceQuant[1,6])
    if (is.na(prepost$reason[row])){ #update reason
      prepost$reason[row] = "Price in 1st Quantile"
    } else if (!is.na(prepost$reason[row])){
      prepost$reason[row] = paste(prepost$reason[row], ";", "Price in 1st Quantile")
    }
  }
  row = row + 1
}

#update results
prepost$result[prepost$score >= "2"]="scammer"
prepost$result[prepost$score == "0"]="non-scammer"

#generate confusion matrix
confusion_matrix = table(prepost$status, prepost$result)
confusion_matrix
#             non-scammer scammer
#non-scammer         272     200
#scammer              29     111

recall_accuracy(prepost$status, prepost$result)
#0.625817