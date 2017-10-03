##GG PLOTS
library(ggplot2)

#dataframe of how many each phone numbers are present
phoneTable = data.frame(prepost$phone_num)
colnames(phoneTable) = c('phoneNum')

#new dataframe counting NA phone & phone
phoneNaTable = data.frame(matrix(NA, nrow=2, ncol=2))
colnames(phoneNaTable) = c("type","count")
phoneNaTable[1,1] = "naPhone"
phoneNaTable[2,1] = 'phone'

#function to calculate number of NA phone num
row = 1
naCount = 0
count = 0
while (row <= nrow(phoneTable)) {
  if (is.na(phoneTable[row, 1])){
    naCount = naCount + 1
  } else {
    count = count + 1
  }
  row = row + 1
  phoneNaTable[1,2] = naCount
  phoneNaTable[2,2] = count
}

#ggplot of total number of each phone type
phonePlot = ggplot(phoneNaTable) + geom_bar(aes(x = type, y = count), stat = "identity") + geom_text(aes(x = type, y = count, label = count))
phonePlot + labs(title = "Number of NA phone vs phone", y = "Count", x = "Type")

#dataframe of how many scammers is present in each type
phoneScammersTable = data.frame(phoneTable$phoneNum, prepost$status)
colnames(phoneScammersTable) = c('type', 'status')

#new dataframe counting NA phone & phone
phoneScammers = data.frame(matrix(NA, nrow=4, ncol=3))
colnames(phoneScammers) = c("type", "status", "count")
phoneScammers[c(1,3),1] = "naPhone"
phoneScammers[c(2,4),1] = "phone"
phoneScammers[c(1,2),2] = "scammers"
phoneScammers[c(3,4),2] = "non-scammers"

#function to calculate how many scammers of each type
row = 1
naScam = 0
naNonScam = 0
phoneScam = 0
phoneNonScam = 0
while (row <= nrow(phoneScammersTable)){
  if (is.na(phoneScammersTable[row, 1])) {
    if (phoneScammersTable[row,2] == "scammer"){
      naScam = naScam + 1
    } else if (phoneScammersTable[row,2] == "non-scammer"){
      naNonScam = naNonScam + 1
    }
  } else if (!is.na(phoneScammersTable[row, 1])){
      if (phoneScammersTable[row,2] == "scammer"){
        phoneScam = phoneScam + 1
      } else if (phoneScammersTable[row,2] == "non-scammer"){
        phoneNonScam = phoneNonScam + 1
      }
  }
  row = row + 1
  phoneScammers[1,3] = naScam
  phoneScammers[2,3] = phoneScam
  phoneScammers[3,3] = naNonScam
  phoneScammers[4,3] = phoneNonScam
}

#plot of how many scammers in each phone category
phoneScammersPlot = ggplot(phoneScammers) + geom_bar(aes(x = reorder(type, -count), y = count, fill = status, group = status), stat = "identity", position = 'dodge') + geom_text(aes(x = reorder(type, -count), y = count, label = count, group = status), position = position_dodge(width = 1), vjust = -0.5)
phoneScammersPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Scammers based on Phone Type", y = "Count", x = "Phone Type", fill = "Status")

#plot of how many posters in each status
scammersPlot = ggplot(phoneScammers, aes(x = reorder(status, -count), y = count, fill = type)) + geom_bar(stat = "identity", position = 'dodge') + geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5)
scammersPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Num of Phone Type of each scammer status", y = "Count", x = "Status", fill = "Type")

##FUNCTIONS

#prints out the % of na phone scammers
calNaPhoneScammer <- function(){
  row = 1
  counter = 0
  phoneCol = 7
  verifyCol = 11
  total = 0
  while (row <= nrow(prepost)){
    phone = prepost[row, phoneCol]
    verify = prepost[row, verifyCol]
    if (is.na(phone)){ #NA
      total = total + 1
      if (verify == "scammer"){ #Scammers
        counter = counter + 1
      }
    }
    row = row + 1
  }
  paste(counter/total*100)
}

#prints out the % of phone scammers
calPhoneScammer <- function(){
  row = 1
  counter = 0
  phoneCol = 7
  verifyCol = 11
  total = 0
  while (row <= nrow(prepost)){
    phone = prepost[row, phoneCol]
    verify = prepost[row, verifyCol]
    if (!is.na(phone)){ #NA
      total = total + 1
      if (verify == "scammer"){ #Scammers
        counter = counter + 1
      }
    }
    row = row + 1
  }
  paste(counter/total*100)
}

##SCORING MECHANISM

phoneScammers['percentage'] = ""
phoneScammers[1,4] = calNaPhoneScammer()
phoneScammers[2,4] = calPhoneScammer()
phoneScammers['score'] = ""

#calculates score to be allocated for phone type based on % scammers
row = 1
score = 0
percentageCol = 4
scoreCol = 5
while (row <= 2){
  percentage = as.numeric(phoneScammers[row, percentageCol])
  score = percentage/100*5
  phoneScammers[row,scoreCol] = round(score)
  row = row + 1
}

#create phone type column in merged
merged['phone']<-NA

#extract phone type of each usernames into col
row = 1
phoneCol = 15
while (row <= nrow(merged)){
  if (is.na(merged[row,7])){
    merged[row,phoneCol] = "naPhone"
  } else if (!is.na(merged[row,7])){
    merged[row,phoneCol] = "phone"
  }
  row = row + 1
}

#update score & reason for phone scammers
row = 1
statusCol=11
scoreCol=12
reasonCol=14
while (row <= nrow(merged)){
  if (merged[row, phoneCol] == "naPhone"){
    if (merged[row, statusCol] == "scammer"){
      merged[row,scoreCol] = as.numeric(merged[row,scoreCol]) + as.numeric(phoneScammers[1,5])
      if (is.na(merged[row,reasonCol])){ #update reason
        merged[row,reasonCol] = "missing phone no."
      } else if (!is.na(merged[row,reasonCol])){
        merged[row,reasonCol] = paste(merged[row,reasonCol], ";", "missing phone no.")
      }
    }
  } else if (merged[row,phoneCol] == "phone"){
    if (merged[row, statusCol] == "scammer"){
      merged[row,scoreCol] = as.numeric(merged[row,scoreCol]) + as.numeric(phoneScammers[2,5])
      if (is.na(merged[row,reasonCol])){ #update reason
        merged[row,reasonCol] = "missing phone no."
      } else if (!is.na(merged[row,reasonCol])){
        merged[row,reasonCol] = paste(merged[row,reasonCol], ";", "missing phone no.")
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
  if (merged[row,score_col] >= 2){ #normalized: mean(phoneScammers$score) = 2
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
#                please verify scammer
#please verify           250     826
#scammer                  82     264

recall_accuracy(merged$status, merged$results)
#increase accuracy: 0.8494208

#rearrange columns in merged df
merged = merged[,c("domain", "account_id", "username", "poster", "license", "carrier", "phone_num", "phone", "citystate", "posting_state", "price", "status", "score", "results", "reason")]
# #Conclusion: threshold 1.6 - scammer: 264/346 = 76.3%, please verify: 250/1076 = 23.23%