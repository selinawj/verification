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
phoneScammersPlot + labs(title = "Scammers based on Phone Type", y = "Count", x = "Phone Type", fill = "Status")

#plot of how many of each phone type in each status
scammersPlot = ggplot(phoneScammers, aes(x = reorder(status, -count), y = count, fill = type)) + geom_bar(stat = "identity", position = 'dodge') + geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5)
scammersPlot + labs(title = "Num of Phone Type of each scammer status", y = "Count", x = "Status", fill = "Type")

#merge table
phoneScammersTrue = phoneScammers[phoneScammers$status!="non-scammers",]
phoneScammersFalse = phoneScammers[phoneScammers$status!="scammers",]
finalPhoneScammersTable = merge(phoneScammersTrue, phoneScammersFalse, by.x="type", by.y="type")
finalPhoneScammersTable = finalPhoneScammersTable[,-c(2,4)]
finalPhoneScammersTable[c("total", "percentage")] = NA
colnames(finalPhoneScammersTable) = c("type", "scammers", "nonscammers", "total", "scammersPercentage")
finalPhoneScammersTable$total = with(finalPhoneScammersTable, scammers+nonscammers)
finalPhoneScammersTable$scammersPercentage = with(finalPhoneScammersTable, scammers/total*100)
