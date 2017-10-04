#set a dataframe for license
license = data.frame(matrix(nrow = 4, ncol = 5))
colnames(license) = c("license","status", "count", "percentage", "score")

i <- sapply(license, is.factor)
license[i] <- lapply(license[i], as.character)

license[c(1,2),1] = "license"
license[c(3,4),1] = NA
license[c(1,3),2] = "scammers"
license[c(2,4),2] = "non-scammers"

#count how many scammers is present in each domain
licenseScammers = data.frame(table(prepost$license, prepost$poster == "agent", prepost$status=='scammer'))
colnames(licenseScammers) = c('license', 'agent', 'scammer_status', 'count')

#remove False agent & TRUE scammers rows
licenseScammers = licenseScammers[licenseScammers$agent!="FALSE" & licenseScammers$scammer_status!="TRUE",]

#insert with non-scammer agents with license into df
license[2,3] = sum(licenseScammers$count)

#count how many scammers is present in each domain
licenseScammers = data.frame(table(prepost$license, prepost$poster == "agent", prepost$status=='scammer'))
colnames(licenseScammers) = c('license', 'agent', 'scammer_status', 'count')

#remove False agent & scammers rows
licenseScammers = licenseScammers[licenseScammers$agent!="FALSE" & licenseScammers$scammer_status!="FALSE",]

#remove agent & scammer_status columns
licenseScammers[,c("agent", "scammer_status")] = NULL

#no license, cannot be a non-scammer
license[4,3] = 0

#calculate number of agent license scammers & non-scammers
row = 1
counter = 0
while (row <= nrow(licenseScammers)){
  if (licenseScammers[row,1] == "" & licenseScammers[row,2] > 0){
    license[3,3] = licenseScammers[row,2]
  } else {
    counter = counter + 1
  }
  row = row + 1
  license[1,3] = counter
}

##GG PLOTS

library(ggplot2)

#plot of how many scammers in each license type category
licenseScammersPlot = ggplot(license) + geom_bar(aes(x = reorder(license, -count), y = count, fill = status, group = status), stat = "identity", position = 'dodge') + geom_text(aes(x = reorder(license, -count), y = count, label = count, group = status), position = position_dodge(width = 1), vjust = -0.5)
licenseScammersPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Scammers based on Agent License", y = "Count", x = "License Provided", fill = "Status")

#plot of how many license type in each status
licensePlot = ggplot(license, aes(x = reorder(status, -count), y = count, fill = license)) + geom_bar(stat = "identity", position = 'dodge') + geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5)
licensePlot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Num of Agent License Type within each scammer status", y = "Count", x = "Status", fill = "License")

#function to calculate how many scammers of each license type
row = 1
licenseCol = 5
statusCol = 11
licenseTotal = 0

licenseScam = 0
licenseNonScam = 0

naTotal = 0
naScam = 0
naNonScam = 0

while (row <= nrow(prepost)){
  if (prepost[row, licenseCol] == "") {
    naTotal = naTotal + 1
    if (prepost[row,statusCol] == "scammer"){
      naScam = naScam + 1
    } else if (prepost[row,statusCol] == "non-scammer"){
      naNonScam = naNonScam + 1
    }
  } else if (prepost[row, licenseCol] != ""){
    licenseTotal = licenseTotal + 1
    if (prepost[row,statusCol] == "scammer"){
      licenseScam = licenseScam + 1
    } else if (prepost[row,statusCol] == "non-scammer"){
      licenseNonScam = licenseNonScam + 1
    }
  }
  row = row + 1
  license[1,4] = licenseScam/licenseTotal*100
  license[2,4] = licenseNonScam/licenseTotal*100
  license[3,4] = naScam/naTotal*100
  license[4,4] = naNonScam/naTotal*100
}

#calculates score to be allocated for license based on % scammers
row = 1
score = 0
percentageCol = 4
scoreCol = 5
while (row <= nrow(license)){
  percentage = as.numeric(license[row, percentageCol])
  score = percentage/100*5
  license[row,scoreCol] = round(score)
  row = row + 1
}

#update score & reason for license scammers
row = 1
licenseCol=5
statusCol=11
scoreCol=12
reasonCol=14
while (row <= nrow(merged)){
  if (merged[row, licenseCol] == ""){
    if (merged[row, statusCol] == "scammer"){
      merged[row,scoreCol] = as.numeric(merged[row,scoreCol]) + as.numeric(license[3,3])
      if (is.na(merged[row,reasonCol])){ #update reason
        merged[row,reasonCol] = "missing license no."
      } else if (!is.na(merged[row,reasonCol])){
        merged[row,reasonCol] = paste(merged[row,reasonCol], ";", "missing license no.")
      }
    }
  } else if (merged[row,licenseCol] != ""){
    if (merged[row, statusCol] == "scammer"){
      merged[row,scoreCol] = as.numeric(merged[row,scoreCol]) + as.numeric(license[1,3])
      if (is.na(merged[row,reasonCol])){ #update reason
        merged[row,reasonCol] = "license scammer"
      } else if (!is.na(merged[row,reasonCol])){
        merged[row,reasonCol] = paste(merged[row,reasonCol], ";", "license scammer")
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
#              non-scammer scammer
#non-scammer         300      78
#scammer               0     140

recall_accuracy(merged$status, merged$results)
#0.8494208