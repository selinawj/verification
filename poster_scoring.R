prepost["score"] <-0 #score starts with 0
prepost["reason"]<-NA

#2 Checking for poster type
row = 1
poster_col = 5
while (row <= nrow(prepost)){
  poster = prepost[row, poster_col]
  if (is.na(poster)){
    prepost[row,16] = prepost[row,16] - 4.1 #missing poster type
    if (is.na(prepost[row,17])){
      prepost[row,17] = "Missing poster type"
    } else if (!is.na(prepost[row,17])){
      prepost[row,17] = paste(prepost[row,17],"; Missing poster type") #append reason
    }
  }
  row = row + 1
}

#7 create listing URL column
prepost["URL"] <-NA
row = 1
URL_col = 18
listing_id_col = 3
while (row <= nrow(prepost)){
  if (!is.na(prepost[row, listing_id_col])){
    prepost[row, URL_col] = paste("https://www.renthop.com/listings/_/_/", prepost[row, listing_id_col], "/show", sep = '')
  }
  row = row + 1
}

#8 create a scammer column
prepost["scammer"] <- NA
row = 1
verify_status_col = 15
scammer_col = 19
while (row <= nrow(prepost)){
  status = as.character(prepost[row, verify_status_col])
  if (grepl("-1", status)) {
    prepost[row, scammer_col] = "scammer"
  } else {
    prepost[row, scammer_col] = "please verify"
  }
  row = row + 1
}

#9 create a results column
prepost["results"] <- NA
row = 1
score_col = 16
results_col = 20
while (row <= nrow(prepost)) {
  score = as.character(prepost[row, score_col])
  if (as.numeric(score) <= -4.1){
    prepost[row,results_col] = "scammer"
  } else {
    prepost[row,results_col] = "please verify"
  }
  row = row + 1
}

#if (as.numeric(score) < 0)

#total number of scammers
total = 0
row = 1
scammer_col = 19
while (row <= nrow(prepost)){
  if (grepl("scammer",prepost[row,scammer_col])){
    total = total + 1
  }
  row = row + 1
}

library(RTextTools)

#generate confusion matrix
confusion_matrix = table(prepost[,19], prepost[,20])
confusion_matrix
#                please verify scammer
#please verify           325     751
#scammer                 102     244

total

recall_accuracy(prepost[,scammer_col], prepost[,results_col])
#0.4001406

#Conclusion: threshold 4 - scammer: 244/346 = 70.52%, please verify: 325/1076 = 30.204%