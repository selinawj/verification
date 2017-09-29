prepost["score"] <-0 #score starts with 0
prepost["reason"]<-NA

#check for voip
row = 1
phone_col = 9 #phone
carrier_col = 10 #carrier
while (row <= nrow(prepost)){
  phone = as.character(prepost[row,phone_col])
  carrier = as.character(prepost[row,carrier_col])
  if (!is.na(phone)){ #given phone no.
    if (!is.na(carrier)) {
      if (grepl(carrier,"voip", ignore.case = T)){
        prepost[row,16] = prepost[row,16] - 2.9 #VOIP
        if (!is.na(prepost[row,17])){
          prepost[row,17] = paste(prepost[row,17], "; VOIP")
        } else if (is.na(prepost[row,17])){
          prepost[row,17] = "VOIP"
        }
      }
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
  if (as.numeric(score) <= -2.9){
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
#please verify          1063      13
#scammer                 328      18

total

recall_accuracy(prepost[,scammer_col], prepost[,results_col])
#0.7601969

#Conclusion: threshold 2.9 - scammer: 18/346 = 5.20%, please verify: 1063/1076 = 98.79%