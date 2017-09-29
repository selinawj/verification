prepost["score"] <-0 #score starts with 0
prepost["reason"]<-NA

#1 Checking for email domain for Scammers
row = 1
email_col = 4
while (row <= nrow(prepost)){
  domain = as.character(prepost[row, email_col])
  if (grepl("@yahoo.uk", domain)) {
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,17] = "Likely scammer: @yahoo.uk domain"
  } else if (grepl("@oficina-molina.com", domain)) {
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,17] = "Likely scammer: @oficina-molina.com domain"
  } else if (grepl("@zoho.com", domain)) {
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,17] = "Likely scammer: @zoho.com domain"
  } else if (grepl("@outlook.com", domain)){
    prepost[row,16] = prepost[row,16] - 2.4 #likely scammer
    prepost[row,17] = "Likely scammer: @outlook.com domain"
  } else if (grepl("@dr.com", domain)){
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,17] = "Likely scammer: @dr.com domain"
  } else if (grepl("@doctor.com", domain)){
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,17] = "Likely scammer: @doctor.com domain"
  } else if (grepl("@gmx.com", domain)){
    prepost[row,16] = prepost[row,16] - 1.6 #likely scammer
    prepost[row,17] = "Likely scammer: @gmx.com domain"
  } else if (grepl("@yandex.com", domain)){
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,17] = "Likely scammer: @yandex.com domain"
  } else if (grepl("@att.blackberry.net", domain)){
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,17] = "Likely scammer: @att.blackberry.net domain"
  } else if (grepl("@dnsdeer.com", domain)){
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,17] = "Likely scammer: @dnsdeer.com domain"
  } else if (grepl("@me.com", domain)){
    prepost[row,16] = prepost[row,16] - 1.5 #likely scammer
    prepost[row,17] = "Likely scammer: @me.com domain"
  } else if (grepl("@juno.com", domain)){
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,17] = "Likely scammer: @juno.com domain"
  } else if (grepl("@tutanota.com", domain)){
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,17] = "Likely scammer: @tutanota.com domain"
  } else if (grepl("@rep1baseball.com", domain)){
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,17] = "Likely scammer: @rep1baseball.com domain"
  } else if (grepl("@stayawhile.com", domain)){
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,17] = "Likely scammer: @stayawhile.com domain"
  } else if (grepl("@citydwellers.nyc", domain)){
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,17] = "Likely scammer: @citydwellers.nyc domain"
  } else if (grepl("@mypacks.net", domain)){
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,17] = "Likely scammer: @mypacks.net domain"
  } else if (grepl("@subzero.com", domain)){
    prepost[row,16] = prepost[row,16] - 5 #likely scammer
    prepost[row,16] = "Likely scammer: @subzero.com domain"
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
  if (as.numeric(score) <= -5){
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
#please verify          1076       0
#scammer                 340       6

total

recall_accuracy(prepost[,results_col],prepost[,scammer_col])
#0.7609001

#Conclusion: Doesn't seem to predict much