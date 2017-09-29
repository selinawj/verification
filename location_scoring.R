prepost["score"] <-0 #score starts with 0
prepost["reason"]<-NA

row = 1
posting_col = 7 #posting
listing_col = 8 #listing
while (row <= nrow(prepost)){
  posting = as.character(prepost[row,posting_col])
  listing = as.character(prepost[row,listing_col])
  if (!is.na(posting)){
    if (!is.na(listing)){
      if ((grepl("New York", posting, ignore.case = T)) & (grepl("NY", listing, ignore.case = T))) {
      } else if ((grepl("New York", posting, ignore.case = T)) & (grepl("Manhattan", listing, ignore.case = T))) {
      } else if ((grepl("New York", posting, ignore.case = T)) & (grepl("Brooklyn", listing, ignore.case = T))) {
      } else if ((grepl("Brooklyn", posting, ignore.case = T)) & (grepl("New York", listing, ignore.case = T))){
      } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("New York", listing, ignore.case = T))){
      } else if ((grepl("Jamaica", posting, ignore.case = T)) & (grepl("NY", listing, ignore.case = T))){
      } else if ((grepl("Ozone Park", posting, ignore.case = T)) & (grepl("New York", listing, ignore.case = T))){
      } else if ((grepl("Dodgewood", posting, ignore.case = T)) & (grepl("New York", listing, ignore.case = T))){
      } else if ((grepl("Elmhurst", posting, ignore.case = T)) & (grepl("New York", listing, ignore.case = T))){
      } else if ((grepl("Milford", posting, ignore.case = T)) & (grepl("NY", listing, ignore.case = T))){
      } else if ((grepl("Mt. Pleasant", posting, ignore.case = T)) & (grepl("New York", listing, ignore.case = T))){
      } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("New York", listing, ignore.case = T))){
      } else if (grepl(posting, listing, ignore.case = T)) {
      } else if (!grepl(posting, listing, ignore.case = T)){
        prepost[row,16] = prepost[row,16] - 3.3 #location differs
        if (is.na(prepost[row,17])){
          prepost[row,17] = "Location differs"
        } else if (!is.na(prepost[row,17])){
          prepost[row,17] = paste(prepost[row,17], ";Location differs")
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
  if (as.numeric(score) <= -3.3){
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
#please verify          1075       1
#scammer                 344       2

total

recall_accuracy(prepost[,scammer_col], prepost[,results_col])
#0.757384

#Conclusion: threshold 3.3 - scammer: 2/346 = 0.578%, please verify: 1037/1076 = 96.37%