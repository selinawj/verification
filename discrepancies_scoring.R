prepost["score"] <-0 #score starts with 0
prepost["reason"]<-NA

#6 Checking for median price comparison to neighborhoods(listing_state)
price_neighborhood = read.csv("output.csv")

#append median price into median col if neighborhood exists -- based on listing_state only
listing_state = as.character(prepost$listing_state)
median_neighborhood = as.character(price_neighborhood$Neighborhood)
for (i in 1:nrow(price_neighborhood)){
  for(j in 1:nrow(prepost)){
    if (!is.na(listing_state[j])){
      if (grepl(median_neighborhood[i], listing_state[j], ignore.case = T)){
        median = as.character(price_neighborhood$Price[i])
        prepost[j,14] = median
      }
    }
  }
}


#check for price discrepancies
row = 1
price_col = 13
median_col = 14
while (row <= nrow(prepost)) {
  price = as.character(prepost[row,price_col])
  median = as.character(prepost[row,median_col])
  if (!is.na(price)) {
    if (!is.na(median)) {
      difference = as.numeric(median) - as.numeric(price)
      if (difference < 500) {
        #do nothing
      } else if (difference >= 500) {#threshold difference: cheaper than median $500
        prepost[row,16] = prepost[row,16] - 0.5
        if (!is.na(prepost[row,17])){
          prepost[row,17] = paste("Listing is $", difference, "cheaper than median")
        } else if (!is.na(prepost[row,17])){
          prepost[row,17] = paste(prepost[row,17], "; Listing is $", difference, "cheaper than median")
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
  if (as.numeric(score) <= -0.5){
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
#please verify          1052      24
#scammer                 343       3

total

recall_accuracy(prepost[,scammer_col], prepost[,results_col])
#0.7419128

#Conclusion: threshold 0.5 - scammer: 3/346 = 0.86%, please verify: 1052/1076 = 97.7%