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

#2 Checking for poster type
row = 1
poster_col = 5
while (row <= nrow(prepost)){
  poster = prepost[row, poster_col]
  if (is.na(poster)){
    prepost[row,16] = prepost[row,16] - 5 #missing poster type
    if (is.na(prepost[row,17])){
      prepost[row,17] = "Missing poster type"
    } else if (!is.na(prepost[row,17])){
      prepost[row,17] = paste(prepost[row,17],"; Missing poster type") #append reason
    }
  }
  row = row + 1
}


#3 Checking for agent's license
row = 1
agent_col = 5 #agent
license_col = 6 #license
while (row <= nrow(prepost)){
  poster = as.character(prepost[row,agent_col])
  if (!is.na(poster)){
    if (poster == "agent") {
      if (is.na(prepost[row,license_col])){
        prepost[row,16] = prepost[row,16] - 1 #missing license
        if (is.na(prepost[row,17])){
          prepost[row,17] = "Missing agent license" #append reason
        } else if (!is.na(prepost[row,16])){
          prepost[row,17] = paste(prepost[row,17], ";Missing agent license") #append reason
        }
      }
    }
  }
  row = row + 1
}

#4 Comparing posting_state vs listing_state (specific cases first - New York vs Manhattan/Brooklyn/NY)
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

#5 Checking for phone no & carrier

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

#check for landline
row = 1
phone_col = 9 #phone
carrier_col = 10 #carrier
while (row <= nrow(prepost)){
  phone = as.character(prepost[row,phone_col])
  carrier = as.character(prepost[row,carrier_col])
  if (!is.na(phone)){ #given phone no.
    if (!is.na(carrier)) {
      if (grepl(carrier,"landline", ignore.case = T)){
          prepost[row,16] = prepost[row,16] - 1.15 #landline
          if (!is.na(prepost[row,17])){
            prepost[row,17] = paste(prepost[row,17], "; Landline")
          } else if (is.na(prepost[row,17])){
            prepost[row,17] = "Landline"
          }
      }
    }
  }
  row = row + 1
}

#check if no phone no.
row = 1
phone_col = 9 #phone
carrier_col = 10 #carrier
while (row <= nrow(prepost)){
  phone = as.character(prepost[row,phone_col])
  carrier = as.character(prepost[row,carrier_col])
  if (is.na(phone)){ #no phone no.
    prepost[row,16] = prepost[row,16] - 5
    if (!is.na(prepost[row,17])){
      prepost[row,17] = paste(prepost[row,17], "; No phone no.")
    } else if (is.na(prepost[row,17])){
      prepost[row,17] = "No phone no."
    }
  }
  row = row + 1
}

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

#check for empty pricing
row = 1
price_col = 13
type_col = 1
while (row <= nrow(prepost)){
  type = as.character(prepost[row, type_col])
  price = as.character(prepost[row,price_col])
  if (grepl("verify_request", type)){
    if (!is.na(price)){ #given price
      if (price == "0"){
        prepost[row,16] = prepost[row,16] - 4.3 #0 price
        if (!is.na(prepost[row,17])){
          prepost[row,17] = paste(prepost[row,17], "; No price")
        } else if (is.na(prepost[row,17])){
          prepost[row,17] = "No Price"
        }
      } else if (price == "1"){
          prepost[row,16] = prepost[row,16] - 4.3 #1 price
        if (!is.na(prepost[row,17])){
          prepost[row,17] = paste(prepost[row,17], "; No price")
        } else if (is.na(prepost[row,17])){
          prepost[row,17] = "No price"
        }
      }
    } else if (is.na(price)){
        prepost[row,16] = prepost[row,16] - 2.3 #NA price
      if (is.na(prepost[row,17])){
        prepost[row,17] = "No price"
      } else if (!is.na(prepost[row,17])){
        prepost[row,17] = paste(prepost[row,17], "; No price")
      }
    }
  }
  row = row + 1
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
  if (as.numeric(score) <= -4){
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
#FOR NA POSTER -5 (-4.1)
#                please verify scammer
#please verify           321     755
#scammer                  95     251

#FOR NA PHONE -5 (-1.6)
#                please verify scammer
#please verify           249     827
#scammer                  76     270

total

recall_accuracy(prepost[,scammer_col], prepost[,results_col])
#0.3649789

write.csv(prepost, "prepost.csv")