##GG PLOTS
library(ggplot2)
library(geosphere)
library(ggmap)

#create new dataframe to store similarity in location
locationTable = data.frame(prepost$account_id, prepost$latitude, prepost$longitude, prepost$postingLatitude, prepost$postingLongitude)
locationTable[c("dist", "listLocation", "postLocation", "within")] = ""
colnames(locationTable) = c("id", "listLat", "listLong", "postLat", "postLong", "dist", "listLocation", "postLocation", "within")

#calculate dist(meters) from list to post
locationTable$listLat = as.numeric(as.character(locationTable$listLat))
locationTable$listLong = as.numeric(as.character(locationTable$listLong))
locationTable$postLat = as.numeric(locationTable$postLat)
locationTable$postLong = as.numeric(locationTable$postLong)
row = 1
listLatCol = 2
listLongCol = 3
postLatCol = 4
postLongCol = 5
distCol = 6
listLocationCol = 7
postLocationCol = 8
while (row <= nrow(locationTable)){
  if ((!is.na(locationTable[row,listLongCol])) & (!is.na(locationTable[row,listLatCol]))) {
    if ((!is.na(locationTable[row,postLatCol])) & (!is.na(locationTable[row,postLongCol]))){
      locationTable[row, distCol] = distm(c(locationTable[row,listLongCol], locationTable[row,listLatCol]), 
               c(locationTable[row,postLongCol], locationTable[row,postLatCol]), fun = distHaversine)
  locationTable[row, listLocationCol] = revgeocode(c(locationTable[row, listLongCol], locationTable[row, listLatCol]))
  locationTable[row, postLocationCol] = revgeocode(c(locationTable[row, postLongCol], locationTable[row, postLatCol]))
    } else if ((is.na(locationTable[row,listLongCol])) & (is.na(locationTable[row,listLatCol]))) {
      if ((is.na(locationTable[row,postLatCol])) & (is.na(locationTable[row,postLongCol]))) {
        locationTable[row, distCol] = NA
        locationTable[row, c(listLocationCol, postLocationCol)] = NA
      }
    }
  }
  row = row + 1
}


row = 1
listCol = 2
postCol = 3
while (row <= nrow(locationTable)){
  listing = locationTable[row, listCol]
  posting = locationTable[row, postCol]
  if ((grepl("", posting)) & (grepl("arverne", listing))) {
  } else if ((grepl("New York", posting, ignore.case = T)) & (grepl("astoria", listing))) {
  } else if ((grepl("New York", posting, ignore.case = T)) & (grepl("bronx", listing))) {
  } else if ((grepl("Brooklyn", posting, ignore.case = T)) & (grepl("brooklyn", listing))){
  } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("bushwick", listing))){
  } else if ((grepl("Jamaica", posting, ignore.case = T)) & (grepl("carnegie hill", listing))){
  } else if ((grepl("Ozone Park", posting, ignore.case = T)) & (grepl("elmhurst", listing))){
  } else if ((grepl("Dodgewood", posting, ignore.case = T)) & (grepl("flushing", listing))){
  } else if ((grepl("Elmhurst", posting, ignore.case = T)) & (grepl("forest hills", listing))){
  } else if ((grepl("Milford", posting, ignore.case = T)) & (grepl("far rockaway", listing))){
  } else if ((grepl("Mt. Pleasant", posting, ignore.case = T)) & (grepl("glendale", listing))){
  } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("howard beach", listing))){
  } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("long island city", listing))){
  } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("new rochelle", listing))){
  } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("new york", listing))){
  } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("ny", listing))){
  } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("queens", listing))){
  } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("rego park", listing))){
  } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("ridgewood", listing))){
  } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("rego park", listing))){
  } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("springfield gardens", listing))){
  } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("staten island", listing))){
  } else if ((grepl("Flushing", posting, ignore.case = T)) & (grepl("washington heights", listing))){
    } else if (grepl(posting, listing, ignore.case = T)) {
  } else if (!grepl(posting, listing, ignore.case = T)){
    prepost[row,16] = prepost[row,16] - 3.3 #location differs
  }
}

#ggplot of total number of each citystate type by descending order
cityStatePlot = ggplot(cityStateTable, aes(x = reorder(cityStates, -count), y = count)) + geom_bar(stat = "identity")
cityStatePlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#dataframe of how many each posting states are present
postingStateTable = data.frame(table(prepost$posting_state))
colnames(postingStateTable) = c('postingStates', 'count')

#ggplot of total number of each postingState type by descending order
postingStatePlot = ggplot(postingStateTable, aes(x = reorder(postingStates, -count), y = count)) + geom_bar(stat = "identity")
postingStatePlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#dataframe of how many scammers is present in each domain
domainScammersTable = data.frame(table(prepost$domain, prepost$status))
colnames(domainScammersTable) = c('domain', 'status', 'count')
domainScammersPlot = ggplot(domainScammersTable, aes(x = reorder(domain, -count), y = count, fill = status)) + geom_bar(stat = "identity")
domainScammersPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Scammers based on Domain", y = "Count", x = "Domain", fill = "Status")

#count how many scammers is present in each domain
domainScammers = data.frame(table(prepost$domain, prepost$status=='scammer'))
colnames(domainScammers) = c('domain', 'scammer_status', 'count')

#remove False rows and 0 Count
domainScammers = domainScammers[domainScammers$scammer_status!="FALSE" & domainScammers$count!="0",]

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