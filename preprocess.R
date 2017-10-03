library(jsonlite)

prepost = read.csv("nyc_prepost_request.csv", na.strings=" ",header=T)

# create a column for prepost blob beside verify
prepost['prepost_blob']<-NA
 
#uniformize from factors to characters format
i <- sapply(prepost, is.factor)
prepost[i] <- lapply(prepost[i], as.character)

#combining prepost blob with verify requests if user account exists
#for each account id, if there exists both prepost & verify, append prepost blob to prepost col
row = 1
accountIdCol = 2
prepostTypeCol = 5
prepostCol = 11
prepostBlobCol = 54
while (row <= nrow(prepost)-1){
  accountId = prepost[row, accountIdCol]
  type = prepost[row, prepostTypeCol]
  nextId = prepost[row+1, accountIdCol]
  if (grepl(accountId, nextId)){
    if (grepl("prepost_completed", type)){
      if (grepl("verify_request", prepost[row+1, prepostTypeCol]))
        prepost[row+1, prepostBlobCol] = prepost[row, prepostCol]
    }
  }
  row = row + 1
}

#clean up rows which are preposts & then delete them
row = 1
prepostTypeCol = 5
while (row <= nrow(prepost)){
  type = prepost[row, prepostTypeCol]
  if (grepl("prepost_completed", type)){
    prepost[row,] <- NA
  }
  row = row + 1
}

#removing completely NA rows
#remove rows with empty prepost blob because they don't tell us much about the various criterias req
prepost = na.omit(prepost)

#save as csv file for pre-process in python
write.csv(prepost, "renthop.csv")

#create domain column
prepost['domain']<-NA

#extract domain of each usernames into domain col
row = 1
usernameCol = 3
domainCol = 55
while (row <= nrow(prepost)){
  domainList = strsplit(prepost[row,usernameCol], "@")
  domain = sapply(domainList, "[[", 2)
  prepost[row,domainCol] = domain
  row = row + 1
}

#create a status column
prepost["status"] <- NA
row = 1
verify_status_col = 10
scammer_col = 56
while (row <= nrow(prepost)){
  status = as.character(prepost[row, verify_status_col])
  if (grepl("-1", status)) {
    prepost[row, scammer_col] = "scammer"
  } else {
    prepost[row, scammer_col] = "non-scammer"
  }
  row = row + 1
}

#read prepostBlob csv file pre-processed by python
details = read.csv("details.csv", header=F)
colnames(details) = c("poster", "license", "carrier", "phone_num", "posting_state")

#remove unnecessary cols in prepost
prepost[, (c(1, 4:18, 20:21, 23:37, 39:54))] <- NULL

#combining prepostBlob details to prepost df
prepost = cbind(prepost, details)

#rearrange columns
prepost = prepost[,c("account_id", "username", "domain", "poster", "license", "carrier", "phone_num", "citystate", "posting_state", "price", "status")]