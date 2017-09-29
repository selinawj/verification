library(jsonlite)

prepost = read.csv("nyc_prepost_request.csv", na.strings=" ",header=T)

# create a column for prepost blob beside verify
prepost['prepost_blob']<-NA
 
# #uniformize from factors to characters format
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

#prints out the % of scammers with the domain x (x is domain in string)
domainScammer <- function(x){
  row = 1
  counter = 0
  usernameCol = 3
  verifyCol = 10
  total = 0
  while (row <= nrow(prepost)){
    domain = prepost[row, usernameCol]
    verify = prepost[row, verifyCol]
    if (grepl(x, domain)){
      total = total + 1
      if (grepl("-1", verify)){
        counter = counter + 1
      }
    }
    row = row + 1
  }
  paste(counter/total*100, "%", sep="")
}

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

#dataframe of how many each domain is present
domainTable = data.frame(table(prepost$domain))
colnames(domainTable) = c('domain', 'count')

#ggplot of total number of each domain type by descending order
domainPlot = ggplot(domainTable, aes(x = reorder(domain, -count), y = count)) + geom_bar(stat = "identity")
domainPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

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
domainScammers['percentage'] = ""
row = 1
domainCol = 1
percentageCol = 4
while (row <= nrow(domainScammers)){
  domainScammers[row, percentageCol] = domainScammer(domainScammers[row, domainCol])
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