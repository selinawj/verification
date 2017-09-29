library(jsonlite)
# library(data.table)

# prepost = fread("nyc_prepost_request.csv", na.strings=" ",header=T)
prepost = read.csv("nyc_prepost_request.csv", na.strings=" ",header=T)

# create a column for prepost blob beside verify
# prepost[,prepost_blob:=character(.N)]
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


#plot domain categories against verify statuses
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

prepostRow = 1
domainDFRow = 1
domainCol = 55
domainDFCol = 1
while (prepostRow <= nrow(prepost)){
  if (!prepost[prepostRow, domainCol] %in% domainDF$domain){
    domainDF[domainDFRow, domainDFCol] = prepost[prepostRow, domainCol]
  }
  prepostRow = prepostRow + 1
}

# poster = read.csv("poster.csv", na.strings=" ", header = F)
# license = read.csv("license.csv", na.strings=" ", header = F)
# posting_state = read.csv("posting_state.csv", na.strings=" ", header = F)
# phone_num = read.csv("phone_num.csv", na.strings=" ", header = F)
# carrier = read.csv("carrier.csv", na.strings=" ", header = F)
# 
# #change all NULL to NA
# is.na(prepost) <- prepost == "NULL"
# 
# #remove unnecessary cols
# prepost[, (c(1, 4, 6, 8:9, 11, 13:18, 20:21, 23:25, 28:37, 38:53))] <- NULL
# 
# #change colname
# colnames(prepost)[4] <- "poster"
# colnames(prepost)[8] <- "listing_state"
# 
# #add in new cols: license, state, phone_num
# prepost["license"] <-NA
# prepost["posting_state"] <-NA
# prepost["carrier"] <-NA
# prepost["median"] <-NA
# 
# prepost <- prepost[c("type", "account_id", "listing_id", "username", "poster", "license", "posting_state", "listing_state",  "carrier", "bedrooms", "bathrooms", "price", "median", "verification_status")]
# 
# library(tidyr)
# 
# #separate columns by nonalphanum
# poster = separate(poster, V1, into = c("account_id", "poster"), sep = "[^[:alnum:]]+")
# license = separate(license, V1, into = c("account_id", "license"), sep = "\\*")
# posting_state = separate(posting_state, V1, into = c("account_id", "posting_state"), sep = "\\*")
# phone_num = separate(phone_num, V1, into = c("account_id", "phone_num"), sep = "\\*")
# carrier = separate(carrier, V1, into = c("account_id", "carrier"), sep = "[^[:alnum:]]+")
# 
# #append poster(existing), license, posting_state, carrier into df
# row = 1
# for (each in poster$poster){
#   if (!is.na(each)){
#     prepost[row,5] = each
#   }
#   row = row + 1
# }
# row = 1
# for (each in license$license){
#   prepost[row,6] = each
#   row = row + 1
# }
# row = 1
# for (each in posting_state$posting_state){
#   prepost[row,7] = each
#   row = row + 1
# }
# row = 1
# for (each in carrier$carrier){
#   prepost[row,9] = each
#   row = row + 1
# }
# 
# #adding phone_num into a new df before appending
# phone_num_df = data.frame(matrix(0, ncol = 1, nrow = 1422))
# colnames(phone_num_df) <- "phone"
# row = 1
# for (each in phone_num$phone_num){
#   each = as.numeric(as.character(each))
#   phone_num_df[row,1] = each
#   row = row + 1
# }
# #doesn't work
# #row = 1
# #for (each in phone_num_df$phone){
# #  prepost[row,9] = each
# #  row = row + 1
# #}
# 
# #combining phone_num
# prepost = cbind(prepost, phone_num_df$phone)
# colnames(prepost)[15] <- "phone_num"
# 
# #rearrange cols
# prepost <- prepost[c("type", "account_id", "listing_id", "username", "poster", "license", "posting_state", "listing_state", "phone_num", "carrier", "bedrooms", "bathrooms", "price", "median", "verification_status")]
# 
# #replace empty cells to NA
# prepost[prepost==""] <- NA
# 
# #doesn't work
# #check poster if agent
# #for (row in prepost$poster){
# #  if (row == "agent")
# #    for (row in prepost$license){
# #      if (is.na(row)){
# #        print (row)
# #      }
# #    }
# #}