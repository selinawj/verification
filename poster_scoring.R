##GGPLOTS

#dataframe of how many each poster is present
posterTable = data.frame(table(prepost$poster))
colnames(posterTable) = c('poster', 'count')

#ggplot of total number of each poster type by descending order
posterPlot = ggplot(posterTable, aes(x = reorder(poster, -count), y = count)) + geom_bar(stat = "identity")
posterPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#dataframe of how many scammers is present in each poster
posterScammersTable = data.frame(table(prepost$poster, prepost$status))
colnames(posterScammersTable) = c('poster', 'status', 'count')
posterScammersPlot = ggplot(posterScammersTable, aes(x = reorder(poster, -count), y = count, fill = status)) + geom_bar(stat = "identity")
posterScammersPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Scammers based on Poster Type", y = "Count", x = "Poster Type", fill = "Status")

#count how many scammers is present in each poster
posterScammers = data.frame(table(prepost$poster, prepost$status=='scammer'))
colnames(posterScammers) = c('poster', 'scammer_status', 'count')

#remove False rows
posterScammers = posterScammers[posterScammers$scammer_status!="FALSE",]

##FUNCTIONS

#prints out the % of scammers with the poster type x
calPosterScammer <- function(x){
  row = 1
  counter = 0
  posterCol = 4
  verifyCol = 11
  total = 0
  while (row <= nrow(prepost)){
    poster = prepost[row, posterCol]
    verify = prepost[row, verifyCol]
    if (grepl(x, poster)){
      total = total + 1
      if (verify == "scammer"){
        counter = counter + 1
      }
    }
    row = row + 1
  }
  paste(counter/total*100)
}

##SCORING MECHANISM

posterScammers['percentage'] = ""
row = 1
posterCol = 1
percentageCol = 4
while (row <= nrow(posterScammers)){
  posterScammers[row, percentageCol] = calPosterScammer(posterScammers[row, posterCol])
  row = row + 1
}

posterScammers["score"] = ""

#calculates score to be allocated for domain based on % scammers
row = 1
score = 0
percentageCol = 4
scoreCol = 5
while (row <= nrow(posterScammers)){
  percentage = as.numeric(posterScammers[row, percentageCol])
  score = percentage/100*5
  posterScammers[row,scoreCol] = round(score)
  row = row + 1
}

#not using poster type to score since it doesn't tell us much about them being scammers
#with landlords having higher chance of being scammers