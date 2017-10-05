##GGPLOTS
library(ggplot2)
library(scales)
library(plyr)

#dataframe of how many each poster is present
posterTable = data.frame(table(prepost$poster))
colnames(posterTable) = c('poster', 'count')

#ggplot of total number of each poster type by descending order
posterPlot = ggplot(posterTable, aes(x = reorder(poster, -count), y = count)) + geom_bar(stat = "identity")
posterPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#dataframe of how many scammers is present in each poster
posterScammersTable = data.frame(table(prepost$poster, prepost$status))
colnames(posterScammersTable) = c('poster', 'status', 'count')

#plot of how many scammers in each poster
posterScammersPlot = ggplot(posterScammersTable) + geom_bar(aes(x = reorder(poster, -count), y = count, fill = status, group = status), stat = "identity", position = 'dodge') + geom_text(aes(x = reorder(poster, -count), y = count, label = count, group = status), position = position_dodge(width = 1), vjust = -0.5)
posterScammersPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Scammers based on Poster Type", y = "Count", x = "Poster Type", fill = "Status")

#plot of how many posters in each status
scammersPlot = ggplot(posterScammersTable, aes(x = reorder(status, -count), y = count, fill = poster)) + geom_bar(stat = "identity", position = 'dodge') + geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5)
scammersPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Num of Posters of each scammer status", y = "Count", x = "Status", fill = "Poster")

#count how many scammers is present in each poster
posterScammers = data.frame(table(prepost$poster, prepost$status=='scammer'))
colnames(posterScammers) = c('poster', 'scammer_status', 'count')

#merge table
posterScammersTrue = posterScammers[posterScammers$scammer_status!="FALSE",]
posterScammersFalse = posterScammers[posterScammers$scammer_status!="TRUE",]
finalPosterTable = merge(posterScammersTrue, posterScammersFalse, by.x="poster", by.y="poster")
finalPosterTable = finalPosterTable[,-c(2,4)]
finalPosterTable [c("total", "percentage")] = NA
colnames(finalPosterTable ) = c("poster", "scammers", "nonscammers", "total", "scammersPercentage")
finalPosterTable $total = with(finalPosterTable , scammers+nonscammers)
finalPosterTable $scammersPercentage = with(finalPosterTable , scammers/total*100)

#remove False rows
posterScammers = posterScammers[posterScammers$scammer_status!="FALSE",]

##FUNCTIONS

#prints out the % of scammers with the poster type x
calPosterScammer <- function(x){
  row = 1
  counter = 0
  posterCol = 4
  verifyCol = 14
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