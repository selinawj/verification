##GGPLOTS
library(ggplot2)
library(scales)
library(plyr)

#dataframe of how many each poster is present
posterTable = data.frame(table(prepost$poster))
colnames(posterTable) = c('poster', 'count')

#ggplot of total number of each poster type by descending order
posterPlot = ggplot(posterTable, aes(x = reorder(poster, -count), y = count)) + geom_bar(stat = "identity") + geom_text(aes(x = reorder(poster, -count), y = count, label = count), position = position_dodge(width = 1), vjust = -0.5)
posterPlot

#dataframe of how many scammers is present in each poster
posterScammersTable = data.frame(table(prepost$poster, prepost$status))
colnames(posterScammersTable) = c('poster', 'status', 'count')

#plot of how many scammers in each poster
posterScammersPlot = ggplot(posterScammersTable) + geom_bar(aes(x = reorder(poster, -count), y = count, fill = status, group = status), stat = "identity", position = 'dodge') + geom_text(aes(x = reorder(poster, -count), y = count, label = count, group = status), position = position_dodge(width = 1), vjust = -0.5)
posterScammersPlot + labs(title = "Scammers based on Poster Type", y = "Count", x = "Poster Type", fill = "Status")

#plot of how many posters in each status
scammersPlot = ggplot(posterScammersTable, aes(x = reorder(status, -count), y = count, fill = poster)) + geom_bar(stat = "identity", position = 'dodge') + geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5)
scammersPlot + labs(title = "Num of Posters of each scammer status", y = "Count", x = "Status", fill = "Poster")

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
finalPosterTable$total = with(finalPosterTable , scammers+nonscammers)
finalPosterTable$scammersPercentage = with(finalPosterTable , scammers/total*100)