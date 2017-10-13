#set a dataframe for license
license = data.frame(matrix(nrow = 4, ncol = 3))
colnames(license) = c("license", "status", "count")

i <- sapply(license, is.factor)
license[i] <- lapply(license[i], as.character)

license[c(1,2),1] = "license"
license[c(3,4),1] = NA
license[c(1,3),2] = "scammers"
license[c(2,4),2] = "non-scammers"

license[1,3] = sum(prepost$poster=="agent" & prepost$license!="" & prepost$status=="scammer")
license[2,3] = sum(prepost$poster=="agent" & prepost$license!="" & prepost$status=="non-scammer")
license[3,3] = sum(prepost$poster=="agent" & prepost$license=="" & prepost$status=="scammer")
license[4,3] = sum(prepost$poster=="agent" & prepost$license=="" & prepost$status=="non-scammer")

##GG PLOTS

library(ggplot2)

#plot of how many scammers in each license type category
licenseScammersPlot = ggplot(license) + geom_bar(aes(x = reorder(license, -count), y = count, fill = status, group = status), stat = "identity", position = 'dodge') + geom_text(aes(x = reorder(license, -count), y = count, label = count, group = status), position = position_dodge(width = 1), vjust = -0.5)
licenseScammersPlot + labs(title = "Scammers based on Agent License", y = "Count", x = "License Provided", fill = "Status")

#plot of how many license type in each status
licensePlot = ggplot(license, aes(x = reorder(status, -count), y = count, fill = license)) + geom_bar(stat = "identity", position = 'dodge') + geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5)
licensePlot + labs(title = "Num of Agent License Type within each scammer status", y = "Count", x = "Status", fill = "License")

#merge table
licenseScammersTrue = license[license$status!="non-scammers",]
licenseScammersFalse = license[license$status!="scammers",]
finalLicenseTable = merge(licenseScammersTrue, licenseScammersFalse, by.x="license", by.y="license")
finalLicenseTable =finalLicenseTable[,-c(2,4)]
finalLicenseTable[c("total", "percentage")] = NA
colnames(finalLicenseTable) = c("license", "scammers", "nonscammers", "total", "scammersPercentage")
finalLicenseTable$total = with(finalLicenseTable, scammers+nonscammers)
finalLicenseTable$scammersPercentage = with(finalLicenseTable, scammers/total*100)