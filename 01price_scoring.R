lowPrice = data.frame(matrix(nrow= 4, ncol=3))
colnames(lowPrice) = c("price", "status", "count")
#low prices constitutes 0 or 1 prices
lowPrice[c(1,3),1] = "low"
lowPrice[c(2,4),1] = "normal"
lowPrice[c(1,2),2] = "scammer"
lowPrice[c(3,4),2] = "nonscammer"
lowPrice[1,3] = sum(prepost$price=="0" & prepost$status=="scammer") + sum(prepost$price=="1" & prepost$status=="scammer")
lowPrice[2,3] = sum(prepost$price!="0" & prepost$price!="0" & prepost$status=="scammer")
lowPrice[3,3] = sum(prepost$price=="0" & prepost$status=="non-scammer") + sum(prepost$price=="1" & prepost$status=="non-scammer")
lowPrice[4,3] = sum(prepost$price!="0" & prepost$price!="1" & prepost$status=="non-scammer")
#why is there 614 in total????