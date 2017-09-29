#to keep track
set.seed(1)

price_neighborhood = read.csv("price_neighborhood.csv", na.strings="", header = T)

#1 obtaining a 1/5 sample of the price_neighborhood df
library("dplyr")
price_neighborhood = sample_n(price_neighborhood,100000)

#2 preprocess price of each neighborhood

#removing rows with price 0 & 1
row = 1
while (row <= nrow(price_neighborhood)){
  price = as.character(price_neighborhood[row,1])
  if (price == "0"){
    price_neighborhood[row,1] = NA
  } else if (price == "1"){
    price_neighborhood[row,1] = NA
  }
  row = row + 1
}

#removing rows with NA values
price_neighborhood = na.omit(price_neighborhood)

#3 create single neighborhood col
price_neighborhood["neighborhood"] <- NA

#4 extract neighborhood from array & append to df -- takes a while
neighborhood = price_neighborhood[,2]

row = 1
for(item in neighborhood){
  item = substring(item, 2, nchar(item)-1)
  alist = unlist(strsplit(item, ","))
  len = as.character(length(alist))
  if (len == "3") {
    fng = substring(alist[1], 0, nchar(alist[1]))
  } else if (len == "4") {
    fng = substring(alist[2], 0, nchar(alist[2]))
  }
  price_neighborhood[row, 3] = fng 
  row = row + 1
}

output = aggregate(as.numeric(as.character(price_neighborhood$price)), list(price_neighborhood$neighborhood), median)

colnames(output) = c("Neighborhood","Price")

write.csv(output, file = "output.csv")
