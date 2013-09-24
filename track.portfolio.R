library(timeSeries)

# load current market data
current.data <- read.csv(file="current.data.csv")
current.data <- timeSeries(data=current.data[,-1], 
                           charvec=as.character(current.data[,1]))

index.portfolio <- read.csv(file="index.portfolio.csv")

# assert correct securities
stopifnot(as.character(index.portfolio$sec) == names(current.data))

# calculate shares
money.share <- round(1e8 * index.portfolio$weight, digits=2)
shares <- floor(money.share / current.data["2013-09-02",])

# calculate portfolio prices
price <- timeSeries(current.data %*% t(shares), charvec=rownames(current.data))
growth.value <- price - price[1]
growth.rate <- log(price / price[1])

# report
portfolio.data <- cbind(Stock=as.character(index.portfolio$sec),
                        Weight=index.portfolio$weight, 
                        Shares=as.vector(t(shares)))
tracking.report <- cbind(price=price, growth=growth.value, rate=growth.rate)

write.csv(portfolio.data, file="portfolio.data.csv")
write.csv(tracking.report, file="tracking.report.csv")
