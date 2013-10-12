library(timeSeries)

# load current market data
current.data <- read.csv(file="current.data.csv")
current.data <- timeSeries(data=current.data[,-1], 
                           charvec=as.character(current.data[,1]))

index.portfolio <- read.csv(file="index.portfolio.csv")

# assert correct securities
portfolio.stocks <- as.character(index.portfolio$sec)
index.data <- current.data[,"XJO"]
current.data <- current.data[,portfolio.stocks]
stopifnot(portfolio.stocks == names(current.data))

# calculate shares
start.date <- "2013-08-30"
money.share <- round(1e8 * index.portfolio$weight, digits=2)
shares <- floor(money.share / current.data[start.date,])
buyPrices <- as.vector(series(current.data[start.date,]))

# calculate portfolio prices
price <- timeSeries(current.data %*% t(shares), charvec=rownames(current.data))
growth.value <- price - price[1]
growth.rate <- log(price / price[1])
ip.return <- na.omit(fapply(price, FUN=returns), method="z")
idx.return <- na.omit(fapply(index.data, FUN=returns), method="z")

# report
portfolio.data <- cbind(Share=as.character(index.portfolio$sec),
                        Price=buyPrices, 
                        No.Shares=as.vector(t(shares)),
                        Value=buyPrices * as.vector(t(shares)),
                        Weight=index.portfolio$weight)
tracking.report <- cbind(price=price, growth=growth.value, rate=growth.rate,
                         Return=ip.return, `Index Return`=idx.return)

write.csv(portfolio.data, file="portfolio.data.csv")
write.csv(tracking.report, file="tracking.report.csv")
