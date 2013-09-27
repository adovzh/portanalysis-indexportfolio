source("import.R")

index.portfolio <- read.csv(file="index.portfolio.csv")
start.date <- "2013-09-02"

# download data from Trading Room
m <- importDailyClose(index.portfolio$sec, from=start.date, method="trading")
i <- importDailyClose("XJO", from=start.date, method="index")

# save csv data
write.csv(cbind(m,i), file="current.data.csv")
