source("import.R")

index.portfolio <- read.csv(file="index.portfolio.csv")
start.date <- "2013-09-02"

# download data from Trading Room
m <- importDailyClose(index.portfolio$sec, from=start.date, method="trading")

# save csv data
write.csv(m, file="current.data.csv")
