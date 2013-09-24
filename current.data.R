library(fImport)

index.portfolio <- read.csv(file="index.portfolio.csv")

start.date <- "2013-09-02"
symbols <- index.portfolio$sec

r <- lapply(symbols, function (x) {
  yhSymbol <- paste(x, ".AX", sep="")
  retryCounter <- 0
  max.retry <- 10
  
  repeat {    
    X <- suppressWarnings(yahooImport(yhSymbol, from=start.date, to=Sys.Date()))  
    if (is.character(X) && retryCounter < max.retry) {
      print(X)
      retryCounter <- retryCounter + 1
    }
    else
      break;
  }
  
  if (retryCounter < max.retry) {
    col <- X@data[, "Close"]
    names(col) <- x
    return(col)        
  } else {
    return(NULL)
  }
})

m <- na.omit(do.call("cbind", r), method="ie", interp="after")
write.csv(m, file="current.data.csv")
