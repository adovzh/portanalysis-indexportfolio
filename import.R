library(timeSeries)
library(fImport)

tradingRoomImport <- function(symbol, from, to) {
  url <- "http://www.tradingroom.com.au/apps/qt/csv/pricehistory.ac?section=yearly_price_download&code="
  url <- paste(url, symbol, sep="")
  tmp <- tempfile()
  download.file(url=url, destfile=tmp)
  tt <- read.csv(file=tmp)
  X <- as.timeSeries(tt)
  X <- window(X, start=from, end=to)
  unlink(tmp)
  new("fWEBDATA", call=match.call(), param = c(Instrument=symbol, `Frequency`="daily"),
      data=X, title="Data Import from Trading Room",
      description=description())
}

importDailyClose <- function(symbols, method=c("yahoo", "tradingroom"), 
                             from=NULL, to=Sys.timeDate()) {
  method <- match.arg(method)
  method.func <- switch(method, yahoo=yahooImport, tradingroom=tradingRoomImport)
  
  r <- lapply(symbols, function(symbol) {
    retryCounter <- 0
    max.retry <- 10
    
    repeat {    
      X <- suppressWarnings(method.func(symbol, from=from, to=to))  
      if (is.character(X) && retryCounter < max.retry) {
        print(X)
        retryCounter <- retryCounter + 1
      }
      else
        break;
    }
    
    if (retryCounter < max.retry) {
      col <- X@data[, "Close"]
      names(col) <- symbol
      return(col)        
    } else {
      return(NULL)
    }    
  })
  
  na.omit(do.call("cbind", r), method="ie", interp="after")
}
