library(timeSeries)
library(quadprog)

history <- read.table("data_historical.csv", sep=",", header=TRUE)
history$Date <- format(as.Date(as.character(history$Date), 
                               format="%d-%B-%y"), "%Y-%m-%d")

# stock indices in the price matrix
# some stocks are excluded as they don't have enough history
equities <- setdiff(2:100, c(29, 31, 64, 69, 77, 87, 100))

# price matrix time series
pc <- timeSeries(history[,equities], as.character(history$Date))
# continuously compounded returns
rt <- fapply(pc, FUN=returns)
# use all history
rt.history <- rt
# covariance matrix with only complete observations used
rt.cov <- cov(rt.history, use="complete.obs")

# number of weights, less one for the XJO index
nw <- nrow(rt.cov) - 1

# returns vector of the specified size with all the elements 
# set to zero except for the n-th element set to 1
idn <- function(n, size) {
  v <- rep(0, size)
  v[n] <- 1
  return(v)
}

# solves the optimisation problem with the excluded
# stocks' weights set to zero
# 
# variables from outside environment:
# nw - number of stocks
# rt.cov - covariance matrix
eval.weights <- function(excluded) {
  Amat <- cbind(
    idn(1, nw + 1),
    c(0, rep(1, nw)),
    rbind(0, sapply(c(excluded, setdiff(1:nw, excluded)),
                    function(x) idn(x, nw))))
  bvec <- c(-1, 1, rep(0, nw))
  qp <- solve.QP(rt.cov, matrix(0, nw+1), Amat, bvec, 2 + length(excluded))
  return(qp$solution[2:(nw+1)])
}

# start value of excluded set
excl <- NULL
# remove 'step' stocks per iteration
step <- 3

repeat {
  # find the weights for the optimal portfolio 
  # with the stocks in the exclusion set excluded from it
  w <- eval.weights(excl)
  
  # how many stocks can we exclude?
  k <- min(step, nw - length(excl) - 25)
  
  # nothing left to exclude
  if (k <= 0) break;
  
  # update exclusion set
  excl <- order(w)[1:(length(excl) + k)]
}

selected <- which(w>1e-9)
report <- data.frame(sec=colnames(rt)[1+selected], 
                     weight=w[selected],
                     rank=selected)
write.csv(report, file="index.portfolio.csv")
