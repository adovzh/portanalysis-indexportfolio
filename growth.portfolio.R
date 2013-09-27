library(fImport)
library(quadprog)

stocks <- c("AQG", "OZL", "NCM", "WBC", "MQG", "LLC", "FLT")

aqg <- yahooSeries("AQG.AX", from="2007-01-01", to="2013-08-30")
ozl <- yahooSeries("OZL.AX", from="2007-01-01", to="2013-08-30")

history <- read.table("data_historical.csv", sep=",", header=TRUE)
history$Date <- format(as.Date(as.character(history$Date), 
                               format="%d-%B-%y"), "%Y-%m-%d")
# history$AQG <- aqg
# history$OZL <- ozl

hs <- timeSeries(history[-1], as.character(history[,1]))
hs <- cbind(hs, AQG=aqg[,"AQG.AX.Close"], OZL=ozl[,"OZL.AX.Close"])
hs <- hs[, stocks]
rt <- fapply(hs, FUN=returns)
rt <- na.omit(rt)
rt.cov <- cov(rt, use="complete.obs")

risk.param <- 9
Amat<-matrix(1, nrow=nrow(rt.cov))
qp <- solve.QP(Dmat=rt.cov, dvec=colMeans(rt) * risk.param,
               Amat=Amat,
               bvec=1, meq=1)