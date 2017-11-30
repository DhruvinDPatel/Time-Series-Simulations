no.of.trades.df <- read.csv('trade_count.csv')
options(scipen = 999, digits.secs = 3)

# for interpolation of data points
require(zoo)


for(i in 1:nrow(no.of.trades.df)){
  
  setwd('PATH')
  
  # cauchy distribution - cauchy.dist
  cauchy.dist <- rcauchy(no.of.trades.df$x[i], 0, 1)
  
  # truncation of base series
  low <- 6
  base.truc.v <- c(low, -low)
  base.mult.v <- c(0.15)
  base.offset <- 30
  df.params <- data.frame(time=rep(NA,4),count=rep(NA,4))
  df.params[1,] <- cbind("Trade Count",no.of.trades.df$x[i])
  
  for(t in base.truc.v){
    df.params[2,] <- cbind("Base dist truncate",t)
    cauchy.dist[cauchy.dist >= low] <- low
    cauchy.dist[cauchy.dist <= -low] <- -low
    
    for(m in base.mult.v){
      df.params[3,] <- c("Base dist multiplier",m)
      df.params[4,] <- c("Base dist offset", base.offset)
      cauchy.dist <- cauchy.dist * m + base.offset
      cauchy.dist <- round(cauchy.dist,2)
      
      # can take random sampling here
      start.ts <- as.POSIXct("2017-06-06 09:45:00.0000")
      timestamps.v <- seq(from=start.ts,by=.0250,length.out =96000)
      timestamps.v <- timestamps.v + .0001 # for accuracy
      # random timestamps
      r.timestamp <- sample(timestamps.v,no.of.trades.df$x[i], replace = TRUE)
      sim.df <- data.frame(time=as.character.Date(r.timestamp), count=as.numeric(cauchy.dist))
      sim.df <- sim.df[order(sim.df$time),]
      setwd('uninterpolated/')
      p <- paste(paste(i, no.of.trades.df$x[i],t,m,base.offset,sep = '__'),"png",sep = ".")
      png(filename = p)
      plot(sim.df$count, main = paste(no.of.trades.df$x[i],t,m,base.offset, sep = ', '),
      					 xlab = "n", ylab = "price", pch= 20)
      dev.off()
      p1 <- paste(paste(i, no.of.trades.df$x[i],t,m,base.offset,sep = '_'),"csv",sep = ".")
      df.params[5,] <- c("Interpolated", "NO")
      df.params[6,] <- c("timestamp","price")
      sim.df <- rbind(df.params, sim.df)
      write.table(sim.df, file = p1, row.names = FALSE, col.names = FALSE)
    }
  }
}