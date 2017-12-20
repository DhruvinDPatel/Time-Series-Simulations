# dataframe containing trade count values
try(if(!file.exists("trade_count.csv")) stop("trade count file not found"))

df2 <- read.csv('trade_count.csv')
options(scipen = 999, digits.secs = 3)
require(zoo)

# uncomment lines containing base.jump.stats if you need "JUMP-NOJUMP" status for base series
# base.jump.stats <- data.frame(trade.count=as.integer(0), trade.count2=as.integer(0), result=rep(NA,1))

if( !dir.exists("base_series")){
  dir.create("base_series")
}

for(i in 1:nrow(df2)){
  # start.timestamp <- as.POSIXct("2017-06-06 09:45:00.0000")
  # below vectors contains timestamps with 25ms time difference
  timestamps.v <- seq(from=as.POSIXct("2017-11-01 09:45:00.0000"),by=.0250,length.out =96000)
  timestamps.v <- timestamps.v + .0001
  # base.jump.stats[i,]$trade.count <- df2[i,]
  b <- 1
  d.dist <- rep(NA,1)
  
  # values 6,-6 are thresold values; followed by multiplication with .15

  while(TRUE){
    # rcauchy to create cauchy distributions
    d.dist[b] <- rcauchy(1,0,1)
    while(d.dist[b] > 6 | d.dist[b] < -6){
      d.dist[b] <- rcauchy(1,0,1)
    }
    b <- b + 1
    if(b == df2[i,] + 1){
      break
    }
  }
  
  d.dist <- round(d.dist*.15 + 30,3)
  
  # random sampling of timestamps - replace = TRUE)
  #r.timestamp <- timestamps.v[sample(length(timestamps.v),df2$x[i], replace = F, prob = NULL)]
  r.timestamp <- sample(timestamps.v, df2[i,], replace = T, prob = NULL)
  
  sim.df <- data.frame(time=as.character.Date(r.timestamp), price=as.numeric(d.dist))
  sim.df <- sim.df[order(sim.df$time),]
  
  # replacing duplicated values manually - not using anymore
  #for(j in 1:(df2$x[i]-1)){
  #  if(sim.df$time[j] == sim.df$time[j+1]){
  #    sim.df$price[j] = sim.df$price[j+1] 
  #  }
  #}
  
  # remove duplicates from last(below); last price value for same time stamp
  sim.df <- sim.df[!duplicated(sim.df$time, fromLast = T),]
  
  #p <- paste(paste(i, 'df',sep = '__'),"png",sep = ".")
  #png(filename = p)
  #plot(sim.df$count, main = paste(df2$x[i],6,0.15,30, sep = ', '), xlab = "n", ylab = "price", pch= 20)
  #dev.off()
  p1 <- paste(paste(i,nrow(sim.df), df2[i,],sep = '_'),"csv",sep = ".")
  p1 <- paste("base_series", p1, sep = "/")
  write('171101100500000', file = p1)
  write.table(sim.df, file = p1, row.names = FALSE, col.names = F, sep = '\t', quote = F, append = T)
  print(paste(i, " file added to base_series"))
  # only if want to know the base series jump status(requires JAM's scrunch script)
  # base.jump.stats[i,]$trade.count2 <- nrow(sim.df)
  # base.jump.stats[i,]$result <- jam(p1)
}

print(paste(nrow(df2), " files added to base_series"))