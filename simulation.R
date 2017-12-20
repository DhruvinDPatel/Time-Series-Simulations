options(scipen = 999, digits.secs = 3)
library(zoo)

try(if(!file.exists("uniquejumptemplates.csv")) stop("Jump template file not found at current directory"))

if( !dir.exists("results")){
  dir.create("results")
}

jam <- function(filename){
  # INTERNAL_PROP_CONF_SCRIPTS
}

astaircase <- function(base.simulation, unit.df, left.sim.seq, right.sim.seq,
                       adaptive.output.path = getwd()){
  
  multiplication.factor = 1
  oscillation.f <- 0
  change.factor = 0
  adaptive <- data.frame(mfactor=as.numeric(),jumpoutput=as.character(), stringsAsFactors=FALSE,
  						oscillationcounter= as.numeric(), change.factor=as.numeric())
  t <- 1
  
  while(TRUE){
    
    final <- rep(NA,96000)
    final[1:48000] <- left.sim.seq
    final[48001:96000] <- right.sim.seq
    final <- final * multiplication.factor
    adaptive[t,]$mfactor <- multiplication.factor
    
    base.simulation[base.simulation$time %in% unit.df$timestamp,]$price <- unit.df$price
    
    base.simulation$price <- as.numeric(base.simulation$price) + final
    new.sim.df <- base.simulation[!is.na(base.simulation$price),]
    
    new.sim.df$price[new.sim.df$price <= 0] <- 0.01
    
    ###########################plotting simulated one: only when required !!!!!!!!!!############
    # plot.ts(new.sim.df$price)
    
    # testing - file that stores uninterpolated time,price pairs
    t.filename <- "temp.txt"
    write('171101100500000', file = t.filename)
    write.table(new.sim.df, t.filename, row.names = F, col.names = F, quote = F, append = T)
    t.result <- jam(t.filename)
    
    adaptive[t,]$jumpoutput <- t.result
    
    if(t==1){
      change.factor <- 0.25
      if(adaptive$jumpoutput[1]!="nojump"){
        oscillation.f <- oscillation.f + 1
      }
    }else if(adaptive$jumpoutput[t] != adaptive$jumpoutput[t-1]){
      oscillation.f <- oscillation.f + 1
      change.factor <- change.factor/2
    }
    
    adaptive[t,]$oscillationcounter <- oscillation.f
    adaptive[t,]$change.factor <- change.factor
    
    if( t.result =="nojump"){
      multiplication.factor <- round(multiplication.factor * (1 + change.factor),4)
    }else{
      multiplication.factor <- round(multiplication.factor * (1 - change.factor),4)
    }
    
    base.simulation$price <- NA
    
    if(nrow(adaptive)>2){
      if(any(nrow(adaptive)>50, abs(adaptive$mfactor[t]-adaptive$mfactor[t-1]) <= 0.0001,
             multiplication.factor > 30 & (adaptive[t,]$jumpoutput == "nojump"))){
        break
      }
    }
    t <- t + 1
    
  }
  # print(multiplication.factor)

  multiplication.factor <- tail(adaptive[adaptive$jumpoutput!="nojump",]$mfactor, 1)
  jump.type <- tail(adaptive[adaptive$jumpoutput!="nojump",]$jumpoutput, 1)
  #write.table(adaptive, adaptive.output.path, row.names = F, sep='\t', quote = F)
  rownames(adaptive) <- NULL
  # print(adaptive)
  out.list <- list("mfactor" = multiplication.factor, "jumptype" = jump.type)
  return(out.list)
}

csv.filenames <- as.list( list.files( path="base_series/", pattern = ".csv$"))

# data frame storing unique jump templates
jump.t.df <- read.table(file = "uniquejumptemplates.csv")
colnames(jump.t.df) <- c("ls","rs","li","ri")

for(base.series.from.file in csv.filenames){
  
  start.ts <- as.POSIXct("2017-11-01 09:45:00.000")
  timestamps.v <- seq(from=as.POSIXct("2017-11-01 09:45:00.000"),by=.0250,length.out =96000)
  timestamps.v <- timestamps.v + .0001
  base.sim.df <- data.frame(time=as.character.Date(timestamps.v), price=rep(NA,96000))
  
  filename = paste("base_series", base.series.from.file, sep = "/")
  
  result <- jam(filename)
  # result
  if(result!="nojump"){
    print(paste("skipping",base.series.from.file, "as it already have a jump of type:",result))
    next
  }
  print(paste("generating simulations for",base.series.from.file) )
  unit.df <- read.table(filename, sep = '\t', dec = '.', skip = 1)
  colnames(unit.df) <- c('timestamp','price')
  # base series plot
  # plot(unit.df$price, col="blue", type="p", xlab = "time", ylab = "price")
  
  dir_name <- paste("results",nrow(unit.df),sep = "/")
  if(!dir.exists(dir_name)){
    dir.create(dir_name)
    # setwd(dir_name)
  }
  
  final.info <- data.frame(ls=as.numeric(),rs=as.numeric(),li=as.numeric(),ri=as.numeric(), jumptype=as.character(),
  							mfactor=as.numeric(),stringsAsFactors=FALSE)
  
  for(i in 1:nrow(jump.t.df)){
    
    # flushing base series price values
    base.sim.df$price <- NA
    
    # generating jump sequences from the template
    left.seq <- rev(seq(from=jump.t.df[i,]$li, by=-jump.t.df[i,]$ls/48000,length.out = 48000))
    right.seq <- rev(seq(to=jump.t.df[i,]$ri, by=-jump.t.df[i,]$rs/48000,length.out = 48000))
    
    # output of staircase method is a list having varibales mfactor and jumptype
    adaptive.output <- astaircase(base.sim.df, unit.df, left.seq, right.seq)
    # print(adaptive.output)
    
    p2 <- paste(jump.t.df[i,]$ls,jump.t.df[i,]$rs, jump.t.df[i,]$li,jump.t.df[i,]$ri)
    
    if(length(adaptive.output$mfactor)==0){
      print(paste("no-jump for template ", p2))
      final.info[i,] <- c(jump.t.df[i,]$ls,jump.t.df[i,]$rs,jump.t.df[i,]$li,jump.t.df[i,]$ri,"nojump",0)
      write(paste("no-jump for template ", p2), file = paste(dir_name, "no_jump_templates_info", sep = "/"), append = T)
    }
    else{
      final.info[i,] <- c(jump.t.df[i,]$ls,jump.t.df[i,]$rs,jump.t.df[i,]$li,jump.t.df[i,]$ri,adaptive.output$jumptype,
      						adaptive.output$mfactor)
      final <- rep(NA,96000)
      final[1:48000] <- left.seq
      final[48001:96000] <- right.seq
      
      # multiplying final vector with m factor from the staircase
      final <- final * adaptive.output$mfactor
      
      base.sim.df[base.sim.df$time %in% unit.df$timestamp,]$price <- unit.df$price
      base.sim.df$price <- as.numeric(base.sim.df$price) + final
      
      base.sim.df$price[base.sim.df$price <= 0] <- 0.01
      
      #removing 'na' from base data frame
      p2 <- paste(jump.t.df[i,]$ls,jump.t.df[i,]$rs, jump.t.df[i,]$li,jump.t.df[i,]$ri, sep = "_")
      new.sim.df <- base.sim.df[!is.na(base.sim.df$price),]
      
      t.filename <- paste(p2, ".csv", sep = "")
      write('171101100500000', file = paste(dir_name,t.filename, sep = "/"))
      write.table(new.sim.df, file = paste(dir_name,t.filename, sep = "/"), row.names = F, col.names = F, quote = F,
      			append = T)
      
      p2 <- paste(i,jump.t.df[i,]$ls,jump.t.df[i,]$rs, jump.t.df[i,]$li,jump.t.df[i,]$ri,
      			adaptive.output$mfactor, sep = '_')
      p2 <- paste(p2, "png", sep=".")
      
      png(filename = paste(dir_name, p2, sep = "/"))
      # this is required for adding plot and legend
      par(mar=c(4.1, 4.1, 4.1, 6.1), xpd=TRUE, col="darkgreen")
      
      # use below for interpolaed time series plot
      # plot(na.locf(base.sim.df$price), col="brown", type="l", xlab = "time", ylab = "price")
      # use below for only points plot
      plot(base.sim.df$price, col="darkblue", type="p", xlab = "time", ylab = "price", pch="*")
      
      # final <- rep(NA,96000)
      final[1:48000] <- left.seq
      final[48001:96000] <- right.seq
      
      lines((final*adaptive.output$mfactor+30), col="red", type = "l")
      p3 <- c(paste('No',i,sep = ':'),paste('left slp',jump.t.df[i,]$ls,sep = ':'),
      			paste('right slp',jump.t.df[i,]$rs,sep = ':'),
              	paste('left int',jump.t.df[i,]$li,sep = ':'),paste('right int',jump.t.df[i,]$ri,sep = ':'),
              	paste('mul factor'),paste(adaptive.output$mfactor),
              	paste('Jump type'), paste(adaptive.output$jumptype))
      
      legend(x = 96000, y= max(base.sim.df$price, na.rm = T), xpd = TRUE, legend = p3, title = "param", bty = 'n')
      dev.off()
      # setwd('../..')
    }
  }
  rownames(final.info) <- NULL
  write.table(final.info, file = paste(dir_name,"final_info.csv", sep = "/"), row.names = F, col.names = F, quote = F,
  				append = T)
  print(paste("results for ",nrow(unit.df)," added"))
}
