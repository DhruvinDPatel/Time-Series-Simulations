left.i <- c(0, 0.4, -0.4, 0.2, -0.2, 0.1, -0.1)
right.i <- c(0, 0.4, -0.4, 0.2, -0.2, 0.1, -0.1)
left.s <- c(0, 1, -1, 0.5, -0.5, 0.25, -0.25)
right.s <- c(0, 1, -1, 0.5, -0.5, 0.25, -0.25)

# data frame containing unique jump templates
jump.t.df <- data.frame(ls=as.numeric(),rs=as.numeric(),li=as.numeric(),ri=as.numeric(),
						difference=as.numeric())

# counters
t.c <- 1
t.c1 <- 0
t.c2 <- 0
t.c3 <- 0
t.c4 <- 1
r.c <- 0

# below vector contains difference of intercept vectors
diff.v <- c(-.8, -.4, -.2, 0, .2, .4, .8)

duplicated.jump <- data.frame(ls=as.numeric(),rs=as.numeric(),li=as.numeric(),ri=as.numeric(),
								difference=as.numeric())

jump.t.df[t.c,] <- c(left.s[t.c],right.s[t.c],left.i[t.c],right.i[t.c],right.i[t.c] - left.i[t.c])
t.c <- sum(t.c,1)
loop.counter <- 1

for(sl in left.s){
  for(sr in right.s){
    for(il in left.i){
      for(ir in right.i){
        
        if(loop.counter==1){
          loop.counter <- sum(loop.counter,1)
          # print("skipped")
          next
        }
        difference <- ir - il
        
        if( all(difference %in% diff.v, abs(ir)==abs(il)) ){
          flag1 <- FALSE
          flag2 <- FALSE
          flag3 <- FALSE
          d.flag <- TRUE
          
          for( i in 1:nrow(jump.t.df)){
            
            if( any(sign(difference) != sign(jump.t.df[i,]$difference),  
                    (sign(sl) != sign(jump.t.df[i,]$ls) ), 
                    (sign(sr) != sign(jump.t.df[i,]$rs) ) ) ){
              flag1 <- TRUE
              t.c1 <- sum(t.c1,1)
            }else if( all(difference == 0, jump.t.df[i,]$difference == 0, sl != 0,
            			  jump.t.df[i,]$ls != 0, sr != 0, jump.t.df[i,]$rs != 0,  
                          (sl*jump.t.df[i,]$rs != jump.t.df[i,]$ls*sr) ) ){
              flag2 <- TRUE
              t.c2 <- sum(t.c2,1)
            }else if( difference != 0 & jump.t.df[i,]$difference != 0 &
                      ((jump.t.df[i,]$difference*sl != difference*jump.t.df[i,]$ls) | 
                       (jump.t.df[i,]$difference*sr != difference*jump.t.df[i,]$rs)) ){
              flag3 <- TRUE
              t.c3 <- sum(t.c3,1)
            }else{
              duplicated.jump[t.c4,] <- c(sl,sr,il,ir,difference)
              t.c4 <- sum(t.c4,1)
              d.flag <- FALSE
              break
            }
          }
          
          if(any(flag1, flag2, flag3) & d.flag){
            jump.t.df[t.c,] <- c(sl,sr,il,ir,difference)
            t.c <- sum(t.c,1) 
          }
          
        }else{
          r.c <- sum(r.c,1)
        }
        
      }
    }
  }
}

# removing first row
# jump.t.df <- jump.t.df[-c(1),]

jump.t.df$difference <- NULL
duplicated.jump$difference <- NULL
# unique stores all jump template that we need; if any error in parsing, remove \t from below
write.table(jump.t.df, file = "uniquejumptemplates.csv", row.names = F, sep='\t', quote = F, 
			col.names = F)
print("successfully added UNIQUE JUMP TEMPLATES file")
# write.table(duplicated.jump, file = "duplicates", row.names = F, sep='\t', quote = F)

# for plotting jump templates
# setwd('jump_template_plots/normal/')
# for(i in 1:nrow(jump.t.df)){
#   final <- rep(NA,96000)
#   left.seq <- rev(seq(from=jump.t.df[i,]$li, by=-jump.t.df[i,]$ls/48000,length.out = 48000))
#   right.seq <- rev(seq(to=jump.t.df[i,]$ri, by=-jump.t.df[i,]$rs/48000,length.out = 48000))
#   
#   final[1:48000] <- left.seq
#   final[48001:96000] <- right.seq
# png(filename = paste(jump.t.df[i,]$ls,jump.t.df[i,]$rs,jump.t.df[i,]$li,jump.t.df[i,]$ri,sep='_'))
#   plot.ts(final)
#   dev.off()
# }
# 
# setwd('../duplicates/')
# for(i in 1:nrow(duplicated.jump)){
#   final <- rep(NA,96000)
#left.seq <-rev(seq(from=duplicated.jump[i,]$li,by=duplicated.jump[i,]$ls/48000,length.out = 48000))
#right.seq <- rev(seq(to=duplicated.jump[i,]$ri,by=duplicated.jump[i,]$rs/48000,length.out = 48000))
#   
#   final[1:48000] <- left.sim.seq
#   final[48001:96000] <- right.sim.seq
#png(filename=paste(duplicated.jump[i,]$ls,duplicated.jump[i,]$rs,duplicated.jump[i,]$li,duplicated.jump[i,]$ri,sep='_'))
#   plot.ts(final)
#   dev.off()
# }