library('fGarch')

#################
Author: Dhruvin Patel
Graduate Student at Stevens | Intern at Acquire Media
dpate78@stevens.edu
#################

# matrix containing all alpha beta coefficients of GARCH simulation
alpha.beta.matrix = data.frame(rbind(.1,.2,.3,.4,.2,.1,.1,.12,.04,.1,.5),
                                rbind(.75,.75,0,0,0,.7,.8,.4,.4,.08,.8))

colnames(alpha.beta.matrix) <- c("alpha","beta")

#no of base series simulations
base.series.num <- 1  # only one base series

for(i in 1:base.series.num){
  
  base.offset <- 100  # mean stock price
  magnifying.factor <- 10 # to enlarge base
  total.points <- 96000 # no of points to simulate
  half <- total.points/2

  # garchSim requires garchSpec
  spec = garchSpec(model = list(alpha = alpha.beta.matrix[i,1],
                                beta = alpha.beta.matrix[i,2]),
                                cond.dist = "sstd",
                                rseed = 1)

  # base series
  base.sim <- (garchSim(spec, n = total.points)*magnifying.factor) + base.offset
  
  yaxis.limits <- c(85,115) # stock price range
  # intercept vectors from (total.points/2, yaxis.limits)
  left.intercept <- seq(from=-0.4,by=0.2,length.out = 5) 
  right.intercept <- seq(from=-0.4,by=0.2,length.out = 5)
  # slope vectors for first half and second half
  left.slope <- c(-1,-0.5,0,0.5,1)
  right.slope <- c(-1,-0.5,0,0.5,1)
  # jump truncation vector
  jump.r <-c(10,3,1)  #please use descending values here
  
  # all possible combinations of left/right intercept and slopes
  intercept.matrix <- combn(c(left.intercept,right.intercept),2)
  slope.matrix <- combn(c(left.slope,right.slope),2)
  
  # nested loop structure to create all possible base + jump series
  for(j in 1:ncol(intercept.matrix)){
    
    temp.intercept <- intercept.matrix[,j]
    
    for(k in 1:ncol(slope.matrix)){
      
      temp.slope <- slope.matrix[,k]
      
      # can vary by param accordingly scaling of plots
      left.seq <- rev(seq(from=temp.intercept[1],
                          by=-temp.slope[1]/(half/10),
                          length.out = half))
      right.seq <- rev(seq(to=-temp.intercept[2],
                            by=-temp.slope[2]/(half/10),
                            length.out = half))
      
      for(l in 1:length(jump.r)){
        
        # truncation of jump vector done
        left.seq[left.seq <= -jump.r[l]] <- -jump.r[l]
        right.seq[right.seq >= jump.r[l]] <- jump.r[l]
        
        # base + jump
        sim1 <- c(base.sim[1:half] + left.seq,
                  base.sim[half+1:total.points] + right.seq)
        
        # verify signs of param
        p2 <- paste(i,temp.intercept[1],temp.intercept[2],temp.slope[1],
                    temp.slope[2],jump.r[l],sep = '_')
        
        # exporting plot to png file
        png(filename = p2)

        # diving plot area for legend and series plot
        par(mar=c(4.1, 4.1, 4.1, 8.1), xpd=TRUE)
        plot.ts(sim1, ylim = yaxis.limits)
        p3 <- c(paste('Base',i,sep = ':'),
                paste('left_int',temp.intercept[1],sep = ':'),
                paste('right_int',temp.intercept[2],sep = ':'),
                paste('left_slp',temp.slope[1],sep = ':'),
                paste('right_slp',temp.slope[2],sep = ':'),
                paste('jump',jump.r[l],sep = ':'))
        
        # putting legend on topright corner having information about params
        legend(x ="topright", xpd = TRUE, legend=p3, inset=c(-0.25,0),
                      title = "Param", bty = 'n')
        dev.off()
      }
    }
  }
}
