set.seed(555)
c.dist <- rcauchy(3451, 0, 1)
plot.ts(c.dist*.15 + 30)
# ni <- length(which(c.dist >= 6))
# ni2 <- length(which(c.dist <= -6))
c.dist[c.dist >= 6 | c.dist <= -6] <- NA
plot.ts(c.dist*.15 + 30)
length(c.dist[is.na(c.dist)])

#uniform dist not useful
u.dist <- runif(3451, min=29.10, max=30.89)
plot.ts(u.dist)
summary(u.dist)

b <- 1
d.dist <- rep(NA,1)
while(TRUE){
  if(b == 3451){
    break
  }
  d.dist[b] <- rcauchy(1,0,1)
  while(d.dist[b] >6 | d.dist[b] < -6){
    d.dist[b] <- rcauchy(1,0,1)
  }
  b <- b + 1
}
plot.ts(d.dist*.15 + 30)
length(d.dist[is.na(d.dist)])
d.dist <- d.dist*.15 + 30

c.dist[c.dist >= 6]
c.dist[c.dist <= -6] <- -6
c.dist <- c.dist * 0.15 + 30
c.dist <- round(c.dist,3)
# random sampling of timestamps - replace = TRUE)
#m <- males[sample(nrow(males), 3500, replace = FALSE, prob = NULL),]
#r.timestamp <- timestamps.v[sample(length(timestamps.v),df2$x[i], replace = F, prob = NULL)]

library(fGarch)
spec = garchSpec(model = list(alpha = 0.1,beta = 0.75),cond.dist = "sstd",rseed = 1)
f.dist <- garchSim(spec, n = 8451)
plot.ts(f.dist*.15 + 30)

r.timestamp <- sample(timestamps.v, df2[i,], replace = T, prob = NULL)

sim.df <- data.frame(time=as.character.Date(r.timestamp), price=as.numeric(c.dist))
sim.df <- sim.df[order(sim.df$time),]

sim.df <- sim.df[!duplicated(sim.df$time, fromLast = T),]