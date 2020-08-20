## This is the file to reproduce the figures ## 

results_intercept <- readRDS("./Results/results_intercept_lme4.Rds")
results_slope <- readRDS("./Results/results_slope_lme4.Rds")
results_lm <- readRDS("./Results/results_intercept_fixed_effect.Rds")

## random intercept plot ##
par(mfrow = c(3,3), oma = c(0,15,0,0))

for(i in 1:3){
  if(i == 1){
    hist(results_intercept[[i]][,3],
       main = paste("number of levels:", i+1),
       xlab = "estimated p-value",
       breaks = 20, xlim = c(0,0.3), col = "grey", cex.main = 2)
    #mtext("a)", side = 3, at = -0.1, line = -2, cex =2)
    mtext(substitute(paste(italic("p-value of"))), side = 3, at = -0.21, 
          line = -13, cex = 1.5)
    mtext(substitute(paste(italic("fixed intercept"))), side = 3, at = -0.21, 
          line = -15, cex = 1.5)
  }
  else{
    hist(results_intercept[[i]][,3],
         main = paste("number of levels:", i+1 ),
         xlab = "estimated p-value",
         breaks = 20, xlim = c(0,0.3), col = "grey", cex.main = 2)
  }
  abline(v = 0.05, col = "red")
}


for(i in 1:3){
  if(i == 1){
  hist(results_intercept[[i]][,5], 
       main = "",
       xlab = "estimated standard deviation of random intercept",
       breaks = 20, xlim = c(0,45), col = "grey")
    mtext(substitute(paste(italic("standard deviation"))), side = 3, at = -30, 
          line = -13, cex = 1.5)
    mtext(substitute(paste(italic("random intercept"))), side = 3, at = -30, 
          line = -15, cex = 1.5)

  }
  else{
    hist(results_intercept[[i]][,5], 
         main = "",
         xlab = "estimated standard deviation of random intercept",
         breaks = 20, xlim = c(0,45), col = "grey")
  }
  abline(v = 0.2*66, col = "red")
}

for(i in 1:3){
  if(i == 1){
  hist(results_intercept[[i]][,1], 
       main = "",
       xlab = "estimated intercept",
       breaks = 40, xlim = c(35,97), col = "grey", ylim = c(0,300))
    mtext(substitute(paste(italic("estimate of"))), side = 3, at = -5, 
          line = -13, cex = 1.5)
    mtext(substitute(paste(italic("fixed intercept"))), side = 3, at = -5, 
          line = -15, cex = 1.5)
  }
  else{
    hist(results_intercept[[i]][,1], 
         main = "",
         xlab = "estimated intercept",
         breaks = 20, xlim = c(35,97), col = "grey", ylim = c(0,300))
  }
  abline(v = 66, col = "red")
}



## random slope plot ## 
for(i in 1:3){
  hist(results_slope[[i]][,4], 
       main = paste("number of groups:", i+1),
       xlab = "estimated p-value",
       breaks = 20,xlim = c(0,0.3), col = "grey")
  abline(v = 0.05, col = "red")
}

for(i in 1:3){
  hist(results_slope[[i]][,5], 
       main = "",
       xlab = "estimated standard deviation of random intercept",
       breaks = 20, xlim = c(0,1.8) ,col = "grey")
  abline(v = 0.2*3, col = "red")
}

for(i in 1:9){
  hist(results_slope[[i]][,2], 
       main = "",
       xlab = "estimated intercept",
       breaks = 20, xlim =c(1.4,4.6), col = "grey")
  abline(v = 3, col = "red")
}


### treating as fixed effect variable ###

dev.off()
par(mfrow = c(2,3), oma = c(0,15,0,0))
for(i in 1:3){
  if(i == 1){
    hist(results_lm[[i]][,1], 
         main = paste("number of levels:", i+1),
         xlab = "mean estimated intercept",
         breaks = 40, xlim = c(10,110), col = "grey",
         las = 1,ylim = c(0, 0.04), prob = T, cex.main = 1.8)
    curve(dnorm(x, mean=66, sd=13.2), 
          col="darkblue", lwd=2, add=TRUE, yaxt="n")
    mtext(substitute(paste(italic("a) estimate of fixed"))), side = 3, at = -36,
          line = -1, cex = 1.5)
    mtext(substitute(paste(italic("intercept in a"))), side = 3, at = -37,
          line = -3, cex = 1.5)
    mtext(substitute(paste(italic("linear model"))), side = 3, at = -38,
          line = -5, cex = 1.5)
  }
  else{
    hist(results_lm[[i]][,1], 
         main = paste("number of levels:", i+1),
         xlab = "mean estimated intercept",
         breaks = 40, xlim = c(10,110), col = "grey",
         las = 1,ylim = c(0, 0.04), prob = T, cex.main = 1.8)
    curve(dnorm(x, mean=66, sd=13.2), 
          col="darkblue", lwd=2, add=TRUE, yaxt="n")
  }
  abline(v = 66, col = "red", lwd = 2)
}
### example glmer


results_glm = readRDS("./Results/results_intercept_glmer.Rds")
  for(i in 1:3){
    if(i == 1){
      hist(results_glm[[i]][,1],
           xlab = "mean estimated intercept",
           breaks = 40, xlim = c(0,4), col = "grey", cex.main = 4,
           prob = T, main = "", ylim = c(0,2.))
      curve(dnorm(x, mean=2, sd=0.4), 
            col="darkblue", lwd=2, add=TRUE, yaxt="n")
      #mtext("a)", side = 3, at = -0.1, line = -2, cex =2)
      mtext(substitute(paste(italic("b) estimate of fixed"))), side = 3, at = -1.8,
            line = -1, cex = 1.5)
      mtext(substitute(paste(italic("intercept in a"))), side = 3, at = -1.8,
            line = -3, cex = 1.5)
      mtext(substitute(paste(italic("binomial model"))), side = 3, at = -1.65,
            line = -5, cex = 1.5)
    }
    else{
      hist(results_glm[[i]][,1],
           xlab = "mean estimated intercept",
           breaks = 40, xlim = c(0,4), col = "grey", cex.main = 2,
           prob = T, main ="", ylim = c(0,2.))
      curve(dnorm(x, mean=2, sd=0.4), 
            col="darkblue", lwd=2, add=TRUE, yaxt="n")
    }
    abline(v = 2., col = "red", lwd = 2)
  }


