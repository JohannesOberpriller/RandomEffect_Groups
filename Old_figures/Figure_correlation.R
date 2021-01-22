library(viridis)

colors = viridis(4)
number_of_levels = 4
number_of_quantiles = 4
plot(x = seq(1,4,1),y =seq(1,4,1),col = colors, pch = 19)

# functions to get the max of each value for the axis limits
maxSD <- function(results_glmm){
  max(max(results_glmm[[1]][,7], na.rm=T), max(results_glmm[[2]][,7], na.rm=T), 
      max(results_glmm[[3]][,7], na.rm=T),max(results_glmm[[4]][,7], na.rm=T))
}
maxSEi <- function(results_glmm){
  max(max(results_glmm[[1]][,5], na.rm=T), max(results_glmm[[2]][,5], na.rm=T), 
      max(results_glmm[[3]][,5], na.rm=T),max(results_glmm[[4]][,5], na.rm=T))
}
maxSEs <- function(results_glmm){
  max(max(results_glmm[[1]][,6], na.rm=T), max(results_glmm[[2]][,6], na.rm=T), 
      max(results_glmm[[3]][,6], na.rm=T),max(results_glmm[[4]][,6], na.rm=T))
}

svg("./Figures/correlations.svg", width = 18, height = 15)
layout(matrix(c(1,2,3,4),nrow =2, ncol =2, byrow = T))

results_glmm <- readRDS("./Results/results_intercept_glmmtmb_normal.Rds")
upsd <- maxSD(results_glmm)
upse <- maxSEi(results_glmm)
par(mar=c(5,6.5,4,2))
plot(NULL, NULL, xlim = c(0, 40), ylim = c(0, 30), 
     main = "a) Normal random intercept",  cex.axis = 2.5,
     bty="l", 
     #xaxt = "n",yaxt = "n", 
     cex.main = 2.7, xlab = "", ylab = "",  las =1)
#axis(side = 1, at = seq(0,40,10), pos =0, las = 1, cex.axis = 2.5)
#axis(side = 2, at = seq(0,30,10), pos =0, las = 1, cex.axis = 2.5)
title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 2.7)
title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 2.7)
for(i in 1:number_of_levels) {
  points(x = results_glmm[[i]][,7], y = results_glmm[[i]][,5] , col = colors[i], cex=1, pch = 19)
}
quants = sapply(1:number_of_levels, function(i) quantile(results_glmm[[i]][,7], probs = c(0.7, 0.8,0.9,0.95)))
where = matrix(NA, number_of_quantiles, number_of_levels)
for(i in 1:number_of_levels){
  for(j in 1:number_of_quantiles){
    p = quants[j,i]
    ordered_results = results_glmm[[i]][,7][order(results_glmm[[i]][,7])]
    where[j,i] = which(ordered_results >= p)[1] 
  }
}
coords = sapply(1:number_of_levels, function(i) results_glmm[[i]][,5][order(results_glmm[[i]][,5])][where[,i]])
for(i in 1:number_of_quantiles){
  lines(y = coords[i,], x = quants[i,], lwd = 5, col = "black")
}
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 2.5)


results_glmm <- readRDS("./Results/results_slope_glmmtmb_normal.Rds")
upsd <- maxSD(results_glmm)
upse <- maxSEs(results_glmm)
plot(NULL, NULL, xlim = c(0, 1.8), ylim = c(0, 1.2), 
     main = "b) Normal random slope", cex.axis = 2.5,
     bty="l", 
    # xaxt = "n",yaxt = "n", 
     cex.main = 2.7, xlab = "", ylab = "", las =1)
#axis(side = 1, at = seq(0,1.8,0.3), pos =0, las =1, cex.axis = 2.5)
#axis(side = 2, at = seq(0,1.2,0.3), pos =0, las =1, cex.axis = 2.5)
title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 2.7)
title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 2.7)
for(i in 1:number_of_levels) {
  points(x = results_glmm[[i]][,7], y = results_glmm[[i]][,6] , col = colors[i], cex=1, pch = 19)
}
quants = sapply(1:number_of_levels, function(i) quantile(results_glmm[[i]][,7], probs = c(0.7, 0.8,0.9,0.95)))
where = matrix(NA, number_of_quantiles, number_of_levels)
for(i in 1:number_of_levels){
  for(j in 1:number_of_quantiles){
    p = quants[j,i]
    ordered_results = results_glmm[[i]][,7][order(results_glmm[[i]][,7])]
    where[j,i] = which(ordered_results >= p)[1] 
  }
}
coords = sapply(1:number_of_levels, function(i) results_glmm[[i]][,6][order(results_glmm[[i]][,6])][where[,i]])
for(i in 1:number_of_quantiles){
  lines(y = coords[i,], x = quants[i,], lwd = 5, col = "black")
}
legend(x = 1.2, y = 0.4, legend = c("5 levels", "4 levels", "3 levels","2 levels"),col = colors[4:1],
       pch = 19, bty = "n", cex = 2.5)
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 2.5)


results_glmm <- readRDS("./Results/results_intercept_glmmtmb_binomial.Rds")
upsd <- maxSD(results_glmm)
upse <- maxSEi(results_glmm)
plot(NULL, NULL, xlim = c(0, 1.6), ylim = c(0,1), 
     main = "c) Binomial random intercept", las =1,
     bty="l", 
    # xaxt = "n",yaxt = "n", 
     cex.axis = 2.5, cex.main = 2.7, xlab = "", ylab = "")
#axis(side = 1, at = seq(0,1.6,0.4), pos =0, las =1, cex.axis = 2.5)
#axis(side = 2, at = seq(0,1,0.2), pos =0, las =1, cex.axis = 2.5)
title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 2.7)
title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 2.7)
for(i in 1:number_of_levels) {
  points(x = results_glmm[[i]][,7], y = results_glmm[[i]][,5] , col = colors[i], cex=1, pch = 19)
}
quants = sapply(1:number_of_levels, function(i) quantile(results_glmm[[i]][,7], probs = c(0.7, 0.8,0.9,0.95)))
where = matrix(NA, number_of_quantiles, number_of_levels)
for(i in 1:number_of_levels){
  for(j in 1:number_of_quantiles){
    p = quants[j,i]
    ordered_results = results_glmm[[i]][,7][order(results_glmm[[i]][,7])]
    where[j,i] = which(ordered_results >= p)[1] 
  }
}
coords = sapply(1:number_of_levels, function(i) results_glmm[[i]][,5][order(results_glmm[[i]][,5])][where[,i]])
for(i in 1:number_of_quantiles){
  lines(y = coords[i,], x = quants[i,], lwd = 5, col = "black")
}
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 2.5)


results_glmm <- readRDS("./Results/results_slope_glmmtmb_binomial.Rds")
upsd <- maxSD(results_glmm)
upse <- maxSEs(results_glmm)
plot(NULL, NULL, xlim = c(0, 1.2), ylim = c(0, 0.8), 
     main = "d) Binomial random slope",  las =1,
     bty="l", 
     #xaxt = "n",yaxt = "n", 
     cex.axis = 2.5, cex.main = 2.7, xlab = "", ylab = "")
#axis(side = 1, at = seq(0,1.2,0.3), pos =0, las =1, cex.axis = 2.5)
#axis(side = 2, at = seq(0,0.8,0.2), pos =0, las =1, cex.axis = 2.5)
title(ylab = "Standard error of fixed effect", line = .24, cex.lab = 2.7)
title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 2.7)
for(i in 1:number_of_levels) {
  points(x = results_glmm[[i]][,7], y = results_glmm[[i]][,6] , col = colors[i], cex=1, pch = 19)
}
quants = sapply(1:number_of_levels, function(i) quantile(results_glmm[[i]][,7], probs = c(0.7, 0.8,0.9,0.95)))
where = matrix(NA, number_of_quantiles, number_of_levels)
for(i in 1:number_of_levels){
  for(j in 1:number_of_quantiles){
    p = quants[j,i]
    ordered_results = results_glmm[[i]][,7][order(results_glmm[[i]][,7])]
    where[j,i] = which(ordered_results >= p)[1] 
  }
}
coords = sapply(1:number_of_levels, function(i) results_glmm[[i]][,6][order(results_glmm[[i]][,6])][where[,i]])
for(i in 1:number_of_quantiles){
  lines(y = coords[i,], x = quants[i,], lwd = 5, col = "black")
}
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 2.5)
dev.off()
