library(viridis)

colors = viridis(4)
number_of_levels = 4
number_of_quantiles = 4
plot(x = seq(1,4,1),y =seq(1,4,1),col = colors, pch = 19)

svg("./Figures/correlations.svg", width = 18, height = 15)
layout(matrix(c(1,2,3,4),nrow =2, ncol =2, byrow = T))
results_glmm <- readRDS("./Results/results_intercept_glmer.Rds")
plot(NULL, NULL, xlim = c(0, 1.1*max(results_glmm[[4]][,7])), ylim = c(0., 1.1*max(results_glmm[[4]][,5])), 
     main = "a) Normal random intercept",
     bty="n", xaxt = "n",yaxt = "n", cex.lab = 1.5, cex.main = 2, xlab = "", ylab = "")
axis(side = 1, at = seq(0,1,0.2), labels = seq(0,1,0.2), pos =0, las =1)
axis(side = 2, at = seq(0,1,0.2), labels = seq(0,1,0.2), pos =0, las = 1)
title(ylab = "Standard deviation of random effect", line = 0.2, cex.lab = 1.5)
title(xlab = "Standard error of fixed effect", line = 1., cex.lab = 1.5)
for(i in 1:number_of_levels) {
  points(x = results_glmm[[i]][,7], y = results_glmm[[i]][,5] , col = colors[i], cex = 0.2, pch = 19)
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
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1.5)


results_glmm <- readRDS("./Results/results_slope_glmer.Rds")
plot(NULL, NULL, xlim = c(0, 1.1*max(results_glmm[[4]][,7])), ylim = c(0., 1.1*max(results_glmm[[4]][,6])), 
     main = "b) Normal random slope",
     bty="n", xaxt = "n",yaxt = "n", cex.lab = 1.5, cex.main = 2, xlab = "", ylab = "")
axis(side = 1, at = seq(0,1,0.2), labels = seq(0,1,0.2), pos =0, las =1)
axis(side = 2, at = seq(0,1,0.2), labels = seq(0,1,0.2), pos =0, las =1)
title(ylab = "Standard deviation of random effect", line = 0.2, cex.lab = 1.5)
title(xlab = "Standard error of fixed effect", line = 1., cex.lab = 1.5)
for(i in 1:number_of_levels) {
  points(x = results_glmm[[i]][,7], y = results_glmm[[i]][,6] , col = colors[i], cex = 0.2, pch = 19)
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
legend(x = 0.7, y = 0.3, legend = c("5 levels", "4 levels", "3 levels","2 levels"),col = colors[4:1],
       pch = 19, bty = "n", cex = 2)
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1.5)


results_glmm <- readRDS("./Results/results_intercept_glmmtmb_binomial.Rds")
plot(NULL, NULL, xlim = c(0, 1.1*max(results_glmm[[4]][,7])), ylim = c(0., 1.1*max(results_glmm[[4]][,5])), 
     main = "c) Binomial random intercept",
     bty="n", xaxt = "n",yaxt = "n", cex.lab = 1.5, cex.main = 2, xlab = "", ylab = "")
axis(side = 1, at = seq(0,1,0.2), labels = seq(0,1,0.2), pos =0, las =1)
axis(side = 2, at = seq(0,1,0.2), labels = seq(0,1,0.2), pos =0, las =1)
title(ylab = "Standard deviation of random effect", line = 0.2, cex.lab = 1.5)
title(xlab = "Standard error of fixed effect", line = 1., cex.lab = 1.5)
for(i in 1:number_of_levels) {
  points(x = results_glmm[[i]][,7], y = results_glmm[[i]][,5] , col = colors[i], cex = 0.2, pch = 19)
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
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1.5)


results_glmm <- readRDS("./Results/results_slope_glmmtmb_binomial.Rds")
plot(NULL, NULL, xlim = c(0, 1.1*max(results_glmm[[4]][,7])), ylim = c(0., 1.1*max(results_glmm[[4]][,6])), 
     main = "d) Binomial random slope",
     bty="n", xaxt = "n",yaxt = "n", cex.lab = 1.5, cex.main = 2, xlab = "", ylab = "")
axis(side = 1, at = seq(0,1,0.2), labels = seq(0,1,0.2), pos =0, las =1)
axis(side = 2, at = seq(0,1,0.2), labels = seq(0,1,0.2), pos =0, las =1)
title(ylab = "Standard deviation of random effect", line = 0.2, cex.lab = 1.5)
title(xlab = "Standard error of fixed effect", line = 1., cex.lab = 1.5)
for(i in 1:number_of_levels) {
  points(x = results_glmm[[i]][,7], y = results_glmm[[i]][,6] , col = colors[i], cex = 0.2, pch = 19)
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
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1.5)
dev.off()
