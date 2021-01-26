
results_lmm = readRDS("Results/results_mountain_lmm.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_200_.Rds")

addA = function(col, alpha = 0.25) apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha)) 



########## Figure 1 Type I, Error, etc. for lmm ########## 
get_p_from_z = function(obj) {
  z = obj$estimate_effect / obj$se_effect
  p = 2*stats::pnorm(abs(z),lower.tail = FALSE)
  return(p)
}

get_confidence_interval = function(object) {
  conf = cbind(
    object$results_w_lme4_reml$estimate_effect - 1.96*object$results_w_lme4_reml$se_effect, 
    object$results_w_lme4_reml$estimate_effect + 1.96*object$results_w_lme4_reml$se_effect)
  return(as.integer(0.4 > conf[,1] & 0.4 < conf[,2]))
}


cols = RColorBrewer::brewer.pal(3, "Set1")
cols = c(cols[1], cols)
lty = c(1, 2, 1, 1)
## Type I error ##
pdf(file = "Figures/Fig_1.pdf", width = 7, height = 4.5)
par(mfrow = c(2,3), mar = c(0.1, 2, 1, 1), oma = c(5, 3, 3, 1)-1)
type_one_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)), #lme4
    sapply(results_lmm, function(l) mean(get_p_from_z(l$results_wo_lme4_reml)< 0.05)),
    sapply(results_lmm, function(l) mean(l$results_wo_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "", xpd = NA)
text(x=-0.2, pos = 2, y = 0.55, labels = "A", cex = 1.2, xpd = NA, font = 2)
text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
legend("topright", legend = c("lme4 t-statistics", "lme4 z-statistics", "glmmTMB z-statistics", "lm"), col = cols, pch = 15:18, bty = "n", lty = lty)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_wo_lme4_reml$p_value_effect < 0.05)), #lme4
    sapply(results_lmm, function(l) sd(l$results_wo_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) sd(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)) #lm
  )
mm =type_one_int[,-2]
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[-1][i], 0.10)))
#text(-0.5, 0.55, labels = "A", cex = 1.3, font = 2, xpd =NA)
#axis(1, at = 1:7, labels = 2:8)

## Power ##
type_two_int = 
  cbind(
    sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect < 0.05)), #lme4
    sapply(results_lmm, function(l) 1-mean(get_p_from_z(l$results_w_lme4_reml)< 0.05)), #lme4
    sapply(results_lmm, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )
matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of levels")
text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")

legend("bottomright", legend = c("lme4 t-statistics", "lme4 z-statistics", "glmmTMB z-statistics", "lm"), col = cols, pch = 15:18, bty = "n", lty = lty)
sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$p_value_effect < 0.05)), #lme4
    sapply(results_lmm, function(l) sd(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) sd(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )
mm =1-type_two_int[,-2]
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[-1][i], 0.10)))
#axis(1, at = 1:7, labels = 2:8)

# coverage
type_two_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_w_lme4_reml$Slope_in_conf )), #lme4
    sapply(results_lmm, function(l) mean(get_confidence_interval(l ), na.rm=TRUE)),
    sapply(results_lmm, function(l) mean(l$results_w_glmmTMB_reml$Slope_in_conf, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) mean(l$results_w_lm$Slope_in_conf , na.rm=TRUE)) #lm
  )

matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "")
text(x= 4, pos = 3, y = 1.02, xpd = NA, labels = "Coverage")

legend("bottomright", legend = c("lme4 t-statistics", "lme4 z-statistics", "glmmTMB z-statistics", "lm"), col = cols, pch = 15:18, bty = "n", lty = lty)
abline(h = 0.95, lty = 3, col = "darkgrey")

sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$Slope_in_conf )), #lme4
    sapply(results_lmm, function(l) sd(l$results_w_glmmTMB_reml$Slope_in_conf, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) sd(l$results_w_lm$Slope_in_conf , na.rm=TRUE)) #lm
  )
mm =type_two_int[,-2]
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[-1][i], 0.10)))
#axis(1, at = 1:7, labels = 2:8)



## glmm
cols = RColorBrewer::brewer.pal(3, "Set1")
type_one_int = 
  cbind(
    sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_effect < 0.05)), #lme4
    sapply(results_glmm, function(l) mean(l$results_wo_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:17, las = 1, lty = 1, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "Number of mountain ranges", xpd = NA)
text(x=-0.2, pos = 2, y = 0.55, labels = "B", cex = 1.2, xpd = NA, font = 2)

axis(1, at = 1:7, labels = 2:8)
legend("topright", legend =c("lme4 z-statistics", "glmmTMB z-statistics", "lm"), col = cols, pch = 15:17, bty = "n")
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_wo_lme4_ml$p_value_effect < 0.05)), #lme4
    sapply(results_glmm, function(l) sd(l$results_wo_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_glmm, function(l) sd(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)) #lm
  )
mm =type_one_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
#text(-0.5, 0.55, labels = "B", cex = 1.3, font = 2, xpd =NA)


## Power ##
cols = RColorBrewer::brewer.pal(3, "Set1")
type_two_int = 
  cbind(
    sapply(results_glmm, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect < 0.05)), #lme4
    sapply(results_glmm, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )

matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:17, las = 1, lty = 1, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountain ranges", xpd = NA)
axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = c("lme4 z-statistics", "glmmTMB z-statistics", "lm"), col = cols, pch = 15:17, bty = "n")
sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$p_value_effect < 0.05)), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_glmm, function(l) sd(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )
mm =1-type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))


## Coverage ##
cols = RColorBrewer::brewer.pal(3, "Set1")
type_two_int = 
  cbind(
    sapply(results_glmm, function(l) mean(l$results_w_lme4_ml$Slope_in_conf )), #lme4
    sapply(results_glmm, function(l) mean(l$results_w_glmmTMB_reml$Slope_in_conf, na.rm=TRUE)), # glmmTMB
    sapply(results_glmm, function(l) mean(l$results_w_lm$Slope_in_conf , na.rm=TRUE)) #lm
  )


matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:17, las = 1, lty = 1, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountain ranges", xpd = NA)
abline(h = 0.95, lty = 3, col = "darkgrey")

axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = c("lme4 z-statistics", "glmmTMB z-statistics", "lm"), col = cols, pch = 15:17, bty = "n")

sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$Slope_in_conf )), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$Slope_in_conf, na.rm=TRUE)), # glmmTMB
    sapply(results_glmm, function(l) sd(l$results_w_lm$Slope_in_conf , na.rm=TRUE)) #lm
  )
mm =type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))



dev.off()







########## Figure S1 different number of observations for GLMM ########## 
files = c("Results/results_mountain_glmm_50_.Rds", 
          "Results/results_mountain_glmm_100_.Rds", 
          "Results/results_mountain_glmm_200_.Rds", 
          "Results/results_mountain_glmm_500_.Rds")

pdf("Figures/Fig_S1.pdf", , width = 7, height = 8.5)
par(mfrow = c(4,3), mar = c(0.1, 2, 1, 1), oma = c(5, 5, 3, 1)-1)
labels = c("A", "B", "C", "D")
for(i in 1:4) {
  
  results_glmm = readRDS(files[i])
  
  if(i == 4) xlab = "Number of mountain ranges"
  else xlab = ""
  
  ## Type I error ##
  cols = RColorBrewer::brewer.pal(3, "Set1")
  type_one_int = 
    cbind(
      sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_effect < 0.05)), #lme4
      sapply(results_glmm, function(l) mean(l$results_wo_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
      sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:17, las = 1, lty = 1, col = cols, 
          ylab = "Rate", xaxt="n", main = "", xlab = xlab, xpd = NA)
  text(x=-0.2, pos = 2, y = 0.55, labels = labels[i], cex = 1.2, xpd = NA, font = 2)
  
  if(i == 4) axis(1, at = 1:7, labels = 2:8)
  legend("topright", legend =c("lme4 z-statistics", "glmmTMB z-statistics", "lm"), col = cols, pch = 15:17, bty = "n")
  abline(h = 0.05, lty = 3, col = "darkgrey")
  sd = 
    cbind(
      sapply(results_glmm, function(l) sd(l$results_wo_lme4_ml$p_value_effect < 0.05)), #lme4
      sapply(results_glmm, function(l) sd(l$results_wo_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
      sapply(results_glmm, function(l) sd(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)) #lm
    )
  mm =type_one_int
  upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
  lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
  sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  #text(-0.5, 0.55, labels = "B", cex = 1.3, font = 2, xpd =NA)
  
  
  ## Power ##
  cols = RColorBrewer::brewer.pal(3, "Set1")
  type_two_int = 
    cbind(
      sapply(results_glmm, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect < 0.05)), #lme4
      sapply(results_glmm, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:17, las = 1, lty = 1, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd = NA)
  if(i == 4) axis(1, at = 1:7, labels = 2:8)
  if(i > 2) position = "bottomright"
  else position = "topleft"
  legend(position, legend = c("lme4 z-statistics", "glmmTMB z-statistics", "lm"), col = cols, pch = 15:17, bty = "n")
  sd = 
    cbind(
      sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$p_value_effect < 0.05)), #lme4
      sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
      sapply(results_glmm, function(l) sd(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
    )
  mm =1-type_two_int
  upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
  lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
  sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
  
  ## Coverage ##
  cols = RColorBrewer::brewer.pal(3, "Set1")
  type_two_int = 
    cbind(
      sapply(results_glmm, function(l) mean(l$results_w_lme4_ml$Slope_in_conf )), #lme4
      sapply(results_glmm, function(l) mean(l$results_w_glmmTMB_reml$Slope_in_conf, na.rm=TRUE)), # glmmTMB
      sapply(results_glmm, function(l) mean(l$results_w_lm$Slope_in_conf , na.rm=TRUE)) #lm
    )
  
  
  matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:17, las = 1, lty = 1, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd = NA)
  abline(h = 0.95, lty = 3, col = "darkgrey")
  
  if(i == 4) axis(1, at = 1:7, labels = 2:8)
  legend("bottomright", legend = c("lme4 z-statistics", "glmmTMB z-statistics", "lm"), col = cols, pch = 15:17, bty = "n")
  
  sd = 
    cbind(
      sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$Slope_in_conf )), #lme4
      sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$Slope_in_conf, na.rm=TRUE)), # glmmTMB
      sapply(results_glmm, function(l) sd(l$results_w_lm$Slope_in_conf , na.rm=TRUE)) #lm
    )
  mm =type_two_int
  upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
  lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
  sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
}  


dev.off()


results_glmm = readRDS("Results/results_mountain_glmm_200_.Rds")




########## Figure 2 Variance estimates lmm ########## 

cols = viridis::viridis(5)

ff = function(i) {
  if(i == 1) return(cols[1])
  if(i == 2) return(cols[2])
  if(i == 4) return(cols[3])
  if(i == 7) return(cols[4])
}

adj = 1.0
pdf(file = "Figures/Fig_2.pdf", width = 7, height = 5)

par(mfrow = c(2,2), mar = c(2, 1, 4, 2))
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 10.0), axes= FALSE, ylab = "", main = "SD of random intercept", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 10, labels = "A", cex = 1.3, font = 2, xpd =NA)
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 10.0), axes= FALSE, ylab = "", main = "SD of random slope", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)



plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 10.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.04, 10, labels = "B", cex = 1.3, font = 2, xpd =NA)

plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 10.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)

dev.off()



########## Figure 3 correlation plot ###########

pdf("./Figures/Fig_3.pdf", width = 9, height = 7.5)
par(mfrow = c(2,2), mar=c(1, 2, 1, 1), oma = c(5, 5, 3, 1))
plot(NULL, NULL, xlim = c(0, 0.5), ylim = c(0, 0.4), 
     main = "",  cex.axis = 1.0, xaxt="n",
     cex.main = 1.0, xlab = "", ylab = "",  las =1, xpd = NA)
text(x = 0.25, y = 0.42, pos = 3, labels = "Intercept", xpd = NA)
text(x = -0.15, y = 0.43, labels = "A", font = 2, cex = 1.2, xpd = NA)
title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 1, xpd = NA)
#title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1)
levels = c(2, 3, 5, 8)-1
for(i in 1:number_of_levels) {
  points(x = results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_inter, 
         y = results_lmm[[levels[i]]]$results_w_lme4_reml$se_intercept , col = colors[i], cex=0.2, pch = 19)
}
quants = sapply(1:number_of_levels, function(i) quantile(results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_inter, probs = c(0.7, 0.8,0.9,0.95)))
where = matrix(NA, number_of_quantiles, number_of_levels)
for(i in 1:number_of_levels){
  for(j in 1:number_of_quantiles){
    p = quants[j,i]
    ordered_results = results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_inter[order(results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_inter)]
    where[j,i] = which(ordered_results >= p)[1] 
  }
}
coords = sapply(1:number_of_levels, function(i) results_lmm[[levels[i]]]$results_w_lme4_reml$se_intercept[order(results_lmm[[levels[i]]]$results_w_lme4_reml$se_intercept)][where[,i]])
for(i in 1:number_of_quantiles){
  lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
}
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)
legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")



plot(NULL, NULL, xlim = c(0, 0.5), ylim = c(0, 0.4), 
     main = "",  cex.axis = 1.0,xaxt="n",
     cex.main = 1.0, xlab = "", ylab = "",  las =1, xpd=NA)
text(x = 0.25, y = 0.42, pos = 3, labels = "Temperature", xpd = NA)

title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 1)
title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1)
for(i in 1:number_of_levels) {
  points(x = results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_x,
         y = results_lmm[[levels[i]]]$results_w_lme4_reml$se_effect , col = colors[i], cex=0.2, pch = 19)
}
quants = sapply(1:number_of_levels, function(i) quantile(results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_x, probs = c(0.7, 0.8,0.9,0.95)))
where = matrix(NA, number_of_quantiles, number_of_levels)
for(i in 1:number_of_levels){
  for(j in 1:number_of_quantiles){
    p = quants[j,i]
    ordered_results = results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_x[order(results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_x)]
    where[j,i] = which(ordered_results >= p)[1] 
  }
}
coords = sapply(1:number_of_levels, function(i) results_lmm[[levels[i]]]$results_w_lme4_reml$se_effect[order(results_lmm[[levels[i]]]$results_w_lme4_reml$se_effect)][where[,i]])
for(i in 1:number_of_quantiles){
  lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
}
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)
legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")




## GLMM
plot(NULL, NULL, xlim = c(0, 0.6), ylim = c(0, 0.5), 
     main = "",  cex.axis = 1.0,
     cex.main = 1.0, xlab = "", ylab = "",  las =1)
title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 1, xpd = NA)
title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1, xpd = NA)
text(x = -0.18, y = 0.5375, labels = "B", font = 2, cex = 1.2, xpd = NA)
for(i in 1:number_of_levels) {
  points(x = results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_inter, 
         y = results_glmm[[levels[i]]]$results_w_lme4_ml$se_intercept , col = colors[i], cex=0.2, pch = 19)
}
quants = sapply(1:number_of_levels, function(i) quantile(results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_inter, probs = c(0.7, 0.8,0.9,0.95)))
where = matrix(NA, number_of_quantiles, number_of_levels)
for(i in 1:number_of_levels){
  for(j in 1:number_of_quantiles){
    p = quants[j,i]
    ordered_results = results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_inter[order(results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_inter)]
    where[j,i] = which(ordered_results >= p)[1] 
  }
}
coords = sapply(1:number_of_levels, function(i) results_glmm[[levels[i]]]$results_w_lme4_ml$se_intercept[order(results_glmm[[levels[i]]]$results_w_lme4_ml$se_intercept)][where[,i]])
for(i in 1:number_of_quantiles){
  lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
}
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)

legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")


plot(NULL, NULL, xlim = c(0, 0.9), ylim = c(0, 0.6), 
     main = "",  cex.axis = 1.0,
     cex.main = 1.0, xlab = "", ylab = "",  las =1)
title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1, xpd=NA)
for(i in 1:number_of_levels) {
  points(x = results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_x,
         y = results_glmm[[levels[i]]]$results_w_lme4_ml$se_effect , col = colors[i], cex=0.1, pch = 19)
}
quants = sapply(1:number_of_levels, function(i) quantile(results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_x, probs = c(0.7, 0.8,0.9,0.95)))
where = matrix(NA, number_of_quantiles, number_of_levels)
for(i in 1:number_of_levels){
  for(j in 1:number_of_quantiles){
    p = quants[j,i]
    ordered_results = results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_x[order(results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_x)]
    where[j,i] = which(ordered_results >= p)[1] 
  }
}
coords = sapply(1:number_of_levels, function(i) results_glmm[[levels[i]]]$results_w_lme4_ml$se_effect[order(results_glmm[[levels[i]]]$results_w_lme4_ml$se_effect)][where[,i]])
for(i in 1:number_of_quantiles){
  lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
}
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)

legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")

dev.off()







########## Figure S2 variance estimate+std error########## 


cols = viridis::viridis(5)

ff = function(i) {
  if(i == 1) return(cols[1])
  if(i == 2) return(cols[2])
  if(i == 4) return(cols[3])
  if(i == 7) return(cols[4])
}

adj = 1.0
pdf(file = "Figures/Fig_S2.pdf", width = 7, height = 5)

par(mfrow = c(2,2), mar = c(2, 1, 4, 2))
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 10.0), axes= FALSE, ylab = "", main = "SD of random intercept", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols[1:4], bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 10, labels = "A", cex = 1.3, font = 2, xpd =NA)
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 37.0), axes= FALSE, ylab = "", main = "SE of Intercept", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$se_intercept, adjust = adj), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols, bty = "n")
#abline(v=0.1, col="darkgrey", lty = 3)



plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 10.0), axes= FALSE, ylab = "", main = "SD of random slope", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_x, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.04, 10, labels = "B", cex = 1.3, font = 2, xpd =NA)

plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 37.0), axes= FALSE, ylab = "", main = "SE of Temperature effect", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$se_effect, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)

dev.off()


########## Figure S3 Variance estimates lmm ML/REML ########## 
cols = viridis::viridis(5)

ff = function(i) {
  if(i == 1) return(cols[1])
  if(i == 2) return(cols[2])
  if(i == 4) return(cols[3])
  if(i == 7) return(cols[4])
}

adj = 1.0
pdf(file = "Figures/Fig_S3.pdf", width = 7, height = 5)

par(mfrow = c(2,2), mar = c(2, 1+0.3, 4, 2))

# lme4
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 24.0), axes= FALSE, ylab = "", main = "SD of random intercept", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols[1:4], bty = "n")
legend("bottomright", legend = c("REML", "ML"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.04, 24, labels = "A", cex = 1.3, font = 2, xpd =NA)


plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 24.0), axes= FALSE, ylab = "", main = "SD of random slope", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_ml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)

axis(1)
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("REML", "ML"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)


# glmmTMB
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 24.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("REML", "ML"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.05, 24, labels = "B", cex = 1.3, font = 2, xpd =NA)


plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 24.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)

axis(1)
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("REML", "ML"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)
dev.off()



########## Figure S4 Variance estimates glmm ########## 
cols = viridis::viridis(5)

ff = function(i) {
  if(i == 1) return(cols[1])
  if(i == 2) return(cols[2])
  if(i == 4) return(cols[3])
  if(i == 7) return(cols[4])
}

# glmm4

adj = 1.0
pdf(file = "Figures/Fig_S4.pdf", width = 7, height = 5)

par(mfrow = c(2,2), mar = c(2, 1+0.3, 4, 2))
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 24.0), axes= FALSE, ylab = "", main = "SD of random intercept", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_glmm[[i]]$results_w_lme4_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols[1:4], bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)

text(-0.04, 25, labels = "A", cex = 1.3, font = 2, xpd =NA)

plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 24.0), axes= FALSE, ylab = "", main = "SD of random slope", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_ml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)


# glmmTMB ML
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 24.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_ml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4, lty = 2)

axis(1)
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("REML", "ML"), lty = c(1, 2), bty = "n", lwd = 1.0)

abline(v=0.1, col="darkgrey", lty = 3)
text(-0.05, 25, labels = "B", cex = 1.3, font = 2, xpd =NA)

plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 24.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_x, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_ml$stddev_randeff_x, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4, lty = 2)

axis(1)
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("REML", "ML"), lty = c(1, 2), bty = "n", lwd = 1.0)

abline(v=0.1, col="darkgrey", lty = 3)
dev.off()







########## Figure S5 Singularities:Type I ########## 
pdf(file = "Figures/Fig_S5.pdf", width = 7, height = 3)
par(mfrow = c(1,3), mar = c(0.1+5, 4, 2, 1), oma = c(1, 1, 1, 1)-1)
cols = RColorBrewer::brewer.pal(3, "Set1")


sing = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 1] < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 0] < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect < 0.05, na.rm = TRUE)))

matplot(sing, type="o", ylim = c(0, 0.5), pch = 15:17, las = 1, col = cols, 
        ylab = "Rate", xaxt="n", main = "lmm - lme4", xlab = "", lty = 1)
legend("topright", legend = c("singularity", "no singularity", "average" ), col = cols, pch = 15:17, bty = "n")
abline(h = 0.05, lty = 3, col = "darkgrey")
axis(1, at = 1:7, labels = 2:8)


sing = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_glmmTMB_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 1]  < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_wo_glmmTMB_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 0]  < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_wo_glmmTMB_reml$p_value_effect < 0.05, na.rm = TRUE)))

matplot(sing, type="o", ylim = c(0, 0.5), pch = 15:17, las = 1, col = cols, 
        ylab = "Rate", xaxt="n", main = "lmm - glmmTMB", xlab = "", lty = 1)
legend("topright", legend = c("singularity", "no singularity", "average" ), col = cols, pch = 15:17, bty = "n")
abline(h = 0.05, lty = 3, col = "darkgrey")
axis(1, at = 1:7, labels = 2:8)


sing = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity == 1]  < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity == 0]  < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect < 0.05, na.rm = TRUE)))

matplot(sing, type="o", ylim = c(0, 0.5), pch = 15:17, las = 1, col = cols, 
        ylab = "Rate", xaxt="n", main = "lm", xlab = "", lty = 1)
legend("topright", legend = c("singularity", "no singularity", "average" ), col = cols, pch = 15:17, bty = "n")
abline(h = 0.05, lty = 3, col = "darkgrey")
axis(1, at = 1:7, labels = 2:8)
dev.off()



########## Figure S6 Singularities: Power ########## 
pdf(file = "Figures/Fig_S6.pdf", width = 7, height = 3)
par(mfrow = c(1,3), mar = c(0.1+5, 4, 2, 1), oma = c(1, 1, 1, 1)-1)
cols = RColorBrewer::brewer.pal(3, "Set1")


sing = 
  cbind(
    sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity == 1] < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity == 0] < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect < 0.05, na.rm = TRUE)))

matplot(1-sing, type="o", ylim = c(0, 1), pch = 15:17, las = 1, col = cols, 
        ylab = "Rate", xaxt="n", main = "lmm - lme4", xlab = "", lty = 1)
legend("bottomright", legend = c("singularity", "no singularity", "average" ), col = cols, pch = 15:17, bty = "n")
abline(h = 0.05, lty = 3, col = "darkgrey")
axis(1, at = 1:7, labels = 2:8)


sing = 
  cbind(
    sapply(results_lmm, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_effect[l$results_w_lme4_reml$Singularity == 1]  < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_effect[l$results_w_lme4_reml$Singularity == 0]  < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm = TRUE)))

matplot(1-sing, type="o", ylim = c(0, 1), pch = 15:17, las = 1, col = cols, 
        ylab = "Rate", xaxt="n", main = "lmm - glmmTMB", xlab = "", lty = 1)
legend("bottomright", legend = c("singularity", "no singularity", "average" ), col = cols, pch = 15:17, bty = "n")
abline(h = 0.05, lty = 3, col = "darkgrey")
axis(1, at = 1:7, labels = 2:8)


sing = 
  cbind(
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity == 1]  < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity == 0]  < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm = TRUE)))

matplot(1-sing, type="o", ylim = c(0, 1), pch = 15:17, las = 1, col = cols, 
        ylab = "Rate", xaxt="n", main = "lm", xlab = "", lty = 1)
legend("bottomright", legend = c("singularity", "no singularity", "average" ), col = cols, pch = 15:17, bty = "n")
abline(h = 0.05, lty = 3, col = "darkgrey")
axis(1, at = 1:7, labels = 2:8)
dev.off()











############### Figure S7 Correlation plot for glmmTMB #################

pdf("./Figures/Fig_S7.pdf", width = 9, height = 7.5)
par(mfrow = c(2,2), mar=c(1, 2, 1, 1), oma = c(5, 5, 3, 1))
plot(NULL, NULL, xlim = c(0, 0.5), ylim = c(0, 0.4), 
     main = "",  cex.axis = 1.0, xaxt="n",
     cex.main = 1.0, xlab = "", ylab = "",  las =1, xpd = NA)
text(x = 0.25, y = 0.42, pos = 3, labels = "Intercept", xpd = NA)
text(x = -0.15, y = 0.43, labels = "A", font = 2, cex = 1.2, xpd = NA)
title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 1, xpd = NA)
#title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1)
levels = c(2, 3, 5, 8)-1
for(i in 1:number_of_levels) {
  points(x = results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter, 
         y = results_lmm[[levels[i]]]$results_w_glmmTMB_reml$se_intercept , col = colors[i], cex=0.2, pch = 19)
}
quants = sapply(1:number_of_levels, function(i) quantile(results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter, probs = c(0.7, 0.8,0.9,0.95), na.rm = TRUE))
where = matrix(NA, number_of_quantiles, number_of_levels)
for(i in 1:number_of_levels){
  for(j in 1:number_of_quantiles){
    p = quants[j,i]
    ordered_results = results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter[order(results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter)]
    where[j,i] = which(ordered_results >= p)[1] 
  }
}
coords = sapply(1:number_of_levels, function(i) results_lmm[[levels[i]]]$results_w_glmmTMB_reml$se_intercept[order(results_lmm[[levels[i]]]$results_w_glmmTMB_reml$se_intercept)][where[,i]])
for(i in 1:number_of_quantiles){
  lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
}
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)
legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")



plot(NULL, NULL, xlim = c(0, 0.5), ylim = c(0, 0.4), 
     main = "",  cex.axis = 1.0,xaxt="n",
     cex.main = 1.0, xlab = "", ylab = "",  las =1, xpd=NA)
text(x = 0.25, y = 0.42, pos = 3, labels = "Temperature", xpd = NA)

title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 1)
title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1)
for(i in 1:number_of_levels) {
  points(x = results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x,
         y = results_lmm[[levels[i]]]$results_w_glmmTMB_reml$se_effect , col = colors[i], cex=0.2, pch = 19)
}
quants = sapply(1:number_of_levels, function(i) quantile(results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x, probs = c(0.7, 0.8,0.9,0.95), na.rm = TRUE))
where = matrix(NA, number_of_quantiles, number_of_levels)
for(i in 1:number_of_levels){
  for(j in 1:number_of_quantiles){
    p = quants[j,i]
    ordered_results = results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x[order(results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x)]
    where[j,i] = which(ordered_results >= p)[1] 
  }
}
coords = sapply(1:number_of_levels, function(i) results_lmm[[levels[i]]]$results_w_glmmTMB_reml$se_effect[order(results_lmm[[levels[i]]]$results_w_glmmTMB_reml$se_effect)][where[,i]])
for(i in 1:number_of_quantiles){
  lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
}
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)
legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")




## GLMM
plot(NULL, NULL, xlim = c(0, 0.6), ylim = c(0, 0.5), 
     main = "",  cex.axis = 1.0,
     cex.main = 1.0, xlab = "", ylab = "",  las =1)
title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 1, xpd = NA)
title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1, xpd = NA)
text(x = -0.18, y = 0.5375, labels = "B", font = 2, cex = 1.2, xpd = NA)
for(i in 1:number_of_levels) {
  points(x = results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter, 
         y = results_glmm[[levels[i]]]$results_w_glmmTMB_reml$se_intercept , col = colors[i], cex=0.2, pch = 19)
}
quants = sapply(1:number_of_levels, function(i) quantile(results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter, probs = c(0.7, 0.8,0.9,0.95), na.rm = TRUE))
where = matrix(NA, number_of_quantiles, number_of_levels)
for(i in 1:number_of_levels){
  for(j in 1:number_of_quantiles){
    p = quants[j,i]
    ordered_results = results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter[order(results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter)]
    where[j,i] = which(ordered_results >= p)[1] 
  }
}
coords = sapply(1:number_of_levels, function(i) results_glmm[[levels[i]]]$results_w_glmmTMB_reml$se_intercept[order(results_glmm[[levels[i]]]$results_w_glmmTMB_reml$se_intercept)][where[,i]])
for(i in 1:number_of_quantiles){
  lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
}
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)

legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")


plot(NULL, NULL, xlim = c(0, 0.9), ylim = c(0, 0.6), 
     main = "",  cex.axis = 1.0,
     cex.main = 1.0, xlab = "", ylab = "",  las =1)
title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1, xpd=NA)
for(i in 1:number_of_levels) {
  points(x = results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x,
         y = results_glmm[[levels[i]]]$results_w_glmmTMB_reml$se_effect , col = colors[i], cex=0.1, pch = 19)
}
quants = sapply(1:number_of_levels, function(i) quantile(results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x, probs = c(0.7, 0.8,0.9,0.95), na.rm = TRUE))
where = matrix(NA, number_of_quantiles, number_of_levels)
for(i in 1:number_of_levels){
  for(j in 1:number_of_quantiles){
    p = quants[j,i]
    ordered_results = results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x[order(results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x)]
    where[j,i] = which(ordered_results >= p)[1] 
  }
}
coords = sapply(1:number_of_levels, function(i) results_glmm[[levels[i]]]$results_w_glmmTMB_reml$se_effect[order(results_glmm[[levels[i]]]$results_w_glmmTMB_reml$se_effect)][where[,i]])
for(i in 1:number_of_quantiles){
  lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
}
text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)

legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")

dev.off()





