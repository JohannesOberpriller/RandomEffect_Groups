
results_lmm = readRDS("Results/results_mountain_lmm.Rds")
results_glmm = readRDS("Results/results_mountain_glmm.Rds")

addA = function(col, alpha = 0.25) apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha)) 



########## Figure 1 Type I, Error, etc. for lmm ########## 
cols = RColorBrewer::brewer.pal(3, "Set1")
## Type I error ##
pdf(file = "Figures/Fig_1.pdf", width = 7, height = 3)
par(mfrow = c(1,3), mar = c(0.1+5, 4, 2, 1), oma = c(1, 1, 1, 1)-1)
type_one_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)), #lme4
    sapply(results_lmm, function(l) mean(l$results_wo_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:17, las = 1, lty = 1, col = cols, 
        ylab = "Rate", xaxt="n", main = "Type I Error", xlab = "")
legend("topright", legend = c("lmm t-values", "lmm z-values", "lm"), col = cols, pch = 15:17, bty = "n")
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_wo_lme4_reml$p_value_effect < 0.05)), #lme4
    sapply(results_lmm, function(l) sd(l$results_wo_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) sd(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)) #lm
  )
mm =type_one_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
#text(-0.5, 0.55, labels = "A", cex = 1.3, font = 2, xpd =NA)
axis(1, at = 1:7, labels = 2:8)

## Power ##
type_two_int = 
  cbind(
    sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect < 0.05)), #lme4
    sapply(results_lmm, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )
matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:17, las = 1, lty = 1, col = cols, 
        ylab = "", xaxt="n", main = "Power", xlab = "Number of levels")
legend("bottomright", legend = c("lmm t-values", "lmm z-values", "lm"), col = cols, pch = 15:17, bty = "n")
sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$p_value_effect < 0.05)), #lme4
    sapply(results_lmm, function(l) sd(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) sd(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )
mm =1-type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
axis(1, at = 1:7, labels = 2:8)

# coverage
cols = RColorBrewer::brewer.pal(3, "Set1")
type_two_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_w_lme4_reml$Slope_in_conf )), #lme4
    sapply(results_lmm, function(l) mean(l$results_w_glmmTMB_reml$Slope_in_conf, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) mean(l$results_w_lm$Slope_in_conf , na.rm=TRUE)) #lm
  )

matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:17, las = 1, lty = 1, col = cols, 
        ylab = "", xaxt="n", main = "Coverage", xlab = "")
legend("bottomright", legend = c("lmm t-values", "lmm z-values", "lm"), col = cols, pch = 15:17, bty = "n")
abline(h = 0.95, lty = 3, col = "darkgrey")

sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$Slope_in_conf )), #lme4
    sapply(results_lmm, function(l) sd(l$results_w_glmmTMB_reml$Slope_in_conf, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) sd(l$results_w_lm$Slope_in_conf , na.rm=TRUE)) #lm
  )
mm =type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
axis(1, at = 1:7, labels = 2:8)
dev.off()


########## Figure S1 Type I, Error, etc. for glmm ########## 
cols = RColorBrewer::brewer.pal(3, "Set1")
## Type I error ##
pdf(file = "Figures/Fig_S1.pdf", width = 7, height = 3)
par(mfrow = c(1,3), mar = c(0.1+5, 4, 2, 1), oma = c(1, 1, 1, 1)-1)

## glmm
cols = RColorBrewer::brewer.pal(3, "Set1")
type_one_int = 
  cbind(
    sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_effect < 0.05)), #lme4
    sapply(results_glmm, function(l) mean(l$results_wo_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:17, las = 1, lty = 1, col = cols, 
        ylab = "Rate", xaxt="n", main = "Type I Error", xlab = "levels")
axis(1, at = 1:7, labels = 2:8)
legend("topright", legend = c("lme4", "glmmTMB", "LM"), col = cols, pch = 15:17, bty = "n")
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
        ylab = "", xaxt="n", main = "Power", xlab = "levels", xpd = NA)
axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = c("lme4", "glmmTMB", "LM"), col = cols, pch = 15:17, bty = "n")
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
        ylab = "", xaxt="n", main = "Coverage", xlab = "levels")
abline(h = 0.95, lty = 3, col = "darkgrey")

axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = c("lme4", "glmmTMB", "LM"), col = cols, pch = 15:17, bty = "n")

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







########## Figure S2 T and Z value ########## 
get_p_from_z = function(obj) {
  z = obj$estimate_effect / obj$se_effect
  p = 2*stats::pnorm(abs(z),lower.tail = FALSE)
  return(p)
}

cols = RColorBrewer::brewer.pal(3, "Set1")
## Type I error ##
par(mfrow = c(1,1), mar = c(0.1+5, 4, 2, 1), oma = c(4, 1, 2, 1))
type_one_int = 
  cbind(
    sapply(results_lmm, function(l) mean(get_p_from_z(l$results_wo_lme4_reml)< 0.05)), #lme4
    sapply(results_lmm, function(l) mean(l$results_wo_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:17, las = 1, lty = 1, col = cols, 
        ylab = "Rate", xaxt="n", main = "Type I Error", xlab = "")
legend("topleft", legend = c("lme4 z-values", "glmmTMB z-values", "lm"), col = cols, pch = 15:17, bty = "n")
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_lmm, function(l) sd(get_p_from_z(l$results_wo_lme4_reml)< 0.05)), #lme4
    sapply(results_lmm, function(l) sd(l$results_wo_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) sd(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)) #lm
  )
mm =type_one_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
#text(-0.5, 0.55, labels = "A", cex = 1.3, font = 2, xpd =NA)
axis(1, at = 1:7, labels = 2:8)





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
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols[1:4], bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 10, labels = "A", cex = 1.3, font = 2, xpd =NA)
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 10.0), axes= FALSE, ylab = "", main = "SD of random slope", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)



plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 10.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = c(2, 3, 5, 8), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.04, 10, labels = "B", cex = 1.3, font = 2, xpd =NA)

plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 10.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
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




