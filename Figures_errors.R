
results_lmm = readRDS("Results/results_mountain_lmm.Rds")
results_glmm = readRDS("Results/results_mountain_glmm.Rds")

addA = function(col, alpha = 0.25) apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha)) 

## Type I error ##
par(mfrow = c(2,3), mar = c(0.1, 4, 2, 1), oma = c(4, 1, 2, 1))
type_one_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lme4$p_value_effect < 0.05)), #lme4
    sapply(results_lmm, function(l) mean(l$results_wo_glmmTMB$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) mean(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:17, las = 1, lty = 1, col = cols, 
        ylab = "Rate", xaxt="n", main = "Type I Error", xlab = "")
legend("topright", legend = c("lme4", "glmmTMB", "LM"), col = cols, pch = 15:17, bty = "n")
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_wo_lme4$p_value_effect < 0.05)), #lme4
    sapply(results_lmm, function(l) sd(l$results_wo_glmmTMB$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) sd(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)) #lm
  )
mm =type_one_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
text(-0.5, 0.55, labels = "A", cex = 1.3, font = 2, xpd =NA)



## Power ##
type_two_int = 
  cbind(
    sapply(results_lmm, function(l) 1-mean(l$results_w_lme4$p_value_effect < 0.05)), #lme4
    sapply(results_lmm, function(l) 1-mean(l$results_w_glmmTMB$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )
matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:17, las = 1, lty = 1, col = cols, 
        ylab = "", xaxt="n", main = "Power", xlab = "")
legend("bottomright", legend = c("lme4", "glmmTMB", "LM"), col = cols, pch = 15:17, bty = "n")
sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_w_lme4$p_value_effect < 0.05)), #lme4
    sapply(results_lmm, function(l) sd(l$results_w_glmmTMB$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) sd(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )
mm =1-type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))



# coverage
cols = RColorBrewer::brewer.pal(3, "Set1")
type_two_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_w_lme4$Slope_in_conf )), #lme4
    sapply(results_lmm, function(l) mean(l$results_w_glmmTMB$Slope_in_conf, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) mean(l$results_w_lm$Slope_in_conf , na.rm=TRUE)) #lm
  )

matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:17, las = 1, lty = 1, col = cols, 
        ylab = "", xaxt="n", main = "Coverage", xlab = "")
legend("bottomright", legend = c("lme4", "glmmTMB", "LM"), col = cols, pch = 15:17, bty = "n")
abline(h = 0.95, lty = 3, col = "darkgrey")

sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_w_lme4$Slope_in_conf )), #lme4
    sapply(results_lmm, function(l) sd(l$results_w_glmmTMB$Slope_in_conf, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) sd(l$results_w_lm$Slope_in_conf , na.rm=TRUE)) #lm
  )
mm =type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))




## glmm
cols = RColorBrewer::brewer.pal(3, "Set1")
type_one_int = 
  cbind(
    sapply(results_glmm, function(l) mean(l$results_wo_lme4$p_value_effect < 0.05)), #lme4
    sapply(results_glmm, function(l) mean(l$results_wo_glmmTMB$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:17, las = 1, lty = 1, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "levels")
axis(1, at = 1:7, labels = 2:8)
legend("topright", legend = c("lme4", "glmmTMB", "LM"), col = cols, pch = 15:17, bty = "n")
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_wo_lme4$p_value_effect < 0.05)), #lme4
    sapply(results_lmm, function(l) sd(l$results_wo_glmmTMB$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) sd(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)) #lm
  )
mm =type_one_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
text(-0.5, 0.55, labels = "B", cex = 1.3, font = 2, xpd =NA)


## Power ##
cols = RColorBrewer::brewer.pal(3, "Set1")
type_two_int = 
  cbind(
    sapply(results_glmm, function(l) 1-mean(l$results_w_lme4$p_value_effect < 0.05)), #lme4
    sapply(results_glmm, function(l) 1-mean(l$results_w_glmmTMB$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )

matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:17, las = 1, lty = 1, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "levels", xpd = NA)
axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = c("lme4", "glmmTMB", "LM"), col = cols, pch = 15:17, bty = "n")
sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_w_lme4$p_value_effect < 0.05)), #lme4
    sapply(results_lmm, function(l) sd(l$results_w_glmmTMB$p_value_effect < 0.05, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) sd(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )
mm =1-type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))



cols = RColorBrewer::brewer.pal(3, "Set1")
type_two_int = 
  cbind(
    sapply(results_glmm, function(l) mean(l$results_w_lme4$Slope_in_conf )), #lme4
    sapply(results_glmm, function(l) mean(l$results_w_glmmTMB$Slope_in_conf, na.rm=TRUE)), # glmmTMB
    sapply(results_glmm, function(l) mean(l$results_w_lm$Slope_in_conf , na.rm=TRUE)) #lm
  )

matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:17, las = 1, lty = 1, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "levels")
abline(h = 0.95, lty = 3, col = "darkgrey")

axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = c("lme4", "glmmTMB", "LM"), col = cols, pch = 15:17, bty = "n")

sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_w_lme4$Slope_in_conf )), #lme4
    sapply(results_lmm, function(l) sd(l$results_w_glmmTMB$Slope_in_conf, na.rm=TRUE)), # glmmTMB
    sapply(results_lmm, function(l) sd(l$results_w_lm$Slope_in_conf , na.rm=TRUE)) #lm
  )
mm =type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i])$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i])$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))











