########## Helper functions ########## 
addA = function(col, alpha = 0.25) apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha)) 

# calculate p-values by z-statistic for LMMs
get_p_from_z = function(obj) {
  z = obj$estimate_effect / obj$se_effect
  p = 2*stats::pnorm(abs(z),lower.tail = FALSE)
  return(p)
}

# calculate confidence intervals for temperature effect based on z-statistic
get_confidence_interval = function(object) {
  conf = cbind(
    object$results_w_lme4_reml$estimate_effect - 1.96*object$results_w_lme4_reml$se_effect, 
    object$results_w_lme4_reml$estimate_effect + 1.96*object$results_w_lme4_reml$se_effect)
  return(as.integer(0.4 > conf[,1] & 0.4 < conf[,2]))
}

# assign color
ff = function(i) {
  if(i == 1) return(cols[1])
  if(i == 2) return(cols[2])
  if(i == 4) return(cols[3])
  if(i == 7) return(cols[4])
}


########## _________________________________  ##########
##########  Calculation of grand mean ########## 
########## _________________________________  ##########
grand_mean = readRDS("Results/results_grand_mean.RDS")
# 1. level mountain -> c(2, 4, 10, 50)
# 2. level sds -> c(0.1, 1.0, 5.0)
# 3. level ss -> c(1000, 10000, 30000)
# 4. level 5000 repetitions, with Fit, Unweighted, Weighted, and means 



par(mfrow = c(2,4), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
ylab = "Rate"
cols = RColorBrewer::brewer.pal(4, "Set1")
lty = 1
labels = c("2 Mountains", "4 Mountains", "10 Mountains", "50 Mountains")
for(m in 1:4) {
  

  type_one_int = 
    cbind(
      sapply(1:3, function(i) mean(grand_mean[[m]][[1]][[i]][,2] < 0.05)),
      sapply(1:3, function(i) mean(grand_mean[[m]][[2]][[i]][,2] < 0.05)),
      sapply(1:3, function(i) mean(grand_mean[[m]][[3]][[i]][,2] < 0.05))
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:17, las = 1, lty = lty, col = cols, 
          ylab = ylab, xaxt="n", main = "", xlab = "", xpd = NA)
  text(x = 2, y = 0.52, pos = 3, xpd = NA, label = labels[m])
  ylab = ""
  if(m == 1) text(x = 0.5, y = 0.51, label = "A", xpd = NA, cex = 1.2, font = 2, pos = 3)
  legend("topright",col = cols[1:3], pch = 15:17, lty = lty, bty="n", legend= c("0.1 SD", "1.0 SD", "5.0 SD"))
  
  axis(1, at = 1:3, labels = c("50", "1,000", "3,000"))
  if(m == 3) text(x = 1, y = -0.1, pos = 1, xpd = NA, label = "Number of observations for each mountain")
}

ylab = "Rate"
for(m in 1:4) {
  
  type_one_int = 
    cbind(
      sapply(1:3, function(i) mean(grand_mean[[m]][[1]][[i]][,3] < 0.05)),
      sapply(1:3, function(i) mean(grand_mean[[m]][[2]][[i]][,3] < 0.05)),
      sapply(1:3, function(i) mean(grand_mean[[m]][[3]][[i]][,3] < 0.05))
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:17, las = 1, lty = lty, col = cols, 
          ylab = ylab, xaxt="n", main = "", xlab = "", xpd = NA)
  ylab = ""
  axis(1, at = 1:3, labels = c("1,000", "10,000", "30,000"))
  if(m == 3) text(x = 1, y = -0.1, pos = 1, xpd = NA, label = "Number of observations for each mountain")
  if(m == 1) text(x = 0.5, y = 0.51, label = "B", xpd = NA, cex = 1.2, font = 2, pos = 3)
  legend("topright",col = cols[1:3], pch = 15:17, lty = lty, bty="n", legend= c("0.1 SD", "1.0 SD", "5.0 SD"))
}




########## _________________________________  ##########
##########  Random intercept only ########## 
########## _________________________________  ##########
results_lmm = readRDS("Results/results_mountain_lmm_random_intercept_only_0.1_.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_random_intercept_only_100_.Rds")
results_miss = readRDS("Results/results_mountain_lmm_random_intercept_only_miss_specified_no_cov_0.1_.Rds")

########## __Figure 1 Random intercept only  Type I, Error, etc. for lmm ########## 


cols = RColorBrewer::brewer.pal(4, "Set1")
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1, 2, 2)
cex_legend = 0.9
labels = c("Height ~ T + (1|mountain)", 
           "Height ~ T + (1|mountain) + (0+T|mountain)", 
           "Height ~ T + mountain", 
           "Height ~ T ")
## Type I error ##
pdf(file = "Figures/Fig_1.pdf", width = 9.2, height = 5.8)
par(mfrow = c(2,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
type_one_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)), #lme4
    sapply(results_miss, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)),
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)) #lm w/go goruping
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "", xpd = NA)
text(x=-0.2, pos = 2, y = 0.55, labels = "A", cex = 1.2, xpd = NA, font = 2)
text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
legend("topright", legend = labels, 
       col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_wo_lme4_reml$p_value_effect< 0.05)/sqrt(5000)), #lme4
    sapply(results_miss, function(l) sd(l$results_wo_lme4_reml$p_value_effect< 0.05)/sqrt(5000)),
    sapply(results_lmm, function(l) sd(l$results_wo_lm$p_value_effect < 0.05, na.rm=TRUE)/sqrt(5000)), #lm
    sapply(results_lmm, function(l) sd(l$results_wo_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)/sqrt(5000)) #lm w/go goruping
  )
mm = type_one_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
#text(-0.5, 0.55, labels = "A", cex = 1.3, font = 2, xpd =NA)
#axis(1, at = 1:7, labels = 2:8)

## Power ##
type_two_int = 
  cbind(
    sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect < 0.05)), #lme4
    sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect < 0.05)), 
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )
matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of levels")
text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")

legend("bottomright", legend = labels, 
       col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$p_value_effect< 0.05)/sqrt(5000)), #lme4
    sapply(results_miss, function(l) sd(l$results_w_lme4_reml$p_value_effect< 0.05)/sqrt(5000)),
    sapply(results_lmm, function(l) sd(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)/sqrt(5000)), #lm
    sapply(results_lmm, function(l) sd(l$results_w_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)/sqrt(5000)) #lm w/go goruping
  )
mm =1-type_two_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
#axis(1, at = 1:7, labels = 2:8)

# coverage
type_two_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_w_lme4_reml$Slope_in_conf )), #lme4
    sapply(results_miss, function(l) mean(l$results_w_lme4_reml$Slope_in_conf )), #lme4
    sapply(results_lmm, function(l) mean(l$results_w_lm$Slope_in_conf , na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf , na.rm=TRUE)) #lm
  )

matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:19, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "")
text(x= 4, pos = 3, y = 1.02, xpd = NA, labels = "Coverage")

legend("bottomright", legend = labels, 
       col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.95, lty = 3, col = "darkgrey")

sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$Slope_in_conf )/sqrt(5000)), #lme4
    sapply(results_miss, function(l) sd(l$results_w_lme4_reml$Slope_in_conf )/sqrt(5000)), #lme4
    sapply(results_lmm, function(l) sd(l$results_w_lm$Slope_in_conf , na.rm=TRUE)/sqrt(5000)), #lm
    sapply(results_lmm, function(l) sd(l$results_w_lm_wo_grouping$Slope_in_conf , na.rm=TRUE)/sqrt(5000)) #lm
  )

mm =type_two_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
#axis(1, at = 1:7, labels = 2:8)



## glmm
cols = RColorBrewer::brewer.pal(4, "Set1")
cols = c(cols[-2])
lty = c(1, 1, 2)
type_one_int = 
  cbind(
    sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_effect < 0.05)), #lme4
    sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) mean(l$results_wo_lm_wo_grouping[[2]] < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "Number of mountain ranges", xpd = NA)
text(x=-0.2, pos = 2, y = 0.55, labels = "B", cex = 1.2, xpd = NA, font = 2)

axis(1, at = 1:7, labels = 2:8)
legend("topright", legend = c("RS ~ T + (1|mountain)", "RS ~ T + mountain", "RS ~ temp "), col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_wo_lme4_ml$p_value_effect < 0.05)/sqrt(1000)), #lme4
    sapply(results_glmm, function(l) sd(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)/sqrt(1000)), #lm
    sapply(results_glmm, function(l) sd(l$results_wo_lm_wo_grouping[[2]] < 0.05, na.rm=TRUE)/sqrt(1000)) #lm
  )
mm =type_one_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
#text(-0.5, 0.55, labels = "B", cex = 1.3, font = 2, xpd =NA)


## Power ##
type_two_int = 
  cbind(
    sapply(results_glmm, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect < 0.05)), #lme4
    sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )

matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountain ranges", xpd = NA)
axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = c("RS ~ T + (1|mountain)", "RS ~ T + mountain", "RS ~ temp "), col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$p_value_effect < 0.05)/sqrt(1000)), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)/sqrt(1000)), # glmmTMB
    sapply(results_glmm, function(l) sd(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)/sqrt(1000)) #lm
  )
mm =1-type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))


## Coverage ##
type_two_int = 
  cbind(
    sapply(results_glmm, function(l) mean(l$results_w_lme4_ml$Slope_in_conf )), #lme4
    sapply(results_glmm, function(l) mean(l$results_w_lm$Slope_in_conf , na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf , na.rm=TRUE)) #lm
  )


matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountain ranges", xpd = NA)
abline(h = 0.95, lty = 3, col = "darkgrey")

axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = c("RS ~ T + (1|mountain)", "RS ~ T + mountain", "RS ~ temp "), col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)

sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$Slope_in_conf )/sqrt(1000)), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$Slope_in_conf, na.rm=TRUE)/sqrt(1000)), # glmmTMB
    sapply(results_glmm, function(l) sd(l$results_w_lm$Slope_in_conf , na.rm=TRUE)/sqrt(1000)) #lm
  )
mm =type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar=0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar=0.1)$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))

dev.off()



########## __Figure S1 Random intercept different SD for random effect ########## 

cols = RColorBrewer::brewer.pal(4, "Set1")
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1, 2, 2)
cex_legend = 0.9
## Type I error ##
pdf(file = "Figures/Fig_S1.pdf", width = 9.2, height = 8.8)

sd_results = c("0.01", "0.1", "0.5", "2")
par(mfrow = c(4,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
labels = c("A", "B", "C", "D")
xlab = ""
for(i in 1:4){
  
  results_lmm = readRDS(paste0("Results/results_mountain_lmm_random_intercept_only_", sd_results[i],"_.Rds"))
  results_miss = readRDS(paste0("Results/results_mountain_lmm_random_intercept_only_miss_specified_no_cov_", sd_results[i],"_.Rds"))
  
  if(i == 4) xlab = "Number of mountain ranges"
  
  type_one_int = 
    cbind(
      sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)), #lme4
      sapply(results_miss, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)),
      sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)) #lm w/go goruping
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "Rate", xaxt="n", main = "", xlab = xlab, xpd = NA)
  text(x=-0.2, pos = 2, y = 0.55, labels = labels[i], cex = 1.2, xpd = NA, font = 2)
  if(i == 1) text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
  legend("topright", legend = c("Growth ~ T + (1|mountain)", "Growth ~ T + (1|mountain) + (0 + T|mountain)", "Growth ~ T + mountain", "Growth ~ T "), 
         col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  abline(h = 0.05, lty = 3, col = "darkgrey")
  sd = 
    cbind(
      sapply(results_lmm, function(l) sd(l$results_wo_lme4_reml$p_value_effect< 0.05)/sqrt(5000)), #lme4
      sapply(results_miss, function(l) sd(l$results_wo_lme4_reml$p_value_effect< 0.05)/sqrt(5000)),
      sapply(results_lmm, function(l) sd(l$results_wo_lm$p_value_effect < 0.05, na.rm=TRUE)/sqrt(5000)), #lm
      sapply(results_lmm, function(l) sd(l$results_wo_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)/sqrt(5000)) #lm w/go goruping
    )
  mm = type_one_int
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  #text(-0.5, 0.55, labels = "A", cex = 1.3, font = 2, xpd =NA)
  if(i == 4) axis(1, at = 1:7, labels = 2:8)

  ## Power ##
  type_two_int = 
    cbind(
      sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect < 0.05)), #lme4
      sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect < 0.05)), 
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)) #lm
    )
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd=NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")
  
  legend("bottomright", legend = c("Growth ~ T + (1|mountain)", "Growth ~ T + (1|mountain) + (0 + T|mountain)", "Growth ~ T + mountain", "Growth ~ T "), 
         col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  sd = 
    cbind(
      sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$p_value_effect< 0.05)/sqrt(5000)), #lme4
      sapply(results_miss, function(l) sd(l$results_w_lme4_reml$p_value_effect< 0.05)/sqrt(5000)),
      sapply(results_lmm, function(l) sd(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)/sqrt(5000)), #lm
      sapply(results_lmm, function(l) sd(l$results_w_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)/sqrt(5000)) #lm w/go goruping
    )
  mm =1-type_two_int
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  if(i == 4) axis(1, at = 1:7, labels = 2:8)
  
  # coverage
  type_two_int = 
    cbind(
      sapply(results_lmm, function(l) mean(l$results_w_lme4_reml$Slope_in_conf )), #lme4
      sapply(results_miss, function(l) mean(l$results_w_lme4_reml$Slope_in_conf )), #lme4
      sapply(results_lmm, function(l) mean(l$results_w_lm$Slope_in_conf , na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf , na.rm=TRUE)) #lm
    )
  
  matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:19, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd=NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.02, xpd = NA, labels = "Coverage")
  
  legend("bottomright", legend = c("Growth ~ T + (1|mountain)", "Growth ~ T + (1|mountain) + (0 + T|mountain)", "Growth ~ T + mountain", "Growth ~ T "), 
         col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  abline(h = 0.95, lty = 3, col = "darkgrey")
  
  sd = 
    cbind(
      sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$Slope_in_conf )/sqrt(5000)), #lme4
      sapply(results_miss, function(l) sd(l$results_w_lme4_reml$Slope_in_conf )/sqrt(5000)), #lme4
      sapply(results_lmm, function(l) sd(l$results_w_lm$Slope_in_conf , na.rm=TRUE)/sqrt(5000)), #lm
      sapply(results_lmm, function(l) sd(l$results_w_lm_wo_grouping$Slope_in_conf , na.rm=TRUE)/sqrt(5000)) #lm
    )
  
  mm =type_two_int
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  if(i == 4)  axis(1, at = 1:7, labels = 2:8)
}


dev.off()







########## __Figure S2 Random intercept different number of observations for GLMM ########## 
files = c("Results/results_mountain_glmm_random_intercept_only_25_.Rds", 
          "Results/results_mountain_glmm_random_intercept_only_50_.Rds", 
          "Results/results_mountain_glmm_random_intercept_only_100_.Rds", 
          "Results/results_mountain_glmm_random_intercept_only_200_.Rds")

pdf("Figures/Fig_S2.pdf", , width = 9.2, height = 8.8)
par(mfrow = c(4,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
cols = RColorBrewer::brewer.pal(3, "Set1")
labels = c("A", "B", "C", "D")
cex_legend = 0.9
for(i in 1:4) {
  
  results_glmm = readRDS(files[i])
  
  if(i == 4) xlab = "Number of mountain ranges"
  else xlab = ""
  
  
  ## glmm
  cols = RColorBrewer::brewer.pal(4, "Set1")
  cols = c(cols[-2])
  lty = c(1, 1, 2)
  type_one_int = 
    cbind(
      sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_effect < 0.05)), #lme4
      sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) mean(l$results_wo_lm_wo_grouping[[2]] < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "Rate", xaxt="n", main = "", xlab = xlab, xpd = NA)
  text(x=-0.2, pos = 2, y = 0.55, labels = labels[i], cex = 1.2, xpd = NA, font = 2)
  if(i == 1) text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
  
  if(i == 4) axis(1, at = 1:7, labels = 2:8)
  legend("topright", legend = c("RS ~ T + (1|mountain)", "RS ~ T + mountain", "RS ~ T "), 
         col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  abline(h = 0.05, lty = 3, col = "darkgrey")
  sd = 
    cbind(
      sapply(results_glmm, function(l) sd(l$results_wo_lme4_ml$p_value_effect < 0.05)/sqrt(1000)), #lme4
      sapply(results_glmm, function(l) sd(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)/sqrt(1000)), #lm
      sapply(results_glmm, function(l) sd(l$results_wo_lm_wo_grouping[[2]] < 0.05, na.rm=TRUE)/sqrt(1000)) #lm
    )
  mm =type_one_int
  upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  #text(-0.5, 0.55, labels = "B", cex = 1.3, font = 2, xpd =NA)
  
  
  ## Power ##
  type_two_int = 
    cbind(
      sapply(results_glmm, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect < 0.05)), #lme4
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd = NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")
  if(i == 4)  axis(1, at = 1:7, labels = 2:8)
  
  pos = "topright"
  if(i >2) pos = "bottomright"
  legend(pos, legend = c("RS ~ T + (1|mountain)", "RS ~ T + mountain", "RS ~ T "), 
         col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  sd = 
    cbind(
      sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$p_value_effect < 0.05)/sqrt(1000)), #lme4
      sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)/sqrt(1000)), # glmmTMB
      sapply(results_glmm, function(l) sd(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)/sqrt(1000)) #lm
    )
  mm =1-type_two_int
  upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
  
  ## Coverage ##
  type_two_int = 
    cbind(
      sapply(results_glmm, function(l) mean(l$results_w_lme4_ml$Slope_in_conf )), #lme4
      sapply(results_glmm, function(l) mean(l$results_w_lm$Slope_in_conf , na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf , na.rm=TRUE)) #lm
    )
  
  
  matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd = NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.02, xpd = NA, labels = "Coverage")
  abline(h = 0.95, lty = 3, col = "darkgrey")
  
  if(i == 4)  axis(1, at = 1:7, labels = 2:8)
  legend("bottomright", legend = c("RS ~ T + (1|mountain)", "RS ~ T + mountain", "RS ~ T "), 
         col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  sd = 
    cbind(
      sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$Slope_in_conf )/sqrt(1000)), #lme4
      sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$Slope_in_conf, na.rm=TRUE)/sqrt(1000)), # glmmTMB
      sapply(results_glmm, function(l) sd(l$results_w_lm$Slope_in_conf , na.rm=TRUE)/sqrt(1000)) #lm
    )
  mm =type_two_int
  upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar=0.1)$y)
  lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar=0.1)$y)
  sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
}  
dev.off()

########## _________________________________  ##########
########## Random intercept and Random slope ########## 
########## _________________________________  ##########
########## __Figure 2 Random intercept + slope Type I, Error, etc. for lmm ########## 

results_lmm_no_cov = readRDS("Results/results_mountain_lmm_no_cov_0.1_.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_0.1_.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_200_.Rds")
results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_200_.Rds")
results_miss = readRDS("Results/results_mountain_lmm_miss_specified_0.1_.Rds")


cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1,1, 2, 2)
pch = c(15, 20, 16:18)
cex_legend = 0.9
## Type I error ##
pdf(file = "Figures/Fig_2.pdf", width = 9.2, height = 5.8)
par(mfrow = c(2,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
labels = c("Height ~ T + (1|mountain) + (0 + T|mountain)",
           "Height ~ T + (T|mountain)", 
           "Height ~ T + (1|mountain)", 
           "Height ~ T * mountain", 
           "Height ~ T ")
type_one_int = 
  cbind(
    sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)), #lme4
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)), #lme4
    sapply(results_miss, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)),
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)) #lm w/go goruping
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "", xpd = NA)
text(x=-0.2, pos = 2, y = 0.55, labels = "A", cex = 1.2, xpd = NA, font = 2)
text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
legend("topright", legend = labels,
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_lmm_no_cov, function(l) sd(l$results_wo_lme4_reml$p_value_effect< 0.05)/sqrt(5000)), #lme4
    sapply(results_lmm, function(l) sd(l$results_wo_lme4_reml$p_value_effect< 0.05)/sqrt(5000)), #lme4
    sapply(results_miss, function(l) sd(l$results_wo_lme4_reml$p_value_effect< 0.05)/sqrt(5000)),
    sapply(results_lmm, function(l) sd(l$results_wo_lm$p_value_effect < 0.05, na.rm=TRUE)/sqrt( sum(!is.na(l$results_wo_lm$p_value_effect < 0.05)) )), #lm
    sapply(results_lmm, function(l) sd(l$results_wo_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)/sqrt(5000)) #lm w/go goruping
  )
mm = type_one_int
upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))

## Power ##
type_two_int = 
  cbind(
    sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect < 0.05)), #lme4
    sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect < 0.05)), #lme4
    sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect < 0.05)), 
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )
matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of levels")
text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")

legend("bottomright", legend = labels,
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_lmm_no_cov, function(l) sd(l$results_w_lme4_reml$p_value_effect< 0.05)/sqrt(5000)), #lme4
    sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$p_value_effect< 0.05)/sqrt(5000)), #lme4
    sapply(results_miss, function(l) sd(l$results_w_lme4_reml$p_value_effect< 0.05)/sqrt(5000)),
    sapply(results_lmm, function(l) sd(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)/sqrt( sum(!is.na( l$results_w_lm$p_value_effect < 0.05 )) )), #lm
    sapply(results_lmm, function(l) sd(l$results_w_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_lm_wo_grouping$p_value_effect < 0.05)) )) #lm w/go goruping
  )
mm =1-type_two_int
upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
#axis(1, at = 1:7, labels = 2:8)

# coverage
type_two_int = 
  cbind(
    sapply(results_lmm_no_cov, function(l) mean(l$results_w_lme4_reml$Slope_in_conf )), #lme4
    sapply(results_lmm, function(l) mean(l$results_w_lme4_reml$Slope_in_conf )), #lme4
    sapply(results_miss, function(l) mean(l$results_w_lme4_reml$Slope_in_conf )), #lme4
    sapply(results_lmm, function(l) mean(l$results_w_lm$Slope_in_conf , na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf , na.rm=TRUE)) #lm
  )

matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "")
text(x= 4, pos = 3, y = 1.02, xpd = NA, labels = "Coverage")

legend("bottomright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.95, lty = 3, col = "darkgrey")

sd = 
  cbind(
    sapply(results_lmm_no_cov, function(l) sd(l$results_w_lme4_reml$Slope_in_conf )/sqrt(5000)), #lme4
    sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$Slope_in_conf )/sqrt(5000)), #lme4
    sapply(results_miss, function(l) sd(l$results_w_lme4_reml$Slope_in_conf )/sqrt(5000)), #lme4
    sapply(results_lmm, function(l) sd(l$results_w_lm$Slope_in_conf , na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_lm$Slope_in_conf)) )), #lm
    sapply(results_lmm, function(l) sd(l$results_w_lm_wo_grouping$Slope_in_conf , na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_lm_wo_grouping$Slope_in_conf)) )) #lm
  )

mm =type_two_int
upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
#axis(1, at = 1:7, labels = 2:8)



# type_one_int = 
#   cbind(
#     sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)), #lme4
#     sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)), #lme4
#     sapply(results_miss, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)),
#     sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect < 0.05, na.rm=TRUE)), #lm
#     sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)) #lm w/go goruping
#   )
## glmm
cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
cols = cols[-3]
lty = c(1, 1, 2, 2)
pch = c(15, 20, 17:18)
labels = c("RS ~ T + (1|mountain) + (0 + T|mountain)", "RS ~ T + (T|mountain)","RS ~ T * mountain", "RS ~ T ")
type_one_int = 
  cbind(
    sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lme4_ml$p_value_effect < 0.05)), #lme4
    sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_effect < 0.05)), #lme4
    sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) mean(l$results_wo_lm_wo_grouping[[2]] < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "Number of mountain ranges", xpd = NA)
text(x=-0.2, pos = 2, y = 0.55, labels = "B", cex = 1.2, xpd = NA, font = 2)

axis(1, at = 1:7, labels = 2:8)
legend("topright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_glmm_no_cov, function(l) sd(l$results_wo_lme4_ml$p_value_effect < 0.05)/sqrt(1000)), #lme4
    sapply(results_glmm, function(l) sd(l$results_wo_lme4_ml$p_value_effect < 0.05)/sqrt(1000)), #lme4
    sapply(results_glmm, function(l) sd(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)/sqrt( sum(!is.na( l$results_wo_lm[[2]] )) )), #lm
    sapply(results_glmm, function(l) sd(l$results_wo_lm_wo_grouping[[2]] < 0.05, na.rm=TRUE)/sqrt( sum(!is.na(l$results_wo_lm_wo_grouping[[2]]  )) )) #lm
  )
mm =type_one_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
#text(-0.5, 0.55, labels = "B", cex = 1.3, font = 2, xpd =NA)


## Power ##
type_two_int = 
  cbind(
    sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect < 0.05)), #lme4
    sapply(results_glmm, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect < 0.05)), #lme4
    sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )

matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountain ranges", xpd = NA)
axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_glmm_no_cov, function(l) sd(l$results_w_lme4_ml$p_value_effect < 0.05)/sqrt(1000)), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$p_value_effect < 0.05)/sqrt(1000)), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_glmmTMB_reml$p_value_effect)) )), # glmmTMB
    sapply(results_glmm, function(l) sd(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_lm$p_value_effect )) )) #lm
  )
mm =1-type_two_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))


## Coverage ##
type_two_int = 
  cbind(
    sapply(results_glmm_no_cov, function(l) mean(l$results_w_lme4_ml$Slope_in_conf )), #lme4
    sapply(results_glmm, function(l) mean(l$results_w_lme4_ml$Slope_in_conf )), #lme4
    sapply(results_glmm, function(l) mean(l$results_w_lm$Slope_in_conf , na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf , na.rm=TRUE)) #lm
  )


matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountain ranges", xpd = NA)
abline(h = 0.95, lty = 3, col = "darkgrey")

axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)

sd = 
  cbind(
    sapply(results_glmm_no_cov, function(l) sd(l$results_w_lme4_ml$Slope_in_conf )/sqrt(1000)), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$Slope_in_conf )/sqrt(1000)), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$Slope_in_conf, na.rm=TRUE)/sqrt( sum(!is.na(  l$results_w_glmmTMB_reml$Slope_in_conf )) )), # glmmTMB
    sapply(results_glmm, function(l) sd(l$results_w_lm$Slope_in_conf , na.rm=TRUE)/sqrt( sum(!is.na( l$results_w_lm$Slope_in_conf  ))  )) #lm
  )
mm =type_two_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar=0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar=0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))

dev.off()





########## __Figure S3 Random intercept+slope different SD for random effect ########## 

cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1,1, 2, 2)
pch = c(15, 20, 16:18)
## Type I error ##
pdf(file = "Figures/Fig_S3.pdf", width = 9.2, height = 8.8)

sd_results = c("0.01", "0.1", "0.5", "2")
par(mfrow = c(4,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
labels = c("A", "B", "C", "D")
xlab = ""
legend_label = c("Height ~ T + (1|mountain) + (0 + T|mountain)",
                 "Height ~ T + (T|mountain)", 
                 "Height ~ T + (1|mountain)", 
                 "Height ~ T * mountain", 
                 "Height ~ T ")
for(i in 1:4){
  
  results_lmm = readRDS(paste0("Results/results_mountain_lmm_", sd_results[i],"_.Rds"))
  results_lmm_no_cov = readRDS(paste0("Results/results_mountain_lmm_no_cov_", sd_results[i],"_.Rds"))
  results_miss = readRDS(paste0("Results/results_mountain_lmm_miss_specified_", sd_results[i],"_.Rds"))
  
  if(i == 4) xlab = "Number of mountain ranges"
  
  type_one_int = 
    cbind(
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)), #lme4
      sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)), #lme4
      sapply(results_miss, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05)),
      sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)) #lm w/go goruping
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = "Rate", xaxt="n", main = "", xlab = xlab, xpd = NA)
  text(x=-0.2, pos = 2, y = 2*0.55, labels = labels[i], cex = 1.2, xpd = NA, font = 2)
  if(i == 1) text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Type I error", xpd = NA)
  if(i == 4) axis(1, at = 1:7, labels = 2:8)
  legend("topright", legend = legend_label,
         col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
  abline(h = 0.05, lty = 3, col = "darkgrey")
  sd = 
    cbind(
      sapply(results_lmm_no_cov, function(l) sd(l$results_wo_lme4_reml$p_value_effect< 0.05)/sqrt(5000)), #lme4
      sapply(results_lmm, function(l) sd(l$results_wo_lme4_reml$p_value_effect< 0.05)/sqrt(5000)), #lme4
      sapply(results_miss, function(l) sd(l$results_wo_lme4_reml$p_value_effect< 0.05)/sqrt(5000)),
      sapply(results_lmm, function(l) sd(l$results_wo_lm$p_value_effect < 0.05, na.rm=TRUE)/sqrt( sum(!is.na(l$results_wo_lm$p_value_effect < 0.05)) )), #lm
      sapply(results_lmm, function(l) sd(l$results_wo_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)/sqrt(5000)) #lm w/go goruping
    )
  mm = type_one_int
  upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  
  ## Power ##
  type_two_int = 
    cbind(
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect < 0.05)), #lme4
      sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect < 0.05)), #lme4
      sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect < 0.05)), 
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)) #lm
    )
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd = NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")
  if(i == 4) axis(1, at = 1:7, labels = 2:8)
  legend("bottomright", legend = legend_label,
         col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
  sd = 
    cbind(
      sapply(results_lmm_no_cov, function(l) sd(l$results_w_lme4_reml$p_value_effect< 0.05)/sqrt(5000)), #lme4
      sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$p_value_effect< 0.05)/sqrt(5000)), #lme4
      sapply(results_miss, function(l) sd(l$results_w_lme4_reml$p_value_effect< 0.05)/sqrt(5000)),
      sapply(results_lmm, function(l) sd(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)/sqrt( sum(!is.na( l$results_w_lm$p_value_effect < 0.05 )) )), #lm
      sapply(results_lmm, function(l) sd(l$results_w_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_lm_wo_grouping$p_value_effect < 0.05)) )) #lm w/go goruping
    )
  mm =1-type_two_int
  upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  #axis(1, at = 1:7, labels = 2:8)
  
  # coverage
  type_two_int = 
    cbind(
      sapply(results_lmm_no_cov, function(l) mean(l$results_w_lme4_reml$Slope_in_conf )), #lme4
      sapply(results_lmm, function(l) mean(l$results_w_lme4_reml$Slope_in_conf )), #lme4
      sapply(results_miss, function(l) mean(l$results_w_lme4_reml$Slope_in_conf )), #lme4
      sapply(results_lmm, function(l) mean(l$results_w_lm$Slope_in_conf , na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf , na.rm=TRUE)) #lm
    )
  
  matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd = NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.02, xpd = NA, labels = "Coverage")
  if(i == 4) axis(1, at = 1:7, labels = 2:8)
  legend("bottomright", legend  = legend_label,
         col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
  abline(h = 0.95, lty = 3, col = "darkgrey")
  
  sd = 
    cbind(
      sapply(results_lmm_no_cov, function(l) sd(l$results_w_lme4_reml$Slope_in_conf )/sqrt(5000)), #lme4
      sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$Slope_in_conf )/sqrt(5000)), #lme4
      sapply(results_miss, function(l) sd(l$results_w_lme4_reml$Slope_in_conf )/sqrt(5000)), #lme4
      sapply(results_lmm, function(l) sd(l$results_w_lm$Slope_in_conf , na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_lm$Slope_in_conf)) )), #lm
      sapply(results_lmm, function(l) sd(l$results_w_lm_wo_grouping$Slope_in_conf , na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_lm_wo_grouping$Slope_in_conf)) )) #lm
    )
  
  mm =type_two_int
  upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  

}


dev.off()






########## __Figure S4 Random intercept different number of observations for GLMM ########## 
files = c( "Results/results_mountain_glmm_no_cov_25_.Rds", 
           "Results/results_mountain_glmm_no_cov_50_.Rds", 
          "Results/results_mountain_glmm_no_cov_100_.Rds", 
          "Results/results_mountain_glmm_no_cov_200_.Rds",
          "Results/results_mountain_glmm_no_cov_500_.Rds")


files_with_cov = c("Results/results_mountain_glmm_25_.Rds",
                   "Results/results_mountain_glmm_50_.Rds",
                   "Results/results_mountain_glmm_100_.Rds",
                   "Results/results_mountain_glmm_200_.Rds",
                   "Results/results_mountain_glmm_500_.Rds")

#results_glmm = readRDS("Results/results_mountain_glmm_200_.Rds")
#results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_200_.Rds")

pdf("Figures/Fig_S4.pdf", width = 8.2, height = 8.8)
par(mfrow = c(5,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
cols = cols[-3]
lty = c(1, 1, 2, 2)
pch = c(15, 20, 17:18)
labels = c("A", "B", "C", "D", "E")
legend = c("RS ~ T + (1|mountain) + (0 + T|mountain)", "RS ~ T + (T|mountain)","RS ~ T * mountain", "RS ~ T ")

cex_legend = 0.9
for(i in 1:5) {
  
  results_glmm_no_cov = readRDS(files[i])
  results_glmm = readRDS(files_with_cov[i])
  
  if(i == 5) xlab = "Number of mountain ranges"
  else xlab = ""
  
  
  ## glmm
  lty = c(1, 1, 2, 2)
  type_one_int = 
    cbind(
      sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lme4_ml$p_value_effect < 0.05)), #lme4
      sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_effect < 0.05)), #lme4
      sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) mean(l$results_wo_lm_wo_grouping[[2]] < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "Rate", xaxt="n", main = "", xlab = xlab, xpd = NA)
  text(x=-0.2, pos = 2, y = 0.55, labels = labels[i], cex = 1.2, xpd = NA, font = 2)
  if(i == 1) text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
  
  if(i == 5) axis(1, at = 1:7, labels = 2:8)
  legend("topright", legend = legend, col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  abline(h = 0.05, lty = 3, col = "darkgrey")
  sd = 
    cbind(
      sapply(results_glmm_no_cov, function(l) sd(l$results_wo_lme4_ml$p_value_effect < 0.05)/sqrt(5000)), #lme4
      sapply(results_glmm, function(l) sd(l$results_wo_lme4_ml$p_value_effect < 0.05)/sqrt(5000)), #lme4
      sapply(results_glmm, function(l) sd(l$results_wo_lm[[2]] < 0.05, na.rm=TRUE)/sqrt(5000)), #lm
      sapply(results_glmm, function(l) sd(l$results_wo_lm_wo_grouping[[2]] < 0.05, na.rm=TRUE)/sqrt(5000)) #lm
    )
  mm =type_one_int
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  #text(-0.5, 0.55, labels = "B", cex = 1.3, font = 2, xpd =NA)
  
  
  ## Power ##
  type_two_int = 
    cbind(
      sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect < 0.05)), #lme4
      sapply(results_glmm, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect < 0.05)), #lme4
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd = NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")
  if(i == 5)  axis(1, at = 1:7, labels = 2:8)
  
  pos = "topright"
  if(i >2) pos = "bottomright"
  legend(pos, legend = legend, col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  sd = 
    cbind(
      sapply(results_glmm_no_cov, function(l) sd(l$results_w_lme4_ml$p_value_effect < 0.05)/sqrt(1000)), #lme4
      sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$p_value_effect < 0.05)/sqrt(1000)), #lme4
      sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)/sqrt(1000)), # glmmTMB
      sapply(results_glmm, function(l) sd(l$results_w_lm$p_value_effect < 0.05, na.rm=TRUE)/sqrt(1000)) #lm
    )
  mm =1-type_two_int
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
  
  ## Coverage ##
  type_two_int = 
    cbind(
      sapply(results_glmm_no_cov, function(l) mean(l$results_w_lme4_ml$Slope_in_conf )), #lme4
      sapply(results_glmm, function(l) mean(l$results_w_lme4_ml$Slope_in_conf )), #lme4
      sapply(results_glmm, function(l) mean(l$results_w_lm$Slope_in_conf , na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf , na.rm=TRUE)) #lm
    )
  
  
  matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd = NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.02, xpd = NA, labels = "Coverage")
  abline(h = 0.95, lty = 3, col = "darkgrey")
  
  if(i == 5)  axis(1, at = 1:7, labels = 2:8)
  legend("bottomright", legend = legend, col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  
  sd = 
    cbind(
      sapply(results_glmm_no_cov, function(l) sd(l$results_w_lme4_ml$Slope_in_conf )/sqrt(1000)), #lme4
      sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$Slope_in_conf )/sqrt(1000)), #lme4
      sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$Slope_in_conf, na.rm=TRUE)/sqrt(1000)), # glmmTMB
      sapply(results_glmm, function(l) sd(l$results_w_lm$Slope_in_conf , na.rm=TRUE)/sqrt(1000)) #lm
    )
  mm =type_two_int
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar=0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar=0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
}  
dev.off()



########## _________________________________  ##########
########## lme4 vs glmmTMB ########## 
########## _________________________________  ##########
########## __Figure S5  Random intercept+slope Type I, Power, Cover########## 

results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_no_cov_200_.Rds")


cols = RColorBrewer::brewer.pal(4, "Set1")
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1, 2, 2)
## Type I error ##
pdf(file = "Figures/Fig_S5.pdf", width = 8.2, height = 5.8)
par(mfrow = c(2,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
type_one_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect< 0.05, na.rm=TRUE)), #lme4
    sapply(results_lmm, function(l) mean(get_p_from_z(l$results_wo_lme4_reml) < 0.05, na.rm=TRUE)),
    sapply(results_lmm, function(l) mean(l$results_wo_glmmTMB_reml$p_value_effect< 0.05, na.rm=TRUE)) #glmmTMB
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "", xpd = NA)
text(x=-0.2, pos = 2, y = 0.55, labels = "A", cex = 1.2, xpd = NA, font = 2)
text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
legend("topright", legend = c("lme4 t-statistic", "lme4 z-statistic", "glmmTMB z-statistic"),
       col = cols, pch = 15:19, bty = "n", lty = lty)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_wo_lme4_reml$p_value_effect< 0.05)/sqrt(5000)), #lme4
    sapply(results_lmm, function(l) sd(get_p_from_z(l$results_wo_lme4_reml)< 0.05)/sqrt(5000)), #lm
    sapply(results_lmm, function(l) sd(l$results_wo_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)/sqrt(sum(!is.na(l$results_wo_glmmTMB_reml$p_value_effect)))) #lm w/go goruping
  )
mm = type_one_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))

## Power ##
type_two_int = 
  cbind(
    sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect < 0.05)), #lme4
    sapply(results_lmm, function(l) 1-mean(get_p_from_z(l$results_w_lme4_reml)< 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )
matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of levels")
text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")

legend("bottomright", legend = c("lme4 t-statistic", "lme4 z-statistic", "glmmTMB z-statistic"), col = cols, pch = 15:19, bty = "n", lty = lty)
sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$p_value_effect< 0.05)/sqrt(5000)), #lme4
    sapply(results_lmm, function(l) sd(get_p_from_z(l$results_w_lme4_reml)< 0.05)/sqrt(5000)), #lm
    sapply(results_lmm, function(l) sd(l$results_w_glmmTMB_reml$p_value_effect < 0.05, na.rm=TRUE)/sqrt(sum(!is.na(l$results_w_glmmTMB_reml$p_value_effect)))) #lm w/go goruping
  )
mm =1-type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
#axis(1, at = 1:7, labels = 2:8)

# coverage
type_two_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_w_lme4_reml$Slope_in_conf )), #lme4
    sapply(results_lmm, function(l) mean(get_confidence_interval(l) , na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) mean(l$results_w_glmmTMB_reml$Slope_in_conf , na.rm=TRUE)) #lm
  )

matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:19, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "")
text(x= 4, pos = 3, y = 1.02, xpd = NA, labels = "Coverage")

legend("bottomright", legend = c("lme4 t-statistic", "lme4 z-statistic", "glmmTMB z-statistic"), col = cols, pch = 15:19, bty = "n", lty = lty)
abline(h = 0.95, lty = 3, col = "darkgrey")

sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$Slope_in_conf )/sqrt(5000)), #lme4
    sapply(results_lmm, function(l) sd(get_confidence_interval(l), na.rm=TRUE)/sqrt(5000)), #lm
    sapply(results_lmm, function(l) sd(l$results_w_glmmTMB_reml$Slope_in_conf , na.rm=TRUE)/sqrt(5000)) #lm
  )

mm =type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
#axis(1, at = 1:7, labels = 2:8)



## glmm
cols = RColorBrewer::brewer.pal(4, "Set1")
lty = c(1,  2)
type_one_int = 
  cbind(
    sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_effect< 0.05, na.rm=TRUE)), #lme4
    sapply(results_glmm, function(l) mean(l$results_wo_glmmTMB_ml$p_value_effect< 0.05, na.rm=TRUE)) #glmmTMB
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "Number of mountain ranges", xpd = NA)
text(x=-0.2, pos = 2, y = 0.55, labels = "B", cex = 1.2, xpd = NA, font = 2)

axis(1, at = 1:7, labels = 2:8)
legend("topright", legend = c("lme4 z-statistic", "glmmTMB z-statistic"), col = cols, pch = 15:19, bty = "n", lty = lty)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_wo_lme4_ml$p_value_effect< 0.05)/sqrt(5000)), #lme4
    sapply(results_glmm, function(l) sd(l$results_wo_glmmTMB_ml$p_value_effect < 0.05, na.rm=TRUE)/sqrt(sum(!is.na(l$results_wo_glmmTMB_reml$p_value_effect)))) #lm w/go goruping
  )
mm =type_one_int
upper = sapply(1:2, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:2, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:2, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
#text(-0.5, 0.55, labels = "B", cex = 1.3, font = 2, xpd =NA)


## Power ##
type_two_int = 
  cbind(
    sapply(results_glmm, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect < 0.05)), #lme4
    sapply(results_glmm, function(l) 1-mean(l$results_w_glmmTMB_ml$p_value_effect < 0.05, na.rm=TRUE)) #lm
  )

matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountain ranges", xpd = NA)
axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend =  c("lme4 z-statistic", "glmmTMB z-statistic"), col = cols, pch = 15:19, bty = "n", lty = lty)
sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$p_value_effect< 0.05)/sqrt(5000)), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_ml$p_value_effect < 0.05, na.rm=TRUE)/sqrt(sum(!is.na(l$results_w_glmmTMB_reml$p_value_effect)))) #lm w/go goruping
  )
mm =1-type_two_int
upper = sapply(1:2, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:2, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:2, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))


## Coverage ##
type_two_int = 
  cbind(
    sapply(results_glmm, function(l) mean(l$results_w_lme4_ml$Slope_in_conf )), #lme4
    sapply(results_glmm, function(l) mean(l$results_w_glmmTMB_ml$Slope_in_conf , na.rm=TRUE)) #lm
  )

matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountain ranges", xpd = NA)
abline(h = 0.95, lty = 3, col = "darkgrey")

axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = c("lme4 z-statistic", "glmmTMB z-statistic"), col = cols, pch = 15:19, bty = "n", lty = lty)

sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$Slope_in_conf )/sqrt(5000)), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_ml$Slope_in_conf , na.rm=TRUE)/sqrt(5000)) #lm
  )
mm =type_two_int
upper = sapply(1:2, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar=0.1)$y)
lower = sapply(1:2, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar=0.1)$y)
sapply(1:2, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))

dev.off()





########## _________________________________  ##########
########## Variance estimates  ##########
########## _________________________________  ##########
########## __Figure 3 Variance estimates lme4 ########## 


cols = viridis::viridis(5)

results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_no_cov_200_.Rds")

adj = 1.0
pdf(file = "Figures/Fig_3.pdf", width = 7, height = 5)

par(mfrow = c(2,2), mar = c(2, 1, 4, 2))
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "SD of random intercept", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 15, labels = "A", cex = 1.3, font = 2, xpd =NA)
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "SD of random slope", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)


plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 30.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) {
  if(i == 1) adj = 5
  else adj = 1.0
  lines(density(results_glmm[[i]]$results_w_lme4_ml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
}
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.04, 30, labels = "B", cex = 1.3, font = 2, xpd =NA)


plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 30.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) {
  if(i == 1) adj = 4000
  else if(i == 2) adj = 2
  else adj = 1.0
  lines(density(results_glmm[[i]]$results_w_lme4_ml$stddev_randeff_x, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
}
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)

dev.off()


########## __Figure S6 Variance estimates glmmTMB ########## 


adj = 1.0
pdf(file = "Figures/Fig_S6.pdf", width = 7, height = 5)

results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_no_cov_200_.Rds")
par(mfrow = c(2,2), mar = c(2, 1, 4, 2))
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "SD of random intercept", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm=TRUE), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 15, labels = "A", cex = 1.3, font = 2, xpd =NA)
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "SD of random slope", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_x, adjust = adj, na.rm=TRUE), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)



plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_glmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.04, 15, labels = "B", cex = 1.3, font = 2, xpd =NA)

plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_glmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_x, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)

dev.off()




########## __Figure S7 variance estimate+std error########## 
cols = viridis::viridis(5)


adj = 1.0
pdf(file = "Figures/Fig_S7.pdf", width = 7, height = 5)

par(mfrow = c(2,2), mar = c(2, 1, 4, 2))
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "SD of random intercept", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 12, labels = "A", cex = 1.3, font = 2, xpd =NA)
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 37.0), axes= FALSE, ylab = "", main = "SE of Intercept", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$se_intercept, adjust = adj), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")



plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "SD of random slope", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_x, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.04, 10, labels = "B", cex = 1.3, font = 2, xpd =NA)

plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 50.0), axes= FALSE, ylab = "", main = "SE of Temperature effect", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$se_effect, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")

dev.off()


########## __Figure S8 Variance estimates lmm MLE/REML ########## 
cols = viridis::viridis(5)

adj = 1.0
pdf(file = "Figures/Fig_S8.pdf", width = 7, height = 5)

par(mfrow = c(2,2), mar = c(2, 1+0.3, 4, 2))

# lme4
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "SD of random intercept", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("REML", "MLE"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.04, 32, labels = "A", cex = 1.3, font = 2, xpd =NA)


plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "SD of random slope", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_ml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)

axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("REML", "MEL"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)


# glmmTMB
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("REML", "MLE"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.05, 32, labels = "B", cex = 1.3, font = 2, xpd =NA)


plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)

axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("REML", "MLE"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)


dev.off()



########## __Figure S9 Variance estimates glmm MLE/REML ########## 
cols = viridis::viridis(5)


# glmm4

adj = 1.0
pdf(file = "Figures/Fig_S9.pdf", width = 7, height = 5)

par(mfrow = c(2,2), mar = c(2, 1+0.3, 4, 2))
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 28.0), axes= FALSE, ylab = "", main = "SD of random intercept", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_glmm[[i]]$results_w_lme4_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)

text(-0.04, 28, labels = "A", cex = 1.3, font = 2, xpd =NA)

plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 28.0), axes= FALSE, ylab = "", main = "SD of random slope", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_ml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)


# glmmTMB ML
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 37.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_ml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4, lty = 2)

axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("REML", "ML"), lty = c(1, 2), bty = "n", lwd = 1.0)

abline(v=0.1, col="darkgrey", lty = 3)
text(-0.05, 37, labels = "B", cex = 1.3, font = 2, xpd =NA)

plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 37.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_x, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_ml$stddev_randeff_x, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4, lty = 2)

axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("REML", "ML"), lty = c(1, 2), bty = "n", lwd = 1.0)

abline(v=0.1, col="darkgrey", lty = 3)
dev.off()







########## __Figure S10 Singularities:Type I ########## 
results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_.Rds")
pdf(file = "Figures/Fig_S10.pdf", width = 7, height = 5)
par(mfrow = c(1,2), mar = c(0.1+5, 4, 2, 1), oma = c(1, 1, 1, 1)-1)
cols = RColorBrewer::brewer.pal(3, "Set1")


sing = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 1] < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 0] < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect < 0.05, na.rm = TRUE)))

matplot(sing, type="o", ylim = c(0, 0.5), pch = 15:17, las = 1, col = cols, 
        ylab = "Type I error rate", xaxt="n", main = "lmm: Height ~ T + (1|mountain) + (0+ T|mountain)", xlab = "", lty = 1, cex.main = 0.7)
legend("topright", legend = c("singularity", "no singularity", "average" ), col = cols, pch = 15:17, bty = "n")
abline(h = 0.05, lty = 3, col = "darkgrey")
axis(1, at = 1:7, labels = 2:8)

sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 1] < 0.05, na.rm = TRUE) / sqrt(sum(l$results_wo_lme4_reml$Singularity != 1)) ),
    sapply(results_lmm, function(l) sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 0] < 0.05, na.rm = TRUE) / sqrt(sum(l$results_wo_lme4_reml$Singularity == 1))),
    sapply(results_lmm, function(l) sd(l$results_wo_lme4_reml$p_value_effect < 0.05, na.rm = TRUE) /sqrt(5000)))
mm =sing
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
#axis(1, at = 1:7, labels = 2:8)





sing = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity == 1]  < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity == 0]  < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect < 0.05, na.rm = TRUE)))

matplot(sing, type="o", ylim = c(0, 0.5), pch = 15:17, las = 1, col = cols, 
        ylab = "Type I error rate", xaxt="n", main = "lm: y ~ temp*mountain", xlab = "", lty = 1, cex.main = 0.7)
legend("topright", legend = c("singularity", "no singularity", "average" ), col = cols, pch = 15:17, bty = "n")
abline(h = 0.05, lty = 3, col = "darkgrey")
axis(1, at = 1:7, labels = 2:8)

sd = 
  cbind(
    sapply(results_lmm, function(l) sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity == 1] < 0.05, na.rm = TRUE) / sqrt(sum(l$results_wo_lme4_reml$Singularity != 1)) ),
    sapply(results_lmm, function(l) sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity == 0] < 0.05, na.rm = TRUE) / sqrt(sum(l$results_wo_lme4_reml$Singularity == 1))),
    sapply(results_lmm, function(l) sd(l$results_wo_lm$p_value_effect < 0.05, na.rm = TRUE) /sqrt(5000)))
mm =sing
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))







# # Power 
# 
# sing = 
#   cbind(
#     sapply(results_lmm, function(l) mean(l$results_w_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 1] < 0.05, na.rm = TRUE)),
#     sapply(results_lmm, function(l) mean(l$results_w_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 0] < 0.05, na.rm = TRUE)),
#     sapply(results_lmm, function(l) mean(l$results_w_lme4_reml$p_value_effect < 0.05, na.rm = TRUE)))
# 
# matplot(sing, type="o", ylim = c(0, 1.0), pch = 15:17, las = 1, col = cols, 
#         ylab = "Power", xaxt="n", main = "lmm: Height ~ T + (1|mountain) + (0+ T|mountain)", xlab = "", lty = 1, cex.main = 0.7)
# legend("topright", legend = c("singularity", "no singularity", "average" ), col = cols, pch = 15:17, bty = "n")
# abline(h = 0.05, lty = 3, col = "darkgrey")
# axis(1, at = 1:7, labels = 2:8)
# 
# sd = 
#   cbind(
#     sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 1] < 0.05, na.rm = TRUE) / sqrt(sum(l$results_wo_lme4_reml$Singularity != 1)) ),
#     sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 0] < 0.05, na.rm = TRUE) / sqrt(sum(l$results_wo_lme4_reml$Singularity == 1))),
#     sapply(results_lmm, function(l) sd(l$results_w_lme4_reml$p_value_effect < 0.05, na.rm = TRUE) /sqrt(5000)))
# mm =sing
# upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
# lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
# sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
# #axis(1, at = 1:7, labels = 2:8)
# 
# 
# 
# 
# 
# sing = 
#   cbind(
#     sapply(results_lmm, function(l) mean(l$results_w_lm$p_value_effect[l$results_wo_lme4_reml$Singularity == 1]  < 0.05, na.rm = TRUE)),
#     sapply(results_lmm, function(l) mean(l$results_w_lm$p_value_effect[l$results_wo_lme4_reml$Singularity == 0]  < 0.05, na.rm = TRUE)),
#     sapply(results_lmm, function(l) mean(l$results_w_lm$p_value_effect < 0.05, na.rm = TRUE)))
# 
# matplot(sing, type="o", ylim = c(0, 1.0), pch = 15:17, las = 1, col = cols, 
#         ylab = "Power", xaxt="n", main = "lm: y ~ temp*mountain", xlab = "", lty = 1, cex.main = 0.7)
# legend("topright", legend = c("singularity", "no singularity", "average" ), col = cols, pch = 15:17, bty = "n")
# abline(h = 0.05, lty = 3, col = "darkgrey")
# axis(1, at = 1:7, labels = 2:8)
# 
# sd = 
#   cbind(
#     sapply(results_lmm, function(l) sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity == 1] < 0.05, na.rm = TRUE) / sqrt(sum(l$results_wo_lme4_reml$Singularity != 1)) ),
#     sapply(results_lmm, function(l) sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity == 0] < 0.05, na.rm = TRUE) / sqrt(sum(l$results_wo_lme4_reml$Singularity == 1))),
#     sapply(results_lmm, function(l) sd(l$results_wo_lm$p_value_effect < 0.05, na.rm = TRUE) /sqrt(5000)))
# mm =sing
# upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
# lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
# sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))


dev.off()











# Figure S7 Correlation plot for glmmTMB 
# 
# pdf("./Figures/Fig_S7.pdf", width = 9, height = 7.5)
# par(mfrow = c(2,2), mar=c(1, 2, 1, 1), oma = c(5, 5, 3, 1))
# plot(NULL, NULL, xlim = c(0, 0.5), ylim = c(0, 0.4), 
#      main = "",  cex.axis = 1.0, xaxt="n",
#      cex.main = 1.0, xlab = "", ylab = "",  las =1, xpd = NA)
# text(x = 0.25, y = 0.42, pos = 3, labels = "Intercept", xpd = NA)
# text(x = -0.15, y = 0.43, labels = "A", font = 2, cex = 1.2, xpd = NA)
# title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 1, xpd = NA)
# #title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1)
# levels = c(2, 3, 5, 8)-1
# for(i in 1:number_of_levels) {
#   points(x = results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter, 
#          y = results_lmm[[levels[i]]]$results_w_glmmTMB_reml$se_intercept , col = colors[i], cex=0.2, pch = 19)
# }
# quants = sapply(1:number_of_levels, function(i) quantile(results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter, probs = c(0.7, 0.8,0.9,0.95), na.rm = TRUE))
# where = matrix(NA, number_of_quantiles, number_of_levels)
# for(i in 1:number_of_levels){
#   for(j in 1:number_of_quantiles){
#     p = quants[j,i]
#     ordered_results = results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter[order(results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter)]
#     where[j,i] = which(ordered_results >= p)[1] 
#   }
# }
# coords = sapply(1:number_of_levels, function(i) results_lmm[[levels[i]]]$results_w_glmmTMB_reml$se_intercept[order(results_lmm[[levels[i]]]$results_w_glmmTMB_reml$se_intercept)][where[,i]])
# for(i in 1:number_of_quantiles){
#   lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
# }
# text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)
# legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")
# 
# 
# 
# plot(NULL, NULL, xlim = c(0, 0.5), ylim = c(0, 0.4), 
#      main = "",  cex.axis = 1.0,xaxt="n",
#      cex.main = 1.0, xlab = "", ylab = "",  las =1, xpd=NA)
# text(x = 0.25, y = 0.42, pos = 3, labels = "Temperature", xpd = NA)
# 
# title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 1)
# title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1)
# for(i in 1:number_of_levels) {
#   points(x = results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x,
#          y = results_lmm[[levels[i]]]$results_w_glmmTMB_reml$se_effect , col = colors[i], cex=0.2, pch = 19)
# }
# quants = sapply(1:number_of_levels, function(i) quantile(results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x, probs = c(0.7, 0.8,0.9,0.95), na.rm = TRUE))
# where = matrix(NA, number_of_quantiles, number_of_levels)
# for(i in 1:number_of_levels){
#   for(j in 1:number_of_quantiles){
#     p = quants[j,i]
#     ordered_results = results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x[order(results_lmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x)]
#     where[j,i] = which(ordered_results >= p)[1] 
#   }
# }
# coords = sapply(1:number_of_levels, function(i) results_lmm[[levels[i]]]$results_w_glmmTMB_reml$se_effect[order(results_lmm[[levels[i]]]$results_w_glmmTMB_reml$se_effect)][where[,i]])
# for(i in 1:number_of_quantiles){
#   lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
# }
# text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)
# legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")
# 
# 
# 
# 
# ## GLMM
# plot(NULL, NULL, xlim = c(0, 0.6), ylim = c(0, 0.5), 
#      main = "",  cex.axis = 1.0,
#      cex.main = 1.0, xlab = "", ylab = "",  las =1)
# title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 1, xpd = NA)
# title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1, xpd = NA)
# text(x = -0.18, y = 0.5375, labels = "B", font = 2, cex = 1.2, xpd = NA)
# for(i in 1:number_of_levels) {
#   points(x = results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter, 
#          y = results_glmm[[levels[i]]]$results_w_glmmTMB_reml$se_intercept , col = colors[i], cex=0.2, pch = 19)
# }
# quants = sapply(1:number_of_levels, function(i) quantile(results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter, probs = c(0.7, 0.8,0.9,0.95), na.rm = TRUE))
# where = matrix(NA, number_of_quantiles, number_of_levels)
# for(i in 1:number_of_levels){
#   for(j in 1:number_of_quantiles){
#     p = quants[j,i]
#     ordered_results = results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter[order(results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_inter)]
#     where[j,i] = which(ordered_results >= p)[1] 
#   }
# }
# coords = sapply(1:number_of_levels, function(i) results_glmm[[levels[i]]]$results_w_glmmTMB_reml$se_intercept[order(results_glmm[[levels[i]]]$results_w_glmmTMB_reml$se_intercept)][where[,i]])
# for(i in 1:number_of_quantiles){
#   lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
# }
# text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)
# 
# legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")
# 
# 
# plot(NULL, NULL, xlim = c(0, 0.9), ylim = c(0, 0.6), 
#      main = "",  cex.axis = 1.0,
#      cex.main = 1.0, xlab = "", ylab = "",  las =1)
# title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1, xpd=NA)
# for(i in 1:number_of_levels) {
#   points(x = results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x,
#          y = results_glmm[[levels[i]]]$results_w_glmmTMB_reml$se_effect , col = colors[i], cex=0.1, pch = 19)
# }
# quants = sapply(1:number_of_levels, function(i) quantile(results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x, probs = c(0.7, 0.8,0.9,0.95), na.rm = TRUE))
# where = matrix(NA, number_of_quantiles, number_of_levels)
# for(i in 1:number_of_levels){
#   for(j in 1:number_of_quantiles){
#     p = quants[j,i]
#     ordered_results = results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x[order(results_glmm[[levels[i]]]$results_w_glmmTMB_reml$stddev_randeff_x)]
#     where[j,i] = which(ordered_results >= p)[1] 
#   }
# }
# coords = sapply(1:number_of_levels, function(i) results_glmm[[levels[i]]]$results_w_glmmTMB_reml$se_effect[order(results_glmm[[levels[i]]]$results_w_glmmTMB_reml$se_effect)][where[,i]])
# for(i in 1:number_of_quantiles){
#   lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
# }
# text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)
# 
# legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")
# 
# dev.off()



# 
# # Figure 4 Correlation plot 
# 
# pdf("./Figures/Fig_4.pdf", width = 9, height = 7.5)
# par(mfrow = c(2,2), mar=c(1, 2, 1, 1), oma = c(5, 5, 3, 1))
# plot(NULL, NULL, xlim = c(0, 0.5), ylim = c(0, 0.4), 
#      main = "",  cex.axis = 1.0, xaxt="n",
#      cex.main = 1.0, xlab = "", ylab = "",  las =1, xpd = NA)
# text(x = 0.25, y = 0.42, pos = 3, labels = "Intercept", xpd = NA)
# text(x = -0.15, y = 0.43, labels = "A", font = 2, cex = 1.2, xpd = NA)
# title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 1, xpd = NA)
# #title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1)
# levels = c(2, 3, 5, 8)-1
# number_of_levels = 4
# for(i in 1:number_of_levels) {
#   points(x = results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_inter, 
#          y = results_lmm[[levels[i]]]$results_w_lme4_reml$se_intercept, col = colors[i], cex=0.2, pch = 19)
# }
# quants = sapply(1:number_of_levels, function(i) quantile(results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_inter, probs = c(0.7, 0.8,0.9,0.95)))
# where = matrix(NA, number_of_quantiles, number_of_levels)
# for(i in 1:number_of_levels){
#   for(j in 1:number_of_quantiles){
#     p = quants[j,i]
#     ordered_results = results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_inter[order(results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_inter)]
#     where[j,i] = which(ordered_results >= p)[1] 
#   }
# }
# coords = sapply(1:number_of_levels, function(i) results_lmm[[levels[i]]]$results_w_lme4_reml$se_intercept[order(results_lmm[[levels[i]]]$results_w_lme4_reml$se_intercept)][where[,i]])
# for(i in 1:number_of_quantiles){
#   lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
# }
# text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)
# legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")
# 
# 
# 
# plot(NULL, NULL, xlim = c(0, 0.5), ylim = c(0, 0.4), 
#      main = "",  cex.axis = 1.0,xaxt="n",
#      cex.main = 1.0, xlab = "", ylab = "",  las =1, xpd=NA)
# text(x = 0.25, y = 0.42, pos = 3, labels = "Temperature", xpd = NA)
# 
# title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 1)
# title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1)
# for(i in 1:number_of_levels) {
#   points(x = results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_x,
#          y = results_lmm[[levels[i]]]$results_w_lme4_reml$se_effect , col = colors[i], cex=0.2, pch = 19)
# }
# quants = sapply(1:number_of_levels, function(i) quantile(results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_x, probs = c(0.7, 0.8,0.9,0.95)))
# where = matrix(NA, number_of_quantiles, number_of_levels)
# for(i in 1:number_of_levels){
#   for(j in 1:number_of_quantiles){
#     p = quants[j,i]
#     ordered_results = results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_x[order(results_lmm[[levels[i]]]$results_w_lme4_reml$stddev_randeff_x)]
#     where[j,i] = which(ordered_results >= p)[1] 
#   }
# }
# coords = sapply(1:number_of_levels, function(i) results_lmm[[levels[i]]]$results_w_lme4_reml$se_effect[order(results_lmm[[levels[i]]]$results_w_lme4_reml$se_effect)][where[,i]])
# for(i in 1:number_of_quantiles){
#   lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
# }
# text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)
# legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")
# 
# 
# 
# 
# ## GLMM
# plot(NULL, NULL, xlim = c(0, 0.6), ylim = c(0, 0.5), 
#      main = "",  cex.axis = 1.0,
#      cex.main = 1.0, xlab = "", ylab = "",  las =1)
# title(ylab = "Standard error of fixed effect", line = 4.2, cex.lab = 1, xpd = NA)
# title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1, xpd = NA)
# text(x = -0.18, y = 0.5375, labels = "B", font = 2, cex = 1.2, xpd = NA)
# for(i in 1:number_of_levels) {
#   points(x = results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_inter, 
#          y = results_glmm[[levels[i]]]$results_w_lme4_ml$se_intercept , col = colors[i], cex=0.2, pch = 19)
# }
# quants = sapply(1:number_of_levels, function(i) quantile(results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_inter, probs = c(0.7, 0.8,0.9,0.95)))
# where = matrix(NA, number_of_quantiles, number_of_levels)
# for(i in 1:number_of_levels){
#   for(j in 1:number_of_quantiles){
#     p = quants[j,i]
#     ordered_results = results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_inter[order(results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_inter)]
#     where[j,i] = which(ordered_results >= p)[1] 
#   }
# }
# coords = sapply(1:number_of_levels, function(i) results_glmm[[levels[i]]]$results_w_lme4_ml$se_intercept[order(results_glmm[[levels[i]]]$results_w_lme4_ml$se_intercept)][where[,i]])
# for(i in 1:number_of_quantiles){
#   lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
# }
# text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)
# 
# legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")
# 
# 
# plot(NULL, NULL, xlim = c(0, 0.9), ylim = c(0, 0.6), 
#      main = "",  cex.axis = 1.0,
#      cex.main = 1.0, xlab = "", ylab = "",  las =1)
# title(xlab = "Standard deviation of random effect", line = 3.2, cex.lab = 1, xpd=NA)
# for(i in 1:number_of_levels) {
#   points(x = results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_x,
#          y = results_glmm[[levels[i]]]$results_w_lme4_ml$se_effect , col = colors[i], cex=0.1, pch = 19)
# }
# quants = sapply(1:number_of_levels, function(i) quantile(results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_x, probs = c(0.7, 0.8,0.9,0.95)))
# where = matrix(NA, number_of_quantiles, number_of_levels)
# for(i in 1:number_of_levels){
#   for(j in 1:number_of_quantiles){
#     p = quants[j,i]
#     ordered_results = results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_x[order(results_glmm[[levels[i]]]$results_w_lme4_ml$stddev_randeff_x)]
#     where[j,i] = which(ordered_results >= p)[1] 
#   }
# }
# coords = sapply(1:number_of_levels, function(i) results_glmm[[levels[i]]]$results_w_lme4_ml$se_effect[order(results_glmm[[levels[i]]]$results_w_lme4_ml$se_effect)][where[,i]])
# for(i in 1:number_of_quantiles){
#   lines(y = coords[i,], x = quants[i,], lwd = 1, col = "black")
# }
# text(y = coords[,1], x = quants[,1], c("70%","80%","90%","95%"), offset = 0.5, pos =3, cex = 1)
# 
# legend("bottomright", legend = paste0(levels+1, " mountain ranges"), pch = 19, col = colors, bty = "n")
# 
# dev.off()
# 
# 



par(mfrow = c(2,2), mar = c(2, 1, 4, 2))
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "SD of random intercept", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_inter[results_lmm[[i]]$results_w_lme4_reml$Singularity == 1], adjust = adj), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 15, labels = "A", cex = 1.3, font = 2, xpd =NA)
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "SD of random slope", xlab = "")

for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_x[results_lmm[[i]]$results_w_lme4_reml$Singularity == 1], adjust = 1), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)



plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "SD of random intercept", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_inter[results_lmm[[i]]$results_w_lme4_reml$Singularity == 0], adjust = adj), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 15, labels = "A", cex = 1.3, font = 2, xpd =NA)
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "SD of random slope", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_x[results_lmm[[i]]$results_w_lme4_reml$Singularity == 0], adjust = adj), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain ranges"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)

