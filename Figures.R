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
##########  Random intercept only ########## 
########## _________________________________  ##########
results_lmm = readRDS("Results/results_mountain_lmm_random_intercept_only_0.1_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_random_intercept_only_100_unbalanced.Rds")
results_miss = readRDS("Results/results_mountain_lmm_random_intercept_only_miss_specified_no_cov_0.1_unbalanced.Rds")
########## __Figure  1 Random intercept only  Type I, Error, etc. for lmm ########## 


get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))

cols = RColorBrewer::brewer.pal(5, "Set1")
lty = c(1, 1, 2, 2)
pch = c(15, 16:18)

cex_legend = 0.9
labels = c("Height ~ T + (1|mountain)", 
           "Height ~ T + (1|mountain) + (0 + T|mountain)", 
           "Height ~ T + mountain", 
           "Height ~ T ")
## Type I error ##
pdf(file = "Figures/Fig_1.pdf", width = 9.2, height = 5.8)
si = c(0)
par(mfrow = c(2,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
type_one_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_miss, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si] < 0.05)),
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si] < 0.05, na.rm=TRUE)) #lm w/go goruping
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
    sapply(results_lmm, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_miss, function(l)get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si]))
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
         sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si] < 0.05)), #lme4
        sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si] < 0.05)), 
                sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si] < 0.05, na.rm=TRUE)) #lm
  )
matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of levels")
text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")

legend("bottomright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
         sapply(results_lmm, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
        sapply(results_miss, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
                sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si]))
  )
mm =1-type_two_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
#axis(1, at = 1:7, labels = 2:8)

# coverage
type_two_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si] )), #lme4
    sapply(results_miss, function(l) mean(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si] )), #lme4
    sapply(results_lmm, function(l) mean(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si] , na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si] , na.rm=TRUE)) #lm
  )

matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "")
text(x= 4, pos = 3, y = 1.02, xpd = NA, labels = "Coverage")

legend("bottomright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.95, lty = 3, col = "darkgrey")

sd = 
  cbind(
    sapply(results_lmm, function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_miss, function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si]))
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
    sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si]  < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) mean(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si]  < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
text(x=-0.2, pos = 2, y = 0.55, labels = "B", cex = 1.2, xpd = NA, font = 2)

axis(1, at = 1:7, labels = 2:8)
legend("topright", legend = c("RS ~ T + (1|mountain)", "RS ~ T + mountain", "RS ~ T "), col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)/sqrt( sum(!is.na(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si])) )), #lme4
    sapply(results_glmm, function(l) sd(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si] < 0.05, na.rm=TRUE)/sqrt(sum(!is.na(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si])))), #lm
    sapply(results_glmm, function(l) sd(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si] < 0.05, na.rm=TRUE)/sqrt(sum(!is.na(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si])))) #lm
  )
mm =type_one_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
#text(-0.5, 0.55, labels = "B", cex = 1.3, font = 2, xpd =NA)


## Power ##
type_two_int = 
  cbind(
    sapply(results_glmm, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si]  < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si]  < 0.05, na.rm=TRUE)) #lm
  )

matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = c("RS ~ T + (1|mountain)", "RS ~ T + mountain", "RS ~ T "), col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si]  < 0.05)/sqrt( sum(!is.na( l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si]  < 0.05, na.rm=TRUE)/sqrt( sum(!is.na( l$results_w_glmmTMB_reml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))), # glmmTMB
    sapply(results_glmm, function(l) sd(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si]  < 0.05, na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))) #lm
  )
mm =1-type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))


## Coverage ##
type_two_int = 
  cbind(
    sapply(results_glmm, function(l) mean(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] )), #lme4
    sapply(results_glmm, function(l) mean(l$results_w_lm$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] , na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] , na.rm=TRUE)) #lm
  )


matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
abline(h = 0.95, lty = 3, col = "darkgrey")

axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = c("RS ~ T + (1|mountain)", "RS ~ T + mountain", "RS ~ T "), col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)

sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] )/sqrt(sum(!is.na(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si])))), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si], na.rm=TRUE)/sqrt(sum(!is.na( l$results_w_glmmTMB_reml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] )))), # glmmTMB
    sapply(results_glmm, function(l) sd(l$results_w_lm$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] , na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_lm$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si])))) #lm
  )
mm =type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar=0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar=0.1)$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))

dev.off()



########## __Figure S8 Random intercept different SD for random effect ########## 

cols = RColorBrewer::brewer.pal(4, "Set1")
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1, 2, 2)
cex_legend = 0.9
## Type I error ##
pdf(file = "Figures/Fig_S8.pdf", width = 9.2, height = 8.8)

sd_results = c("0.01", "0.1", "0.5", "2")
par(mfrow = c(4,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
labels = c("A", "B", "C", "D")
xlab = ""

si = c(0)

get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))
for(i in 1:4){
  
  results_lmm = readRDS(paste0("Results/results_mountain_lmm_random_intercept_only_", sd_results[i],"_unbalanced.Rds"))
  results_miss = readRDS(paste0("Results/results_mountain_lmm_random_intercept_only_miss_specified_no_cov_", sd_results[i],"_unbalanced.Rds"))
  
  if(i == 4) xlab = "Number of mountains"
  
  type_one_int = 
    cbind(
      sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si ]< 0.05)), #lme4
      sapply(results_miss, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si ]< 0.05)),
      sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si ] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si ] < 0.05, na.rm=TRUE)) #lm w/go goruping
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
      sapply(results_lmm, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si ])), #lme4
      sapply(results_miss, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si ])),
      sapply(results_lmm, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si ])), #lm
      sapply(results_lmm, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si ] )) #lm w/go goruping
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
      sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si ] < 0.05)), #lme4
      sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si ] < 0.05)), 
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si ] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si ] < 0.05, na.rm=TRUE)) #lm
    )
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd=NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")
  
  legend("bottomright", legend = c("Growth ~ T + (1|mountain)", "Growth ~ T + (1|mountain) + (0 + T|mountain)", "Growth ~ T + mountain", "Growth ~ T "), 
         col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  sd = 
    cbind(
      sapply(results_lmm, function(l) get_sd(l$results_w_lme4_reml$p_value_effect)), #lme4
      sapply(results_miss, function(l) get_sd(l$results_w_lme4_reml$p_value_effect)),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect)), #lm
      sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect)) #lm w/go goruping
    )
  mm =1-type_two_int
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  if(i == 4) axis(1, at = 1:7, labels = 2:8)
  
  # coverage
  type_two_int = 
    cbind(
      sapply(results_lmm, function(l) mean(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si ] )), #lme4
      sapply(results_miss, function(l) mean(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si ] )), #lme4
      sapply(results_lmm, function(l) mean(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si ] , na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si ] , na.rm=TRUE)) #lm
    )
  
  matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:19, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd=NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.02, xpd = NA, labels = "Coverage")
  
  legend("bottomright", legend = c("Growth ~ T + (1|mountain)", "Growth ~ T + (1|mountain) + (0 + T|mountain)", "Growth ~ T + mountain", "Growth ~ T "), 
         col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  abline(h = 0.95, lty = 3, col = "darkgrey")
  
  sd = 
    cbind(
      sapply(results_lmm, function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si ]  )), #lme4
      sapply(results_miss, function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si ]  )), #lme4
      sapply(results_lmm, function(l) get_sd(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si ] )), #lm
      sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si ])) #lm
    )
  
  mm =type_two_int
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  if(i == 4)  axis(1, at = 1:7, labels = 2:8)
}


dev.off()







########## __Figure S10 Random intercept different number of observations for GLMM ########## 
files = c("Results/results_mountain_glmm_random_intercept_only_25_unbalanced.Rds", 
          "Results/results_mountain_glmm_random_intercept_only_50_unbalanced.Rds", 
          "Results/results_mountain_glmm_random_intercept_only_100_unbalanced.Rds", 
          "Results/results_mountain_glmm_random_intercept_only_200_unbalanced.Rds")

pdf("Figures/Fig_S10.pdf",  width = 9.2, height = 8.8)
par(mfrow = c(4,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
cols = RColorBrewer::brewer.pal(3, "Set1")
labels = c("A", "B", "C", "D")
cex_legend = 0.9
get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))
si = c(0)
for(i in 1:4) {
  
  results_glmm = readRDS(files[i])
  
  if(i == 4) xlab = "Number of mountains"
  else xlab = ""
  
  
  ## glmm
  cols = RColorBrewer::brewer.pal(4, "Set1")
  cols = c(cols[-2])
  lty = c(1, 1, 2)
  type_one_int = 
    cbind(
      sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si ] < 0.05)), #lme4
      sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si ] < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) mean(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si ] < 0.05, na.rm=TRUE)) #lm
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
      sapply(results_glmm, function(l) get_sd(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si ] )), #lme4
      sapply(results_glmm, function(l) get_sd(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si ] )), #lm
      sapply(results_glmm, function(l) get_sd(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si ] )) #lm
    )
  mm =type_one_int
  upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  #text(-0.5, 0.55, labels = "B", cex = 1.3, font = 2, xpd =NA)
  
  
  ## Power ##
  type_two_int = 
    cbind(
      sapply(results_glmm, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si ]  < 0.05)), #lme4
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si ]  < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si ]  < 0.05, na.rm=TRUE)) #lm
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
      sapply(results_glmm, function(l) get_sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si ])), #lme4
      sapply(results_glmm, function(l) get_sd(l$results_w_glmmTMB_reml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si ])), # glmmTMB
      sapply(results_glmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si ] )) #lm
    )
  mm =1-type_two_int
  upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
  
  ## Coverage ##
  type_two_int = 
    cbind(
      sapply(results_glmm, function(l) mean(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si ] )), #lme4
      sapply(results_glmm, function(l) mean(l$results_w_lm$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si ] , na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si ] , na.rm=TRUE)) #lm
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
      sapply(results_glmm, function(l) get_sd(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si ] )), #lme4
      sapply(results_glmm, function(l) get_sd(l$results_w_glmmTMB_reml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si ])), # glmmTMB
      sapply(results_glmm, function(l) get_sd(l$results_w_lm$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si ])) #lm
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

results_lmm_no_cov = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_0.1_50_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_0.1_200_unbalanced.Rds")
results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
results_miss = readRDS("Results/results_mountain_lmm_miss_specified_0.1_50_unbalanced.Rds")

get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))


pdf(file = "Figures/Fig_2.pdf", width = 9.2, height = 5.8)


si = c(0)

cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1,1, 2, 2)
pch = c(15, 20, 16:18)
cex_legend = 0.9
## Type I error ##



par(mfrow = c(2,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
labels = c("Height ~ T + (1|mountain) + (0 + T|mountain)",
           "Height ~ T + (T|mountain)", 
           "Height ~ T + (1|mountain)", 
           "Height ~ 0 + mountain + T : mountain", 
           "Height ~ T ")
type_one_int = 
  cbind(
    sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si]< 0.05)), #lme4
    sapply(results_lmm, function(l)        mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si]< 0.05)), #lme4
    sapply(results_miss, function(l)       mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si]< 0.05)),
    sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si] < 0.05, na.rm=TRUE)) #lm w/go goruping
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
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l)        get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_miss, function(l)       get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si]))
  )
mm = type_one_int
upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))

## Power ##
type_two_int = 
  cbind(
    sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), #lme4
    sapply(results_lmm, function(l)        1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), #lme4
    sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), 
    sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05, na.rm=TRUE)) #lm
  )
matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of levels")
text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")

legend("bottomright", legend = labels,
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_lmm,        function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_miss,       function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si]))
  )
mm =1-type_two_int
upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
#axis(1, at = 1:7, labels = 2:8)

# coverage
type_two_int = 
  cbind(
    sapply(results_lmm_no_cov, function(l) mean(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )), #lme4
    sapply(results_lmm,        function(l) mean(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )), #lme4
    sapply(results_miss,       function(l) mean(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )), #lme4
    sapply(results_lmm_no_cov, function(l) mean(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] , na.rm=TRUE)), #lm
    sapply(results_lmm_no_cov, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] , na.rm=TRUE)) #lm
  )

matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "")
text(x= 4, pos = 3, y = 1.02, xpd = NA, labels = "Coverage")

legend("bottomright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.95, lty = 3, col = "darkgrey")

sd = 
  cbind(
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )),
    sapply(results_lmm,        function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )),
    sapply(results_miss,       function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )),
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si])),
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] ))
  )

mm =type_two_int
upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
#axis(1, at = 1:7, labels = 2:8)



## glmm
cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
cols = cols[-3]
lty = c(1, 1, 2, 2)
pch = c(15, 20, 17:18)
si = c(0)
labels = c("RS ~ T + (1|mountain) + (0 + T|mountain)", 
           "RS ~ T + (T|mountain)","RS ~ 0 + T * mountain - T", "RS ~ T ")
type_one_int = 
  cbind(
    sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm,        function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si] < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si] < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
text(x=-0.2, pos = 2, y = 0.55, labels = "B", cex = 1.2, xpd = NA, font = 2)

axis(1, at = 1:7, labels = 2:8)
legend("topright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] )),
    sapply(results_glmm,        function(l) get_sd(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] )),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si] )),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si] ))
  )
mm =type_one_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
#text(-0.5, 0.55, labels = "B", cex = 1.3, font = 2, xpd =NA)


## Power ##
type_two_int = 
  cbind(
    sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm,        function(l) 1-mean(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si]  < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si]  < 0.05, na.rm=TRUE)) #lm
  )

matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si])),
    sapply(results_glmm,        function(l) get_sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si])),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si])),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] ))
  )
mm =1-type_two_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))


## Coverage ##
type_two_int = 
  cbind(
    sapply(results_glmm_no_cov, function(l) mean(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] )), #lme4
    sapply(results_glmm,        function(l) mean(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] )), #lme4
    sapply(results_glmm_no_cov, function(l) mean(l$results_w_lm$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si]  , na.rm=TRUE)), #lm
    sapply(results_glmm_no_cov, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si]  , na.rm=TRUE)) #lm
  )


matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
abline(h = 0.95, lty = 3, col = "darkgrey")

axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)

sd = 
  cbind(
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] )),
    sapply(results_glmm,        function(l) get_sd(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] )),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lm$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] )),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si]))
  )
mm =type_two_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar=0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar=0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))

dev.off()



########## __Figure S9 Random intercept+slope different SD for random effect ########## 

cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1,1, 2, 2)
pch = c(15, 20, 16:18)
## Type I error ##
pdf(file = "Figures/Fig_S9.pdf", width = 9.2, height = 8.8)

sds = c(0.01, 0.1, 0.5, 2)
par(mfrow = c(4,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
labels = c("A", "B", "C", "D")
xlab = ""
legend_label = c("Height ~ T + (1|mountain) + (0 + T|mountain)",
                 "Height ~ T + (T|mountain)", 
                 "Height ~ T + (1|mountain)", 
                 "Height ~ 0 + T * mountain - T", 
                 "Height ~ T ")
for(i in 1:4){
  
  results_lmm = readRDS(paste0("Results/results_mountain_lmm_",sds[i],"_", 50,"_unbalanced.Rds"))
  results_lmm_no_cov = readRDS(paste0("Results/results_mountain_lmm_no_cov_",sds[i],"_", 50,"_unbalanced.Rds"))
  results_miss = readRDS(paste0("Results/results_mountain_lmm_miss_specified_",sds[i],"_", 50,"_unbalanced.Rds"))
  
  if(i == 4) xlab = "Number of mountains"
  
  type_one_int = 
    cbind(
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si]< 0.05)), #lme4
      sapply(results_lmm, function(l)        mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si]< 0.05)), #lme4
      sapply(results_miss, function(l)       mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si]< 0.05)),
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si] < 0.05, na.rm=TRUE)) #lm w/go goruping
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
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
      sapply(results_lmm, function(l)        get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
      sapply(results_miss, function(l)       get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si]))
    )
  mm = type_one_int
  upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  
  ## Power ##
  type_two_int = 
    cbind(
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), #lme4
      sapply(results_lmm, function(l)        1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), #lme4
      sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), 
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05, na.rm=TRUE)) #lm
    )
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd = NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")
  if(i == 4) axis(1, at = 1:7, labels = 2:8)
  legend("bottomright", legend = legend_label,
         col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
  sd = 
    cbind(
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
      sapply(results_lmm,        function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
      sapply(results_miss,       function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si]))
    )
  mm =1-type_two_int
  upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  #axis(1, at = 1:7, labels = 2:8)
  
  # coverage
  type_two_int = 
    cbind(
      sapply(results_lmm_no_cov, function(l) mean(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )), #lme4
      sapply(results_lmm,        function(l) mean(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )), #lme4
      sapply(results_miss,       function(l) mean(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )), #lme4
      sapply(results_lmm_no_cov, function(l) mean(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] , na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] , na.rm=TRUE)) #lm
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
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )),
      sapply(results_lmm,        function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )),
      sapply(results_miss,       function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si])),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] ))
    )
  
  mm =type_two_int
  upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
  
}
dev.off()


########## __Figure S11 Random intercept+slope different observations for random effect ########## 

cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1,1, 2, 2)
pch = c(15, 20, 16:18)
## Type I error ##
pdf(file = "Figures/Fig_S11.pdf", width = 9.2, height = 8.8)

sds = c(0.01, 0.1, 0.5, 2)
par(mfrow = c(4,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
labels = c("A", "B", "C", "D")
xlab = ""
legend_label = c("Height ~ T + (1|mountain) + (0 + T|mountain)",
                 "Height ~ T + (T|mountain)", 
                 "Height ~ T + (1|mountain)", 
                 "Height ~ 0 + T * mountain - T", 
                 "Height ~ T ")
obs = c( 50, 100, 200, 500)
sds = 0.1
for(i in 1:4){
  
  results_lmm = readRDS(paste0("Results/results_mountain_lmm_",sds,"_", obs[i],"_unbalanced.Rds"))
  results_lmm_no_cov = readRDS(paste0("Results/results_mountain_lmm_no_cov_",sds,"_", obs[i],"_unbalanced.Rds"))
  results_miss = readRDS(paste0("Results/results_mountain_lmm_miss_specified_",sds,"_", obs[i],"_unbalanced.Rds"))
  
  if(i == 4) xlab = "Number of mountains"
  
  type_one_int = 
    cbind(
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si]< 0.05)), #lme4
      sapply(results_lmm, function(l)        mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si]< 0.05)), #lme4
      sapply(results_miss, function(l)       mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si]< 0.05)),
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si] < 0.05, na.rm=TRUE)) #lm w/go goruping
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
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
      sapply(results_lmm, function(l)        get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
      sapply(results_miss, function(l)       get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si]))
    )
  mm = type_one_int
  upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  
  ## Power ##
  type_two_int = 
    cbind(
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), #lme4
      sapply(results_lmm, function(l)        1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), #lme4
      sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), 
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05, na.rm=TRUE)) #lm
    )
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd = NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")
  if(i == 4) axis(1, at = 1:7, labels = 2:8)
  legend("bottomright", legend = legend_label,
         col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
  sd = 
    cbind(
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
      sapply(results_lmm,        function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
      sapply(results_miss,       function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si]))
    )
  mm =1-type_two_int
  upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  #axis(1, at = 1:7, labels = 2:8)
  
  # coverage
  type_two_int = 
    cbind(
      sapply(results_lmm_no_cov, function(l) mean(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )), #lme4
      sapply(results_lmm,        function(l) mean(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )), #lme4
      sapply(results_miss,       function(l) mean(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )), #lme4
      sapply(results_lmm_no_cov, function(l) mean(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] , na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] , na.rm=TRUE)) #lm
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
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )),
      sapply(results_lmm,        function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )),
      sapply(results_miss,       function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] )),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si])),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si] ))
    )
  
  mm =type_two_int
  upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
  
}
dev.off()






########## __Figure S12 Random intercept different number of observations for GLMM ########## 
files = c( "Results/results_mountain_glmm_no_cov_0.1_25_unbalanced.Rds", 
           "Results/results_mountain_glmm_no_cov_0.1_50_unbalanced.Rds", 
          "Results/results_mountain_glmm_no_cov_0.1_100_unbalanced.Rds", 
          "Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds",
          "Results/results_mountain_glmm_no_cov_0.1_500_unbalanced.Rds")


files_with_cov = c("Results/results_mountain_glmm_0.1_25_unbalanced.Rds",
                   "Results/results_mountain_glmm_0.1_50_unbalanced.Rds",
                   "Results/results_mountain_glmm_0.1_100_unbalanced.Rds",
                   "Results/results_mountain_glmm_0.1_200_unbalanced.Rds",
                   "Results/results_mountain_glmm_0.1_500_unbalanced.Rds")


pdf("Figures/Fig_S12.pdf", width = 8.2, height = 8.8)
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
  
  if(i == 5) xlab = "Number of mountains"
  else xlab = ""
  
  
  ## glmm
  lty = c(1, 1, 2, 2)
  type_one_int = 
    cbind(
      sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm,        function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si] < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) mean(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si] < 0.05, na.rm=TRUE)) #lm
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
      sapply(results_glmm_no_cov, function(l) get_sd(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] )),
      sapply(results_glmm,        function(l) get_sd(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] )),
      sapply(results_glmm, function(l) get_sd(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si] )),
      sapply(results_glmm, function(l) get_sd(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si] ))
    )
  mm =type_one_int
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  #text(-0.5, 0.55, labels = "B", cex = 1.3, font = 2, xpd =NA)
  
  
  ## Power ##
  type_two_int = 
    cbind(
      sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm,        function(l) 1-mean(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si]  < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si]  < 0.05, na.rm=TRUE)) #lm
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
      sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si])),
      sapply(results_glmm,        function(l) get_sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si])),
      sapply(results_glmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si])),
      sapply(results_glmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] ))
    )
  mm =1-type_two_int
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
  
  ## Coverage ##
  type_two_int = 
    cbind(
      sapply(results_glmm_no_cov, function(l) mean(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] )), #lme4
      sapply(results_glmm,        function(l) mean(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] )), #lme4
      sapply(results_glmm, function(l) mean(l$results_w_lm$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si]  , na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si]  , na.rm=TRUE)) #lm
    )
  
  
  
  matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd = NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.02, xpd = NA, labels = "Coverage")
  abline(h = 0.95, lty = 3, col = "darkgrey")
  
  if(i == 5)  axis(1, at = 1:7, labels = 2:8)
  legend("bottomright", legend = legend, col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  
  sd = 
    cbind(
      sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] )),
      sapply(results_glmm,        function(l) get_sd(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] )),
      sapply(results_glmm, function(l) get_sd(l$results_w_lm$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si] )),
      sapply(results_glmm, function(l) get_sd(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si]))
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
########## __Figure S7  Random intercept+slope Type I, Power, Cover########## 

results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")


cols = RColorBrewer::brewer.pal(4, "Set1")
lty = c(1, 1, 2, 2)
## Type I error ##
pdf(file = "Figures/Fig_S7.pdf", width = 8.2, height = 5.8)
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
        ylab = "Rate", xaxt="n", main = "", xlab = "number of mountains", xpd = NA)
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
        ylab = "", xaxt="n", main = "", xlab = "number of mountains", xpd = NA)
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
        ylab = "", xaxt="n", main = "", xlab = "number of mountains", xpd = NA)
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

results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")

adj = 1.0
pdf(file = "Figures/Fig_3.pdf", width = 7, height = 5)

par(mfrow = c(2,2), mar = c(2, 1, 4, 2))


plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_inter, 
                                      adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 15, labels = "A", cex = 1.3, font = 2, xpd =NA)
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 15, labels = "B", cex = 1.3, font = 2, xpd =NA)




plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_inter[results_lmm[[i]]$results_w_lme4_reml$Singularity == 0], 
                                      adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
text(-0.03, 15, labels = "C", cex = 1.3, font = 2, xpd =NA)

axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_x[results_lmm[[i]]$results_w_lme4_reml$Singularity == 0], adjust = adj), col = ff(i), lwd = 1.4)
axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " mountain"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 15, labels = "D", cex = 1.3, font = 2, xpd =NA)

dev.off()


########## __Figure S1 Variance estimates glmmTMB ########## 

cols = viridis::viridis(5)
adj = 1.0
pdf(file = "Figures/Fig_S1.pdf", width = 7, height = 5)

results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
par(mfrow = c(2,2), mar = c(2, 1, 4, 2))
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm=TRUE), col = ff(i), lwd = 1.4)
axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " mountains"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 15, labels = "A", cex = 1.3, font = 2, xpd =NA)
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_x, adjust = adj, na.rm=TRUE), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountains"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 15, labels = "B", cex = 1.3, font = 2, xpd =NA)



plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_glmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " mountains"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.04, 15, labels = "C", cex = 1.3, font = 2, xpd =NA)

plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_glmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_x, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " mountains"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.04, 15, labels = "D", cex = 1.3, font = 2, xpd =NA)

dev.off()


########## __Figure S2 Variance estimates glmmTMB ########## 

pdf(file = "Figures/Fig_S2.pdf", width = 7, height = 5)

par(mfrow = c(2,2), mar = c(2, 1, 4, 2))


plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, 
                                      adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " mountains"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 15, labels = "A", cex = 1.3, font = 2, xpd =NA)
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_x, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountains"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 15, labels = "B", cex = 1.3, font = 2, xpd =NA)

threshold = 1e-3

plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter[(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter > threshold) & (results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_x > threshold)], 
                                      adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " mountains"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 15, labels = "C", cex = 1.3, font = 2, xpd =NA)
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 15.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_x[(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter > threshold) & (results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_x > threshold)], adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " mountains"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.03, 15, labels = "D", cex = 1.3, font = 2, xpd =NA)

dev.off()



########## __Figure S3 Variance estimates lmm MLE/REML ########## 
cols = viridis::viridis(5)

adj = 1.0
pdf(file = "Figures/Fig_S3.pdf", width = 7, height = 5)

par(mfrow = c(2,2), mar = c(2, 1+0.3, 4, 2))

# lme4
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " mountains"), pch = 15, col = cols, bty = "n")
#legend("bottomright", legend = c("REML", "MLE"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.04, 32, labels = "A", cex = 1.3, font = 2, xpd =NA)


plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_ml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
text(-0.04, 32, labels = "B", cex = 1.3, font = 2, xpd =NA)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountains"), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("REML", "MLE"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)


# glmmTMB
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " mountains"), pch = 15, col = cols, bty = "n")
#legend("bottomright", legend = c("REML", "MLE"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.05, 32, labels = "C", cex = 1.3, font = 2, xpd =NA)


plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
text(-0.05, 32, labels = "D", cex = 1.3, font = 2, xpd =NA)
axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " mountains"), pch = 15, col = cols, bty = "n")
#legend("bottomright", legend = c("REML", "MLE"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)


dev.off()









########## __Figure S4 Variance estimates glmm MLE/REML ########## 
cols = viridis::viridis(5)

results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")


# glmm4

adj = c(1e6, 1.0, 1.0, 1.0)*1
pdf(file = "Figures/Fig_S4.pdf", width = 7, height = 5)

par(mfrow = c(2,2), mar = c(2, 1+0.3, 4, 2))
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 28.0), axes= FALSE, ylab = "", main = "", xlab = "")
counter = 1
for(i in c(1, 2, 4, 7)) {
  lines(density(results_glmm[[i]]$results_w_lme4_ml$stddev_randeff_inter, adjust = adj[counter]), col = ff(i), lwd = 1.4, lty = 2)
  counter = counter+1
}
adj = 1.0
axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " moutains"), pch = 15, col = cols, bty = "n")
abline(v=0.1, col="darkgrey", lty = 3)

text(-0.04, 28, labels = "A", cex = 1.3, font = 2, xpd =NA)

plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 28.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_ml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " moutains"), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("REML", "MLE"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.04, 28, labels = "B", cex = 1.3, font = 2, xpd =NA)


# glmmTMB ML
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 37.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_ml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4, lty = 2)

axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " moutains"), pch = 15, col = cols, bty = "n")
#legend("bottomright", legend = c("REML", "ML"), lty = c(1, 2), bty = "n", lwd = 1.0)

abline(v=0.1, col="darkgrey", lty = 3)
text(-0.05, 37, labels = "C", cex = 1.3, font = 2, xpd =NA)

adj = c(100, 1.0, 1.0, 1.0)
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 37.0), axes= FALSE, ylab = "", main = "", xlab = "")
counter = 1
for(i in c(1, 2, 4, 7)) {
  lines(density(results_lmm[[i]]$results_w_glmmTMB_ml$stddev_randeff_x, adjust = adj[counter], na.rm = TRUE), col = ff(i), lwd = 1.4, lty = 2)
  counter = counter + 1
}
adj = 1
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_x, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
axis(1)
#legend("topright", legend = paste0(c(2, 3, 5, 8), " moutains"), pch = 15, col = cols, bty = "n")
#legend("bottomright", legend = c("REML", "ML"), lty = c(1, 2), bty = "n", lwd = 1.0)
text(-0.05, 37, labels = "D", cex = 1.3, font = 2, xpd =NA)
abline(v=0.1, col="darkgrey", lty = 3)
dev.off()








########## __Figure S5 Variance estimates lmm REML imbalanced vs REML balanced ########## 
cols = viridis::viridis(5)

results_lmm_im = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_.Rds")


adj = 1.0
pdf(file = "Figures/Fig_S5.pdf", width = 7, height = 5)

par(mfrow = c(2,2), mar = c(2, 1+0.3, 4, 2))

# lme4
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm_im[[i]]$results_w_lme4_reml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.04, 32, labels = "A", cex = 1.3, font = 2, xpd =NA)


plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm_im[[i]]$results_w_lme4_reml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_reml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
text(-0.04, 32, labels = "B", cex = 1.3, font = 2, xpd =NA)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountains"), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("imbalanced", "balanced"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)


# glmmTMB
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm_im[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.05, 32, labels = "C", cex = 1.3, font = 2, xpd =NA)


plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm_im[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4, lty = 2)
text(-0.05, 32, labels = "D", cex = 1.3, font = 2, xpd =NA)
axis(1)
abline(v=0.1, col="darkgrey", lty = 3)

dev.off()




########## __Figure S6 Variance estimates lmm MLE imbalanced vs MLE balanced ########## 
cols = viridis::viridis(5)

results_lmm_im = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_.Rds")

adj = 1.0
pdf(file = "Figures/Fig_S6.pdf", width = 7, height = 5)

par(mfrow = c(2,2), mar = c(2, 1+0.3, 4, 2))

# lme4
plot(NULL, NULL, xlim = c(0.0, 0.5), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm_im[[i]]$results_w_lme4_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.04, 32, labels = "A", cex = 1.3, font = 2, xpd =NA)


plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm_im[[i]]$results_w_lme4_ml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_lme4_ml$stddev_randeff_x, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
text(-0.04, 32, labels = "B", cex = 1.3, font = 2, xpd =NA)
axis(1)
legend("topright", legend = paste0(c(2, 3, 5, 8), " mountains"), pch = 15, col = cols, bty = "n")
legend("bottomright", legend = c("imbalanced", "balanced"), lty = c(1, 2), bty = "n", lwd = 1.0)
abline(v=0.1, col="darkgrey", lty = 3)


# glmmTMB
plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm_im[[i]]$results_w_glmmTMB_ml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
axis(1)
abline(v=0.1, col="darkgrey", lty = 3)
text(-0.05, 32, labels = "C", cex = 1.3, font = 2, xpd =NA)


plot(NULL, NULL, xlim = c(0.0, 0.6), ylim = c(0., 32.0), axes= FALSE, ylab = "", main = "", xlab = "")
for(i in c(1, 2, 4, 7)) lines(density(results_lmm_im[[i]]$results_w_glmmTMB_ml$stddev_randeff_inter, adjust = adj, na.rm = TRUE), col = ff(i), lwd = 1.4)
for(i in c(1, 2, 4, 7)) lines(density(results_lmm[[i]]$results_w_glmmTMB_ml$stddev_randeff_inter, adjust = adj), col = ff(i), lwd = 1.4, lty = 2)
text(-0.05, 32, labels = "D", cex = 1.3, font = 2, xpd =NA)
axis(1)
abline(v=0.1, col="darkgrey", lty = 3)

dev.off()









########## __Figure 4 Singularities:Type I ########## 
results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
pdf(file = "Figures/Fig_4.pdf", width = 8, height = 6.4)


labels = c("Singular fit: Height ~ T + (1|mountain) + (0 + T|mountain)",
           "Non-singular fit: Height ~ T + (1|mountain) + (0 + T|mountain)",
           "Height ~ T")

labels2 = c("Singular fit: Height ~ 0 + mountain + T * mountain",
            "Non-singular fit: Height ~ 0 + mountain + T * mountain")
par(mfrow = c(2,2), mar = c(0.1, 2.4, 2.2, 1), oma = c(5, 3, 3, 1)-1)
cols = RColorBrewer::brewer.pal(3, "Set1")[1:2]
get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))


cols2 = RColorBrewer::brewer.pal(5, "Set1")[4]
lty = c(1, 1, 2)
pch = c(15, 15, 18)
cex_legend = 0.7


## Type I error ##

cols = c(cols, cols2)

sing = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 1] < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 0] < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity == 1] < 0.05, na.rm = TRUE))
  )

matplot(sing, type="o", ylim = c(0, 0.3), pch = pch, las = 1, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "", lty = lty, cex.main = 0.7, xpd = NA)
legend("topright", legend = labels, col = cols, pch = pch,lty = lty, bty = "n", cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
#axis(1, at = 1:7, labels = 2:8)
text(xpd=NA, x = -0.0, y = 0.33, labels = "A", cex = 1.1, font = 2)
text(x= 4, pos = 3, y = 0.3*1.04, xpd = NA, labels = "Type I error")

sd = 
  cbind(
    sapply(results_lmm, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 1])),
    sapply(results_lmm, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity == 0])),
    sapply(results_lmm, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity == 1]))
  )
mm =sing
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:2, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
#axis(1, at = 1:7, labels = 2:8)


## Power
sing = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity == 1] < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity == 0] < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity == 1] < 0.05, na.rm = TRUE))
  )

matplot(sing, type="o", ylim = c(0, 1.0), pch = pch, las = 1, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "", lty = lty, cex.main = 0.7, xpd = NA)
legend("bottomright", legend = labels, col = cols, pch = pch,lty = lty, bty = "n", cex = cex_legend)
#axis(1, at = 1:7, labels = 2:8)
text(x= 4, pos = 3, y = 1.0*1.04, xpd = NA, labels = "Power")
text(xpd=NA, x = -0.0, y = 1.1, labels = "B", cex = 1.1, font = 2)

sd = 
  cbind(
    sapply(results_lmm, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity == 1])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity == 0])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity == 1]))
  )
mm =sing
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:2, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
#axis(1, at = 1:7, labels = 2:8)



### Type I error rate
sing = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity == 1]  < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity == 0]  < 0.05, na.rm = TRUE)))

matplot(sing, type="o", ylim = c(0, 0.3), pch = pch, las = 1, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "Number of mountains", lty = lty, cex.main = 0.7, xpd = NA)
legend("topright", legend = labels2, col = cols, pch = pch,lty = lty, bty = "n", cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
axis(1, at = 1:7, labels = 2:8)

text(xpd=NA, x = -0.0, y = 0.33, labels = "C", cex = 1.1, font = 2)
sd = 
  cbind(
    sapply(results_lmm, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity == 1])),
    sapply(results_lmm, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity == 0])))
mm =sing
upper = sapply(1:2, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:2, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:2, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))





sing = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity == 1]  < 0.05, na.rm = TRUE)),
    sapply(results_lmm, function(l) mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity == 0]  < 0.05, na.rm = TRUE)))

matplot(sing, type="o", ylim = c(0, 1.0), pch = pch, las = 1, col = cols, xpd =NA,
        ylab = "", xaxt="n", main = "", xlab = "Number of mountains", lty = lty, cex.main = 0.7)
legend("bottomright", legend = labels2, col = cols, pch = pch,lty = lty, bty = "n", cex = cex_legend)
axis(1, at = 1:7, labels = 2:8)
text(xpd=NA, x = -0.0, y = 1.1, labels = "D", cex = 1.1, font = 2)

sd = 
  cbind(
    sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity == 1])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity == 0])))
mm =sing
upper = sapply(1:2, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:2, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:2, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))



dev.off()







########## __Figure 5 LM ########## 
test = readRDS("Results/C_results_mountain_unbalanced.RDS")

addCI = function(x, se, m) {
  bb = 0.005
  segments(y0=x, y1=x, x0=m-1.96*se, x1 = m+1.96*se)
  
  segments(x0=m-1.96*se, x1= m-1.96*se, y0=x-bb, y1=x+bb)
  segments(x0=m+1.96*se, x1= m+1.96*se, y0=x-bb, y1=x+bb)
}

plotEffects = function(eff, se, cols = c("grey", "red"), labels = rep("",20), XX = rep(0,20)) {
  eff = eff[nrow(eff):1,]
  se = se[nrow(se):1,]
  a = 0.8
  n = nrow(eff)+1
  barsX = matrix(seq(0, a, length.out = n), byrow = TRUE , nrow = n, ncol = 1)
  barsX = barsX + sapply(0:(n-1), function(i) i*(1.0-a)/(n-1)) 
  b = a/n/2
  for(i in 1:(n-1)) {
    rect(ytop = barsX[i,1], xleft = 0, ybottom = barsX[i,1]+b, xright = eff[i,1], col = cols[1] )
    
    addCI(barsX[i,1]+b/2, se[i,1], eff[i,1])
    
    rect(ytop = barsX[i,1]+b, xleft= 0, ybottom = barsX[i,1]+2*b,xright = eff[i,2], col = cols[2] )
    
    addCI(barsX[i,1]+b+b/2, se[i,2], eff[i,2])
    
    text(x = XX, y = barsX[i,1]+b, xpd = NA, labels=labels[i], pos = 2)
  }
  abline(v = 0, lty = 2, col = "grey")
}

labels = c("Height ~ T + (1|mountain) + (0 + T|mountain)",
           "Height ~ 0 + mountain + T * mountain")

labels_y = (c( "mountain:nobs", "sd:nobs",      "sd:mountain" , "imbalanced", "nobs"   ,      "mountain" ,     "sd" , "Average value"   ))
labels_y = c("nobs:imbalanced","mountain:imbalanced","mountain:nobs","sd:imbalanced","sd:nobs","sd:mountain","imbalanced","nobs" ,"mountain" ,"sd","Average value"  )

pdf(file="Figures/Fig_5.pdf", width = 9.4,  height = 8)
cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = addA(cols2[1:2], 0.5)
par(mfrow = c(2,2), mar = c(1,1,0.3,1), oma = c(8, 8, 4, 1))
## All mountain < 10 
sub = test[test$moutain <= 10 & test$moutain > 1 , ]
sub$sd = scale(sub$sd)
sub$nobs = scale(sub$nobs)
sub$moutain = scale((sub$moutain)) 
sub$balanced = scale((sub$balanced)) 
form = function(resp) paste0(resp, "~ sd + moutain + nobs + balanced + sd:moutain + sd:nobs + moutain:nobs")
form = function(resp) paste0(resp, "~ .^2")
powerLM = (lm( form("PowerLM") , data = sub[,c(1:4, 8)]))
powerLMM = (lm(form("PowerLMM") , data = sub[,c(1:4, 7)] ))
TypeLM = (lm(form("TypeOneLM - 0.05"), data = sub[,c(1:4, 6)] ))
TypeLMM = (lm(form("TypeOneLMM - 0.05"), data = sub[,c(1:4, 5)] ))


cex_legend = 0.7

plot(NULL, NULL, ylim = c(0, 1), xlim = c(-0.015, 0.015), yaxt="n",xaxt="n", las = 1)
coefsType = cbind(coef(TypeLMM), coef(TypeLM))
seType = cbind(coef(summary(TypeLMM))[,2], coef(summary(TypeLM))[,2] )
plotEffects(coefsType, seType, cols = cols, XX = -0.016, labels =labels_y)
axis(3)
text(-0.015*0.95, y = 1.0, pos = 2, xpd = NA, cex = 1.1, labels = "A", font = 2 )
text(0.0, y = 1.2, pos = 3, labels = "Effect on type I error rate - 5%", xpd = NA)

plot(NULL, NULL, ylim = c(0, 1), xlim = c(-0.5, 0.5), yaxt="n",xaxt="n", las = 1)
coefsType = cbind(coef(powerLMM), coef(powerLM))
seType = cbind(coef(summary(powerLMM))[,2], coef(summary(powerLM))[,2] )
plotEffects(coefsType, seType, cols = cols)
axis(3)
text(-0.5*0.95, y = 1.0, pos = 2, xpd = NA, cex = 1.1, labels = "B", font = 2 )
text(0.0, y = 1.2, pos = 3, labels = "Effect on power", xpd = NA)

## All mountain > 10 
sub = test[test$moutain > 10, ]
sub$sd = scale(sub$sd)
sub$nobs = scale(sub$nobs)
sub$moutain = scale((sub$moutain)) 
sub$balanced = scale((sub$balanced)) 
form = function(resp) paste0(resp, "~ sd + moutain + nobs + balanced + sd:moutain + sd:nobs + moutain:nobs")
form = function(resp) paste0(resp, "~ .^2")
powerLM = (lm( form("PowerLM") , data = sub[,c(1:4, 8)]))
powerLMM = (lm(form("PowerLMM") , data = sub[,c(1:4, 7)] ))
TypeLM = (lm(form("TypeOneLM - 0.05"), data = sub[,c(1:4, 6)] ))
TypeLMM = (lm(form("TypeOneLMM - 0.05"), data = sub[,c(1:4, 5)] ))

plot(NULL, NULL, ylim = c(0, 1), xlim = c(-0.015, 0.015), yaxt="n", las = 1)
coefsType = cbind(coef(TypeLMM), coef(TypeLM))
seType = cbind(coef(summary(TypeLMM))[,2], coef(summary(TypeLM))[,2] )
plotEffects(coefsType, seType, cols = cols,XX = -0.016, labels =labels_y)
text(-0.015*0.95, y = 1.0, pos = 2, xpd = NA, cex = 1.1, labels = "C", font = 2 )
text(0.0, y = -0.2, pos = 1, labels = "Effect on type I error rate - 5 %", xpd = NA)

plot(NULL, NULL, ylim = c(0, 1), xlim = c(-0.5, 0.5), yaxt="n", las = 1)
coefsType = cbind(coef(powerLMM), coef(powerLM))
seType = cbind(coef(summary(powerLMM))[,2], coef(summary(powerLM))[,2] )
plotEffects(coefsType, seType, cols = cols)
legend(x = -0.28, y = -0.33, xpd = NA, col = cols, pch = 16,legend = labels, bty = "n")
text(-0.5*0.95, y = 1.0, pos = 2, xpd = NA, cex = 1.1, labels = "D", font = 2 )
text(0.0, y = -0.2, pos = 1, labels = "Effect on power", xpd = NA)

dev.off()




########## _________________________________  ##########
########## Singular fit proportions  ##########
########## _________________________________  ##########

results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
cbind(
sapply(results_lmm, function(l )mean(l$results_w_lme4_reml$Singularity, na.rm=TRUE)),
sapply(results_lmm, function(l )mean(l$results_w_lme4_ml$Singularity, na.rm=TRUE)),
sapply(results_glmm, function(l )mean(l$results_w_lme4_ml$Singularity, na.rm=TRUE))
)
