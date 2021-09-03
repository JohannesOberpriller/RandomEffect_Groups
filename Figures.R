########## Helper functions ########## 
addA = function(col, alpha = 0.25) apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))

# calculate p-values by z-statistic
get_p_from_z = function(obj) {
  z = obj$estimate_effect / obj$se_effect
  p = 2*stats::pnorm(abs(z),lower.tail = FALSE)
  return(p)
}

# calculate p-values by z-statistic
change_to_z = function(obj, beta = 0.4) {
  p_value = get_p_from_z(obj)
  coverage = ((obj$estimate_effect - (1.96*obj$se_effect)) < beta ) & ( beta < (obj$estimate_effect + (1.96*obj$se_effect)))
  data = data.frame(cbind(obj[,1], p_value, obj[,3], coverage))
  colnames(data) = colnames(obj)
  return(data)
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
           "Height ~ 0 + T + mountain", 
           "Height ~ T ")


## Type I error ##
pdf(file = "Figures/Fig_1.pdf", width = 9.2, height = 7.8)
si = c(0)
si2 = c(0, 1)
par(mfrow = c(2,2), mar = c(2.1, 2.4, 1.5, 1), oma = c(4, 3, 3, 1)-1)
type_one_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_miss, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si] < 0.05)),
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "", xpd = NA)
text(x=0.5, pos = 2, y = 0.55, labels = "a", cex = 1.2, xpd = NA, font = 2)
text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
legend("topright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_lmm, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_miss, function(l)get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2])),
    sapply(results_lmm, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2]))
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
                sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm
  )
matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of levels")
text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")
text(x=0.5, pos = 2, y = 1.1, labels = "b", cex = 1.2, xpd = NA, font = 2)

legend("bottomright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
         sapply(results_lmm, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
        sapply(results_miss, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
                sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2]))
  )
mm =1-type_two_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))


## glmm
cols = RColorBrewer::brewer.pal(4, "Set1")
cols = c(cols[-2])
lty = c(1, 1, 2)
type_one_int = 
  cbind(
    sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) mean(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
text(x=0.5, pos = 2, y = 0.55, labels = "c", cex = 1.2, xpd = NA, font = 2)

axis(1, at = 1:7, labels = 2:8)
legend("topright", legend = c("RS ~ T + (1|mountain)", "RS ~ 0 + T + mountain", "RS ~ T "), col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)/sqrt( sum(!is.na(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si])) )), #lme4
    sapply(results_glmm, function(l) sd(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)/sqrt(sum(!is.na(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si])))), #lm
    sapply(results_glmm, function(l) sd(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)/sqrt(sum(!is.na(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si])))) #lm
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
    sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
  )

matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
axis(1, at = 1:7, labels = 2:8)
text(x=0.5, pos = 2, y = 1.1, labels = "d", cex = 1.2, xpd = NA, font = 2)
legend("bottomright", legend = c("RS ~ T + (1|mountain)", "RS ~ 0 + T + mountain", "RS ~ T "), col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si]  < 0.05)/sqrt( sum(!is.na( l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)/sqrt( sum(!is.na( l$results_w_glmmTMB_reml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))), # glmmTMB
    sapply(results_glmm, function(l) sd(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))) #lm
  )
mm =1-type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))

dev.off()


########## __Figure  S8 Random intercept only  Type I, Error, etc. for lmm with SI ########## 


get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))

cols = RColorBrewer::brewer.pal(5, "Set1")
lty = c(1, 1, 2, 2)
pch = c(15, 16:18)

cex_legend = 0.9
labels = c("Height ~ T + (1|mountain)", 
           "Height ~ T + (1|mountain) + (0 + T|mountain)", 
           "Height ~ 0 + T + mountain", 
           "Height ~ T ")


## Type I error ##
pdf(file = "Figures/Fig_S8.pdf", width = 9.2, height = 7.8)
si = c(0, 1)
si2 = c(0, 1)
par(mfrow = c(2,2), mar = c(2.1, 2.4, 1.5, 1), oma = c(4, 3, 3, 1)-1)
type_one_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_miss, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si] < 0.05)),
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "", xpd = NA)
text(x=0.5, pos = 2, y = 0.55, labels = "a", cex = 1.2, xpd = NA, font = 2)
text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
legend("topright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_lmm, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_miss, function(l)get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2])),
    sapply(results_lmm, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2]))
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
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm
  )
matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of levels")
text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")
text(x=0.5, pos = 2, y = 1.1, labels = "b", cex = 1.2, xpd = NA, font = 2)

legend("bottomright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_lmm, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_miss, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2]))
  )
mm =1-type_two_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))


## glmm
cols = RColorBrewer::brewer.pal(4, "Set1")
cols = c(cols[-2])
lty = c(1, 1, 2)
type_one_int = 
  cbind(
    sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) mean(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
text(x=0.5, pos = 2, y = 0.55, labels = "c", cex = 1.2, xpd = NA, font = 2)

axis(1, at = 1:7, labels = 2:8)
legend("topright", legend = c("RS ~ T + (1|mountain)", "RS ~ 0 + T + mountain", "RS ~ T "), col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)/sqrt( sum(!is.na(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si])) )), #lme4
    sapply(results_glmm, function(l) sd(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)/sqrt(sum(!is.na(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si])))), #lm
    sapply(results_glmm, function(l) sd(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)/sqrt(sum(!is.na(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si])))) #lm
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
    sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
  )

matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
axis(1, at = 1:7, labels = 2:8)
text(x=0.5, pos = 2, y = 1.1, labels = "d", cex = 1.2, xpd = NA, font = 2)
legend("bottomright", legend = c("RS ~ T + (1|mountain)", "RS ~ 0 + T + mountain", "RS ~ T "), col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si]  < 0.05)/sqrt( sum(!is.na( l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)/sqrt( sum(!is.na( l$results_w_glmmTMB_reml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))), # glmmTMB
    sapply(results_glmm, function(l) sd(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))) #lm
  )
mm =1-type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))

dev.off()


########## __Figure  S9 Random intercept only  Type I, Error, etc. for intercept ########## 


get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))

cols = RColorBrewer::brewer.pal(5, "Set1")
lty = c(1, 1, 2, 2)
pch = c(15, 16:18)

cex_legend = 0.9
labels = c("Height ~ T + (1|mountain)", 
           "Height ~ T + (1|mountain) + (0 + T|mountain)", 
           "Height ~ 0 + T + mountain", 
           "Height ~ T ")


## Type I error ##
pdf(file = "Figures/Fig_S9.pdf", width = 9.2, height = 7.8)
si = c(0, 1)
si2 = c(0, 1)
par(mfrow = c(2,2), mar = c(2.1, 2.4, 1.5, 1), oma = c(4, 3, 3, 1)-1)
type_one_int = 
  cbind(
    sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_miss, function(l) mean(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si] < 0.05)),
    sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "", xpd = NA)
text(x=0.5, pos = 2, y = 0.55, labels = "a", cex = 1.2, xpd = NA, font = 2)
text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
legend("topright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_lmm, function(l) get_sd(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_miss, function(l)get_sd(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_wo_lm$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si2])),
    sapply(results_lmm, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si2]))
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
    sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity %in% si] < 0.05)), 
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_inter[l$results_w_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_inter[l$results_w_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm
  )
matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of levels")
text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")
text(x=0.5, pos = 2, y = 1.1, labels = "b", cex = 1.2, xpd = NA, font = 2)

legend("bottomright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_lmm, function(l) get_sd(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_miss, function(l) get_sd(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_inter[l$results_w_lme4_reml$Singularity %in% si2])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_inter[l$results_w_lme4_reml$Singularity %in% si2]))
  )
mm =1-type_two_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))


## glmm
cols = RColorBrewer::brewer.pal(4, "Set1")
cols = c(cols[-2])
lty = c(1, 1, 2)
type_one_int = 
  cbind(
    sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm, function(l) mean(l$results_wo_lm$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
text(x=0.5, pos = 2, y = 0.55, labels = "c", cex = 1.2, xpd = NA, font = 2)

axis(1, at = 1:7, labels = 2:8)
legend("topright", legend = c("RS ~ T + (1|mountain)", "RS ~ 0 + T + mountain", "RS ~ T "), col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)/sqrt( sum(!is.na(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si])) )), #lme4
    sapply(results_glmm, function(l) sd(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)/sqrt(sum(!is.na(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si])))), #lm
    sapply(results_glmm, function(l) sd(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)/sqrt(sum(!is.na(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si])))) #lm
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
    sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
  )

matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
axis(1, at = 1:7, labels = 2:8)
text(x=0.5, pos = 2, y = 1.1, labels = "d", cex = 1.2, xpd = NA, font = 2)
legend("bottomright", legend = c("RS ~ T + (1|mountain)", "RS ~ 0 + T + mountain", "RS ~ T "), col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si]  < 0.05)/sqrt( sum(!is.na( l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))), #lme4
    sapply(results_glmm, function(l) sd(l$results_w_glmmTMB_reml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)/sqrt( sum(!is.na( l$results_w_glmmTMB_reml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))), # glmmTMB
    sapply(results_glmm, function(l) sd(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))) #lm
  )
mm =1-type_two_int
upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))

dev.off()




########## __Figure S12 Random intercept different SD for random effect ########## 

cols = RColorBrewer::brewer.pal(4, "Set1")
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1, 2, 2)
cex_legend = 0.9
## Type I error ##
pdf(file = "Figures/Fig_S12.pdf", width = 9.2, height = 8.8)

sd_results = c("0.01", "0.1", "0.5", "2")
par(mfrow = c(4,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
labels = letters[1:4]
xlab = ""

si = c(0)
si2 = c(0, 1)

get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))
for(i in 1:4){
  
  results_lmm = readRDS(paste0("Results/results_mountain_lmm_random_intercept_only_", sd_results[i],"_unbalanced.Rds"))
  results_miss = readRDS(paste0("Results/results_mountain_lmm_random_intercept_only_miss_specified_no_cov_", sd_results[i],"_unbalanced.Rds"))
  
  if(i == 4) xlab = "Number of mountains"
  
  type_one_int = 
    cbind(
      sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si ]< 0.05)), #lme4
      sapply(results_miss, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si ]< 0.05)),
      sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2 ] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2 ] < 0.05, na.rm=TRUE)) #lm w/go goruping
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "Rate", xaxt="n", main = "", xlab = xlab, xpd = NA)
  text(x=-0.2, pos = 2, y = 0.55, labels = labels[i], cex = 1.2, xpd = NA, font = 2)
  if(i == 1) text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
  legend("topright", legend = c("Height ~ T + (1|mountain)", "Height ~ T + (1|mountain) + (0 + T|mountain)", "Height ~ 0 + T + mountain", "Height ~ T "), 
         col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  abline(h = 0.05, lty = 3, col = "darkgrey")
  sd = 
    cbind(
      sapply(results_lmm, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si ])), #lme4
      sapply(results_miss, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si ])),
      sapply(results_lmm, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2 ])), #lm
      sapply(results_lmm, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2 ] )) #lm w/go goruping
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
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2 ] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2 ] < 0.05, na.rm=TRUE)) #lm
    )
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd=NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")
  
  legend("bottomright", legend = c("Height ~ T + (1|mountain)", "Height ~ T + (1|mountain) + (0 + T|mountain)", "Height ~ 0 + T + mountain", "Height ~ T "), 
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
      sapply(results_lmm, function(l) mean(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si2 ] , na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si2 ] , na.rm=TRUE)) #lm
    )
  
  matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:19, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd=NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.02, xpd = NA, labels = "Coverage")
  
  legend("bottomright", legend = c("Height ~ T + (1|mountain)", "Height ~ T + (1|mountain) + (0 + T|mountain)", "Height ~ 0 + T + mountain", "Height ~ T "), 
         col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  abline(h = 0.95, lty = 3, col = "darkgrey")
  
  sd = 
    cbind(
      sapply(results_lmm, function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si ]  )), #lme4
      sapply(results_miss, function(l) get_sd(l$results_w_lme4_reml$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si ]  )), #lme4
      sapply(results_lmm, function(l) get_sd(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si2 ] )), #lm
      sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity %in% si2 ])) #lm
    )
  
  mm =type_two_int
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  if(i == 4)  axis(1, at = 1:7, labels = 2:8)
}


dev.off()







########## __Figure S14 Random intercept different number of observations for GLMM ########## 
files = c("Results/results_mountain_glmm_random_intercept_only_25_unbalanced.Rds", 
          "Results/results_mountain_glmm_random_intercept_only_50_unbalanced.Rds", 
          "Results/results_mountain_glmm_random_intercept_only_100_unbalanced.Rds", 
          "Results/results_mountain_glmm_random_intercept_only_200_unbalanced.Rds")

pdf("Figures/Fig_S14.pdf",  width = 9.2, height = 8.8)
par(mfrow = c(4,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
cols = RColorBrewer::brewer.pal(3, "Set1")
labels = letters[1:4]
cex_legend = 0.9
get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))
si = c(0)
si2 = c(0, 1)
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
      sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si2 ] < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) mean(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2 ] < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "Rate", xaxt="n", main = "", xlab = xlab, xpd = NA)
  text(x=-0.2, pos = 2, y = 0.55, labels = labels[i], cex = 1.2, xpd = NA, font = 2)
  if(i == 1) text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
  
  if(i == 4) axis(1, at = 1:7, labels = 2:8)
  legend("topright", legend = c("RS ~ T + (1|mountain)", "RS ~ 0 + T + mountain", "RS ~ T "), 
         col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  abline(h = 0.05, lty = 3, col = "darkgrey")
  sd = 
    cbind(
      sapply(results_glmm, function(l) get_sd(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si ] )), #lme4
      sapply(results_glmm, function(l) get_sd(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si2 ] )), #lm
      sapply(results_glmm, function(l) get_sd(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2 ] )) #lm
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
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2 ]  < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2 ]  < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd = NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")
  if(i == 4)  axis(1, at = 1:7, labels = 2:8)
  
  pos = "topright"
  if(i >2) pos = "bottomright"
  legend(pos, legend = c("RS ~ T + (1|mountain)", "RS ~ 0 + T + mountain", "RS ~ T "), 
         col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  sd = 
    cbind(
      sapply(results_glmm, function(l) get_sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si ])), #lme4
      sapply(results_glmm, function(l) get_sd(l$results_w_glmmTMB_reml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2 ])), # glmmTMB
      sapply(results_glmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2 ] )) #lm
    )
  mm =1-type_two_int
  upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
  
  ## Coverage ##
  type_two_int = 
    cbind(
      sapply(results_glmm, function(l) mean(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si ] )), #lme4
      sapply(results_glmm, function(l) mean(l$results_w_lm$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si2 ] , na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si2 ] , na.rm=TRUE)) #lm
    )
  
  
  matplot(type_two_int, type="o", ylim = c(0.5, 1.0), pch = 15:18, las = 1, lty = lty, col = cols, 
          ylab = "", xaxt="n", main = "", xlab = xlab, xpd = NA)
  if(i == 1) text(x= 4, pos = 3, y = 1.02, xpd = NA, labels = "Coverage")
  abline(h = 0.95, lty = 3, col = "darkgrey")
  
  if(i == 4)  axis(1, at = 1:7, labels = 2:8)
  legend("bottomright", legend = c("RS ~ T + (1|mountain)", "RS ~ 0 + T + mountain", "RS ~ T "), 
         col = cols, pch = 15:19, bty = "n", lty = lty, cex = cex_legend)
  sd = 
    cbind(
      sapply(results_glmm, function(l) get_sd(l$results_w_lme4_ml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si ] )), #lme4
      sapply(results_glmm, function(l) get_sd(l$results_w_glmmTMB_reml$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si2 ])), # glmmTMB
      sapply(results_glmm, function(l) get_sd(l$results_w_lm$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si2 ])) #lm
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

results_lmm_no_cov = readRDS("Results/results_mountain_lmm_no_cov_0.1_100_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_0.1_100_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_0.1_200_unbalanced.Rds")
results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
results_miss = readRDS("Results/results_mountain_lmm_miss_specified_0.1_100_unbalanced.Rds")

get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))


pdf(file = "Figures/Fig_2.pdf", width = 9.2, height = 7.8)


si = c(0)
si2 = c(0, 1)
cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1,1, 2, 2)
pch = c(15, 20, 16:18)
cex_legend = 0.9
## Type I error ##



par(mfrow = c(2,2), mar = c(2.1, 2.4, 1.5, 1), oma = c(4, 3, 3, 1)-1)
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
    sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "", xpd = NA)
text(x=0.5, pos = 2, y = 0.55, labels = "a", cex = 1.2, xpd = NA, font = 2)
text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
legend("topright", legend = labels,
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l)        get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_miss, function(l)       get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2])),
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2]))
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
    sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)) #lm
  )
matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of levels")
text(x=0.5, pos = 2, y = 1.1, labels = "b", cex = 1.2, xpd = NA, font = 2)

text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")

legend("bottomright", legend = labels,
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_lmm,        function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_miss,       function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2]))
  )
mm =1-type_two_int
upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))



## glmm
cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
cols = cols[-3]
lty = c(1, 1, 2, 2)
pch = c(15, 20, 17:18)
si = c(0)
labels = c("RS ~ T + (1|mountain) + (0 + T|mountain)", 
           "RS ~ T + (T|mountain)","RS ~ 0 + mountain + T : mountain", "RS ~ T ")
type_one_int = 
  cbind(
    sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm,        function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
text(x=0.5, pos = 2, y = 0.55, labels = "c", cex = 1.2, xpd = NA, font = 2)


axis(1, at = 1:7, labels = 2:8)
legend("topright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] )),
    sapply(results_glmm,        function(l) get_sd(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] )),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si2] )),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2] ))
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
    sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
  )

matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
text(x=0.5, pos = 2, y = 1.1, labels = "d", cex = 1.2, xpd = NA, font = 2)

axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si])),
    sapply(results_glmm,        function(l) get_sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si])),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2])),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2] ))
  )
mm =1-type_two_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))

dev.off()


########## __Figure S10 Random intercept + slope Type I, Error, etc. for lmm with SI ########## 

results_lmm_no_cov = readRDS("Results/results_mountain_lmm_no_cov_0.1_100_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_0.1_100_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_0.1_200_unbalanced.Rds")
results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
results_miss = readRDS("Results/results_mountain_lmm_miss_specified_0.1_100_unbalanced.Rds")

get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))


pdf(file = "Figures/Fig_S10.pdf", width = 9.2, height = 7.8)


si = c(0,1)
si2 = c(0, 1)
cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1,1, 2, 2)
pch = c(15, 20, 16:18)
cex_legend = 0.9
## Type I error ##



par(mfrow = c(2,2), mar = c(2.1, 2.4, 1.5, 1), oma = c(4, 3, 3, 1)-1)
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
    sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "", xpd = NA)
text(x=0.5, pos = 2, y = 0.55, labels = "a", cex = 1.2, xpd = NA, font = 2)
text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
legend("topright", legend = labels,
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l)        get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_miss, function(l)       get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2])),
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2]))
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
    sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)) #lm
  )
matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of levels")
text(x=0.5, pos = 2, y = 1.1, labels = "b", cex = 1.2, xpd = NA, font = 2)

text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")

legend("bottomright", legend = labels,
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_lmm,        function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_miss,       function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2]))
  )
mm =1-type_two_int
upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))



## glmm
cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
cols = cols[-3]
lty = c(1, 1, 2, 2)
pch = c(15, 20, 17:18)
labels = c("RS ~ T + (1|mountain) + (0 + T|mountain)", 
           "RS ~ T + (T|mountain)","RS ~ 0 + mountain + T : mountain", "RS ~ T ")
type_one_int = 
  cbind(
    sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm,        function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
text(x=0.5, pos = 2, y = 0.55, labels = "c", cex = 1.2, xpd = NA, font = 2)


axis(1, at = 1:7, labels = 2:8)
legend("topright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] )),
    sapply(results_glmm,        function(l) get_sd(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] )),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si2] )),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2] ))
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
    sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
  )

matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
text(x=0.5, pos = 2, y = 1.1, labels = "d", cex = 1.2, xpd = NA, font = 2)

axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si])),
    sapply(results_glmm,        function(l) get_sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si])),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2])),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2] ))
  )
mm =1-type_two_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))

dev.off()


########## __Figure S11 Random intercept + slope Type I, Error, etc. for intercept ########## 

results_lmm_no_cov = readRDS("Results/results_mountain_lmm_no_cov_0.1_100_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_0.1_100_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_0.1_200_unbalanced.Rds")
results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
results_miss = readRDS("Results/results_mountain_lmm_miss_specified_0.1_100_unbalanced.Rds")

get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))


pdf(file = "Figures/Fig_S11.pdf", width = 9.2, height = 7.8)


si = c(0)
si2 = c(0, 1)
cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1,1, 2, 2)
pch = c(15, 20, 16:18)
cex_legend = 0.9
## Type I error ##



par(mfrow = c(2,2), mar = c(2.1, 2.4, 1.5, 1), oma = c(4, 3, 3, 1)-1)
labels = c("Height ~ T + (1|mountain) + (0 + T|mountain)",
           "Height ~ T + (T|mountain)", 
           "Height ~ T + (1|mountain)", 
           "Height ~ 0 + mountain + T : mountain", 
           "Height ~ T ")
type_one_int = 
  cbind(
    sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si]< 0.05)), #lme4
    sapply(results_lmm, function(l)        mean(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity%in% si]< 0.05)), #lme4
    sapply(results_miss, function(l)       mean(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity%in% si]< 0.05)),
    sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm$p_value_inter[l$results_wo_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping$p_value_inter[l$results_wo_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "", xpd = NA)
text(x=0.5, pos = 2, y = 0.55, labels = "a", cex = 1.2, xpd = NA, font = 2)
text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Type I error")
legend("topright", legend = labels,
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l)        get_sd(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_miss, function(l)       get_sd(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si])),
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si2])),
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si2]))
  )
mm = type_one_int
upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))

## Power ##
type_two_int = 
  cbind(
    sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), #lme4
    sapply(results_lmm, function(l)        1-mean(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), #lme4
    sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), 
    sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_inter[l$results_w_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_inter[l$results_w_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)) #lm
  )
matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of levels")
text(x=0.5, pos = 2, y = 1.1, labels = "b", cex = 1.2, xpd = NA, font = 2)

text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "Power")

legend("bottomright", legend = labels,
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_lmm,        function(l) get_sd(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_miss,       function(l) get_sd(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity %in% si])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_inter[l$results_w_lme4_reml$Singularity %in% si2])),
    sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_inter[l$results_w_lme4_reml$Singularity %in% si2]))
  )
mm =1-type_two_int
upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))



## glmm
cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
cols = cols[-3]
lty = c(1, 1, 2, 2)
pch = c(15, 20, 17:18)
labels = c("RS ~ T + (1|mountain) + (0 + T|mountain)", 
           "RS ~ T + (T|mountain)","RS ~ 0 + mountain + T : mountain", "RS ~ T ")
type_one_int = 
  cbind(
    sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lme4_ml$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm,        function(l) mean(l$results_wo_lme4_ml$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lm$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm
  )

matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "Rate", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
text(x=0.5, pos = 2, y = 0.55, labels = "c", cex = 1.2, xpd = NA, font = 2)


axis(1, at = 1:7, labels = 2:8)
legend("topright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
abline(h = 0.05, lty = 3, col = "darkgrey")
sd = 
  cbind(
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_wo_lme4_ml$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si] )),
    sapply(results_glmm,        function(l) get_sd(l$results_wo_lme4_ml$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si] )),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_wo_lm$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si2] )),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si2] ))
  )
mm =type_one_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
#text(-0.5, 0.55, labels = "B", cex = 1.3, font = 2, xpd =NA)


## Power ##
type_two_int = 
  cbind(
    sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lme4_ml$p_value_inter[l$results_w_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm,        function(l) 1-mean(l$results_w_lme4_ml$p_value_inter[l$results_w_lme4_ml$Singularity %in% si] < 0.05)), #lme4
    sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_inter[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
    sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_inter[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
  )

matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
        ylab = "", xaxt="n", main = "", xlab = "Number of mountains", xpd = NA)
text(x=0.5, pos = 2, y = 1.1, labels = "d", cex = 1.2, xpd = NA, font = 2)

axis(1, at = 1:7, labels = 2:8)
legend("bottomright", legend = labels, 
       col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
sd = 
  cbind(
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lme4_ml$p_value_inter[l$results_w_lme4_ml$Singularity %in% si])),
    sapply(results_glmm,        function(l) get_sd(l$results_w_lme4_ml$p_value_inter[l$results_w_lme4_ml$Singularity %in% si])),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lm$p_value_inter[l$results_w_lme4_ml$Singularity %in% si2])),
    sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2] ))
  )
mm =1-type_two_int
upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))

dev.off()



########## __Figure S13 Random intercept+slope different SD for random effect ########## 

cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1,1, 2, 2)
pch = c(15, 20, 16:18)
## Type I error ##
pdf(file = "Figures/Fig_S13.pdf", width = 9.2, height = 8.8)
si = c(0)
si2 = c(0, 1)
sds = c(0.01, 0.1, 0.5, 2)
par(mfrow = c(4,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
labels = letters[1:4]
xlab = ""
legend_label = c("Height ~ T + (1|mountain) + (0 + T|mountain)",
                 "Height ~ T + (T|mountain)", 
                 "Height ~ T + (1|mountain)", 
                 "Height ~ 0 + mountain + T : mountain", 
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
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
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
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2])),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2]))
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
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)) #lm
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
      sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2]))
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
      sapply(results_lmm_no_cov, function(l) mean(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si2] , na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si2] , na.rm=TRUE)) #lm
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
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si2])),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si2] ))
    )
  
  mm =type_two_int
  upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
  
}
dev.off()


########## __Figure S15 Random intercept+slope different observations for random effect ########## 

cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
#cols = c(cols[1], cols, cols[3])
lty = c(1, 1,1, 2, 2)
pch = c(15, 20, 16:18)
si2 = c(0, 1)
## Type I error ##
pdf(file = "Figures/Fig_S15.pdf", width = 9.2, height = 8.8)

sds = c(0.01, 0.1, 0.5, 2)
par(mfrow = c(4,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
labels = letters[1:4]
xlab = ""
legend_label = c("Height ~ T + (1|mountain) + (0 + T|mountain)",
                 "Height ~ T + (T|mountain)", 
                 "Height ~ T + (1|mountain)", 
                 "Height ~ 0 + mountain + T : mountain", 
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
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
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
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2])),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2]))
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
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)) #lm
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
      sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2]))
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
      sapply(results_lmm_no_cov, function(l) mean(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si2] , na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si2] , na.rm=TRUE)) #lm
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
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lm$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si2])),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_reml$Singularity%in% si2] ))
    )
  
  mm =type_two_int
  upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
  
}
dev.off()






########## __Figure S16 Random intercept different number of observations for GLMM ########## 
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


pdf("Figures/Fig_S16.pdf", width = 8.2, height = 8.8)
par(mfrow = c(5,3), mar = c(0.1, 2.4, 1, 1), oma = c(5, 3, 3, 1)-1)
cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
cols = cols[-3]
lty = c(1, 1, 2, 2)
pch = c(15, 20, 17:18)
si2 = c(0, 1)
labels = letters[1:5]
legend = c("RS ~ T + (1|mountain) + (0 + T|mountain)", "RS ~ T + (T|mountain)","RS ~ 0 + mountain + T : mountain", "RS ~ T ")

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
      sapply(results_glmm_no_cov, function(l) mean(change_to_z(l$results_wo_lm)[[2]][l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm
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
      sapply(results_glmm_no_cov, function(l) get_sd(change_to_z(l$results_wo_lm)[[2]][l$results_wo_lme4_ml$Singularity %in% si2] )),
      sapply(results_glmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2] ))
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
      sapply(results_glmm_no_cov, function(l) 1-mean(change_to_z(l$results_w_lm)$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
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
      sapply(results_glmm_no_cov, function(l) get_sd(change_to_z(l$results_w_lm)$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2])),
      sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2] ))
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
      sapply(results_glmm_no_cov, function(l) mean(change_to_z(l$results_w_lm)$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si2]  , na.rm=TRUE)), #lm
      sapply(results_glmm_no_cov, function(l) mean(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si2]  , na.rm=TRUE)) #lm
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
      sapply(results_glmm_no_cov, function(l) get_sd(change_to_z(l$results_w_lm)$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si2] )),
      sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lm_wo_grouping$Slope_in_conf[l$results_w_lme4_ml$Singularity %in% si2]))
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
si2 = c(0, 1)
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
text(x=-0.2, pos = 2, y = 0.55, labels = "a", cex = 1.2, xpd = NA, font = 2)
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
text(x=-0.2, pos = 2, y = 0.55, labels = "b", cex = 1.2, xpd = NA, font = 2)

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

pdf(file = "Figures/Fig_3.pdf", width = 9, height = 9)

par(mfrow = c(2,2), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
plot_mean = function(x, w = 1) {
  mean = mean(x, na.rm=TRUE)
  segments(w-0.4, x1 = w+0.4, y0 = mean, y1 = mean, col = "red", lwd = 1.7)
  
}
lty = 2
lwd = 1.4
bb= boxplot(results_lmm[[1]]$results_w_lme4_reml$stddev_randeff_inter**2, 
            results_lmm[[2]]$results_w_lme4_reml$stddev_randeff_inter**2, 
            results_lmm[[3]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[4]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[5]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[6]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[7]]$results_w_lme4_reml$stddev_randeff_inter**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)

legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.084, pos = 3, labels = letters[1], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_lme4_reml$stddev_randeff_inter**2, i))

bb= boxplot(results_lmm[[1]]$results_w_lme4_reml$stddev_randeff_x**2, 
            results_lmm[[2]]$results_w_lme4_reml$stddev_randeff_x**2, 
            results_lmm[[3]]$results_w_lme4_reml$stddev_randeff_x**2, 
            results_lmm[[4]]$results_w_lme4_reml$stddev_randeff_x**2, 
            results_lmm[[5]]$results_w_lme4_reml$stddev_randeff_x**2, 
            results_lmm[[6]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm[[7]]$results_w_lme4_reml$stddev_randeff_x**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)
legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.084, pos = 3, labels = letters[2], xpd= NA, cex = 1.3, font = 2)

abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_lme4_reml$stddev_randeff_x**2, i))

### Without SI
bb= boxplot(results_lmm[[1]]$results_w_lme4_reml$stddev_randeff_inter[results_lmm[[1]]$results_w_lme4_reml$Singularity == 0]**2, 
            results_lmm[[2]]$results_w_lme4_reml$stddev_randeff_inter[results_lmm[[2]]$results_w_lme4_reml$Singularity == 0]**2, 
            results_lmm[[3]]$results_w_lme4_reml$stddev_randeff_inter[results_lmm[[2]]$results_w_lme4_reml$Singularity == 0]**2, 
            results_lmm[[4]]$results_w_lme4_reml$stddev_randeff_inter[results_lmm[[2]]$results_w_lme4_reml$Singularity == 0]**2, 
            results_lmm[[5]]$results_w_lme4_reml$stddev_randeff_inter[results_lmm[[2]]$results_w_lme4_reml$Singularity == 0]**2, 
            results_lmm[[6]]$results_w_lme4_reml$stddev_randeff_inter[results_lmm[[4]]$results_w_lme4_reml$Singularity == 0]**2,
            results_lmm[[7]]$results_w_lme4_reml$stddev_randeff_inter[results_lmm[[7]]$results_w_lme4_reml$Singularity == 0]**2, ylim = c(0, 0.13), outline=FALSE, las = 1,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)
legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.1365, pos = 3, labels = letters[3], xpd= NA, cex = 1.3, font = 2)

abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i ]]$results_w_lme4_reml$stddev_randeff_inter[results_lmm[[i]]$results_w_lme4_reml$Singularity == 0]**2, i))

bb= boxplot(results_lmm[[1]]$results_w_lme4_reml$stddev_randeff_x[results_lmm[[1]]$results_w_lme4_reml$Singularity == 0]**2, 
            results_lmm[[2]]$results_w_lme4_reml$stddev_randeff_x[results_lmm[[2]]$results_w_lme4_reml$Singularity == 0]**2, 
            results_lmm[[3]]$results_w_lme4_reml$stddev_randeff_x[results_lmm[[2]]$results_w_lme4_reml$Singularity == 0]**2, 
            results_lmm[[4]]$results_w_lme4_reml$stddev_randeff_x[results_lmm[[2]]$results_w_lme4_reml$Singularity == 0]**2, 
            results_lmm[[5]]$results_w_lme4_reml$stddev_randeff_x[results_lmm[[2]]$results_w_lme4_reml$Singularity == 0]**2, 
            results_lmm[[6]]$results_w_lme4_reml$stddev_randeff_x[results_lmm[[4]]$results_w_lme4_reml$Singularity == 0]**2,
            results_lmm[[7]]$results_w_lme4_reml$stddev_randeff_x[results_lmm[[7]]$results_w_lme4_reml$Singularity == 0]**2, ylim = c(0, 0.13), outline=FALSE,las = 1,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)
legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.1365, pos = 3, labels = letters[4], xpd= NA, cex = 1.3, font = 2)

abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i ]]$results_w_lme4_reml$stddev_randeff_x[results_lmm[[i]]$results_w_lme4_reml$Singularity == 0]**2, i))

dev.off()

########## __Figure S1 Variance estimates glmmTMB ########## 

cols = viridis::viridis(5)
adj = 1.0

results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
pdf(file = "Figures/Fig_S1.pdf", width = 9, height = 9)
par(mfrow = c(2,2), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
lty = 2
lwd = 1.4
bb= boxplot(results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, 
            results_lmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, 
            results_lmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)

legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.084, pos = 3, labels = letters[1], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, i))

bb= boxplot(results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_lmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_lmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_lmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_lmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_lmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_x**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)
legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.084, pos = 3, labels = letters[2], xpd= NA, cex = 1.3, font = 2)

abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_lme4_reml$stddev_randeff_x**2, i))

bb= boxplot(results_glmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, 
            results_glmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, 
            results_glmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, 
            results_glmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, 
            results_glmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, 
            results_glmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_glmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, ylim = c(0, 0.13), outline=FALSE, las = 1,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)
legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.1365, pos = 3, labels = letters[3], xpd= NA, cex = 1.3, font = 2)

abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_glmm[[ i ]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, i))

bb= boxplot(results_glmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_glmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_glmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_glmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_glmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_glmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_glmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_x**2, ylim = c(0, 0.13), outline=FALSE,las = 1,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)
legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.1365, pos = 3, labels = letters[4], xpd= NA, cex = 1.3, font = 2)

abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_glmm[[ i ]]$results_w_glmmTMB_reml$stddev_randeff_x**2, i))


dev.off()


########## __Figure S2 Variance estimates glmmTMB ########## 

pdf(file = "Figures/Fig_S2.pdf", width = 9, height = 9)
par(mfrow = c(2,2), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
lty = 2
lwd = 1.4
bb= boxplot(results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, 
            results_lmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, 
            results_lmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)

legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.084, pos = 3, labels = letters[1], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, i))

bb= boxplot(results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_lmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_lmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_lmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_lmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_lmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_x**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)
legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.084, pos = 3, labels = letters[2], xpd= NA, cex = 1.3, font = 2)

abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_lme4_reml$stddev_randeff_x**2, i))
threshold = 1e-3
bb= boxplot(results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_inter[results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_inter > threshold]**2, 
            results_lmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_inter[results_lmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_inter > threshold]**2, 
            results_lmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_inter[results_lmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_inter > threshold]**2, 
            results_lmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_inter[results_lmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_inter > threshold]**2, 
            results_lmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_inter[results_lmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_inter > threshold]**2, 
            results_lmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_inter[results_lmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_inter > threshold]**2,
            results_lmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_inter[results_lmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_inter > threshold]**2, ylim = c(0, 0.13), outline=FALSE, las = 1,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)
legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.1365, pos = 3, labels = letters[3], xpd= NA, cex = 1.3, font = 2)

abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i ]]$results_w_glmmTMB_reml$stddev_randeff_inter[results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_inter > threshold]**2, i))

bb= boxplot(results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_x[results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_x > threshold]**2, 
            results_lmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_x[results_lmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_x > threshold]**2, 
            results_lmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_x[results_lmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_x > threshold]**2, 
            results_lmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_x[results_lmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_x > threshold]**2, 
            results_lmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_x[results_lmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_x > threshold]**2, 
            results_lmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_x[results_lmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_x > threshold]**2,
            results_lmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_x[results_lmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_x > threshold]**2, ylim = c(0, 0.13), outline=FALSE, las = 1,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)
legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.1365, pos = 3, labels = letters[4], xpd= NA, cex = 1.3, font = 2)

abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i ]]$results_w_glmmTMB_reml$stddev_randeff_x[results_lmm[[i]]$results_w_glmmTMB_reml$stddev_randeff_x > threshold]**2, i))


dev.off()




########## __Figure S3 Variance estimates lmm MLE/REML ########## 



pdf(file = "Figures/Fig_S3.pdf", width = 9, height = 9)
par(mfrow = c(2,2), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
lty = 2
lwd = 1.4
at = as.vector(t(matrix(1:21, ncol = 3L, byrow = TRUE)[,-3]))
bb= boxplot(results_lmm[[1]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[1]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm[[2]]$results_w_lme4_reml$stddev_randeff_inter**2, 
            results_lmm[[2]]$results_w_lme4_ml$stddev_randeff_inter**2, 
            results_lmm[[3]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[3]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm[[4]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[4]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm[[5]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[5]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm[[6]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[6]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm[[7]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[7]]$results_w_lme4_ml$stddev_randeff_inter**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_lme4_reml$stddev_randeff_inter**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_lme4_ml$stddev_randeff_inter**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "REML", "MLE"), 
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[1], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)


bb= boxplot(results_lmm[[1]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm[[1]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm[[2]]$results_w_lme4_reml$stddev_randeff_x**2, 
            results_lmm[[2]]$results_w_lme4_ml$stddev_randeff_x**2, 
            results_lmm[[3]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm[[3]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm[[4]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm[[4]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm[[5]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm[[5]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm[[6]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm[[6]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm[[7]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm[[7]]$results_w_lme4_ml$stddev_randeff_x**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_lme4_reml$stddev_randeff_x**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_lme4_ml$stddev_randeff_x**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "REML", "MLE"), 
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[2], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)


bb= boxplot(results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[1]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, 
            results_lmm[[2]]$results_w_glmmTMB_ml$stddev_randeff_inter**2, 
            results_lmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[3]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[4]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[5]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[6]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[7]]$results_w_glmmTMB_ml$stddev_randeff_inter**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_glmmTMB_ml$stddev_randeff_inter**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "REML", "MLE"), 
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[3], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)

bb= boxplot(results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm[[1]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            
            results_lmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_lmm[[2]]$results_w_glmmTMB_ml$stddev_randeff_x**2, 
            
            results_lmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm[[3]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            
            results_lmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm[[4]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            
            results_lmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm[[5]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            
            results_lmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm[[6]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            
            results_lmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm[[7]]$results_w_glmmTMB_ml$stddev_randeff_x**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_glmmTMB_reml$stddev_randeff_x**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_glmmTMB_ml$stddev_randeff_x**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "REML", "MLE"), 
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[4], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)

dev.off()








########## __Figure S4 Variance estimates glmm MLE/REML ########## 
cols = viridis::viridis(5)

results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")


pdf(file = "Figures/Fig_S4.pdf", width = 9, height = 9)
par(mfrow = c(2,2), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
lty = 2
lwd = 1.4
at = as.vector(t(matrix(1:21, ncol = 3L, byrow = TRUE)[,-3]))
bb= boxplot(results_glmm[[1]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_glmm[[1]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_glmm[[2]]$results_w_lme4_reml$stddev_randeff_inter**2, 
            results_glmm[[2]]$results_w_lme4_ml$stddev_randeff_inter**2, 
            results_glmm[[3]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_glmm[[3]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_glmm[[4]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_glmm[[4]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_glmm[[5]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_glmm[[5]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_glmm[[6]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_glmm[[6]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_glmm[[7]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_glmm[[7]]$results_w_lme4_ml$stddev_randeff_inter**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_glmm[[ i]]$results_w_lme4_reml$stddev_randeff_inter**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_glmm[[ i]]$results_w_lme4_ml$stddev_randeff_inter**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "REML", "MLE"), 
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[1], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)


bb= boxplot(results_glmm[[1]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_glmm[[1]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_glmm[[2]]$results_w_lme4_reml$stddev_randeff_x**2, 
            results_glmm[[2]]$results_w_lme4_ml$stddev_randeff_x**2, 
            results_glmm[[3]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_glmm[[3]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_glmm[[4]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_glmm[[4]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_glmm[[5]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_glmm[[5]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_glmm[[6]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_glmm[[6]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_glmm[[7]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_glmm[[7]]$results_w_lme4_ml$stddev_randeff_x**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_glmm[[ i]]$results_w_lme4_reml$stddev_randeff_x**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_glmm[[ i]]$results_w_lme4_ml$stddev_randeff_x**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "REML", "MLE"), 
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[2], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)


bb= boxplot(results_glmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_glmm[[1]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_glmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, 
            results_glmm[[2]]$results_w_glmmTMB_ml$stddev_randeff_inter**2, 
            results_glmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_glmm[[3]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_glmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_glmm[[4]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_glmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_glmm[[5]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_glmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_glmm[[6]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_glmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_glmm[[7]]$results_w_glmmTMB_ml$stddev_randeff_inter**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_glmm[[ i]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_glmm[[ i]]$results_w_glmmTMB_ml$stddev_randeff_inter**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "REML", "MLE"), 
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[3], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)

bb= boxplot(results_glmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_glmm[[1]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_glmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_glmm[[2]]$results_w_glmmTMB_ml$stddev_randeff_x**2, 
            results_glmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_glmm[[3]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_glmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_glmm[[4]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_glmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_glmm[[5]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_glmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_glmm[[6]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_glmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_glmm[[7]]$results_w_glmmTMB_ml$stddev_randeff_x**2, ylim = c(0, 0.12), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.0105, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_glmm[[ i]]$results_w_glmmTMB_reml$stddev_randeff_x**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_glmm[[ i]]$results_w_glmmTMB_ml$stddev_randeff_x**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "REML", "MLE"), 
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.126, pos = 3, labels = letters[4], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)

dev.off()





########## __Figure S5 Variance estimates lmm REML imbalanced vs REML balanced ########## 
cols = viridis::viridis(5)

results_lmm_im = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_.Rds")


pdf(file = "Figures/Fig_S5.pdf", width = 9, height = 9)
par(mfrow = c(2,2), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
lty = 2
lwd = 1.4
at = as.vector(t(matrix(1:21, ncol = 3L, byrow = TRUE)[,-3]))
bb= boxplot(results_lmm[[1]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm_im[[1]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[2]]$results_w_lme4_reml$stddev_randeff_inter**2, 
            results_lmm_im[[2]]$results_w_lme4_reml$stddev_randeff_inter**2, 
            results_lmm[[3]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm_im[[3]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[4]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm_im[[4]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[5]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm_im[[5]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[6]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm_im[[6]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm[[7]]$results_w_lme4_reml$stddev_randeff_inter**2,
            results_lmm_im[[7]]$results_w_lme4_reml$stddev_randeff_inter**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_lme4_reml$stddev_randeff_inter**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_lmm_im[[ i]]$results_w_lme4_reml$stddev_randeff_inter**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "balanced", "unbalanced"),
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[1], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)


bb= boxplot(results_lmm[[1]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm_im[[1]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm[[2]]$results_w_lme4_reml$stddev_randeff_x**2, 
            results_lmm_im[[2]]$results_w_lme4_reml$stddev_randeff_x**2, 
            results_lmm[[3]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm_im[[3]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm[[4]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm_im[[4]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm[[5]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm_im[[5]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm[[6]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm_im[[6]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm[[7]]$results_w_lme4_reml$stddev_randeff_x**2,
            results_lmm_im[[7]]$results_w_lme4_reml$stddev_randeff_x**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_lme4_reml$stddev_randeff_x**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_lmm_im[[ i]]$results_w_lme4_reml$stddev_randeff_x**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "balanced", "unbalanced"),
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[2], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)


bb= boxplot(results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm_im[[1]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, 
            results_lmm_im[[2]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, 
            results_lmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm_im[[3]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm_im[[4]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm_im[[5]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm_im[[6]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_inter**2,
            results_lmm_im[[7]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_lmm_im[[ i]]$results_w_glmmTMB_reml$stddev_randeff_inter**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "balanced", "unbalanced"),
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[3], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)

bb= boxplot(results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm_im[[1]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_lmm_im[[2]]$results_w_glmmTMB_reml$stddev_randeff_x**2, 
            results_lmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm_im[[3]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm_im[[4]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm_im[[5]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm_im[[6]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_x**2,
            results_lmm_im[[7]]$results_w_glmmTMB_reml$stddev_randeff_x**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_glmmTMB_reml$stddev_randeff_x**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_lmm_im[[ i]]$results_w_glmmTMB_reml$stddev_randeff_x**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "balanced", "unbalanced"), 
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[4], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)

dev.off()




########## __Figure S6 Variance estimates lmm MLE imbalanced vs MLE balanced ########## 
cols = viridis::viridis(5)

results_lmm_im = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_.Rds")

pdf(file = "Figures/Fig_S6.pdf", width = 9, height = 9)
par(mfrow = c(2,2), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
lty = 2
lwd = 1.4
at = as.vector(t(matrix(1:21, ncol = 3L, byrow = TRUE)[,-3]))
bb= boxplot(results_lmm[[1]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm_im[[1]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm[[2]]$results_w_lme4_ml$stddev_randeff_inter**2, 
            results_lmm_im[[2]]$results_w_lme4_ml$stddev_randeff_inter**2, 
            results_lmm[[3]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm_im[[3]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm[[4]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm_im[[4]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm[[5]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm_im[[5]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm[[6]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm_im[[6]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm[[7]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_lmm_im[[7]]$results_w_lme4_ml$stddev_randeff_inter**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_lme4_ml$stddev_randeff_inter**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_lmm_im[[ i]]$results_w_lme4_ml$stddev_randeff_inter**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "balanced", "unbalanced"),
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[1], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)


bb= boxplot(results_lmm[[1]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm_im[[1]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm[[2]]$results_w_lme4_ml$stddev_randeff_x**2, 
            results_lmm_im[[2]]$results_w_lme4_ml$stddev_randeff_x**2, 
            results_lmm[[3]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm_im[[3]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm[[4]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm_im[[4]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm[[5]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm_im[[5]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm[[6]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm_im[[6]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm[[7]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_lmm_im[[7]]$results_w_lme4_ml$stddev_randeff_x**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_lme4_ml$stddev_randeff_x**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_lmm_im[[ i]]$results_w_lme4_ml$stddev_randeff_x**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "balanced", "unbalanced"),
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[2], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)


bb= boxplot(results_lmm[[1]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm_im[[1]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm[[2]]$results_w_glmmTMB_ml$stddev_randeff_inter**2, 
            results_lmm_im[[2]]$results_w_glmmTMB_ml$stddev_randeff_inter**2, 
            results_lmm[[3]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm_im[[3]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm[[4]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm_im[[4]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm[[5]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm_im[[5]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm[[6]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm_im[[6]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm[[7]]$results_w_glmmTMB_ml$stddev_randeff_inter**2,
            results_lmm_im[[7]]$results_w_glmmTMB_ml$stddev_randeff_inter**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_glmmTMB_ml$stddev_randeff_inter**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_lmm_im[[ i]]$results_w_glmmTMB_ml$stddev_randeff_inter**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "balanced", "unbalanced"),
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[3], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)

bb= boxplot(results_lmm[[1]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_lmm_im[[1]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_lmm[[2]]$results_w_glmmTMB_ml$stddev_randeff_x**2, 
            results_lmm_im[[2]]$results_w_glmmTMB_ml$stddev_randeff_x**2, 
            results_lmm[[3]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_lmm_im[[3]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_lmm[[4]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_lmm_im[[4]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_lmm[[5]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_lmm_im[[5]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_lmm[[6]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_lmm_im[[6]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_lmm[[7]]$results_w_glmmTMB_ml$stddev_randeff_x**2,
            results_lmm_im[[7]]$results_w_glmmTMB_ml$stddev_randeff_x**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            xaxt = "n", xlab = "Number of mountains",
            at = at,
            col = c("grey", "white"), ylab = "Variance", xpd = NA)
axis(side = 1,at = at[seq(1, 13,length.out=7 )]+0.5, labels = rep("", 7))
text(x = at[seq(1, 13,length.out=7 )]+0.5, y = -0.007, pos = 1, labels = 2:8, xpd = NA)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_glmmTMB_ml$stddev_randeff_x**2, at[seq(1, 13, length.out=7)][i]))
sapply(1:7, function(i) plot_mean(results_lmm_im[[ i]]$results_w_glmmTMB_ml$stddev_randeff_x**2, at[seq(2, 14, length.out=7)][i]))
legend("topright", bty = "n", 
       legend = c("mean", "balanced", "unbalanced"), 
       col = c("red", "black", "black"), 
       pt.bg=c("white", "grey"),
       lty = c(1, NA, NA),
       pch = c(NA, 22, 22))
text(x = -0.1, y = 0.084, pos = 3, labels = letters[4], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)

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
text(xpd=NA, x = -0.0, y = 0.33, labels = "a", cex = 1.1, font = 2)
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
text(xpd=NA, x = -0.0, y = 1.1, labels = "b", cex = 1.1, font = 2)

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

text(xpd=NA, x = -0.0, y = 0.33, labels = "c", cex = 1.1, font = 2)
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
text(xpd=NA, x = -0.0, y = 1.1, labels = "d", cex = 1.1, font = 2)

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
test$balanced2 = (test$max - test$min)/test$max

library(qgam)
library(mgcv)
library(mgcViz)
library(ggplot2)
library(tidymv)
library(purrr)
sub = test
sub$total = sub$moutain*sub$nobs
typeI_LM = qgam(TypeOneLM_GM~s(var)+s(moutain)+s(total)+s(balanced2),
                data = sub, qu = 0.5, control = list(link = "identity"))

typeI_LMM = qgam(TypeOneLMM~s(var)+s(moutain)+s(total)+s(balanced2),
                   data = sub, qu = 0.5, control = list(link = "identity"))
power_LM = qgam(PowerLM_GM~s(var)+s(moutain)+s(total)+s(balanced2),
                data = sub, qu = 0.5, control = list(link = "identity"))
power_LMM = qgam(PowerLMM~s(var)+s(moutain)+s(total)+s(balanced2),
                 data = sub, qu = 0.5, control = list(link = "identity"))
mods = list(typeI_LM, typeI_LMM)
mods_power = list(power_LM, power_LMM)
names(mods) =  c("Fixed Effect", "Random Effect")
names(mods_power) = c("Fixed Effect", "Random Effect")
viz = getViz(mods)
viz_power = getViz(mods_power)
get_ses = function(model){
  se = list()
  viz = getViz(model)
  for(i in 1:4){
    fit = plot(sm(viz,i))
    se[[i]] = fit$data$fit$se
  }
  return(se)
}

plot(sm(getViz(power_LM),1))
se_typeI_lm = get_ses(typeI_LM)
se_typeI_lmm = get_ses(typeI_LMM)
se_power_lm = get_ses(power_LM)
se_power_lmm = get_ses(power_LMM)
se_power_lm[[1]]
typeI_var = plot(sm(viz,1))
power_var = plot(sm(viz_power,1))
typeI_mountain = plot(sm(viz,2))
power_mountain = plot(sm(viz_power,2))
typeI_total = plot(sm(viz,3))
power_total = plot(sm(viz_power,3))
typeI_balance = plot(sm(viz,4))
power_balance = plot(sm(viz_power,4))
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  ## Save the color
  invisible(t.col)
}

add_label = function(x=-0.5, y=0.013, i=1) {
  text(x = x, y = y, xpd = NA, labels = letters[i],font =2, cex = 1.4)
}
build_plot = function(ggplot_obj, ylim, name, xlab = "", se_fixed, se_random){
  x_values_fixed = ggplot_obj$data$fit$x[ggplot_obj$data$fit$id == 'Fixed Effect' ]
  y_values_fixed = ggplot_obj$data$fit$y[ggplot_obj$data$fit$id == 'Fixed Effect' ]
  x_values_random = ggplot_obj$data$fit$x[ggplot_obj$data$fit$id == 'Random Effect' ]
  y_values_random = ggplot_obj$data$fit$y[ggplot_obj$data$fit$id == 'Random Effect' ]
  plot(x_values_fixed,
       y_values_fixed,
       type = "l", ylim = ylim, col = "red", ylab = paste("spline",name) ,
       xlab = xlab, lwd = 2, cex.lab = 1.18, las = 1)
  polygon(c(rev(x_values_fixed), x_values_fixed),
          c(rev(y_values_fixed -2*se_fixed), y_values_fixed+ 2*se_fixed)
          , col = t_col('red',80), border = NA)
  lines(ggplot_obj$data$fit$x[ggplot_obj$data$fit$id == 'Random Effect' ],
        ggplot_obj$data$fit$y[ggplot_obj$data$fit$id == 'Random Effect' ],
        type = "l", ylim = ylim, col = "blue", lwd =2)
  polygon(c(rev(x_values_random), x_values_random),
          c(rev(y_values_random -2*se_random), y_values_random+ 2*se_random)
          , col = t_col('blue',80), border = NA)
}
pdf("Figures/Fig_5.pdf", width = 12, height = 6.2)
par(mfrow = c(2,4))
par(mar = c(4.2,4,0.1,1.2), oma = c(2, 2, 3, 2))
ylim = c(-0.03,0.01)

build_plot(typeI_var,ylim = ylim, "variance", se_fixed = se_typeI_lm[[1]], se_random =se_typeI_lmm[[1]])
add_label(i = 1)
build_plot(typeI_mountain,ylim = ylim, "mountain", se_fixed = se_typeI_lm[[2]], se_random =se_typeI_lmm[[2]])
add_label(x = -2.5,i = 2)
build_plot(typeI_total,ylim = ylim, "observations", se_fixed = se_typeI_lm[[3]], se_random =se_typeI_lmm[[3]])
add_label(x=-1250,i = 3)
build_plot(typeI_balance,ylim = ylim, "balance", se_fixed = se_typeI_lm[[4]], se_random =se_typeI_lmm[[4]])
add_label(x=-0.125,i = 4)

#par(mar = c(6,4,rep(1.2,2)))
build_plot(power_var,ylim = c(-0.1,0.8), "variance", xlab = "variance", se_fixed = se_power_lm[[1]], se_random =se_power_lmm[[1]])
add_label(i = 5, y = 0.87)
build_plot(power_mountain,ylim = c(-0.1,0.8), "mountain", xlab = "number of mountains", se_fixed = se_power_lm[[2]], se_random =se_power_lmm[[2]])
add_label(x = -2.5,i = 6, y = 0.87)

build_plot(power_total,ylim = c(-0.1,0.8), "observations", xlab = "total number of observations", se_fixed = se_power_lm[[3]], se_random =se_power_lmm[[3]])
add_label(x=-1250,i = 7, y = 0.87)

build_plot(power_balance,ylim = c(-0.1,0.8), "balance", xlab = "balance between groups", se_fixed = se_power_lm[[4]], se_random =se_power_lmm[[4]])
add_label(x=-0.125,i = 8, y = 0.87)

legend( "topright",legend=c("Fixed Effect", "Random Effect"),
        col=c("red", "blue"), lty=1, cex=1.3, bty = "n")

dev.off()







########## _________________________________  ##########
########## Grand mean simulations  ##########
########## _________________________________  ##########



########## __Figure S18 Grand mean simulation ########## 
# t+ non-centered

mountains = c(1, 2, 5, 10, 50, 100)
sd_re = c(0.1, 0.5, 1.0)
unbalanced = c(TRUE, FALSE)
t = c(TRUE, FALSE)
center = c(TRUE, FALSE)
parameter = expand.grid(mountains, sd_re, unbalanced, t, center)
colnames(parameter) = c("mountains", "sd_re","unbalanced","t","center")

results = readRDS("Results/boot.RDS")
pdf("Figures/Fig_S18.pdf", height = 10.5, width = 10)

par(mfrow = c(6, 6), mar = c(1, 2, 1, 1), oma = c(5, 4, 2, 1))
mounts = c(1, 2, 5, 10, 50, 100) +1
ylab = c("0.1", "0.5", "1.0")
counter = 1
for(sd in c(0.1, 0.5, 1.0)) {
  vars = as.integer(rownames(parameter[parameter$sd_re==sd & parameter$t & !parameter$center & parameter$unbalanced,]))
  yy = paste0("SD: ", ylab[counter])
  counter = counter + 1
  for(i in 1:6) {
    if(i > 1) yy = ""
    if(sd=="0.1") hist((results[[vars[i]]][[2]][,2]), main = paste0(mounts[i], " mountains"), xlab = "", col = "blue", ylab = yy, xpd = NA)
    else hist((results[[vars[i]]][[2]][,2]), main = "", xlab = "", col = "blue", ylab = yy, xpd = NA)
  }
}
counter = 1

for(sd in c(0.1, 0.5, 1.0)) {
  vars = as.integer(rownames(parameter[parameter$sd_re==sd & parameter$t & !parameter$center & !parameter$unbalanced,]))
  yy = paste0("SD: ", ylab[counter])
  counter = counter + 1
  for(i in 1:6) {
    if(i > 1) yy = ""
    hist((results[[vars[i]]][[2]][,2]), main = "", xlab = "", col = "red", ylab = yy, xpd = NA)
  }
}
legend(-2.5, y = -200, xpd = NA, legend = c("unbalanced", "balanced"), horiz = TRUE, pch = 15, col = c("blue", "red"), bty = "n")
dev.off()



########## __Figure S19 Grand mean simulation ########## 
# t+ non-centered

mountains = c(1, 2, 5, 10, 50, 100)
sd_re = c(0.1, 0.5, 1.0)
unbalanced = c(TRUE, FALSE)
t = c(TRUE)
center = c(TRUE, FALSE)
parameter = expand.grid(mountains, sd_re, unbalanced, t, center)
colnames(parameter) = c("mountains", "sd_re","unbalanced","t","center")

results = readRDS("Results/boot_glm.RDS")
pdf("Figures/Fig_S19.pdf", height = 10.5, width = 10)

par(mfrow = c(6, 6), mar = c(1, 2, 1, 1), oma = c(5, 4, 2, 1))
mounts = c(1, 2, 5, 10, 50, 100) +1
ylab = c("0.1", "0.5", "1.0")
counter = 1
for(sd in c(0.1, 0.5, 1.0)) {
  vars = as.integer(rownames(parameter[parameter$sd_re==sd & parameter$t & !parameter$center & parameter$unbalanced,]))
  yy = paste0("SD: ", ylab[counter])
  counter = counter + 1
  for(i in 1:6) {
    if(i > 1) yy = ""
    if(sd=="0.1") hist((results[[vars[i]]][[2]][,2]), main = paste0(mounts[i], " mountains"), xlab = "", col = "blue", ylab = yy, xpd = NA)
    else hist((results[[vars[i]]][[2]][,2]), main = "", xlab = "", col = "blue", ylab = yy, xpd = NA)
  }
}
counter = 1

for(sd in c(0.1, 0.5, 1.0)) {
  vars = as.integer(rownames(parameter[parameter$sd_re==sd & parameter$t & !parameter$center & !parameter$unbalanced,]))
  yy = paste0("SD: ", ylab[counter])
  counter = counter + 1
  for(i in 1:6) {
    if(i > 1) yy = ""
    hist((results[[vars[i]]][[2]][,2]), main = "", xlab = "", col = "red", ylab = yy, xpd = NA)
  }
}
legend(-2.5, y = -200, xpd = NA, legend = c("unbalanced", "balanced"), horiz = TRUE, pch = 15, col = c("blue", "red"), bty = "n")
dev.off()



########## __Figure S20 Zero-sum simultion ########## 


mountains = c(1, 2, 5, 10, 50, 100)
sd_re = c(0.1, 0.5, 1.0)
unbalanced = c(TRUE, FALSE)
parameter = expand.grid(mountains, sd_re, unbalanced)
colnames(parameter) = c("mountains", "sd_re","unbalanced")

results = readRDS("Results/boot_zero_sum.RDS")
# t+ non-centered
pdf("Figures/Fig_S20.pdf", height = 10.5, width = 10)
par(mfrow = c(6, 6), mar = c(1, 2, 1, 1), oma = c(5, 4, 2, 1))
mounts = c(1, 2, 5, 10, 50, 100) +1
ylab = c("0.1", "0.5", "1.0")
counter = 1
for(sd in c(0.1, 0.5, 1.0)) {
  vars = as.integer(rownames(parameter[parameter$sd_re==sd  & parameter$unbalanced,]))
  yy = paste0("SD: ", ylab[counter])
  counter = counter + 1
  for(i in 1:6) {
    if(i > 1) yy = ""
    if(sd=="0.1") hist(unlist(results[[vars[i]]][[2]][,2]), main = paste0(mounts[i], " mountains"), xlab = "", col = "blue", ylab = yy, xpd = NA)
    else hist(unlist(results[[vars[i]]][[2]][,2]), main = "", xlab = "", col = "blue", ylab = yy, xpd = NA)
  }
}
counter = 1

for(sd in c(0.1, 0.5, 1.0)) {
  vars = as.integer(rownames(parameter[parameter$sd_re==sd & !parameter$unbalanced,]))
  yy = paste0("SD: ", ylab[counter])
  counter = counter + 1
  for(i in 1:6) {
    if(i > 1) yy = ""
    hist(unlist(results[[vars[i]]][[2]][,2]), main = "", xlab = "", col = "red", ylab = yy, xpd = NA)
  }
}
legend(-2.5, y = -2000, xpd = NA, legend = c("unbalanced", "balanced"), horiz = TRUE, pch = 15, col = c("blue", "red"), bty = "n")
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






########## _________________________________  ##########
########## Population estimate  ##########
########## _________________________________  ##########

########## __Figure S21 Population slope estimate ########## 
results_lmm_no_cov = readRDS("Results/results_mountain_lmm_no_cov_0.1_100_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_0.1_100_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_0.1_200_unbalanced.Rds")
results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
results_miss = readRDS("Results/results_mountain_lmm_miss_specified_0.1_100_unbalanced.Rds")




pdf("Figures/Fig_S21.pdf", width = 11, height = 12)
par(mfrow = c(4, 5), oma = c(4, 3, 2, 1), mar = c(2, 2, 1, 1))
plot_hist = function(x, xlim, ...) {
  hist(x, breaks = 30, xlim = xlim, main = "", ylim = c(0, 670), xlab = "")
  text(0.7, 570, paste0("Mean: ", round(mean(x), digits = 3)), xpd = NA)
  text(0.7, 500, paste0("Var: ", round((var(x)), digits = 3)), xpd = NA)
}
labels = c("Height ~ T + (1|mountain) + (0 + T|mountain)",
           "Height ~ T + (T|mountain)", 
           "Height ~ T + (1|mountain)", 
           "Height ~ 0 + mountain + T : mountain", 
           "Height ~ T ")

counter = 1
letters
for(i in c(1, 2, 4, 7)) {
  xlim = c(0.1, 0.8)
  plot_hist(results_lmm_no_cov[[i]]$results_w_lme4_reml$estimate_effect, breaks = 30, xlim = xlim, main = "")
  text(x = -0.08, y = 750, labels = letters[counter], xpd = NA, font = 2, cex = 1.3)
  if(i == 1) text(x = 0.5, y = 650, pos = 3, xpd = NA, labels = labels[1])
  plot_hist(results_lmm[[i]]$results_w_lme4_reml$estimate_effect, breaks = 30, xlim = xlim, main = "")
  if(i == 1) text(x = 0.5, y = 650, pos = 3, xpd = NA, labels = labels[2])
  plot_hist(results_miss[[i]]$results_w_lme4_reml$estimate_effect, breaks = 30, xlim = xlim, main = "")
  if(i == 1) text(x = 0.5, y = 650, pos = 3, xpd = NA, labels = labels[3])
  plot_hist(results_lmm_no_cov[[i]]$results_w_lm$estimate_effect, breaks = 30, xlim = xlim, main = "")
  if(i == 1) text(x = 0.5, y = 650, pos = 3, xpd = NA, labels = labels[4])
  plot_hist(results_lmm_no_cov[[i]]$results_w_lm_wo_grouping$estimate_effect, breaks = 30, xlim = xlim, main = "")
  if(i == 1) text(x = 0.5, y = 650, pos = 3, xpd = NA, labels = labels[5])
  counter = counter + 1
}
dev.off()





########## _________________________________  ##########
########## Population estimate  ##########
########## _________________________________  ##########

########## __Figure S22 Population intercept estimate ########## 
results_lmm_no_cov = readRDS("Results/results_mountain_lmm_no_cov_0.1_100_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_0.1_100_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_0.1_200_unbalanced.Rds")
results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
results_miss = readRDS("Results/results_mountain_lmm_miss_specified_0.1_100_unbalanced.Rds")




pdf("Figures/Fig_S22.pdf", width = 11, height = 12)
par(mfrow = c(4, 5), oma = c(4, 3, 2, 1), mar = c(2, 2, 1, 1))
plot_hist = function(x, xlim, ...) {
  hist(x, breaks = 30, xlim = xlim, main = "", ylim = c(0, 670), xlab = "")
  text(0.7, 570, paste0("Mean: ", round(mean(x), digits = 3)), xpd = NA)
  text(0.7, 500, paste0("Var: ", round((var(x)), digits = 3)), xpd = NA)
}
labels = c("Height ~ T + (1|mountain) + (0 + T|mountain)",
           "Height ~ T + (T|mountain)", 
           "Height ~ T + (1|mountain)", 
           "Height ~ 0 + mountain + T : mountain", 
           "Height ~ T ")

counter = 1
letters
for(i in c(1, 2, 4, 7)) {
  xlim = c(0.1, 0.8)
  plot_hist(results_lmm_no_cov[[i]]$results_w_lme4_reml$estimate_inter, breaks = 30, xlim = xlim, main = "")
  text(x = -0.08, y = 750, labels = letters[counter], xpd = NA, font = 2, cex = 1.3)
  if(i == 1) text(x = 0.5, y = 650, pos = 3, xpd = NA, labels = labels[1])
  plot_hist(results_lmm[[i]]$results_w_lme4_reml$estimate_inter, breaks = 30, xlim = xlim, main = "")
  if(i == 1) text(x = 0.5, y = 650, pos = 3, xpd = NA, labels = labels[2])
  plot_hist(results_miss[[i]]$results_w_lme4_reml$estimate_inter, breaks = 30, xlim = xlim, main = "")
  if(i == 1) text(x = 0.5, y = 650, pos = 3, xpd = NA, labels = labels[3])
  plot_hist(results_lmm_no_cov[[i]]$results_w_lm$estimate_inter, breaks = 30, xlim = xlim, main = "")
  if(i == 1) text(x = 0.5, y = 650, pos = 3, xpd = NA, labels = labels[4])
  plot_hist(results_lmm_no_cov[[i]]$results_w_lm_wo_grouping$estimate_inter, breaks = 30, xlim = xlim, main = "")
  if(i == 1) text(x = 0.5, y = 650, pos = 3, xpd = NA, labels = labels[5])
  counter = counter + 1
}
dev.off()
