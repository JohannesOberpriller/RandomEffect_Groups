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
plot_mean = function(x, w = 1) {
  mean = mean(x, na.rm=TRUE)
  segments(w-0.4, x1 = w+0.4, y0 = mean, y1 = mean, col = "red", lwd = 1.7)
  
}



########## __Figure  S1 Scenario A Intercept ########## 

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
pdf(file = "Figures/Fig_S1_Intercept.pdf", width = 11.2, height = 5.8)
par(mfrow = c(2,4), mar = c(1.3, 0.5, 1.5, 0.5), oma = c(4, 6, 3, 3)-1)
results_lmm = readRDS("Results/results_mountain_lmm_random_intercept_only_0.1_unbalanced.Rds")
results_miss = readRDS("Results/results_mountain_lmm_random_intercept_only_miss_specified_no_cov_0.1_unbalanced.Rds")
x_letters = 1.0
for(i in 1:4) {
  if(i == 3) {
    results_lmm = readRDS("Results/results_mountain_lmm_random_intercept_only_0.5_unbalanced.Rds")
    results_miss = readRDS("Results/results_mountain_lmm_random_intercept_only_miss_specified_no_cov_0.5_unbalanced.Rds")
  }
  si = c(0)
  if( i %in% c(2, 4)) si = c(0, 1)
  si2 = c(0, 1)
  
  ylab = "Average type I error rate" 
  yaxt = NULL
  if(i != 1) ylab =""
  if(i != 1) yaxt = "n"
  
  type_one_int = 
    cbind(
      sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_miss, function(l) mean(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si] < 0.05)),
      sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = ylab, xaxt="n", main = "", xlab = "", xpd = NA, yaxt=yaxt)
  text(x=x_letters, pos = 2, y = 0.54, labels = letters[i], cex = 1.2, xpd = NA, font = 2)
  if(i %in% c(1, 2)) text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Var = 0.01")
  if(i %in% c(3, 4)) text(x= 4, pos = 3, y = 0.52, xpd = NA, labels = "Var = 0.25")
  
  if(i == 1) legend("topright", legend = labels, 
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
}


results_lmm = readRDS("Results/results_mountain_lmm_random_intercept_only_0.1_unbalanced.Rds")
results_miss = readRDS("Results/results_mountain_lmm_random_intercept_only_miss_specified_no_cov_0.1_unbalanced.Rds")
for(i in 1:4) {
  if(i == 3) {
    results_lmm = readRDS("Results/results_mountain_lmm_random_intercept_only_0.5_unbalanced.Rds")
    results_miss = readRDS("Results/results_mountain_lmm_random_intercept_only_miss_specified_no_cov_0.5_unbalanced.Rds")
  }
  si = c(0)
  if( i %in% c(2, 4)) si = c(0, 1)
  si2 = c(0, 1)
  
  ylab = "Average power" 
  yaxt = NULL
  if(i != 1) ylab =""
  if(i != 1) yaxt = "n"
  
  type_two_int = 
    cbind(
      sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity %in% si] < 0.05)), 
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_inter[l$results_w_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_inter[l$results_w_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm
    )
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = ylab, xaxt="n",yaxt=yaxt, main = "", xlab = "Number of mountains", xpd=NA)
  text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "")
  text(x=x_letters, pos = 2, y = 1.08, labels = letters[i+4], cex = 1.2, xpd = NA, font = 2)
  axis(1, 1:7, 2:8)
  
  if(i == 5) legend("bottomright", legend = labels, 
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
  
  
}
dev.off()






########## __Figure  S2 Scenario B Intercept ########## 

pdf(file = "Figures/Fig_S2_intercept.pdf", width = 11.2, height = 5.8)

labels = c("Height ~ T + (1|mountain) + (0 + T|mountain)",
           "Height ~ T + (T|mountain)", 
           "Height ~ T + (1|mountain)", 
           "Height ~ 0 + mountain + T : mountain", 
           "Height ~ T ")


par(mfrow = c(2,4), mar = c(1.3, 0.5, 1.5, 0.5), oma = c(4, 6, 3, 3)-1)

si = c(0)
si2 = c(0, 1)
cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
lty = c(1, 1,1, 2, 2)
pch = c(15, 20, 16:18)
cex_legend = 0.9
## Type I error ##

results_lmm_no_cov = readRDS("Results/results_mountain_lmm_no_cov_0.1_100_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_0.1_100_unbalanced.Rds")
results_miss = readRDS("Results/results_mountain_lmm_miss_specified_0.1_100_unbalanced.Rds")
x_letters = 1.0
for(i in 1:4) {
  if(i == 3) {
    results_lmm_no_cov = readRDS("Results/results_mountain_lmm_no_cov_0.5_100_unbalanced.Rds")
    results_lmm = readRDS("Results/results_mountain_lmm_0.5_100_unbalanced.Rds")
    results_miss = readRDS("Results/results_mountain_lmm_miss_specified_0.5_100_unbalanced.Rds")
    
  }
  si = c(0)
  if( i %in% c(2, 4)) si = c(0, 1)
  si2 = c(0, 1)
  
  ylab = "Average type I error rate" 
  yaxt = NULL
  if(i != 1) ylab =""
  if(i != 1) yaxt = "n"
  
  
  type_one_int = 
    cbind(
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity %in% si]< 0.05)), #lme4
      sapply(results_lmm, function(l)        mean(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity%in% si]< 0.05)), #lme4
      sapply(results_miss, function(l)       mean(l$results_wo_lme4_reml$p_value_inter[l$results_wo_lme4_reml$Singularity%in% si]< 0.05)),
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm$p_value_inter[l$results_wo_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping$p_value_inter[l$results_wo_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 0.8), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = ylab, xaxt="n", main = "", xlab = "", xpd = NA, yaxt=yaxt)
  text(x=x_letters, pos = 2, y = 0.864, labels = letters[i], cex = 1.2, xpd = NA, font = 2)
  if(i %in% c(1, 2)) text(x= 4, pos = 3, y = 0.832, xpd = NA, labels = "Var = 0.01")
  if(i %in% c(3, 4)) text(x= 4, pos = 3, y = 0.832, xpd = NA, labels = "Var = 0.25")
  
  if(i == 1) legend("topright", legend = labels, 
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
  
}



results_lmm_no_cov = readRDS("Results/results_mountain_lmm_no_cov_0.1_100_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_0.1_100_unbalanced.Rds")
results_miss = readRDS("Results/results_mountain_lmm_miss_specified_0.1_100_unbalanced.Rds")
x_letters = 1.0
for(i in 1:4) {
  if(i == 3) {
    results_lmm_no_cov = readRDS("Results/results_mountain_lmm_no_cov_0.5_100_unbalanced.Rds")
    results_lmm = readRDS("Results/results_mountain_lmm_0.5_100_unbalanced.Rds")
    results_miss = readRDS("Results/results_mountain_lmm_miss_specified_0.5_100_unbalanced.Rds")
    
  }
  si = c(0)
  if( i %in% c(2, 4)) si = c(0, 1)
  si2 = c(0, 1)
  
  ylab = "Average power" 
  yaxt = NULL
  if(i != 1) ylab =""
  if(i != 1) yaxt = "n"
  
  type_two_int = 
    cbind(
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), #lme4
      sapply(results_lmm, function(l)        1-mean(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), #lme4
      sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_inter[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), 
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_inter[l$results_w_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_inter[l$results_w_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)) #lm
    )
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = ylab, xaxt="n",yaxt=yaxt, main = "", xlab = "Number of mountains", xpd=NA)
  text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "")
  text(x=x_letters, pos = 2, y = 1.08, labels = letters[i+4], cex = 1.2, xpd = NA, font = 2)
  axis(1, 1:7, 2:8)
  
  if(i == 5) legend("bottomright", legend = labels, 
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
  
}
dev.off()





########## __Figure S3 Variance estimates lmm MLE/REML ########## 
results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")

pdf(file = "Figures/Fig_S3.pdf", width = 9, height = 5)
par(mfrow = c(1,2), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
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

dev.off()












########## __Figure S4 Variance estimates lmm REML imbalanced vs REML balanced ########## 
cols = viridis::viridis(5)

results_lmm_im = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_.Rds")


pdf(file = "Figures/Fig_S4.pdf", width = 9, height = 5)
par(mfrow = c(1,2), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
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



dev.off()




########## __Figure S5 Variance estimates lmm MLE imbalanced vs MLE balanced ########## 
cols = viridis::viridis(5)

results_lmm_im = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_.Rds")

pdf(file = "Figures/Fig_S5.pdf", width = 9, height = 5)
par(mfrow = c(1,2), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
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


dev.off()







########## _________________________________  ##########
########## GLMM  ##########
########## _________________________________  ##########

########## __Figure  S6 Scenario A Slope ########## 

get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))

cols = RColorBrewer::brewer.pal(4, "Set1")
cols = c(cols[-2])
lty = c(1, 1, 2)
pch = c(15:18)

cex_legend = 0.9
labels = c("RS ~ T + (1|mountain)", "RS ~ 0 + T + mountain", "RS ~ T ")


## Type I error ##
pdf(file = "Figures/Fig_S6.pdf", width = 11.2, height = 8.8)
par(mfrow = c(2,2), mar = c(1.3, 0.5, 1.5, 0.5), oma = c(4, 6, 3, 3)-1)
results_glmm = readRDS("Results/results_mountain_glmm_random_intercept_only_100_unbalanced.Rds")
x_letters = 1.0
for(i in 1:2) {
  si = c(0)
  if( i %in% c(2, 4)) si = c(0, 1)
  si2 = c(0, 1)
  
  ylab = "Average type I error rate" 
  yaxt = NULL
  if(i != 1) ylab =""
  if(i != 1) yaxt = "n"
  
  type_one_int = 
    cbind(
      sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm, function(l) mean(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) mean(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = ylab, xaxt="n", main = "", xlab = "", xpd = NA, yaxt=yaxt)
  text(x=x_letters, pos = 2, y = 0.54, labels = letters[i], cex = 1.2, xpd = NA, font = 2)
  
  if(i == 1) legend("topright", legend = labels, 
                    col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
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
}


for(i in 1:2) {
  si = c(0)
  if( i %in% c(2, 4)) si = c(0, 1)
  si2 = c(0, 1)
  
  ylab = "Average power" 
  yaxt = NULL
  if(i != 1) ylab =""
  if(i != 1) yaxt = "n"
  
  type_two_int = 
    cbind(
      sapply(results_glmm, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = ylab, xaxt="n",yaxt=yaxt, main = "", xlab = "Number of mountains", xpd=NA)
  text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "")
  text(x=x_letters, pos = 2, y = 1.08, labels = letters[i+2], cex = 1.2, xpd = NA, font = 2)
  axis(1, 1:7, 2:8)
  
  if(i == 3) legend("bottomright", legend = labels, 
                    col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
  sd = 
    cbind(
      sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si]  < 0.05)/sqrt( sum(!is.na( l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))), #lme4
      sapply(results_glmm, function(l) sd(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)/sqrt( sum(!is.na( l$results_w_glmmTMB_reml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))), # glmmTMB
      sapply(results_glmm, function(l) sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))) #lm
    )
  mm =1-type_two_int
  upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
  
}
dev.off()






########## __Figure  S7 Scenario B Slope ########## 

pdf(file = "Figures/Fig_S7.pdf", width = 11.2, height = 5.8)

par(mfrow = c(2,4), mar = c(1.3, 0.5, 1.5, 0.5), oma = c(4, 6, 3, 3)-1)

cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
cols = cols[-3]
lty = c(1, 1, 2, 2)
pch = c(15, 20, 17:18)
si = c(0)
labels = c("RS ~ T + (1|mountain) + (0 + T|mountain)", 
           "RS ~ T + (T|mountain)","RS ~ 0 + mountain + T : mountain", "RS ~ T ")
## Type I error ##

results_glmm = readRDS("Results/results_mountain_glmm_0.1_200_unbalanced.Rds")
results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
x_letters = 1.0
for(i in 1:4) {
  if(i == 3) {
    results_glmm = readRDS("Results/results_mountain_glmm_0.5_200_unbalanced.Rds")
    results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.5_200_unbalanced.Rds")
    
  }
  si = c(0)
  if( i %in% c(2, 4)) si = c(0, 1)
  si2 = c(0, 1)
  
  ylab = "Average type I error rate" 
  yaxt = NULL
  if(i != 1) ylab =""
  if(i != 1) yaxt = "n"
  
  
  type_one_int = 
    cbind(
      sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm,        function(l) mean(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 0.8), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = ylab, xaxt="n", main = "", xlab = "", xpd = NA, yaxt=yaxt)
  text(x=x_letters, pos = 2, y = 0.864, labels = letters[i], cex = 1.2, xpd = NA, font = 2)
  if(i %in% c(1, 2)) text(x= 4, pos = 3, y = 0.832, xpd = NA, labels = "Var = 0.01")
  if(i %in% c(3, 4)) text(x= 4, pos = 3, y = 0.832, xpd = NA, labels = "Var = 0.25")
  
  if(i == 1) legend("topright", legend = labels, 
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
  
}



results_glmm = readRDS("Results/results_mountain_glmm_0.1_200_unbalanced.Rds")
results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
x_letters = 1.0
for(i in 1:4) {
  if(i == 3) {
    results_glmm = readRDS("Results/results_mountain_glmm_0.5_200_unbalanced.Rds")
    results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.5_200_unbalanced.Rds")
    
  }
  si = c(0)
  if( i %in% c(2, 4)) si = c(0, 1)
  si2 = c(0, 1)
  
  ylab = "Average power" 
  yaxt = NULL
  if(i != 1) ylab =""
  if(i != 1) yaxt = "n"
  
  type_two_int = 
    cbind(
      sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm,        function(l) 1-mean(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = ylab, xaxt="n",yaxt=yaxt, main = "", xlab = "Number of mountains", xpd=NA)
  text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "")
  text(x=x_letters, pos = 2, y = 1.08, labels = letters[i+4], cex = 1.2, xpd = NA, font = 2)
  axis(1, 1:7, 2:8)
  
  if(i == 5) legend("bottomright", legend = labels, 
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
  
}
dev.off()



########## __Figure  S8 Scenario A Intercept ########## 

get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))

cols = RColorBrewer::brewer.pal(4, "Set1")
cols = c(cols[-2])
lty = c(1, 1, 2)
pch = c(15:18)

cex_legend = 0.9
labels = c("RS ~ T + (1|mountain)", "RS ~ 0 + T + mountain", "RS ~ T ")


## Type I error ##
pdf(file = "Figures/Fig_S8.pdf", width = 11.2, height = 8.8)
par(mfrow = c(2,2), mar = c(1.3, 0.5, 1.5, 0.5), oma = c(4, 6, 3, 3)-1)
results_glmm = readRDS("Results/results_mountain_glmm_random_intercept_only_100_unbalanced.Rds")
x_letters = 1.0
for(i in 1:2) {
  si = c(0)
  if( i %in% c(2, 4)) si = c(0, 1)
  si2 = c(0, 1)
  
  ylab = "Average type I error rate" 
  yaxt = NULL
  if(i != 1) ylab =""
  if(i != 1) yaxt = "n"
  
  type_one_int = 
    cbind(
      sapply(results_glmm, function(l) mean(l$results_wo_lme4_ml$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm, function(l) mean(l$results_wo_lm$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 0.5), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = ylab, xaxt="n", main = "", xlab = "", xpd = NA, yaxt=yaxt)
  text(x=x_letters, pos = 2, y = 0.54, labels = letters[i], cex = 1.2, xpd = NA, font = 2)
  
  if(i == 1) legend("topright", legend = labels, 
                    col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
  abline(h = 0.05, lty = 3, col = "darkgrey")
  sd = 
    cbind(
      sapply(results_glmm, function(l) sd(l$results_wo_lme4_ml$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)/sqrt( sum(!is.na(l$results_wo_lme4_ml$p_value_effect[l$results_wo_lme4_ml$Singularity %in% si])) )), #lme4
      sapply(results_glmm, function(l) sd(l$results_wo_lm$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)/sqrt(sum(!is.na(l$results_wo_lm[[2]][l$results_wo_lme4_ml$Singularity %in% si])))), #lm
      sapply(results_glmm, function(l) sd(l$results_wo_lm_wo_grouping$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)/sqrt(sum(!is.na(l$results_wo_lm_wo_grouping[[2]][l$results_wo_lme4_ml$Singularity %in% si])))) #lm
    )
  mm =type_one_int
  upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
}


for(i in 1:2) {
  si = c(0)
  if( i %in% c(2, 4)) si = c(0, 1)
  si2 = c(0, 1)
  
  ylab = "Average power" 
  yaxt = NULL
  if(i != 1) ylab =""
  if(i != 1) yaxt = "n"
  
  type_two_int = 
    cbind(
      sapply(results_glmm, function(l) 1-mean(l$results_w_lme4_ml$p_value_inter[l$results_w_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm$p_value_inter[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_inter[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = ylab, xaxt="n",yaxt=yaxt, main = "", xlab = "Number of mountains", xpd=NA)
  text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "")
  text(x=x_letters, pos = 2, y = 1.08, labels = letters[i+2], cex = 1.2, xpd = NA, font = 2)
  axis(1, 1:7, 2:8)
  
  if(i == 3) legend("bottomright", legend = labels, 
                    col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
  sd = 
    cbind(
      sapply(results_glmm, function(l) sd(l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si]  < 0.05)/sqrt( sum(!is.na( l$results_w_lme4_ml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))), #lme4
      sapply(results_glmm, function(l) sd(l$results_w_lm$p_value_inter[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)/sqrt( sum(!is.na( l$results_w_glmmTMB_reml$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))), # glmmTMB
      sapply(results_glmm, function(l) sd(l$results_w_lm_wo_grouping$p_value_inter[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)/sqrt( sum(!is.na(l$results_w_lm$p_value_effect[l$results_w_lme4_ml$Singularity %in% si] )))) #lm
    )
  mm =1-type_two_int
  upper = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:3, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:3, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
  
}
dev.off()








########## __Figure  S9 Scenario B Intercept ########## 
pdf(file = "Figures/Fig_S9.pdf", width = 11.2, height = 5.8)

par(mfrow = c(2,4), mar = c(1.3, 0.5, 1.5, 0.5), oma = c(4, 6, 3, 3)-1)

cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
cols = cols[-3]
lty = c(1, 1, 2, 2)
pch = c(15, 20, 17:18)
si = c(0)
labels = c("RS ~ T + (1|mountain) + (0 + T|mountain)", 
           "RS ~ T + (T|mountain)","RS ~ 0 + mountain + T : mountain", "RS ~ T ")
## Type I error ##

results_glmm = readRDS("Results/results_mountain_glmm_0.1_200_unbalanced.Rds")
results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
x_letters = 1.0
for(i in 1:4) {
  if(i == 3) {
    results_glmm = readRDS("Results/results_mountain_glmm_0.5_200_unbalanced.Rds")
    results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.5_200_unbalanced.Rds")
    
  }
  si = c(0)
  if( i %in% c(2, 4)) si = c(0, 1)
  si2 = c(0, 1)
  
  ylab = "Average type I error rate" 
  yaxt = NULL
  if(i != 1) ylab =""
  if(i != 1) yaxt = "n"
  
  
  type_one_int = 
    cbind(
      sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lme4_ml$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm,        function(l) mean(l$results_wo_lme4_ml$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lm$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping$p_value_inter[l$results_wo_lme4_ml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(type_one_int, type="o", ylim = c(0, 0.8), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = ylab, xaxt="n", main = "", xlab = "", xpd = NA, yaxt=yaxt)
  text(x=x_letters, pos = 2, y = 0.864, labels = letters[i], cex = 1.2, xpd = NA, font = 2)
  if(i %in% c(1, 2)) text(x= 4, pos = 3, y = 0.832, xpd = NA, labels = "Var = 0.01")
  if(i %in% c(3, 4)) text(x= 4, pos = 3, y = 0.832, xpd = NA, labels = "Var = 0.25")
  
  if(i == 1) legend("topright", legend = labels, 
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
  
}



results_glmm = readRDS("Results/results_mountain_glmm_0.1_200_unbalanced.Rds")
results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
x_letters = 1.0
for(i in 1:4) {
  if(i == 3) {
    results_glmm = readRDS("Results/results_mountain_glmm_0.5_200_unbalanced.Rds")
    results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.5_200_unbalanced.Rds")
    
  }
  si = c(0)
  if( i %in% c(2, 4)) si = c(0, 1)
  si2 = c(0, 1)
  
  ylab = "Average power" 
  yaxt = NULL
  if(i != 1) ylab =""
  if(i != 1) yaxt = "n"
  
  type_two_int = 
    cbind(
      sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lme4_ml$p_value_inter[l$results_w_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm,        function(l) 1-mean(l$results_w_lme4_ml$p_value_inter[l$results_w_lme4_ml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_inter[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)), #lm
      sapply(results_glmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_inter[l$results_w_lme4_ml$Singularity %in% si2]  < 0.05, na.rm=TRUE)) #lm
    )
  
  matplot(1-type_two_int, type="o", ylim = c(0, 1.0), pch = pch, las = 1, lty = lty, col = cols, 
          ylab = ylab, xaxt="n",yaxt=yaxt, main = "", xlab = "Number of mountains", xpd=NA)
  text(x= 4, pos = 3, y = 1.04, xpd = NA, labels = "")
  text(x=x_letters, pos = 2, y = 1.08, labels = letters[i+4], cex = 1.2, xpd = NA, font = 2)
  axis(1, 1:7, 2:8)
  
  if(i == 5) legend("bottomright", legend = labels, 
                    col = cols, pch = pch, bty = "n", lty = lty, cex = cex_legend)
  sd = 
    cbind(
      sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lme4_ml$p_value_inter[l$results_w_lme4_ml$Singularity %in% si])),
      sapply(results_glmm,        function(l) get_sd(l$results_w_lme4_ml$p_value_inter[l$results_w_lme4_ml$Singularity %in% si])),
      sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lm$p_value_inter[l$results_w_lme4_ml$Singularity %in% si2])),
      sapply(results_glmm_no_cov, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_inter[l$results_w_lme4_ml$Singularity %in% si2] ))
    )
  mm =1-type_two_int
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.10)))
  
}
dev.off()


########## __Figure S10 Variance estimates lme4 ########## 


cols = viridis::viridis(5)

results_glmm = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")

adj = 1.0

pdf(file = "Figures/Fig_S10.pdf", width = 9, height = 9)

par(mfrow = c(2,2), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
plot_mean = function(x, w = 1) {
  mean = mean(x, na.rm=TRUE)
  segments(w-0.4, x1 = w+0.4, y0 = mean, y1 = mean, col = "red", lwd = 1.7)
  
}
lty = 2
lwd = 1.4
bb= boxplot(results_glmm[[1]]$results_w_lme4_ml$stddev_randeff_inter**2, 
            results_glmm[[2]]$results_w_lme4_ml$stddev_randeff_inter**2, 
            results_glmm[[3]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_glmm[[4]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_glmm[[5]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_glmm[[6]]$results_w_lme4_ml$stddev_randeff_inter**2,
            results_glmm[[7]]$results_w_lme4_ml$stddev_randeff_inter**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)

legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.084, pos = 3, labels = letters[1], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_lme4_reml$stddev_randeff_inter**2, i))

bb= boxplot(results_glmm[[1]]$results_w_lme4_ml$stddev_randeff_x**2, 
            results_glmm[[2]]$results_w_lme4_ml$stddev_randeff_x**2, 
            results_glmm[[3]]$results_w_lme4_ml$stddev_randeff_x**2, 
            results_glmm[[4]]$results_w_lme4_ml$stddev_randeff_x**2, 
            results_glmm[[5]]$results_w_lme4_ml$stddev_randeff_x**2, 
            results_glmm[[6]]$results_w_lme4_ml$stddev_randeff_x**2,
            results_glmm[[7]]$results_w_lme4_ml$stddev_randeff_x**2, ylim = c(0, 0.08), las = 1, outline=FALSE,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)
legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.084, pos = 3, labels = letters[2], xpd= NA, cex = 1.3, font = 2)

abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_lme4_reml$stddev_randeff_x**2, i))

### Without SI
bb= boxplot(results_glmm[[1]]$results_w_lme4_ml$stddev_randeff_inter[results_lmm[[1]]$results_w_lme4_ml$Singularity == 0]**2, 
            results_glmm[[2]]$results_w_lme4_ml$stddev_randeff_inter[results_lmm[[2]]$results_w_lme4_ml$Singularity == 0]**2, 
            results_glmm[[3]]$results_w_lme4_ml$stddev_randeff_inter[results_lmm[[2]]$results_w_lme4_ml$Singularity == 0]**2, 
            results_glmm[[4]]$results_w_lme4_ml$stddev_randeff_inter[results_lmm[[2]]$results_w_lme4_ml$Singularity == 0]**2, 
            results_glmm[[5]]$results_w_lme4_ml$stddev_randeff_inter[results_lmm[[2]]$results_w_lme4_ml$Singularity == 0]**2, 
            results_glmm[[6]]$results_w_lme4_ml$stddev_randeff_inter[results_lmm[[4]]$results_w_lme4_ml$Singularity == 0]**2,
            results_glmm[[7]]$results_w_lme4_ml$stddev_randeff_inter[results_lmm[[7]]$results_w_lme4_ml$Singularity == 0]**2, ylim = c(0, 0.13), outline=FALSE, las = 1,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)
legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.1365, pos = 3, labels = letters[3], xpd= NA, cex = 1.3, font = 2)

abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i ]]$results_w_lme4_reml$stddev_randeff_inter[results_lmm[[i]]$results_w_lme4_reml$Singularity == 0]**2, i))

bb= boxplot(results_glmm[[1]]$results_w_lme4_ml$stddev_randeff_x[results_lmm[[1]]$results_w_lme4_ml$Singularity == 0]**2, 
            results_glmm[[2]]$results_w_lme4_ml$stddev_randeff_x[results_lmm[[2]]$results_w_lme4_ml$Singularity == 0]**2, 
            results_glmm[[3]]$results_w_lme4_ml$stddev_randeff_x[results_lmm[[2]]$results_w_lme4_ml$Singularity == 0]**2, 
            results_glmm[[4]]$results_w_lme4_ml$stddev_randeff_x[results_lmm[[2]]$results_w_lme4_ml$Singularity == 0]**2, 
            results_glmm[[5]]$results_w_lme4_ml$stddev_randeff_x[results_lmm[[2]]$results_w_lme4_ml$Singularity == 0]**2, 
            results_glmm[[6]]$results_w_lme4_ml$stddev_randeff_x[results_lmm[[4]]$results_w_lme4_ml$Singularity == 0]**2,
            results_glmm[[7]]$results_w_lme4_ml$stddev_randeff_x[results_lmm[[7]]$results_w_lme4_ml$Singularity == 0]**2, ylim = c(0, 0.13), outline=FALSE,las = 1,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)
legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.1365, pos = 3, labels = letters[4], xpd= NA, cex = 1.3, font = 2)

abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i ]]$results_w_lme4_reml$stddev_randeff_x[results_lmm[[i]]$results_w_lme4_reml$Singularity == 0]**2, i))

dev.off()




########## _________________________________  ##########
########## Grand mean simulations  ##########
########## _________________________________  ##########



########## __Figure S11 Grand mean simulation ########## 
# t+ non-centered

mountains = c(1, 2, 5, 10, 50, 100)
sd_re = c(0.1, 0.5, 1.0)
unbalanced = c(TRUE, FALSE)
t = c(TRUE, FALSE)
center = c(TRUE, FALSE)
parameter = expand.grid(mountains, sd_re, unbalanced, t, center)
colnames(parameter) = c("mountains", "sd_re","unbalanced","t","center")

results = readRDS("Results/boot.RDS")
pdf("Figures/Fig_S11.pdf", height = 10.5, width = 10)

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



########## __Figure S12 Grand mean simulation ########## 
# t+ non-centered

mountains = c(1, 2, 5, 10, 50, 100)
sd_re = c(0.1, 0.5, 1.0)
unbalanced = c(TRUE, FALSE)
t = c(TRUE)
center = c(TRUE, FALSE)
parameter = expand.grid(mountains, sd_re, unbalanced, t, center)
colnames(parameter) = c("mountains", "sd_re","unbalanced","t","center")

results = readRDS("Results/boot_glm.RDS")
pdf("Figures/Fig_S12.pdf", height = 10.5, width = 10)

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
legend(-2.5, y = -800, xpd = NA, legend = c("unbalanced", "balanced"), horiz = TRUE, pch = 15, col = c("blue", "red"), bty = "n")
dev.off()



########## __Figure S13 Zero-sum simultion ########## 


mountains = c(1, 2, 5, 10, 50, 100)
sd_re = c(0.1, 0.5, 1.0)
unbalanced = c(TRUE, FALSE)
parameter = expand.grid(mountains, sd_re, unbalanced)
colnames(parameter) = c("mountains", "sd_re","unbalanced")

results = readRDS("Results/boot_zero_sum.RDS")
# t+ non-centered
pdf("Figures/Fig_S13.pdf", height = 10.5, width = 10)
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



########## __Figure S14 Zero-sum simultion glm ########## 


mountains = c(1, 2, 5, 10, 50, 100)
sd_re = c(0.1, 0.5, 1.0)
unbalanced = c(TRUE, FALSE)
parameter = expand.grid(mountains, sd_re, unbalanced)
colnames(parameter) = c("mountains", "sd_re","unbalanced")

results = readRDS("Results/boot_zero_sum_glm.RDS")
# t+ non-centered
pdf("Figures/Fig_S14.pdf", height = 10.5, width = 10)
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
legend(-2.5, y = -1800, xpd = NA, legend = c("unbalanced", "balanced"), horiz = TRUE, pch = 15, col = c("blue", "red"), bty = "n")
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

########## __Figure S15 Population slope estimate ########## 
results_lmm_no_cov = readRDS("Results/results_mountain_lmm_no_cov_0.1_100_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_0.1_100_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_0.1_200_unbalanced.Rds")
results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
results_miss = readRDS("Results/results_mountain_lmm_miss_specified_0.1_100_unbalanced.Rds")




pdf("Figures/Fig_S15.pdf", width = 11, height = 12)
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

########## __Figure S16 Population intercept estimate ########## 
results_lmm_no_cov = readRDS("Results/results_mountain_lmm_no_cov_0.1_100_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_0.1_100_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_0.1_200_unbalanced.Rds")
results_glmm_no_cov = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
results_miss = readRDS("Results/results_mountain_lmm_miss_specified_0.1_100_unbalanced.Rds")




pdf("Figures/Fig_S16.pdf", width = 11, height = 12)
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



########## _________________________________  ##########
########## Box figures ##########
########## _________________________________  ##########
set.seed(4)
n_each = 100
number_groups =n_groups= 5
n = (number_groups)*n_each
x <- runif(n, -1, 1) # Temperature
X <- matrix(c(rep(1, n), x), nrow = n) # Intercept, Temperature
sd_randeff = 0.1 # sd for random effects
beta = 0.4    # Temperature effect meaning when the temperature increases 1 degree the respective variable also does 
beta0 = 10.0   # Intercept: the mean height of a reporductive plant  
continue = TRUE
while(continue) {
  g <- sample.int(n_groups, n_each*n_groups, replace = TRUE, prob = runif(n_groups, 0.1/(n_groups/2), 0.9/(n_groups/2)))
  if(min(table(g)) > 2) continue = FALSE
}
group <-  as.factor(g)
randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff) # random intercept
randslope <- rnorm(n_groups, mean = beta, sd = sd_randeff)     # random slope    # random slope

mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],randslope[g[i]]))

sigma <- 0.5
y <- rnorm(n, mu, sd = sigma) 
library(lme4)
library(lmerTest)
fit_lmm <- lmer(y ~ x  + (1 | group) + (0+x | group), REML = FALSE) 
fit_lm = lm(y ~ 0+x*group-x)

summary(fit_lmm)
summary(fit_lm)

pdf(file = "Figures/box_1.pdf")
effs = coef(summary(fit_lmm))[1:2,1]
re = ranef(fit_lmm)
par(mfrow = c(1,1))
cols2 = RColorBrewer::brewer.pal(5, "Set1")
cols = c(cols2[1], cols2[5], cols2[2:4])
plot(NULL, NULL, xlab="", ylab ="", xlim = c(0,1), ylim = c(9.8, 10.8), xaxt="n", yaxt="n")
sapply(1:nrow(re$group), function(i) abline(re$group[i, 1]+effs[1], re$group[i, 2]+effs[2], col = "black", lty = 1))
axis(1, at = seq(0, 1, length.out=5), labels = rep("", 5))
axis(2, at = seq(9.8, 10.8, length.out=5), labels = rep("", 5))
abline(effs[1], effs[2], col=cols[3], lwd=4)
dev.off()

pdf(file = "Figures/box_2.pdf", width = 9, height = 8)
ylim = c(9.8, 10.8)
par(mfrow = c(2,2), oma = c(3,5,3,3), mar = c(2.4, 1.5, 1.5, 1.5))
### intercept only
fit_lmm <- lmer(y ~ x  + (1 | group) , REML = FALSE) 
fit_lm = lm(y ~ 0+group+x)


plot(NULL, NULL, xlab="", ylab ="Height (cm)", xlim = c(0,1), ylim = ylim, las = 1, xpd=NA)
effs = coef(summary(fit_lm))[,1]
sapply(1:5, function(i) abline(effs[i], effs[6], col = "black", lty = 1))
text(-0.1, ylim[2]*1.01, labels = "a", font = 2, xpd=NA, cex=1.2)


effs = coef(summary(fit_lmm))[1:2,1]
re = ranef(fit_lmm)
plot(NULL, NULL, xlab="", ylab ="", xlim = c(0,1), ylim = ylim, las = 1)
sapply(1:nrow(re$group), function(i) abline(re$group[i, 1]+effs[1], effs[2], col = "black", lty = 1))
abline(effs[1], effs[2], col=cols[3], lwd=4)
text(-0.05, ylim[1]*1.05, labels = "a")
text(-0.1, ylim[2]*1.01, labels = "b", font = 2, xpd=NA, cex=1.2)


## intercept+slope
fit_lmm <- lmer(y ~ x  + (1 | group) + (0+x | group), REML = FALSE) 
fit_lm = lm(y ~ 0+x*group-x)


plot(NULL, NULL, xlab="Temperature", ylab ="Height (cm)", xlim = c(0,1), ylim = ylim, las = 1, xpd = NA)
effs = coef(summary(fit_lm))[,1]
sapply(1:5, function(i) abline(effs[i], effs[i+5], col = "black", lty = 1))
text(-0.1, ylim[2]*1.01, labels = "c", font = 2, xpd=NA, cex=1.2)

effs = coef(summary(fit_lmm))[,1]
re = ranef(fit_lmm)
plot(NULL, NULL, xlab="Temperature", ylab ="", xlim = c(0,1), ylim = ylim, las = 1, xpd=NA)
sapply(1:nrow(re$group), function(i) abline(re$group[i, 1]+effs[1], re$group[i, 2]+effs[2], col = "black", lty = 1))
abline(effs[1], effs[2], col=cols[3], lwd=4)
text(-0.1, ylim[2]*1.01, labels = "d", font = 2, xpd=NA, cex=1.2)
dev.off()



