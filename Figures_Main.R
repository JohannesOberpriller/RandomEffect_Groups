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



########## __Figure  1 Random intercept only  Type I, Error, etc. for lmm ########## 


get_sd = function(v) sd(v < 0.05, na.rm=TRUE) / sum(!is.na(v))

cols = RColorBrewer::brewer.pal(5, "Set1")
lty = c(1, 1, 2, 2)
pch = c(15, 16:18)

cex_legend = 0.74
labels = c("Height ~ T + (1|mountain)", 
           "Height ~ T + (1|mountain) + (0 + T|mountain)", 
           "Height ~ 0 + T + mountain", 
           "Height ~ T ")


## Type I error ##
pdf(file = "Figures/Fig_1.pdf", width = 9.5, height = 4.8)
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
      sapply(results_lmm, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_miss, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si] < 0.05)),
      sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
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
      sapply(results_lmm, function(l) get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
      sapply(results_miss, function(l)get_sd(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si])),
      sapply(results_lmm, function(l) get_sd(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2])),
      sapply(results_lmm, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si2]))
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
      sapply(results_lmm, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si] < 0.05)), #lme4
      sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si] < 0.05)), 
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2] < 0.05, na.rm=TRUE)) #lm
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
      sapply(results_lmm, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
      sapply(results_miss, function(l) get_sd(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity %in% si])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity %in% si2]))
    )
  mm =1-type_two_int
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  
  
}
dev.off()








########## __Figure  2 Random intercept + slope  Type I, Error, etc. for lmm ########## 

pdf(file = "Figures/Fig_2.pdf", width = 9.5, height = 4.8)

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
cex_legend = 0.74
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
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity %in% si]< 0.05)), #lme4
      sapply(results_lmm, function(l)        mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si]< 0.05)), #lme4
      sapply(results_miss, function(l)       mean(l$results_wo_lme4_reml$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si]< 0.05)),
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[l$results_wo_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
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
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), #lme4
      sapply(results_lmm, function(l)        1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), #lme4
      sapply(results_miss, function(l) 1-mean(l$results_w_lme4_reml$p_value_effect[l$results_w_lme4_reml$Singularity%in% si] < 0.05)), 
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_effect[l$results_w_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[l$results_w_lme4_reml$Singularity%in% si2] < 0.05, na.rm=TRUE)) #lm
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
  
}
dev.off()



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
typeI_LM = qgam(TypeOneLM_GM~ s(var)+s(moutain)+s(total)+s(balanced2),
                data = sub, qu = 0.5, control = list(link = "identity"))

typeI_LMM = qgam(TypeOneLMM~  s(var)+s(moutain)+s(total)+s(balanced2),
                   data = sub, qu = 0.5, control = list(link = "identity"))
power_LM = qgam(PowerLM_GM~ s(var)+s(moutain)+s(total)+s(balanced2),
                data = sub, qu = 0.5, control = list(link = "identity"))
power_LMM = qgam(PowerLMM~  s(var)+s(moutain)+s(total)+s(balanced2),
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
       type = "l", ylim = ylim, col = "red", ylab = name,
       xlab = xlab, lwd = 2, cex.lab = 1.3, las = 1)
  polygon(c(rev(x_values_fixed), x_values_fixed),
          c(rev(y_values_fixed -2*se_fixed), y_values_fixed+ 2*se_fixed)
          , col = t_col('red',80), border = NA)
  lines(x_values_random,
        y_values_random,
        type = "l", ylim = ylim, col = "blue", lwd =2)
  polygon(c(rev(x_values_random), x_values_random),
          c(rev(y_values_random -2*se_random), y_values_random+ 2*se_random)
          , col = t_col('blue',80), border = NA)
}

make_bar_plot = function(coefficient_fixed, coefficient_random, 
                         ylab, ylim){
barplot(height = c(coefficient_fixed,
          coefficient_random),
        names = c("Fixed","Random"),
        col = c(t_col('red',80),t_col('blue',80)),
        ylab = ylab, cex.lab = 1.3,
        cex.names = 1.2, las =1, ylim =ylim)

}

make_bar_plot(typeI_LM$coefficients[1],typeI_LMM$coefficients[1],"Average Type I error rate",
              c(0,0.05))

pdf("Figures/Fig_5.pdf", width = 12, height = 6.2)
par(mfrow = c(2,5))
par(mar = c(4.2,4,0.1,1.2), oma = c(2, 2, 3, 2))
ylim = c(-0.03,0.01)

make_bar_plot(typeI_LM$coefficients[1],typeI_LMM$coefficients[1],"Average Type I error rate",
              c(0,0.05))
add_label(i =1, y = 0.051)
build_plot(typeI_var,ylim = ylim, "Type I error rate change", se_fixed = se_typeI_lm[[1]], se_random =se_typeI_lmm[[1]])
add_label(i = 2)
build_plot(typeI_mountain,ylim = ylim, "Type I error rate change", se_fixed = se_typeI_lm[[2]], se_random =se_typeI_lmm[[2]])
add_label(x = -2.5,i = 3)
build_plot(typeI_total,ylim = ylim, "Type I error rate change", se_fixed = se_typeI_lm[[3]], se_random =se_typeI_lmm[[3]])
add_label(x=-1250,i = 4)
build_plot(typeI_balance,ylim = ylim, "Type I error rate change", se_fixed = se_typeI_lm[[4]], se_random =se_typeI_lmm[[4]])
add_label(x=-0.125,i = 5)

#par(mar = c(6,4,rep(1.2,2)))
make_bar_plot(power_LM$coefficients[1],power_LMM$coefficients[1],"Average Power",
              c(0,0.3))
add_label(i = 6, y = 0.31)
build_plot(power_var,ylim = c(-0.1,0.8), "Power change", xlab = "variance", se_fixed = se_power_lm[[1]], se_random =se_power_lmm[[1]])
add_label(i = 7, y = 0.87)
build_plot(power_mountain,ylim = c(-0.1,0.8), "Power change", xlab = "number of mountains", se_fixed = se_power_lm[[2]], se_random =se_power_lmm[[2]])
add_label(x = -2.5,i = 8, y = 0.87)

build_plot(power_total,ylim = c(-0.1,0.8), "Power change", xlab = "total number of observations", se_fixed = se_power_lm[[3]], se_random =se_power_lmm[[3]])
add_label(x=-1250,i = 9, y = 0.87)

build_plot(power_balance,ylim = c(-0.1,0.8), "Power change", xlab = "balance between groups", se_fixed = se_power_lm[[4]], se_random =se_power_lmm[[4]])
add_label(x=-0.125,i = 10, y = 0.87)

legend( "topright",legend=c("Fixed Effect", "Random Effect"),
        col=c("red", "blue"), lty=1, cex=1.3, bty = "n")

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



