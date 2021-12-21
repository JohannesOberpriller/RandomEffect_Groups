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
    object$results_w_glmmTMB_reml$estimate_effect - 1.96*object$results_w_glmmTMB_reml$se_effect, 
    object$results_w_glmmTMB_reml$estimate_effect + 1.96*object$results_w_glmmTMB_reml$se_effect)
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


########## _________________________________  ##########
########## Singular fit proportions  ##########
########## _________________________________  ##########

results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")
results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_inter

get_sing = function(tmp,...) mean((tmp$stddev_randeff_inter**2 < 1e-4) | (tmp$stddev_randeff_x**2 < 1e-4) , na.rm = TRUE)
round(cbind(
  sapply(results_lmm, function(l )get_sing(l$results_w_glmmTMB_reml, na.rm=TRUE)),
  sapply(results_lmm, function(l )get_sing(l$results_w_glmmTMB_ml, na.rm=TRUE)),
  sapply(results_glmm, function(l )get_sing(l$results_w_glmmTMB_reml, na.rm=TRUE)),
  sapply(results_glmm, function(l )get_sing(l$results_w_glmmTMB_ml, na.rm=TRUE))
), digits = 2)*100



########## __Figure  S1 Scenario A Slope ########## 
get_sing = function(tmp,...) (tmp$stddev_randeff_inter**2 < 1e-4)
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
pdf(file = "Figures/Fig_S1_glmmTMB.pdf", width = 11.2, height = 5.8)
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
      sapply(results_lmm, function(l) mean(l$results_wo_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si] < 0.05, na.rm=TRUE)), #glmmTMB
      sapply(results_miss, function(l) mean(l$results_wo_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)  %in% si] < 0.05, na.rm=TRUE)),
      sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)  %in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)  %in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
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
      sapply(results_lmm, function(l) get_sd(l$results_wo_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si])),
      sapply(results_miss, function(l)get_sd(l$results_wo_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si])),
      sapply(results_lmm, function(l) get_sd(l$results_wo_lm$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si2])),
      sapply(results_lmm, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si2]))
    )
  mm = type_one_int
  try({
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  }, silent = TRUE)
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
      sapply(results_lmm, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si] < 0.05, na.rm=TRUE)), #glmmTMB
      sapply(results_miss, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si] < 0.05, na.rm=TRUE)), 
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si2] < 0.05, na.rm=TRUE)) #lm
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
      sapply(results_lmm, function(l) get_sd(l$results_w_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si])),
      sapply(results_miss, function(l) get_sd(l$results_w_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si2])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si2]))
    )
  mm =1-type_two_int
  try({
  upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  }, silent = TRUE)
  
}
dev.off()






########## __Figure  S2 Scenario B Slope ########## 
threshold = 1e-4
get_sing = function(tmp,...) (tmp$stddev_randeff_inter**2 < threshold) | (tmp$stddev_randeff_x**2 < 1e-4) 
get_sing2 = function(tmp,...) (tmp$stddev_randeff_inter**2 < threshold)
pdf(file = "Figures/Fig_S2_glmmTMB.pdf", width = 11.2, height = 5.8)

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
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si]< 0.05, na.rm=TRUE)), #glmmTMB
      sapply(results_lmm, function(l)        mean(l$results_wo_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)%in% si]< 0.05, na.rm=TRUE)), #glmmTMB
      sapply(results_miss, function(l)       mean(l$results_wo_glmmTMB_reml$p_value_effect[get_sing2(l$results_wo_glmmTMB_reml)%in% si]< 0.05, na.rm=TRUE)),
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)%in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)%in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
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
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)%in% si])),
      sapply(results_lmm, function(l)        get_sd(l$results_wo_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)%in% si])),
      sapply(results_miss, function(l)       get_sd(l$results_wo_glmmTMB_reml$p_value_effect[get_sing2(l$results_wo_glmmTMB_reml)%in% si])),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si2])),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si2]))
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
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)%in% si] < 0.05, na.rm=TRUE)), #glmmTMB
      sapply(results_lmm, function(l)        1-mean(l$results_w_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)%in% si] < 0.05, na.rm=TRUE)), #glmmTMB
      sapply(results_miss, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_effect[get_sing2(l$results_wo_glmmTMB_reml)%in% si] < 0.05, na.rm=TRUE)), 
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)%in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)%in% si2] < 0.05, na.rm=TRUE)) #lm
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
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)%in% si])),
      sapply(results_lmm,        function(l) get_sd(l$results_w_glmmTMB_reml$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)%in% si])),
      sapply(results_miss,       function(l) get_sd(l$results_w_glmmTMB_reml$p_value_effect[get_sing2(l$results_wo_glmmTMB_reml)%in% si])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_effect[get_sing(l$results_wo_glmmTMB_reml) %in% si2])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_effect[get_sing(l$results_wo_glmmTMB_reml)%in% si2]))
    )
  mm =1-type_two_int
  upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  
}
dev.off()



########## __Figure  S3 Scenario A Intercept ########## 
results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")

get_sing = function(tmp,...) mean((tmp$stddev_randeff_inter**2 < 1e-4) | (tmp$stddev_randeff_x**2 < 1e-4) , na.rm = TRUE)
round(cbind(
  sapply(results_lmm, function(l )get_sing(l$results_w_glmmTMB_reml, na.rm=TRUE)),
  sapply(results_lmm, function(l )get_sing(l$results_w_glmmTMB_ml, na.rm=TRUE)),
  sapply(results_glmm, function(l )get_sing(l$results_w_glmmTMB_reml, na.rm=TRUE)),
  sapply(results_glmm, function(l )get_sing(l$results_w_glmmTMB_ml, na.rm=TRUE))
), digits = 2)*100



get_sing = function(tmp,...) (tmp$stddev_randeff_inter**2 < 1e-4)
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
pdf(file = "Figures/Fig_S3_glmmTMB.pdf", width = 11.2, height = 5.8)
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
      sapply(results_lmm, function(l) mean(l$results_wo_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si] < 0.05, na.rm=TRUE)), #glmmTMB
      sapply(results_miss, function(l) mean(l$results_wo_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)  %in% si] < 0.05, na.rm=TRUE)),
      sapply(results_lmm, function(l) mean(l$results_wo_lm$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)  %in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) mean(l$results_wo_lm_wo_grouping$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)  %in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
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
      sapply(results_lmm, function(l) get_sd(l$results_wo_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si])),
      sapply(results_miss, function(l)get_sd(l$results_wo_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si])),
      sapply(results_lmm, function(l) get_sd(l$results_wo_lm$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si2])),
      sapply(results_lmm, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si2]))
    )
  mm = type_one_int
  try({
    upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
    lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
    sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  }, silent = TRUE)
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
      sapply(results_lmm, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si] < 0.05, na.rm=TRUE)), #glmmTMB
      sapply(results_miss, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si] < 0.05, na.rm=TRUE)), 
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si2] < 0.05, na.rm=TRUE)) #lm
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
      sapply(results_lmm, function(l) get_sd(l$results_w_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si])),
      sapply(results_miss, function(l) get_sd(l$results_w_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si2])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si2]))
    )
  mm =1-type_two_int
  try({
    upper = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
    lower = sapply(1:4, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
    sapply(1:4, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  }, silent = TRUE)
  
}
dev.off()



########## __Figure  S4 Scenario B Intercept ########## 
get_sing = function(tmp,...) (tmp$stddev_randeff_inter**2 < 1e-4) | (tmp$stddev_randeff_x**2 < 1e-4) 
get_sing2 = function(tmp,...) (tmp$stddev_randeff_inter**2 < 1e-4)
pdf(file = "Figures/Fig_S4_glmmTMB.pdf", width = 11.2, height = 5.8)

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
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si]< 0.05, na.rm=TRUE)), #glmmTMB
      sapply(results_lmm, function(l)        mean(l$results_wo_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)%in% si]< 0.05, na.rm=TRUE)), #glmmTMB
      sapply(results_miss, function(l)       mean(l$results_wo_glmmTMB_reml$p_value_inter[get_sing2(l$results_wo_glmmTMB_reml)%in% si]< 0.05, na.rm=TRUE)),
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)%in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) mean(l$results_wo_lm_wo_grouping$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)%in% si2] < 0.05, na.rm=TRUE)) #lm w/go goruping
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
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)%in% si])),
      sapply(results_lmm, function(l)        get_sd(l$results_wo_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)%in% si])),
      sapply(results_miss, function(l)       get_sd(l$results_wo_glmmTMB_reml$p_value_inter[get_sing2(l$results_wo_glmmTMB_reml)%in% si])),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si2])),
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_wo_lm_wo_grouping$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si2]))
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
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)%in% si] < 0.05, na.rm=TRUE)), #glmmTMB
      sapply(results_lmm, function(l)        1-mean(l$results_w_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)%in% si] < 0.05, na.rm=TRUE)), #glmmTMB
      sapply(results_miss, function(l) 1-mean(l$results_w_glmmTMB_reml$p_value_inter[get_sing2(l$results_wo_glmmTMB_reml)%in% si] < 0.05, na.rm=TRUE)), 
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)%in% si2] < 0.05, na.rm=TRUE)), #lm
      sapply(results_lmm_no_cov, function(l) 1-mean(l$results_w_lm_wo_grouping$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)%in% si2] < 0.05, na.rm=TRUE)) #lm
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
      sapply(results_lmm_no_cov, function(l) get_sd(l$results_w_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)%in% si])),
      sapply(results_lmm,        function(l) get_sd(l$results_w_glmmTMB_reml$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)%in% si])),
      sapply(results_miss,       function(l) get_sd(l$results_w_glmmTMB_reml$p_value_inter[get_sing2(l$results_wo_glmmTMB_reml)%in% si])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm$p_value_inter[get_sing(l$results_wo_glmmTMB_reml) %in% si2])),
      sapply(results_lmm, function(l) get_sd(l$results_w_lm_wo_grouping$p_value_inter[get_sing(l$results_wo_glmmTMB_reml)%in% si2]))
    )
  mm =1-type_two_int
  upper = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm+sd)[,i], spar = 0.1)$y)
  lower = sapply(1:5, function(i) smooth.spline(x=1:7, y = (mm-sd)[,i], spar = 0.1)$y)
  sapply(1:5, function(i) polygon(c(1:7, 7:1), c(upper[,i], rev(lower[,i])),border = NA, col =addA(cols[i], 0.3)))
  
}
dev.off()


########## __Figure  S5 Scenario B Intercept ########## 
get_sing = function(tmp,...) (tmp$stddev_randeff_inter**2 < 1e-4) | (tmp$stddev_randeff_x**2 < 1e-4) 

cols = viridis::viridis(5)

results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_glmm = readRDS("Results/results_mountain_glmm_no_cov_0.1_200_unbalanced.Rds")

adj = 1.0

pdf(file = "Figures/Fig_S5_glmmTMB.pdf", width = 9, height = 9)

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
sapply(1:7, function(i) plot_mean(results_lmm[[ i]]$results_w_glmmTMB_reml$stddev_randeff_x**2, i))

### Without SI

bb= boxplot(results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_inter[get_sing(results_lmm[[1]]$results_w_glmmTMB_reml)== 0]**2, 
            results_lmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_inter[get_sing(results_lmm[[2]]$results_w_glmmTMB_reml)== 0]**2, 
            results_lmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_inter[get_sing(results_lmm[[2]]$results_w_glmmTMB_reml)== 0]**2, 
            results_lmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_inter[get_sing(results_lmm[[2]]$results_w_glmmTMB_reml)== 0]**2, 
            results_lmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_inter[get_sing(results_lmm[[2]]$results_w_glmmTMB_reml)== 0]**2, 
            results_lmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_inter[get_sing(results_lmm[[4]]$results_w_glmmTMB_reml)== 0]**2,
            results_lmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_inter[get_sing(results_lmm[[7]]$results_w_glmmTMB_reml)== 0]**2, ylim = c(0, 0.13), outline=FALSE, las = 1,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)
legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.1365, pos = 3, labels = letters[3], xpd= NA, cex = 1.3, font = 2)

abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i ]]$results_w_glmmTMB_reml$stddev_randeff_inter[results_lmm[[i]]$results_w_glmmTMB_reml$Singularity == 0]**2, i))

bb= boxplot(results_lmm[[1]]$results_w_glmmTMB_reml$stddev_randeff_x[get_sing(results_lmm[[1]]$results_w_glmmTMB_reml)== 0]**2, 
            results_lmm[[2]]$results_w_glmmTMB_reml$stddev_randeff_x[get_sing(results_lmm[[2]]$results_w_glmmTMB_reml)== 0]**2, 
            results_lmm[[3]]$results_w_glmmTMB_reml$stddev_randeff_x[get_sing(results_lmm[[2]]$results_w_glmmTMB_reml)== 0]**2, 
            results_lmm[[4]]$results_w_glmmTMB_reml$stddev_randeff_x[get_sing(results_lmm[[2]]$results_w_glmmTMB_reml)== 0]**2, 
            results_lmm[[5]]$results_w_glmmTMB_reml$stddev_randeff_x[get_sing(results_lmm[[2]]$results_w_glmmTMB_reml)== 0]**2, 
            results_lmm[[6]]$results_w_glmmTMB_reml$stddev_randeff_x[get_sing(results_lmm[[4]]$results_w_glmmTMB_reml)== 0]**2,
            results_lmm[[7]]$results_w_glmmTMB_reml$stddev_randeff_x[get_sing(results_lmm[[7]]$results_w_glmmTMB_reml)== 0]**2, ylim = c(0, 0.13), outline=FALSE,las = 1,
            names = 2:8, xlab = "Number of mountains", ylab = "Variance", xpd = NA)
legend("topright", bty = "n", legend = "mean", col = "red", lty = 1)
text(x = -0.1, y = 0.1365, pos = 3, labels = letters[4], xpd= NA, cex = 1.3, font = 2)

abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)
sapply(1:7, function(i) plot_mean(results_lmm[[ i ]]$results_w_glmmTMB_reml$stddev_randeff_x[results_lmm[[i]]$results_w_glmmTMB_reml$Singularity == 0]**2, i))

dev.off()





########## __Figure S6 Variance estimates lmm MLE/REML ########## 
results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")

pdf(file = "Figures/Fig_S6_glmmTMB.pdf", width = 9, height = 5)
par(mfrow = c(1,2), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
lty = 2
lwd = 1.4
at = as.vector(t(matrix(1:21, ncol = 3L, byrow = TRUE)[,-3]))
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
text(x = -0.1, y = 0.084, pos = 3, labels = letters[1], xpd= NA, cex = 1.3, font = 2)
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
text(x = -0.1, y = 0.084, pos = 3, labels = letters[2], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)

dev.off()












########## __Figure S7 Variance estimates lmm REML imbalanced vs REML balanced ########## 
cols = viridis::viridis(5)

results_lmm_im = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_.Rds")


pdf(file = "Figures/Fig_S7_glmmTMB.pdf", width = 9, height = 5)
par(mfrow = c(1,2), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
lty = 2
lwd = 1.4
at = as.vector(t(matrix(1:21, ncol = 3L, byrow = TRUE)[,-3]))
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
text(x = -0.1, y = 0.084, pos = 3, labels = letters[1], xpd= NA, cex = 1.3, font = 2)
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
text(x = -0.1, y = 0.084, pos = 3, labels = letters[2], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)



dev.off()




########## __Figure S8 Variance estimates lmm MLE imbalanced vs MLE balanced ########## 
cols = viridis::viridis(5)

results_lmm_im = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_unbalanced.Rds")
results_lmm = readRDS("Results/results_mountain_lmm_no_cov_0.1_50_.Rds")

pdf(file = "Figures/Fig_S8_glmmTMB.pdf", width = 9, height = 5)
par(mfrow = c(1,2), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
lty = 2
lwd = 1.4
at = as.vector(t(matrix(1:21, ncol = 3L, byrow = TRUE)[,-3]))
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
text(x = -0.1, y = 0.084, pos = 3, labels = letters[1], xpd= NA, cex = 1.3, font = 2)
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
text(x = -0.1, y = 0.084, pos = 3, labels = letters[2], xpd= NA, cex = 1.3, font = 2)
abline(h = 0.01, col = "blue", lty = lty, lwd = lwd)


dev.off()



