

simulate_lm_re = function(groups = c(20, 20, 20), group_center = TRUE, sd_re = 0.5) {
  g = as.factor(unlist(sapply(1:length(groups), function(i) rep(i, groups[i]))))
  x = runif(sum(groups), -1, 1)
  X <- matrix(c(rep(1, sum(groups)), x), nrow = sum(groups))
  beta = 0.0  
  beta0 = 0.0 
  randintercep <- rnorm(length(groups), mean = beta0, sd = sd_re)
  randslope <- rnorm(length(groups), mean = beta, sd = sd_re)
  mu <- sapply(1:sum(groups), FUN = function(j) X[j,] %*% c(randintercep[g[j]],randslope[g[j]]))
  sigma <- 0.3
  y <- rnorm(sum(groups), mu, sd = sigma)
  if(group_center) {
  data = data.frame(iv = X[,2], id = g)
  d <- data %>% 
    mutate(iv.gmc = iv-mean(iv)) %>%
    group_by(id) %>% 
    mutate(iv.cm = mean(iv),
           iv.cwc = iv-iv.cm) %>%
    ungroup %>%
    mutate(iv.cmc = iv.cm-mean(iv.cm))
  d$y = y  
  fit_lm = lm(y ~ 0+id+iv.cwc:id, data = d)
  } else { fit_lm = lm(y ~ 0+x:g + g) }
  ind = (length(groups)+1):(2*length(groups))
  sim = list(means = coef(fit_lm)[ind], V = vcov(fit_lm)[ind, ind])
  return(sim)
}


mountains = c(1, 2, 5, 10, 50, 100)
sd_re = c(0.1, 0.5, 1.0)
unbalanced = c(TRUE, FALSE)
t = c(TRUE, FALSE)
center = c(TRUE, FALSE)
parameter = expand.grid(mountains, sd_re, unbalanced, t, center)
colnames(parameter) = c("mountains", "sd_re","unbalanced","t","center")


cl = snow::makeCluster(36L)
snow::clusterEvalQ(cl, {library(lme4); library(lmerTest); library(tidyverse)})
snow::clusterExport(cl, list("grand_mean", "parameter", "simulate_lm_re"), envir = environment())

results = 
  snow::parLapply(cl, 1:nrow(parameter), function(i) { 
    pars = parameter[i,]
    last = 30
    if(pars$unbalanced) last = 1000
    results = 
      sapply(1:10000, function(k) {
        sim = simulate_lm_re(c(rep(30, pars$mountains), last), group_center = pars$center, sd_re = pars$sd_re)
        return(grand_mean(sim$means, sim$V, 0, t = pars$t))
      })
    res = t(results)
    
    return(list(pars = pars, result = res))
    
    })
saveRDS(results, "boot.RDS")

results[[40]]$pars


# t-test, centered, sd = 0.1
vars = as.integer(rownames(parameter[parameter$sd_re=="0.1" & parameter$t & parameter$center & parameter$unbalanced,]))

# 1:6 unbalanced
# 7:12 balanced
par(mfrow = c(6, 6), mar = c(1, 2, 1, 1), oma = c(5, 2, 2, 1))
mounts = c(1, 2, 5, 10, 50, 100) +1
ylab = c("0.1", "0.5", "1.0")
counter = 1
for(sd in c(0.1, 0.5, 1.0)) {
  vars = as.integer(rownames(parameter[parameter$sd_re==sd & parameter$t & parameter$center & parameter$unbalanced,]))
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
  vars = as.integer(rownames(parameter[parameter$sd_re==sd & parameter$t & parameter$center & !parameter$unbalanced,]))
  yy = paste0("SD: ", ylab[counter])
  counter = counter + 1
  for(i in 1:6) {
    if(i > 1) yy = ""
    hist((results[[vars[i]]][[2]][,2]), main = "", xlab = "", col = "red", ylab = yy, xpd = NA)
  }
}
legend(-0.5, y = -200, xpd = NA, legend = c("unbalanced", "balanced"), horiz = TRUE, pch = 15, col = c("blue", "red"))
text(-3.5, y = -200, xpd = NA, labels = "t-test + centered groups", pos = 1, font = 2)







# z-stat
par(mfrow = c(6, 6), mar = c(1, 2, 1, 1), oma = c(5, 2, 2, 1))
mounts = c(1, 2, 5, 10, 50, 100) +1
ylab = c("0.1", "0.5", "1.0")
counter = 1
for(sd in c(0.1, 0.5, 1.0)) {
  vars = as.integer(rownames(parameter[parameter$sd_re==sd & !parameter$t & parameter$center & parameter$unbalanced,]))
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
  vars = as.integer(rownames(parameter[parameter$sd_re==sd & !parameter$t & parameter$center & !parameter$unbalanced,]))
  yy = paste0("SD: ", ylab[counter])
  counter = counter + 1
  for(i in 1:6) {
    if(i > 1) yy = ""
    hist((results[[vars[i]]][[2]][,2]), main = "", xlab = "", col = "red", ylab = yy, xpd = NA)
  }
}
legend(-0.5, y = -200, xpd = NA, legend = c("unbalanced", "balanced"), horiz = TRUE, pch = 15, col = c("blue", "red"))
text(-3.5, y = -200, xpd = NA, labels = "z-stat + centered groups", pos = 1, font = 2)



# t+ non-centered
par(mfrow = c(6, 6), mar = c(1, 2, 1, 1), oma = c(5, 2, 2, 1))
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
legend(-0.5, y = -200, xpd = NA, legend = c("unbalanced", "balanced"), horiz = TRUE, pch = 15, col = c("blue", "red"))
text(-3.5, y = -200, xpd = NA, labels = "t-test + non-centered groups", pos = 1, font = 2)




