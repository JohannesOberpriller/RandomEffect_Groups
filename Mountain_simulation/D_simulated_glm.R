

simulate_lm_re = function(groups = c(20, 20, 20), group_center = TRUE, sd_re = 0.5) {
  g = as.factor(unlist(sapply(1:length(groups), function(i) rep(i, groups[i]))))
  x = runif(sum(groups), -1, 1)
  X <- matrix(c(rep(1, sum(groups)), x), nrow = sum(groups))
  beta = 0.0  
  beta0 = 0.0 
  randintercep <- rnorm(length(groups), mean = beta0, sd = sd_re)
  randslope <- rnorm(length(groups), mean = beta, sd = sd_re)
  mu <- sapply(1:sum(groups), FUN = function(j) X[j,] %*% c(randintercep[g[j]], randslope[g[j]]))

  y <- rbinom(sum(groups), 1, 1/(1+exp(-mu)))
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
  fit_lm = glm(y ~ 0+id+iv.cwc:id, data = d, family = binomial)
  } else { fit_lm = glm(y ~ 0+x:g + g, family = binomial) }
  return(fit_lm)
}


mountains = c(1, 2, 5, 10, 50, 100)
sd_re = c(0.1, 0.5, 1.0)
unbalanced = c(TRUE, FALSE)
t = c(FALSE)
center = c(TRUE, FALSE)
parameter = expand.grid(mountains, sd_re, unbalanced, t, center)
colnames(parameter) = c("mountains", "sd_re","unbalanced","t","center")
source("Mountain_simulation/grand_mean_function.R")

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
        r = rep(NA, 4)
        try({
          sim = simulate_lm_re(c(rep(30, pars$mountains), last), group_center = pars$center, sd_re = pars$sd_re)
          r = grand_mean(sim, length(c(rep(30, pars$mountains), last)), beta=0, t = FALSE, intercept = FALSE)
        }, silent = TRUE)
        #print(r)
        return(r)
      })
    res = t(results)
    
    return(list(pars = pars, result = res))
    
    })


saveRDS(results, "Results/boot_glm.RDS")

# results
# parameter
# #tt = c(1:8, 17:24)
# tt = 17:32
# par(mfrow = c(4, 4))
# for(i in tt) hist(results[[i]]$result[,2])
# 
# sim = simulate_lm_re(c(rep(30, pars$mountains), last), group_center = pars$center, sd_re = pars$sd_re)
# k = profile(sim, which = c("x:g1", "x:g2"))
# plot(k)
# pairs(k)
# 
# prof = k
# disp <- attr(prof,"summary")$dispersion
# mindev <- attr(prof,"original.fit")$deviance
# 
# dev1 <- prof[[2]]$z^2
# dev2 <- dev1*disp+mindev
# plot(prof[[2]][,1],dev2,type="b")
# 
# install.packages("profileModel")
# 
# library(profileModel)
# 
# groups = c(rep(30, 15), 30)+120
# sd_intercept = 0.1
# sd_slope = 0.1
# g = as.factor(unlist(sapply(1:length(groups), function(i) rep(i, groups[i]))))
# x = runif(sum(groups), -1, 1)
# X <- matrix(c(rep(1, sum(groups)), x), nrow = sum(groups))
# beta = 0.2
# beta0 = 0.4
# randintercep <- rnorm(length(groups), mean = beta0, sd = sd_intercept)
# randslope <- rnorm(length(groups), mean = beta, sd = sd_slope)
# mu <- sapply(1:sum(groups), FUN = function(j) X[j,] %*% c(randintercep[g[j]], randslope[g[j]]))
# 
# 
# gl <- rbinom(sum(groups), 1, 1/(1+exp(-mu)))
# #gl <- rbinom(sum(groups), 1, pnorm(mu))
# ll = rnorm(sum(groups), mu, 0.3)
# 
# m1 = glm(gl ~ 0+x:g + g, family = binomial("logit"), contrasts = list(g=contr.sum))
# sum(coef(m1)[17:32])
# m2 = glm(ll ~ 0+x:g + g, family = gaussian)
# 
# library(lme4)
# m2 = glmmTMB::glmmTMB(ll ~ X[,2]+(1|g) + (0+X[,2]|g),  contrasts = contr.treatment)
# m3 = glmer(gl ~ X[,2]+(1|g) + (0+X[,2]|g) , family = binomial)
# 
# colSums(ranef(m2)$cond$g)
# colSums(ranef(m3)$g)
# 
# p1= profileModel(m1,  objective = "ordinaryDeviance")
# p2= profileModel(m2,  objective = "ordinaryDeviance")
# plot(p1)
# plot(p2)
