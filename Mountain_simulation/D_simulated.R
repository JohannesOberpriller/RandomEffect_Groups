

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
  return(fit_lm)
}


mountains = c(1, 2, 5, 10, 50, 100)
sd_re = c(0.1, 0.5, 1.0)
unbalanced = c(TRUE, FALSE)
t = c(TRUE, FALSE)
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
        sim = simulate_lm_re(c(rep(30, pars$mountains), last), group_center = pars$center, sd_re = pars$sd_re)
        return(grand_mean(sim, length(c(rep(30, pars$mountains), last)), beta=0, t = pars$t, intercept = FALSE))
      })
    res = t(results)
    
    return(list(pars = pars, result = res))
    
    })
saveRDS(results, "Results/boot.RDS")

