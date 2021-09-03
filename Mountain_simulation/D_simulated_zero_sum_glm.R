

simulate_lm_re = function(groups = c(20, 20, 20),  sd_re = 0.5) {
  g = as.factor(unlist(sapply(1:length(groups), function(i) rep(i, groups[i]))))
  x = runif(sum(groups), -1, 1)
  X <- matrix(c(rep(1, sum(groups)), x), nrow = sum(groups))
  beta = 0.0  
  beta0 = 0.0 
  randintercep <- rnorm(length(groups), mean = beta0, sd = sd_re)
  randslope <- rnorm(length(groups), mean = beta, sd = sd_re)
  mu <- sapply(1:sum(groups), FUN = function(j) X[j,] %*% c(randintercep[g[j]],randslope[g[j]]))
  sigma <- 0.3
  y <- rbinom(sum(groups), 1, 1/(1+exp(-mu)))
  fit_lm = glm(y ~ x*g, contrasts = list(g = contr.sum), family = binomial)
  
  res = list(eff = coef(fit_lm)["x"], p = summary(fit_lm)$coef["x",4])
  return(res)
}


mountains = c(1, 2, 5, 10, 50, 100)
sd_re = c(0.1, 0.5, 1.0)
unbalanced = c(TRUE, FALSE)
parameter = expand.grid(mountains, sd_re, unbalanced)
colnames(parameter) = c("mountains", "sd_re","unbalanced")

cl = snow::makeCluster(36L)
snow::clusterExport(cl, list("parameter", "simulate_lm_re"), envir = environment())

results = 
  snow::parLapply(cl, 1:nrow(parameter), function(i) { 
    pars = parameter[i,]
    last = 30
    if(pars$unbalanced) last = 1000
    results = 
      sapply(1:10000, function(k) {
        res = simulate_lm_re(c(rep(30, pars$mountains), last), sd_re = pars$sd_re)
        return(res)
      })
    res = t(results)
    
    return(list(pars = pars, result = res))
    
    })
saveRDS(results, "Results/boot_zero_sum_glm.RDS")

