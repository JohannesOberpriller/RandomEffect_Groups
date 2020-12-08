# Simulation --------------------------------------------------------------
# y_i = a + (β_g)*x_i + ε_i
# with groups g and observations i
library(lme4)
library(lmerTest)
library(snow)
set.seed(1)

extract_results_t = function(fit_lmm, confs, beta, beta0) {
  return(
    c(summary(fit_lmm)$coefficients["(Intercept)","Estimate"],
      summary(fit_lmm)$coefficients["x","Estimate"],
      summary(fit_lmm)$coefficients["(Intercept)","Pr(>|t|)"],
      summary(fit_lmm)$coefficients["x","Pr(>|t|)"],
      summary(fit_lmm)$coefficients["(Intercept)","Std. Error"],
      summary(fit_lmm)$coefficients["x","Std. Error"],
      attr(summary(fit_lmm)$varcor$group, "stddev"),
      !is.null(fit_lmm@optinfo$conv$lme4$messages),
      as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2]),
      as.integer(beta > confs["x",1] & beta < confs["x",2])))
}

extract_results_z = function(fit_lmm, confs, beta, beta0) {
  return(
    c(summary(fit_lmm)$coefficients["(Intercept)","Estimate"],
      summary(fit_lmm)$coefficients["x","Estimate"],
      summary(fit_lmm)$coefficients["(Intercept)","Pr(>|z|)"],
      summary(fit_lmm)$coefficients["x","Pr(>|z|)"],
      summary(fit_lmm)$coefficients["(Intercept)","Std. Error"],
      summary(fit_lmm)$coefficients["x","Std. Error"],
      attr(summary(fit_lmm)$varcor$group, "stddev"),
      !is.null(fit_lmm@optinfo$conv$lme4$messages),
      as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2]),
      as.integer(beta > confs["x",1] & beta < confs["x",2])))
}

inv.logit <- function(p){return(exp(p)/(1 + exp(p)))}

cl = snow::makeCluster(7L)
snow::clusterEvalQ(cl, {library(lme4); library(lmerTest); number_experiments =5000})
snow::clusterExport(cl, list("extract_results_z","extract_results_t", "inv.logit"), envir = environment())
#### NORMAL Random intercept ---------------------------------------------

system.time({
result_list = 
  snow::parLapply(cl, 2:8, function(number_groups)  {
    n_each <- 20
    n_groups <- number_groups
    
    results_w = results_wo = matrix(nrow = number_experiments, ncol = 10)
    colnames(results_w) = c("estimate_intercept","estimate_effect", 
                          "p_value_intercept", "p_value_effect",
                          "se_intercept", "se_effect",
                          "stddev_randeff", "Singularity",
                          "Int_in_conf",  "Slope_in_conf")
    colnames(results_wo) = colnames(results_w)
    
    ## The data generating process ### 
    n = (number_groups)*n_each
    
    x <- runif(n, -1, 1)
    X <- matrix(c(rep(1, n), x), nrow = n) # 
    sd_randeff = 0.1
    
    
    for(experiment in 1:number_experiments){
      # w effect
      beta = 0.3
      beta0 = 0.3
      g <- rep(1:n_groups, n_each)
      group <-  as.factor(g)
      randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
      
      mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta)) 
      sigma <- 0.5
      y <- rnorm(n, mu, sd = sigma) 
      
      fit_lmm <- lmer(y ~ x  + (1 | group)) # only beta[c(1,2)] are also random
      confs = confint(fit_lmm)
      results_w[experiment, ] = extract_results_t(fit_lmm, confs, beta, beta0)
      
      # w/o effect
      beta0 = 0.0
      beta = 0.0
      g <- rep(1:n_groups, n_each)
      group <-  as.factor(g)
      randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
      
      mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta)) 
      sigma <- 0.5
      y <- rnorm(n, mu, sd = sigma) 
      
      fit_lmm <- lmer(y ~ x  + (1 | group)) # only beta[c(1,2)] are also random
      confs = confint(fit_lmm)
      results_wo[experiment, ] = extract_results_t(fit_lmm, confs, beta, beta0)
    
    }
    
    return(list(results_w = data.frame(results_w), 
                results_wo = data.frame(results_wo)))
  })
})
saveRDS(result_list, file = "Results/results_intercept_lme4.Rds")


#### NORMAL Random slope -----------------------------------------------

n_each <- 20

system.time({
  result_list_slope = 
    snow::parLapply(cl, 2:8, function(number_groups)  {
      n_each <- 20
      n_groups <- number_groups
      
      results_w = results_wo = matrix(nrow = number_experiments, ncol = 10)
      colnames(results_w) = c("estimate_intercept","estimate_effect", 
                              "p_value_intercept", "p_value_effect",
                              "se_intercept", "se_effect",
                              "stddev_randeff", "Singularity",
                              "Int_in_conf",  "Slope_in_conf")
      colnames(results_wo) = colnames(results_w)
      ## The data generating process ### 
      n = (number_groups)*n_each
      
      x <- runif(n, -1, 1)
      X <- matrix(c(rep(1, n), x), nrow = n) # 
      sd_randeff = 0.1
      
      
      for(experiment in 1:number_experiments){
        # w effect
        beta = 0.3
        beta0 = 0.3
        g <- rep(1:n_groups, n_each)
        group <-  as.factor(g)
        randslope <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta0,randslope[g[i]])) 
        sigma <- 0.5
        y <- rnorm(n, mu, sd = sigma) 
        
        fit_lmm <- lmer(y ~ x  + (0 + x | group))  # only beta[c(1,2)] are also random
        confs = confint(fit_lmm)
        results_w[experiment, ] = extract_results_t(fit_lmm, confs, beta, beta0)
        
        # w/o effect
        beta0 = 0.0
        beta = 0.0
        g <- rep(1:n_groups, n_each)
        group <-  as.factor(g)
        randslope <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta0,randslope[g[i]])) 
        sigma <- 0.5
        y <- rnorm(n, mu, sd = sigma) 
        
        fit_lmm <- lmer(y ~ x  + (0 + x | group))  # only beta[c(1,2)] are also random
        confs = confint(fit_lmm)
        results_wo[experiment, ] = extract_results_t(fit_lmm, confs, beta, beta0)
        
      }
      
      return(list(results_w = data.frame(results_w), 
                  results_wo = data.frame(results_wo)))
    })
})
saveRDS(result_list_slope, file = "Results/results_slope_lme4.Rds")


#### Binomial Random intercept -----------------------------------------------
system.time({
  result_list = 
    snow::parLapply(cl, 2:8, function(number_groups)  {
      n_each <- 40
      n_groups <- number_groups
      
      results_w = results_wo = matrix(nrow = number_experiments, ncol = 10)
      colnames(results_w) = c("estimate_intercept","estimate_effect", 
                              "p_value_intercept", "p_value_effect",
                              "se_intercept", "se_effect",
                              "stddev_randeff", "Singularity",
                              "Int_in_conf",  "Slope_in_conf")
      colnames(results_wo) = colnames(results_w)
      
      ## The data generating process ### 
      n = (number_groups)*n_each
      
      x <- runif(n, -1, 1)
      X <- matrix(c(rep(1, n), x), nrow = n) # 
      sd_randeff = 0.1
      for(experiment in 1:number_experiments){
        # w effect
        beta = 0.3
        beta0 = 0.3
        g <- rep(1:n_groups, n_each)
        group <-  as.factor(g)
        randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta)) 
        y <- rbinom(n, size = 1, prob = inv.logit(mu))
        
        fit_lmm <- glmer(y ~ x  + (1 | group), family=binomial) # only beta[c(1,2)] are also random
        
        summ = summary(fit_lmm)
        confs = cbind(summ$coefficients[,"Estimate"] - 1.96*summ$coefficients[,"Std. Error"], summ$coefficients[,"Estimate"] + 1.96*summ$coefficients[,"Std. Error"])
        rownames(confs) = rownames(summ$coefficients)
        
        
        results_w[experiment, ] = extract_results_z(fit_lmm, confs, beta, beta0)
        
        # w/o effect        
        beta0 = 0.0
        beta = 0.0
        g <- rep(1:n_groups, n_each)
        group <-  as.factor(g)
        randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta)) 
        y <- rbinom(n, size = 1, prob = inv.logit(mu))
        
        fit_lmm <- glmer(y ~ x  + (1 | group), family=binomial) # only beta[c(1,2)] are also random
        summ = summary(fit_lmm)
        confs = cbind(summ$coefficients[,"Estimate"] - 1.96*summ$coefficients[,"Std. Error"], summ$coefficients[,"Estimate"] + 1.96*summ$coefficients[,"Std. Error"])
        rownames(confs) = rownames(summ$coefficients)
        results_wo[experiment, ] = extract_results_z(fit_lmm, confs, beta, beta0)
        
      }
      
      return(list(results_w = data.frame(results_w), 
                  results_wo = data.frame(results_wo)))
    })
})
saveRDS(result_list, file = "Results/results_intercept_glmer.Rds")

#### Binomial Random slope -----------------------------------------------

system.time({
  result_list_slope = 
    snow::parLapply(cl, 2:8, function(number_groups)  {
      n_each <- 40
      n_groups <- number_groups
      
      results_w = results_wo = matrix(nrow = number_experiments, ncol = 10)
      colnames(results_w) = c("estimate_intercept","estimate_effect", 
                              "p_value_intercept", "p_value_effect",
                              "se_intercept", "se_effect",
                              "stddev_randeff", "Singularity",
                              "Int_in_conf",  "Slope_in_conf")
      colnames(results_wo) = colnames(results_w)
      
      ## The data generating process ### 
      n = (number_groups)*n_each
      x <- runif(n, -1, 1)
      X <- matrix(c(rep(1, n), x), nrow = n) # 
      sd_randeff = 0.1
      for(experiment in 1:number_experiments){
        beta = 0.3
        beta0 = 0.3
        
        g <- rep(1:n_groups, n_each)
        group <-  as.factor(g)
        randslope <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta0,randslope[g[i]])) 
        y <- rbinom(n, 1, prob = inv.logit(mu)) 
        
        fit_lmm <- glmer(y ~ x  + (0 + x | group), family = binomial)  # only beta[c(1,2)] are also random
        summ = summary(fit_lmm)
        confs = cbind(summ$coefficients[,"Estimate"] - 1.96*summ$coefficients[,"Std. Error"], summ$coefficients[,"Estimate"] + 1.96*summ$coefficients[,"Std. Error"])
        rownames(confs) = rownames(summ$coefficients)
        results_w[experiment, ] = extract_results_z(fit_lmm, confs, beta, beta0)
        
        # w/o effect
        beta0 = 0.0
        beta = 0.0
        g <- rep(1:n_groups, n_each)
        group <-  as.factor(g)
        randslope <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta0,randslope[g[i]])) 
        y <- rbinom(n, 1, prob = inv.logit(mu)) 
        
        fit_lmm <- glmer(y ~ x  + (0 + x | group), family = binomial)  # only beta[c(1,2)] are also random
        summ = summary(fit_lmm)
        confs = cbind(summ$coefficients[,"Estimate"] - 1.96*summ$coefficients[,"Std. Error"], summ$coefficients[,"Estimate"] + 1.96*summ$coefficients[,"Std. Error"])
        rownames(confs) = rownames(summ$coefficients)
        results_wo[experiment, ] = extract_results_z(fit_lmm, confs, beta, beta0)
        
      }
      
      return(list(results_w = data.frame(results_w), 
                  results_wo = data.frame(results_wo)))
    })
})
saveRDS(result_list_slope, file = "Results/results_slope_glmer.Rds")

