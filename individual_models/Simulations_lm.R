# Simulation --------------------------------------------------------------
# y_i = a + (β_g)*x_i + ε_i
# with groups g and observations i
library(snow)
set.seed(1)

cl = snow::makeCluster(7L)
snow::clusterEvalQ(cl, { number_experiments = 10000})

#### NORMAL Random intercept ---------------------------------------------

n_each <- 20

system.time({
result_list = 
  snow::parLapply(cl, 2:8, function(number_groups)  {
    n_each <- 20
    n_groups <- number_groups
    results_w=results_wo = matrix(nrow = number_experiments, ncol =7)
    colnames(results_w) = c("estimate_intercept","estimate_effect", 
                          "p_value_intercept", "p_value_effect", "Slope_in_conf", "coverage_rate", "p_value_rate")
    
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
      
      fit_lm = lm(y ~ x  + group-1)
      confs = confint(fit_lm)
      fit_summary = summary(fit_lm)$coefficients[-which("x" == rownames(summary(fit_lm)$coefficients)),]
      
      se_mean = (fit_summary[,"Std. Error"])
      est_mean = (fit_summary[,"Estimate"])
      estimates = MuMIn::par.avg(est_mean, se_mean, weight = rep(1, length(se_mean)))
      
      df = max(summary(fit_lm)$df)
      t_value = estimates[1] / estimates[2]
      p_value = 2*stats::pt(abs(t_value),lower.tail = FALSE, df = df)
      conf_int=c(estimates[1]-qt(0.975, df)*estimates[2], estimates[1]+qt(0.975, df)*estimates[2])
      
      results_w[experiment, 1]= estimates[1]
      results_w[experiment, 2]= summary(fit_lm)$coefficients["x","Estimate"]
      results_w[experiment, 3]= p_value
      results_w[experiment, 4]= summary(fit_lm)$coefficients["x","Pr(>|t|)"]
      results_w[experiment, 5]= as.integer(beta > confs["x",1] & beta < confs["x",2])
      results_w[experiment, 6]= beta0 > conf_int[1] & beta0 < conf_int[1] 
      #results_w[experiment, 7]= mean(fit_summary[,"Pr(>|t|)"] < 0.05)
   
      # w/o effect
      beta0 = 0.0
      beta = 0.0
      
      g <- rep(1:n_groups, n_each)
      group <-  as.factor(g)
      randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
      randintercep = rep(0, 2)
      
      mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta)) 
      sigma <- 0.5
      y <- rnorm(n, mu, sd = sigma) 

      
      fit_lm = lm(y ~ x  + group-1)
      confs = confint(fit_lm)
      fit_summary = summary(fit_lm)$coefficients[-which("x" == rownames(summary(fit_lm)$coefficients)),]
      
      se_mean = (fit_summary[,"Std. Error"])
      est_mean = (fit_summary[,"Estimate"])
      estimates = MuMIn::par.avg(est_mean, se_mean, weight = rep(1, length(se_mean)))
      
      df = max(summary(fit_lm)$df)
      t_value = estimates[1] / estimates[2]
      p_value = 2*stats::pt(abs(t_value),lower.tail = FALSE, df = df)
      conf_int=c(estimates[1]-qt(0.975, df)*estimates[2], estimates[1]+qt(0.975, df)*estimates[2])
      
      results_wo[experiment, 1]= estimates[1]
      results_wo[experiment, 2]= summary(fit_lm)$coefficients["x","Estimate"]
      results_wo[experiment, 3]= p_value
      results_wo[experiment, 4]= summary(fit_lm)$coefficients["x","Pr(>|t|)"]
      results_wo[experiment, 5]= as.integer(beta > confs["x",1] & beta < confs["x",2])
      results_wo[experiment, 6]= beta0 > conf_int[1] & beta0 < conf_int[1] 
    
    }
    
    return(list(results_w = data.frame(results_w), 
                results_wo = data.frame(results_wo)))
  })
})
saveRDS(result_list, file = "Results/results_intercept_lm.Rds")


#### NORMAL Random slope -----------------------------------------------

n_each <- 20

system.time({
  result_list_slope = 
    snow::parLapply(cl, 2:8, function(number_groups)  {
      n_each <- 20
      n_groups <- number_groups
      
      results_w=results_wo= matrix(nrow = number_experiments, ncol =7)
      colnames(results_w) = c("estimate_intercept","estimate_effect", 
                                 "p_value_intercept", "p_value_effect", "Intercept_in_conf", "coverage_rate", "p_value_rate")
      
      colnames(results_wo) = colnames(results_w)
      
      ## The data generating process ### 
      n = (number_groups)*n_each
      
      x <- runif(n, -1, 1)
      X <- matrix(c(rep(1, n), x), nrow = n) # 
      sd_randeff = 0.1
      
      
      for(experiment in 1:number_experiments){
        # effect
        beta = 0.3
        beta0 = 0.3
        g <- rep(1:n_groups, n_each)
        group <-  as.factor(g)
        randslope <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta0,randslope[g[i]])) 
        sigma <- 0.5
        y <- rnorm(n, mu, sd = sigma) 
        
        fit_lm = lm(y ~ x:group+x)
        confs = confint(fit_lm)
        # results_w[experiment,1] = summary(fit_lm)$coefficients["(Intercept)","Estimate"]
        results_w[experiment,1] = summary(fit_lm)$coefficients["x", "Pr(>|t|)"]
        # results_w[experiment,3] = summary(fit_lm)$coefficients["(Intercept)","Pr(>|t|)"]
        # results_w[experiment,5] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
        # fit_summary = summary(fit_lm)$coefficients[-which("(Intercept)" == rownames(summary(fit_lm)$coefficients)),]
        # confs_wo = confs[-which(rownames(confs) == "(Intercept)"),]
        results_w[experiment,6] = beta > confs["x",1] & beta < confs["x",2]
        # results_w[experiment,7] = mean(fit_summary[,"Pr(>|t|)"] < 0.05)
        
        # w/o effect        
        beta0 = 0.0
        beta = 0.0
        
        g <- rep(1:n_groups, n_each)
        group <-  as.factor(g)
        randslope <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta0,randslope[g[i]])) 
        
        ## The normal model
        sigma <- 0.5
        y <- rnorm(n, mu, sd = sigma) 
        
        fit_lm = lm(y ~ x:group+x)
        confs = confint(fit_lm)
        # results_w[experiment,1] = summary(fit_lm)$coefficients["(Intercept)","Estimate"]
        results_wo[experiment,1] = summary(fit_lm)$coefficients["x", "Pr(>|t|)"]
        # results_w[experiment,3] = summary(fit_lm)$coefficients["(Intercept)","Pr(>|t|)"]
        # results_w[experiment,5] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
        # fit_summary = summary(fit_lm)$coefficients[-which("(Intercept)" == rownames(summary(fit_lm)$coefficients)),]
        # confs_wo = confs[-which(rownames(confs) == "(Intercept)"),]
        results_wo[experiment,6] = beta > confs["x",1] & beta < confs["x",2]
        # 
      }
      
      return(list(results_w = data.frame(results_w), 
                  results_wo = data.frame(results_wo)))
    })
})
saveRDS(result_list_slope, file = "Results/results_slope_lm.Rds")


#### Binomial Random intercept -----------------------------------------------

inv.logit <- function(p){return(exp(p)/(1 + exp(p)))}


system.time({
  result_list = 
    snow::parLapply(cl, 2:8, function(number_groups)  {
      n_each <- 40
      n_groups <- number_groups

      results_w=results_wo = matrix(nrow = number_experiments, ncol =7)
      colnames(results_w) = c("estimate_intercept","estimate_effect", 
                                 "p_value_intercept", "p_value_effect", "Slope_in_conf", "coverage_rate", "p_value_rate")
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
  
        fit_lm = glm(y ~ x  + group - 1, family = binomial)
        confs = confint(fit_lm)
        fit_summary = summary(fit_lm)$coefficients[-which("x" == rownames(summary(fit_lm)$coefficients)),]
        
        se_mean = (fit_summary[,"Std. Error"])
        est_mean = (fit_summary[,"Estimate"])
        estimates = MuMIn::par.avg(est_mean, se_mean, weight = rep(1, length(se_mean)))
        
        z_value = estimates[1] / estimates[2]
        p_value = 2*stats::pnorm(abs(z_value),lower.tail = FALSE)
        conf_int=c(estimates[1]-1.96*estimates[2], estimates[1]+1.96*estimates[2])
        
        results_w[experiment, 1]= estimates[1]
        results_w[experiment, 2]= summary(fit_lm)$coefficients["x","Estimate"]
        results_w[experiment, 3]= p_value
        results_w[experiment, 4]= summary(fit_lm)$coefficients["x","Pr(>|z|)"]
        results_w[experiment, 5]= as.integer(beta > confs["x",1] & beta < confs["x",2])
        results_w[experiment, 6]= beta0 > conf_int[1] & beta0 < conf_int[1] 
        
        # w/o effect
        beta0 = 0.0
        beta = 0.0
        
        g <- rep(1:n_groups, n_each)
        group <-  as.factor(g)
        randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta)) 
        y <- rbinom(n, size = 1, prob = inv.logit(mu))
        
        fit_lm = glm(y ~ x  + group - 1, family = binomial)
        confs = confint(fit_lm)
        fit_summary = summary(fit_lm)$coefficients[-which("x" == rownames(summary(fit_lm)$coefficients)),]
        
        se_mean = (fit_summary[,"Std. Error"])
        est_mean = (fit_summary[,"Estimate"])
        estimates = MuMIn::par.avg(est_mean, se_mean, weight = rep(1, length(se_mean)))
        
        z_value = estimates[1] / estimates[2]
        p_value = 2*stats::pnorm(abs(z_value),lower.tail = FALSE)
        conf_int=c(estimates[1]-1.96*estimates[2], estimates[1]+1.96*estimates[2])
        
        results_wo[experiment, 1]= estimates[1]
        results_wo[experiment, 2]= summary(fit_lm)$coefficients["x","Estimate"]
        results_wo[experiment, 3]= p_value
        results_wo[experiment, 4]= summary(fit_lm)$coefficients["x","Pr(>|z|)"]
        results_wo[experiment, 5]= as.integer(beta > confs["x",1] & beta < confs["x",2])
        results_wo[experiment, 6]= beta0 > conf_int[1] & beta0 < conf_int[1] 
        
      }
      
      return(list(results_w = data.frame(results_w), 
                  results_wo = data.frame(results_wo)))
    })
})
saveRDS(result_list, file = "Results/results_intercept_glm.Rds")

#### Binomial Random slope -----------------------------------------------

system.time({
  result_list_slope = 
    snow::parLapply(cl, 2:8, function(number_groups)  {
      n_each <- 40
      n_groups <- number_groups
      results_w=results_wo = matrix(nrow = number_experiments, ncol =7)
      colnames(results_w) = c("estimate_intercept","estimate_effect", 
                                 "p_value_intercept", "p_value_effect", "Intercept_in_conf", "coverage_rate", "p_value_rate")
      
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
        y <- rbinom(n, 1, prob = inv.logit(mu)) 
        
        fit_lm = glm(y ~ x:group+x, family =  binomial)
        confs = confint(fit_lm)
        # results_w[experiment,1] = summary(fit_lm)$coefficients["(Intercept)","Estimate"]
        results_w[experiment,1] = summary(fit_lm)$coefficients["x", "Pr(>|z|)"]
        # results_w[experiment,3] = summary(fit_lm)$coefficients["(Intercept)","Pr(>|t|)"]
        # results_w[experiment,5] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
        # fit_summary = summary(fit_lm)$coefficients[-which("(Intercept)" == rownames(summary(fit_lm)$coefficients)),]
        # confs_wo = confs[-which(rownames(confs) == "(Intercept)"),]
        results_w[experiment,6] = beta > confs["x",1] & beta < confs["x",2]
        # results_w[experiment,7] = mean(fit_summary[,"Pr(>|t|)"] < 0.05)
        
        # w/o effect
        beta0 = 0.0
        beta = 0.0
        g <- rep(1:n_groups, n_each)
        group <-  as.factor(g)
        randslope <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta0,randslope[g[i]])) 
        y <- rbinom(n, 1, prob = inv.logit(mu)) 
        
        fit_lm = glm(y ~ x:group+x, family =  binomial)
        confs = confint(fit_lm)
        # results_w[experiment,1] = summary(fit_lm)$coefficients["(Intercept)","Estimate"]
        results_wo[experiment,1] = summary(fit_lm)$coefficients["x", "Pr(>|z|)"]
        # results_w[experiment,3] = summary(fit_lm)$coefficients["(Intercept)","Pr(>|t|)"]
        # results_w[experiment,5] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
        # fit_summary = summary(fit_lm)$coefficients[-which("(Intercept)" == rownames(summary(fit_lm)$coefficients)),]
        # confs_wo = confs[-which(rownames(confs) == "(Intercept)"),]
        results_wo[experiment,6] = beta > confs["x",1] & beta < confs["x",2]
        # results_w[experiment,7] = mean(fit_summary[,"Pr(>|t|)"] < 0.05)
        
      }
      
      return(list(results_w = data.frame(results_w), 
                  results_wo = data.frame(results_wo)))
    })
})
saveRDS(result_list_slope, file = "Results/results_slope_glm.Rds")

