# Simulation --------------------------------------------------------------
# y_i = a + (β_g)*x_i + ε_i
# with groups g and observations i
library(lme4)
library(lmerTest)
library(snow)
set.seed(1)

cl = snow::makeCluster(7L)
snow::clusterEvalQ(cl, {library(lme4); library(lmerTest); number_experiments = 10})

#### NORMAL Random intercept ---------------------------------------------

n_each <- 20

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
    
    results_w_lm=results_wo_lm = matrix(nrow = number_experiments, ncol =7)
    colnames(results_w_lm) = c("estimate_intercept","estimate_effect", 
                          "p_value_intercept", "p_value_effect", "Slope_in_conf", "coverage_rate", "p_value_rate")
    
    colnames(results_wo_lm) = colnames(results_w_lm)
    
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
      randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
      
      mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta)) 
      
      ## The normal model
      sigma <- 0.5
      y <- rnorm(n, mu, sd = sigma) 
      
      fit_lmm <- lmer(y ~ x  + (1 | group)) # only beta[c(1,2)] are also random
      confs = confint(fit_lmm)
    
      results_w[experiment,1] = summary(fit_lmm)$coefficients["(Intercept)","Estimate"]
      results_w[experiment,2] = summary(fit_lmm)$coefficients["x","Estimate"]
      results_w[experiment,3] = summary(fit_lmm)$coefficients["(Intercept)","Pr(>|t|)"]
      results_w[experiment,4] = summary(fit_lmm)$coefficients["x","Pr(>|t|)"]
      results_w[experiment,5] = summary(fit_lmm)$coefficients["(Intercept)","Std. Error"]
      results_w[experiment,6] = summary(fit_lmm)$coefficients["x","Std. Error"]
      results_w[experiment,7] = attr(summary(fit_lmm)$varcor$group, "stddev")
      results_w[experiment,8] = !is.null(fit_lmm@optinfo$conv$lme4$messages)
      results_w[experiment,9] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
      results_w[experiment,10] = as.integer(beta > confs["x",1] & beta < confs["x",2])
      
      fit_lm = lm(y ~ x  + group)
      confs = confint(fit_lm)
      results_w_lm[experiment,1] = summary(fit_lm)$coefficients["(Intercept)","Estimate"] + (1/n_groups)*sum(summary(fit_lm)$coefficients[-(1:2),"Estimate"])  
      results_w_lm[experiment,2] = summary(fit_lm)$coefficients["x","Estimate"]
      results_w_lm[experiment,3] = summary(fit_lm)$coefficients["(Intercept)","Pr(>|t|)"]
      results_w_lm[experiment,4] = summary(fit_lm)$coefficients["x","Pr(>|t|)"]
      results_w_lm[experiment,5] = as.integer(beta > confs["x",1] & beta < confs["x",2])
      fit_summary = summary(fit_lm)$coefficients[-which("x" == rownames(summary(fit_lm)$coefficients)),]
      confs_wo = confs[-which(rownames(confs) == "x"),]
      results_w_lm[experiment,6] = mean(  apply(confs_wo, 1, function(co) as.integer(beta > co[1] & beta < co[2]))  )
      results_w_lm[experiment,7] = mean(fit_summary[,"Pr(>|t|)"] < 0.05)
      
      
      beta0 = 0.0
      beta = 0.0
      
      g <- rep(1:n_groups, n_each)
      group <-  as.factor(g)
      randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
      
      mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta)) 
      
      ## The normal model
      sigma <- 0.5
      y <- rnorm(n, mu, sd = sigma) 
      
      fit_lmm <- lmer(y ~ x  + (1 | group)) # only beta[c(1,2)] are also random
      confs = confint(fit_lmm)
      
      results_wo[experiment,1] = summary(fit_lmm)$coefficients["(Intercept)","Estimate"]
      results_wo[experiment,2] = summary(fit_lmm)$coefficients["x","Estimate"]
      results_wo[experiment,3] = summary(fit_lmm)$coefficients["(Intercept)","Pr(>|t|)"]
      results_wo[experiment,4] = summary(fit_lmm)$coefficients["x","Pr(>|t|)"]
      results_wo[experiment,5] = summary(fit_lmm)$coefficients["(Intercept)","Std. Error"]
      results_wo[experiment,6] = summary(fit_lmm)$coefficients["x","Std. Error"]
      results_wo[experiment,7] = attr(summary(fit_lmm)$varcor$group, "stddev")
      results_wo[experiment,8] = !is.null(fit_lmm@optinfo$conv$lme4$messages)
      results_wo[experiment,9] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
      results_wo[experiment,10] = as.integer(beta > confs["x",1] & beta < confs["x",2])
      
      fit_lm = lm(y ~ x  + group)
      confs = confint(fit_lm)
      results_wo_lm[experiment,1] = summary(fit_lm)$coefficients["(Intercept)","Estimate"] + (1/n_groups)*sum(summary(fit_lm)$coefficients[-(1:2),"Estimate"])  
      results_wo_lm[experiment,2] = summary(fit_lm)$coefficients["x","Estimate"]
      results_wo_lm[experiment,3] = summary(fit_lm)$coefficients["(Intercept)","Pr(>|t|)"]
      results_wo_lm[experiment,4] = summary(fit_lm)$coefficients["x","Pr(>|t|)"]
      results_wo_lm[experiment,5] = as.integer(beta > confs["x",1] & beta < confs["x",2])
      fit_summary = summary(fit_lm)$coefficients[-which("x" == rownames(summary(fit_lm)$coefficients)),]
      confs_wo = confs[-which(rownames(confs) == "x"),]
      results_wo_lm[experiment,6] = mean(  apply(confs_wo, 1, function(co) as.integer(beta > co[1] & beta < co[2]))  )
      results_wo_lm[experiment,7] = mean(fit_summary[,"Pr(>|t|)"] < 0.05)
    
    }
    
    return(list(results_w = data.frame(results_w), 
                results_w_lm = data.frame(results_w_lm), 
                results_wo = data.frame(results_wo), 
                results_wo_lm = data.frame(results_wo_lm)))
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
      
      results_w_lm=results_wo_lm = matrix(nrow = number_experiments, ncol =7)
      colnames(results_w_lm) = c("estimate_intercept","estimate_effect", 
                                 "p_value_intercept", "p_value_effect", "Intercept_in_conf", "coverage_rate", "p_value_rate")
      
      colnames(results_wo_lm) = colnames(results_w_lm)
      
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
        
        ## The normal model
        sigma <- 0.5
        y <- rnorm(n, mu, sd = sigma) 
        
        fit_lmm <- lmer(y ~ x  + (0 + x | group))  # only beta[c(1,2)] are also random
        confs = confint(fit_lmm)
        
        results_w[experiment,1] = summary(fit_lmm)$coefficients["(Intercept)","Estimate"]
        results_w[experiment,2] = summary(fit_lmm)$coefficients["x","Estimate"]
        results_w[experiment,3] = summary(fit_lmm)$coefficients["(Intercept)","Pr(>|t|)"]
        results_w[experiment,4] = summary(fit_lmm)$coefficients["x","Pr(>|t|)"]
        results_w[experiment,5] = summary(fit_lmm)$coefficients["(Intercept)","Std. Error"]
        results_w[experiment,6] = summary(fit_lmm)$coefficients["x","Std. Error"]
        results_w[experiment,7] = attr(summary(fit_lmm)$varcor$group, "stddev")
        results_w[experiment,8] = !is.null(fit_lmm@optinfo$conv$lme4$messages)
        results_w[experiment,9] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
        results_w[experiment,10]= as.integer(beta > confs["x",1] & beta < confs["x",2])
        
        fit_lm = lm(y ~ x:group)
        confs = confint(fit_lm)
        results_w_lm[experiment,1] = summary(fit_lm)$coefficients["(Intercept)","Estimate"]
        #results_w_lm[experiment,2] = summary(fit_lm)$coefficients["x","Estimate"]
        results_w_lm[experiment,3] = summary(fit_lm)$coefficients["(Intercept)","Pr(>|t|)"]
        #results_w_lm[experiment,4] = summary(fit_lm)$coefficients["x","Pr(>|t|)"]
        results_w_lm[experiment,5] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
        fit_summary = summary(fit_lm)$coefficients[-which("(Intercept)" == rownames(summary(fit_lm)$coefficients)),]
        confs_wo = confs[-which(rownames(confs) == "(Intercept)"),]
        results_w_lm[experiment,6] = mean(  apply(confs_wo, 1, function(co) as.integer(beta0 > co[1] & beta0 < co[2]))  )
        results_w_lm[experiment,7] = mean(fit_summary[,"Pr(>|t|)"] < 0.05)
        
        
        beta0 = 0.0
        beta = 0.0
        
        g <- rep(1:n_groups, n_each)
        group <-  as.factor(g)
        randslope <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta0,randslope[g[i]])) 
        
        ## The normal model
        sigma <- 0.5
        y <- rnorm(n, mu, sd = sigma) 
        
        fit_lmm <- lmer(y ~ x  + (0 + x | group))  # only beta[c(1,2)] are also random
        confs = confint(fit_lmm)
        
        results_wo[experiment,1] = summary(fit_lmm)$coefficients["(Intercept)","Estimate"]
        results_wo[experiment,2] = summary(fit_lmm)$coefficients["x","Estimate"]
        results_wo[experiment,3] = summary(fit_lmm)$coefficients["(Intercept)","Pr(>|t|)"]
        results_wo[experiment,4] = summary(fit_lmm)$coefficients["x","Pr(>|t|)"]
        results_wo[experiment,5] = summary(fit_lmm)$coefficients["(Intercept)","Std. Error"]
        results_wo[experiment,6] = summary(fit_lmm)$coefficients["x","Std. Error"]
        results_wo[experiment,7] = attr(summary(fit_lmm)$varcor$group, "stddev")
        results_wo[experiment,8] = !is.null(fit_lmm@optinfo$conv$lme4$messages)
        results_wo[experiment,9] = as.integer(beta0 > confs[3,1] & beta0 < confs[3,2])
        results_wo[experiment,10] = as.integer(beta > confs[4,1] & beta < confs[4,2])
        
        fit_lm = lm(y ~ x:group)
        confs = confint(fit_lm)
        results_wo_lm[experiment,1] = summary(fit_lm)$coefficients["(Intercept)","Estimate"]
        #results_wo_lm[experiment,2] = summary(fit_lm)$coefficients["x","Estimate"]
        results_wo_lm[experiment,3] = summary(fit_lm)$coefficients["(Intercept)","Pr(>|t|)"]
        #results_wo_lm[experiment,4] = summary(fit_lm)$coefficients["x","Pr(>|t|)"]
        results_wo_lm[experiment,5] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
        fit_summary = summary(fit_lm)$coefficients[-which("(Intercept)" == rownames(summary(fit_lm)$coefficients)),]
        confs_wo = confs[-which(rownames(confs) == "(Intercept)"),]
        results_wo_lm[experiment,6] = mean(  apply(confs_wo, 1, function(co) as.integer(beta0 > co[1] & beta0 < co[2]))  )
        results_wo_lm[experiment,7] = mean(fit_summary[,"Pr(>|t|)"] < 0.05)
        
      }
      
      return(list(results_w = data.frame(results_w), 
                  results_w_lm = data.frame(results_w_lm), 
                  results_wo = data.frame(results_wo), 
                  results_wo_lm = data.frame(results_wo_lm)))
    })
})
saveRDS(result_list_slope, file = "Results/results_slope_lme4.Rds")


#### Binomial Random intercept -----------------------------------------------

inv.logit <- function(p){return(exp(p)/(1 + exp(p)))}


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
      
      results_w_lm=results_wo_lm = matrix(nrow = number_experiments, ncol =7)
      colnames(results_w_lm) = c("estimate_intercept","estimate_effect", 
                                 "p_value_intercept", "p_value_effect", "Slope_in_conf", "coverage_rate", "p_value_rate")
      
      colnames(results_wo_lm) = colnames(results_w_lm)
      
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
        randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta)) 
        y <- rbinom(n, size = 1, prob = inv.logit(mu))
        
        fit_lmm <- glmer(y ~ x  + (1 | group), family=binomial) # only beta[c(1,2)] are also random
        confs = confint(fit_lmm)
        
        results_w[experiment,1] = summary(fit_lmm)$coefficients["(Intercept)","Estimate"]
        results_w[experiment,2] = summary(fit_lmm)$coefficients["x","Estimate"]
        results_w[experiment,3] = summary(fit_lmm)$coefficients["(Intercept)","Pr(>|z|)"]
        results_w[experiment,4] = summary(fit_lmm)$coefficients["x","Pr(>|z|)"]
        results_w[experiment,5] = summary(fit_lmm)$coefficients["(Intercept)","Std. Error"]
        results_w[experiment,6] = summary(fit_lmm)$coefficients["x","Std. Error"]
        results_w[experiment,7] = attr(summary(fit_lmm)$varcor$group, "stddev")
        results_w[experiment,8] = !is.null(fit_lmm@optinfo$conv$lme4$messages)
        results_w[experiment,9] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
        results_w[experiment,10] = as.integer(beta > confs["x",1] & beta < confs["x",2])
        
        fit_lm = glm(y ~ x  + group, family = binomial)
        confs = confint(fit_lm)
        results_w_lm[experiment,1] = summary(fit_lm)$coefficients["(Intercept)","Estimate"] + (1/n_groups)*sum(summary(fit_lm)$coefficients[-(1:2),"Estimate"])  
        results_w_lm[experiment,2] = summary(fit_lm)$coefficients["x","Estimate"]
        results_w_lm[experiment,3] = summary(fit_lm)$coefficients["(Intercept)","Pr(>|z|)"]
        results_w_lm[experiment,4] = summary(fit_lm)$coefficients["x","Pr(>|z|)"]
        results_w_lm[experiment,5] = as.integer(beta > confs["x",1] & beta < confs["x",2])
        fit_summary = summary(fit_lm)$coefficients[-which("x" == rownames(summary(fit_lm)$coefficients)),]
        confs_wo = confs[-which(rownames(confs) == "x"),]
        results_w_lm[experiment,6] = mean(  apply(confs_wo, 1, function(co) as.integer(beta > co[1] & beta < co[2]))  )
        results_w_lm[experiment,7] = mean(fit_summary[,"Pr(>|z|)"] < 0.05)
        
        
        beta0 = 0.0
        beta = 0.0
        
        g <- rep(1:n_groups, n_each)
        group <-  as.factor(g)
        randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta)) 
        y <- rbinom(n, size = 1, prob = inv.logit(mu))
        
        fit_lmm <- glmer(y ~ x  + (1 | group), family=binomial) # only beta[c(1,2)] are also random
        confs = confint(fit_lmm)
        
        results_wo[experiment,1] = summary(fit_lmm)$coefficients["(Intercept)","Estimate"]
        results_wo[experiment,2] = summary(fit_lmm)$coefficients["x","Estimate"]
        results_wo[experiment,3] = summary(fit_lmm)$coefficients["(Intercept)","Pr(>|z|)"]
        results_wo[experiment,4] = summary(fit_lmm)$coefficients["x","Pr(>|z|)"]
        results_wo[experiment,5] = summary(fit_lmm)$coefficients["(Intercept)","Std. Error"]
        results_wo[experiment,6] = summary(fit_lmm)$coefficients["x","Std. Error"]
        results_wo[experiment,7] = attr(summary(fit_lmm)$varcor$group, "stddev")
        results_wo[experiment,8] = !is.null(fit_lmm@optinfo$conv$lme4$messages)
        results_wo[experiment,9] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
        results_wo[experiment,10] = as.integer(beta > confs["x",1] & beta < confs["x",2])
        
        fit_lm = glm(y ~ x  + group, family = binomial)
        confs = confint(fit_lm)
        results_wo_lm[experiment,1] = summary(fit_lm)$coefficients["(Intercept)","Estimate"] + (1/n_groups)*sum(summary(fit_lm)$coefficients[-(1:2),"Estimate"])  
        results_wo_lm[experiment,2] = summary(fit_lm)$coefficients["x","Estimate"]
        results_wo_lm[experiment,3] = summary(fit_lm)$coefficients["(Intercept)","Pr(>|z|)"]
        results_wo_lm[experiment,4] = summary(fit_lm)$coefficients["x","Pr(>|z|)"]
        results_wo_lm[experiment,5] = as.integer(beta > confs["x",1] & beta < confs["x",2])
        fit_summary = summary(fit_lm)$coefficients[-which("x" == rownames(summary(fit_lm)$coefficients)),]
        confs_wo = confs[-which(rownames(confs) == "x"),]
        results_wo_lm[experiment,6] = mean(  apply(confs_wo, 1, function(co) as.integer(beta > co[1] & beta < co[2]))  )
        results_wo_lm[experiment,7] = mean(fit_summary[,"Pr(>|z|)"] < 0.05)
        
      }
      
      return(list(results_w = data.frame(results_w), 
                  results_w_lm = data.frame(results_w_lm), 
                  results_wo = data.frame(results_wo), 
                  results_wo_lm = data.frame(results_wo_lm)))
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
      
      results_w_lm=results_wo_lm = matrix(nrow = number_experiments, ncol =7)
      colnames(results_w_lm) = c("estimate_intercept","estimate_effect", 
                                 "p_value_intercept", "p_value_effect", "Intercept_in_conf", "coverage_rate", "p_value_rate")
      
      colnames(results_wo_lm) = colnames(results_w_lm)
      
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
        confs = confint(fit_lmm)
        
        results_w[experiment,1] = summary(fit_lmm)$coefficients["(Intercept)","Estimate"]
        results_w[experiment,2] = summary(fit_lmm)$coefficients["x","Estimate"]
        results_w[experiment,3] = summary(fit_lmm)$coefficients["(Intercept)","Pr(>|z|)"]
        results_w[experiment,4] = summary(fit_lmm)$coefficients["x","Pr(>|z|)"]
        results_w[experiment,5] = summary(fit_lmm)$coefficients["(Intercept)","Std. Error"]
        results_w[experiment,6] = summary(fit_lmm)$coefficients["x","Std. Error"]
        results_w[experiment,7] = attr(summary(fit_lmm)$varcor$group, "stddev")
        results_w[experiment,8] = !is.null(fit_lmm@optinfo$conv$lme4$messages)
        results_w[experiment,9] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
        results_w[experiment,10]= as.integer(beta > confs["x",1] & beta < confs["x",2])
        
        fit_lm = glm(y ~ x:group, family =  binomial)
        confs = confint(fit_lm)
        results_w_lm[experiment,1] = summary(fit_lm)$coefficients["(Intercept)","Estimate"]
        #results_w_lm[experiment,2] = summary(fit_lm)$coefficients["x","Estimate"]
        results_w_lm[experiment,3] = summary(fit_lm)$coefficients["(Intercept)","Pr(>|z|)"]
        #results_w_lm[experiment,4] = summary(fit_lm)$coefficients["x","Pr(>|t|)"]
        results_w_lm[experiment,5] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
        fit_summary = summary(fit_lm)$coefficients[-which("(Intercept)" == rownames(summary(fit_lm)$coefficients)),]
        confs_wo = confs[-which(rownames(confs) == "(Intercept)"),]
        results_w_lm[experiment,6] = mean(  apply(confs_wo, 1, function(co) as.integer(beta0 > co[1] & beta0 < co[2]))  )
        results_w_lm[experiment,7] = mean(fit_summary[,"Pr(>|z|)"] < 0.05)
        
        
        beta0 = 0.0
        beta = 0.0
        
        g <- rep(1:n_groups, n_each)
        group <-  as.factor(g)
        randslope <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(beta0,randslope[g[i]])) 
        
        y <- rbinom(n, 1, prob = inv.logit(mu)) 
        
        fit_lmm <- glmer(y ~ x  + (0 + x | group), family = binomial)  # only beta[c(1,2)] are also random
        confs = confint(fit_lmm)
        
        results_wo[experiment,1] = summary(fit_lmm)$coefficients["(Intercept)","Estimate"]
        results_wo[experiment,2] = summary(fit_lmm)$coefficients["x","Estimate"]
        results_wo[experiment,3] = summary(fit_lmm)$coefficients["(Intercept)","Pr(>|z|)"]
        results_wo[experiment,4] = summary(fit_lmm)$coefficients["x","Pr(>|z|)"]
        results_wo[experiment,5] = summary(fit_lmm)$coefficients["(Intercept)","Std. Error"]
        results_wo[experiment,6] = summary(fit_lmm)$coefficients["x","Std. Error"]
        results_wo[experiment,7] = attr(summary(fit_lmm)$varcor$group, "stddev")
        results_wo[experiment,8] = !is.null(fit_lmm@optinfo$conv$lme4$messages)
        results_wo[experiment,9] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
        results_wo[experiment,10] = as.integer(beta > confs["x",1] & beta < confs["x",2])
        
        fit_lm = glm(y ~ x:group, family = binomial)
        confs = confint(fit_lm)
        results_wo_lm[experiment,1] = summary(fit_lm)$coefficients["(Intercept)","Estimate"]
        #results_wo_lm[experiment,2] = summary(fit_lm)$coefficients["x","Estimate"]
        results_wo_lm[experiment,3] = summary(fit_lm)$coefficients["(Intercept)","Pr(>|z|)"]
        #results_wo_lm[experiment,4] = summary(fit_lm)$coefficients["x","Pr(>|t|)"]
        results_wo_lm[experiment,5] = as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2])
        fit_summary = summary(fit_lm)$coefficients[-which("(Intercept)" == rownames(summary(fit_lm)$coefficients)),]
        confs_wo = confs[-which(rownames(confs) == "(Intercept)"),]
        results_wo_lm[experiment,6] = mean(  apply(confs_wo, 1, function(co) as.integer(beta0 > co[1] & beta0 < co[2]))  )
        results_wo_lm[experiment,7] = mean(fit_summary[,"Pr(>|z|)"] < 0.05)
        
      }
      
      return(list(results_w = data.frame(results_w), 
                  results_w_lm = data.frame(results_w_lm), 
                  results_wo = data.frame(results_wo), 
                  results_wo_lm = data.frame(results_wo_lm)))
    })
})
saveRDS(result_list_slope, file = "Results/results_slope_glmer.Rds")

