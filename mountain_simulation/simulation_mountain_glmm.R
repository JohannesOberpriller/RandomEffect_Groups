library(lme4)
library(lmerTest)
library(snow)
library(glmmTMB)
set.seed(1)

extract_results_t = function(fit_lmm, confs, beta, beta0) {
  return(
    c(summary(fit_lmm)$coefficients["(Intercept)","Estimate"],
      summary(fit_lmm)$coefficients["x","Estimate"],
      summary(fit_lmm)$coefficients["(Intercept)","Pr(>|z|)"],
      summary(fit_lmm)$coefficients["x","Pr(>|z|)"],
      summary(fit_lmm)$coefficients["(Intercept)","Std. Error"],
      summary(fit_lmm)$coefficients["x","Std. Error"],
      attr(summary(fit_lmm)$varcor$group, "stddev")["(Intercept)"],
      attr(summary(fit_lmm)$varcor$group, "stddev")["x"],
      !is.null(fit_lmm@optinfo$conv$lme4$messages),
      as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2]),
      as.integer(beta > confs["x",1] & beta < confs["x",2])))
}

extract_results = function(fit_lmm, confs, beta, beta0) {
  return(
    c(summary(fit_lmm)$coefficients$cond["(Intercept)","Estimate"],
      summary(fit_lmm)$coefficients$cond["x","Estimate"],
      summary(fit_lmm)$coefficients$cond["(Intercept)","Pr(>|z|)"],
      summary(fit_lmm)$coefficients$cond["x","Pr(>|z|)"],
      summary(fit_lmm)$coefficients$cond["(Intercept)","Std. Error"],
      summary(fit_lmm)$coefficients$cond["x","Std. Error"],
      attr(summary(fit_lmm)$varcor$cond$group,"stddev")["(Intercept)"],
      attr(summary(fit_lmm)$varcor$cond$group,"stddev")["x"],
      as.integer(!fit_lmm$sdr$pdHess),
      as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2]),
      as.integer(beta > confs["x",1] & beta < confs["x",2])))
}
inv.logit <- function(p){return(exp(p)/(1 + exp(p)))}


cl = snow::makeCluster(7L)
snow::clusterEvalQ(cl, {library(lme4); library(lmerTest);library(glmmTMB); number_experiments =10000})
snow::clusterExport(cl, list("extract_results","extract_results_t", "inv.logit"), envir = environment())

system.time({
  result_list = 
    snow::parLapply(cl, 2:8, function(number_groups)  {
      
      n_each <- 80
      n_groups <- number_groups
      
      results_w_lme4 = results_wo_lme4 = matrix(nrow = number_experiments, ncol = 11)
      colnames(results_w_lme4) = c("estimate_intercept","estimate_effect", 
                              "p_value_intercept", "p_value_effect",
                              "se_intercept", "se_effect",
                              "stddev_randeff_inter","stddev_randeff_x", "Singularity",
                              "Int_in_conf",  "Slope_in_conf")
      colnames(results_wo_lme4) = colnames(results_w_lme4)
      
      
      results_w_glmmTMB = results_wo_glmmTMB = matrix(nrow = number_experiments, ncol = 11)
      colnames(results_w_glmmTMB) = c("estimate_intercept","estimate_effect", 
                                   "p_value_intercept", "p_value_effect",
                                   "se_intercept", "se_effect",
                                   "stddev_randeff_inter","stddev_randeff_x", "Singularity",
                                   "Int_in_conf",  "Slope_in_conf")
      colnames(results_wo_glmmTMB) = colnames(results_w_glmmTMB)
      
      
      results_w_lm = results_wo_lm = matrix(nrow = number_experiments, ncol = 4)
      colnames(results_w_lm) = c("estimate_effect", "p_value_effect","se_effect", "Slope_in_conf")
      colnames(results_wo_lm) = colnames(results_w_lm)
      
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
        
        
        # lme4
        fit_lmm <- glmer(y ~ x  + (x | group), family = binomial) 
        summ = summary(fit_lmm)
        
        confs = rbind(cbind(summ$coefficients[1,"Estimate"] - 1.96*summ$coefficients[1,"Std. Error"], 
                            summ$coefficients[1,"Estimate"] + 1.96*summ$coefficients[1,"Std. Error"]),
                      cbind(summ$coefficients[2,"Estimate"] - 1.96*summ$coefficients[2,"Std. Error"], 
                            summ$coefficients[2,"Estimate"] + 1.96*summ$coefficients[2,"Std. Error"])
                      )
        rownames(confs) = c("(Intercept)", "x")
        results_w_lme4[experiment, ] = extract_results_t(fit_lmm, confs, beta, beta0)
        
        # glmmTMB
        fit_glmmTMB <- glmmTMB::glmmTMB(y ~ x  + (x | group), family = binomial) 
        summ = summary(fit_glmmTMB)
        confs = rbind(cbind(summ$coefficients$cond[1,"Estimate"] - 1.96*summ$coefficients$cond[1,"Std. Error"], 
                            summ$coefficients$cond[1,"Estimate"] + 1.96*summ$coefficients$cond[1,"Std. Error"]),
                      cbind(summ$coefficients$cond[2,"Estimate"] - 1.96*summ$coefficients$cond[2,"Std. Error"], 
                            summ$coefficients$cond[2,"Estimate"] + 1.96*summ$coefficients$cond[2,"Std. Error"])
        )
        rownames(confs) = c("(Intercept)", "x")
        results_w_glmmTMB[experiment, ] = extract_results(fit_glmmTMB, confs, beta, beta0)
        
        # lm 
        fit_lm = glm(y ~ x*group, family = binomial)
        summ = summary(fit_lm)
        confs = confint(fit_lm)
        results_w_lm[experiment, ] = c(summ$coefficients["x", 1], summ$coefficients["x", 4], summ$coefficients["x", 2], as.integer(beta > confs["x",1] & beta < confs["x",2]))
        
        
        
        
        # w/o effect
        beta0 = 0.0
        beta = 0.0
        g <- rep(1:n_groups, n_each)
        group <-  as.factor(g)
        randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)
        
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta)) 
        y <- rbinom(n, size = 1, prob = inv.logit(mu))
        
        fit_lmm <- glmer(y ~ x  + (x | group), family = binomial) 
        summ = summary(fit_lmm)
        confs = rbind(cbind(summ$coefficients[1,"Estimate"] - 1.96*summ$coefficients[1,"Std. Error"], 
                            summ$coefficients[1,"Estimate"] + 1.96*summ$coefficients[1,"Std. Error"]),
                      cbind(summ$coefficients[2,"Estimate"] - 1.96*summ$coefficients[2,"Std. Error"], 
                            summ$coefficients[2,"Estimate"] + 1.96*summ$coefficients[2,"Std. Error"])
        )
        rownames(confs) = c("(Intercept)", "x")
        results_wo_lme4[experiment, ] = extract_results_t(fit_lmm, confs, beta, beta0)
        
        # glmmTMB
        fit_glmmTMB <- glmmTMB::glmmTMB(y ~ x  + (x | group), family = binomial) 
        summ = summary(fit_glmmTMB)
        confs = rbind(cbind(summ$coefficients$cond[1,"Estimate"] - 1.96*summ$coefficients$cond[1,"Std. Error"], 
                            summ$coefficients$cond[1,"Estimate"] + 1.96*summ$coefficients$cond[1,"Std. Error"]),
                      cbind(summ$coefficients$cond[2,"Estimate"] - 1.96*summ$coefficients$cond[2,"Std. Error"], 
                            summ$coefficients$cond[2,"Estimate"] + 1.96*summ$coefficients$cond[2,"Std. Error"])
        )
        rownames(confs) = c("(Intercept)", "x")
        results_wo_glmmTMB[experiment, ] = extract_results(fit_glmmTMB, confs, beta, beta0)
        
        # lm
        fit_lm = glm(y ~ x*group, family = binomial)
        summ = summary(fit_lm)
        confs = confint(fit_lm)
        results_wo_lm[experiment, ] = c(summ$coefficients["x", 1], summ$coefficients["x", 4], summ$coefficients["x", 2], as.integer(beta > confs["x",1] & beta < confs["x",2]))
        
      }
      
      return(list(results_w_lme4 = data.frame(results_w_lme4), 
                  results_wo_lme4 = data.frame(results_wo_lme4),
                  results_w_glmmTMB = data.frame(results_w_glmmTMB), 
                  results_wo_glmmTMB = data.frame(results_wo_glmmTMB),
                  results_w_lm = data.frame(results_w_lm),
                  results_wo_lm = data.frame(results_wo_lm)
                  ))
    })
})
saveRDS(result_list, file = "Results/results_mountain_glmm.Rds")




