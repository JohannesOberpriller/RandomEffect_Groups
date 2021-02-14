library(lme4)
library(lmerTest)
library(snow)
library(glmmTMB)
set.seed(1)


################ Helper functions  ################ 
# helper functions to extracts model properties 
extract_results_t = function(fit_lmm, confs, beta, beta0) {
  return(
    c(summary(fit_lmm)$coefficients["(Intercept)","Estimate"],
      summary(fit_lmm)$coefficients["x","Estimate"],
      summary(fit_lmm)$coefficients["(Intercept)","Pr(>|t|)"],
      summary(fit_lmm)$coefficients["x","Pr(>|t|)"],
      summary(fit_lmm)$coefficients["(Intercept)","Std. Error"],
      summary(fit_lmm)$coefficients["x","Std. Error"],
      attr(summary(fit_lmm)$varcor$group, "stddev")["(Intercept)"],
      #attr(summary(fit_lmm)$varcor$group, "stddev")["x"],
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
      #attr(summary(fit_lmm)$varcor$cond$group,"stddev")["x"],
      as.integer(!fit_lmm$sdr$pdHess),
      as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2]),
      as.integer(beta > confs["x",1] & beta < confs["x",2])))
}



cl = snow::makeCluster(7L) # reduce the number of cores if you have not 7 physical CPU cores
snow::clusterEvalQ(cl, {library(lme4); library(lmerTest);library(glmmTMB); number_experiments =5000})
snow::clusterExport(cl, list("extract_results","extract_results_t"), envir = environment())

for(sd_re in c(0.01, 0.1, 0.5, 2.0)) {
  snow::clusterExport(cl, list("sd_re"))
  system.time({
    result_list = 
      snow::parLapply(cl, 2:8, function(number_groups)  {
        
        n_each <- 50/2
        n_groups <- number_groups
        
        results_w_lme4_ml = results_wo_lme4_ml = results_w_lme4_reml = results_wo_lme4_reml = matrix(nrow = number_experiments, ncol = 10)
        colnames(results_w_lme4_ml) = c("estimate_intercept","estimate_effect", 
                                "p_value_intercept", "p_value_effect",
                                "se_intercept", "se_effect",
                                "stddev_randeff_inter", "Singularity",
                                "Int_in_conf",  "Slope_in_conf")
        colnames(results_wo_lme4_ml) = colnames(results_w_lme4_ml)
        colnames(results_w_lme4_reml) = colnames(results_w_lme4_ml)
        colnames(results_wo_lme4_reml) = colnames(results_w_lme4_ml)
        
        
        results_w_glmmTMB_ml = results_wo_glmmTMB_ml = results_w_glmmTMB_reml = results_wo_glmmTMB_reml = matrix(nrow = number_experiments, ncol = 10)
        colnames(results_w_glmmTMB_ml) = c("estimate_intercept","estimate_effect", 
                                     "p_value_intercept", "p_value_effect",
                                     "se_intercept", "se_effect",
                                     "stddev_randeff_inter", "Singularity",
                                     "Int_in_conf",  "Slope_in_conf")
        colnames(results_wo_glmmTMB_ml) = colnames(results_w_glmmTMB_ml)
        colnames(results_w_glmmTMB_reml) = colnames(results_w_glmmTMB_ml)
        colnames(results_wo_glmmTMB_reml) = colnames(results_w_glmmTMB_ml)
        
        
        results_w_lm = results_wo_lm = results_wo_lm_wo_grouping = results_w_lm_wo_grouping = matrix(nrow = number_experiments, ncol = 4)
        colnames(results_w_lm) = c("estimate_effect", "p_value_effect","se_effect", "Slope_in_conf")
        colnames(results_wo_lm) = colnames(results_w_lm)
        colnames(results_wo_lm_wo_grouping) = colnames(results_w_lm)
        colnames(results_w_lm_wo_grouping) = colnames(results_w_lm)
        
        ################  The data generating process ################ 
        # Number of observations, 50 observations for each mountain range 
        n = (number_groups)*n_each
        
        ### Environmental predictors (Intercept and Temperature) ###
        x <- runif(n, -1, 1) # Temperature
        X <- matrix(c(rep(1, n), x), nrow = n) # Intercept, Temperature
        
        sd_randeff = sd_re # sd for random effects
        
        
        for(experiment in 1:number_experiments){
          
          ################ Simulation of LMM with Temperature effect  ################ 
          # fixed effects
          beta = 0.4    # Temperature effect
          beta0 = 10.0  # Intercept
          
          # random effects are sampled around the fixed effects
          g <- rep(1:n_groups, n_each) # Grouping variable (mountain range)
          group <-  as.factor(g)
          randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)  # random intercept
          
          # calculate linear response, different intercept and slope for each mountain range
          mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta))
          
          # sample residuals from a Normal distribution
          sigma <- 0.5
          y <- rnorm(n, mu, sd = sigma) 
          
          
          ################ Fitting: lme4, glmmTMB, and LM ################ 
          # we tested MLE (maximum likelihood estimation) and REML (restricted maximum likelihood estimation) for glmmTMB and lme4
          
          # lme4 - MLE
          try({
            fit_lmm <- lmer(y ~ x  + (1 | group), REML = FALSE) 
            summ = summary(fit_lmm)
            
            # calculate confidence intervals for the temperature effect
            confs = rbind(cbind(summ$coefficients[1,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"], 
                                summ$coefficients[1,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"]),
                          cbind(summ$coefficients[2,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"], 
                                summ$coefficients[2,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"])
                          )
            rownames(confs) = c("(Intercept)", "x")
            results_w_lme4_ml[experiment, ] = extract_results_t(fit_lmm, confs, beta, beta0)
          }, silent = TRUE)
          
          
          # lme4 - REML
          try({
            fit_lmm <- lmer(y ~ x  + (1 | group), REML = TRUE) 
            summ = summary(fit_lmm)
            
            # calculate confidence intervals for the temperature effect
            confs = rbind(cbind(summ$coefficients[1,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"], 
                                summ$coefficients[1,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"]),
                          cbind(summ$coefficients[2,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"], 
                                summ$coefficients[2,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"])
            )
            rownames(confs) = c("(Intercept)", "x")
            results_w_lme4_reml[experiment, ] = extract_results_t(fit_lmm, confs, beta, beta0)
          }, silent = TRUE)
            
          # glmmTMB - MLE
          try({
            fit_glmmTMB <- glmmTMB::glmmTMB(y ~ x  + (1 | group), REML = FALSE) 
            summ = summary(fit_glmmTMB)
            
            # calculate confidence intervals for the temperature effect
            confs = rbind(cbind(summ$coefficients$cond[1,"Estimate"] - 1.96*summ$coefficients$cond[1,"Std. Error"], 
                                summ$coefficients$cond[1,"Estimate"] + 1.96*summ$coefficients$cond[1,"Std. Error"]),
                          cbind(summ$coefficients$cond[2,"Estimate"] - 1.96*summ$coefficients$cond[2,"Std. Error"], 
                                summ$coefficients$cond[2,"Estimate"] + 1.96*summ$coefficients$cond[2,"Std. Error"])
            )
            rownames(confs) = c("(Intercept)", "x")
            results_w_glmmTMB_ml[experiment, ] = extract_results(fit_glmmTMB, confs, beta, beta0)
          }, silent = TRUE)
          
          # glmmTMB - REML
          try({
            fit_glmmTMB <- glmmTMB::glmmTMB(y ~ x  + (1 | group), REML = TRUE)
            summ = summary(fit_glmmTMB)
            
            # calculate confidence intervals for the temperature effect
            confs = rbind(cbind(summ$coefficients$cond[1,"Estimate"] - 1.96*summ$coefficients$cond[1,"Std. Error"], 
                                summ$coefficients$cond[1,"Estimate"] + 1.96*summ$coefficients$cond[1,"Std. Error"]),
                          cbind(summ$coefficients$cond[2,"Estimate"] - 1.96*summ$coefficients$cond[2,"Std. Error"], 
                                summ$coefficients$cond[2,"Estimate"] + 1.96*summ$coefficients$cond[2,"Std. Error"])
            )
            rownames(confs) = c("(Intercept)", "x")
            results_w_glmmTMB_reml[experiment, ] = extract_results(fit_glmmTMB, confs, beta, beta0)
          }, silent = TRUE)
          
          # lm 
          try({
            fit_lm = lm(y ~ x + group)
            summ = summary(fit_lm)
            confs = confint(fit_lm)
            results_w_lm[experiment, ] = c(summ$coefficients["x", 1], summ$coefficients["x", 4], summ$coefficients["x", 2], as.integer(beta > confs["x",1] & beta < confs["x",2]))
          }, silent = TRUE)
          
          
          
          # lm w/o grouping
          try({
            fit_lm = lm(y ~ x)
            summ = summary(fit_lm)
            confs = confint(fit_lm)
            results_w_lm_wo_grouping[experiment, ] = c(summ$coefficients["x", 1], summ$coefficients["x", 4], summ$coefficients["x", 2], as.integer(beta > confs["x",1] & beta < confs["x",2]))
          }, silent = TRUE)
          
          
          
          
          ################ Simulation of LMM without Temperature effect  ################ 
          # fixed effects
          beta0 = 10.0  # Intercept
          beta = 0.0    # Temperature, now set to zero
          
          # random effects
          g <- rep(1:n_groups, n_each) # Grouping variable (mountain range)
          group <-  as.factor(g)
          randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)  # random intercept
          
          # calculate linear response, different intercept and slope for each mountain range
          mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],beta)) 
          
          # sample residuals from a Normal distribution
          sigma <- 0.5
          y <- rnorm(n, mu, sd = sigma) 
          
          
          ################ Fitting: lme4, glmmTMB, and LM ################ 
          # we tested MLE (maximum likelihood estimation) and REML (restricted maximum likelihood estimation) for glmmTMB and lme4
          
          # lme4 - MLE
          try({
            fit_lmm <- lmer(y ~ x  + (1 | group), REML = FALSE) 
            summ = summary(fit_lmm)
            
            # calculate confidence intervals for the temperature effect
            confs = rbind(cbind(summ$coefficients[1,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"], 
                                summ$coefficients[1,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"]),
                          cbind(summ$coefficients[2,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"], 
                                summ$coefficients[2,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"])
            )
            rownames(confs) = c("(Intercept)", "x")
            results_wo_lme4_ml[experiment, ] = extract_results_t(fit_lmm, confs, beta, beta0)
          }, silent = TRUE)
          
          # lme4 - REML
          try({
            fit_lmm <- lmer(y ~ x  + (1 | group), REML = TRUE) 
            summ = summary(fit_lmm)
            
            # calculate confidence intervals for the temperature effect
            confs = rbind(cbind(summ$coefficients[1,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"], 
                                summ$coefficients[1,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"]),
                          cbind(summ$coefficients[2,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"], 
                                summ$coefficients[2,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"])
            )
            rownames(confs) = c("(Intercept)", "x")
            results_wo_lme4_reml[experiment, ] = extract_results_t(fit_lmm, confs, beta, beta0)
          }, silent = TRUE)
          
          # glmmTMB - MLE
          try({
            fit_glmmTMB <- glmmTMB::glmmTMB(y ~ x  + (1 | group), REML = FALSE) 
            summ = summary(fit_glmmTMB)
            
            # calculate confidence intervals for the temperature effect
            confs = rbind(cbind(summ$coefficients$cond[1,"Estimate"] - 1.96*summ$coefficients$cond[1,"Std. Error"], 
                                summ$coefficients$cond[1,"Estimate"] + 1.96*summ$coefficients$cond[1,"Std. Error"]),
                          cbind(summ$coefficients$cond[2,"Estimate"] - 1.96*summ$coefficients$cond[2,"Std. Error"], 
                                summ$coefficients$cond[2,"Estimate"] + 1.96*summ$coefficients$cond[2,"Std. Error"])
            )
            rownames(confs) = c("(Intercept)", "x")
            results_wo_glmmTMB_ml[experiment, ] = extract_results(fit_glmmTMB, confs, beta, beta0)
          }, silent = TRUE)
          
          # glmmTMB - REML
          try({
            fit_glmmTMB <- glmmTMB::glmmTMB(y ~ x  + (1 | group), REML = TRUE) 
            summ = summary(fit_glmmTMB)
            confs = rbind(cbind(summ$coefficients$cond[1,"Estimate"] - 1.96*summ$coefficients$cond[1,"Std. Error"], 
                                summ$coefficients$cond[1,"Estimate"] + 1.96*summ$coefficients$cond[1,"Std. Error"]),
                          cbind(summ$coefficients$cond[2,"Estimate"] - 1.96*summ$coefficients$cond[2,"Std. Error"], 
                                summ$coefficients$cond[2,"Estimate"] + 1.96*summ$coefficients$cond[2,"Std. Error"])
            )
            rownames(confs) = c("(Intercept)", "x")
            results_wo_glmmTMB_reml[experiment, ] = extract_results(fit_glmmTMB, confs, beta, beta0)
          }, silent = TRUE)  
          
          # lm
          try({
            fit_lm = lm(y ~ x + group)
            summ = summary(fit_lm)
            confs = confint(fit_lm)
            results_wo_lm[experiment, ] = c(summ$coefficients["x", 1], summ$coefficients["x", 4], summ$coefficients["x", 2], as.integer(beta > confs["x",1] & beta < confs["x",2]))
          }, silent = TRUE)  
          
          # lm w/o grouping
          try({
            fit_lm = lm(y ~ x)
            summ = summary(fit_lm)
            confs = confint(fit_lm)
            results_wo_lm_wo_grouping[experiment, ] = c(summ$coefficients["x", 1], summ$coefficients["x", 4], summ$coefficients["x", 2], as.integer(beta > confs["x",1] & beta < confs["x",2]))
          }, silent = TRUE)  
        }
        
        return(list(results_w_lme4_ml = data.frame(results_w_lme4_ml), 
                    results_wo_lme4_ml = data.frame(results_wo_lme4_ml),
                    results_w_glmmTMB_ml = data.frame(results_w_glmmTMB_ml), 
                    results_wo_glmmTMB_ml = data.frame(results_wo_glmmTMB_ml),
                    results_w_lm = data.frame(results_w_lm),
                    results_wo_lm = data.frame(results_wo_lm),
                    results_w_lme4_reml = data.frame(results_w_lme4_reml), 
                    results_wo_lme4_reml = data.frame(results_wo_lme4_reml),
                    results_w_glmmTMB_reml = data.frame(results_w_glmmTMB_reml), 
                    results_wo_glmmTMB_reml = data.frame(results_wo_glmmTMB_reml),
                    results_w_lm_wo_grouping = data.frame(results_w_lm_wo_grouping),
                    results_wo_lm_wo_grouping = data.frame(results_wo_lm_wo_grouping)
                    ))
      })
  })
  saveRDS(result_list, file = paste0("Results/results_mountain_lmm_random_intercept_only_", sd_re, "_.Rds"))
}



