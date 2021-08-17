library(lme4)
library(lmerTest)
library(snow)
library(glmmTMB)
set.seed(1)

## Note:
#The first simulation (Simulation of LMM with Temperature effect) tests if the effect is significant when there is an effect 
#The second simulation (Simulation of LMM without Temperature effect) tests if the effect is significant, when there is an effect

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
      attr(summary(fit_lmm)$varcor$group.1, "stddev")["x"],
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
      attr(summary(fit_lmm)$varcor$cond$group.1,"stddev")["x"],
      as.integer(!fit_lmm$sdr$pdHess),
      as.integer(beta0 > confs["(Intercept)",1] & beta0 < confs["(Intercept)",2]),
      as.integer(beta > confs["x",1] & beta < confs["x",2])))
}


grand_mean_metafor = function(fit, mountain, beta, fixed = FALSE, tresh = FALSE, method = "REML") {
  ind = (mountain+1):(2*mountain)
  V = vcov(fit)[ind, ind]
  if(tresh) V[V < 0.0001] = 0
  id = as.factor(1:mountain)
  if(!fixed) meta = metafor::rma.mv(coef(fit)[ind], V = V, test = "t", method = method, random = ~ 1 | id)
  else meta = metafor::rma.mv(coef(fit)[ind], V = V, test = "t", method = method)
  return(c(meta$beta, meta$pval, meta$se, as.integer(beta > meta$ci.lb & beta < meta$ci.ub)))
}

grand_mean_metafor_uni = function(fit, mountain, beta, fixed = FALSE, method = "REML") {
  ind = (mountain+1):(2*mountain)
  if(fixed) meta = metafor::rma.uni(coef(fit)[ind], vi = diag(vcov(fit)[ind, ind]), test = "t", method = "FE")
  else meta = metafor::rma.uni(coef(fit)[ind], vi = diag(vcov(fit)[ind, ind]), test = "t", method = method)
  return(c(meta$beta, meta$pval, meta$se, as.integer(beta > meta$ci.lb & beta < meta$ci.ub)))
}

# set up the cluster and export variables as well as functions to the cluster 

source("Mountain_simulation/grand_mean_function.R")

var = runif(1000, 0.0001, 4.0)
mountains = sample.int(19, 1000, replace = TRUE)+1
nobs = sample.int(491, 1000, replace = TRUE)+9
balanced = runif(1000, 0, 0.9)


parameter = data.frame(var = var, moutain = mountains, nobs = nobs, balanced = balanced, min = NA, max = NA)


cl = snow::makeCluster(64L)
snow::clusterEvalQ(cl, {library(lme4); library(lmerTest)})
snow::clusterExport(cl, list("extract_results","extract_results_t", "grand_mean", "parameter", "grand_mean_metafor", "grand_mean_metafor_uni"), envir = environment())


result_list = 
  snow::parLapply(cl, 1:nrow(parameter), function(p)  {
    
    
    sd_re = sqrt(parameter[p,]$var)
    n_each <- parameter[p,]$nobs
    n_groups = number_groups = parameter[p,]$moutain
    balanced = parameter[p,]$balanced
    
    
    range = (1 - balanced)/2
    continue = TRUE
    while(continue) {
      prob = runif(n_groups, range, 1-range)
      g <- sample.int(n_groups, n_each*n_groups, replace = TRUE, prob = prob)
      if(min(table(g)) > 2) continue = FALSE
    } # Grouping variable (mountain range)
    group = as.factor(g)
    min_max = c(min(table(g)), max(table(g)))
    parameter[p,]$min = min_max[1]
    parameter[p,]$max = min_max[2]
    
    # set up matrices to store the results of the different runs 
    n_tries = 1000
    results_w_lme4_reml = results_wo_lme4_reml = matrix(nrow = n_tries, ncol = 11)
    colnames(results_w_lme4_reml) = c("estimate_intercept","estimate_effect", 
                                    "p_value_intercept", "p_value_effect",
                                    "se_intercept", "se_effect",
                                    "stddev_randeff_inter","stddev_randeff_x", "Singularity",
                                    "Int_in_conf",  "Slope_in_conf")
    colnames(results_wo_lme4_reml) = colnames(results_w_lme4_reml)
    
    
    results_w_lm = results_wo_lm = results_w_lm_GM = results_wo_lm_GM = results_w_lm_Uni = results_wo_lm_Uni = 
      results_w_lm_Uni_FE = results_wo_lm_Uni_FE = results_w_lm_Uni_MLE = results_wo_lm_Uni_MLE= 
      results_w_lm_FE = results_wo_lm_FE = results_w_lm_MLE = results_wo_lm_MLE = matrix(nrow = n_tries, ncol = 4)
    colnames(results_w_lm) = c("estimate_effect", "p_value_effect","se_effect", "Slope_in_conf")
    colnames(results_wo_lm) = colnames(results_w_lm)
    colnames(results_wo_lm_GM) = colnames(results_w_lm)
    colnames(results_w_lm_GM) = colnames(results_w_lm)
    colnames(results_wo_lm_Uni) = colnames(results_w_lm)
    colnames(results_w_lm_Uni) = colnames(results_w_lm)
    colnames(results_wo_lm_Uni_MLE) = colnames(results_w_lm)
    colnames(results_w_lm_Uni_MLE) = colnames(results_w_lm)
    colnames(results_wo_lm_Uni_FE) = colnames(results_w_lm)
    colnames(results_w_lm_Uni_FE) = colnames(results_w_lm)
    colnames(results_wo_lm_MLE) = colnames(results_w_lm)
    colnames(results_w_lm_MLE) = colnames(results_w_lm)
    colnames(results_wo_lm_FE) = colnames(results_w_lm)
    colnames(results_w_lm_FE) = colnames(results_w_lm)
    
    nonSing1 = 1
    nonSing2 = 1
    abort1 = 1
    abort2 = 1
    counter = 1
    while((nonSing1 < (n_tries+1) ) & nonSing2 < (n_tries+1)){
      abort1 = abort1 + 1
      abort2 = abort2 + 1
      if((abort1 == 20000) & (abort2 == 20000)) break
      
      ################  The data generating process ################ 
      # Number of overall observations is equal to 50 observations for each mountain range 
      n = (number_groups)*n_each
      
      ### Raw environmental predictors (Intercept and Temperature) ###
      # the temperature is drawn from a uniform distribution as we want to simulate 
      # an altidudinal gradient and the intercept is kept constant at 1 
      x <- runif(n, -1, 1) # Temperature
      X <- matrix(c(rep(1, n), x), nrow = n) # Intercept, Temperature
      
      sd_randeff = sd_re # sd for random effects
      
      ################ Simulation of LMM with Temperature effect  ################ 
      # fixed effects
      beta = 0.4    # Temperature effect meaning when the temperature increases 1 degree the respective variable also does 
      beta0 = 10.0   # Intercept: the mean height of a reporductive plant  
      
      # random effect sizes are sampled around the fixed effects with no correlation between the 
      # random intercept and random slope
      randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)  # random intercept
      randslope <- rnorm(n_groups, mean = beta, sd = sd_randeff)      # random slope
      
      # calculate linear response, different intercept and slope for each mountain range
      mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],randslope[g[i]]))
      
      # sample random error from a Normal distribution
      sigma <- 0.5
      y <- rnorm(n, mu, sd = sigma) 
      
      
      ################ Fitting: lme4, glmmTMB, and LM ################ 
      # we tested MLE (maximum likelihood estimation) and REML (restricted maximum likelihood estimation) for glmmTMB and lme4
      
      # lme4 - REML
      if(nonSing1 < (n_tries+1)) {
        try({
          fit_lmm <- lmer(y ~ x  + (1|group) + (0+x | group), REML = TRUE) 
          summ = summary(fit_lmm)
          
          # calculate confidence intervals for the temperature effect
          confs = rbind(cbind(summ$coefficients[1,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"], 
                              summ$coefficients[1,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"]),
                        cbind(summ$coefficients[2,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"], 
                              summ$coefficients[2,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"])
          )
          rownames(confs) = c("(Intercept)", "x")
          results_w_lme4_reml[nonSing1, ] = extract_results_t(fit_lmm, confs, beta, beta0)
        }, silent = TRUE)
        
        if(results_w_lme4_reml[nonSing1, ][9] < 1.0) {
          # linear model w/ mountain range as grouping variable
          nonSing1 = nonSing1+1
          
        }
      }
      
      if(counter <= n_tries) {
        try({
          fit_lm = lm(y ~ 0+x*group-x)
          results_w_lm[counter, ] = grand_mean_metafor(fit_lm, n_groups, beta = beta)
          results_w_lm_MLE[counter, ] = grand_mean_metafor(fit_lm, n_groups, beta = beta, method = "ML", fixed = FALSE)
          results_w_lm_FE[counter, ] = grand_mean_metafor(fit_lm, n_groups, beta = beta, fixed = TRUE)
          results_w_lm_GM[counter, ] = grand_mean(fit_lm, n_groups, beta = beta)
          results_w_lm_Uni[counter, ] = grand_mean_metafor_uni(fit_lm, n_groups, beta = beta)
          results_w_lm_Uni_MLE[counter, ] = grand_mean_metafor_uni(fit_lm, n_groups, beta = beta, method = "ML")
          results_w_lm_Uni_FE[counter, ] = grand_mean_metafor_uni(fit_lm, n_groups, beta = beta, method = "FE")
        }, silent = TRUE)
      }
        
        
        ################ Simulation of LMM without Temperature effect  ################ 
        # fixed effects
        beta0 = 10.0  # Intercept
        beta = 0.0    # Temperature, now set to zero
        
        # random intercept
        randintercep <- rnorm(n_groups, mean = beta0, sd = sd_randeff)  # random intercept
        randslope <- rnorm(n_groups, mean = beta, sd = sd_randeff)      # random slope
        
        # calculate linear response, different intercept and slope for each mountain range
        mu <- sapply(1:n, FUN = function(i) X[i,] %*% c(randintercep[g[i]],randslope[g[i]]))
        
        # sample random errors from a Normal distribution
        sigma <- 0.5
        y <- rnorm(n, mu, sd = sigma) 
        
        
        ################ Fitting: lme4, glmmTMB, and LM ################ 
        
        # lme4 - REML
        if(nonSing2 < (n_tries+1)) {
          try({
            fit_lmm <- lmer(y ~ x  + (1|group) + (0+x | group), REML = TRUE) 
            summ = summary(fit_lmm)
            
            # calculate confidence intervals for the temperature effect
            confs = rbind(cbind(summ$coefficients[1,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"], 
                                summ$coefficients[1,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][1])*summ$coefficients[1,"Std. Error"]),
                          cbind(summ$coefficients[2,"Estimate"] - qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"], 
                                summ$coefficients[2,"Estimate"] + qt(0.975, df = summ$coefficients[,"df"][2])*summ$coefficients[2,"Std. Error"])
            )
            rownames(confs) = c("(Intercept)", "x")
            results_wo_lme4_reml[nonSing2, ] = extract_results_t(fit_lmm, confs, beta, beta0)
          }, silent = TRUE)
          
          
          # linear model w mountain range as grouping variable
          if(results_wo_lme4_reml[nonSing2, ][9] < 1.0) {
            nonSing2 = nonSing2 + 1
          }
        }
          if(counter <= n_tries) {
            try({
              fit_lm = lm(y ~ 0+x*group-x)
              results_wo_lm[counter, ] = grand_mean_metafor(fit_lm, n_groups, beta = beta)
              results_wo_lm_MLE[counter, ] = grand_mean_metafor(fit_lm, n_groups, beta = beta, fixed = FALSE, method = "ML")
              results_wo_lm_FE[counter, ] = grand_mean_metafor(fit_lm, n_groups, beta = beta, fixed = TRUE)
              results_wo_lm_GM[counter, ] = grand_mean(fit_lm, n_groups, beta = beta)
              results_wo_lm_Uni[counter, ] = grand_mean_metafor_uni(fit_lm, n_groups, beta = beta)
              results_wo_lm_Uni_MLE[counter, ] = grand_mean_metafor_uni(fit_lm, n_groups, beta = beta, method = "ML")
              results_wo_lm_Uni_FE[counter, ] = grand_mean_metafor_uni(fit_lm, n_groups, beta = beta, method = "FE")
            }, silent = FALSE)
          }
        counter = counter + 1
        
    }
    
    results_wo_lme4_reml = data.frame(results_wo_lme4_reml)
    results_w_lme4_reml = data.frame(results_w_lme4_reml)
    results_wo_lm = data.frame(results_wo_lm)
    results_w_lm = data.frame(results_w_lm)
    results_wo_lm_GM = data.frame(results_wo_lm_GM)
    results_w_lm_GM = data.frame(results_w_lm_GM)
    results_wo_lm_Uni = data.frame(results_wo_lm_Uni)
    results_w_lm_Uni = data.frame(results_w_lm_Uni)
    results_wo_lm_Uni_FE = data.frame(results_wo_lm_Uni_FE)
    results_w_lm_Uni_FE = data.frame(results_w_lm_Uni_FE)
    results_wo_lm_Uni_MLE = data.frame(results_wo_lm_Uni_MLE)
    results_w_lm_Uni_MLE = data.frame(results_w_lm_Uni_MLE)
    
    results_wo_lm_FE = data.frame(results_wo_lm_FE)
    results_w_lm_FE = data.frame(results_w_lm_FE)
    results_wo_lm_MLE = data.frame(results_wo_lm_MLE)
    results_w_lm_MLE = data.frame(results_w_lm_MLE)
    
    TypeOneLMM = mean(results_wo_lme4_reml$p_value_effect < 0.05, na.rm=TRUE)
    TypeOneLM = mean(results_wo_lm$p_value_effect < 0.05, na.rm=TRUE)
    TypeOneLM_GM = mean(results_wo_lm_GM$p_value_effect < 0.05, na.rm=TRUE)
    TypeOneLM_Uni = mean(results_wo_lm_Uni$p_value_effect < 0.05, na.rm=TRUE)
    TypeOneLM_Uni_FE = mean(results_wo_lm_Uni_FE$p_value_effect < 0.05, na.rm=TRUE)
    TypeOneLM_Uni_MLE = mean(results_wo_lm_Uni_MLE$p_value_effect < 0.05, na.rm=TRUE)
    TypeOneLM_FE = mean(results_wo_lm_FE$p_value_effect < 0.05, na.rm=TRUE)
    TypeOneLM_MLE = mean(results_wo_lm_MLE$p_value_effect < 0.05, na.rm=TRUE)
    
    PowerLMM = 1-mean(results_w_lme4_reml$p_value_effect > 0.05, na.rm=TRUE)
    PowerLM = 1-mean(results_w_lm$p_value_effect > 0.05, na.rm=TRUE)
    PowerLM_GM = 1-mean(results_w_lm_GM$p_value_effect > 0.05, na.rm=TRUE)
    PowerLM_Uni = 1-mean(results_w_lm_Uni$p_value_effect > 0.05, na.rm=TRUE)
    PowerLM_Uni_FE = 1-mean(results_w_lm_Uni_FE$p_value_effect > 0.05, na.rm=TRUE)
    PowerLM_Uni_MLE = 1-mean(results_w_lm_Uni_MLE$p_value_effect > 0.05, na.rm=TRUE)
    PowerLM_FE = 1-mean(results_w_lm_FE$p_value_effect > 0.05, na.rm=TRUE)
    PowerLM_MLE = 1-mean(results_w_lm_MLE$p_value_effect > 0.05, na.rm=TRUE)
    
    CoverLMM = mean(results_w_lme4_reml$Slope_in_conf , na.rm=TRUE)
    CoverLM = mean(results_w_lm$Slope_in_conf, na.rm=TRUE)
    CoverLM_GM = mean(results_w_lm_GM$Slope_in_conf, na.rm=TRUE)
    CoverLM_Uni = mean(results_w_lm_Uni$Slope_in_conf, na.rm=TRUE)
    CoverLM_Uni_MLE = mean(results_w_lm_Uni_MLE$Slope_in_conf, na.rm=TRUE)
    CoverLM_Uni_FE = mean(results_w_lm_Uni_FE$Slope_in_conf, na.rm=TRUE)
    CoverLM_MLE = mean(results_w_lm_MLE$Slope_in_conf, na.rm=TRUE)
    CoverLM_FE = mean(results_w_lm_FE$Slope_in_conf, na.rm=TRUE)
    
    return(cbind(parameter[p,], data.frame(TypeOneLMM = TypeOneLMM, 
                                           TypeOneLM = TypeOneLM, 
                                           TypeOneLM_GM = TypeOneLM_GM,
                                           TypeOneLM_Uni = TypeOneLM_Uni,
                                           TypeOneLM_Uni_FE = TypeOneLM_Uni_FE,
                                           TypeOneLM_Uni_MLE = TypeOneLM_Uni_MLE,
                                           TypeOneLM_FE = TypeOneLM_FE,
                                           TypeOneLM_MLE = TypeOneLM_MLE,
                                           PowerLMM = PowerLMM, 
                                           PowerLM = PowerLM, 
                                           PowerLM_GM = PowerLM_GM,
                                           PowerLM_Uni = PowerLM_Uni,
                                           PowerLM_Uni_FE = PowerLM_Uni_FE,
                                           PowerLM_Uni_MLE = PowerLM_Uni_MLE,
                                           PowerLM_FE = PowerLM_FE,
                                           PowerLM_MLE = PowerLM_MLE,
                                           CoverLMM = CoverLMM, 
                                           CoverLM = CoverLM,
                                           CoverLM_GM = CoverLM_GM,
                                           CoverLM_Uni = CoverLM_Uni,
                                           CoverLM_Uni_FE = CoverLM_Uni_FE,
                                           CoverLM_Uni_MLE = CoverLM_Uni_MLE,
                                           CoverLM_FE = CoverLM_FE,
                                           CoverLM_MLE = CoverLM_MLE
                                           )))
})

results = do.call(rbind, result_list)
saveRDS(results, "Results/C_results_mountain_unbalanced.RDS")
