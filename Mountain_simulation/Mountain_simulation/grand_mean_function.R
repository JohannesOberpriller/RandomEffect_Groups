grand_mean = function(fit, mountain, beta, t = TRUE, intercept=FALSE) {
  
  if(!intercept)ind = (mountain+1):(2*(mountain))
  else ind = 1:mountain
  mean = coef(fit)[ind]
  V = vcov(fit)[ind, ind]
  
  SS = as.vector(mvtnorm::rmvnorm(2000, mean = mean, sigma = V))
  eff = mean(SS)
  se = sd(SS)/sqrt(mountain-1)
  
  if(t) {
    p_value = 2*pt(abs(eff/se),df=mountain-1, lower.tail = FALSE)
    bound = qt(0.975, df = mountain-1)
  } else {
    p_value = 2*pnorm(abs(eff/se),lower.tail = FALSE)
    bound = 1.96
  }
  confs = cbind( eff - bound*se, eff + bound*se)
  return(c(eff, p_value, se, as.integer(beta > confs[1,1] & beta < confs[1,2])))
}
