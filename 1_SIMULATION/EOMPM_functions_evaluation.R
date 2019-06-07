source("EOMPM_functions_estimation.R")

### FUNCTION - load_result()
#   Used to load single results from estimation step.
load_result <- function(index,                               # registry index
                        data=NULL,                           # batchtools arg
                        job=NULL,                            # batchtools arg
                        wd=getwd(),                          # working directory
                        subdir=paste0('/DATA/',sim.chr,'/'), # subdirectory
                        regname='registry_estimation'){      # registry name
  require(batchtools)
  reg.dir <- paste0(wd, subdir, regname)
  reg <- loadRegistry(reg.dir, writeable = TRUE)
  models <- reduceResultsList(ids=index, fun=function(x){x[[1]]$models},
                              reg=reg)[[1]]
  covariance <- reduceResultsList(ids=index, fun=function(x){x[[1]]$covariance},
                                  reg=reg)[[1]]
  jobpars <- getJobPars(ids=index, reg=reg)
  return(list(models=models,
              covariance=covariance,
              jobpars=jobpars))
}

### FUNCTION - evaluation()
#   Main function for evaluation phase- Permorm model selection according to
#   specified rule and inference via multiple test for selected models.
evaluation <- function(data=NULL,                 # batchtools argument
                       job=NULL,                  # batchtools argument
                       instance,                  # test data 
                       inf.transform = "none",    # transformation of estimates
                       inf.method= "maxT",        # multiple test
                       inf.alpha=0.05,            # significance level
                       inf.tail="lower.tail",     # type of statistical test
                       select.method = "default", # selection method
                       select.k=NULL,             # selection hyperparameter 
                       select.q=NULL,             # selection hyperparameter 
                       ...){
  require(data.table)
  require(dplyr)
  result <- list()
  if("models" %in% names(instance)){ instance <- list(instance)}
  for(i in 1:length(instance)){
    fullMod <- instance[[i]]$models
    fullCov <- instance[[i]]$covariance
    JP <- instance[[i]]$jobpars
    if("prob.pars" %in% names(JP)){ 
      JP <- unwrap(JP) # -> protect against batchtools change
    }
    n <- JP$n
    # prevent zero variance estimates
    oz <- which(fullMod$thetahat.test %in% c(0,1))
    fullCov[oz, oz] <- ((1-1/n)*(1/n))/n
    
    theta_opt <- max(fullMod$theta)
    delta <- (-5:10)/100
    theta0 <- theta_opt - delta
    
    selected <- selection(models = fullMod, theta0.list = theta0,
                          method = select.method, k=select.k, q=select.q)

    Mod.list <- lapply(selected, function(s){fullMod[s, ]})
    Cov.list <- lapply(selected, function(s){fullCov[s,s]})
    
    infDT <- inference(models=Mod.list[[1]], covariance=Cov.list[[1]], n = n,
                       theta0=theta0, transform=inf.transform, 
                       alpha = inf.alpha, tail=inf.tail)
    
    infDT <- infDT %>% mutate(theta_opt = theta_opt, delta = delta)
    infDT %>% setcolorder(c("theta_opt","delta","theta0_assumed","lower", 
                            "reject","thetahat","thetahat.median","theta", 
                            "theta_opt_sel", "Mstar","CR", "FR", "CA", "FA"))
    result[[i]] <- infDT
  }
  ## return result (as data.table if possible)
  if(length(result)==1){
    result <- result[[1]]}
  return(result) 
}

### FUNCTION - inference()
#   Given model selection and performance and covariance estimates, 
#   apply (multiple) statistical test.
inference <- function(models,             # model information
                      covariance,         # covariance matrix
                      n,                  # evluation sample size
                      theta0,             # performance threshold
                      transform="none",   # transformation of estimates 
                      alpha=0.05,         # significance level
                      tail="lower.tail"){ # type of test
  require(mvtnorm)
  require(dplyr)
  require(data.table)
  thetahat <- models$thetahat.test
  theta <- models$theta
  if(class(covariance) != 'matrix') covariance <- matrix(covariance)
  
  ## untransformed
  se <- sqrt(thetahat*(1-thetahat)/n) 
    
  if(transform=="logit"){
    J <- 1/(thetahat*(1-thetahat))
    se <- se*J          
    thetahat <- logit.fun(thetahat)
  }
  
  ## make sure proper correlation matrix is used
  R <- cov2cor(covariance)
  ## calculate critical value
  Q <- qmvnorm(p = 1-alpha, tail=tail, mean=0, sigma=R)$quantile
  lower <- thetahat - Q*se
  ## point estimator of median
  Q2 <- qmvnorm(p = 0.5, tail=tail, mean=0, sigma=R)$quantile
  thetahat.median <- thetahat - Q2*se

  if(transform=="logit"){ #back transform
    lower <- logistic.fun(lower)
    thetahat <- logistic.fun(thetahat)
    thetahat.median <- logistic.fun(thetahat.median)
  }
  
  # count correct/false rejections/accepts of H0:
  CR <- sapply(theta0, function(t0){sum(lower >  t0 & theta >  t0)})
  FR <- sapply(theta0, function(t0){sum(lower >  t0 & theta <= t0)})
  CA <- sapply(theta0, function(t0){sum(lower <= t0 & theta <= t0)})
  FA <- sapply(theta0, function(t0){sum(lower <= t0 & theta >  t0)})
  
  # final model:
  index_winner <- which.max(lower)
  
  result <- data.table(theta0_assumed = theta0, lower = max(lower),
                       CR, FR, CA, FA)
  result <- result %>% mutate(Mstar = nrow(models),
                    reject = lower > theta0_assumed,
                    thetahat = thetahat[index_winner],
                    thetahat.median = thetahat.median[index_winner],
                    theta = models$theta[index_winner],
                    theta_opt_sel = max(models$theta))
  return(result)
}

### FUNCTION - selection()
#   Wrapper for selection1() used to apply selection rules.
selection <- function(models,
                      theta0.list=NULL, # data.frame with model information
                      method="default", # selection rule
                      k = NULL,         # for method close2opt [= within c SE]
                      q = NULL,         # for method quantile [= best 100*q %]
                      ...){
  selected1 <- selection1(models=models, theta0=NULL, 
                          method=method, k=k, q=q)
  selected <- list(selected1)[rep(1, length(theta0.list))]
  return(selected=selected)
}

### FUNCTION - selection1()
#   Apply selection rule based on cross-validation estimates from learning phase.
selection1 <- function(models=models,    # data.frame with model information
                       method='default', # selection rule
                       k = NULL,         # for method close2opt [= within c SE]
                       q = NULL,         # for method quantile [= best 100*q %]
                       ...){
  thetahat.cv <- models$thetahat.cv
  se.cv <- models$se.cv
  if(method=="oracle"){
    return(which.max(models$theta)[1])
  }
  if(method=="default"){
    return(which.max(thetahat.cv)[1])
  }
  if(method=="close2opt"){
    index.opt <- which.max(thetahat.cv)
    co <- thetahat.cv[index.opt] - k*se.cv[index.opt]
    return(which(thetahat.cv >= co))
  }
  if(method=="quantile"){
    co <- quantile(thetahat.cv, probs=1-q)
    return(which(thetahat.cv >= co))
  }
}
