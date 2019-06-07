source("EOMPM_functions_training.R")

### FUNCTION - acc.cov()
#   Estimation of covariance (of performance estimates).
acc.cov <- function(actual,      # true labels
                    pred,        # predicted labels matrix with dim n,M
                    thetahat,    # performance estimates of length M
                    corr=FALSE){ # T: output correlation matrix
  n <- length(actual)
  M <- ncol(pred)
  if( (n != nrow(pred)) | (length(thetahat)!=M) ){
    stop("acc.cov: Wrong dimensions!")}
  if(M==1){
    var <- thetahat*(1-thetahat)/n
    R <- matrix(var)
  }
  if(M > 1){
    Q <- apply(pred, 2, function(p){p==actual}) - matrix(thetahat, nrow=n, ncol=M, byrow=T)
    R <- cov(Q)/n # scale variance appropiatly
  }
  if(corr){
    R <- cov2cor(R)
  }
  return(R)
}

### FUNCTION - estimation1()
#   Apply prediction models to evaluation data and estimate performances and 
#   covariance matrix
estimation1 <- function(models,               # list of models
                        test,                 # test data
                        cv.metric='class',    # 'class' for binary classification
                        compare='accuracy',   # performance measure
                        convert='oneminus'){  # transformation of cv-estimates
  require(glmnet)
  require(data.table)
  require(Metrics)
  # models is list of cv.glmnet objects
  modDT.list <- list()
  pred.list <- list()
  for(m in 1:length(models)){
    mod <- models[[m]]
    pred <- predict(mod, test[,-1], s=mod$lambda, type=cv.metric)
    if(is.character(pred[1,1])){class(pred) <- "integer"}
    pred.list[[m]] <- data.table(t(pred))
    thetahat <- apply(pred, 2, function(predicted){
      do.call(compare, list(actual=test[,1], predicted=predicted))})
    modDT <- mod2dt(model=mod, convert=convert)
    modDT <- merge(modDT, data.frame(lambda.en = mod$lambda, thetahat.test = thetahat), by="lambda.en")
    modDT %>% setorder(-lambda.en)
    modDT.list[[m]] <- modDT
  }
  modDTC <- rbindlist(modDT.list)
  predC <- rbindlist(pred.list)
  predC <- t(predC)
  COV <- acc.cov(actual = test[,1], pred = predC, thetahat=modDTC$thetahat.test, corr=FALSE)
  return(list(models = modDTC,
              covariance = COV))
}

### FUNCTION - estimation()
#   Apply prediction models to evaluation data and estimate performances and 
#   covariance matrix. Wrapper for estimation1().
estimation <- function(data=NULL,          # contains data from learning phase
                       job=NULL,           # batchtools argument
                       instance,           # contains test data
                       type='class',       # 'class' for binary classification
                       compare='accuracy', # performance measure
                       convert='oneminus', # transformation of cv-estimates
                       ...){
  require(glmnet)
  require(data.table)
  require(Metrics)
  test.data <- instance$data
  test.args <- instance$args
  
  models.list <- data$models
  results.list <- data$results
  JP <- data$jobpars
  
  excl.str <- c("n", "reset.seeds", "reset.pert")
  
  index.list <- findIndex(test=test.args, compare=JP, exclude=excl.str)
  if(length(index.list)==0 )stop("No models with fitting parameters found!")
  
  return.list <- list()
  for(i in 1:length(index.list)){
    m <- index.list[i]
    models <- models.list[[m]]
    return.list[[i]] <- estimation1(models, test.data, cv.metric=type,
                                    compare=compare, convert=convert)
  }
  return(return.list)
}
