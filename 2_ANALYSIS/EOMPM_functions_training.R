source("EOMPM_functions_data.R")

### FUNCTION - misc
#   Various helper functions
sym_diff <- function(a,b){setdiff(union(a,b), intersect(a,b))}

retain <- function(x){x}
oneminus <- function(x){1-x}

logit.fun <- function(x){log(x/(1-x))}
logistic.fun <- function(x){1/(1+exp(-x))}

arcsin_sqrt.fun <- function(x){asin(sqrt(x))}
square_sin.fun<- function(x){sin(x)^2}

### FUNCTION - findIndex()
#   Helper function for correct connection of different simulation phases.
findIndex <- function(test, compare, exclude){
  require(data.table)
  require(dplyr)
  missCols <- sym_diff(names(compare), names(test))
  if(length(intersect(missCols, names(test))) > 0){
    compare[, intersect(missCols, names(test))] <- -9.99
  } 
  if(length(intersect(missCols, names(compare))) > 0){
    test[, intersect(missCols, names(compare))] <- -9.99
  } 
  test %>% setcolorder(names(compare))
  rr <- rep(1,nrow(compare))
  testL <- test[rr, ]
  logi.dt <- data.table(testL == compare)
  logi.dt[, eval(intersect(c(missCols, exclude), names(logi.dt))):=NULL]
  model.index <- which(apply(logi.dt, 1, function(x)all(x)))
  return(model.index)
}

### FUNCTION - mod2dt()
#   Helper function which transforms model objects into data.table which
#   summarizes major properties.
mod2dt <- function(model, convert){
  require(data.table)
  out <- data.table(alpha.en = model$alpha, 
                    lambda.en = model$lambda,
                    thetahat.cv = do.call(convert, list(x=model$cvm)),
                    se.cv = model$cvsd,
                    nzero = model$nzero,
                    theta = model$theta)
  return(out)
}

### FUNCTION - train.model.en()
#   Allows model training and cross-validation via glmnet.cv() for single
#   value of alpha.en (= mixture term: alpha.en=1 => LASSO, alpha.en=0 => RIDGE)
train.model.en <- function(train,             # learning data
                           nlambda=20,        # number of lambdas per alpha.en
                           cv.metric='class', # set to 'class' for accuracy 
                           fold.id,           # vector of length nrow(train)
                           family='binomial', # set to 'binomial' for binary Y
                           alpha.en=1,        # mixture penalty term (see above)
                           standardize=FALSE, # F: do not standardize features
                           ...){
  require(glmnet)
  require(data.table)
  require(Metrics)
  
  n <- nrow(train)
  p <- ncol(train)
  eps <- ifelse(n>p, 0.0001, 0.01) # see ?glmnet -> eps = 'lambda.min.ratio'
  # (1) calc max.lambda:
  max.lambda <- glmnet(train[,-1], train[,1],
                       family=family,
                       alpha=alpha.en, # alpha=1 <=> LASSO, alpha=0 <=> RIDGE
                       standardize = standardize, 
                       nlambda=nlambda)$lambda[1]
  # (2) manually create actual lambda sequence to be used:
  lambda.seq <- seq(max.lambda, eps*max.lambda, length.out = nlambda)
  # (3) model fit involving cross-validated error/performance estimates:
  model.cv <- cv.glmnet(train[,-1], train[,1], family=family,
                        alpha=alpha.en, # alpha=1 <=> LASSO, alpha=0 <=> RIDGE
                        standardize = standardize, 
                        lambda=lambda.seq,
                        type.measure = cv.metric,
                        foldid = fold.id)
  model.cv$alpha = alpha.en
  return(model.cv)
}

### FUNCTION - training()
#   Main function for learning phase, i.e. model training and cross-validation
training <- function(data=NULL,          # results from 'population' phase
                     job=NULL,           # batchtools argument
                     instance,           # batchtools argument (training data)
                     nlambda=20,  
                     cv.metric='class', 
                     cv.folds=10,        # number of CV folds
                     family='binomial',
                     alpha.num=5,        # number of different alpha.en in [0,1]
                     standardize=FALSE,
                     compare='accuracy', # function: compare(actual, predicted)
                     convert='oneminus', # set 'oneminus' 
                     ...){
  ### (0) Population data
  pop.data <- data$data
  pop.args <- data$args

  ### (1) Generate training data
  train.data <- instance$data
  train.args <- instance$args
  
  ### (2) Train all EN models
  alpha.en <- rev(seq(1,0, length.out=alpha.num))
  model.list <- list()
  fold.id <- sample(rep(1:cv.folds, each=nrow(train.data)/cv.folds), size=nrow(train.data), replace=FALSE)
  for(i in 1:alpha.num){
    model.list[[i]] <- train.model.en(train=train.data,
                                      nlambda=nlambda,
                                      cv.metric = cv.metric,
                                      fold.id = fold.id,
                                      family = family,
                                      alpha.en = alpha.en[[i]],
                                      standardize=standardize)
  }
  excl.str <- c("n", "pert.switch", "pert.diff", "pert.modify", "pert.mult", "beta.seed", "pert.seed",
                "X.seed", "Y.seed")
  if(train.args$beta.dist != "const"){excl.str <- c(excl.str, "beta.seed")}
  ### (3) Calculate true performances
  for(i in 1:length(model.list)){
    model <- model.list[[i]] 
    p.index <- findIndex(train.args, pop.args, exclude = excl.str)
    ## -> beta seed needs to be the same (doesnt matter for const coeffs)
    ## -> pertubation params needs to be ignored (checking performance on true dataset)
    if(length(p.index)>1)stop("Found more than one ground truth population!")
    pop <- pop.data[[p.index]]
    pop.x <- pop[,-1]
    pop.y <- pop[,1]
    pred <- predict(model, newx=pop.x, type=cv.metric, s=model$lambda) # matrix (ntruth x nlambda)
    if(is.character(pred[1,1])){class(pred) <- "integer"}
    theta <- apply(pred, 2, function(predicted){
      do.call(compare, list(actual=pop.y, predicted=predicted))})
    model.list[[i]]$theta <- theta
  }
  ### (4) Return results as data.table
  result.list <- lapply(model.list, mod2dt, convert=convert)
  result <- rbindlist(result.list)
  #result <- result[order(-theta)]
  return(list(result=result, 
              model.list=model.list))   
}






