source("EOMPM_functions_evaluation.R")

### FUNCTION - training_applied()
#   Modified training function from simulation: True performances cannot be
#   calculated from population data and are hence unknown (set to 0). The output
#   from model evaluation is hence not meaningful, i.e. test decisions cannot
#   be assessed to be correct or false etc.
training_applied <- function(learn,
                             nlambda=20,
                             cv.metric = "class",
                             cv.folds= 10,
                             family="binomial",
                             alpha.num = 5,
                             standardize=TRUE){
  train.data <- learn
  alpha.en <- rev(seq(1,0, length.out=alpha.num))
  model.list <- list()
  ids <- rep(1:cv.folds, each=nrow(train.data)/cv.folds)
  ids <- c(ids, 1:(nrow(train.data)-length(ids)))
  fold.id <- sample(ids, size=nrow(train.data), replace=FALSE)
  for(i in 1:alpha.num){
    model.list[[i]] <- train.model.en(train=train.data,
                                      nlambda=nlambda,
                                      cv.metric = cv.metric,
                                      fold.id = fold.id,
                                      family = family,
                                      alpha.en = alpha.en[[i]],
                                      standardize=standardize)
    model.list[[i]]$alpha <- alpha.en[[i]]
    model.list[[i]]$theta <- 0
  }
  result.list <- lapply(model.list, mod2dt, convert="oneminus")
  result <- rbindlist(result.list)
  return(list(result=result, models=model.list))
}
