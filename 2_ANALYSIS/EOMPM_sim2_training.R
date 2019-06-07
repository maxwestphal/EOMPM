### Training data registry:
train.dir <- paste0(getwd(), "/DATA/", sim.chr, "/registry_training")
train.reg <- makeExperimentRegistry(file.dir = train.dir, seed = 1) 
# -> should only be used initially and throws an error if registry already exists
train.reg <- loadRegistry(train.dir, writeable = T)
nc <- detectCores()
train.reg$cluster.functions <- makeClusterFunctionsSocket(nc-1) ## set core number
train.reg$source <- paste0(getwd(), "/EOMPM_functions_training.R")

# Define relevant parameters for sample generation:
prob2 <- list(generate_data = params2)
algo2 <- list(training = args2)

# Add problems / algorithms:
addProblem(name="generate_data", data=RESULTS1, fun=generate_data, seed=1, reg=train.reg)
addAlgorithm(name = "training", fun = training, reg=train.reg)

# Add experiments:
addExperiments(prob2, algo2, combine="crossprod", repls=1, reg=train.reg)

# Test:
# testJob(1, reg=train.reg)

# Execute jobs:
submitJobs(reg=train.reg) 
waitForJobs(reg=train.reg)
getStatus(reg=train.reg)

# Data object for next stage:
Sys.sleep(60)
RESULTS2 <- list(models = reduceResultsList(fun=function(x) x$model.list, reg=train.reg),
                 results = reduceResultsList(fun=function(x) x$result, reg=train.reg),
                 jobpars = unwrap(getJobPars(reg=train.reg)))

# Export true performances:
# theta <- lapply(RESULTS2$results, 
#                 function(x)data.frame(t(quantile(x$theta, probs=c(0,0.05,0.25,0.5,0.75,0.95,1)))))
# theta <- rbindlist(theta)
# names(theta) <- paste0("theta_", c("q0", "q05", "q25", "q50", "q75", "q95", "q100"))
# 
# theta.full <- cbind(theta, RESULTS2$jobpars)
# write_csv(theta.full, paste0("RESULTS/LEARNING/theta_", sim.chr, ".csv"))
# 
# theta <- theta.full <- NULL


