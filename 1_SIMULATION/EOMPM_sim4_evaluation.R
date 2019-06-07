### EVALUTION data registry:
eval.dir <- paste0(getwd(), "/DATA/", sim.chr, "/registry_evaluation")
eval.reg <- makeExperimentRegistry(file.dir = eval.dir, seed = 1) 
# -> should only be used initially and throws an error if registry already exists
eval.reg <- loadRegistry(eval.dir, writeable=T)
nc <- detectCores()
eval.reg$cluster.functions <- makeClusterFunctionsSocket(nc-1)
eval.reg$source <- paste0(getwd(), "/EOMPM_functions_evaluation.R")

# Define relevant parameters:
estIDS <- findExperiments(reg=est.reg)$job.id
params4 <- CJ(index = estIDS,
              wd=getwd(),
              subdir=paste0('/DATA/',sim.chr,'/'),
              regname='registry_estimation')
dim(params4)

prob4 <- list(load_result = params4) 
algo4 <- list(evaluation = args4)

# Add problems / algorithms:
addProblem(name="load_result", data=NULL, fun=load_result, seed=1, reg=eval.reg)
addAlgorithm(name = "evaluation", fun = evaluation, reg=eval.reg)

# Add experiments:
addExperiments(prob4, algo4, combine="crossprod", repls=nsim.test, reg=eval.reg) 

# Test:
# testJob(1, reg=eval.reg)

# Execute jobs:
submitJobs(reg=eval.reg) 
waitForJobs(reg=eval.reg)
getStatus(reg=eval.reg)

# EXPORT RESULTS:
Sys.sleep(60)
merge_results(ids_eval = findExperiments(reg=eval.reg)$job.id,
              ids_est = estIDS, csv= T)



