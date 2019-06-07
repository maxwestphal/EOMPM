### ESTIMATION data registry:
#removeRegistry(reg=est.reg)
est.dir <- paste0(getwd(), "/DATA/", sim.chr, "/registry_estimation")
est.reg <- makeExperimentRegistry(file.dir = est.dir, seed = 1) 
# -> should only be used initially and throws an error if registry already exists
est.reg <- loadRegistry(est.dir, writeable = T)
nc <- detectCores()
est.reg$cluster.functions <- makeClusterFunctionsSocket(nc-1)
est.reg$source <- paste0(getwd(), "/EOMPM_functions_estimation.R")

# Define relevant parameters for sample generation:
prob3 <- list(generate_data = params3)
algo3 <- list(estimation = args3)

# Add problems / algorithms:
addProblem(name = "generate_data", data=RESULTS2, fun=generate_data, seed=1, reg=est.reg)
addAlgorithm(name = "estimation", fun = estimation, reg=est.reg)

# Add experiments:
addExperiments(prob3, algo3, combine="crossprod", repls=nsim.test, reg=est.reg)

# Test:
# testJob(1, reg=est.reg)

# Execute jobs:
submitJobs(reg=est.reg) 
waitForJobs(reg=est.reg)
getStatus(reg=est.reg)

Sys.sleep(60)
