### Population data registry:
pop.dir <- paste0(getwd(), "/DATA/", sim.chr, "/registry_population")
pop.reg <- makeExperimentRegistry(file.dir = pop.dir, seed = 1) 
# -> should only be used initially and throws an error if registry already exists
pop.reg <- loadRegistry(pop.dir, writeable = TRUE)
nc <- detectCores()
pop.reg$cluster.functions <- makeClusterFunctionsSocket(nc-1) 
pop.reg$source <- paste0(getwd(), "/EOMPM_functions_data.R")

# Define relevant parameters for sample generation:
prob1 <- list(generate_data = params1)

addProblem(name="generate_data", data=NULL, fun=generate_data, seed=1, reg=pop.reg)
addAlgorithm(name = "return_data", reg=pop.reg)

# See all experiments:
addExperiments(prob.designs=prob1, 
               algo.designs = NULL, ### only return raw data
               repls=1,
               reg=pop.reg)


# run jobs
submitJobs(reg=pop.reg) 
#waitForJobs(reg=pop.reg)
getStatus(reg=pop.reg)
#getErrorMessages()

#clearRegistry(reg=pop.reg)

# Save data for next stage:
Sys.sleep(10)
RESULTS1 <- list(data=reduceResultsList(fun=function(x) x$data, reg=pop.reg),
                 args=rbindlist(reduceResultsList(fun=function(x) x$args, reg=pop.reg)))


