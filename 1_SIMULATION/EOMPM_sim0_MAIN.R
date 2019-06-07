### SIMULATION STUDY: Evaluation of multiple prediction models (EOMPM)
##
#   This script allows to carry out a subset of simulations by submitting the
#   following commands. The suffix of the first script may be altered to
#   conduct the simulations for different parameter settings.

### INSTALL PACKAGES (if needed):
# install.packages(c("caret", "plyr", "dplyr", "tidyr", "data.table", "readr",
#                    "optiRum", "batchtools", "parallel", "Metrics", "mvtnorm",
#                    "snow", "glmnet", "ggplot2", "ggpubr", "cowplot"))


### SIMULATION SCENARIOS:
#   A1 - A5: batches for n.train=400, cv.folds=10
#   B1 - B5: batches for n.train=200, cv.folds=10
#   C1 - C2: batches for n.train=400, cv.folds=5
#   D1 - D2: batches for n.train=200, cv.folds=5
#   E1 - E5: batches for perturbed learning distributon 

# WD is assumed to be ".../EOMPM/1_SIMULATION"
# Change suffix for first script to obtain all results
# Start with "..._TEST.R" for small test run 
source("CONFIG/EOMPM_sim0_parameters_TEST.R")
source("EOMPM_sim1_population.R")
source("EOMPM_sim2_training.R")
source("EOMPM_sim3_estimation.R")
source("EOMPM_sim4_evaluation.R")
