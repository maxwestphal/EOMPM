### DEFINITION all relevant PARAMETERS (paramsN) AND ARGUMENTS (argsN) 
#   [N = number of simulation]:
#
#   N=1: GENERATE POPULATION DATA
#   N=2: TRAINING (data generation, model training and calculation of true 
#                  performances)
#   N=3: ESTIMATION (for evaluation, i.e. test data generation and calculation 
#                    of all test statistics and full covariance matrix)
#   N=4: EVALUATION (application of selection rules and
#                    maxT-approach for statistical inference)

### INSTALL PACKAGES (if needed):
# install.packages(c("caret", "plyr", "dplyr", "tidyr", "data.table", "readr",
#                    "optiRum", "batchtools", "parallel", "Metrics", "mvtnorm",
#                    "snow", "glmnet", "ggplot2"))

### LOAD PACKAGES:
require(data.table)
require(dplyr)
require(readr)
require(optiRum)
require(batchtools)
require(parallel)
require(Metrics)
require(mvtnorm)
require(snow)
require(glmnet)
require(ggplot2)


### SET WD:
#   set working directory to folder ".../EOMPM/1_SIMULATION" (if not done already):
#   setwd("1_SIMULATION")

### LOAD CUSTOM FUNCTIONS:
source("EOMPM_functions_export.R")

### SIMULATION SCENARIOS:
#   A1 - A5: batches for n.train=400, cv.folds=10
#   B1 - B5: batches for n.train=200, cv.folds=10
#   C1 - C5: batches for n.train=400, cv.folds=5
#   D1 - D5: batches for n.train=200, cv.folds=5
#   E1 - E5: batches for perturbed learning distributon 

sim.chr <- "E3"

n.train <- 200
n.test  <- c(100, 200, 400)

cv.folds <- 10

nsim.train <- 1000 # 
nsim.test  <- 1    # for 1000 simulations

seed.lag <- (as.numeric(substr(sim.chr, 2, 2))-1)*10000

### CREATE SUBDIRECTORY
dir.create(paste0(getwd(), "/DATA/", sim.chr), recursive=T)

################################################################################
## SIM 1: POPULATION

## Population parameters:
params1 <- CJ(n = 100000,
              P = 50,
              Pact = 5,
              score.model = "linear",
              intercept = 0,
              beta.mean = 4, 
              beta.scale = 0,
              beta.dist = "const",
              pert.switch=0,
              pert.diff=0,
              pert.modify=0,
              pert.mult=0,
              X.dist="mvnorm",
              X.corr=0,
              X.corr.type="equi",
              beta.seed = 0,
              pert.seed = 0,
              X.seed = 0,
              Y.seed = 0,
              reset.seeds=FALSE,
              reset.pert=FALSE)

dim(params1)

################################################################################
## SIM 2: TRAINING

## Training parameters:
params2 <- CJ(beta.seed = (seed.lag+1):(seed.lag+nsim.train),
              n = n.train,
              P = 50,
              Pact = 5,
              score.model = "linear",
              intercept = 0,
              beta.mean = c(4),
              beta.scale = 0,
              beta.dist = "const",
              pert.switch=c(0, 1, 2, 4),
              pert.diff=c(0, 1, -1, 2, -2, 4, -4),
              pert.modify=c(0, 1, 2, 4),
              pert.mult=c(0, 0.5, -1, 2),
              X.dist="mvnorm",
              X.corr=0,
              X.corr.type="equi",
              reset.seeds = FALSE,
              reset.pert = FALSE)

params2$X.seed <- params2$pert.seed <- params2$beta.seed

## Necessary/intended restrictions:
params2 <- params2 %>% filter(pert.switch == 0 | pert.switch/Pact == 0.2)
params2 <- params2 %>% filter(pert.diff == 0 | pert.diff/Pact == 0.2 |
                                pert.diff/Pact == -0.2)
params2 <- params2 %>% filter(pert.modify == 0 | pert.modify/Pact == 0.2 |
                                pert.modify/Pact == 0.4)
params2 <- params2 %>% filter(pert.switch==0 & pert.diff==0 |
                                pert.modify==0 & pert.mult==0)
params2 <- params2 %>% filter(!(pert.modify == 0 & pert.mult != 0))
params2 <- params2 %>% filter(!(pert.modify != 0 & pert.mult == 0))
params2 <- as.data.table(params2)


## Training arguments:
args2 <- CJ(nlambda = 20,
            cv.metric = "class", 
            cv.folds = cv.folds,
            family = "binomial",
            alpha.num =5, 
            standardize=FALSE,
            compare = "accuracy",
            convert = "oneminus")

dim(params2)/nsim.train
dim(args2)

################################################################################
### SIM 3: ESTIMATION

## Estimation parameters:
tmp <- data.table(n=n.test)
cpy <- copy(params2)
cpy[,"n"] <- NULL

params3 <- CJ.dt(tmp, cpy)
# set TRUE for estimation stage to get independent learning and evaluation data
params3$reset.seeds <- TRUE  
params3$reset.pert  <- TRUE

cpy <- NULL

## Check:
nrow(params3) == nrow(params2)*length(n.test)

## Estimation arguments:
args3 <- CJ(type="class",
            compare="accuracy",
            convert="oneminus")

dim(params3)
dim(args3)

################################################################################
## SIM 4: EVALUATION
# set params (=job.ids of estimation registry) directly in evaluation sim script

args4 <- CJ(inf.transform = c("none"),
            inf.method = "maxT",
            inf.alpha = 0.05,
            inf.tail = "lower.tail",
            select.method = c("default", "close2opt", "quantile", "oracle"),
            select.k = c(0, 1),
            select.q = c(0, 0.1, 1))


args4 <- args4 %>% 
  filter(select.method=="quantile"  & select.k==0 & select.q >0 | 
         select.method=="default"   & select.k==0 & select.q==0 | 
         select.method=="close2opt" & select.k==1 & select.q==0 |
         select.method=="oracle"    & select.k==0 & select.q==0) %>% 
  as.data.table()
 
args4

