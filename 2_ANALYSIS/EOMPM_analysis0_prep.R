require(readr)
require(plyr)
require(dplyr)
require(data.table)
require(ggplot2)
require(grDevices)
require(RColorBrewer)
require(gtable)
require(grid)
require(gridExtra)
require(lemon)
require(tidyr)
require(ggpubr)
require(cowplot) 
require(glmnet)
require(batchtools)
require(parallel)
require(Metrics)
require(mvtnorm)

# load custom functions:
setwd("C:/Users/maxwe/Documents/R/Projects/EOMPM/2_ANALYSIS")
source("EOMPM_functions_export.R")

################################################################################
##  STEP (A) Prepare results for 'perturbed learning distribution' case, 
##  i.e. calculate KL divergence between true and perturbed distribution(s)

kl <- function(x, beta.a, beta.b){
  p1.a <- 1/(1+exp(-(x%*%beta.a)))
  p1.b <- 1/(1+exp(-(x%*%beta.b)))
  p0.a <- 1-p1.a
  p0.b <- 1-p1.b
  s1 <- p1.a*log(p1.a/p1.b)
  s0 <- p0.a*log(p0.a/p0.b)
  (s1 + s0)
}

KL <- function(beta.b, 
               beta.a=c(rep(1,5), rep(0,45)),
               nsim = 100000){
  M <- length(beta.a)
  x <- rmvnorm(nsim, mean=rep(0,M))
  v <- apply(x, 1, kl, beta.a=beta.a, beta.b=beta.b)
  mean(v[is.finite(v)])
}

### Cases to consider = interaction of the following factors:
# pert.switch, pert.diff, pert.modify, pert.mult
cases <- c("0.0.0.0",
           "0.-1.0.0",
           "1.-1.0.0",
           "1.0.0.0",
           "0.1.0.0",
           "1.1.0.0",
           "0.0.1.-1",
           "0.0.2.-1",
           "0.0.1.0.5",
           "0.0.2.0.5",
           "0.0.1.2",
           "0.0.2.2")

KLpert <- function(pert, beta=c(rep(4,5),rep(0,45)), nsim=10^6){
  s <- strsplit(pert, ".", fixed=T)[[1]]
  if(length(s)==5){s[4] <- paste(s[4], s[5], sep="."); s <- s[1:4]}
  p <- as.numeric(s)
  beta_pert <- perturb_beta(beta, p[1], p[2], p[3], p[4])
  KL(beta_pert, beta, nsim=nsim)
}

divs <- sapply(cases, KLpert)
KLdat <- data.table(case=cases, div=divs)
KLdat #-> to be merged with simulation results in next script

################################################################################
##  STEP (B) Calculate bayes performance (performance of data generating models)

# run simulation:
source("EOMPM_sim0_parameters.R")
source("EOMPM_sim1_population.R")
source("EOMPM_sim2_training.R")

perfCal <- function(pop, model){
  pred <- predict(model, newx=pop[,-1], type="class") # matrix (ntruth x nlambda)
  if(is.character(pred[1,1])){class(pred) <- "integer"}
  theta <- apply(pred, 2, function(predicted){
    do.call("accuracy", list(actual=pop[,1], predicted=predicted))})
  names(theta) <- NULL
  return(theta)
}

str(RESULTS1, 1)

mods <- lapply(RESULTS1[[1]], function(r){glmnet(r[1:100,-1], r[1:100,1], 
                                                 family="binomial",
                                                 alpha=1, 
                                                 standardize = FALSE, 
                                                 lambda=0.1)})

JP <- unwrap(getJobPars(reg=train.reg))

true_coeffs <- lapply(1:8, function(i){
  generate_beta(P=JP[i,]$P, Pact = JP[i,]$Pact, beta.mean=JP[i,]$beta.mean, 
                beta.scale=0, beta.dist=JP[i,]$beta.dist, beta.seed=NULL)}) 


true_mods <- lapply(1:8, function(i){
  mod = mods[[i]]; mod$a0=0; names(mod$a0) = "s0";
  mod$beta=Matrix(true_coeffs[[i]], sparse=T); return(mod)})


BP <- sapply(1:8, function(i) perfCal(RESULTS1$data[[i]], true_mods[[i]]))
BP
#-> corresponds to the bayes performances for the eight considered scenarios
#   (same order as in paper see data.table JP)













