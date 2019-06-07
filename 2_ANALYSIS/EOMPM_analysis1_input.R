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
library(tidyr)
library(ggpubr)
library(cowplot) 
require(glmnet)
require(batchtools)
require(parallel)
require(Metrics)
require(mvtnorm)

# load custom functions:
source("EOMPM_functions_export.R")

# set RESULTS folder from simulation study (assuming current WD is .../2_ANALYSIS)
sf <- paste0(substr(getwd(),1, nchar(getwd())-10), "1_SIMULATION/RESULTS/")

# Simulation scenarios:
colspec <- cols(X.corr="d", select.k="d", select.q="d", pert.mult="d")

# load evaluation results, adapt if not all results have been computed (yet)
# in simulation phase
A <- rbind(read_csv(paste0(sf, "EVALUATION/results_A1.csv"), col_types=colspec),
           read_csv(paste0(sf, "EVALUATION/results_A2.csv"), col_types=colspec),
           read_csv(paste0(sf, "EVALUATION/results_A3.csv"), col_types=colspec),
           read_csv(paste0(sf, "EVALUATION/results_A4.csv"), col_types=colspec),
           read_csv(paste0(sf, "EVALUATION/results_A5.csv"), col_types=colspec))
A <- A %>% select(-beta.dist)

B <- rbind(read_csv(paste0(sf, "EVALUATION/results_B1.csv"), col_types=colspec),
           read_csv(paste0(sf, "EVALUATION/results_B2.csv"), col_types=colspec),
           read_csv(paste0(sf, "EVALUATION/results_B3.csv"), col_types=colspec),
           read_csv(paste0(sf, "EVALUATION/results_B4.csv"), col_types=colspec),
           read_csv(paste0(sf, "EVALUATION/results_B5.csv"), col_types=colspec))

C <- rbind(read_csv(paste0(sf, "EVALUATION/results_C1.csv"), col_types=colspec),
           read_csv(paste0(sf, "EVALUATION/results_C2.csv"), col_types=colspec))

D <- rbind(read_csv(paste0(sf, "EVALUATION/results_D1.csv"), col_types=colspec),
           read_csv(paste0(sf, "EVALUATION/results_D2.csv"), col_types=colspec))

E <- rbind(read_csv(paste0(sf, "EVALUATION/results_E1.csv"), col_types=colspec),
           read_csv(paste0(sf, "EVALUATION/results_E2.csv"), col_types=colspec),
           read_csv(paste0(sf, "EVALUATION/results_E3.csv"), col_types=colspec),
           read_csv(paste0(sf, "EVALUATION/results_E4.csv"), col_types=colspec),
           read_csv(paste0(sf, "EVALUATION/results_E5.csv"), col_types=colspec))

A <- cbind(A, n.train=400, pertTF=FALSE, cv.folds=10)
B <- cbind(B, n.train=200, pertTF=FALSE, cv.folds=10)
C <- cbind(C, n.train=400, pertTF=FALSE, cv.folds=5)
D <- cbind(D, n.train=200, pertTF=FALSE, cv.folds=5)
E <- cbind(E, n.train=200, pertTF=TRUE,  cv.folds=10)



COMB <- rbindlist(list(A, B, C, D, E))
dim(COMB)
A <- B <- E <- C <- D <- NULL


## Reducing results and calculating summary statistics
RAW <- COMB %>% 
  mutate(pert = interaction(pert.switch, pert.diff, pert.modify, pert.mult),
         selection = interaction(select.method, select.k, select.q),
         RD_naive = (thetahat-theta)/theta,
         RD_alt = (thetahat.median-theta)/theta,
         RD_trueVSopt = (theta-theta_opt)/theta_opt) %>%
  mutate(selection = revalue(selection, c("default.0.0" = "default",
                                          "oracle.0.0" = "oracle",
                                          "close2opt.1.0" = "c2o",
                                          "quantile.0.0.1" = "quantile",
                                          "quantile.0.1" = "all"))) %>%
  mutate(selection = ordered(selection, c("all", "oracle", "quantile",
                                          "default", "c2o"))) %>%
  select(-select.method, -select.k, -select.q) %>%
  select(-pert.switch, -pert.diff, -pert.modify, -pert.mult) %>%
  mutate(selection2 = ordered(selection, c("oracle", "default", "c2o",
                                           "quantile", "all")))

dim(RAW)  

COMB <- NULL 

# merge with KL divergence data
RAW <- merge(RAW, KLdat, by.x="pert", by.y="case")
RAW <- RAW %>% mutate(div=round(div, 3))

# summary statistics for power analysis
EVAL <- RAW %>%
  group_by(n.train, X.corr, P, Pact, n, beta.mean, pert, pertTF, 
           selection, delta, cv.folds) %>%
  dplyr::summarize(N_sim = n(),
            div = mean(div),
            theta_opt.mean = mean(theta_opt),
            theta_opt.median = median(theta_opt),
            theta_opt_sel.mean = mean(theta_opt_sel),
            theta_opt_sel.median = median(theta_opt_sel),
            RR = mean(reject),
            CRR = mean(reject & theta > theta0_assumed),
            IRR = mean(reject & theta <= theta0_assumed),
            POWER = mean(CR>0),
            FWER = mean(FR>0),
            RB_naive.median = median((thetahat-theta)/theta),
            RB_alt.median = median((thetahat.median-theta)/theta),
            RB_naive.mean = mean((thetahat-theta)/theta),
            RB_alt.mean = mean((thetahat.median-theta)/theta),
            Mstar.median = median(Mstar)) %>% 
  mutate(beta.str = ifelse(beta.mean==4, "A", ifelse(beta.mean==6, "B",
                                                     as.character(beta.mean))))

dim(EVAL)

###
colspec2 <- cols(X.corr="d", pert.mult="d")

TT_A <- rbind(read_csv(paste0(sf, "LEARNING/theta_A1.csv"), col_types=colspec2),
              read_csv(paste0(sf, "LEARNING/theta_A2.csv"), col_types=colspec2),
              read_csv(paste0(sf, "LEARNING/theta_A3.csv"), col_types=colspec2),
              read_csv(paste0(sf, "LEARNING/theta_A4.csv"), col_types=colspec2),
              read_csv(paste0(sf, "LEARNING/theta_A5.csv"), col_types=colspec2))
TT_B <- rbind(read_csv(paste0(sf, "LEARNING/theta_B1.csv"), col_types=colspec2),
              read_csv(paste0(sf, "LEARNING/theta_B2.csv"), col_types=colspec2),
              read_csv(paste0(sf, "LEARNING/theta_B3.csv"), col_types=colspec2),
              read_csv(paste0(sf, "LEARNING/theta_B4.csv"), col_types=colspec2),
              read_csv(paste0(sf, "LEARNING/theta_B5.csv"), col_types=colspec2))

TT_A <- cbind(TT_A, n.train=400)
TT_B <- cbind(TT_B, n.train=200)  
# scenarios C and D not relevant as the only difference is cv-folds=5 instead of 10
               
TT <- rbind(TT_A, TT_B) %>%
  mutate(scenario = interaction(X.corr, beta.mean)) %>%
  mutate(scenario.num = recode(as.character(interaction(X.corr, beta.mean)),
                               "0.2"=1, "0.5.2"=2, "0.4"=3, "0.5.4"=4, " " = 5,
                               "0.3"=6, "0.5.3"=7, "0.6"=8, "0.5.6"=9, .default=-1)) %>%
  mutate(scenario=factor(scenario, 
                         levels = c("0.2", "0.5.2", "0.4", "0.5.4", "0.3", "0.5.3", "0.6", "0.5.6"),
                         ordered = F)) %>%
  mutate(dist.class = factor(ifelse(scenario.num <= 4, "Coefficient model 1 (sparse)",
                                    "Coefficient model 2 (dense)"))) %>%
  as.data.table()
               

TT_A <- TT_B <- NULL


### Save memory:
# COMB <- RAW <- EVAL <- NULL
# TT <- NULL



      