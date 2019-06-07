# Clean environment:
# rm(list=ls())

require(readr)
require(data.table)
require(plyr)
require(dplyr)
require(glmnet)
require(caret)
require(Metrics)
require(mvtnorm)
require(tidyr)

source("EOMPM_functions_application.R")

### Evaluation of multiple prediction models (EOMPM)
### Real data example B: Cardiotocography Data Set 
### Data source: 
#   https://archive.ics.uci.edu/ml/datasets/cardiotocography
### Goal: Application of the 'within 1 SE' selection and maxT-evaluation 
#         strategy on real data
### Author: Max Westphal

# Make analysis reproducible:
set.seed(1)

# Read and prepare in data:
# Note: I initially did a manual export via MS Excel from the original file CTG.xls 
#       (3rd sheet 'RAW data') to the 'CTG_raw.csv' file, both files are provided.
data <- read_csv2("DATA/CTG_raw.csv") 
data <- data[- which(rowSums(is.na(data))>0), ] 
# -> remove 4 incomplete observations, everything else is complete
data <- data %>% 
  mutate(Tendency = factor(Tendency),
         CLASS = factor(CLASS),
         NSP = factor(NSP)) %>%
  mutate(Date = (as.Date(Date, format="%d.%m.%Y"))) %>%
  select(-c(FileName, SegFile, b, e)) %>%
  select(-c(A, B, C, D, E, AD, DE, LD, FS, SUSP)) %>%
  arrange(Date) %>%
  select(-Date)

# EDA on endpoints
table(data$CLASS) # -> Class code (1 to 10) for classes A to SUSP
table(data$NSP)   # -> Normal=1; Suspect=2; Pathologic=3
table(data$CLASS, data$NSP)
colSums(is.na(data))

# Goal: prediction of NSP != 1, i.e. suspect or pathologic (without CLASS AS PREDICTOR)
dat <- data %>% 
  mutate(Y = as.integer(NSP != 1)) %>%
  select(-c(CLASS, NSP))
dat %>% setcolorder(c("Y", names(dat)[-ncol(dat)])) 

# split data: first 75% learning, last 25% evaluation:
indices <- 1:round(nrow(dat)*0.75)
learn <- dat[indices, ]
test  <- dat[-indices, ]

# use dummy coding for factor variables:
dummyMap <- dummyVars(~ . , data=dat, fullRank=TRUE)
learn <- predict(dummyMap, learn)
test <- predict(dummyMap, test)

# Model training and CV:
learning_result <- training_applied(learn=learn, cv.folds=10)

# Inspect validation stage result and mark selected models:
best.val.mod <- which.max(learning_result$result$thetahat.cv)
val.result <- learning_result$result %>%
  mutate(within1SE = thetahat.cv >= thetahat.cv[best.val.mod] - se.cv[best.val.mod])

val.result %>% filter(within1SE)

# Estimate parameters for evaluation:
estimation_result <- estimation1(models=learning_result$models,
                                 test=test,
                                 cv.metric="class",
                                 compare="accuracy",
                                 convert="oneminus")
estimation_result$jobpars <- data.frame(n=nrow(test))

# Inspect relevant covariance:
cov2cor(estimation_result$covariance[val.result$within1SE, val.result$within1SE])

# Validation of selected models:
estimation_result$models[val.result$within1SE] %>% select(-theta, -thetahat.test)

# Evaluation result:
evaluation_result <- evaluation(data=NULL,
                                job=NULL,
                                instance = estimation_result,
                                inf.transform = "none",
                                inf.method= "maxT",
                                inf.alpha=0.05,
                                inf.tail="lower.tail",
                                select.method = "close2opt",
                                select.a=0,
                                select.k=1,
                                select.q=0)

# final model characteristics / evaluation study conclusion:
evaluation_result[1, ] %>% select(lower, thetahat, thetahat.median)


