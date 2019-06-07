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
### Real data example A: Breast Cancer Wisconsin (Diagnostic) Data Set
### Data source: 
#   https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Diagnostic)
### Goal: Application of the 'within 1 SE' selection and maxT-evaluation 
#         strategy on real data
### Author: Max Westphal

# Make analysis reproducible:
set.seed(1)

# Read and prepare in data:
data <- read_csv("DATA/wdbc.data.txt", 
                 col_names = c("ID", "Y", paste0("X", 1:30)),
                 col_types = cols(.default="c"),
                 na=c("", "?"))

data$Y <- recode(data$Y, M=1, B=0)

data <- data %>% 
  mutate_all(as.numeric) %>%
  select(-ID) %>%
  as.data.table()

# Missing values?
colSums(is.na(data)) # -> no missing values
summary(data)
dim(data)
table(data$Y)

# Model matrix:
dat <- data %>% as.matrix()

# split data: first 75% learning, last 25% evaluation:
indices <- sample(nrow(dat), round(nrow(dat)*0.75))
learn <- dat[indices, ]
test  <- dat[-indices, ]

# Model training and CV:
learning_result <- training_applied(learn=learn, cv.folds=10)

# Inspect validation stage result and mark selected models:
best.val.mod <- which.max(learning_result$result$thetahat.cv)
val.result <- learning_result$result %>%
  mutate(within1SE = thetahat.cv >= thetahat.cv[best.val.mod] - se.cv[best.val.mod]) %>%
  as.data.table()
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

# Validation ino on selected models:
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
 

