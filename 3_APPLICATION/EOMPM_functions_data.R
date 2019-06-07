### FUNCTION - flex_cormat()
#   Used to generate a correlation matrix (deterministic).
#   Argument type should be function of i (row), j (column), M and rho.
flex_cormat <- function(M,     # dimensions
                        rho,   # correlation parameter
                        type){ # correlation matrix model
  rows <- matrix(rep(1:M), M, M)
  cols <- matrix(rep(1:M, each=M), M, M)
  R <- do.call(type, list(i=rows, j=cols, M=M, rho=rho))
  return(R)
}

### FUNCTION - equi()
#   Used to generate equicorrelation matrix (set type='equi' in flex_cormat()).
equi <- function(i, j, M, rho){
  ifelse(i==j,1,rho)}

### FUNCTION - ak()
#   Used to generate autocorrelation matrix (set type='ak' in flex_cormat()).
ak <- function(i, j, M, rho){
  R <- 1 - abs(i-j)/(M-1)*(1-rho)}

### FUNCTION generate_beta()
#   Generate true regression coefficients based on some model beta.dist and 
#   parameters P, Pact, beta.mean, beta.scale. The parameter beta.seed sets the
#   random seed and is only meaningful for randomized models, e.g. 'unif'.
generate_beta <- function(P=50,              # number of features
                          Pact=5,            # number of nonzero coefficients
                          beta.mean=1,       # location parameter
                          beta.scale=0,      # scale parameter
                          beta.dist='const', # coeff model
                          beta.seed=NULL){   # random.seed
  Pinact = P-Pact
  set.seed(beta.seed)
  if(beta.dist=="const"){ # 'sparse' coefficient model from paper
    beta <- c(rep(beta.mean, Pact), rep(0, Pinact))
  }
  if(beta.dist=="seq1"){
    beta <- beta.mean*c(1/(1:Pact), rep(0, Pinact))
  }
  if(beta.dist=="seq2"){
    beta <- beta.mean*c(1/(2^(1:Pact-1)), rep(0, Pinact))
  }
  if(beta.dist=="equi"){
    beta <- beta.mean*c((Pact:1)/Pact, rep(0, Pinact))
  }
  if(beta.dist=="mix1"){ # 'dense' coefficient model from paper
    beta <- beta.mean*c((-1)^(1:Pact-1)*1/(1:Pact), rep(0, Pinact))
  }
  if(beta.dist=="norm"){
    beta <- c(rnorm(Pact, mean=beta.mean, sd=beta.scale), rep(0, Pinact))
  }
  if(beta.dist=="unif"){
    beta <- c(runif(Pact, min=beta.mean-beta.scale, max=beta.mean+beta.scale),
              rep(0, Pinact))
  }
  return(beta)
}

### FUNCTION - perturb_beta()
#   Used to permute coefficient vector beta from sparse coefficients model.
perturb_beta <- function(beta,      # initial/input beta
                         switch=0, # coeffs to switch between inactive and active
                         diff=0,  # number of nontrivial coeffs to add/substract
                         modify=0,  # number of old coeffs to modify
                         mult=1,    # multiplier for coeffs to be modified
                         perturb.seed=NULL){ # random.seed for pertubation
  c <- switch
  d <- diff
  
  if(d==0 & c==0 & modify==0)return(beta)
  
  P <- length(beta)
  act <- which(beta != 0)
  beta_act <- beta[act]
  Pact <- length(act)
  inact <-  which(beta == 0)
  
  if(c<0)stop("switch needs to be >0!")
  if(c>Pact)stop("switch > Pact: Cannot change more coeffs than Pact!")
  if(d > P-Pact | d < -Pact)
    stop("Something's wrong with diff (number of coeffs to add/remove)!")
  if(modify<0)stop("modify needs to be >=0!")
  
  set.seed(perturb.seed)
  
  act1 <- sample(act, Pact-c+ifelse(d<0,d,0))
  act2 <- sample(inact, c+ifelse(d>0,d,0))
  actN <- c(act1, act2)
  betaN <- rep(0, P)
  betaN[actN] <- beta[actN]
  inactF <- betaN == 0 & 1:P %in% actN
  betaN[inactF] <- sample(beta_act, sum(inactF), replace=T)
  actL <- intersect(act, actN)
  if(length(actL) < modify)
    stop("modify too large: Not enough coefficients left to modify!")
  if(modify > 0) betaN[sample(actL, modify)] <- mult*sample(beta_act, modify)
  return(betaN)
}

### FUNCTION - generate_X()
#   Used for feature/covariate generation.
generate_X <- function(n=100,              # number of samples
                       P=50,               # number of features
                       X.dist='mvnorm',    # feature distribution
                       X.corr=0,           # correlation parameter
                       X.corr.type='equi', # correlation type
                       X.seed=NULL){       # control random.seed
  set.seed(X.seed)
  if(X.dist=="mvnorm"){
    require(mvtnorm)
    if(X.corr.type=="equi"){
      X.sigma <- flex_cormat(M=P, rho=X.corr, type="equi")
    }
    if(X.corr.type=="ak"){
      X.sigma <- flex_cormat(M=P, rho=X.corr, type="ak")
    }
    X <- rmvnorm(n, sigma=X.sigma)
  }
  return(X)
}

### FUNCTION - generate_Y()
#   Generate labels Y based on  conditional distribution P(Y|X).
generate_Y <- function(X,                    # feature data
                       beta,                 # model coefficients
                       intercept=0,          # intercept term beta_0
                       score.model="linear", # conditional distribution model
                       Y.seed=NULL){         # control random.seed
  set.seed(Y.seed)
  if(ncol(X) != length(beta)){stop("#Coefficients != #Variables")}
  if(score.model=="linear"){
    score <- intercept + X%*%beta
    prob <- 1/(1+exp(-score))
    n <- length(prob)
    y <- rbinom(n,1,prob)   
  }
  return(y)
}

### FUNCTION - generate_data()
#   Combines all previous functions and allows data generation for learning and
#   evaluation phase.
generate_data <- function(data=NULL, # batchtools argument
                          job=NULL,  # batchtools argument
                          n=100,
                          P=50,
                          Pact=5,
                          score.model="linear",
                          intercept=0,
                          beta.dist="const",
                          beta.mean=1,
                          beta.scale=0,
                          pert.switch=0,
                          pert.diff=0,
                          pert.modify=0,
                          pert.mult=0,
                          X.dist="mvnorm",
                          X.corr=0,
                          X.corr.type="equi",
                          beta.seed=NULL,
                          pert.seed=NULL,
                          X.seed=NULL,
                          Y.seed=NULL,
                          reset.seeds=FALSE, # T: ignore random.seed settings
                          reset.pert=FALSE,  # T: ignore coefficient pertubations
                          ...){
  require(data.table)
  args.list <- c(as.list(environment()), list(...))
  args.list$data <- args.list$job <- NULL
  args <- as.data.table(t(unlist(args.list)))
  if(reset.pert){pert.switch = pert.diff = pert.modify = pert.mult = 0}
  if(reset.seeds){beta.seed = pert.seed = X.seed = Y.seed = NULL}
  beta <- generate_beta(P=P,
                        Pact=Pact, 
                        beta.dist=beta.dist,
                        beta.mean=beta.mean,
                        beta.scale=beta.scale,
                        beta.seed=beta.seed)
  beta <- perturb_beta(beta,
                       switch=pert.switch,
                       diff=pert.diff,
                       modify=pert.modify,
                       mult=pert.mult,
                       perturb.seed=pert.seed)
  X <- generate_X(n=n, 
                  P=P,
                  X.dist=X.dist,
                  X.corr=X.corr,
                  X.corr.type=X.corr.type,
                  X.seed=X.seed)
  Y <- generate_Y(X=X,
                  beta=beta,
                  intercept=intercept, 
                  score.model=score.model,
                  Y.seed=Y.seed)
  colnames(X) <- paste0("X", 1:P)
  data.out <- cbind(Y=Y,X)
  return(list(data = data.out,
              args = args)) # DT with nrow=1
}

################################################################################
# TEST:
# generate_data()


