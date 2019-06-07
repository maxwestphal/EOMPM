source("EOMPM_functions_evaluation.R")

### FUNCTION - merge_results()
#   Creates final results table from evaluation phase. 
#   Merges results from estimation and evaluation step.
merge_results <- function(ids_eval,                     # evaluation registry IDs
                          ids_est,                      # estimation registry IDs 
                          csv=TRUE,                     # T: save as csv
                          folder='RESULTS/EVALUATION/', # subfolder 
                          filename=paste0('results_', sim.chr, '.csv')){  
  require(data.table)
  require(dplyr)
  R_eval.list <- reduceResultsList(ids=ids_eval, reg=eval.reg)
  N <- length(R_eval.list)
  JP_eval <- unwrap(getJobPars(ids=ids_eval, reg=eval.reg))
  JP_est <- unwrap(getJobPars(ids=ids_est, reg=est.reg))
  
  R_eval <- rbindlist(lapply(1:N, function(i){
    y = R_eval.list[[i]]; y$job.id=JP_eval$job.id[i]; return(y)})) %>%
              rename(job.id_eval=job.id)
  # reduce to minimum set of variables to save space:
  JP_eval <- JP_eval %>% 
    select(job.id, index, inf.transform, select.method,
                                select.a , select.k , select.q) %>%
    rename(job.id_eval=job.id)
  JP_est <- JP_est %>% 
    select(job.id, n, beta.seed, P, Pact, beta.mean, 
           pert.switch, pert.diff, pert.modify, pert.mult, X.corr) %>%
    rename(job.id_est = job.id)
  # merging:
  JPR_eval <- merge(JP_eval, R_eval, by.x="job.id_eval", by.y="job.id_eval",
                    allow.cartesian = T)
  C <- merge(JP_est, JPR_eval, by.x="job.id_est", by.y="index",
             allow.cartesian = T)
  if(csv){
    require(readr)
    write_csv(C, path=paste0(folder, filename))
  }
  return(C)  
}


