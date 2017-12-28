

compute_alert_levels <- function(df_meta, model_res) {
  
  n_stations <- nrow(df_meta)
  n_models <- length(model_res)
  
  alert_levels <- matrix(1, n_stations, n_models)
  
  for (irow in 1:n_stations) {
    
    for (imodel in 1) {
    
      ifuture <- model_res[[imodel]]$time > forecast_date
      
      runoff <- model_res[[imodel]]$runoff[[df_meta$stat_id[irow]]]
      
      runoff <- runoff[ifuture]
      
      if (!any(is.na(runoff))) {
        if (any(runoff > df_meta$alert_limit_3[irow])) {
          alert_levels[irow, imodel] <- 4
        } else if (any(runoff > df_meta$alert_limit_2[irow])) {
          alert_levels[irow, imodel] <- 3
        } else if (any(runoff > df_meta$alert_limit_1[irow])) {
          alert_levels[irow, imodel] <- 2
        }
      }
      
    }
    
  }
  
  df_meta$alert_level <- alert_levels[,1]
  
  return(df_meta)
  
}


