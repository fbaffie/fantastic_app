


run_custom_model <- function(active_stat, obs_prec_all, obs_tair_all, model_res, input_manip, param) {
  
  stations <- colnames(obs_prec_all$data)
  
  for (station in stations) {
    
    time_sim <- obs_prec_all$time
    prec_sim <- obs_prec_all$data[[station]]
    tair_sim <- obs_tair_all$data[[station]]
    
    icol <- which(colnames(input_manip$prec_manip) == station)
    
    for (irow in 1:length(input_manip$time)) {
      isim <- which(time_sim == input_manip$time[irow])
      if (!is.na(input_manip$prec_manip[irow, icol])) {
        prec_sim[isim] <- input_manip$prec_manip[irow, icol]
      }
    }
    
    Ind_Run <- seq(1, length(time_sim))
    
    InputsModel <- CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR4J,
                                     DatesR = time_sim[Ind_Run],
                                     Precip = prec_sim[Ind_Run],
                                     TempMean = tair_sim[Ind_Run],
                                     PotEvap = rep(0, length(Ind_Run)))
    
    RunOptions <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
                                   IndPeriod_Run = Ind_Run,
                                   InputsModel = InputsModel,
                                   IniStates = NULL,
                                   IniResLevels = NULL,
                                   IndPeriod_WarmUp = NULL)
    
    OutputsModel <- RunModel_CemaNeigeGR4J(InputsModel = InputsModel,
                                           RunOptions = RunOptions,
                                           Param = param[[df_meta$regine_main[df_meta$stat_id == station]]])  # this can be simplified
    
    df_sim <- data.frame(time = time_sim, runoff = OutputsModel$Qsim)
    
    df_ref <- data.frame(time = model_res$custom_d$time)
    
    df_final <- semi_join(df_sim, df_ref, by = "time")
    
    model_res$custom_d$runoff[[station]] <- df_final$runoff
    
  }
  
  return(model_res)
  
  
  
  
  # time_sim <- obs_prec_all$time
  # prec_sim <- obs_prec_all$data[[active_stat]]
  # tair_sim <- obs_tair_all$data[[active_stat]]
  # 
  # icol <- which(colnames(input_manip$prec_manip) == active_stat)
  # 
  # for (irow in 1:length(input_manip$time)) {
  #   isim <- which(time_sim == input_manip$time[irow])
  #   if (!is.na(input_manip$prec_manip[irow, icol])) {
  #     prec_sim[isim] <- input_manip$prec_manip[irow, icol]
  #   }
  # }
  # 
  # Ind_Run <- seq(1, length(time_sim))
  # 
  # InputsModel <- CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR4J,
  #                                  DatesR = time_sim[Ind_Run],
  #                                  Precip = prec_sim[Ind_Run],
  #                                  TempMean = tair_sim[Ind_Run],
  #                                  PotEvap = rep(0, length(Ind_Run)))
  # 
  # RunOptions <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
  #                                IndPeriod_Run = Ind_Run,
  #                                InputsModel = InputsModel,
  #                                IniStates = NULL,
  #                                IniResLevels = NULL,
  #                                IndPeriod_WarmUp = NULL)
  # 
  # OutputsModel <- RunModel_CemaNeigeGR4J(InputsModel = InputsModel,
  #                                        RunOptions = RunOptions,
  #                                        Param = param[[df_meta$regine_main[df_meta$stat_id == active_stat]]])  # this can be simplified
  # 
  # df_sim <- data.frame(time = time_sim, runoff = OutputsModel$Qsim)
  # 
  # df_ref <- data.frame(time = model_res$custom_d$time)
  # 
  # df_final <- semi_join(df_sim, df_ref, by = "time")
  # 
  # model_res$custom_d$runoff[[active_stat]] <- df_final$runoff
  # 
  # return(model_res)
  # 
}































