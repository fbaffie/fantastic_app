source("global.R")

library(airGR)

nstat <- ncol(obs_prec_all$data)
ntimes <- length(obs_prec_all$time)
regine_main <- colnames(obs_prec_all$data)

nse_all <- c()

param <- list()

for (istat in 1:nstat) {
  
  print(istat)
  
  Ind_Run <- seq(1, ntimes)
  
  InputsModel <- CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR4J,
                                   DatesR = obs_prec_all$time[Ind_Run],
                                   Precip = obs_prec_all$data[Ind_Run, istat],
                                   TempMean = obs_tair_all$data[Ind_Run, istat],
                                   PotEvap = rep(0, length(Ind_Run)))
  
  RunOptions <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
                                 IndPeriod_Run = Ind_Run,
                                 InputsModel = InputsModel,
                                 IniStates = NULL,
                                 IniResLevels = NULL,
                                 IndPeriod_WarmUp = NULL)
  
  InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE,
                                 InputsModel = InputsModel, 
                                 RunOptions = RunOptions,
                                 Qobs = obs_runoff_all$data[Ind_Run, istat])
  
  CalibOptions <- CreateCalibOptions(FUN_MOD = RunModel_CemaNeigeGR4J, FUN_CALIB = Calibration_Michel)
  
  OutputsCalib <- Calibration_Michel(InputsModel = InputsModel,
                                     RunOptions = RunOptions,
                                     InputsCrit = InputsCrit,
                                     CalibOptions = CalibOptions,
                                     FUN_MOD = RunModel_CemaNeigeGR4J,
                                     FUN_CRIT = ErrorCrit_NSE)
  
  Param <- OutputsCalib$ParamFinalR
  OutputsModel <- RunModel_CemaNeigeGR4J(InputsModel = InputsModel,
                                         RunOptions = RunOptions,
                                         Param = Param)
  
  OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
  
  nse_all <- c(nse_all, OutputsCrit$CritValue)
  
  param[[regine_main[istat]]] <- Param
  
}

save(param, file = "data/param_gr4j.RData")







