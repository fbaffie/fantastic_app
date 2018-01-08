


# Load metadata -----------------------------------------------------------

load(file.path(base_folder, "data/df_meta.RData"))


# Load observations -------------------------------------------------------

load(file.path(base_folder,"data/obs_runoff.RData"))

load(file.path(base_folder,"data/obs_prec.RData"))

load(file.path(base_folder,"data/obs_tair.RData"))

obs_runoff$data[obs_runoff$data < 0] <- NA


load(file.path(base_folder,"data/obs_runoff_all.RData"))

load(file.path(base_folder,"data/obs_prec_all.RData"))

load(file.path(base_folder,"data/obs_tair_all.RData"))

obs_runoff_all$data[obs_runoff_all$data < 0] <- NA



# Load parameters ---------------------------------------------------------

file_param <- file.path(base_folder, "data/param_gr4j.RData")

load(file_param)




# Load simulations --------------------------------------------------------

# Path to model results

path_sim <- file.path(base_folder, "data/res_forecast/201711211233")   #"C:/Work/Rcode/fantastic_app/data/res_forecast/201711211233"

# Load hbv model results

hbv_d <- read_sim(file.path(path_sim, "model_hbv_light", "tables_short_forecast"), df_meta)

hbv_d <- det_model(hbv_d$time, hbv_d$runoff, "HBV (det)", "blue")

# Load gr4j model results

gr4j_d <- read_sim(file.path(path_sim, "model_gr4j", "tables_short_forecast"), df_meta)

gr4j_d <- det_model(gr4j_d$time, gr4j_d$runoff, "GR4J (det)", "red")

# Load fake ensemble model

gr4j_e <- ens_model(gr4j_d$time, gr4j_d$runoff*0.9, gr4j_d$runoff*1.1, "GR4J (ens)", "mangenta")


# Load fusk model results

custom_d <- read_sim(file.path(path_sim, "model_fusk", "tables_short_forecast"), df_meta)

custom_d <- det_model(custom_d$time, custom_d$runoff, "CUSTOM (det)", "green")

custom_d$runoff[, ] = NA




# # Load gr4j model results
# 
# frenchy <- read_sim(file.path(path_sim, "model_frenchy", "tables_short_forecast"), df_meta)
# 
# frenchy <- det_model(frenchy$time, frenchy$runoff*10, "frenchy", "pink")







# Add to list with models

model_res <- list(hbv_d = hbv_d, gr4j_d = gr4j_d, gr4j_e = gr4j_e, custom_d = custom_d)


# Create changeable inputs 

ikeep <- obs_prec_all$time %in% custom_d$time

input_manip <- list(time = obs_prec_all$time[ikeep],
                    prec_manip = obs_prec_all$data[ikeep, ],
                    prec_orig = obs_prec_all$data[ikeep, ])




