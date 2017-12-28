
library(lubridate)
library(plotly)
library(R.utils)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(shinyjs)
library(airGR)
library(shinydashboard)
library(sp)



source("class_definitions.R")

source("data_utils.R")

source("load_data.R")

source("plotting.R")

source("alert_levels.R")

source("run_custom_model.R")


stations <- colnames(model_res$hbv$runoff)

models <- names(model_res)

forecast_date <- ymd_hm("201711211233")


df_meta <- compute_alert_levels(df_meta, model_res)

