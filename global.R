
if (!require('lubridate')) install.packages('lubridate', repos = "http://cran.us.r-project.org"); library(lubridate)
if (!require('plotly')) install.packages('plotly', repos = "http://cran.us.r-project.org"); library(plotly)
if (!require('R.utils')) install.packages('R.utils', repos = "http://cran.us.r-project.org"); library(R.utils)
if (!require('leaflet')) install.packages('leaflet', repos = "http://cran.us.r-project.org"); library(leaflet)
if (!require('leaflet.extras')) install.packages('leaflet.extras', repos = "http://cran.us.r-project.org"); library(leaflet.extras)
if (!require('dplyr')) install.packages('dplyr', repos = "http://cran.us.r-project.org"); library(dplyr)
if (!require('shinyjs')) install.packages('shinyjs', repos = "http://cran.us.r-project.org"); library(shinyjs)
if (!require('airGR')) install.packages('airGR', repos = "http://cran.us.r-project.org"); library(airGR)
if (!require('shinydashboard')) install.packages('shinydashboard', repos = "http://cran.us.r-project.org"); library(shinydashboard)
if (!require('sp')) install.packages('sp', repos = "http://cran.us.r-project.org"); library(sp)



base_folder <- "./"


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

