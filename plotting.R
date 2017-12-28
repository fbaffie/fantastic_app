

# Plot runoff -------------------------------------------------------------

plot_sim_runoff <- function(data, stat, p) UseMethod("plot_sim_runoff", data)


# Plot runoff for deterministic models

plot_sim_runoff.det_model <- function(data, stat, p) {
  
  icol <- colnames(data$runoff) == stat
  
  p <- add_lines(p,
                 x = data$time,
                 y = data$runoff[, icol],
                 name = data$name,
                 type = 'scatter',
                 mode = 'lines',
                 line = list(color = data$color),
                 alpha = 0.5)
  
  return(p)
  
}


# Plot runoff for ensemble models

plot_sim_runoff.ens_model <- function(data, stat, p) {
  
  icol <- colnames(data$runoff_min) == stat
  
  p <- add_ribbons(p,
                   x = data$time,
                   ymin = data$runoff_min[, icol],
                   ymax = data$runoff_max[, icol],
                   name = data$name,
                   line = list(color = data$color),
                   fillcolor = data$color,
                   opacity = 0.4)
  
  return(p)
  
}



# Main time series plot ---------------------------------------------------

main_plot <- function(selected_station, selected_models, model_res, obs_runoff, obs_prec, obs_tair, df_meta) {
  
  # Horizontal lines indicating alert levels
  
  # Does not work due to https://github.com/ropensci/plotly/issues/1019
  
  # alart_line_1 <- list(type = "line",
  #                      x0 = 0,
  #                      x1 = 1,
  #                      y0 = df_meta$alert_limit_1[df_meta$stat_id == selected_station],
  #                      y1 = df_meta$alert_limit_1[df_meta$stat_id == selected_station],
  #                      xref = "paper",
  #                      yref = "y1",
  #                      line  = list(color = "#FFCC32"))
  # 
  # alart_line_2 <- list(type = "line",
  #                      x0 = 0,
  #                      x1 = 1,
  #                      y0 = df_meta$alert_limit_2[df_meta$stat_id == selected_station],
  #                      y1 = df_meta$alert_limit_2[df_meta$stat_id == selected_station],
  #                      xref = "paper",
  #                      yref = "y",
  #                      line  = list(color = "#E8650A"))
  # 
  # alart_line_3 <- list(type = "line",
  #                      x0 = 0,
  #                      x1 = 1,
  #                      y0 = df_meta$alert_limit_3[df_meta$stat_id == selected_station],
  #                      y1 = df_meta$alert_limit_3[df_meta$stat_id == selected_station],
  #                      xref = "paper",
  #                      yref = "y",
  #                      line  = list(color = "#CE1722"))
  
  # Line indicating when forecast was issued
  
  verticle_line <- list(type = "line",
                        x0 = forecast_date,
                        x1 = forecast_date,
                        y0 = 0,
                        y1 = 1,
                        xref = "x",
                        yref = "paper",
                        line  = list(color = "gray"))
  
  # Runoff panel
  
  p1 <- plot_ly()
  
  p1 <- add_lines(p1,
                  x = c(obs_runoff$time[1], tail(obs_runoff$time, 1)),
                  y = c(df_meta$alert_limit_1[df_meta$stat_id == selected_station],
                        df_meta$alert_limit_1[df_meta$stat_id == selected_station]),
                  name = "Alert level 1",
                  type = 'scatter',
                  mode = 'lines',
                  showlegend = FALSE,
                  line = list(color = "#FFCC32"))
  
  
  p1 <- add_lines(p1,
                  x = c(obs_runoff$time[1], tail(obs_runoff$time, 1)),
                  y = c(df_meta$alert_limit_2[df_meta$stat_id == selected_station],
                        df_meta$alert_limit_2[df_meta$stat_id == selected_station]),
                  name = "Alert level 2",
                  type = 'scatter',
                  mode = 'lines',
                  showlegend = FALSE,
                  line = list(color = "#E8650A"))
  
  p1 <- add_lines(p1,
                  x = c(obs_runoff$time[1], tail(obs_runoff$time, 1)),
                  y = c(df_meta$alert_limit_3[df_meta$stat_id == selected_station],
                        df_meta$alert_limit_3[df_meta$stat_id == selected_station]),
                  name = "Alert level 3",
                  type = 'scatter',
                  mode = 'lines',
                  showlegend = FALSE,
                  line = list(color = "#CE1722"))
  
  p1 <- add_lines(p1,
                  x = obs_runoff$time,
                  y = obs_runoff$data[, colnames(obs_runoff$data) == selected_station],
                  name = "obs",
                  type = 'scatter',
                  mode = 'lines',
                  line = list(color = "black"))
  
  for (i in selected_models) {
    p1 <- plot_sim_runoff(model_res[[i]], selected_station, p1)
  }
  
  p1 <- layout(p1,
               hovermode = "x",
               xaxis = list(showgrid = F, showline = FALSE),
               yaxis = list(side = "left", title = "Runoff (mm/day)", showgrid = F, showline = TRUE, zeroline = TRUE, mirror = "ticks"),
               shapes = verticle_line)
  
  # Plot meteorological data
  
  p2 <- plot_ly()
  
  p2 <-  add_lines(p2,
                   x = obs_prec$time,
                   y = obs_prec$data[, colnames(obs_prec$data) == selected_station],
                   name = "Precipitation",
                   yaxis = "y",
                   line = list(color = "black"),
                   showlegend = FALSE)
  
  p2 <-  add_lines(p2,
                   x = obs_tair$time,
                   y = obs_tair$data[, colnames(obs_tair$data) == selected_station],
                   name = "Temperature",
                   yaxis = "y2",
                   line = list(color = "red"),
                   showlegend = FALSE)
  
  p2 <- layout(p2,
               hovermode = "x",
               xaxis = list(showgrid = F, showline = FALSE),
               yaxis = list(side = "left",
                            title = "Precipitation (mm/day)",
                            showgrid = F,
                            showline = TRUE,
                            zeroline = TRUE,
                            rangemode = "tozero"),
               yaxis2 = list(side = "right", title = "Temperature (C)", overlaying = "y2", showgrid = F, showline = TRUE, zeroline = FALSE),
               shapes = verticle_line)
  
  # Stich plots together
  
  p <- subplot(list(p1, p2),nrows = 2, shareX = TRUE, titleY = TRUE, margin = 0.04)
  
  layout(p,
         showlegend = TRUE,
         autosize = F, height = 900, width = 800,
         xaxis = list(range = c(first(model_res[[1]]$time), last(model_res[[1]]$time))))
  
}





# Plot runoff -------------------------------------------------------------

plot_runoff <- function(selected_station, selected_models, model_res, obs_runoff, df_meta) {
  
  # Horizontal lines indicating alert levels
  
  # Does not work due to https://github.com/ropensci/plotly/issues/1019
  
  # alart_line_1 <- list(type = "line",
  #                      x0 = 0,
  #                      x1 = 1,
  #                      y0 = df_meta$alert_limit_1[df_meta$stat_id == selected_station],
  #                      y1 = df_meta$alert_limit_1[df_meta$stat_id == selected_station],
  #                      xref = "paper",
  #                      yref = "y1",
  #                      line  = list(color = "#FFCC32"))
  # 
  # alart_line_2 <- list(type = "line",
  #                      x0 = 0,
  #                      x1 = 1,
  #                      y0 = df_meta$alert_limit_2[df_meta$stat_id == selected_station],
  #                      y1 = df_meta$alert_limit_2[df_meta$stat_id == selected_station],
  #                      xref = "paper",
  #                      yref = "y",
  #                      line  = list(color = "#E8650A"))
  # 
  # alart_line_3 <- list(type = "line",
  #                      x0 = 0,
  #                      x1 = 1,
  #                      y0 = df_meta$alert_limit_3[df_meta$stat_id == selected_station],
  #                      y1 = df_meta$alert_limit_3[df_meta$stat_id == selected_station],
  #                      xref = "paper",
  #                      yref = "y",
  #                      line  = list(color = "#CE1722"))
  
  # Line indicating when forecast was issued
  
  verticle_line <- list(type = "line",
                        x0 = forecast_date,
                        x1 = forecast_date,
                        y0 = 0,
                        y1 = 1,
                        xref = "x",
                        yref = "paper",
                        line  = list(color = "gray"))
  
  # Runoff panel
  
  p1 <- plot_ly(source = "runoff")
  
  p1 <- add_lines(p1,
                  x = c(obs_runoff$time[1], tail(obs_runoff$time, 1)),
                  y = c(df_meta$alert_limit_1[df_meta$stat_id == selected_station],
                        df_meta$alert_limit_1[df_meta$stat_id == selected_station]),
                  name = "Alert level 1",
                  type = 'scatter',
                  mode = 'lines',
                  showlegend = FALSE,
                  line = list(color = "#FFCC32"))
  
  
  p1 <- add_lines(p1,
                  x = c(obs_runoff$time[1], tail(obs_runoff$time, 1)),
                  y = c(df_meta$alert_limit_2[df_meta$stat_id == selected_station],
                        df_meta$alert_limit_2[df_meta$stat_id == selected_station]),
                  name = "Alert level 2",
                  type = 'scatter',
                  mode = 'lines',
                  showlegend = FALSE,
                  line = list(color = "#E8650A"))
  
  p1 <- add_lines(p1,
                  x = c(obs_runoff$time[1], tail(obs_runoff$time, 1)),
                  y = c(df_meta$alert_limit_3[df_meta$stat_id == selected_station],
                        df_meta$alert_limit_3[df_meta$stat_id == selected_station]),
                  name = "Alert level 3",
                  type = 'scatter',
                  mode = 'lines',
                  showlegend = FALSE,
                  line = list(color = "#CE1722"))
  
  p1 <- add_lines(p1,
                  x = obs_runoff$time,
                  y = obs_runoff$data[, colnames(obs_runoff$data) == selected_station],
                  name = "obs",
                  type = 'scatter',
                  mode = 'lines',
                  line = list(color = "black"))
  
  for (i in selected_models) {
    p1 <- plot_sim_runoff(model_res[[i]], selected_station, p1)
  }
  
  p1 <- layout(p1,
               hovermode = "x",
               xaxis = list(showgrid = F,
                            showline = TRUE,
                            mirror = "all"),
               yaxis = list(side = "left",
                            title = "Runoff (mm/day)",
                            showgrid = F,
                            showline = TRUE,
                            zeroline = TRUE,
                            mirror = "all",
                            rangemode = "tozero"),
               shapes = verticle_line,
               margin = list(r = 150))
  
}



# Plot meteorology --------------------------------------------------------

plot_meteorology <- function(selected_station, obs_prec, obs_tair, df_meta, rv) {
  
  # Line indicating when forecast was issued
  
  verticle_line <- list(type = "line",
                        x0 = forecast_date,
                        x1 = forecast_date,
                        y0 = 0,
                        y1 = 1,
                        xref = "x",
                        yref = "paper",
                        line  = list(color = "gray"))
  
  # Plot meteorological data
  
  p2 <- plot_ly(source = "meteorology")
  
  p2 <-  add_lines(p2,
                   x = obs_prec$time,
                   y = obs_prec$data[, colnames(obs_prec$data) == selected_station],
                   name = "Precipitation",
                   yaxis = "y",
                   line = list(color = "black"),
                   showlegend = FALSE)
  
  p2 <-  add_lines(p2,
                   x = obs_tair$time,
                   y = obs_tair$data[, colnames(obs_tair$data) == selected_station],
                   name = "Temperature",
                   yaxis = "y2",
                   line = list(color = "red"),
                   showlegend = FALSE)
  
  p2 <- add_trace(p2,
                  x = rv$input_manip$time,
                  y = rv$input_manip$prec_manip[[selected_station]],
                  name = 'prec_changed',
                  mode = 'markers',
                  marker = list(size = 10,
                                color = 'rgb(137, 139, 142)',
                                line = list(color = 'rgb(0, 0, 0)',
                                            width = 2)))
  
  p2 <- layout(p2,
               #hovermode = "x",
               xaxis = list(showgrid = F,
                            showline = TRUE,
                            mirror = "all",
                            range = c(first(rv$input_manip$time), last(rv$input_manip$time)),
                            autorange = FALSE),
               yaxis = list(side = "left",
                            title = "Precipitation (mm/day)",
                            showgrid = F,
                            showline = TRUE,
                            zeroline = TRUE,
                            rangemode = "tozero"),
               yaxis2 = list(side = "right",
                             title = "Temperature (C)",
                             overlaying = "y1",
                             showgrid = F,
                             showline = TRUE,
                             zeroline = FALSE),
               shapes = verticle_line,
               margin = list(r = 150))
  
}



# Station map -------------------------------------------------------------

plot_map <- function(df_meta) {
  
  
  # Colors and legend label
  
  col_pal <- c("#77B100", "#FFCC32", "#E8650A", "#CE1722")
  
  col_breaks <- c(-Inf, 1.5, 2.5, 3.5, Inf)
  
  legend_str <- c("Green", "Yellow", "Orange", "Red")
  
  col_binned <- cut(df_meta$alert_level, col_breaks, labels = col_pal)
  
  leaflet() %>%
    
    addTiles("http://opencache.statkart.no/gatekeeper/gk/gk.open_gmaps?layers=topo2graatone&zoom={z}&x={x}&y={y}",
             attribution = "? Kartverket") %>%
    
    # fitBounds(min(df_meta$longitude),
    #           min(df_meta$latitude),
    #           max(df_meta$longitude),
    #           max(df_meta$latitude)) %>%
    
    addCircleMarkers(lng = df_meta$longitude,
                     lat = df_meta$latitude,
                     layerId = paste("all", df_meta$stat_id, sep = "_"),
                     group = "stations_all",
                     color = col_binned,
                     radius = 6,
                     stroke = FALSE,
                     opacity = 0.9,
                     fillOpacity = 0.8,
                     label = df_meta$stat_id,
                     labelOptions = labelOptions(style=list('font-size' = '18px')))
    
    # addDrawToolbar(targetGroup = "test", 
    #                rectangleOptions = F, 
    #                polylineOptions = F, 
    #                markerOptions = F,
    #                circleOptions = F,
    #                singleFeature = TRUE)
  
}

