

# Global ------------------------------------------------------------------

source("global.R")


# User interface ----------------------------------------------------------

ui <- dashboardPage(skin = "blue",
                    
                    dashboardHeader(
                      title = "Flood forecasting",
                      titleWidth = 350),
                    
                    dashboardSidebar(
                      
                      width = 350,
                      
                      useShinyjs(),
                      
                      br(),
                      
                      box(title ="Select station",
                          background ="black",
                          status = "primary",
                          solidHeader = TRUE, 
                          selectInput("station",
                                      NULL,
                                      choices = df_meta$stat_id,
                                      selectize = TRUE),
                          fluidRow(
                            column(3, actionButton("prev_stat", "Prev", width = 70)),
                            column(3, actionButton("next_stat", "Next", width = 70))),
                          width = 12),
                      
                      box(title ="Select models",
                          background ="black",
                          status = "primary",
                          solidHeader = TRUE, 
                          checkboxGroupInput("models",
                                             NULL,
                                             choices = models,
                                             selected = "hbv_d"),  
                          width = 12),
                      
                      box(title ="Adjust forcings",
                          background ="black",
                          status = "primary",
                          solidHeader = TRUE, 
                          numericInput("new_forcing",
                                       label = "Change input:",
                                       value = NULL),
                          fluidRow(
                            column(3, actionButton("change_forcing", "Change")),
                            column(3, actionButton("clear_stat", "Clear selection"))),
                          width = 12),
                      
                      box(title ="Rerun models",
                          background ="black",
                          status = "primary",
                          solidHeader = TRUE, 
                          actionButton("run_model", "Run model"),
                          width = 12),
                      
                      absolutePanel(
                        bottom = 10,
                        left = 10,
                        draggable = F,
                        width='100%',
                        height='auto',
                        p(a(icon('github fa-2x'),href='https://github.com/jmgnve/fantastic_app',target='_blank')))
                      
                    ),
                    
                    
                    dashboardBody(
                      
                      tags$head(
                        tags$style(HTML(".leaflet-container { background: #ffffff; }"))
                      ),
                      
                      fluidRow(
                        
                        column(7,
                               
                               plotlyOutput("runoff",
                                            width = "100%",
                                            height = 450),
                               
                               plotlyOutput("meteorology",
                                            width = "100%",
                                            height = 450)
                               
                        ),
                        
                        column(5,
                               
                               leafletOutput(outputId = "map",
                                             height = 900,
                                             width = "100%")
                               
                        )
                        
                      )
                      
                    )
                    
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  
  # Reactive values
  
  active_stat <- reactive({input$station})
  
  rv <- reactiveValues(
    model_res = model_res,
    input_manip = input_manip,
    stat_manip = NULL,
    click_info = NULL
  )
  
  
  # Action button for moving to next station
  
  observeEvent(input$next_stat, {
    istat <- which(df_meta$stat_id == active_stat())
    if (istat + 1 <= nrow(df_meta)) {
      updateSelectInput(
        session,
        "station",
        selected = df_meta$stat_id[istat + 1]
      )
    }
  })
  
  
  # Action button for moving to previous station
  
  observeEvent(input$prev_stat, {
    istat <- which(df_meta$stat_id == active_stat())
    if (istat - 1 > 0) {
      updateSelectInput(
        session,
        "station",
        selected = df_meta$stat_id[istat - 1]
      )
    }
  })
  
  # Update map when changing station using dropdown menu, next and previous action button
  
  # Update station selection for dropdown menu and map
  
  observeEvent(input$map_marker_click, {
    if (input$map_marker_click$id != "selected") {
      updateSelectInput(
        session,
        "station",
        selected = input$map_marker_click$id
      )
    }
  })
  
  
  
  observeEvent(active_stat(), {
    istat <- which(df_meta$stat_id == active_stat())
    leafletProxy("map", session) %>%
      addCircleMarkers(lng = df_meta$longitude[istat],
                       lat = df_meta$latitude[istat],
                       layerId = "selected",
                       color = "black",
                       radius = 8,
                       stroke = TRUE,
                       opacity = 1,
                       fillOpacity = 0)
  })
  
  # Plot time series of runoff and meteorology
  
  output$runoff <- renderPlotly({
    plot_runoff(active_stat(), input$models, rv$model_res, obs_runoff, df_meta)
  })
  
  output$meteorology <- renderPlotly({
    plot_meteorology(active_stat(), obs_prec, obs_tair, df_meta, rv)
  })
  
  # Plot map with stations
  
  output$map <- renderLeaflet({
    plot_map(df_meta)
  })
  
  # Change forcing data
  
  observe({
    if (!is.numeric(input$new_forcing)) {
      shinyjs::disable("new_forcing")
      shinyjs::disable("change_forcing")
      shinyjs::disable("clear_stat")
    } else {
      shinyjs::enable("new_forcing")
      shinyjs::enable("change_forcing")
      if (!is.null(rv$stat_manip)) {
        shinyjs::enable("clear_stat")
      }
    }
  })
  
  observeEvent(event_data("plotly_click", source = "meteorology"), {
    
    rv$click_info <- event_data("plotly_click", source = "meteorology")
    
    if (rv$click_info$curveNumber == 0 || rv$click_info$curveNumber == 2) {
      updateNumericInput(session, "new_forcing", label = "Change precipitation:")
      updateNumericInput(session, "new_forcing", value = rv$click_info$y)
    }
    
    if (rv$click_info$curveNumber == 1) {
      updateNumericInput(session, "new_forcing", value = NA)
    }
    
    leafletProxy("map", session) %>%
      
      showGroup("changeable") %>%
      
      addDrawToolbar(targetGroup = "test",
                     rectangleOptions = F,
                     polylineOptions = F,
                     markerOptions = F,
                     circleOptions = F,
                     singleFeature = TRUE)
    
    
    
    print(rv$stat_manip)
    
  })
  
  
  observeEvent(input$clear_stat, {
    
    leafletProxy("map", session) %>%
      clearGroup("changeable") %>%
      hideGroup("test")
    
    rv$stat_manip = NULL
    
  })
  
  
  observeEvent(input$change_forcing, {
    
    if (is.null(rv$stat_manip)) {
      change_stat <- active_stat()
    } else {
      change_stat <- rv$stat_manip
    }
    
    tmp <- input$new_forcing
    if (rv$click_info$curveNumber == 0 || rv$click_info$curveNumber == 2) {
      tmp[tmp < 0] <- 0
      tmp[tmp > 300] <- 300
      irow <- rv$input_manip$time == ymd(rv$click_info$x)
      icol <- colnames(rv$input_manip$prec_manip) %in% change_stat
      rv$input_manip$prec_manip[irow, icol] <- tmp
    }
    
    updateNumericInput(session, "new_forcing", value = NA)
    
    leafletProxy("map", session) %>%
      removeDrawToolbar() %>%
      hideGroup("changeable") %>%
      hideGroup("test")
    
  })
  
  observeEvent(active_stat(), {
    updateNumericInput(session, "new_forcing", value = NA)
    leafletProxy("map", session) %>%
      removeDrawToolbar()
  })
  
  
  observeEvent(input$run_model, {
    rv$model_res <- run_custom_model(active_stat(), obs_prec_all, obs_tair_all, rv$model_res, rv$input_manip, param)
  })
  
  
  observeEvent(input$map_draw_new_feature,{
    
    # Extract coordinates from drawn polygon
    
    p_all <- input$map_draw_new_feature$geometry$coordinates
    
    longitude_poly <- c()
    latitude_poly <- c()
    
    for (p in p_all[[1]]) {
      longitude_poly <- c(longitude_poly, p[[1]][1])
      latitude_poly <- c(latitude_poly, p[[2]][1])
    }
    
    # Find stations in polygon
    
    inside <- point.in.polygon(df_meta$longitude,
                               df_meta$latitude,
                               longitude_poly,
                               latitude_poly)
    
    leafletProxy("map", session) %>%
      
      clearGroup("changeable") %>%
      
      addCircleMarkers(lng = df_meta$longitude[inside == 1],
                       lat = df_meta$latitude[inside == 1],
                       layerId = paste("man_", df_meta$stat_id[inside == 1], sep = ""),
                       group = "changeable",
                       color = "blue",
                       radius = 7,
                       stroke = TRUE,
                       opacity = 1,
                       fillOpacity = 0) %>%
      
      
      clearGroup("test")
    
    rv$stat_manip <- df_meta$stat_id[inside == 1]
    
  })
  
}


# Run app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)

