################################################################################
# Project Name:         Refresh of Retail Planning Tool
# Project Nickname:     LX Refresh
# Project goal:         Refresh existing algorithms and improve 
#                       functionality & features of Planning Tool
# Project Timeline:     November 2017 - August 2018
# First Author:         Selen Onel
# Second Author:        Soothsayer Analytics www.http://soothsayeranalytics.com/
#                       Main Author:        Pranav Shah
#                       Supervisor Authors: Sree Shravan & Akshay Deshpande

## Content Description
# This code achieves the following:
# Using Lat/Lon data to draw Polygons(As known as Price Zone)

## Function Description
# Group data by outlet, sort points clockwise
# Convert Lat/Lon into Sp format to draw Polygons
# Adjust buffer size and companys to highlight
# Display result in interactive map with various customize setting
# The function outputs either an interactive map or a map enbedded in shiny.app

## Input Variables to Function
# data: Dataset contains variables with name OwnLat, OwnLon, CompLat, CompLon 
# Company: A list of companys to Highlight. Default NULL
# Buffer: Buffer Size in mile, usually between 0.1 and 0.5. Default 0
# APP: Display a map or a shiny.app. Default F(map), could switch to T(app)

## Output of the Function
# A map or a shiny.app

## This code should run after KCA.code. 

#### Required Packages ####
# install.packages("devtools")
# devtools::install_github("skgrange/gissr")
library(tidyverse)
library(leaflet)
library(gissr)
library(shiny)
library(shinydashboard)
#library(rgeos) Need to installed but not load
#library(data.table) Need to installed but not load

## If you want to futhur speed up, run code below
# library(furrr) 
# plan(multiprocess)
# change all map function to future_map

#### Build up function ####
PZ <- function(data, Company = NULL, Buffer = 0, APP = F){
  
  #### Make up Sp-Polygon data with Lat/Lon ####
  df_Own <- data %>%
    group_by_at(.[names(.) %>% str_detect("^Own")] %>% names) %>%
    nest() 
  
  df_Comp <- data %>%
    group_by_at(.[names(.) %>% str_detect("^Comp")] %>% names) %>%
    nest() 
  
  stations <- data.table::rbindlist(
    list(df_Own %>% select(-data), df_Comp %>% select(-data))
  ) %>% distinct()
  
  SpP <- df_Own$data %>% 
    map(select, CompLon, CompLat) %>% 
    map(as.data.frame) %>% 
    map(sort_points, y = "CompLat", x = "CompLon", clockwise = T) %>% 
    map(~bind_rows(.x, .x[1, ])) %>% 
    map(~Polygon(cbind(.x$CompLon, .x$CompLat))) %>% 
    map2(paste0("s", 1:nrow(df_Own)),~Polygons(list(.x), .y)) %>% 
    map(~SpatialPolygons(list(.x)))
  
  #### Draw Interactive Leaflet Map ####
  m <- leaflet() %>% 
    addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga",
             attribution = 'Google') %>% #Using Google map layout. 
    setView(lng = stations$OwnLon[1], lat = stations$OwnLat[1], zoom = 12) %>% 
    addMeasure() %>% addMiniMap(position = "bottomleft") 
  
  # Add polygon and buffers. 
  for (i in SpP) {
    m <- m %>% addPolygons(data = i %>% rgeos::gBuffer(width = Buffer*7/500), stroke = F)
  }
  
  # Add point to highlight sites. Could also add popup and markers.
  m <- m %>% 
    addCircles(~OwnLon, ~OwnLat, data = stations,
               stroke = F, fillOpacity = 1, radius = 70#, 
               #            popup = ~paste('Brand: ', OwnBrand, '<br/>',
               #                           'Name: ', OwnName, '<br/>',
               #                           'Address: ', OwnAddress, '<br/>')
               # ) %>%
               # addMarkers(~OwnLon, ~OwnLat, data = stations %>% filter(OwnBrand %in% Company),
               #            popup = ~paste('Brand: ', OwnBrand, '<br/>',
               #                           'Name: ', OwnName, '<br/>',
               #                           'Address: ', OwnAddress, '<br/>')
    )
  
  #### Build up Shiny app ####
  # The logic of shiny app is the same as map above. Only UI changed
  box111 <- box(width = NULL,
                tags$style(type = "text/css", "#map {height:calc(100vh - 130px) !important;}"),
                leafletOutput("map")
  )
  
  box112 <- box(width = NULL,
                h3("Station Explorer"),
                
                checkboxGroupInput("brand", "Brand to Highlight", 
                                   choices = stations$OwnBrand %>% base::unique()
                ),
                
                sliderInput("bf", h3("Buffer Size (Mile)"), min = 0, max = 0.5, 
                            round = F, value = 0.1)
  )
  
  ui <- dashboardPage(skin = "red",
                      
                      header = dashboardHeader(title = "Kalibrate Price Zone"),
                      
                      sidebar = dashboardSidebar(disable = T),
                      
                      body = dashboardBody(
                        fluidRow(column(width = 9, box111), 
                                 column(width = 3, box112)
                        )
                      )
  )
  
  # Define server logic
  server <- function(input, output) {
    
    # Create the map
    output$map <- renderLeaflet({
      leaflet() %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga",
                 attribution = 'Google') %>% 
        setView(lng = stations$OwnLon[1], lat = stations$OwnLat[1], zoom = 12) %>% 
        addMeasure() %>% addMiniMap(position = "bottomleft") 
    })
    
    # Incremental changes to the map
    observe({
      #circles for staions
      m <- leafletProxy("map", data = SpP) %>% clearShapes() 
      
      for (i in SpP) {
        m <- m %>% addPolygons(data = i %>% 
                                 rgeos::gBuffer(width = input$bf*7/500),
                               stroke = F)     
      }
      
      m %>% addCircles(~OwnLon, ~OwnLat, data = stations,
                       stroke = F, fillOpacity = 1, radius = 70#, 
      #                  popup = ~paste('Brand: ', OwnBrand, '<br/>',
      #                                 'Name: ', OwnName, '<br/>',
      #                                 'Address: ', OwnAddress, '<br/>')
      # ) %>% 
      #   addMarkers(~OwnLon, ~OwnLat, 
      #              data = stations %>% filter(OwnBrand %in% input$brand),
      #              popup = ~paste('Brand: ', OwnBrand, '<br/>',
      #                             'Name: ', OwnName, '<br/>',
      #                             'Address: ', OwnAddress, '<br/>')
        )
      return(m)
    })
  }
  
  if(APP == F) return(m)
  else return(shinyApp(ui = ui, server = server))
}

#### Sample Test ####
PZ(KCA)
PZ(KCA, APP = T)
