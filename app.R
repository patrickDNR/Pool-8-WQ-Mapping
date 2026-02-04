#Try leaflet?

library(leaflet)
library(tidyverse)
library(lubridate)
library(sf)
library(paletteer)
library(shiny)
library(bslib)
library(viridis)

#load data
wq <- read.csv('Data/LTRM_WQ_all.csv') %>%
  dplyr::mutate(Year = format(mdy(DATE), '%Y')) %>%
  dplyr::mutate(Month = month(mdy(DATE), label = T)) %>%
  dplyr::mutate(DATE = as.Date(format(mdy(DATE), '%Y-%m-%d')))

#convert UTM to lat lon
utm_crs <- 32615

#Convert to sf object
utm_sf <- st_as_sf(wq, coords = c('EASTING', 'NORTHING'), crs = utm_crs)

#transform to geographic coordinates
latlon_sf <- st_transform(utm_sf, crs = 4326)

#make into data frame
latlon_df <- as.data.frame(st_coordinates(latlon_sf))
colnames(latlon_df) <- c('lng', 'lat')

#combine with water quality data
wq <- cbind(wq, latlon_df)

#years
years.num <- seq(1993, 2024)
years.char <- as.character(years.num)

#Now select date and month?


# Define UI for water quality map app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Water Qualiy Mapping"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Let's try to do date range
      dateRangeInput(inputId = 'date_range', 
                     label = 'Select Date Range:', 
                     start = min(wq$DATE), 
                     end = max(wq$DATE), 
                     min = min(wq$DATE), 
                     max = max(wq$DATE), 
                     format = 'yyyy-mm-dd'),
      
      
      #Input: Select constituent
      selectInput(inputId = 'variable', 
                  label = 'Variable:', 
                  choices = c('Temperature' = 'TEMP', 
                              'Dissolved O2' = 'DO', 
                              'pH' = 'PH', 
                              'Turbidity' = 'TURB', 
                              'Secchi' = 'SECCHI',
                              'Conductivity' = 'COND', 
                              'Velocity' = 'VEL', 
                              'Suspended Sediments' = 'SS', 
                              'Total P' = 'TP', 
                              'Total N' = 'TN', 
                              'Nitrite Nitrate' = 'NOX', 
                              'Ammonium' = 'NHX', 
                              'Clorophyll' = 'CHLcal', 
                              'Chloride' = 'CL', 
                              'Silica' = 'SI')),
      
      #select x-axis variable
      selectInput(inputId = 'xvar', 
                  label = 'Boxplot x axis variable:', 
                  choices = c('Year' = 'Year', 
                              'Habitat Class' = 'HABCLASS')),
      
      #checkbox to select if you want to show outliers or not
      checkboxInput(inputId = 'outliers', 
                    label = 'Show outliers:', 
                    value = TRUE),
      
      #checkbox to select if you want growing season only (May - Sept)
      checkboxInput(inputId = 'growing', 
                    label = 'Show growing season only (May - Sept):', 
                    value = TRUE)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      fluidRow(
        plotOutput('wqBoxes', height = 400, width = 600)
      ),
      
      # Output: Mpa of WQ variable ----
      fluidRow(
        leafletOutput("wqMap", height = 800))
      
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste(input$variable)
  })
  
  filtered_data <- reactive({
    if(input$growing){
      wq %>%
        filter(DATE >= input$date_range[1] & DATE <= input$date_range[2]) %>%
        filter(!is.na(get(input$variable))) %>%
        filter(Month == 'May' | Month == 'Jun' | Month == 'Jul' | Month == 'Aug' |
                 Month == 'Sept')
      
    }
    else{
    wq %>%
      filter(DATE >= input$date_range[1] & DATE <= input$date_range[2]) %>%
      filter(!is.na(get(input$variable)))
    }
  })
  
  colorpal <- reactive({
    df <- filtered_data()
    
    colorNumeric('RdYlBu', domain = as.numeric(df[[input$variable]]), reverse = T)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  #Generate a boxplot across habitat classes
  output$wqBoxes <- renderPlot({
    df <- filtered_data()
    
    boxplot(
      df[[input$variable]] ~ df[[input$xvar]], 
      xlab = input$xvar,
      ylab = input$variable, 
      outline = input$outliers
    )
  })
  
  # Generate a plot of the requested variable in a pool 8 map
  output$wqMap <- renderLeaflet({
    df <- filtered_data()
    
    pal <- colorpal()
    
    chart <- df %>%
      leaflet() %>%
      addTiles() %>%
      setView(lng = -91.2, lat = 43.6, zoom = 12) %>%
      addCircleMarkers(data = df,
                       color = ~pal(df[[input$variable]]), 
                       popup = paste(df$DATE, '\n',input$variable,' = ', as.character(df[[input$variable]])),
                       fillOpacity = 0.8, 
                       lat = df$lat, 
                       lng = df$lng) %>%
      addLegend(
        position = 'bottomright', 
        pal = pal, 
        values = ~df[[input$variable]], 
        title = input$variable, 
        opacity = 1
      )
    
    chart
  })
  
}


shinyApp(ui, server)



