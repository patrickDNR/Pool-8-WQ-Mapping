#Make a plot 

library(leaflet)
library(tidyverse)
library(lubridate)
library(sf)
library(paletteer)
library(shiny)
library(bslib)
library(viridis)

#download data
data_url <- "https://raw.githubusercontent.com/patrickDNR/Pool-8-WQ-Mapping/refs/heads/main/Data/WaterQual_SRS.csv"
download.file(data_url, 'WaterQual_SRS.csv')

wq <- read.csv('WaterQual_SRS.csv')


# Define UI for water quality map app ----
ui <- bslib::page_sidebar(
  
  id = 'WQ data',
  
  # App title ----
  title = "Water Qualiy Mapping",
  
  #theme
  theme = bslib::bs_theme(version = 5),
  
  #background color
  fillable_mobile = TRUE,
  
  #window title
  window_title = 'WQ data',
  
  #background color 
  bg = 'darkgreen',
  
  
  # Sidebar panel for inputs ----
  sidebar = 
    
    sidebar(
    sidebarPanel(
      width = 12,
      
      # Input: Let's try to do date range
      # Input: Maybe a slider for time series?
      sliderInput(inputId = 'date_range', 
                  label = 'Select Date Range:', 
                  min = min(wq$Year), 
                  max = max(wq$Year),
                  step = 1,
                  value = c(min(wq$Year), max(wq$Year)), 
                  sep = '', 
                  ticks = F),
      
      #Select months of interest
      checkboxGroupInput(inputId = 'months', 
                         label = 'Select Sampling Months:', 
                         choices = c('January' = 'Jan', 
                                     'February' = 'Feb', 
                                     'March' = 'Mar',
                                     'April' = 'Apr', 
                                     'May' = 'May', 
                                     'June' = 'Jun', 
                                     'July' = 'Jul', 
                                     'August' = 'Aug', 
                                     'September' = "Sep", 
                                     'October' = 'Oct', 
                                     'November' = 'Nov', 
                                     'December' = 'Dec'), 
                         selected = c('Jun', 'Jul', 'Aug', 'Sep', 
                                      'Oct', 'Nov')),
      
      
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
      
      downloadButton(outputId = 'downloadData', 
                     label = 'Download CSV')
    )
    )
  ,
  
  # Main panel for displaying outputs ----

  navset_card_underline(
    
    nav_panel('About', 
              tags$img(height = 100, width = 100,
                       src = 'https://umesc.usgs.gov/ltrmp/images/buttons/water-hi.png'),
              tags$html(
                tags$head(
                  tags$title('UMRR Long Term Resource Monitoring - Water Quality component')
                ),
                tags$body(
                  'Data from this visualization comes from the Long Term Resource
                    Monitoring element of the Upper Mississippi River Restoration program. The data displayed
                    are specific for Pool 8 of the UMR, collected by the La Crosse Field Station and the 
                    Wisconsin Department of Natural Resources in collaboration with USGS and US Army Corps of 
                    Engineers. For more information on the water quality component, please visit: ', a(
                    "the LTRM website",
                    target = "_blank",
                    href = "https://umesc.usgs.gov/data_library/water_quality/water_quality_page.html"
                  ), 
                  
                  tags$p("For more information on the river and Wisconsin's work on the UMR, 
                         please visit:", a('UMR by the Wisconsin DNR', 
                                           target = "_blank", 
                                           href = "https://dnr.wisconsin.gov/topic/UMR/About.html"))
                ), 
  
              )),
    
    nav_panel('Time Series', plotOutput('wqBoxes', height = 500, width = 900))
    ,
    
    # Output: Map of WQ variable ----
    
    nav_panel('WQ sample point map',leafletOutput("wqMap", height = 800))
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
    
    wq %>%
      filter(Year >= input$date_range[1] & Year <= input$date_range[2]) %>%
      filter(!is.na(get(input$variable))) %>%
      filter(Month %in% input$months)
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
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste('WQdata-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
}


shinyApp(ui, server)



