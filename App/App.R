# This code does the following:

#1 load libraries
#2. Read in static data
#3 extract/process data from data.act.gov.au
#4 extract/process site metadata from data.act.gov.au
#5 Create UI
#6 Create Server

#1 Load libraries

library(dplyr)
library(tidyr)
library(RSocrata)
library(soql)
library(lubridate)
library(ggplot2)
library(shiny)
library(leaflet)
library(plotly)

#2. Read in static data

setwd("./App")

Static_data <- read.csv("streamflow_data.csv", header = T, stringsAsFactors = T)
Static_data$Value <- as.numeric(Static_data$Value)
Static_data$DatetimeAEST <- as.POSIXct(Static_data$DatetimeAEST, format = "%Y-%m-%d")
Static_data$DatetimeAEST <- as_date(Static_data$DatetimeAEST)
Static_data$SiteID <- as.factor(Static_data$SiteID)
Static_data <- Static_data[,1:4]


#1 Extract and process data from data.act.gov.au
# This code uses the Socrata API. I have pulled the data to 1 March 2020. From these data
# I will compute the long-term statistics


#Pull historic data

Query <- soql() %>%
  soql_add_endpoint("https://www.data.act.gov.au/resource/yuhh-28ai.json") %>%
  soql_simple_filter("VariableName", "Stream Discharge Ml/Day") %>%
  soql_where("DatetimeAEST > '2020-03-01T09:00.000'") %>% #dynamic date using sys.Date()
  soql_select("DatetimeAEST, Value, SiteID, QualityCode") %>%
  as.character()

streamflow_data <- read.socrata(Query)

#Prep data
streamflow_data$Value <- as.numeric(streamflow_data$Value)
streamflow_data$DatetimeAEST <- as.POSIXct(streamflow_data$DatetimeAEST, format = "%Y-%m-%dT%H:%M:%S")
streamflow_data$DatetimeAEST <- as_date(streamflow_data$DatetimeAEST)
streamflow_data$SiteID <- as.factor(streamflow_data$SiteID)
streamflow_data$QualityCode <- as.integer(streamflow_data$QualityCode)

streamflow_data <- bind_rows(streamflow_data, Static_data)

# Extract metadata from data.act.gov.au

Query2 <- soql() %>%
  soql_add_endpoint("https://www.data.act.gov.au/resource/tsq4-63ge.json") %>%
  soql_select("Siteid, siteName, latitude, longitude") %>%
  as.character()

My_meta <- read.socrata(Query2)

My_meta$Siteid <- as.factor(My_meta$Siteid)
My_meta$latitude <- as.numeric(My_meta$latitude)
My_meta$longitude <- as.numeric(My_meta$longitude)

#filter to relevant sites in the streamflow dataset
My_meta <- My_meta[My_meta$Siteid %in% streamflow_data$SiteID,]

My_meta$Name <- paste0(My_meta$Siteid, " - ", My_meta$siteName)

Intermediate <- My_meta %>%
  select(Siteid, Name)

streamflow_data <- left_join(streamflow_data, Intermediate, by = c("SiteID"="Siteid"))

streamflow_data$Month <- month(streamflow_data$DatetimeAEST)
streamflow_data$Year <- year(streamflow_data$DatetimeAEST)

#Read in other datasets

IQR <- read.csv("IQR.csv", header = T)
Site_FEC <- read.csv("Site_FEC.csv", header = T)

##5// UI

ui <- fluidPage(titlePanel("ACT Streamflow Explorer"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select site to plot
                    selectInput(inputId = "site", label = strong("Streamflow Gauge"),
                                choices = unique(My_meta$Name),
                                selected = "410776 - Licking Hole Creek above Cotter Junction"),
                    
                    # Select date range to be plotted
                    # sliderInput("Date", strong("Date range"), min = min(streamflow_data$DatetimeAEST), max = max(streamflow_data$DatetimeAEST),
                    #              value = c(as.Date("1980-01-01"), as.Date("2016-12-31")),
                    #              timeFormat = "%Y-%m-%d"),
                    
                    radioButtons(inputId = "timeframe", "Time frame",
                                 choices = c("last 7 days", "last 30 days", "last year", "all data", "custom"),
                                 selected = "all data"),
                    
                    conditionalPanel(condition = "input.timeframe == 'custom'",
                                     dateRangeInput("Date", strong("Date range (YYYY-mm-dd)"), min = min(streamflow_data$DatetimeAEST), max = max(streamflow_data$DatetimeAEST),
                                                    start = min(streamflow_data$DatetimeAEST), end = max(streamflow_data$DatetimeAEST))), 
                    #value = c(as.Date("1980-01-01"), as.Date("2016-12-31")),
                    #timeFormat = "%Y-%m-%d"),
                    
                    
                    # Add leaflet map
                    leafletOutput("my_map")),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotlyOutput(outputId = "lineplot", height = "400px"),
                    #textOutput(outputId = "cumplot", height = "400px"),
                    downloadButton("download", "Download data"),
                    p(),
                    tags$body("Clicking on pin on map can select a site, as can selecting from dropdown menu. Data provided by ACT Government,", a("ACT Government Open Data Portal", href= "https://www.data.act.gov.au/browse?q=ACT%20Daily%20Rainfall%20and%20Streamflow&sortBy=relevance"),". Questions and comments can be directed to
                    danswell(dot)starrs(at)act(dot)gov(dot)au. Data can be aggregated to monthly or calendar year. Aggregation method is summation. Note this is 
                    sensitive to the date picker input - partial months and years will be computed as selected on the date picker. So select whole months and years to compute meaningful statistics. 
                    Likewise, mean is computed based upon the time range selected. 
                    Code for this app can be found on", a("github", href="https://github.com/danswell/shiny_streamflow"))
                  )
                )
          )



#6// Define server function
server <- function(input, output, session) {
  
  # Subset data by site
  selected_flow <- reactive({
    streamflow_data %>%
      filter(
        Name == input$site
      )
    
  })
  
  #Update slider input to reflect site selected
  observeEvent(input$site, {
    updateDateRangeInput(session, "Date", start = min(selected_flow()$DatetimeAEST), end = max(selected_flow()$DatetimeAEST),
                         min = min(streamflow_data$DatetimeAEST), max = max(streamflow_data$DatetimeAEST))
  })
  
  #Update selected site based on map click
  observeEvent(input$my_map_marker_click, {
    p <- input$my_map_marker_click
    
    #updateSelectInput(session, "site", selected = p$Siteid) 
    updateSelectInput(session, "site", "Update my site", selected = p$id)
  })
  
  
  #rewriting this  
  
  #subset data by selected daterange
  selected_flow2 <- reactive({
    req(input$Date)
    validate(need(!is.na(input$Date[1]) & !is.na(input$Date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$Date < input$Date[2], "Error: Start date should be earlier than end date."))
    selected_flow() %>%
      filter(DatetimeAEST > input$Date[1] & DatetimeAEST < input$Date[2]
      )
  })  
  
  #####
  
  selected_flow2 <- reactive({
    switch(input$timeframe, 
           "last 7 days" = selected_flow() %>%
             filter(DatetimeAEST > max(DatetimeAEST-7))
           ,
           "last 30 days" = selected_flow() %>%
             filter(DatetimeAEST > max(DatetimeAEST-30))
           ,
           "last year" = selected_flow() %>%
             filter(DatetimeAEST > max(DatetimeAEST)-years(1))
           ,
           "all data" = selected_flow()
           ,
           "custom" =  selected_flow() %>%
             filter(DatetimeAEST > input$Date[1] & DatetimeAEST < input$Date[2]
             )
    )
  })
  
  
  output$my_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = My_meta, lng = ~longitude, lat = ~latitude, layerId = ~Name, popup = ~Name, label = ~Name) %>%
      setView(lng = 149.0, lat = -35.5, zoom = 9)
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlotly({ggplotly(selected_flow2() %>%
                                                  ggplot()+
                                                      geom_line(aes(x = DatetimeAEST, y = Value), color = "blue") + 
                                                      theme_bw()+
                                                      labs(x = "Date", y = "ML/Day", title = paste0("Streamflow at ", input$site)), tooltip = c("DatetimeAEST", "Value"))
  })
  
  #Data download
  
  dfile <-reactive({selected_flow2()
  })
  
  
  output$download <- downloadHandler(
    filename = function() {
      paste(input$site, "data.csv", sep = "_")
    },
    content = function(file){
      write.csv(dfile(), file, row.names = F)
    }
  )
}

# Create Shiny object
shinyApp(ui = ui, server = server)
