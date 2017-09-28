
# clear objects in memory
rm(list = ls())


### firewall workaround 
# httr::set_config( config( ssl_verifypeer = 0L ) )
# devtools::install_github('hadley/ggplot2')

# Load packages

# importing data
library(jsonlite)
library(data.table)

# transforming data
library(dplyr)
library(lubridate)

# mapping
library(leaflet)

# plotting
library(ggplot2)
library(plotly)

# publishing
library(shiny)
library(shinydashboard)

# import CoGo historical data
# source- https://s3.amazonaws.com/cogo-sys-data/index.html
hist_data <- fread('2017-08_COGO_Trip_Data.csv', stringsAsFactors = F, drop = c('trip_id'),nrows = 1000)

# extra dates and filter out Dependents (too few to use)
hist_data <- hist_data %>% 
  filter(usertype != 'Dependent', tripduration < 1800) %>%
  mutate(start.date = as.Date(start_time, format ='%m/%d/%Y')) %>% 
  mutate(trip.minutes = round(tripduration / 60,0),
         # day2 = weekdays(start.date,abbreviate = T),
         day = as.character(wday(start.date)),
         # month2 = months(start.date),
         month = format(start.date,'%m')) %>%
  mutate(day = if_else(day == '1', '8', day)) %>%
  mutate(monthn = as.numeric(month))
  
#grab min and max dates to limit Date Range input
dates <- hist_data %>% summarise(min.start = min(start.date),max.start = max(start.date))

#identify unique values of usertype
users <- hist_data %>% select(usertype) %>% .[[1]] %>% unique()

## import station information
# source- 'https://gbfs.cogobikeshare.com/gbfs/en/station_information.json'
bike_data <- 'station_information.json'
dd <- read_json(bike_data,simplifyVector = T)

dd1 <- dd$last_updated %>% as.POSIXct( origin = "1960-01-01") 

dd2 <- dd$data$stations %>%
  mutate( station_id = as.numeric(station_id)) %>% #convert to numeric for merging with historical data
  select(-cross_street) #cross_street field incomplete

stations <- dd2 %>% select(name) %>% .[[1]] #identify unique station names

server <- function(input,output,session){

  filtered <- reactive({

    h <- hist_data %>% 
      filter(
        # usertype %in% ifelse (is.null(input$users),users,input$users)
        # start.date >= input$start.date[1],
        # start.date <= input$start.date[2],
        monthn == input$month
             )
    # station.from <- h %>% group_by(from_station_id) %>% summarise(n.from = n() )
    # station.to <- h %>% group_by(to_station_id) %>% summarise(n.to = n() )
    # to.from <- cbind(station.from, station.to)
    # maxn <- max(to.from$n.from, to.from$n.to)
    # minn <- min(to.from$n.from, to.from$n.to)
    # pal <<- colorNumeric(topo.colors(4), c(minn:maxn),na.color = 'green')
    # # pal <- colorNumeric(c('red','green'), c(0:max(to.from$n.from, to.from$n.to)),na.color = 'green')
    # count <- left_join(x = station.from,y =  dd2, by = c('from_station_id'='station_id' ))
    # count <- left_join(x = station.to,y =  count, by = c('to_station_id'='from_station_id' )) %>%
    #   mutate(color.code.from = pal(n.from),
    #          color.code.to = pal(n.to))
  # select(station_id = to_station_id, n.to, n.from, lat, lon, color.code)
  })
  
  output$duration <- renderPlotly({
    g <- ggplot(filtered(), aes(trip.minutes, fill = usertype)) +
      geom_bar(position = 'dodge')
    j <- ggplotly(g)
    })
  
  output$day <- renderPlotly({
    g <- ggplot(filtered(), aes(day, fill = usertype)) +
      geom_bar(position='dodge')
      # geom_bar(position = 'dodge')
    j <- ggplotly(g)
    })

  # output$month <- renderPlotly({
  output$month <- renderPlot({
    ggplot(hist_data, aes(month, fill = usertype)) +
      geom_bar(position = 'dodge')
        # ggplotly(t)
    })
  
  output$leaf <- renderLeaflet({
    # leaflet() %>%
    #   # addTiles() %>%
    #   addTiles(
    #     urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    #     attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    #   ) %>%
    #   setView(lng = -83.00150, lat = 39.96587, zoom = 13)
   })
  output$leaf2 <- renderLeaflet({
    # leaflet() %>%
    #   # addTiles() %>%
    #   addTiles(
    #     urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    #     attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    #   ) %>%
    #   setView(lng = -83.00150, lat = 39.96587, zoom = 13)
   })

  observe({
    
    # filtered <- filtered()
    
    # leafletProxy("leaf", data = filtered )%>%
    #   clearMarkers() %>%
    #   clearControls() %>%
    #   addCircleMarkers(
    #     lng = ~lon, 
    #     lat = ~lat,
    #     # stroke = FALSE,
    #     fillOpacity = 1,
    #     radius = 5,
    #     color = ~color.code.from,
    #     popup = ~paste0(name,': ', n.from)
    #     ) %>%
    #     addLegend(pal = pal, values = ~n.from)

  })
  
}

ui <- 
  dashboardPage(title = 'CoGo Activity',skin = 'red',
    dashboardHeader(title = HTML(paste('CoGo Activity',icon('bicycle')))
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem(text = 'Map',icon = icon('map'),tabName = 'map'),
        menuItem(text = 'Analysis',icon = icon('bar-chart'),tabName = 'analysis',selected = T),
        menuItem(text = 'Code',icon = icon('github'),href = 'https://github.com/ishmaelc')
      )
    ),
    dashboardBody(
  tabItems(
    tabItem('map',
      fluidRow(
        # tags$script(' var setInitialCodePosition = function() { setCodePosition(false, false); }; '),
        column(width=6
          # leafletOutput('leaf',height = '700px',width='100%')
        ),
        column(width=6
               # plotOutput('duration')
        # leafletOutput('leaf2',height = '700px',width='100%')
        ),
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    h2("CoGo Stations"),
                    selectInput('users',label = 'Users',choices = users,selected = users ,multiple = T,selectize = T),
                    # dateRangeInput('start.date',label= 'Time Span',min = dates$min.start, max = dates$max.start)
                    dateRangeInput('start.date',label= 'Time Span',start = '2017-01-01', end = '2017-08-31')
                    # selectInput('bikes',label = 'Bike #',choices = opt,selected = 15),
                    # sliderInput('bikes',
                    #             label = 'Bike Capacity',
                    #             min = 11,
                    #             max = 19,
                    #             step = 4, value = 11,ticks = F,
                    #             animate = T,post = ' Bikes')
                    ))),
    tabItem('analysis',
             fluidRow(
             sliderInput('month',label = 'Month Selector',value = 8, animate = T, min = 1,max = 12,ticks = F),
               column(width = 6,
                 plotlyOutput('duration')
                      ),
               column(width= 6,
                 plotlyOutput('day')
                      )
             ),
             br(),
             fluidRow(
               column(width = 6,
                 plotOutput('month')
                      ),
               column(width= 6
                      )
             )
             ))
))

shinyApp(ui,server)