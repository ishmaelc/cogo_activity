
rm(list = ls())

library(jsonlite)
library(dplyr)
library(shiny)
library(leaflet)
library(data.table)
library(readr)

# data.table::fread('http://s3.amazonaws.com/cogo-sys-data/2017-08_COGO_Trip_Data.csv',stringsAsFactors = F))
# system.time(read_csv('http://s3.amazonaws.com/cogo-sys-data/2017-08_COGO_Trip_Data.csv'))
# system.time(read.csv('http://s3.amazonaws.com/cogo-sys-data/2017-08_COGO_Trip_Data.csv'))
# system.time(read.csv('2017-08_COGO_Trip_Data.csv',stringsAsFactors = F))
# system.time(read_csv('2017-08_COGO_Trip_Data.csv'))

hist_data <- data.table::fread('2017-08_COGO_Trip_Data.csv', stringsAsFactors = F)

hist_data <- hist_data %>% filter(usertype != 'Dependent')
users <- hist_data %>% select(usertype) %>% .[[1]] %>% unique()

bike_data <- 'https://gbfs.cogobikeshare.com/gbfs/en/station_information.json'
bike_data <- 'station_information.json'

dd <- read_json(bike_data,simplifyVector = T)
dd1 <- dd$last_updated %>% as.POSIXct( origin = "1960-01-01") 

dd2 <- dd$data$stations
dd2 <- dd2 %>%
  mutate( station_id = as.numeric(station_id))

stations <- dd2 %>% select(name) %>% .[[1]]

server <- function(input,output,session){

  filtered <- reactive({

    # h <- hist_data
    h <- hist_data %>% filter(usertype %in% ifelse (is.null(input$users),users,input$users ))

    station.from <- h %>% group_by(from_station_id,usertype) %>% summarise(n.from = n() )
    station.to <- h %>% group_by(to_station_id,usertype) %>% summarise(n.to = n() )
    to.from <- cbind(station.from, station.to)

    maxn <- max(to.from$n.from, to.from$n.to)
    minn <- min(to.from$n.from, to.from$n.to)
    
    pal <<- colorNumeric(topo.colors(4), c(minn:maxn),na.color = 'green')
    # pal <- colorNumeric(c('red','green'), c(0:max(to.from$n.from, to.from$n.to)),na.color = 'green')

    count <- left_join(x = station.from,y =  dd2, by = c('from_station_id'='station_id' ))
    count <- left_join(x = station.to,y =  count, by = c('to_station_id'='from_station_id' )) %>%
      mutate(color.code.from = pal(n.from),
             color.code.to = pal(n.to))
  # select(station_id = to_station_id, n.to, n.from, lat, lon, color.code)
  })

  output$leaf <- renderLeaflet({
    leaflet() %>%
      # addTiles() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -83.00150, lat = 39.96587, zoom = 13)
   })
  output$leaf2 <- renderLeaflet({
    leaflet() %>%
      # addTiles() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -83.00150, lat = 39.96587, zoom = 13)
   })

  observe({
    
    filtered <- filtered()
    filtered1 <- filtered()
    
    leafletProxy("leaf", data = filtered )%>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        # stroke = FALSE, 
        fillOpacity = 1,
        radius = 5,
        color = ~color.code.from
        # popup = ~paste0(n.from)
        ) %>% 
        addLegend(pal = pal, values = ~n.from)

    leafletProxy("leaf2", data = filtered1 )%>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        # stroke = FALSE, 
        fillOpacity = 1,
        radius = 5,
        color = ~color.code.to
        # popup = ~paste0(n.to)
        ) %>% addLegend(pal = pal, values = ~n.to)
  })
  
}
  
ui <- fluidPage( 
  fluidRow(
    column(width=6,
      leafletOutput('leaf',height = '700px',width='100%')
    ),
    column(width=6,
    leafletOutput('leaf2',height = '700px',width='100%')
    ),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                h2("CoGo Explorer"),
                selectInput('users',label = 'Users',choices = users,selected = users ,multiple = T,selectize = T)
                # selectInput('bikes',label = 'Bike #',choices = opt,selected = 15),
                # sliderInput('bikes',
                #             label = 'Bike Capacity',
                #             min = 11,
                #             max = 19,
                #             step = 4, value = 11,ticks = F,
                #             animate = T,post = ' Bikes')
                )
))

shinyApp(ui,server)
