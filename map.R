library(shiny)
library(leaflet)
library(reshape2)
library(geosphere)
library(plotly)

ui <- bootstrapPage(
  fluidRow(
#    column(3, align="center",
#           absolutePanel(top = 10, left = 10, width = 390, draggable = F, id = "sidebar",
#                         h3("Car tracking"),
#                         p()
#           )
#    ),
    column(12, leafletOutput("map2", height = 300))
  ),
  fluidRow(
    p(),
    column(12, plotlyOutput("chart1", height = 200)),
    p()
  ),
  fluidRow(
    p(),
    column(12, plotlyOutput("chart2", height = 200))
  ),
  fluidRow(
    p(),
    column(12, plotlyOutput("p", height = 300))
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(trip) %>%
      addTiles('//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png') %>%
      addPolylines(~lng_dd, ~lat_dd) %>% 
      setView(mean(trip$lng_dd), mean(trip$lat_dd), zoom = 14)
  })
  trip <- s0612
  trip <- trip[,c(1,5,7)]
#  trip$lng <- substr(trip$gps_update_full,15,23)
#  trip$lat <- substr(trip$gps_update_full,7,14)
#  trip$lng_dd <- -(as.numeric(substr(trip$lng,1,3)) + as.numeric(substr(trip$lng,4,5))/60 + as.numeric(substr(trip$lng,6,9))/600000)
#  trip$lat_dd <- as.numeric(substr(trip$lat,1,2)) + as.numeric(substr(trip$lat,3,4))/60 + as.numeric(substr(trip$lat,5,8))/600000
  trip$lng_df <- c(0, diff(trip$lng_dd))
  trip$lat_df <- c(0, diff(trip$lat_dd))
  trip$dir <- atan2(trip$lng_df ,trip$lat_df)*180/pi
  trip$dir[1] <- trip$dir[2]
  trip$dir[trip$dir <0] <- 360 + trip$dir[trip$dir <0]
  trip$dir_c <- c(0, diff(trip$dir))
  trip$dir_c[trip$dir_c < -180] <- 360 +trip$dir_c[trip$dir_c < -180]
  trip$dir_c[trip$dir_c > 180] <- trip$dir_c[trip$dir_c > 180] - 360
  trip$dir_c_s <- trip$dir_c/c(1, diff(as.numeric(trip$time)))
  trip$sp <- strtoi((substr(trip$conditional_stream, 25, 26)), base=16)
  trip$t_c <- c(1, diff(trip$time))
  trip$dis <- c(0, sapply(2:nrow(trip),function(i){distm(trip[i-1,6:7],trip[i,6:7])}))
  trip$sp_new <- trip$dis/trip$t_c*3.6
  trip$dir_c_s_n <- trip$dir_c_s/30 #30 degree
  trip$sp_new_n <- trip$sp_new/15 #30km/h
  trip$rp_t <- trip$dir_c_s_n * trip$sp_new_n^2
  trip$rp <- abs(round(trip$rp_t*100))+1
  trip$sp_c <- c(0, diff(trip$sp_new))
  trip_ana <- trip[,c(2,17:19,21)]
  trip_ana$rp_t <- trip_ana$rp_t*2
  trip_ana$time_b <- 1:nrow(trip_ana)
  trip_melt <- melt(trip_ana[,c(2:4,6)], id="time_b")
  trip_melt$value <- abs(trip_melt$value)
  trip_ana$dir_c_s_n <- trip_ana$dir_c_s_n/(max(trip_ana$dir_c_s_n)-min(trip_ana$dir_c_s_n))
  trip_ana$sp_new_n <-trip_ana$sp_new_n/(max(trip_ana$sp_new_n)-min(trip_ana$sp_new_n))
  trip_ana$sp_c <- trip_ana$sp_c/(max(trip_ana$sp_c)-min(trip_ana$sp_c))
  trip_ana$rp_t <- trip_ana$rp_t/(max(trip_ana$rp_t)-min(trip_ana$rp_t))
  
  chart1 <- plot_ly(trip_ana, x = ~time_b, y = ~(sp_new_n), name = 'Speed', type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
    add_trace(y = ~(dir_c_s_n), name = 'Direction change', mode = 'lines') %>%
    config(displayModeBar = F) %>% 
    layout(title = "Driving Context", xaxis = list(title = "Time",showgrid = F), yaxis = list(title = "Degree"))
  
  chart2 <- plot_ly(trip_ana, x = ~time_b, y = ~(rp_t), name = 'Rapidturn', type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
    add_trace(y = ~(sp_c), name = 'Harsh Acc/Dec', mode = 'lines') %>% 
    config(displayModeBar = F) %>% 
    layout(title = "Behaviour Analysis", xaxis = list(title = "Time",showgrid = F), yaxis = list(title = "Degree"))
  
  p <- plot_ly() %>%
    add_pie(data = count(diamonds, cut), labels = ~cut, values = ~n,
            name = "Score", domain = list(x = c(0, 0.4), y = c(0.4, 1))) %>%
    add_pie(data = count(diamonds, color), labels = ~cut, values = ~n,
            name = "Gasolin", domain = list(x = c(0.6, 1), y = c(0.4, 1))) %>%
#    add_pie(data = count(diamonds, clarity), labels = ~cut, values = ~n,
#            name = "Clarity", domain = list(x = c(0.25, 0.75), y = c(0, 0.6))) %>%
    config(displayModeBar = F) %>%
    layout(title = "Driving Scores", showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  
  output$map2 <- renderLeaflet({
    leaflet(trip) %>%
      addTiles('//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png') %>%
      setView(mean(trip$lng_dd), mean(trip$lat_dd), zoom = 14)
  })
  pal <- colorNumeric(c("green","blue","orange", "red"), 1:nrow(seg))
  for (i in 1:nrow(seg)) {
    proxy <- leafletProxy("map2", data = trip[seg[i,1]:seg[i,2],])
    proxy %>% addPolylines(~lng_dd, ~lat_dd, color = pal(i), popup = paste("trip", i, p(paste("overspeed : ", i*3)),
                                                                           p(paste("rapidturn : ", i*2)),
                                                                           p(paste("score : ", i*2.5))), layerId = i)
  }
  output$chart1 <- renderPlotly({
    chart1
  })
  output$chart2 <- renderPlotly({
    chart2
  })
  output$p <- renderPlotly({
    p
  })
#  observeEvent(input$map_shape_click$id, {
#    seq <- input$map_shape_click$id
#    print(seq)
#  })
  #  proxy <- leafletProxy("map2", data = tracking)
  #  proxy %>% addMarkers(tracking.detail$Lng, tracking.detail$Lat, icon = Icn)
}

shinyApp(ui, server)