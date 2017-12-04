library(ggplot2)
#dri_beh(k25_3)
#pal <- colorNumeric(c("red", "orange", "yellow", "blue", "green"), 1:30)
trip <- c02
as.numeric(rownames(trip[trip$engine_on == 0,]))[which(diff(as.numeric(rownames(trip[trip$engine_on == 0,]))) != 1)]
trip <- c02[649:657,c(1,5,7)]

trip$lng <- substr(trip$gps_update_full,15,23)
trip$lat <- substr(trip$gps_update_full,7,14)
trip$lng_dd <- -(as.numeric(substr(trip$lng,1,3)) + as.numeric(substr(trip$lng,4,5))/60 + as.numeric(substr(trip$lng,6,9))/600000)
trip$lat_dd <- as.numeric(substr(trip$lat,1,2)) + as.numeric(substr(trip$lat,3,4))/60 + as.numeric(substr(trip$lat,5,8))/600000
trip$lng_df <- c(0, diff(trip$lng_dd))
trip$lat_df <- c(0, diff(trip$lat_dd))
trip$dir <- atan2(trip$lng_df ,trip$lat_df)*180/pi
trip$dir[1] <- trip$dir[2]
trip$dir[trip$dir <0] <- 360 + trip$dir[trip$dir <0]
trip$dir_c <- c(0, diff(trip$dir))
trip$dir_c[trip$dir_c < -180] <- 360 +trip$dir_c[trip$dir_c < -180]
trip$dir_c[trip$dir_c > 180] <- trip$dir_c[trip$dir_c > 180] - 360
#trip$dir_c[trip$dir_c > 180] <- 360 -trip$dir_c[trip$dir_c > 180] 
#trip$dir_c[trip$dir_c < -180] <- 360 +trip$dir_c[trip$dir_c < -180]
trip$dir_c_s <- trip$dir_c/c(1, diff(as.numeric(trip$time)))
#trip$dir_c_s[trip$dir_c_s > 180] <- 360 -trip$dir_c_s[trip$dir_c_s > 180] 
#trip$dir_c_s[trip$dir_c_s < -180] <- 360 +trip$dir_c_s[trip$dir_c_s < -180]
trip$sp <- strtoi((substr(trip$conditional_stream, 25, 26)), base=16)

#trip$sp_n <- trip$sp/180

#trip$rp_t <- .4*(trip$dir_c_s_n^2) + .6*(trip$sp_n^2)

trip$t_c <- c(1, diff(trip$time))
trip$dis <- c(0, sapply(2:nrow(trip),function(i){distm(trip[i-1,6:7],trip[i,6:7])}))
trip$sp_new <- trip$dis/trip$t_c*3.6
###threshold
trip$dir_c_s_n <- trip$dir_c_s/30 #30 degree
trip$sp_new_n <- trip$sp_new/15 #30km/h
trip$rp_t <- trip$dir_c_s_n * trip$sp_new_n
#trip$sp_c_s <- trip$sp_c/trip$t_c
#trip$sp_c_s[1] <- 0
#trip$rp_t <- .4*trip$dir_c_s_n + .6*trip$sp_new_n
###
trip$rp <- abs(round(trip$rp_t*100))+1
trip$sp_c <- c(0, diff(trip$sp_new))


trip_ana <- trip[,c(2,17:19,21)]
#trip_ana$dir_c_s_n <- scale(trip_ana$dir_c_s_n)
#trip_ana$sp_new_n <- scale(trip_ana$sp_new_n)
#trip_ana$rp_t <- scale(trip_ana$rp_t)
trip_ana$rp_t <- trip_ana$rp_t*2
trip_ana$time_b <- 1:nrow(trip_ana)
trip_melt <- melt(trip_ana[,c(2:4,6)], id="time_b")
trip_melt$value <- abs(trip_melt$value)

library(ggplot2)
library(cowplot)
library(leaflet)
library(shiny)

pal <- colorNumeric(c("green","blue","yellow","orange","red"), min(abs(trip$rp)):(max(abs(trip$rp))))
leaflet(trip) %>%
  addTiles('//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png') %>%
  #  addTiles() %>%
  addPolylines(~lng_dd, ~lat_dd, color= "#055ea8") %>% 
  setView(mean(trip$lng_dd), mean(trip$lat_dd), zoom = 15) %>%
  addCircleMarkers(~lng_dd, ~lat_dd, radius = 3, color = ~pal(rp), popup = ~paste(time-min(time), " secs", p(),"Lng ", lng_dd, p(), "Lat ", lat_dd, p(), "Dir ", dir, " ; ", dir_c, p(), "Dir_cs", dir_c_s, p(), "Spd ", sp_new, p(), rp))
#  addPopups(~lng_dd, ~lat_dd, paste(~dir_c_s, ~sp, ~rp))

pal <- colorNumeric(c("green","blue","yellow","orange","red"), min(abs(trip$sp_new)):(max(abs(trip$sp_new))))
leaflet(trip) %>%
  addTiles('//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png') %>%
  #  addTiles() %>%
  addPolylines(~lng_dd, ~lat_dd, color= "#055ea8") %>% 
  setView(mean(trip$lng_dd), mean(trip$lat_dd), zoom = 15) %>%
  addCircleMarkers(~lng_dd, ~lat_dd, radius = 3, color = ~pal(sp_new), popup = ~paste(time-min(time), " secs", p(),"Lng ", lng_dd, p(), "Lat ", lat_dd, p(), "Dir ", dir, " ; ", dir_c, p(), "Dir_cs", dir_c_s, p(), "Spd ", sp_new, p(), rp))

p1 <- ggplot(trip_melt, aes(time_b, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")

p2 <- ggplot(trip_ana, aes(x=time, y=sp_c)) +
  geom_line() +
  geom_point()
p3 <- ggplot(trip,aes(x=time, y=sp_new)) + 
  geom_line() +
  geom_point()
plot_grid(p1, p2, p3, labels=c(paste("rapidturn", mean(trip$rp^2/20)), paste("harsh", mean(trip_ana$sp_c^2/2)), paste("speed", mean(trip$sp_new))), ncol = 1, nrow = 3)
