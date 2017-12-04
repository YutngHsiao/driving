get_track <- function(x) {
  time <- gsub(" ", "%20", Sys.time())
  time_p <- gsub(" ", "%20", Sys.time()-86400*7)
  req.url <- paste0("https://")
  req <- httr::GET(req.url)
  json <- httr::content(req, as="text", encoding="UTF-8")#, as = "text")
  json <- as.data.frame(fromJSON(json))
  return(json)
}

driver1 <- get_track(imei)

req.url <- paste0("https://")
req.url <- paste0("https://")
req <- httr::GET(req.url)
json <- httr::content(req, as="text", encoding="UTF-8")
json <- as.data.frame(fromJSON(json))

C <- json
C_0420_ori <- json
C <- C[,-c(2,3,4,6)]
C$lng <- substr(C$gps_update_full,15,23)
C$lat <- substr(C$gps_update_full,7,14)
C$lng_dd <- -(as.numeric(substr(C$lng,1,3)) + as.numeric(substr(C$lng,4,5))/60 + as.numeric(substr(C$lng,6,9))/600000)
C$lat_dd <- as.numeric(substr(C$lat,1,2)) + as.numeric(substr(C$lat,3,4))/60 + as.numeric(substr(C$lat,5,8))/600000
library(ggplot2)

ggplot(C_melt, aes(x=time, y=value, fill=variable))  + geom_line()

###location (turn)
C$lng_n <- as.numeric(C$lng)
C$lat_n <- as.numeric(C$lat)
C$lng_n1 <- scale(C$lng_n)
C$lat_n1 <- scale(C$lat_n)
C_melt <- melt(C[,c(2,8,9)], id="time")
ggplot(C_melt, aes(x=time, y=value, group=variable, colour=variable)) +
  geom_line() +
  geom_point()

###diff location (turn)
C$lng_dif <- c(0, diff(C$lng_n1))
C$lat_dif <- c(0, diff(C$lat_n1))
C_melt_d <- melt(C[,c(2,10:11)], id="time")
ggplot(C_melt_d, aes(x=time, y=value, group=variable, colour=variable)) +
  geom_line() +
  geom_point()

###speed
C$speed <- substr(C$conditional_stream, 25, 26)
C$speed <- gsub(":", "", C$speed)
C$speed <- strtoi(C$speed, base = 16)
ggplot(C, aes(x=time, y=speed)) +
  geom_line() +
  geom_point()
C$cr <- c(0, diff(C$speed)/(C$speed)[-length(C$speed)])
ggplot(C, aes(x=time, y=cr)) +
  geom_line() +
  geom_point() + scale_color_gradient2(midpoint=0, low="blue", mid="white", high="orange" )

###rpm
C$rpm <- substr(C$conditional_stream, 16, 20)
C$rpm <- gsub(":", "", C$rpm)
C$rpm <- strtoi(C$rpm, base = 16)/4
ggplot(C, aes(x=time, y=rpm)) +
  geom_line() +
  geom_point()

###lnglat change
C$lnglatchange <- C$lng_dif-C$lat_n1
ggplot(C, aes(x=time, y=lnglatchange)) +
  geom_line() +
  geom_point()


###location diff
req.url <- paste0("https://")
req <- httr::GET(req.url)
k <- httr::content(req, as="text", encoding="UTF-8")
k <- as.data.frame(fromJSON(k))

k_bk <- k
k <- k[,-c(2,3,4,6)]
k$lng <- substr(k$gps_update_full,15,23)
k$lat <- substr(k$gps_update_full,7,14)
k$lng_dd <- -(as.numeric(substr(k$lng,1,3)) + as.numeric(substr(k$lng,4,5))/60 + as.numeric(substr(k$lng,6,9))/600000)
k$lat_dd <- as.numeric(substr(k$lat,1,2)) + as.numeric(substr(k$lat,3,4))/60 + as.numeric(substr(k$lat,5,8))/600000

k$lng_dif <- 1000000* (k$lng_dd- c(0, k$lng_dd[-nrow(k)]))

k_dir$dir <- atan2(k_dir$lng_df ,k_dir$lat_df)*180/pi

k$lat_dif <- 
  
  
  which(k$dir_c == max(k$dir_c))
as.numeric(rownames(k[k$engine_on == 1,]))[which(diff(as.numeric(rownames(k[k$engine_on == 1,]))) != 1)]+1
#[1]    7   47   73  113  202  259  309  386  432  542  608  648  746  805  891  975 1003 1079 1116 1190
as.numeric(rownames(k[k$engine_on == 0,]))[which(diff(as.numeric(rownames(k[k$engine_on == 0,]))) != 1)]
#[1]    8   48   76  115  204  260  311  389  435  544  610  651  748  807  892  980 1006 1081 1118
k_dir$dir <- atan2(k_dir$lng_df ,k_dir$lat_df)*180/pi
k_dir$dir_c_s <- k_dir$dir_c/c(1, diff(as.numeric(k_dir$time)))
k_dir$dir_c_s[k_dir$dir_c_s > 180] <- 360 -k_dir$dir_c_s[k_dir$dir_c_s > 180] 
k_dir$dir_c_s[k_dir$dir_c_s < -180] <- 360 +k_dir$dir_c_s[k_dir$dir_c_s < -180]

k_melt <- melt(k_dir[,c(1,8,10)], id="time")
ggplot(k_melt, aes(x=time, y=value, group=variable, colour=variable)) +
  geom_line() +
  geom_point()

#k$dir_360[k$dir_360 < 0] <- 360+ k$dir_360[k$dir_360 < 0]
k_dir$dir_c <- c(0, 0, diff(k_dir$dir)[-1])
k_dir$dir_o <-as.numeric(substr(k$gps_update_full,27,28))
k_dir$dir_o[k_dir$dir_o >18] <- k_dir$dir_o[k_dir$dir_o >18] -36
k_dir$dir_o <- k_dir$dir_o*10


###COS function
get_cos <- function(x,y){
  cosine <- sum(x*y)/(sqrt(sum(x*x)) * sqrt(sum(x*x)))
  return (cosine)
}
###
ggplot(k_dir, aes(x=time, y=dir_c_s)) +
  geom_line() +
  geom_point()


ggplot(k[1:115,], aes(x=time, y=dir_c)) +
  geom_line() +
  geom_point()

k_melt_d <- melt(k[,c(2,9:10)], id="time")
ggplot(k_melt_d, aes(x=time, y=value, group=variable, colour=variable)) +
  geom_line() +
  geom_point()


###k 0424
req.url <- paste0("https://")
req <- httr::GET(req.url)
k24 <- httr::content(req, as="text", encoding="UTF-8")
k24 <- as.data.frame(fromJSON(k24))


###speed

k_dir$sp <- strtoi((substr(k_bk$conditional_stream, 25, 26)), base=16)
k_dir$dir_c_s_n <- k_dir$dir_c_s/180
k_dir$sp_n <- k_dir$sp/180
k_dir$rp_t <- .4*k_dir$dir_c_s_n + .6*k_dir$sp_n

ggplot(k_dir[1:115,], aes(x=time, y=rp_t)) +
  geom_line() +
  geom_point()

k_m <- k_dir[,c(1,11, 13:14)]
k_melt <- melt(k_m[14:20,], id="time")
ggplot(k_melt, aes(time, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")

k_spd <- k_bk[,2:3]
k_spd$sp_c <- c(0, diff(k_spd$sp))
k_spd$t_c <- c(1, diff(k_spd$time))
k_spd$sp_c_s <- k_spd$sp_c/k_spd$t_c
k_spd$sp_c_s[1] <- 0

ggplot(k_spd[1:115,], aes(x=time, y=sp_c_s)) +
  geom_line() +
  geom_point()


req.url <- paste0("https://")
req <- httr::GET(req.url)
k25 <- httr::content(req, as="text", encoding="UTF-8")
k25 <- as.data.frame(fromJSON(k25))

req.url <- paste0("https://")
req <- httr::GET(req.url)
c02 <- httr::content(req, as="text", encoding="UTF-8")#, as = "text")
c02 <- as.data.frame(fromJSON(c02))

d <- 23
c02$t_d <- c(0, diff(c02$time))
c02$t_d[c02$t_d < d] <- 1
c02$t_d[c02$t_d >= d] <- 0

p1 <- ggplot(c02, aes(x=time, y=engine_on)) +
  geom_bar()
p2 <- ggplot(c02,aes(x=time, y=t_d)) + 
  geom_bar()
plot_grid(p1, p2, ncol = 1, nrow = 2)


req.url <- paste0("https://")
req <- httr::GET(req.url)
driver <- httr::content(req, as="text", encoding="UTF-8")#, as = "text")
driver <- as.data.frame(fromJSON(driver))

a <- "This is a \\string."
gsub("\\p{P}", "", a, perl=TRUE)