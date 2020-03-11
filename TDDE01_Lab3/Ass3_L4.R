set.seed(1234567890)
library(geosphere)

stations <- read.csv("/home/augjo318/Desktop/TDDE01_Lab3/stations.csv", fileEncoding="latin1")
temps <- read.csv("/home/augjo318/Desktop/TDDE01_Lab3/temps50k.csv")
st <- merge(stations,temps,by="station_number")
h_distance <- 100*1000 # These three values are up to the students
h_date <- 30
h_time <- 4

a <- 62.3908
b <- 17.3069


my_date <- "2004-09-03" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00",
           "14:00:00", "16:00:00", "18:00:00", "20:00:00","22:00:00", "24:00:00")

temp <- vector(length=length(times))

# Students’ code here
# 1. distance from a station to point of interest (a)
# 2. distance between the day a temperature measurement was made + the day of interest
# 3. Distance between the hour of the day a temperature measurement was made

stat <- subset(st, as.Date(st$date) < as.Date(my_date))
stat
#distance
pos <- cbind(stat[,5],stat[,4])

#distance difference stations to PoI 1.1
distance_diff <- 1:length(stat[,1])
for (i in 1:length(stat[,1])) {
  x <- distm(c(b, a), c(stat$longitude[i], stat$latitude[i]), fun=distHaversine)
  m <- x/h_distance
  distance_diff[i] = exp(-m^2)
}
distance_diff

plot(1:length(distance_diff), distance_diff) # should be similar to kernel function

#difference in days 1.2
#Distance between the day a measurement was made and day of interest

#stat, my_date, h_date
date_diff <- 1:length(stat[,1])
for(i in 1:length(stat[,1])) {
  x <- as.numeric(as.Date(my_date)-as.Date(stat$date[i]))
  m <- x/h_date
  date_diff[i] <- exp(-(m)^2)
}

plot(1:length(date_diff), date_diff)

#hour of the day 1.3
# Distance between the hour of the dat a temperature measurement was made
# and the hour of interest
# sel dates, times, h_time

#test1
##############################
t1 <- hms(stat$time[1])
t2 <- hms(times[1])
t2
t1
p <- t1-t2
p

timed <- function(data, mytime, h) {
  ret <- 1:length(data[,1])
  for(i in ret) {
    tim1 <- as.POSIXct(as.character(data$time[i]), format = "%H:%M:%S", tz="UTC")
    tim2 <- as.POSIXct(as.character(mytime), format = "%H:%M:%S", tz="UTC")
    
    x <- (tim1-tim2)/h
    
    ret[i] <- exp(-as.numeric(x)^2) 
  }
  return(ret)
}

timed2 <- function(data, mytime, h) {
  ret <- 1:length(data[,1])
  for(i in ret) {
    t1 <- hms(as.character(data$time[i]))
    t2 <- hms(as.character(mytime))
    
    x <- (tim1-tim2)/h
    
    ret[i] <- exp(-as.numeric(x)^2) 
  }
  return(ret)
}

#######################
dt <- 1:length(times)
for(i in 1:length(times)) {
  #dt <- convert_hours(stat, times[i], h_time)
  diffs_time <- timed2(stat, times[i], h_time)
  
  kernel_sum <- distance_diff + date_diff + diffs_time
  kernel_prod <- distance_diff * date_diff * diffs_time
  
  temp[i] <- sum(kernel_sum*stat$air_temperature)/sum(kernel_sum)
}

plot(1:length(dt), dt)

# use kernel that is the sum if three gaussian kernels:
# * The first to account for the distance from a station to the point of interest


plot(temp, type="o", xlab = "Hours", ylab = "Temperature2", xaxt="n")
axis(1, at=1:length(temp), labels=times)
print("hej")





