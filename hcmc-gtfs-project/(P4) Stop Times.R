library(stringr)
library(plyr)
library(dplyr)
library(tidyr)
library(readxl)
library(geojsonR)


geojson_stops <- FROM_GeoJson("D:/Infrastructure Victoria/Ho Chi Minh City Bus/Ho Chi Minh City OSM Bus Stops.geojson")
confirmed_busroute_datasheet <- read_excel("D:/Infrastructure Victoria/Ho Chi Minh City Bus/2019 HCMC Bus Service Plan - JK.xlsx", sheet = 'Combined Table' )

HCMC_BUS_STOPS_ROUTES_LIST <- read.csv("D:/Infrastructure Victoria/Ho Chi Minh City Bus/BUS_ROUTES_LIST.txt")
HCMC_BUS_STOPS_ROUTES_SHAPES <- read.csv("D:/Infrastructure Victoria/Ho Chi Minh City Bus/HCMC_BUS_STOPS_ROUTES_SHAPES.txt")
trips <- read.csv("D:/Infrastructure Victoria/Ho Chi Minh City Bus/trips.txt")

trips$departure_time <- strptime(trips$departure_time, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')

OSM_stops <- data.frame()
#we need to create a separate but identical point for all bus routes using the same stop 
#create a loop that extracts this information from the geojson file
for (i in 1:length(geojson_stops$features)) {
  
  if(geojson_stops$features[[i]]$geometry$type == "Point"){
    
    #for (j in 1:length(geojson$features[[i]]$properties$`@relations`)){ #the number of iterations for inner loop
      
      OSM_stops[i, "stop_node_id"] <- geojson_stops$features[[i]]$properties$`@id`  #the node id 
      
      if(!is.null(geojson_stops$features[[i]]$properties$bus)){
      OSM_stops[i, "bus_stop"] <- geojson_stops$features[[i]]$properties$bus} else {OSM_stops[i, "bus_stop"] <- ""} #is this a bus stop?
      
      if(!is.null(geojson_stops$features[[i]]$properties$name)){
      OSM_stops[i, "name"] <- geojson_stops$features[[i]]$properties$name} else {OSM_stops[i, "name"] <- ""} # the stop location name
      
      OSM_stops[i, "long"] <- geojson_stops$features[[i]]$geometry$coordinates[1] #long
      OSM_stops[i, "lat"] <- geojson_stops$features[[i]]$geometry$coordinates[2] #lat
      
      print(i)
      
    }
    
}
  
#generating the shapes.txt file for creating the stop_times.txt file 
HCMC_BUS_STOPS_ROUTES_LIST_A <- HCMC_BUS_STOPS_ROUTES_LIST %>%
  select(route_number, from, to, route_long_name) #route_long_name is the same as "OSM Routes Table"

HCMC_BUS_STOPS_ROUTES_SHAPES_A <- HCMC_BUS_STOPS_ROUTES_SHAPES %>%
  select(stop_node_id, route_code, route_number, from, to, lat, long, shape_pt_sequence, shape_dist_traveled) 

HCMC_BUS_STOPS_ROUTES_SHAPES_B <- merge(HCMC_BUS_STOPS_ROUTES_LIST_A, HCMC_BUS_STOPS_ROUTES_SHAPES_A, by = c("route_number", "from", "to")) %>% arrange(route_number, from, shape_pt_sequence) 

HCMC_BUS_STOPS_ROUTES_SHAPES_C <- merge(HCMC_BUS_STOPS_ROUTES_SHAPES_B, OSM_stops, by = "stop_node_id", all = T) 



travel_times <- confirmed_busroute_datasheet %>% 
  select(`OSM Concordance`, `OSM Routes Table`, `Travel time - lower`, `Travel time - upper`) %>%
  filter(!is.na(`OSM Concordance`),
         !is.na(`OSM Routes Table`),
         !is.na(`Travel time - lower`)) %>%
  rename(route_long_name = `OSM Routes Table`,
         route_number = `OSM Concordance`)


HCMC_BUS_STOPS_ROUTES_SHAPES_D <- merge(HCMC_BUS_STOPS_ROUTES_SHAPES_C, travel_times, by = c("route_long_name")) 
  

HCMC_BUS_STOPS_ROUTES_SHAPES_E <- HCMC_BUS_STOPS_ROUTES_SHAPES_D %>%
  group_by(route_code) %>% 
  mutate(route_proportion = shape_dist_traveled/max(shape_dist_traveled),
         offpeak_elapsed_time = route_proportion * `Travel time - lower`,
         peak_elapsed_time = route_proportion * `Travel time - upper`) 
  



stop_times <- data.frame()

for (i in 1:length(trips[,1])){
  
  one_trip <- HCMC_BUS_STOPS_ROUTES_SHAPES_E %>% 
    filter(trips[i, "route_number"] == route_number.x, 
           trips[i, "trip_headsign"] == to) %>%
    arrange(shape_pt_sequence) %>% #the sequence for each route-direction remains the same (in this case)
    select(stop_node_id, shape_pt_sequence, shape_dist_traveled, peak_elapsed_time,  offpeak_elapsed_time)
  
  
  if(trips[i, "departure_time"] <  strptime("1899-12-31 05:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')){
    
    one_trip$trip_id <- trips[i, "trip_id"]
    one_trip$departure_time <-trips[i, "departure_time"] + one_trip$offpeak_elapsed_time*60
    one_trip$arrival_time <- one_trip$departure_time #in the absence of any further information
    one_trip$pick_up_type <- 0
    one_trip$drop_off_type <- 0
    one_trip$stop_headsign <- ""
    
  }
  
  else if((trips[i, "departure_time"] >=  strptime("1899-12-31 05:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')) & (trips[i, "departure_time"] <  strptime("1899-12-31 07:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))){
    
    one_trip$trip_id <- trips[i, "trip_id"]
    one_trip$departure_time <-trips[i, "departure_time"] + one_trip$peak_elapsed_time*60
    one_trip$arrival_time <- one_trip$departure_time #in the absence of any further information
    one_trip$pick_up_type <- 0
    one_trip$drop_off_type <- 0
    one_trip$stop_headsign <- ""
    
  }
  
  else if((trips[i, "departure_time"] >=  strptime("1899-12-31 07:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')) & (trips[i, "departure_time"] <  strptime("1899-12-31 15:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))){
    
    one_trip$trip_id <- trips[i, "trip_id"]
    one_trip$departure_time <-trips[i, "departure_time"] + one_trip$offpeak_elapsed_time*60
    one_trip$arrival_time <- one_trip$departure_time #in the absence of any further information
    one_trip$pick_up_type <- 0
    one_trip$drop_off_type <- 0
    one_trip$stop_headsign <- ""
    
  }
  
  
  else if((trips[i, "departure_time"] >=  strptime("1899-12-31 15:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')) & (trips[i, "departure_time"] <  strptime("1899-12-31 17:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))){
    
    one_trip$trip_id <- trips[i, "trip_id"]
    one_trip$departure_time <-trips[i, "departure_time"] + one_trip$peak_elapsed_time*60
    one_trip$arrival_time <- one_trip$departure_time #in the absence of any further information
    one_trip$pick_up_type <- 0
    one_trip$drop_off_type <- 0
    one_trip$stop_headsign <- ""
    
  }
  
  
  else if((trips[i, "departure_time"] >=  strptime("1899-12-31 18:00:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')) & (trips[i, "departure_time"] <  strptime("1899-12-31 23:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))){
    
    one_trip$trip_id <- trips[i, "trip_id"]
    one_trip$departure_time <-trips[i, "departure_time"] + one_trip$offpeak_elapsed_time*60
    one_trip$arrival_time <- one_trip$departure_time #in the absence of any further information
    one_trip$pick_up_type <- 0
    one_trip$drop_off_type <- 0
    one_trip$stop_headsign <- ""
    
  }
  
  stop_times <- rbind.fill(one_trip, stop_times)
  print(i)
  
}
  

stop_times_final <- stop_times %>%
  select(trip_id, departure_time, arrival_time, stop_node_id, shape_pt_sequence, stop_headsign, pick_up_type, drop_off_type, shape_dist_traveled) %>%
  rename(stop_id = stop_node_id,
         stop_sequence = shape_pt_sequence)

write.table(stop_times_final, "stop_times.txt", fileEncoding = "UTF-8", sep = ",", row.names = F)

stops <- OSM_stops %>% filter(!is.na(bus_stop)) %>%
  rename(stop_id = stop_node_id,
         stop_name = name,
         stop_lat = lat,
         stop_lon = long) %>%
  select(-bus_stop)

write.table(stops, "stops.txt", fileEncoding = "UTF-8", sep = ",", row.names = F)



