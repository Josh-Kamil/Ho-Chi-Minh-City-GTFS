library(stringr)
library(plyr)
library(dplyr)
library(tidyr)
library(readxl)



confirmed_busroute_datasheet <- read_excel("D:/Infrastructure Victoria/Ho Chi Minh City Bus/2019 HCMC Bus Service Plan - JK.xlsx", sheet = 'Combined Table' )

HCMC_BUS_STOPS_ROUTES_LIST <- read.csv("D:/Infrastructure Victoria/Ho Chi Minh City Bus/BUS_ROUTES_LIST.txt")
HCMC_BUS_STOPS_ROUTES_SHAPES <- read.csv("D:/Infrastructure Victoria/Ho Chi Minh City Bus/HCMC_BUS_STOPS_ROUTES_SHAPES.txt")
routes <- read.csv("D:/Infrastructure Victoria/Ho Chi Minh City Bus/routes.txt")


#generating the shapes.txt file
HCMC_BUS_STOPS_ROUTES_LIST_A <- HCMC_BUS_STOPS_ROUTES_LIST %>%
  select(route_number, from, to, route_long_name) #route_long_name is the same as "OSM Routes Table"

HCMC_BUS_STOPS_ROUTES_SHAPES_A <- HCMC_BUS_STOPS_ROUTES_SHAPES %>%
  select(route_number, from, to, lat, long, shape_pt_sequence, shape_dist_traveled) 

HCMC_BUS_STOPS_ROUTES_SHAPES_B <- merge(HCMC_BUS_STOPS_ROUTES_LIST_A, HCMC_BUS_STOPS_ROUTES_SHAPES_A, by = c("route_number", "from", "to")) %>% arrange(route_number, from, shape_pt_sequence) 


HCMC_BUS_STOPS_ROUTES_SHAPES_B$shape_id <- paste('24', HCMC_BUS_STOPS_ROUTES_SHAPES_B$route_number ,sapply(str_extract_all(HCMC_BUS_STOPS_ROUTES_SHAPES_B$from, '[:upper:]{1,}'),paste0, collapse = ''), 
                                                 sapply(str_extract_all(HCMC_BUS_STOPS_ROUTES_SHAPES_B$to, '[:upper:]{1,}'),paste0, collapse = ''),
                                                 'vn','1', sep = '-')


shapes <- HCMC_BUS_STOPS_ROUTES_SHAPES_B %>% 
  rename(shape_pt_lat = lat,
         shape_pt_lon = long) %>%
  select(shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence, shape_dist_traveled)

write.table(shapes, "shapes.txt", fileEncoding = "UTF-8", sep = ",", row.names = F)

#creating UID for shapes to merge with operator data lower down
shapes_uid <- HCMC_BUS_STOPS_ROUTES_SHAPES_B %>% select(route_number, from, to, shape_id) %>% distinct()


###handling raw input data from operators
confirmed_busroute_A <- confirmed_busroute_datasheet %>% 
  filter(!is.na(`OSM Routes Table`)) %>% #filter out bus routes that DO NOT feature on Buyttphcm website or within Open Street Maps 
  select(`Subsity Status`,
         `Route ID`,
         `Route Name`,
         `OSM Routes Table`,
         #`Trip/day - CN`, `Trip/day - T2`, `Trip/day - T3`, `Trip/day - T4`, `Trip/day - T5`, `Trip/day - T6`, `Trip/day - T7`,
         `Travel time - lower`,
         `Travel time - upper`,
         `Peak hour headway - CN`, `Peak hour headway - T2`, `Peak hour headway - T3`, `Peak hour headway - T4`, `Peak hour headway - T5`, `Peak hour headway - T5`, `Peak hour headway - T6`, `Peak hour headway - T7`,
         `Non-peak hour headway - CN`, `Non-peak hour headway - T2`, `Non-peak hour headway - T3`, `Non-peak hour headway - T4`, `Non-peak hour headway - T5`, `Non-peak hour headway - T6`, `Non-peak hour headway - T7`,
         `Openning Hour`,
         `Ending hour`) %>%
  filter(!is.na(`Peak hour headway - T2`)) %>%
  rename(route_long_name = `OSM Routes Table`)



confirmed_busroute_B <- merge(confirmed_busroute_A, HCMC_BUS_STOPS_ROUTES_LIST, by = "route_long_name")

confirmed_busroute_C <- merge(confirmed_busroute_B, routes, by = "route_long_name")

confirmed_busroute_D <- merge(confirmed_busroute_C, shapes_uid, by = c("route_number", "from", "to"))

confirmed_busroute_D$`Openning Hour` <- strptime(confirmed_busroute_D$`Openning Hour`, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC' )
confirmed_busroute_D$`Ending hour` <- strptime(confirmed_busroute_D$`Ending hour`, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC' ) 

confirmed_busroute_D <- confirmed_busroute_D %>% group_by(route_number) %>% mutate(direction_id = 1:n()-1) %>% filter(direction_id <= 1) #create direction_id attribute

confirmed_busroute_D <- as.data.frame(confirmed_busroute_D)

servicedaylist <- c("CN", "T2", "T3", "T4", "T5", "T6", "T7")

trips <- data.frame() #the first loop sets up the first services in each direction

for (i in 1:length(servicedaylist)){

  for (j in 1:length(confirmed_busroute_D[,1])) {
    
    timestep <- 0
    
    time_position <- strptime("1899-12-31 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC') #set up dummy time to get the while loop in motion
    
    X <- data.frame(day = servicedaylist[i])
    
    while (time_position <=  confirmed_busroute_D[j, "Ending hour"]){
      
      timestep <- timestep + 1
      
      if(timestep == 1){time_position <- confirmed_busroute_D[j, "Openning Hour"]} 
      
      if(timestep > 1){
        
      if(time_position <=  strptime("1899-12-31 05:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')){
        
        time_position <- time_position + confirmed_busroute_D[j, paste0("Non-peak hour headway - ", servicedaylist[i])]*60
        
      }
      
      else if((time_position >=  strptime("1899-12-31 05:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')) & (time_position <=  strptime("1899-12-31 07:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))){
        
        time_position <- time_position + confirmed_busroute_D[j, paste0("Peak hour headway - ", servicedaylist[i])]*60
        
      }
      
      else if((time_position >=  strptime("1899-12-31 07:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')) & (time_position <=  strptime("1899-12-31 15:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))){
        
        time_position <- time_position + confirmed_busroute_D[j, paste0("Non-peak hour headway - ", servicedaylist[i])]*60
        
      }
      
      
      else if((time_position >=  strptime("1899-12-31 15:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')) & (time_position <=  strptime("1899-12-31 17:59:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))){
        
        time_position <- time_position + confirmed_busroute_D[j, paste0("Peak hour headway - ", servicedaylist[i])]*60
        
      }
      
      
      else if((time_position >=  strptime("1899-12-31 18:00:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')) & (time_position <=  confirmed_busroute_D[j, "Ending hour"])){
        
        time_position <- time_position + confirmed_busroute_D[j, paste0("Non-peak hour headway - ", servicedaylist[i])]*60
      
      }
        
        #trips[timestep ,"departure_time"] <- time_position
        
          
      }
      

      X$route_id <- confirmed_busroute_D[j, "route_id"]
      X$service_id <- paste0(servicedaylist[i],"_0") #"_0" as there is no clear service variation apart from between weekdays
      X$shape_id <- confirmed_busroute_D[j, "shape_id"]
      X$trip_id <- paste0(24,'-', confirmed_busroute_D[j,"route_number"], '-vn-', timestep, '-', confirmed_busroute_D[j, "direction_id"])
      X$departure_time <- time_position
      X$route_number <- confirmed_busroute_D[j, "route_number"]
      X$trip_headsign <- confirmed_busroute_D[j, "to"]
      X$direction_id <- confirmed_busroute_D[j, "direction_id"]
      trips <- rbind.fill(X, trips)
      #print(X)  
    }
    
  }
  print(servicedaylist[i])
}


trips_ordered <- trips %>%
  arrange(route_number, day, departure_time) %>%
  select(-day)

write.table(trips_ordered, "trips.txt", fileEncoding = "UTF-8", sep = ",", row.names = F)

calendar <- trips_ordered %>% 
  select(service_id, day) %>% 
  distinct() %>%
  mutate(value = 1,
         start_date = "20190701",
         end_date = "20200701") %>% #ideally this value would reflect the active period for the proposed timetable but this information is not available 
  pivot_wider(names_from = day, values_from = value, values_fill = 0)

write.table(calendar, "calendar.txt", fileEncoding = "UTF-8", sep = ",", row.names = F)



  



