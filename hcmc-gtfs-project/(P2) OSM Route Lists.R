library(stringr)
library(plyr)
library(dplyr)
library(tidyr)
library(foreign)
library(geojsonR)

HCMC_BUS_STOPS_ROUTES <-  read.csv("D:/Infrastructure Victoria/Ho Chi Minh City Bus/Stops_and_routes.csv")

HCMC_BUS_ROUTES <-  read.dbf("D:/Infrastructure Victoria/Ho Chi Minh City Bus/HCMC_BUS_STOPS_AND_ROUTES_VN2000_JOINED.dbf")


#summarise the single chainage point for each route at each stop (some routes will not stop at that bus stops but this will be resolved when
#the output dataframe is merged with HCMC_BUS_STOPS, a process that will only join routes which use that bus stop) 

HCMC_BUS_ROUTES_V1 <- HCMC_BUS_ROUTES  %>%
  rename(stop_node_id = id,
         route_code = id_2,
         route_number = ref) %>%
  mutate(route_code = as.numeric(gsub('relation/',"", route_code))) %>%
  group_by(stop_node_id, route_code, route_number) %>% 
  summarise(shape_dist_mean = mean(distance)) 

HCMC_BUS_STOPS_ROUTES_V1 <- HCMC_BUS_STOPS_ROUTES %>% select(-X)

HCMC_BUS_STOPS_ROUTES_V2 <- merge(HCMC_BUS_STOPS_ROUTES_V1, HCMC_BUS_ROUTES_V1, by = c("stop_node_id", "route_code", "route_number")) %>%
  distinct() %>%
  arrange(route_code, shape_dist_mean) %>%
  group_by(route_code) %>%
  mutate(shape_pt_sequence = 1:n(),
         shape_dist_traveled = shape_dist_mean - min(shape_dist_mean))

write.table(HCMC_BUS_STOPS_ROUTES_V2, "HCMC_BUS_STOPS_ROUTES_SHAPES.txt", fileEncoding = "UTF-8", sep = ",")

HCMC_BUS_STOPS_ROUTES_LIST <- HCMC_BUS_STOPS_ROUTES_V2 %>% 
  ungroup() %>% 
  select(route_number, from, to, route_name, headway, network) %>% 
  distinct() %>%
  filter(grepl('Hồ', network)) %>%  #filter for Ho Chi Minh City routes
  group_by(route_number) %>%
  mutate(route_long_name = paste0(route_name,  collapse = " - ")) %>%
  select(-route_name) %>% distinct()

write.table(HCMC_BUS_STOPS_ROUTES_LIST, "BUS_ROUTES_LIST.txt", fileEncoding = "UTF-8", sep = ",") 


routes <- HCMC_BUS_STOPS_ROUTES_V2 %>%
  filter(grepl(	'Xe buýt Thành phố Hồ Chí', network)) %>%
  select(-stop_node_id, -shape_dist_mean, -shape_dist_traveled, -shape_pt_sequence, -lat, -long) %>%
  distinct() %>%
  group_by(route_number) %>%
  mutate(route_long_name = paste0(route_name,  collapse = " - ")) %>%
  reframe(route_long_name,
          route_id =  paste0('HCMC-', route_number,'-vn-','OSM-', paste0(route_code,  collapse = "-")),
          agency_id = network,
          route_short_name = route_number,
          route_type = 2, #Assume Saigon Metro is '1' as it is the highest capacity transport mode
          route_color = "#FF8200",
          route_text_color = "#FFFFFF")  %>%
  distinct() %>%
  select(route_id, agency_id, route_long_name, route_short_name, route_type,  route_color, route_text_color)


write.table(routes, "routes.txt", fileEncoding = "UTF-8", sep = "," , row.names = F) 








