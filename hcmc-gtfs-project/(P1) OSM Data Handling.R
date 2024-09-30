library(stringr)
library(plyr)
library(dplyr)
library(tidyr)
library(foreign)
library(geojsonR)


geojson <- FROM_GeoJson("D:/Infrastructure Victoria/Ho Chi Minh City Bus/Ho Chi Minh City OSM Data.geojson")


internal_table <- data.frame()
Stops_and_Routes <- data.frame()
#we need to create a separate but identical point for all bus routes using the same stop 
#create a loop that extracts this information from the geojson file
for (i in 1:length(geojson$features)) {
  if(geojson$features[[i]]$geometry$type == "Point"){
    
    for (j in 1:length(geojson$features[[i]]$properties$`@relations`)){ #the number of iterations for inner loop
      
      internal_table[j, "stop_node_id"] <- geojson$features[[i]]$properties$`@id`  #the node id 
      internal_table[j, "route_code"] <- geojson$features[[i]]$properties$`@relations`[[j]]$rel #the route code identified in the inner loop 
      
      if(!is.null(geojson$features[[i]]$properties$`@relations`[[j]]$reltags$ref)){
      internal_table[j, "route_number"] <- geojson$features[[i]]$properties$`@relations`[[j]]$reltags$ref} else {internal_table[j, "route_number"] <- ""}
      
      internal_table[j, "route_name"] <- geojson$features[[i]]$properties$`@relations`[[j]]$reltags$name
      
      if(!is.null(geojson$features[[i]]$properties$`@relations`[[j]]$reltags$from)){
      internal_table[j, "from"] <- geojson$features[[i]]$properties$`@relations`[[j]]$reltags$from} else {internal_table[j, "from"] <- "" }
      
      if(!is.null(geojson$features[[i]]$properties$`@relations`[[j]]$reltags$to)){
      internal_table[j, "to"] <- geojson$features[[i]]$properties$`@relations`[[j]]$reltags$to} else {internal_table[j, "to"] <- "" } 
      
      if(!is.null(geojson$features[[i]]$properties$`@relations`[[j]]$reltags$interval)){
      internal_table[j, "headway"] <- geojson$features[[i]]$properties$`@relations`[[j]]$reltags$interval} else {internal_table[j, "headway"] <- ""}
      
      if(!is.null(geojson$features[[i]]$properties$`@relations`[[j]]$reltags$operator)){
      internal_table[j, "operator"] <- geojson$features[[i]]$properties$`@relations`[[j]]$reltags$operator} else {internal_table[j, "operator"] <- ""}
      
      if(!is.null(geojson$features[[i]]$properties$`@relations`[[j]]$reltags$network)){
        internal_table[j, "network"] <- geojson$features[[i]]$properties$`@relations`[[j]]$reltags$network} else {internal_table[j, "network"] <- ""}
      
      internal_table[j, "long"] <- geojson$features[[i]]$geometry$coordinates[1] #long
      internal_table[j, "lat"] <- geojson$features[[i]]$geometry$coordinates[2] #lat
      
      print(j)
      
    }
    
    Stops_and_Routes <- rbind.fill(Stops_and_Routes, internal_table)
    
    print(i)
    
  }

}

write.csv(Stops_and_Routes, "Stops_and_routes.csv")







#The remaining columns are filled with the below  


# 
# #this loop will produce a 2-column table with node/XXXXXXXXX stop identifier and route id merged with the LAT and LONG values for each stop (obtained by copy and pasting QGIS data into here)
# #this is then exported back into qgis, a 40m radius buffer drawn and overlapped with Chainage points 
# 
# 
# Stop_Chainage_Join <- read.dbf("D:/Infrastructure Victoria/Ho Chi Minh City Bus/HCMC_BUS_STOPS_VN3405_CHAINAGE.dbf") #not useful until the complete list of bus services using a stop is determined
# 
# 
# Stop_Chainage_Join_Route <- Stop_Chainage_Join %>%
#   separate_wider_delim(X.relations, delim = ",", names = c("Trash1", "OSM_Route_ID"), too_many = "debug" ) %>%
#   select(id, OSM_Route_ID, id_2, distance) %>%
#   mutate(OSM_StopRoute_Serviced = str_remove(OSM_Route_ID, ' "rel": ')) %>%
#   mutate(OSM_RouteChainage = str_remove(id_2, 'relation/')) %>%
#   select(-OSM_Route_ID) %>%
#   filter(OSM_StopRoute_Serviced == OSM_RouteChainage) %>%
#   group_by(id,  id_2, OSM_StopRoute_Serviced) %>%
#   summarise(dist = median(distance))
# 
# 
# write.csv(Stop_Chainage_Join_Route, "test.csv")