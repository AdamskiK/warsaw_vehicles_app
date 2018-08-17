library(readtext)
library(stringi)
library(dplyr)
setwd("C:/Users/Adam/Desktop/Shiny/warsaw-public-transport")

# custom function for getting last n characters from a sting
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

substrLeft <- function(x, n){
  substr(x, 1, n)
}

# example data for tests
ex_schedule <- readtext("3229_example_from_bus_schedule_ztm.txt")
ex_schedule$text

# get all bus stop ids and names
all_busstopids <- read.csv("all_busstop_ids.csv")
all_busstopids$stopId

# get the raw text data
bus_schedule_info <- readtext("RA180815.TXT")
# bus_schedule_info

# gsub("yyy.yyyyyyyy", "00.000000", bus_schedule_info)

# create an empty df


# set parameter to see the remaining time
update_every_iter <- 50

time_start <- Sys.time()

#iterate through every busstop id
extracted_bus_stops <- data.frame()
for(i in 1:length(all_busstopids$stopId)){
  
  # use regexp to extract all
  get_bus_id_nr <- stri_extract_all(bus_schedule_info$text, regex = paste0("\\s", all_busstopids$stopId[i],"\\d{2}"))
  get_lat <- stri_extract_all(bus_schedule_info$text, regex = paste0("\\s", all_busstopids$stopId[i],".*Ul.*(Y=\\s\\d{2}\\.\\d{6}?)"))[[1]]
  get_lon <- stri_extract_all(bus_schedule_info$text, regex = paste0("\\s", all_busstopids$stopId[i],".*Ul.*(X=\\s\\d{2}\\.\\d{6}?)"))[[1]]
  busstop_name <- stri_extract_all(bus_schedule_info$text, regex = paste0(all_busstopids$stopId[i],"\\d{2}.*?:\\s(.+?),"))[[1]]

  
  processed_lon <- substrRight(get_lon, 9)
  processed_lat <- substrRight(get_lat, 9)
  processed_busstop_name <- unlist(stri_extract_all(busstop_name, regex = "(?<=:\\s).+[^,]"))
  
  
  df <- data.frame(id_nr = unlist(get_bus_id_nr), 
                   lon = processed_lon, 
                   lat = processed_lat, 
                   busstop_name = processed_busstop_name)
  
  
  extracted_bus_stops <- rbind(extracted_bus_stops, df)
  
  if(i %% update_every_iter == 0){
    all_iter <- length(all_busstopids$stopId)
    remain_iter <- all_iter - i
    diff <- difftime(Sys.time(), time_start, units = "secs")
    time_per_iter <- as.numeric(diff)/i
    
    time_left <- remain_iter*time_per_iter
    cat("time left: ", time_left, " seconds", "\n")
  }
}


extracted_bus_stops$id_nr <- as.character(extracted_bus_stops$id_nr)
extracted_bus_stops$lat <- as.character(extracted_bus_stops$lat)
extracted_bus_stops$lon <- as.character(extracted_bus_stops$lon)

extracted_bus_stops %>%
  filter(lat != "00.000000" | lon != "00.000000") ->
  extracted_bus_stops

sys_time <- gsub(":", "_", gsub(" ", "_", gsub("-", "_", Sys.time())))


write.csv(extracted_bus_stops, paste0(sys_time, "_extracted_bus_stops.csv"), row.names = F)


# testing

et_bus_id_nr <- stri_extract_all(ex_schedule$text, regex = paste0("\\s", 3229,"\\d\\d"))
get_lat <- stri_extract_all(ex_schedule$text, regex = paste0("\\s", 3229,".*Ul.Pl.:\\s(.?)\\,"))[[1]]
get_lon <- stri_extract_all(ex_schedule$text, regex = paste0("\\s", 3229,"\\d{2}.*Ul.*(X=\\s\\d{2}\\.\\d{6}?)"))[[1]]
processed_lon <- substrRight(get_lon, 9)
processed_lat <- substrRight(get_lat, 9)

                                             