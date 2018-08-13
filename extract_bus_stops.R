library(readtext)
library(stringi)
setwd("C:/Users/Adam/Desktop/Shiny/warsaw-public-transport")


# example data for tests
ex_schedule <- readtext("3229_example_schedule.txt")
ex_schedule$text

# get all bus stop ids and names
all_busstopids <- read.csv("all_busstop_ids.csv")
all_busstopids$stopId

# get the raw text data
bus_schedule_info <- readtext("RA180815.TXT")
bus_schedule_info

# create an empty df
extracted_bus_stops <- data.frame()

#iterate through every busstop id
for(i in 1:length(all_busstopids$stopId)){
  
  print(i)
  # use regexp to extract all
  get_bus_id_nr <- stri_extract_all(bus_schedule_info$text, regex = paste0(all_busstopids$stopId[i],"\\d\\d"))
  get_lon <- stri_extract_all(bus_schedule_info$text, regex = "Y=\\s\\d{2}\\.\\d{6}")
  get_lat <- stri_extract_all(bus_schedule_info$text, regex = "X=\\s\\d{2}\\.\\d{6}")
  
  print(c(get_bus_id_nr, get_lon, get_lat))
  df <- data.frame(id_nr = unlist(get_bus_id_nr), lon = substr(unlist(get_lon), 4,12), lat = substr(unlist(get_lat), 4,12))
  extracted_bus_stops <- rbind(extracted_bus_stops, df)
  break
}



### testing

get_bus_id_nr <- stri_extract_all(ex_schedule$text, regex = paste0(3229,"\\d\\d"))
get_lon <- stri_extract_all(ex_schedule$text, regex = "Y=\\s\\d{2}\\.\\d{6}")
get_lat <- stri_extract_all(ex_schedule$text, regex = "X=\\s\\d{2}\\.\\d{6}")
stri_locate_all(ex_schedule$text, regex = "Y=\\s\\d{2}\\.\\d{6}")

stri_locate_all(ex_schedule$text, regex = paste0(3229,"\\d\\d"))

stri_locate_all_boundaries(ex_schedule$text)


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


str_e <- stri_extract_all(ex_schedule$text, regex = paste0(3229,".*(Y=\\s\\d{2}\\.\\d{6}?)"))[[1]]
substrRight(str_e, 9)

