# setwd("C:/Users/Adam/Desktop/Shiny/warsaw-public-transport")

# set a local language as polish
Sys.setlocale("LC_CTYPE", "polish")

library("httr")
library("jsonlite")
library("shiny")
library("leaflet")
library("dplyr")
library("shinydashboard")


# read bus stop nr and coordinates
bus_stop_df <- read.csv("2018_08_17_22_09_53_extracted_bus_stops.csv", stringsAsFactors = F)
bus_stop_df <- bus_stop_df[sample(nrow(bus_stop_df), 100), ]


# read mapped bus lines to bus ids
bus_line_mapping <- read.csv("mapped_bus_lines.csv", stringsAsFactors = F)
bus_line_mapping

for(i in 1:length(bus_line_mapping$busline)){
  bus_line_mapping$busline[i] <- list(gsub(" ", "", as.list(strsplit(bus_line_mapping$busline[[i]], ","))[[1]]))
}


aligned_bus_stops <- function(line_vector){
  
  if(!is.null(line_vector) == T){
    
    df <- data.frame()
    for(i in 1:length(line_vector)){
      
      for(j in 1:length(bus_line_mapping$busline)){
        
        if(line_vector[i] %in% bus_line_mapping$busline[[j]]){
          df <- rbind(df, data.frame(id_nr = bus_line_mapping$id_nr[j],
                                     lat = bus_line_mapping$lat[j],
                                     lon = bus_line_mapping$lon[j],
                                     busstop_name = bus_line_mapping$busstop_name[j]))
        }else{
          
        }
        
      }
    }
    
  }else{
    df <- bus_stop_df
  }
  
  return(df)
}
  

# API KEY
apikey <- "2b5e76a6-5515-4eb8-b173-130a648f210a"

###1### bus stop value
id_bus_stop_value <- "b27f4c17-5c50-4a5b-89dd-236b282bc499"


# debug
# https://api.um.warszawa.pl/api/action/dbtimetable_get/?
# id=b27f4c17-5c50-4a5b-89dd-236b282bc499&
# name=Madali%C5%84skiego&
# apikey=2b5e76a6-5515-4eb8-b173-130a648f210a


###2### available lines at the given bus stop
id_bus_lines <- "88cd555f-6f31-43ca-9de4-66c479ad5942"


# debug
# "https://api.um.warszawa.pl/api/action/dbtimetable_get?
# id=88cd555f-6f31-43ca-9de4-66c479ad5942&
# busstopId=3229&
# busstopNr=01&
# apikey=2b5e76a6-5515-4eb8-b173-130a648f210a"


###3### bus time table
bus_info_base <- "https://api.um.warszawa.pl/api/action/dbtimetable_get"
id_bus_time_table <- "e923fa0e-d96c-43f9-ae6e-60518c9f3238"


# debug
# https://api.um.warszawa.pl/api/action/dbtimetable_get/?
# id=e923fa0e-d96c-43f9-ae6e-60518c9f3238&busstopId=7009&
# busstopNr=01&
# line=523&
# apikey=2b5e76a6-5515-4eb8-b173-130a648f210a


# API call function
get_API_response <- function(call){
  
  
  get_response <- GET(call)
  get_text <- content(get_response, "text")
  get_json <- fromJSON(get_text, flatten = TRUE)
  data <- get_json$result
  
}


handle_empty_response <- function(data, call) {
  
  while(class(data) == "list"){
    
    Sys.sleep(1)
    data <- get_API_response(call)
    cat("-empty-")
    
  }
  return(data)
}


# convert polish letters into coded values
convert_polish_letters <- function(name) {

  old_letters <- c("A","C","E","L","N","?","S","Z","Z","a","c","e","l","n","?","s","z","z")
  new_letters <- c("%C4%84","%C4%86","%C4%98","%C5%81","%C5%83","%C3%93","%C5%9A","%C5%B9",
                   "%C5%BB","%C4%85","%C4%87","%C4%99","%C5%82","%C5%84","%C3%B3","%C5%9B",
                   "%C5%BA","%C5%BC")
  
  for(i in 1:length(old_letters)) {
    
    name <- gsub(old_letters[i], new_letters[i], name)
    
  }
  
  return(name)
}


get_bus_stop_id <- function(name){
  
  call3 <- paste0(bus_info_base,
                  "/?id=", id_bus_stop_value,
                  "&name=", convert_polish_letters(name),
                  "&apikey=", apikey)
  
  bus_stop_id <- get_API_response(call3)
  bus_stop_id <- handle_empty_response(bus_stop_id, call3)
  
  return(bus_stop_id)
}
  

get_bus_stop_lines <- function(busstopId, busstopNr){
  
  call4 <- paste0(bus_info_base,
                  "/?id=", id_bus_lines,
                  "&busstopId=", busstopId,
                  "&busstopNr=", busstopNr,
                  "&apikey=", apikey)
  
  bus_stop_lines <- get_API_response(call4)
  bus_stop_lines <- handle_empty_response(bus_stop_lines, call4)
  
  return(bus_stop_lines)
}

# get all the vehicle numbers for a given bus stop
# sapply(get_bus_stop_lines(get_bus_stop_id("Madalinskiego")[[1]][[1]]$value[1], "01")[1]$values, function(x) x$value)


get_bus_timetable <- function(busstopId, busstopNr,line){
  
  call5 <- paste0(bus_info_base,
                  "/?id=", id_bus_time_table,
                  "&busstopId=", busstopId,
                  "&busstopNr=", busstopNr,
                  "&line=", line,
                  "&apikey=", apikey)
  

  bus_timetable <- get_API_response(call5)
  bus_timetable <- handle_empty_response(bus_timetable, call4)

  
  return(list("bus_timetable" = bus_timetable))
}


return_bus_stop_info <- function(layer_id){

  # print(c("layer_id is null: ", is.null(layer_id)))
  # debug
  # layer_id <- "322901"
  print("#0#")
  print(c("layer id is: ", layer_id))
  
  if(!is.null(layer_id)){
    # print("enter return_bus_stop_info")
    print(c("layer id is: ", layer_id))
    busStopId <-  substr(layer_id, 1, 4)
    busStopNr <- substr(layer_id, 5, 6)
    
    print(c(busStopId, busStopNr))
    
    print("#1#")
    
    get_list <- get_bus_stop_lines(busStopId, busStopNr)
    
    if(get_list == "BadSqlGrammarException"){

      print("#1a#")
      base <- "API Error"

    }else{
      
    # vehicle_list <- sapply(get_bus_stop_lines(get_bus_stop_id("Madalinskiego")[[1]][[1]]$value[1], "01")[1]$values, function(x) x$value)
    vehicle_list <- sapply(get_list$values, function(x) x$value)
    print("#2#")
    for(i in 1:length(vehicle_list)){
      print("#3#")
      if(i == 1){
        get_bt <- get_bus_timetable(busStopId, busStopNr, vehicle_list[i])[[1]]
        df <- get_bt %>% mutate(bus = vehicle_list[i])
        print("#4#")
      }else{
        print("#5#")
        # double brackets can be helpful because of functions priority
        df <- df %>% 
          bind_rows((get_bus_timetable(busStopId, busStopNr, vehicle_list[i])[[1]] %>% mutate(bus = vehicle_list[i])))
          
        
      }
    }
    print("#6#")
    df_time <- sapply(df$values, function(x) x$value)[6,]
    df_bus <- df$bus
    print("#6#")
    # let's work with df
    df_time <- data.frame(do.call(c, lapply(df_time, function(x) as.POSIXct(x, format = "%H:%M:%S"))))
    df_bus <- df$bus
    df_final <- cbind(df_bus, df_time)
    names(df_final) <- c("busNr", "time")
    df_final_output <- df_final %>%
      filter(time > Sys.time()) %>%
      group_by(busNr) %>%
      mutate(rank = dense_rank(c(time))) %>%
      ungroup() %>%
      filter(rank <= 2) %>%
      arrange(time) %>%
      mutate(time_left = round(time - Sys.time())) %>%
      select(busNr, time_left)
    
    
    print("#7#")
    # a new way of getting proper print output
    df_final_output <- data.frame(df_final_output)
    print("#9#")
    aa <- apply(df_final_output[ , c("busNr", "time_left") ] , 1 , paste , collapse = "-" )
    print("#10#")
    gdb <- gsub("-", " - ", gsub(" ", "", aa))
    
    print("#11#")
    base <- paste0(gdb[1],"</strong>", "<br/>")
    for(i in 2:length(gdb)){
      base <- paste0(base, "<strong>", gdb[i],"</strong>", "<br/>")
    }
    print("#12#")
    base <- paste0("<strong>", base)
    print("#13#")
    base <- base %>% lapply(htmltools::HTML)
    base <- base[[1]]
    print("#14#")
    
    }
  }else{
    
    # if the layer_id is empty then return an empty sting 
    base <- ""
    
  }
  
  
  print("#15#")
  return(base)
}


getData <- function(){
  
  # trams API
  base_tram <- "https://api.um.warszawa.pl/api/action/wsstore_get/"
  id_tram <- "c7238cfe-8b1f-4c38-bb4a-de386db7e776"
  
  
  # buses API
  base_bus <- "https://api.um.warszawa.pl/api/action/busestrams_get/"
  id_bus <- "f2e5503e-927d-4ad3-9500-4ab9e55deb59"
  
  # API CALLS
  call1 <- paste0(base_tram,"?","id=",id_tram,"&","apikey=",apikey)
  
  call2 <- paste0(base_bus,"?","resource_id=",id_bus,"&","apikey=",apikey,"&type=1")
  
  
  print("### 1 ###")
  
  trams_data <- get_API_response(call1)
  buses_data <- get_API_response(call2)
  
  
  print("### 2 ###")
  
  trams_data <- handle_empty_response(trams_data, call1)
  buses_data <- handle_empty_response(buses_data, call2)
  
  
  print("### 4 ###")
  # the function filters out outliers
  filter_outliers <- function(data) {
    
    if(class(data) == "data.frame"){
      
      data <- data %>%
        filter(Lat > 52.140083,
               Lat < 52.346209,
               Lon > 20.866590,
               Lon < 21.143558)
    }
    else{
      # print(c("#2# class of data is: ", class(data)))
      # print(c("#2# print data: ", data))
    }

    return(data)
  }
  
  print("### 5 ###")
  
  trams_data <- filter_outliers(trams_data)
  buses_data <- filter_outliers(buses_data)
  
  print("### 6 ###")
  
  trams_data$FirstLine <- as.character(as.numeric(trams_data$FirstLine))
  
  print("### 7 ###")
  
  # setting a backup for the dropdown list
  backup_tram_data <- trams_data
  backup_bus_data <- buses_data
  
  print("### 8 ###")
  
  # split converts the f (second) argument to factors, if it isn't already one.
  # So, if you want the order to be retained, factor the column yourself
  # with the desired levels.
  get_labels <- function(data_, numeric_type){
    
    if(numeric_type){
      # uniq_first_lines <- c("all", unique(as.character(sort(as.numeric(data_)))))
      print("### 9a ###")
      uniq_first_lines <- c(unique(as.character(sort(as.numeric(data_)))))
    }else{
      # uniq_firstb_lines <- c("all", unique(as.character(sort(data_))))
      print("### 9 ###")
      uniq_first_lines <- c(unique(as.character(sort(data_))))
    }
    
    print("### 9c ###")
    sorted_factor <- factor(uniq_first_lines, levels=uniq_first_lines)
    label_list <- split(uniq_first_lines, sorted_factor)
    
    return(label_list)
  }
  
  print("### 9 ###")

  tram_label_list <- get_labels(backup_tram_data[["FirstLine"]], TRUE)
  bus_label_list <- get_labels(backup_bus_data[["Lines"]], FALSE)
  
  print("### 10 ###")
  
  rownames(trams_data) <- NULL
  rownames(buses_data) <- NULL
  
  print("### 11 ###")
  
  return(list("trams_data" = trams_data,
              "buses_data" = buses_data,
              "tram_label_list" = tram_label_list,
              "bus_label_list" = bus_label_list,
              "backup_tram_data" = backup_tram_data))
}


ui <- dashboardPage(
  dashboardHeader(title  = "Warsaw Public Transport", titleWidth = "270px"),
  dashboardSidebar(width = "270px", 
    
    div(class="outer", h3("Controls")),
    textOutput("tram_location_labels"),
    uiOutput("tram_lines"),
    uiOutput("test"),
    h6(textOutput("cor_bind")),
    h6(textOutput("last_ref")),
    
    
    tags$style(type='text/css', 
               ".selectize-input { font-size: 20px; line-height: 20px; text-align: center; 
             text-indent: 0px; } 
             .selectize-dropdown { font-size: 15px; line-height: 15px; text-align: center; 
             text-indent: 0px; }
             .panel-primary { margin: 40px; font-size: 15px; 
             text-indent: 20px; }
             .custom_text { font-size: 10 px; }" 
    ),
    
    tags$script('
              $(document).ready(function () {
              navigator.geolocation.getCurrentPosition(onSuccess, onError);

              function onError (err) {
              Shiny.onInputChange("geolocation", false);
              }

              function onSuccess (position) {
              setTimeout(function () {
              var coords = position.coords;
              console.log(coords.latitude + ", " + coords.longitude);
              Shiny.onInputChange("geolocation", true);
              Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude);
              }, 1100)
              }
              });
              ')
    
  ),
  dashboardBody(

  leafletOutput("mymap", width = "auto", height = "1080px")
  
  
 
  )
  )


server <- shinyServer(function(input, output, session) {
  
  
  # renderUI - showing the drop-down list
  output$tram_lines <- renderUI({
    
    selectInput("tram_location_labels", 
                label = h4("Filter tram line"), 
                choices = tramLabels(),
                selected = "",
                multiple = T)
    
  })
  
  
  output$test <- renderUI({
    
    selectInput("bus_location_labels", 
                label = h4("Filter bus line"), 
                choices = busLabels(),
                selected = "",
                multiple = T)
    
  })
  
  
  # time set to refresh every x msec - has to be passed as an argument
  autoInvalidate <- reactiveTimer(10000)
  
  
  reData <- eventReactive(autoInvalidate(), {
    
    data <- getData()
    backup_tram_data <- data$backup_tram_data
    
    if(is.null(input$tram_location_labels) == T & is.null(input$bus_location_labels) == T){
      
      print("enter 1")
      trams_data <- data$trams_data %>% dplyr::filter(FirstLine %in% c("33"))
      buses_data <- data$buses_data %>% dplyr::filter(Lines %in% c("174"))
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list
      # print("exit 1")
      
    }else if(is.null(input$tram_location_labels) == T & is.null(input$bus_location_labels) == F){
      
      # print("enter 2")
      trams_data <- data$trams_data %>% dplyr::filter(FirstLine %in% c("9"))
      buses_data <- data$buses_data %>% dplyr::filter(Lines %in% print(input$bus_location_labels))
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list
      
    }else if(is.null(input$tram_location_labels) == F & is.null(input$bus_location_labels) == T){
      
      # print("enter 3")
      trams_data <- data$trams_data %>% dplyr::filter(FirstLine %in% input$tram_location_labels)
      buses_data <- data$buses_data %>% dplyr::filter(Lines %in% c("174"))
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list
      
    }else{
      
      # print("enter 4")
      trams_data <- data$trams_data %>% dplyr::filter(FirstLine %in% input$tram_location_labels)
      buses_data <- data$buses_data %>% dplyr::filter(Lines %in% input$bus_location_labels)
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list
    
    }
    
    return(list("trams_data" = trams_data,
                "buses_data" = buses_data,
                "tram_label_list" = tram_label_list,
                "bus_label_list" = bus_label_list,
                "backup_tram_data" = backup_tram_data))
  })
  
  
  # get tram labels
  tramLabels <- eventReactive(reactiveTimer(300000), {
    
    reData()$tram_label_list
    
  })
  
  
  # get bus labels every 5 minuts
  busLabels <- eventReactive(reactiveTimer(300000), {
    
    reData()$bus_label_list
    
  })
  
  
  tram_points <- reactive({
    
    tram_points <- cbind(reData()[["trams_data"]]["Lon"], reData()[["trams_data"]]["Lat"])
    
    return(tram_points)
  })
  
  
  tram_labels <- reactive({
    
    tram_labels <- paste("line: ", reData()[["trams_data"]][[2]])
    
    return(tram_labels)
  })
  
  
  bus_points <- reactive({

    bus_points <- cbind(reData()[["buses_data"]]["Lon"], reData()[["buses_data"]]["Lat"])

    return(bus_points)
  })

  
  bus_labels <- reactive({

    bus_labels <- paste("line: ", reData()$buses_data[[4]])
    
    return(bus_labels)
  })
  
  
  
  
  output$cor_bind <- renderText({

    cor_bind <- c("Your lattitude and longitude: ", input$lat, ",", input$long, sep="")

  })
  
  
  output$last_ref <- renderText({
    
    time_sample <- reData()$backup_tram_data$Time[1]
    
    if(is.null(time_sample) == T){
      # do nothing
      print("time sample is null")
    }
    else{
      time1 <- as.POSIXct(time_sample, format = '%Y-%m-%dT%H:%M:%S')
      time2  <- as.POSIXct(Sys.time())
      if(is.null(time1) == T){print(c("time 1 is null, needs some debugging"))}
      
      timeDiff <- round(difftime(time2,time1, units="sec"),0)
      
      last_ref <- paste("Last update: ",
                        strptime(time_sample, format='%Y-%m-%dT%H:%M:%S'),
                        " rerfeshed: ", timeDiff, " sec ago")
      
      return(last_ref)
    }
  })
  
  
  
  iconize <- function(icon_file_name, x_size, y_size){
    icon <- iconList(
      ship = makeIcon(icon_file_name, x_size, y_size)
    )
  }
  
  bus_stop_info <-  data.frame(busstopId = "3229", 
                         busstopNr = "01", 
                         Lat = 52.202810, 
                         Lon = 21.010190,
                         label = "",
                         markerId = "322901")
  
  icon.home <- makeAwesomeIcon(icon = 'home', library = "fa", markerColor = "green")
  icon.tram <- makeAwesomeIcon(icon = 'train', library = "fa", markerColor = "blue")
  icon.bus <- makeAwesomeIcon(icon = 'bus', library = "fa", markerColor = "red")
  icon.users <- makeAwesomeIcon(icon = 'users', library = "fa", markerColor = "beige")
  
  
  output$mymap <- renderLeaflet({
    url_map <- a("OpenStreetMap", href="https://www.openstreetmap.org/copyright")
    url_my_github <- a("Kamil Adamski", href="https://github.com/AdamskiK")
    url_contrib <- a("Miasto Stoleczne Warszawa", href="https://api.um.warszawa.pl/")
    leaflet <- leaflet() %>%
      addTiles() %>%
      addTiles(attribution =
                 paste("(c) 2018 ",url_map, ", ", url_my_github, ", ", url_contrib, sep="")) %>%
      fitBounds(input$long-0.005, input$lat-0.005, input$long+0.005, input$lat+0.005)
      
    return(leaflet)
  })
  
  # observeEvent(input$mymap_marker_click, { # update the location selectInput on map clicks
  #   print("hello")
  #   p <- input$mymap_marker_click
  #   print(return_bus_stop_info(input$mymap_marker_click$id))
  # })
  
  trigger_bus_info <- eventReactive(input$mymap_marker_click, {
    output <- return_bus_stop_info(input$mymap_marker_click$id)
    return(output)
  })

  # bus_labels <- reactive({
  #   
  #   bus_labels <- return_bus_stop_info(input$mymap_marker_click$id)
  #   
  #   return(bus_labels)
  # })
  
  
  observeEvent(autoInvalidate(), {
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      
      addAwesomeMarkers(
        lng = tram_points()$Lon,
        lat = tram_points()$Lat,
        icon = icon.tram,
        label = tram_labels()
      ) %>%
  
      addAwesomeMarkers(
        lng = bus_points()$Lon,
        lat = bus_points()$Lat,
        icon = icon.bus,
        label = bus_labels()
      ) %>%
      
      addAwesomeMarkers(
        lng = ifelse(is.null(input$long) == T, 0, input$long),
        lat = ifelse(is.null(input$lat) == T, 0, input$lat),
        icon = icon.home,
        label = "Your position"
      ) %>%
      
      addAwesomeMarkers(
        # lng = ifelse(is.null(input$bus_location_labels), 
        #              bus_stop_df$lon, 
        #              aligned_bus_stops(input$bus_location_labels)$lon), # bus_stop_info$Lon,
        # 
        # lat = ifelse(is.null(input$bus_location_labels),
        #              bus_stop_df$lat,
        #              aligned_bus_stops(input$bus_location_labels)$lat), # bus_stop_info$Lat,
        # 
        # icon = icon.users,
        # 
        # layerId = ifelse(is.null(input$bus_location_labels),
        #                  bus_stop_df$id_nr,
        #                  aligned_bus_stops(input$bus_location_labels)$id_nr), # bus_stop_info$markerId,
        
        lng = aligned_bus_stops(c(input$bus_location_labels, input$tram_location_labels))$lon,
        
        lat = aligned_bus_stops(c(input$bus_location_labels, input$tram_location_labels))$lat,
        
        icon = icon.users,
        
        layerId = aligned_bus_stops(c(input$bus_location_labels, input$tram_location_labels))$id_nr,
        
        # popup = return_bus_stop_info(input$mymap_marker_click$id),
        
        label = aligned_bus_stops(c(input$bus_location_labels, input$tram_location_labels))$busstop_name

      )
    
  },ignoreNULL = FALSE)
})


shinyApp(ui, server)
