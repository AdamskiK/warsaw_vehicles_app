# set a local language as polish
Sys.setlocale("LC_CTYPE", "polish")

library("httr")
library("jsonlite")
library("shiny")
library("leaflet")
library("dplyr")
library("shinydashboard")
library("readtext")
library("DT")


# read bus stop nr and coordinates
bus_stop_df_full <- read.csv("fully_extracted_bus_stops.csv", stringsAsFactors = F)
bus_stop_df <- bus_stop_df_full[sample(nrow(bus_stop_df_full), 50), ]


# read mapped bus lines to bus ids
bus_line_mapping <- read.csv("mapped_bus_lines.csv", stringsAsFactors = F)
bus_line_mapping

# add bus stop names to the mapped bus lines
bus_line_mapping <- merge(x = bus_line_mapping, 
                          y = bus_stop_df_full[, c("id_nr", "street_name", "busstop_name")], 
                          by = "id_nr",
                          all.x = TRUE)

for(i in 1:length(bus_line_mapping$busline)){
  bus_line_mapping$busline[i] <- list(gsub(" ", "", as.list(strsplit(bus_line_mapping$busline[[i]], ","))[[1]]))
}


aligned_bus_stops <- function(line_vector, busid_full){
  start0 <- Sys.time()
  
  if(!is.null(line_vector) == T){
    
    df <- data.frame()
    
    # a search through a vector of bus/tram lines
    for(i in 1:length(line_vector)){
      
      get_vector_with_indices <- which(lapply(lapply(bus_line_mapping$busline, function(x) grep(line_vector[i],x)), length) > 0)

      get_vec_bus_id_index <- which(lapply(lapply(bus_line_mapping$id_nr, function(x) grep(as.numeric(busid_full),x)), length) > 0)
      
      for(j in get_vector_with_indices){
        # start <- Sys.time()
        
        df <- rbind(df, data.frame(id_nr = bus_line_mapping$id_nr[j],
                                   lat = bus_line_mapping$lat[j],
                                   lon = bus_line_mapping$lon[j],
                                   street_name = bus_line_mapping$street_name[j],
                                   busstop_name = bus_line_mapping$busstop_name[j],

                                   bus_time_table = if(as.character(j) == get_vec_bus_id_index){
                                     
                                     return_bus_stop_info(as.character(bus_line_mapping$id_nr[j]))["base"][[1]]
                                     
                                   }else{
                                     
                                     ""
                                     # return_bus_stop_info(as.character(bus_line_mapping$id_nr[get_vector_with_indices[1]]))["base"][[1]]
                                     
                                   }
        ))
        
        # print(c("one loop takes: ", Sys.time() - start, " per one bus time table"))
        # print(c("till now it takes: ", Sys.time() - start0))
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

###2### available lines at the given bus stop
id_bus_lines <- "88cd555f-6f31-43ca-9de4-66c479ad5942"

###3### bus time table
bus_info_base <- "https://api.um.warszawa.pl/api/action/dbtimetable_get"
id_bus_time_table <- "e923fa0e-d96c-43f9-ae6e-60518c9f3238"

# API call function
get_API_response <- function(call){
  
  
  get_response <- GET(call)
  get_text <- content(get_response, "text")
  get_json <- fromJSON(get_text, flatten = TRUE)
  data <- get_json$result
  
}


handle_empty_response <- function(data, call) {
  

  while(class(data) == "list"){
    
    Sys.sleep(3)
    data <- get_API_response(call)
    
    cat("-empty")
    
  }
  
  return(data)
  }



# convert polish letters into coded values
convert_polish_letters <- function(name) {

  # old_letters <- c("A","C","E","L","N","?","S","Z","Z","a","c","e","l","n","?","s","z","z")
  old_letters <- read.csv("polish_letters.csv", stringsAsFactors = F)$letters
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

# get all vehicle info for a given bus stop
get_bus_timetable <- function(busstopId, busstopNr,line){
  
  call5 <- paste0(bus_info_base,
                  "/?id=", id_bus_time_table,
                  "&busstopId=", busstopId,
                  "&busstopNr=", busstopNr,
                  "&line=", line,
                  "&apikey=", apikey)

  bus_timetable <- get_API_response(call5)
  
  return(list("bus_timetable" = bus_timetable))
}


return_bus_stop_info <- function(layer_id){
  # start <- Sys.time()

  # debug
  # layer_id <- "322901"
  
  bolean <-  !is.na(is.numeric(layer_id)) & !is.null(layer_id) & length(layer_id) != 0

  if(bolean){
    busStopId <-  substr(layer_id, 1, 4)
    busStopNr <- substr(layer_id, 5, 6)
    
    print(c(busStopId, busStopNr))
    
    print("#1 - getting busstop lines #")
    get_list <- get_bus_stop_lines(busStopId, busStopNr)
    print(c("get list consists of :", get_list))
    
    if(length(get_list) == 0){

      print("#API Error#")
      base <- "API Error"

    }else{
      
    
    vehicle_list <- sapply(get_list$values, function(x) x$value)
    
    bt_df <- data.frame()
    for(i in 1:length(vehicle_list)){
      
      get_bus_tt <- get_bus_timetable(busStopId, busStopNr, vehicle_list[i])[[1]]
      if(class(get_bus_tt) == "list"){next}
      
      bt_df <- bt_df %>% 
        bind_rows(get_bus_tt %>% mutate(bus = vehicle_list[i]))
      
    }
    
    # check cpnfition if final bt_df is a list, then return empty string
    if(class(bt_df) == "list"){
      
      base <-  ""
      df_final_output <- ""
    
    }else{
      
      print("#6#")
      df_time <- sapply(bt_df$values, function(x) x$value)[6,]
      df_bus <- bt_df$bus
      
      # let's work with df
      df_time <- data.frame(do.call(c, lapply(df_time, function(x) as.POSIXct(x, format = "%H:%M:%S"))))
      df_bus <- bt_df$bus
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
      
      # a new way of getting proper print output
      df_final_output <- data.frame(df_final_output)
      aa <- apply(df_final_output[ , c("busNr", "time_left") ] , 1 , paste , collapse = "-" )
      gdb <- gsub("-", " - ", gsub(" ", "", aa))
      
      base <- paste0(gdb[1],"</strong>", "<br/>")
      for(i in 2:length(gdb)){
        base <- paste0(base, "<strong>", gdb[i],"</strong>", "<br/>")
      }

      base <- paste0("<strong>", base)
      base <- base %>% lapply(htmltools::HTML)
      base <- base[[1]]
    }
    }
    
  }else{
    
    # if the layer_id is empty then return an empty sting 
    base <- ""
    df_final_output <- ""
    
  }
  
  # testing purpose
  print("#15 - return timetable#")
  # print(c("one loop takes: ", (Sys.time() - start)/i, " per one bus time table"))

  return(list("base" = base,
              "df_final_output" = df_final_output))
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
  
  
  # print("### 1 - get api###")
  
  trams_data <- get_API_response(call1)
  buses_data <- get_API_response(call2)
  
  
  # print("### 2 - handling empty response ###")
  
  trams_data <- handle_empty_response(trams_data, call1)
  buses_data <- handle_empty_response(buses_data, call2)
  
  
  # print("### 4 ###")
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
      # debug
      # print(c("#2# print data: ", data))
    }

    return(data)
  }
  
  trams_data <- filter_outliers(trams_data)
  buses_data <- filter_outliers(buses_data)
  
  trams_data$FirstLine <- as.character(as.numeric(trams_data$FirstLine))
  
  # setting a backup for the dropdown list
  backup_tram_data <- trams_data
  backup_bus_data <- buses_data
  
  # split converts the f (second) argument to factors, if it isn't already one.
  # So, if you want the order to be retained, factor the column yourself
  # with the desired levels.
  get_labels <- function(data_, numeric_type){
    
    if(numeric_type){
      
      uniq_first_lines <- c(unique(as.character(sort(as.numeric(data_)))))
      
    }else{
      
      uniq_first_lines <- c(unique(as.character(sort(data_))))
    }
    
    sorted_factor <- factor(uniq_first_lines, levels=uniq_first_lines)
    label_list <- split(uniq_first_lines, sorted_factor)
    
    return(label_list)
  }

  tram_label_list <- get_labels(backup_tram_data$FirstLine , TRUE)
  bus_label_list <- get_labels(backup_bus_data$Lines , FALSE)
  
  rownames(trams_data) <- NULL
  rownames(buses_data) <- NULL
  
  return(list("trams_data" = trams_data,
              "buses_data" = buses_data,
              "tram_label_list" = tram_label_list,
              "bus_label_list" = bus_label_list,
              "backup_tram_data" = backup_tram_data,
              "backup_bus_data" = backup_bus_data))
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

  leafletOutput("mymap", width = "auto", height = "700px"),
  DT::dataTableOutput("tableDT")
  
  
 
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
  autoInvalidate <- reactiveTimer(15000)
  
  
  reData <- eventReactive(autoInvalidate(), {
    
    data <- getData()
    
    backup_tram_data <- data$backup_tram_data
    backup_bus_data <- data$backup_bus_data
    
    
    random_tram_vehicle <- sample(backup_tram_data$FirstLine, 1)
    random_bus_vehicle <- sample(backup_bus_data$Lines, 1)
    

    if(is.null(input$tram_location_labels) == T & is.null(input$bus_location_labels) == T){
      
      print("taking random tram and bus stop info")
      trams_data <- data$trams_data %>% dplyr::filter(FirstLine %in% random_tram_vehicle)
      buses_data <- data$buses_data %>% dplyr::filter(Lines %in% random_bus_vehicle)
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list
      
    }else if(is.null(input$tram_location_labels) == T & is.null(input$bus_location_labels) == F){
      
      trams_data <- data$trams_data %>% dplyr::filter(FirstLine %in% random_tram_vehicle)
      buses_data <- data$buses_data %>% dplyr::filter(Lines %in% input$bus_location_labels)
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list
      
    }else if(is.null(input$tram_location_labels) == F & is.null(input$bus_location_labels) == T){
      
      trams_data <- data$trams_data %>% dplyr::filter(FirstLine %in% input$tram_location_labels)
      buses_data <- data$buses_data %>% dplyr::filter(Lines %in% random_bus_vehicle)
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list
      
    }else{
      
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
    print(c("before printing tram points"))
    tram_points <- cbind(reData()[["trams_data"]]["Lon"], reData()[["trams_data"]]["Lat"])
    print(tram_points)
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
      
      fitBounds(input$long-0.030, input$lat-0.030, input$long+0.030, input$lat+0.030) #%>%
      
    return(leaflet)
  })
  
  # a function which creates layer_id's for a given dataset and a layer_name
  create_layer_id <- function(data, layer_name){
    
    data_length <- length(data)
    
    if(data_length > 0){
      empty_string <- paste0(layer_name, 1)
      
      for(i in 2:data_length){
        empty_string <- c(empty_string, paste0(layer_name,i))
      }
      
    }else{
      # do nothing
    }
    
    return(empty_string)
  }
  
  # create a backup_layer_id dataset with purpose of removing the existing layer_id's
  data <- rep(1, 500)
  backup_tram_layer_id <- create_layer_id(data, "tram")
  backup_bus_layer_id <- create_layer_id(data, "bus")
  
  
  # the purpose of managing tram and bus locations
  observeEvent(autoInvalidate(), {
    
    
    print(c("enter observeEvent#1a"))
    tram_data <- tram_points()
    print(c("tram_data are: ", tram_data))
    tram_layer_id <- create_layer_id(tram_data$Lat, "tram")
    print(c("after showing tram data for Lat"))
    
    
    print(c("enter observeEvent#1b"))
    bus_data <- bus_points()
    bus_layer_id <- create_layer_id(bus_data$Lat, "bus")
    
    
    leafletProxy("mymap") %>%
      removeMarker(backup_tram_layer_id) %>%
      removeMarker(backup_bus_layer_id) %>%
      #debug
      # clearMarkers() %>% # remove all markers with not defined layer_id's
      
      addAwesomeMarkers(
        lng = tram_data$Lon,
        lat = tram_data$Lat,
        icon = icon.tram,
        label = tram_labels(),
        layerId = tram_layer_id
      ) %>%
  
      addAwesomeMarkers(
        lng = bus_data$Lon,
        lat = bus_data$Lat,
        icon = icon.bus,
        label = bus_labels(),
        layerId = bus_layer_id
      )
      
  },ignoreNULL = FALSE)
  
  
  # get all bus stop IDs
  all_busstop_layer_ids <- bus_stop_df_full$id_nr
                

  reactive_timetable <- reactive({

    boolean_value <- is.null(input$bus_location_labels)
    
    null_clickid <- is.null(input$mymap_marker_click)
    choose_layer_id <- ifelse(null_clickid == T, "322901", input$mymap_marker_click$id)
    print(c("choosen layer id is: ", choose_layer_id))
    
    if(boolean_value){

      aligned_data <- aligned_bus_stops("174", "322901") %>%
        filter(bus_time_table != "")
      
      bus_timetable <- return_bus_stop_info("322901")["df_final_output"][[1]]

    }else{
  
      print(c("reactive_timetable - line_vector: ", c(input$bus_location_labels)))
      aligned_data <- aligned_bus_stops(c(input$bus_location_labels),
                                        choose_layer_id)
      
      bus_timetable <- return_bus_stop_info(as.character(input$mymap_marker_click$id))["df_final_output"][[1]]

    }

    return(list("aligned_data" = aligned_data,
                "bus_timetable" = bus_timetable))
  })

  output$tableDT <- DT::renderDataTable(DT::datatable(data.frame(reactive_timetable()$bus_timetable), 
                                                      filter = "top",
                                                      selection = 'none',
                                                      options = list(paging = F,
                                                                     rownames = F,
                                                                     bFilter = 0)))
  
  
  observeEvent(c(input$bus_location_labels, input$tram_location_labels), {
    
    print(c("enter observeEvent#2"))
    
    tram_data <- tram_points()
    tram_layer_id <- create_layer_id(tram_data$Lat, "tram")
    print(c("after tram_layer_id creation"))
    
    bus_data <- bus_points()

    bus_layer_id <- create_layer_id(bus_data$Lat, "bus")
    print(c("after bus_layer_id creation"))
    
    # remove necessary markers
    leafletProxy("mymap") %>%
      removeMarker("home") %>%
      removeMarker(as.character(all_busstop_layer_ids)) %>%
      removeMarker(backup_tram_layer_id) %>%
      removeMarker(backup_bus_layer_id) %>%
      
      
    # update your position
    addAwesomeMarkers(
      lng = ifelse(is.null(input$long) == T, 0, input$long),
      lat = ifelse(is.null(input$lat) == T, 0, input$lat),
      icon = icon.home,
      label = "Your position",
      layerId = "home"
    ) %>%

    
    # update tram or bus stop positions  
    addAwesomeMarkers(
      lng = reactive_timetable()$aligned_data$lon,
      lat = reactive_timetable()$aligned_data$lat,
      icon = icon.users,
      layerId = as.character(reactive_timetable()$aligned_data$id_nr),
      label = reactive_timetable()$aligned_data$busstop_names
    ) %>%
    
      
    # update tram positions
    addAwesomeMarkers(
      lng = tram_data$Lon,
      lat = tram_data$Lat,
      icon = icon.tram,
      label = tram_labels(),
      layerId = tram_layer_id
    ) %>%
    
      
    # update bus postions
    addAwesomeMarkers(
      lng = bus_data$Lon,
      lat = bus_data$Lat,
      icon = icon.bus,
      label = bus_labels(),
      layerId = bus_layer_id
    )
    
  },ignoreNULL = FALSE)
  
})


shinyApp(ui, server)
