# setwd("C:/Users/Adam/Desktop/Shiny/warsaw-public-transport")

# set a local language as polish
Sys.setlocale("LC_CTYPE", "polish")

library("httr")
library("jsonlite")
library("shiny")
library("leaflet")
library("dplyr")
library("shinydashboard")


convert_polish_letters <- function(name) {

  old_letters <- c("A","C","E","L","N","Ó","S","Z","Z","a","c","e","l","n","ó","s","z","z")
  new_letters <- c("%C4%84","%C4%86","%C4%98","%C5%81","%C5%83","%C3%93","%C5%9A","%C5%B9",
                   "%C5%BB","%C4%85","%C4%87","%C4%99","%C5%82","%C5%84","%C3%B3","%C5%9B",
                   "%C5%BA","%C5%BC")
  
  for(i in 1:length(old_letters)) {
    
    name <- gsub(old_letters[i], new_letters[i], name)
    
  }
  
  return(name)
}

getData <- function(){
  
  # API KEY
  apikey <- "2b5e76a6-5515-4eb8-b173-130a648f210a"
  
  
  # trams API
  base_tram <- "https://api.um.warszawa.pl/api/action/wsstore_get/"
  id_tram <- "c7238cfe-8b1f-4c38-bb4a-de386db7e776"
  
  
  # buses API
  base_bus <- "https://api.um.warszawa.pl/api/action/busestrams_get/"
  id_bus <- "f2e5503e-927d-4ad3-9500-4ab9e55deb59"
  
  
  # bus stop value
  id_bus_stop_value <- "b27f4c17-5c50-4a5b-89dd-236b282bc499"
  name__id_bus_stop_value = "Madalinskiego"
  
  # example
  # https://api.um.warszawa.pl/api/action/dbtimetable_get/?
  # id=b27f4c17-5c50-4a5b-89dd-236b282bc499&
  # name=Madali%C5%84skiego&
  # apikey=2b5e76a6-5515-4eb8-b173-130a648f210a
  
  
  # available lines on the bus stop
  id_bus_lines <- "e923fa0e-d96c-43f9-ae6e-60518c9f3238"
  busstopId__id_bus_lines = "7009"
  busstopNr__id_bus_lines = "01"
  line = "174"
  
  # example
  # https://api.um.warszawa.pl/api/action/dbtimetable_get/?
  # id=88cd555f-6f31-43ca-9de4-66c479ad5942&
  # busstopId=3229&
  # busstopNr=01&
  # apikey=2b5e76a6-5515-4eb8-b173-130a648f210a
  

  # bus time table
  bus_info_base <- "https://api.um.warszawa.pl/api/action/dbtimetable_get"
  id_bus_time_table <- "e923fa0e-d96c-43f9-ae6e-60518c9f3238"
  busstopId__id_bus_time_table = "7009"
  busstopNr__id_bus_time_table = "01"
  line__id_bus_time_table = "174"
  
  # example
  # https://api.um.warszawa.pl/api/action/dbtimetable_get/?
  # id=e923fa0e-d96c-43f9-ae6e-60518c9f3238&busstopId=7009&
  # busstopNr=01&
  # line=523&
  # apikey=2b5e76a6-5515-4eb8-b173-130a648f210a
  
  
  # API CALLS
  call1 <- paste0(base_tram,"?","id=",id_tram,"&","apikey=",apikey)
  
  call2 <- paste0(base_bus,"?","resource_id=",id_bus,"&","apikey=",apikey,"&type=1")
  
  call3 <- paste0(bus_info_base,
                  "/?id=", id_bus_stop_value,
                  "&name=", convert_polish_letters(name__id_bus_stop_value),
                  "&apikey=", apikey)

  call4 <- paste0(bus_info_base,
                  "/?id=", id_bus_lines,
                  "&busstopId=", busstopId__id_bus_lines,
                  "&busstopNr=", busstopNr__id_bus_lines,
                  "&apikey=", apikey)
  
  call5 <- paste0(bus_info_base,
                  "/?id=", id_bus_time_table,
                  "&busstopId=", busstopId__id_bus_time_table,
                  "&busstopNr=", busstopNr__id_bus_time_table,
                  "&line=", line__id_bus_time_table,
                  "&apikey=", apikey)
  
  
  # API call function
  get_API_response <- function(call){
    
    get_response <- GET(call)
    get_text <- content(get_response, "text")
    get_json <- fromJSON(get_text, flatten = TRUE)
    data <- get_json$result
    
  }
  
  API_tram_call <- get_API_response(call1)
  API_bus_call <- get_API_response(call2)
  API_bus_stop_call <- get_API_response(call3)
  API_bus_stop_info_call <- get_API_response(call4)
  API_bus_timetable_call <- get_API_response(call5)

  
  # print("### 1 ###")
  
  trams_data <- API_tram_call
  buses_data <- API_bus_call
  bus_stop <- API_bus_stop_call
  bus_timetable <- API_bus_timetable_call
  
  # print("### 2 ###")
  
  # handling empty response from the API call
  while(class(trams_data) == "list"){
    
    Sys.sleep(1)
    trams_data <- API_tram_call()
    print("------> empty trams_data")
    
  }
  
  # print("### 3 ###")
  
  while(class(buses_data) == "list"){
    
    Sys.sleep(1)
    buses_data <- API_bus_call()
    print("------> empty buses_data")
    
  }
  
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
      uniq_first_lines <- c(unique(as.character(sort(as.numeric(data_)))))
    }else{
      # uniq_first_lines <- c("all", unique(as.character(sort(data_))))
      uniq_first_lines <- c(unique(as.character(sort(data_))))
    }
    
    sorted_factor <- factor(uniq_first_lines, levels=uniq_first_lines)
    label_list <- split(uniq_first_lines, sorted_factor)
    
    return(label_list)
  }
  
  print("### 9 ###")

  tram_label_list <- get_labels(backup_tram_data$FirstLine, TRUE)
  bus_label_list <- get_labels(backup_bus_data$Lines, FALSE)
  
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

  leafletOutput("mymap", width = "auto", height = "560px")
  
  
 
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
    
    if((is.null(input$tram_location_labels) == T & is.null(input$bus_location_labels) == T)){
      
      print("enter 1")
      trams_data <- data$trams_data %>% dplyr::filter(FirstLine %in% c("33"))
      buses_data <- data$buses_data %>% dplyr::filter(Lines %in% c("174"))
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list
      
    }else if(is.null(input$tram_location_labels) == T & is.null(input$bus_location_labels) == F){
      
      print("enter 2")
      trams_data <- data$trams_data %>% dplyr::filter(FirstLine %in% c("9"))
      print(trams_data)
      buses_data <- data$buses_data %>% dplyr::filter(Lines %in% input$bus_location_labels)
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list
      
    }else if(is.null(input$tram_location_labels) == F & is.null(input$bus_location_labels) == T){
      
      print("enter 3")
      trams_data <- data$trams_data %>% dplyr::filter(FirstLine %in% input$tram_location_labels)
      buses_data <- data$buses_data %>% dplyr::filter(Lines %in% c("174"))
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list
      
    }else{
      
      print("enter 4")
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
  
  
  icon.home <- makeAwesomeIcon(icon = 'home', library = "fa", markerColor = "green")
  icon.tram <- makeAwesomeIcon(icon = 'train', library = "fa", markerColor = "blue")
  icon.bus <- makeAwesomeIcon(icon = 'bus', library = "fa", markerColor = "red")
  
  
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
      )

  },ignoreNULL = FALSE)
})


shinyApp(ui, server)
