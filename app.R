# setwd("C:/Users/Adam/Desktop/Shiny/warsaw-public-transport")

library("httr")
library("jsonlite")
library("shiny")
library("leaflet")
library("dplyr")
library("shinydashboard")

getData <- function(){
  
  # API KEY
  apikey <- "2b5e76a6-5515-4eb8-b173-130a648f210a"
  
  
  # trams API
  base_tram <- "https://api.um.warszawa.pl/api/action/wsstore_get/"
  id_tram <- "c7238cfe-8b1f-4c38-bb4a-de386db7e776"
  
  
  # buses API
  base_bus <- "https://api.um.warszawa.pl/api/action/busestrams_get/"
  id_bus <- "f2e5503e-927d-4ad3-9500-4ab9e55deb59"
  
  
  # API CALLS
  call1 <- paste(base_tram,"?","id=",id_tram,"&","apikey=",apikey, sep="")
  call2 <- paste(base_bus,"?","resource_id=",id_bus,"&","apikey=",apikey,"&type=1", sep="")
  
  
  # API call #1 response
  API_tram_call <- function(){
    
    get_trams <- GET(call1)
    get_trams_text <- content(get_trams, "text")
    get_trams_json <- fromJSON(get_trams_text, flatten = TRUE)
    trams_data <- get_trams_json$result
    
  }
  
  
  # API call #2 response
  API_bus_call <-  function(){
    
    get_buses <- GET(call2)
    get_buses_text <- content(get_buses, "text")
    get_bueses_json <- fromJSON(get_buses_text, flatten = TRUE)
    buses_data <- get_bueses_json$result
    
  }
  
  # print("### 1 ###")
  
  trams_data <- API_tram_call()
  buses_data <- API_bus_call()
  
  # print("### 2 ###")
  
  # print(c("start class of tram data: "))
  # print(class(trams_data))
  # print(c("end class of tram data: "))
  
  # handling empty response from the API call
  while(class(trams_data) == "list"){
    
    print(c("trams_data is:", trams_data))
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
  
  
  # split converts the f (second) argument to factors, if it isn't already one.
  # So, if you want the order to be retained, factor the column yourself
  # with the desired levels.
  get_labels <- function(data_, numeric_type){
    
    if(numeric_type){
      uniq_first_lines <- c("all", unique(as.character(sort(as.numeric(data_)))))
    }else{
      uniq_first_lines <- c("all", unique(as.character(sort(data_))))
    }
    
    sorted_factor <- factor(uniq_first_lines, levels=uniq_first_lines)
    label_list <- split(uniq_first_lines, sorted_factor)
    
    return(label_list)
  }
  

  tram_label_list <- get_labels(backup_tram_data$FirstLine, TRUE)
  bus_label_list <- get_labels(backup_bus_data$Lines, FALSE)
  
  # print("### 8 ###")
  
  rownames(trams_data) <- NULL
  rownames(buses_data) <- NULL
  
  # print("### 9 ###")
  
  return(list("trams_data" = trams_data,
              "buses_data" = buses_data,
              "tram_label_list" = tram_label_list,
              "bus_label_list" = bus_label_list))
}


ui <- dashboardPage(
  dashboardHeader(title  = "Warsaw Public Transport", titleWidth = "270px"),
  dashboardSidebar(width = "270px", 
    
    div(class="outer", h3("Controls")),
    uiOutput("tram_lines"),
    uiOutput("test"),
    h6(textOutput("cor_bind")),
    h6(textOutput("last_ref")),
    
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



server <- shinyServer(function(input, output) {
  
  
  # renderUI - showing the drop-down list
  output$tram_lines <- renderUI({
    
    selectInput("tram_location_labels", label = h4("Filter tram line"), choices = tramLabels() ,selected = "all")
    
  })
  
  
  output$test <- renderUI({
    
    selectInput("bus_location_labels", label = h4("Filter bus line"), choices = busLabels() ,selected = "all")
    
  })
  
  
  # output$bus_location_labels <- renderUI({
  # 
  #   selectInput("bus_location_labels", label = h4("Filter bus lines"), choices = busLabels() ,selected = "all")
  # 
  # })
  
  
  # time set to refresh every x msec - has to be passed as an argument
  autoInvalidate <- reactiveTimer(10000)
  
  
  reData <- eventReactive(autoInvalidate(), {
    
    
    data <- getData()
    
    if((is.null(input$tram_location_labels) == T)){

      trams_data <- data$trams_data
      buses_data <- data$buses_data
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list

    }
    else if (input$tram_location_labels == "all" & input$bus_location_labels == "all") {

      trams_data <- data$trams_data
      buses_data <- data$buses_data
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list

    }
    else if (input$tram_location_labels == "all" & input$bus_location_labels != "all") {
      
      trams_data <- data$trams_data
      buses_data <- data$buses_data %>% dplyr::filter(Lines == input$bus_location_labels)
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list
      
    }
    else if (input$tram_location_labels != "all" & input$bus_location_labels == "all") {
      
      trams_data <- data$trams_data %>% dplyr::filter(FirstLine == input$tram_location_labels)
      buses_data <- data$buses_data
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list
      
    }
    else {

      trams_data <- data$trams_data %>% dplyr::filter(FirstLine == input$tram_location_labels)
      buses_data <- data$buses_data %>% dplyr::filter(Lines == input$bus_location_labels)
      tram_label_list <- data$tram_label_list
      bus_label_list <- data$bus_label_list

    }
    
    # trams_data <- data$trams_data
    # buses_data <- data$buses_data
    # tram_label_list <- data$tram_label_list
    # bus_label_list <- data$bus_label_list
    
    return(list("trams_data" = trams_data,
                "buses_data" = buses_data,
                "tram_label_list" = tram_label_list,
                "bus_label_list" = bus_label_list))
  })
  
  
  # get tram labels
  tramLabels <- eventReactive(reactiveTimer(300000), {
    
    reData()$tram_label_list
    
  })
  
  
  # get bus labels
  busLabels <- eventReactive(reactiveTimer(300000), {
    
    print(c("printing bus data: ", reData()$bus_label_list))
    reData()$bus_label_list
    # reData()$bus_label_list
    
  })
  
  
  # reactive points and labels
  tram_points <- reactive({

    tram_points <- cbind(reData()[["trams_data"]]["Lon"], reData()[["trams_data"]]["Lat"])

    return(tram_points)
  })
  
  
  tram_labels <- reactive({
    
    tram_labels <- paste("line: ", reData()[["trams_data"]][[2]])
    
    return(tram_labels)
  })
  
  
  bus_points <- reactive({

    bus_points <- cbind(reData()$buses_data[[2]], reData()$buses_data[[1]])

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
    time_sample <- reData()[["trams_data"]][[5]][1]
    if(is.null(time_sample) == T){
      # do nothing
    }
    else{
      time1 <- as.POSIXct(time_sample, format = '%Y-%m-%dT%H:%M:%S')
      time2  <- as.POSIXct(Sys.time())
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
  
  home_icon <- iconize("home_icon.png", 25, 25) 
  tram_icon <- iconize("tram_icon.png", 35, 35)
  bus_icon <- iconize("bus_icon.png", 35, 35)
  
  
  observeEvent(autoInvalidate(), {
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addMarkers(
        data = tram_points(),
        label = tram_labels(),
        # clusterOptions = markerClusterOptions(showCoverageOnHover  = T))
        icon = tram_icon) %>%
      
      addMarkers(
        data = bus_points(),
        label = bus_labels(),
        icon = bus_icon) %>%
      addMarkers(
        data = cbind(as.numeric(as.character(input$long)),as.numeric(as.character(input$lat))),
        label = "Your position",
        icon = home_icon
      )
  },ignoreNULL = FALSE)
})


shinyApp(ui, server)
