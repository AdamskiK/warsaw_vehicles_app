setwd("C:/Users/Adam/Desktop/Shiny-tram-app")

library("httr")
library("jsonlite")
library("shiny")
library("leaflet")
library("dplyr")

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
  get_trams <- GET(call1)
  get_trams_text <- content(get_trams, "text")
  get_trams_json <- fromJSON(get_trams_text, flatten = TRUE)
  trams_data <- get_trams_json$result
  
  
  # API call #2 response
  get_buses <- GET(call2)
  get_buses_text <- content(get_buses, "text")
  get_bueses_json <- fromJSON(get_buses_text, flatten = TRUE)
  buses_data <- get_bueses_json$result
  
  
  # handling empty response from the API call
  while(class(trams_data) == "list"){
    Sys.sleep(1)
    get_trams <- GET(call1)
    get_trams_text <- content(get_trams, "text")
    get_trams_json <- fromJSON(get_trams_text, flatten = TRUE)
    trams_data <- get_trams_json$result
    print(c("inside", summary(get_trams$content)["Length"]))
  }
  
  
  # the function filters out outliers
  filter_outliers <- function(data) {
    data <- data[data$Lat > 52.140083
                 & data$Lat < 52.346209
                 & data$Lon > 20.866590
                 & data$Lon < 21.143558,]
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
  uniq_first_lines <- c("all", unique(as.character(sort(as.numeric(backup_tram_data$FirstLine)))))
  sorted_factor <- factor(uniq_first_lines, levels=uniq_first_lines)
  my_new_list <- split(uniq_first_lines, sorted_factor)
  
  rownames(trams_data) <- NULL
  rownames(buses_data) <- NULL
  
  return(list("trams_data" = trams_data, "buses_data" = buses_data, "my_new_list" = my_new_list))
}


ui <- shinyUI(fluidPage(
  tags$style(type='text/css', 
             ".selectize-input { font-size: 20px; line-height: 20px; text-align: center; 
             text-indent: 0px; } 
             .selectize-dropdown { font-size: 15px; line-height: 15px; text-align: center; 
             text-indent: 0px; }
             .panel-primary { margin: 50px; font-size: 15px; 
             text-indent: 20px; }
             .custom_text { font-size: 10 px; }" 
  ),
  
  navbarPage("Warsaw Tram Finder (WTF)",
             tabPanel("MAP",
                      
                      leafletOutput("mymap", width = "auto", height = "560px")
             )
  ),
  
  absolutePanel(class = "panel panel-primary", draggable = F, top = 40, left="auto",
                right = 0, bottom = "auto", width = "350", height = "auto", margin = "0px",
                
                div(class="outer", h3("Controls")),
                h6(textOutput("cor_bind")),
                h6(textOutput("last_ref")),
                uiOutput("location_labels")
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
  )
  )



server <- shinyServer(function(input, output) {
  
  # renderUI - showing the drop-down list
  output$location_labels <- renderUI({
    
    selectInput("location_labels", label = h4("Choose location"), choices = labelsData() ,selected = "all")
    
  })
  
  # time set to refresh every 5000 msec - has to be passed as an argument
  autoInvalidate <- reactiveTimer(5000)
  
  reData <- eventReactive(autoInvalidate(), {
    
    if((is.null(input$location_labels) == T)){
      trams_data <- getData()$trams_data
    }
    else if (input$location_labels == "all") {
      trams_data <- getData()$trams_data
    }
    else {
      trams_data <- getData()$trams_data %>% dplyr::filter(FirstLine == input$location_labels)  
    }
    
    buses_data <- getData()$buses_data
    #print(head(trams_data))
    
    return(list("trams_data" = trams_data, "buses_data" = buses_data))
  })
  
  labelsData <- eventReactive(reactiveTimer(300000), {
    
    trams_data <- getData()$my_new_list
    
  })
  
  
  points <- reactive({
    cbind(reData()$trams_data$Lon, reData()$trams_data$Lat)
  })
  
  labels <- reactive({
    paste("line: ", reData()$trams_data$FirstLine)
  })
  
  
  output$mymap <- renderLeaflet({
    url_map <- a("OpenStreetMap", href="https://www.openstreetmap.org/copyright")
    url_my_github <- a("Kamil Adamski", href="https://github.com/AdamskiK")
    url_contrib <- a("Miasto Stoleczne Warszawa", href="https://api.um.warszawa.pl/")
    leaflet() %>%
      addTiles() %>%
      addTiles(attribution =
                 paste("© 2018 ",url_map, ", ", url_my_github, ", ", url_contrib, sep="")) %>%
      fitBounds(input$long-0.005, input$lat-0.005, input$long+0.005, input$lat+0.005)
  })
  
  output$cor_bind <- renderText({
    cor_bind <- c("Your lattitude and longitude: ", input$lat, ",", input$long, sep="")
  })
  
  
  # output$last_ref <- renderText({
  #   if(is.null(reData()$trams_data$Time[1]) == T){
  #     print(c("last_ref part1: ", reData()$trams_data$Time[1]))
  #     # do nothing
  #   }
  #   else{
  #     print(c("last_ref part2: ", reData()$trams_data$Time[1]))
  #     time1 <- as.POSIXct(reData()$trams_data$Time[1], format = '%Y-%m-%dT%H:%M:%S')
  #     time2  <- as.POSIXct(Sys.time())
  #     timeDiff <- round(difftime(time2,time1, units="sec"),0)
  #     
  #     last_ref <- paste("Last update: ",
  #                       strptime(reData()$trams_data$Time[1], format='%Y-%m-%dT%H:%M:%S'),
  #                       " rerfeshed: ", timeDiff, " sec ago")
  #   }
  # })
  
  
  
  
  home_icon <- iconList(
    ship = makeIcon("home_icon.png", 25, 25)
  )
  
  
  observeEvent(autoInvalidate(), {
    print(c("points: ", head(points())))
    print(c("labels: ", head(labels())))
    print(" ")
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addMarkers(
        data = points(),
        label = labels()) %>%
      addMarkers(
        data = cbind(as.numeric(as.character(input$long)),as.numeric(as.character(input$lat))),
        label = "Your position",
        icon = home_icon
      )
  },ignoreNULL = FALSE)
})


shinyApp(ui, server)
