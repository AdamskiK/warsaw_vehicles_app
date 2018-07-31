library("httr")
library("jsonlite")
library("shiny")
library("leaflet")
library("dplyr")

ui <- fluidPage(
  tags$style(type='text/css', 
             ".selectize-input { font-size: 20px; line-height: 20px; position: relative; 
left: 20px; top: 0px; } 
             .selectize-dropdown { font-size: 15px; line-height: 15px; }
             .panel-primary { margin: 50px; font-size: 15px; text-align: center; }
             .custom_text { font-size: 10 px; }" 
             ),
  
  navbarPage("Warsaw Tram Finder (WTF)",
             tabPanel("MAP",
                      
                      leafletOutput("mymap", width = "auto", height = "560px")
                      )
             ),

  absolutePanel(class = "panel panel-primary", draggable = TRUE, top = 40, left="auto",
                right = 0, bottom = "auto", width = "350", height = "auto", margin = "10px",
                
                div(class="outer", h3("Controls")),
                h6(textOutput("cor_bind")),
                h6(textOutput("last_ref")),
                selectInput("distLineVals", "Choose a tram line:",
                            choices = my_new_list )
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

server <- function(input, output, session) {
  
  base <- "https://api.um.warszawa.pl/api/action/wsstore_get/"
  id <- "c7238cfe-8b1f-4c38-bb4a-de386db7e776"
  apikey <- "2b5e76a6-5515-4eb8-b173-130a648f210a"
  
  call1 <- paste(base,"?","id=",id,"&","apikey=",apikey, sep="")
  
  autoInvalidate <- reactiveTimer(5000)
  
  reData <- eventReactive(autoInvalidate(), {
  
      get_trams <- GET(call1)
      get_trams_text <- content(get_trams, "text")
      get_trams_json <- fromJSON(get_trams_text, flatten = TRUE)
      trams_data <- get_trams_json$result
      
      # handling empty response from the API call
      while(class(trams_data) == "list"){
        Sys.sleep(5)
        get_trams <- GET(call1)
        get_trams_text <- content(get_trams, "text")
        get_trams_json <- fromJSON(get_trams_text, flatten = TRUE)
        trams_data <- get_trams_json$result
        print(c("inside", summary(get_trams$content)["Length"]))
      }
      
      trams_data <- trams_data[trams_data$Lat > 52.140083
                       & trams_data$Lat < 52.346209
                       & trams_data$Lon > 20.866590
                       & trams_data$Lon < 21.143558,]

      trams_data$FirstLine <- as.character(as.numeric(trams_data$FirstLine))
      
      
      # setting backup for dropdown list
      backup_data <- trams_data
      
      # split converts the f (second) argument to factors, if it isn't already one. 
      # So, if you want the order to be retained, factor the column yourself 
      # with the desired level.
      uniq_first_lines <- c("all", unique(as.character(sort(as.numeric(backup_data$FirstLine)))))
      sorted_factor <- factor(uniq_first_lines, levels=uniq_first_lines)
      my_new_list <- split(uniq_first_lines, sorted_factor)
      
      if(input$distLineVals != "all") {
      trams_data <- trams_data %>%
        filter_at(
          vars(contains("FirstLine")),
          any_vars(.==input$distLineVals))
      }
      
      rownames(trams_data) <- NULL
      
      return(trams_data)
      }, ignoreNULL = FALSE)
  
  
  
  # Reactive values
  
  points <- eventReactive(autoInvalidate(), {
    cbind(reData()$Lon, reData()$Lat)
  }, ignoreNULL = FALSE)

  labels <- eventReactive(autoInvalidate(), {
    paste("line: ", reData()$FirstLine)
  },ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(input$long-0.005, input$lat-0.005, input$long+0.005, input$lat+0.005)
  })
  
  output$cor_bind <- renderText({
    cor_bind <- c("Your lattitude and longitude: ", input$lat, ",", input$long, sep="")
  })
  
  output$last_ref <- renderText({
    last_ref <- paste("Last update: ", 
                      strptime(reData()$Time[1], format='%Y-%m-%dT%H:%M:%S'),
                      " (refresh every 30 secs)")
  })
  
  
  home_icon <- iconList(
    ship = makeIcon("home_icon.png", 25, 25)
  )
  
  
  observeEvent(autoInvalidate(), {
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
}

shinyApp(ui, server)