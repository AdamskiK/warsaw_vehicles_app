library("httr")
library("jsonlite")
library("shiny")
library("leaflet")

ui <- fluidPage(
  navbarPage("Warsaw Tram Finder (WTF)",
             tabPanel("MAP",
                      textOutput("cor_bind"),
                      textOutput("last_ref"),
                      leafletOutput("mymap", width = "auto", height = "560px")
                      ),
             absolutePanel(top = 10, right = 10,
                           selectInput("colors", "Color Scheme", "lines")
             ),
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
      # print(c("before", summary(get_trams$content)["Length"])) # for value of 15 error appears
      
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
      
      # print(c("after", summary(get_trams$content)["Length"]))
      
      trams_data <- trams_data[trams_data$Lat > 52.140083
                       & trams_data$Lat < 52.346209
                       & trams_data$Lon > 20.866590
                       & trams_data$Lon < 21.143558,]

      trams_data$FirstLine <- as.character(as.numeric(trams_data$FirstLine))
      rownames(trams_data) <- NULL
      
      return(trams_data)
      }, ignoreNULL = FALSE)

  
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
    cor_bind <- c("Your lattitude: ", input$lat, "and longitude: ", input$long, sep="")
  })
  output$last_ref <- renderText({
    last_ref <- paste("Last update: ", 
                      strptime(reData()$Time[1], format='%Y-%m-%dT%H:%M:%S'),
                      " --> time refresh within 30 seconds")
  })
  
  observeEvent(autoInvalidate(), {
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addMarkers(
        data = points(),
        label = labels())
  },ignoreNULL = FALSE)
}

shinyApp(ui, server)