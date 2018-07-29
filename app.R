library("httr")
library("jsonlite")
library("shiny")
library("leaflet")

ui <- fluidPage(
  navbarPage("Warsaw Tram Finder (WTF)",
             tabPanel("MAP",
                      textOutput("cor_bind"),
                      leafletOutput("mymap", width = "auto", height = "560px")
                      )
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
  
  autoInvalidate <- reactiveTimer(10000)
  
  reData <- eventReactive(autoInvalidate(), {
  
      get_trams <- GET(call1)
      get_trams_text <- content(get_trams, "text")
      get_trams_json <- fromJSON(get_trams_text, flatten = TRUE)
      trams_data <- get_trams_json$result
      
      # trams_data <- trams_data[trams_data$Lat > 52.140083
      #                  & trams_data$Lat < 52.346209
      #                  & trams_data$Lon > 20.866590
      #                  & trams_data$Lon < 21.143558,]
      # 
      # trams_data$FirstLine <- as.character(as.numeric(filtered_data$FirstLine))
      # rownames(trams_data) <- NULL
      
      #print(c("1st place trams_data$Lat: ", length(trams_data$Lat)))
      
      return(trams_data)
      }, ignoreNULL = FALSE)

  
  # points <- eventReactive(autoInvalidate(), {
  #   cbind(print(reData()$Lon), print(reData()$Lat))
  #   #print(c("2nd place points: ", length(reData()$Lon)))
  # }, ignoreNULL = FALSE)
  # 
  # labels <- eventReactive(autoInvalidate(), {
  #   print(reData()$FirstLine)
  # },ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      #fitBounds(input$long-0.005, input$lat-0.005, input$long+0.005, input$lat+0.005)
      fitBounds(20.866590, 52.140083, 21.143558, 52.346209)
  })
  
  output$cor_bind <- renderText({
    cor_bind <- c("lattitude: ", "\n", input$lat, ", longitude: ", input$long, sep="")
    #print(c("3rd place filtered_data$Lon: ", length(length(filtered_data$Lon))))
  })
  
  observeEvent(autoInvalidate(), {
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      
    
      # addMarkers(data = points(),  
      #            label = labels()) # error appears
    
    
      addMarkers(
        data = cbind(print(
          if(is.null(reData()$Lon) == TRUE)
          {21.0053246}
          else{reData()$Lon}),
          if(is.null(reData()$Lat) == TRUE)
          {52.2046528}
          else{reData()$Lat}),
        
      # even if reactive functions are not used, the error appears
      label = print(
        if(is.null(reData()$FirstLine) == TRUE)
        {"home"}
        else{reData()$FirstLine})) # error appears
      # label = reData()$FirstLine) # error appears
  },ignoreNULL = FALSE)
}

shinyApp(ui, server)