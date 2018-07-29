library("httr")
library("jsonlite")
library("shiny")
library("leaflet")


ui <- fluidPage(
  leafletOutput("mymap", width = "auto", height = "560px"),
  p(),
  actionButton("refresh", "Refresh")
)

server <- function(input, output, session) {
  
  base <- "https://api.um.warszawa.pl/api/action/wsstore_get/"
  id <- "c7238cfe-8b1f-4c38-bb4a-de386db7e776"
  apikey <- "2b5e76a6-5515-4eb8-b173-130a648f210a"
  
  call1 <- paste(base,"?","id=",id,"&","apikey=",apikey, sep="")
  
  # reaload a page if the refresh button gets clicked
  #observeEvent(input$refresh, {
  #  session$reload()
  #  return()
  #})
  
  reData <- eventReactive(input$refresh, {
  
  get_trams <- GET(call1)
  get_trams_text <- content(get_trams, "text")
  get_trams_json <- fromJSON(get_trams_text, flatten = TRUE)
  trams_data <- get_trams_json$result
  
  # # outlier_detection_lon <
  # mean_lon <- mean(trams_data$Lon)
  # mean_lat <- mean(trams_data$Lat)
  # trams_data_lon_matrix <- as.matrix(trams_data$Lon)
  # trams_data_lat_matrix <- as.matrix(trams_data$Lat)
  # 
  # # OUTLIER OR NOT
  # ODLON <- apply(trams_data_lon_matrix,1,function(x, mean=mean_lon, margin=0.05) 
  #   if(x>mean*(1+margin))
  #   {1}
  #   else if(x<mean*(1-margin))
  #   {1}
  #   else
  #   {0})
  # 
  # ODLAT <- apply(trams_data_lat_matrix,1,function(x, mean=mean_lat, margin=0.05) 
  #   if(x>mean*(1+margin))
  #   {1}
  #   else if(x<mean*(1-margin))
  #   {1}
  #   else
  #   {0})
  # 
  # ODFINAL <- apply(cbind(ODLON, ODLAT), 1, function(x) if(sum(x)>=1){TRUE}else{FALSE})
  # 
  # trams_data <- trams_data[!ODFINAL,]
  # 
  # rownames(trams_data) <- NULL
  
  return(trams_data)
  }, ignoreNULL = FALSE)
  
  points <- eventReactive(input$refresh, {
    cbind(reData()$Lon, reData()$Lat)
  }, ignoreNULL = FALSE)
  
  labels <- eventReactive(input$refresh, {
    reData()$FirstLine
  },ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds("20.866590", "52.140083", "21.143558", "52.346209")
  })
  
  observeEvent(input$refresh, {
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addMarkers(data = points(), label = labels())
  },ignoreNULL = FALSE)
}

shinyApp(ui, server)