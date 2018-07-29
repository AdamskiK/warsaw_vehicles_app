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
  
  autoInvalidate <- reactiveTimer(5000)
  
  reData <- eventReactive(autoInvalidate(), {
  
  get_trams <- GET(call1)
  get_trams_text <- content(get_trams, "text")
  get_trams_json <- fromJSON(get_trams_text, flatten = TRUE)
  trams_data <- get_trams_json$result
  
  filtered_data <- trams_data[trams_data$Lat > 52.140083
                   & trams_data$Lat < 52.346209
                   & trams_data$Lon > 20.866590
                   & trams_data$Lon < 21.143558,]

  rownames(filtered_data) <- NULL
  
  return(filtered_data)
  }, ignoreNULL = FALSE)
  
  points <- eventReactive(autoInvalidate(), {
    cbind(reData()$Lon, reData()$Lat)
  }, ignoreNULL = FALSE)
  
  labels <- eventReactive(autoInvalidate(), {
    reData()$FirstLine
  },ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(20.866590, 52.140083, 21.143558, 52.346209)
  })
  
  observeEvent(autoInvalidate(), {
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addMarkers(data = points(), label = labels())
  },ignoreNULL = FALSE)
}

shinyApp(ui, server)