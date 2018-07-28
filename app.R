library("httr")
library("jsonlite")
library("shiny")
library("leaflet")

base <- "https://api.um.warszawa.pl/api/action/wsstore_get/"
id <- "c7238cfe-8b1f-4c38-bb4a-de386db7e776"
apikey <- "2b5e76a6-5515-4eb8-b173-130a648f210a"

call1 <- paste(base,"?","id=",id,"&","apikey=",apikey, sep="")

get_trams <- GET(call1)
get_trams_text <- content(get_trams, "text")
get_trams_json <- fromJSON(get_trams_text, flatten = TRUE)
trams_data <- get_trams_json$result

# outlier_detection_lon <
mean_lon <- mean(trams_data$Lon)  
mean_lat <- mean(trams_data$Lat)
trams_data_lon_matrix <- as.matrix(trams_data$Lon)
trams_data_lat_matrix <- as.matrix(trams_data$Lat)

# OUTLIER OR NOT
ODLON <- apply(trams_data_lon_matrix,1,function(x, mean=mean_lon, margin=0.05) 
  if(x>mean*(1+margin))
    {1}
  else if(x<mean*(1-margin))
    {1}
  else
    {0})

ODLAT <- apply(trams_data_lat_matrix,1,function(x, mean=mean_lat, margin=0.05) 
  if(x>mean*(1+margin))
  {1}
  else if(x<mean*(1-margin))
  {1}
  else
  {0})

ODFINAL <- apply(cbind(ODLON, ODLAT), 1, function(x) if(sum(x)>=1){TRUE}else{FALSE})

trams_data <- trams_data[!ODFINAL,]

rownames(trams_data) <- NULL


ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(trams_data$Lon, trams_data$Lat)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = points())
  })
}

shinyApp(ui, server)