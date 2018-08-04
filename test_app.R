library("httr")
library("jsonlite")
library("shiny")
library("leaflet")
library("dplyr")


ui <- shinyUI(fluidPage(
  navbarPage("Title",
             tabPanel("MAP",
                      
                      leafletOutput("mymap", width = "auto", height = "560px")
             )
  ),
  uiOutput("loc")
  )
  )

server <- shinyServer(function(input, output) {
  
  autoInvalidate <- reactiveTimer(5000)
  
  reData <- eventReactive(autoInvalidate(), {
    
    # # example data
    # lat <- c(20.51,20.52,20.65)
    # long <- c(10.33,13.43,23.54)
    # labels <- c('John','Peter','Jolie')
    # data <- data.frame(lat, long, labels)
    
    # API call #1 response
    get_data <- GET(call1)
    get_data_text <- content(get_data, "text")
    get_data_json <- fromJSON(get_data_text, flatten = TRUE)
    data <- get_data_json$result

    # handling empty API response
    while(class(data) == "list"){
      Sys.sleep(1)
      get_trams <- GET(call1)
      get_data_text <- content(get_data, "text")
      get_data_json <- fromJSON(get_data_text, flatten = TRUE)
      data <- get_data_json$result
    }
    
    
    # saving data before filtering - purpose of getting labels for the drop-down list and
    # creating a sorted list for selectInput function
    list_of_vals <- data
    uniq_first_lines <- c("all", unique(as.character(sort(as.numeric(list_of_vals$FirstLine)))))
    sorted_factor <- factor(uniq_first_lines, levels=uniq_first_lines)
    my_new_list <- split(uniq_first_lines, sorted_factor)
    
    selectInput("loc", label = h4("Choose location"),
                choices = reData()$my_new_list ,selected = "all"
    )
    
    # filter data
    if(input$loc != "all") {
      data <- data %>%
      filter_at(
        vars(one_of("FirstLine")),
        any_vars(.==input$loc))
    }
    
    rownames(data) <- NULL

    
    return(list(data=data, my_new_list=my_new_list))
  }, ignoreNULL = FALSE)
  

  output$loc <-renderUI({
    
  })
  
  
  points <- eventReactive(autoInvalidate(), {
    cbind(reData()$trams_data$Lon, reData()$trams_data$Lat)
  },ignoreNULL = FALSE)
  
  labels <- eventReactive(autoInvalidate(), {
    paste("line: ", reData()$trams_data$FirstLine)
  },ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  observeEvent(autoInvalidate(), {
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addMarkers(
        data = points(),
        label = labels()
      )
  },ignoreNULL = FALSE)
})


shinyApp(ui, server)
