#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    sampleR[sampleR$user_id==input$UIndex,]
  })
  
  output$URcount<-renderText({ 
    paste("URcount:",user_list$review_count.x[user_list$user_id==input$UIndex])
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(sampleR) %>% addTiles() #%>%
      #fitBounds(~min(longitude-10), ~min(latitude-10), ~max(longitude+10), ~max(latitude+10))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(
        ~longitude,~latitude,
        radius = 30, weight = 30, color = "red",
        fill="navy",fillOpacity = 0.7, popup = ~paste(name.y)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  
}
