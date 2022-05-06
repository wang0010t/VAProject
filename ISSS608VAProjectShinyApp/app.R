library(shiny)
library(tidyverse)



ui <- fluidPage(
  titlePanel("Dashboard"),
  sidebarLayout(
    sidebarPanel(
      
      
    ),
    mainPanel(
      
      
    )
  )
)

server <- function(input, output){

}

shinyApp(ui = ui, server = server)