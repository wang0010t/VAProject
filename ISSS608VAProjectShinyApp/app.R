library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
options(spinner.color="#006272")
library(timevis)
## build ui.R -----------------------------------
## 1. header -------------------------------
header <- 
  dashboardHeader( title = HTML("city of Engagement, Ohio USA"))



## 2. siderbar ------------------------------
siderbar <- 
  dashboardSidebar(
    menuItem("Demographics analysis", tabName = "demographics_tab", icon = icon("dashboard")),
    menuItem("Social activity", tabName = "social_activity_tab"),
    menuItem("Predominant Business", tabName = "predominant_business_tab")
  )

## 3. body --------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "demographics_tab",
            h2("Demographics analysis content")
            
    ),
    
    tabItem(tabName = "social_activity_tab",
            h2("Social activity content")
    ),
    tabItem(tabName = "predominant_business_tab",
            h2("Predominant Business")
    )
  )
)



## put UI together --------------------
ui <- 
  dashboardPage(header, siderbar, body )

server <- function(input, output){

}

shinyApp(ui = ui, server = server)