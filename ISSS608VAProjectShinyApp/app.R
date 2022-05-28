library(shinydashboard)
library(shinythemes)
library(shiny)
library(tidyverse)
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
  dashboardHeader( title = HTML("Ohio USA Demo"))



## 2. siderbar ------------------------------
siderbar <- 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics analysis", tabName = "demographics_tab"),
      menuItem("Social activity", tabName = "social_activity_tab"),
      menuItem("Predominant Business", tabName = "predominant_business_tab")
    )
  )

## 3. body --------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "demographics_tab",
            h2("Demographics analysis content"),
            box(
              plotOutput("demographics_plot")
              # selectInput("features","Features:",
              #             c("Sepal.Width","Petal.Length","Petal.Width")),
              # width=8
              )
            
    ),
    
    tabItem(tabName = "social_activity_tab",
            fluidPage(
              h2("Social activity content"),
              dataTableOutput("social_act_table")
            )
    ),
    tabItem(tabName = "predominant_business_tab",
            h2("Predominant Business")
    )
  )
)



## put UI together --------------------
ui <- dashboardPage(skin = "blue",
                    header, 
                    siderbar, 
                    body )
participants_data <- read_csv('./data/Participants.csv')
server <- function(input, output){
  # generate a sample plot
  
  output$demographics_plot <-renderPlot({
    ggplot(data=participants_data,
           aes(x = educationLevel, y = joviality)) + geom_boxplot(notch=TRUE)+
      stat_summary(geom = "point",
                   fun="mean",
                   colour="red",
                   size=4) +
      ggtitle("Distribution of Juviality for different interest group")
  })
  # generate a sample table
  output$social_act_table <- renderDataTable(mtcars)
}

shinyApp(ui = ui, server = server)