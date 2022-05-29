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

packages = c('tidyverse', 'sf', 'tmap', 'lubridate', 'clock', 'sftime', 'rmarkdown')

for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
}


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
            fluidPage(
              titlePanel("Predominant Business Areas"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "locationType", 
                              label = "Location Type:",
                              choices = c("Apartments" = "Apartments",
                                          "Employers/Jobs" = "Employers",
                                          "Pubs" = "Pubs",
                                          "Restaurants" = "Restaurants",
                                          "Schools" = "Schools"),
                              multiple = FALSE),
                  
                  
                ),
                mainPanel(
                  h4("Map for all buildings and locations"),
                  tmapOutput(outputId = "mapPlotAll"),
                  h4("Map for selected location type"),
                  tmapOutput(outputId = "mapPlotbyType")
                )
              )
            )
    )
  )
)



## put UI together --------------------
ui <- dashboardPage(skin = "blue",
                    header, 
                    siderbar, 
                    body )

## Start of Data Import
participants_data <- read_csv('data/Participants.csv')
schools <- read_sf("data/Schools.csv", 
                   options = "GEOM_POSSIBLE_NAMES=location")

buildings <- read_sf("data/buildings.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")

pubs <- read_sf("data/pubs.csv", 
                options = "GEOM_POSSIBLE_NAMES=location")

apartments <- read_sf("data/apartments.csv", 
                      options = "GEOM_POSSIBLE_NAMES=location")

employers <- read_sf("data/employers.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")

restaurants <- read_sf("data/restaurants.csv", 
                       options = "GEOM_POSSIBLE_NAMES=location")

jobs <- read_csv("data/jobs.csv")

apartments <- apartments%>%
  mutate(rentalCost = as.numeric(rentalCost))%>%
  mutate(maxOccupancy = as.numeric(maxOccupancy))%>%
  mutate(numberOfRooms = as.numeric(numberOfRooms))

buildings <- buildings%>%
  mutate(maxOccupancy = as.numeric(maxOccupancy))

pubs <- pubs%>%
  mutate(hourlyCost = as.numeric(hourlyCost))%>%
  mutate(maxOccupancy = as.numeric(maxOccupancy))

restaurants <- restaurants%>%
  mutate(foodCost = as.numeric(foodCost))%>%
  mutate(maxOccupancy = as.numeric(maxOccupancy))

schools <- schools%>%
  mutate(monthlyCost = as.numeric(monthlyCost))%>%
  mutate(maxEnrollment = as.numeric(maxEnrollment))

jobs <- jobs%>%
  mutate(jobId = as.character(jobId))%>%
  mutate(employerId = as.character(employerId))

jobs_employers <- merge(jobs, employers, by="employerId")
jobs_employers <- st_as_sf(jobs_employers)

## End of Data Import


server <- function(input, output){
  ## Start of Section 1
  
  output$demographics_plot <-renderPlot({
    ggplot(data=participants_data,
           aes(x = educationLevel, y = joviality)) + geom_boxplot(notch=TRUE)+
      stat_summary(geom = "point",
                   fun="mean",
                   colour="red",
                   size=4) +
      ggtitle("Distribution of Juviality for different interest group")
  })
  
  ## End of Section 1
  
  
  ## Start of Section 2
  output$social_act_table <- renderDataTable(mtcars)
  
  
  ## End of Section 2
  
  
  ## Start of Section 3
  output$mapPlotAll <- renderTmap({
    tmap_mode("view")
    tm_shape(buildings)+
      tm_polygons(col = "buildingType",
                  palette="Accent",
                  border.col = "black",
                  border.alpha = .5,
                  border.lwd = 0.5)+
      tm_shape(employers) +
      tm_dots(col = "red") +
      tm_shape(apartments) +
      tm_dots(col = "lightblue") +
      tm_shape(pubs) +
      tm_dots(col = "green") +
      tm_shape(restaurants) +
      tm_dots(col = "blue") +
      tm_shape(schools) +
      tm_dots(col = "yellow")
    
  })
  
  output$mapPlotbyType <- renderTmap({
    if (input$locationType == "Apartments"){
      tmap_mode("view")
      tm_shape(buildings)+
        tm_polygons(col = "buildingType",
                    palette="Accent",
                    border.col = "black",
                    border.alpha = .5,
                    border.lwd = 0.5)+
        tm_shape(apartments)+
        tm_bubbles(col = "rentalCost",
                   alpha = 0.8,
                   n = 6,
                   style = "jenks",
                   palette="Purples",
                   size = "numberOfRooms",
                   scale = 0.8,
                   border.col = "black",
                   border.lwd = 0.5)
    }
    
    else if (input$locationType == "Pubs"){
      tmap_mode("view")
      tm_shape(buildings)+
        tm_polygons(col = "buildingType",
                    palette="Accent",
                    border.col = "black",
                    border.alpha = .5,
                    border.lwd = 0.5)+
        tm_shape(pubs)+
        tm_bubbles(col = "hourlyCost",
                   alpha = 0.8,
                   n = 6,
                   style = "jenks",
                   palette="BuGn",
                   size = "maxOccupancy",
                   scale = 0.8,
                   border.col = "black",
                   border.lwd = 0.5)
    }
    
    else if (input$locationType == "Restaurants"){
      tmap_mode("view")
      tm_shape(buildings)+
        tm_polygons(col = "buildingType",
                    palette="Accent",
                    border.col = "black",
                    border.alpha = .5,
                    border.lwd = 0.5)+
        tm_shape(restaurants)+
        tm_bubbles(col = "foodCost",
                   alpha = 0.8,
                   n = 6,
                   style = "jenks",
                   palette="Blues",
                   size = "maxOccupancy",
                   scale = 0.8,
                   border.col = "black",
                   border.lwd = 0.5)
    } 
    
    else if (input$locationType == "Schools"){
      tmap_mode("view")
      tm_shape(buildings)+
        tm_polygons(col = "buildingType",
                    palette="Accent",
                    border.col = "black",
                    border.alpha = .5,
                    border.lwd = 0.5)+
        tm_shape(schools)+
        tm_bubbles(col = "monthlyCost",
                   alpha = 0.8,
                   n = 6,
                   style = "jenks",
                   palette="YlOrRd",
                   size = "maxEnrollment",
                   scale = 0.8,
                   border.col = "black",
                   border.lwd = 0.5)
    } 
    
    else if (input$locationType == "Employers"){
      tmap_mode("view")
      tm_shape(buildings)+
        tm_polygons(col = "buildingType",
                    palette="Accent",
                    border.col = "black",
                    border.alpha = .5,
                    border.lwd = 0.5)+
        tm_shape(jobs_employers)+
        tm_bubbles(col = "educationRequirement",
                   alpha = 0.8,
                   n = 6,
                   style = "jenks",
                   palette="Reds",
                   size = "hourlyRate",
                   scale = 1.2,
                   border.col = "black",
                   border.lwd = 0.5)
    } 
    
    
  })
  
  ## End of Section 3
  
}

shinyApp(ui = ui, server = server)