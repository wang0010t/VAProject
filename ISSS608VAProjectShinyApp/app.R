library(shinydashboard)
library(shinythemes)
library(gghighlight)
library(shiny)
library(plotly)
library(tidyverse)
library(ggplot2)
library(ggiraph)
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
library(patchwork) #combine separate ggplots into the same graphic
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
              helpText("Create demographic maps with 
               information from the 2010 US Census."),
              
              selectInput(inputId = "demographicType", 
                          label = "Choose a variable to display",
                          choices = c("Education Level", 
                                      "Joviality",
                                      "Age", 
                                      "Have Kids"),
                          selected = "Percent White"),
              plotOutput("demographics_plot")
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

financeJ <- read_csv(file = "data/FinancialJournal.csv")

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

## Start of data processing
participants_data_ag <- participants_data %>%
  mutate(ageGroup = case_when(
    age <=25 ~ "25 and below",
    age > 25 & age <=35 ~ "26-35",
    age > 35 & age <=45 ~ "36-45",
    age > 45 & age <=55 ~ "46-55",
    age > 55 ~ "56 and over")) %>% 
  select(-age)
  

participants_data_ag_haveKids <- participants_data_ag %>%
  filter(`haveKids` ==  TRUE) %>%
  mutate (householdSize = -householdSize)


participants_data_ag_noKids <-participants_data_ag %>%
  filter(`haveKids` ==  FALSE)

participants_data_ag_byKids <- rbind(participants_data_ag_haveKids, participants_data_ag_noKids)

## End of data processing

server <- function(input, output){
  ## Start of Section 1
  education_level_plot <- ggplot(data=participants_data,
         aes(x = educationLevel, y = joviality)) + geom_boxplot(notch=TRUE)+
    stat_summary(geom = "point",
                 fun="mean",
                 colour="red",
                 size=4) +
    ggtitle("Distribution of Juviality for different interest group")
  
  output$demographics_plot <- renderPlot({
    if (input$demographicType == 'Education Level')
      {education_level_plot}
    else if (input$demographicType == 'Have Kids')
    {
      ggplot(participants_data_ag_byKids, aes (x = ageGroup, y = householdSize , fill = haveKids)) +
        geom_bar(stat = "identity") +
        coord_flip()+
        scale_y_continuous(breaks = seq(-250, 250, 50),
                           labels = paste0(as.character(c(seq(250, 0, -50), seq(50, 250, 50)))),
                           name = "Household Size")+
        labs(x = "Age Group", title = "Household size by age groups and whether have kids")+
        theme_bw()
    }
    else if (input$demographicType == 'Joviality'){
      data <- participants_data
      dpp <- data %>%
        group_by(age) %>%
        summarise(joviality = mean(joviality))
      p1 <- ggplot(data=dpp, aes(x=age, y=joviality)) + geom_point() +
        coord_cartesian(xlim=c(20, 60), ylim=c(0, 1)) + 
        labs(y= 'Joviality', x= 'Age',
             title = "Distribution of Residents' Joviality vs. Age",
             subtitle = "People's Joviality doesn't affect by Age") +
        geom_hline(yintercept=0.5, linetype="dashed", color="grey60", size=1) +  
        geom_vline(xintercept=40, linetype="dashed", color="grey60", size=1)
      p1
    }
    else if (input$demographicType == 'Age'){
      # participants_data$rate_edu <- factor(participants_data$educationLevel,levels = c("Graduate", "Bachelors","HighSchoolOrCollege","Low"))
      # ggplot(data=participants_data,aes(y = age, x= rate_edu)) + geom_boxplot(fill = "royalblue2",alpha= 0.5)+
      #   labs(x="Education", y="Age", title = "Age vs.Education Level") +
      #   theme_minimal()
      brks <- c(17, 20, 30, 40, 50, 60, Inf)
      grps <- c('<=20', '21-30', '31-40', '41-50', '51-60', '>60')
      data <- participants_data
      data$Age_Group <- cut(data$age, breaks=brks, labels = grps, right = FALSE)
      p2 <- ggplot(data = data, 
                   aes(x = Age_Group)) +
        geom_bar(fill="light blue") +
        ylim(0, 300) +
        geom_text(stat = 'count',
                  aes(label= paste0(stat(count), ' (', 
                                    round(stat(count)/sum(stat(count))*100, 
                                          1), '%)')), vjust= -0.5, size= 2.5) +
        gghighlight(Age_Group != "<=20" & Age_Group != ">60")+
        labs(y= 'No. of\nResidents', x= 'Age Group',
             title = "Distribution of Residents' Age",
             subtitle = "Most of residents are in working age(20-60)") +
        theme(axis.title.y= element_text(angle=90), axis.ticks.x= element_blank(),
              panel.background= element_blank(), axis.line= element_line(color= 'grey'))
      p2
    }

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