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
library(timevis)
library(sf)
library(tmap)
library(clock)
library(sftime)
library(visNetwork)
library(tidygraph)
library(igraph)
library(ggraph)
library(graphlayouts)
library(ggstatsplot)
library(ggcorrplot)


# packages = c('tidyverse', 'sf', 'tmap', 'lubridate', 'clock', 'sftime', 'rmarkdown', 'plotly')
# 
# for (p in packages){
#   if(!require(p, character.only = T)){
#     install.packages(p)
#   }
# }


## build ui.R -----------------------------------
## 1. header -------------------------------
header <- 
  dashboardHeader( title = HTML("City of Engagament"))



## 2. siderbar ------------------------------
siderbar <- 
  dashboardSidebar(
    sidebarMenu(id = 'sidebarmenu',
                menuItem("Demographics analysis", 
                         tabName = "demographics_tab", startExpanded = FALSE,
                           menuSubItem("Overall DemoGraphic", tabName = "overall_demo_analysis"),
                           menuSubItem("Wage analysis", tabName = "wage_analysis"), # TO-DO
                         menuSubItem("Joviality analysis", tabName = "jov_analysis") # TO-DO
                ),
                menuItem("Social activity", tabName = "social_activity_tab"),
                menuItem("Predominant business", tabName = "predominant_business_tab", startExpanded = FALSE,
                         menuSubItem("Overall Town Map", tabName = "townmap_tab"),
                         menuSubItem("Map by Venue Type", tabName = "venuetype_tab"),
                         menuSubItem("Check-in Analysis", tabName = "checkin_tab"),
                         menuSubItem("TBC", tabName = "xxx_tab")
                )
    )
  )

## 3. body --------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "overall_demo_analysis",
      h2("Demographics analysis content"),
      fluidRow(
        valueBoxOutput('wage'),
        valueBoxOutput('age'),
        valueBoxOutput('education'),
        box(plotlyOutput("kids_plot")),
        box(plotlyOutput("wage_edu_plot")),
        box(plotOutput('jov_education_plot')),
        box(plotOutput('corplot'))
        # box(plotlyOutput('statsTest_plot'))
      )
    ),
    tabItem( # TO-DO
      tabName = "wage_analysis",
      h2("Demographics analysis content"),
      fluidRow(
        valueBoxOutput('wage'),
        box(plotlyOutput("wage_edu_plot")),
        box(plotOutput('corplot'))
        # box(plotlyOutput('statsTest_plot'))
      )
    ),
    tabItem(tabName = "social_activity_tab",
            fluidPage(
              h2("Social activity content"),
              visNetworkOutput("network")
            )
    ),
    
    tabItem(tabName = "townmap_tab",
            fluidPage(
              titlePanel("Overall Town Map"),
              sidebarLayout(
                sidebarPanel(
                  
                  
                ),
                mainPanel(
                  h4("Map for all buildings and locations"),
                  tmapOutput(outputId = "mapPlotAll", width = 800, height = 400),
                  plotOutput(outputId = "mapPlotAreas", width = 800, height = 400)
                )
              )
            )
    ),
    
    tabItem(tabName = "venuetype_tab",
            fluidPage(
              titlePanel("Map and Attributes by Venue Type"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "locationType", 
                              label = "Location Type:",
                              choices = c("Apartments" = "Apartments",
                                          "Employers/Jobs" = "Employers",
                                          "Pubs" = "Pubs",
                                          "Restaurants" = "Restaurants",
                                          "Schools" = "Schools"),
                              multiple = FALSE)
                  
                ),
                mainPanel(
                  h4("Map for selected location type"),
                  tmapOutput(outputId = "mapPlotbyType", width = 800, height = 800)
                )
              )
            )
    ),
    
    tabItem(tabName = "checkin_tab",
            fluidPage(
              titlePanel("Check-in Analysis for Pubs and Restaurants"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "checkinType", 
                              label = "Check-in Location:",
                              choices = c("Pubs" = "Pubs",
                                          "Restaurants" = "Restaurants",
                                          "Workplaces" = "Workplaces"),
                              multiple = FALSE),
                  sliderInput(inputId = "checkinDate",
                              label = "Date Range",
                              min = as.Date("2022-03-01","%Y-%m-%d"),
                              max = as.Date("2023-05-25","%Y-%m-%d"),
                              value=c(as.Date("2022-03-01"), as.Date("2023-05-25")),
                              timeFormat="%Y-%m-%d")
                ),
                mainPanel(
                  h4("Check-in trends for selected venue type"),
                  plotlyOutput("checkinPlot", width = 800, height = 800)
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
participants_data <- read_rds('data/participants.rds')

financeJ <- read_csv(file = "data/FinancialJournal.csv")

schools <- read_sf("data/Schools.csv", 
                   options = "GEOM_POSSIBLE_NAMES=location")

buildings <- read_sf("data/Buildings.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")

pubs <- read_sf("data/Pubs.csv", 
                options = "GEOM_POSSIBLE_NAMES=location")

apartments <- read_sf("data/Apartments.csv", 
                      options = "GEOM_POSSIBLE_NAMES=location")

employers <- read_sf("data/Employers.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")

restaurants <- read_sf("data/Restaurants.csv", 
                       options = "GEOM_POSSIBLE_NAMES=location")

jobs <- read_csv("data/Jobs.csv")

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

checkin_journal <- read_csv("data/CheckinJournal.csv")
checkin_journal$timestamp <- as.Date(checkin_journal$timestamp, "%Y-%m-%d")


network_nodes <- read_csv("data/Participants.csv")
network_edges <- read_csv("data/SocialNetwork.csv")

buildings_shp <- read_sf("data/buildings.shp", 
                         options = "GEOM_POSSIBLE_NAMES=location")

## End of Data Import

## Start of data processing
participants_data_ag_haveKids <- participants_data %>%
  filter(`haveKids` ==  TRUE) %>%
  mutate (householdSize = -householdSize)


participants_data_ag_noKids <-participants_data %>%
  filter(`haveKids` ==  FALSE)

participants_data_ag_byKids <- rbind(participants_data_ag_haveKids, participants_data_ag_noKids)

network_nodes <- network_nodes %>%
  mutate(participantId = participantId +1)

network_edges <- network_edges %>%
  mutate(source = source +1) %>%
  mutate(target =  target +1)

network_edges <- network_edges %>%
  mutate(Weekday = wday(timestamp,
                        label = TRUE,
                        abbr = FALSE))%>%
  mutate(YearMonth = format(timestamp,'%Y-%m'))

network_edges <- rename(network_edges,
                        from = source,
                        to = target)

network_edges_aggregated <- network_edges %>%
  filter(YearMonth == "2022-03") %>%
  group_by(from, to) %>%
  summarise(Weight = n()) %>%
  filter(from!=to) %>%
  filter(Weight > 20) %>%
  ungroup()

node_from <- pull(network_edges_aggregated, from)
node_to <- pull(network_edges_aggregated, to)
nodes_distinct <- unique(append(node_from, node_to))
network_nodes <- filter(network_nodes, participantId %in% nodes_distinct)
network_nodes <- network_nodes %>%
  rename(id = participantId)


## End of data processing

server <- function(input, output){
  ## Start of Section 1
  valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                            icon = NULL, color = "aqua", width = 4, href = NULL){
    
    shinydashboard:::validateColor(color)
    
    if (!is.null(icon))
      shinydashboard:::tagAssert(icon, type = "i")
    
    info_icon <- tags$small(
      tags$i(
        class = "fa fa-info-circle fa-lg",
        title = info,
        `data-toggle` = "tooltip",
        style = "color: rgba(255, 255, 255, 0.75);"
      ),
      # bs3 pull-right 
      # bs4 float-right
      class = "pull-right float-right"
    )
    
    boxContent <- div(
      class = paste0("small-box bg-", color),
      div(
        class = "inner",
        tags$small(title),
        if (!is.null(sparkobj)) info_icon,
        h3(value),
        if (!is.null(sparkobj)) sparkobj,
        p(subtitle)
      ),
      # bs3 icon-large
      # bs4 icon
      if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
    )
    
    if (!is.null(href)) 
      boxContent <- a(href = href, boxContent)
    
    div(
      class = if (!is.null(width)) paste0("col-sm-", width), 
      boxContent
    )
  }
  # joviality_plot <- ggplot(data=participants_data,
  #              aes(x = educationLevel, y = joviality)) + geom_boxplot(notch=TRUE)+
  #   stat_summary(geom = "point",
  #                fun="mean",
  #                colour="red",
  #                size=4) +
  #   ggtitle("Distribution of Juviality for different Education Level")
  wage_edu_plot <- ggplot(participants_data, aes(x = wage, fill = educationLevel)) + 
    geom_histogram(data=participants_data, alpha=.5) +
    geom_histogram() +
    labs(x = "Wage", title = "Participants'wage wih different Education Level")+
    facet_wrap(~ educationLevel) + 
    guides(fill = "none") + 
    theme_bw()
  kids_plot <- ggplot(participants_data_ag_byKids, aes (x = ageGroup, y = participantId , fill = haveKids)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(name = "Count")+
    labs(x = "Age Group", title = "Participants'num by age groups and whether have kids")+
    theme_bw()
  set.seed(1234)
  anova_education_plot <- ggbetweenstats(
    data = participants_data,
    outlier.tagging = TRUE, ## whether outliers should be flagged
    outlier.label = participantId, ## label to attach to outlier values
    outlier.label.args = list(color = "red"), ## outlier point label color
    ## turn off messages
    ggtheme = ggplot2::theme_gray(), ## a different theme
    package = "yarrr", ## package from which color palette is to be take
    palette = "info2", ## choosing a different color palette
    title = "Jovality in different education level",
    caption = "Source: VAST Challenge",
    x = educationLevel,
    y = joviality,
    type = "robust", ## type of statistics
    xlab = "Education Level", ## label for the x-axis
    ylab = "Social Interactions", ## label for the y-axis
    plot.type = "boxviolin", ## type of plot
  )
  jov_cor_plot <- ggcorrmat(
    data     = participants_data[c("age",'joviality','wage')],
    colors   = c("#B2182B", "white", "#4D4D4D"),
    title    = "Correlalogram for participants' data",
    subtitle = "Wage:-Joviality;"
  )
  wage_hc <- hchart(participants_data, "area", hcaes(participantId, round(wage,2)), name = "wage")  %>% 
    hc_size(height = 100) %>% 
    hc_credits(enabled = FALSE) %>% 
    hc_add_theme(hc_theme_sparkline_vb()) 
  wage_vb <- valueBoxSpark(
    value = paste0('$',round(mean(participants_data$wage)/1000,2), "K/month"),
    title = toupper("Monthly Wage of participants in Ohio City"),
    sparkobj = wage_hc,
    subtitle = tagList(HTML("&uarr;"), "25% Since last year"),
    info = "This is the monthly wage of all participants in Ohio City",
    icon = NULL,
    width = 4,
    color = "teal",
    href = NULL
  )
  ageHist <- participants_data %>%
    group_by(ageGroup) %>%
    mutate(count = n()) %>%
    arrange(ageGroup)
  age_hc <- hchart(ageHist, "column", hcaes(ageGroup,count), name = "No. People")  %>% 
    hc_size(height = 100) %>% 
    hc_credits(enabled = FALSE) %>% 
    hc_add_theme(hc_theme_sparkline_vb()) 
  age_vb <- valueBoxSpark(
    value = paste0(round(mean(participants_data$age),0), "yrs old"),
    title = toupper("Age of participants in Ohio City"),
    sparkobj = age_hc,
    subtitle = tagList("Most people are in their 20-60"),
    info = "Most people in Ohio city are in their working age",
    icon = NULL,
    width = 4,
    color = "red",
    href = NULL
  )
  educationHist <- participants_data %>%
    group_by(educationLevel) %>%
    mutate(count = n()) %>%
    arrange(educationLevel)
  education_hc <- hchart(educationHist, "column", hcaes(educationLevel,count), name = "No. People")  %>% 
    hc_size(height = 100) %>% 
    hc_credits(enabled = FALSE) %>% 
    hc_add_theme(hc_theme_sparkline_vb()) 
  education_vb <- valueBoxSpark(
    value = "HighShool or College Degree",
    title = toupper("Education Level of participants in Ohio City"),
    sparkobj = education_hc,
    subtitle = tagList("Most people have HighSchool or College degree"),
    info = "Most people in Ohio city have HighSchool or College degree",
    icon = NULL,
    width = 4,
    color = "blue",
    href = NULL
  )
  
  
  output$wage_edu_plot <- renderPlotly({
    wage_edu_plot
  })
  output$kids_plot <- renderPlotly({
    kids_plot
  })
  output$joviality_plot <- renderPlotly({
    joviality_plot
  })
  output$jov_education_plot <- renderPlot({
    anova_education_plot
  })
  output$corplot <- renderPlot(jov_cor_plot)
  output$wage <- renderValueBox(wage_vb)
  output$age <- renderValueBox(age_vb)
  output$education <- renderValueBox(education_vb)
  
  ## End of Section 1
  
  
  ## Start of Section 2
  #output$social_act_table <- renderDataTable(mtcars)
  output$network <- renderVisNetwork({
    visNetwork(network_nodes,
               network_edges_aggregated) %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = TRUE,
                 selectedBy = "joviality") %>%
      visLegend() %>%
      visLayout(randomSeed = 1234)
  })
  
  
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
  
  output$mapPlotAreas <- renderPlot({
    ggplot(buildings_shp)+
      geom_sf(aes(fill = region),
              color = "black",
              size = 0.1,
              show.legend = TRUE) +
      coord_sf()+
      theme_bw()+
      labs(title = "Geographical region of the study area")
    
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
  
  output$checkinPlot <- renderPlotly({
    if (input$checkinType == "Restaurants"){
      checkin_journal_rest <- checkin_journal %>%
        filter(`venueType` == "Restaurant" & `timestamp` >= input$checkinDate[1] & `timestamp` <= input$checkinDate[2])
      #print(input$checkinDate[1])
      #print(input$checkinDate[2])
      checkin_rest <- checkin_journal_rest %>%
        group_by(`venueId`, `timestamp`) %>%
        summarise('checkins' = n()) %>%
        ungroup()
      checkin_rest$venueId <- as.character(checkin_rest$venueId)
      p1 <- ggplot(data=checkin_rest, 
                   aes(x = timestamp,
                       y = checkins, 
                       color=venueId
                   )) +
        labs(x ="Month-Year", y = "No. of Check-ins", 
             title = "No. of check-ins for restaurants") +
        geom_line()
      
      ggplotly(p1)
    }
    
    else if (input$checkinType == "Pubs"){
      checkin_journal_pub <- checkin_journal %>%
        filter(`venueType` == "Pub" & `timestamp` >= input$checkinDate[1] & `timestamp` <= input$checkinDate[2])
      print(input$checkinDate[1])
      print(input$checkinDate[2])
      checkin_pub <- checkin_journal_pub %>%
        group_by(`venueId`, `timestamp`) %>%
        summarise('checkins' = n()) %>%
        ungroup()
      checkin_pub$venueId <- as.character(checkin_pub$venueId)
      p2 <- ggplot(data=checkin_pub, 
                   aes(x = timestamp,
                       y = checkins, 
                       color=venueId
                   )) +
        labs(x ="Month-Year", y = "No. of Check-ins", 
             title = "No. of check-ins for pubs") +
        geom_line()
      
      ggplotly(p2)
      
    }
    
    
  })
  ## End of Section 3
  
}

shinyApp(ui = ui, server = server)