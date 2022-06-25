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
library(reactable)
library(reactablefmtr)
library(gt)
library(gtExtras)
library(ggthemes)



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
                         menuSubItem("Joviality analysis", tabName = "jov_analysis"), # TO-DO
                         menuSubItem("Consume analysis", tabName = "consume_analysis")
                ),
                menuItem("Social activity", tabName = "social_activity_tab", startExpanded = FALSE,
                         menuSubItem("Overall Network Graph", tabName = "network_tab"),
                         menuSubItem("Network Among Different Groups", tabName = "group_tab"),
                         menuSubItem("visnetwork Graph", tabName = "vis_tab")
                ),
                menuItem("Predominant business", tabName = "predominant_business_tab", startExpanded = FALSE,
                         menuSubItem("Overall Town Map", tabName = "townmap_tab"),
                         menuSubItem("Cost Analysis", tabName = "venuetype_tab"),
                         menuSubItem("Check-in Analysis", tabName = "checkin_tab"),
                         menuSubItem("Revenue Analysis", tabName = "revenue_tab")
                )
    )
  )

## 3. body --------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "overall_demo_analysis",
      h2("The overall view of demographics in Ohio City"),
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
    tabItem(
      tabName = 'consume_analysis',
      h2("Consume analysis"),
      h4('What is influencing people\'s consume'),
      fluidRow(
        box(
          DT::dataTableOutput("values")
        )
      )
    ),
    tabItem( # TO-DO
      tabName = "wage_analysis",
      h2("Wage Analysis"),
      h4('What is influencing people\'s wage'),
      fluidRow(
        box(
          sliderInput(inputId = "wageDate",
                      label = "Date Range",
                      min = as.Date("2022-03-01","%Y-%m-%d"),
                      max = as.Date("2023-05-25","%Y-%m-%d"),
                      value=c(as.Date("2022-03-01"), as.Date("2023-05-25")),
                      timeFormat="%Y-%m-%d"),
          plotOutput("wage_ana_plot")
        ),
        box(
          selectInput(inputId = "wage_factor", 
                      label = "Factor that may influence Wage",
                      choices = c(
                        "Education Level" = "educationLevel",
                        "Interest Group" = "interestGroup",
                        "Age Group" = "ageGroup"),
                      multiple = FALSE)
        )
        )
    ),
    tabItem(tabName = "network_tab",
            fluidPage(
              titlePanel("social network"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "yearMonth", 
                              label = "yearMonth",
                              choices = c("2022-03" = "2022-03",
                                          "2023-03" = "2023-03"),
                                          multiple = FALSE)
                  ),
                mainPanel(
                  h2("Social network"),
                  plotOutput(outputId = "network_plot",width = 800, height = 400)
                )
              )
            )
    ),
    tabItem(tabName = "group_tab",
            fluidPage(
              titlePanel("social network of groups"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "yearMonth", 
                              label = "yearMonth",
                              choices = c("2022-03" = "2022-03",
                                          "2023-03" = "2023-03"),
                              multiple = FALSE),
                  selectInput(inputId = "groups", 
                              label = "group",
                              choices = c("educationLevel" = "educationLevel",
                                          "interestGroup" = "interestGroup",
                                          "joviality" = "joviality"),
                              multiple = FALSE)
                ),
                mainPanel(
                  h2("Social network of Different Groups"),
                  plotOutput(outputId = "network_group")
            )
          )
        )
      ),
    tabItem(tabName = "vis_tab",
            fluidPage(
              h2("Social network"),
              visNetworkOutput("network")
            )
    ),
    tabItem(tabName = "townmap_tab",
            titlePanel("Overall Town Map"),
            fluidRow(
              valueBoxOutput('s3_buildings', width = 2),
              valueBoxOutput('s3_apartments', width = 2),
              valueBoxOutput('s3_employers', width = 2),
              valueBoxOutput('s3_pubs', width = 2),
              valueBoxOutput('s3_restaurants', width = 2),
              valueBoxOutput('s3_schools', width = 2),
              box( h4("Geographical region of the city"), plotlyOutput(outputId = "mapPlotAreas", height = 300)),
              box(h4("Building type of the city"), tmapOutput(outputId = "mapPlotAll", height = 300)),
              
            ),
            fluidRow(
              box(
                selectInput(inputId = "venueTypeSelected", 
                            label = "Please select venue type to be shown in the map:",
                            choices = c("All" = "All",
                                        "Apartments" = "Apartments",
                                        "Employers" = "Employers",
                                        "Pubs" = "Pubs",
                                        "Restaurants" = "Restaurants",
                                        "Schools" = "Schools"),
                            multiple = FALSE)
              ),
              
              box(h4("Selected venue type of the city"), tmapOutput(outputId = "mapPlotAllDetails", height = 300))
              
            ),
            
    ),
    
    tabItem(tabName = "venuetype_tab",
            fluidPage(
              titlePanel("Cost Analysis by Venue Type"),
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
                  
                  HTML('<hr style="color: black;">'),
                  helpText("Please select below fields for a statistical test on selected variables."),
                  selectInput(inputId = "s3_xvariable",
                              label = "Select x-variable:",
                              choices = c("Region" = "Region"),
                              selected = "Region"),
                  selectInput(inputId = "s3_yvariable",
                              label = "Select y-variable:",
                              choices = c("Apartment Rental Cost" = "rentalCost",
                                          "Job Hourly Rate" = "jobRate",
                                          "Pub Hourly Cost" = "pubCost",
                                          "Restaurant Food Cost" = "foodCost",
                                          "School Monthly Cost" = "schoolCost"),
                              selected = "rentalCost"),
                  selectInput(inputId = "s3_test",
                              label = "Type of statistical test:",
                              choices = c("nonparametric" = "np",
                                          "parametric" = "p",
                                          "robust" = "r",
                                          "Bayes Factor" = "bf"),
                              selected = "np"),
                  selectInput(inputId = "s3_plotType",
                              label = "Type of plot:",
                              choices = c("boxviolin" = "boxviolin",
                                          "box" = "box",
                                          "violin" = "violin"),
                              selected = "boxviolin"),
                  
                ),
                fluidRow(
                  box(h4("Map for selected venue type with cost details"), tmapOutput(outputId = "mapPlotbyType", width = 780, height = 360)),
                  box(h4("Statistical test"), plotOutput(outputId = "costTestPlot",width = 780, height = 360)),
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
ui <- dashboardPage(skin = "green",
                    header, 
                    siderbar, 
                    body )

## Start of Data Import
participants_data <- read_rds('data/participants.rds')
participants <- read_csv("data/Participants.csv")
consume_report <- read_rds('data/consume_report.rds')
financeJ <- read_rds(file = "data/financeJ.rds")

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

network_edges_aggregated_2022 <- read_rds("data/network_edges_aggregated_2022.rds")
network_edges_aggregated_2023 <- read_rds("data/network_edges_aggregated_2023.rds")

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

#node_from <- pull(network_edges_aggregated, from)
#node_to <- pull(network_edges_aggregated, to)
#nodes_distinct <- unique(append(node_from, node_to))
#network_nodes <- filter(network_nodes, participantId %in% nodes_distinct)
#network_nodes <- network_nodes %>%
#  rename(id = participantId)

network_nodes_2022<- data.frame(participantId = 
                                  c(network_edges_aggregated_2022$from, 
                                    network_edges_aggregated_2022$to)) %>% 
  unique()
network_nodes_2022<- merge(x = network_nodes_2022, y = network_nodes, by = "participantId", all.x = TRUE)

network_nodes_2023<- data.frame(participantId = 
                                  c(network_edges_aggregated_2023$from, 
                                    network_edges_aggregated_2023$to)) %>% 
  unique()
network_nodes_2023<- merge(x = network_nodes_2023, y = network_nodes, by = "participantId", all.x = TRUE)


network_graph_2022 <- graph_from_data_frame(network_edges_aggregated_2022, 
                                            vertices = network_nodes_2022,
                                            directed = TRUE) %>%
  as_tbl_graph()

network_graph_2023 <- graph_from_data_frame(network_edges_aggregated_2023, 
                                            vertices = network_nodes_2023,
                                            directed = TRUE) %>%
  as_tbl_graph()

network_graph_2022 %>%
  activate(edges) %>%
  arrange(desc(Weight))
network_graph_2023 %>%
  activate(edges) %>%
  arrange(desc(Weight))


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
    subtitle = "The mean wage is 3.8K/monnth",
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
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Timeline"),
      Value = as.character(c(input$Month),
                           stringsAsFactors = FALSE)
    )
    
  })
  output$values <- DT::renderDataTable({
      dt <-  DT::datatable(as.data.frame(consume_report %>%
                                           group_by(category) %>%
                                           summarize('Monthly Consume' = list(consume), 
                                                     .groups = "drop") %>%
                                           gt() %>%
                                           gt_plt_sparkline('Monthly Consume')),  rownames = FALSE)
  })
  output$wage_ana_plot <- renderPlotly({
    wage_ana_plot
  })
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
  output$wage_ana_plot <- renderPlot({
    ggbetweenstats(
      data = financeJ %>%
        filter(`timestamp` >= input$wageDate[1] & `timestamp` <= input$wageDate[2],category=='Wage') %>%
        group_by(participantId) %>%
        summarise(Wage = mean(amount)) %>%
        ungroup() %>%
        merge(participants,by='participantId')
      ,
      outlier.tagging = TRUE, ## whether outliers should be flagged
      outlier.label = participantId, ## label to attach to outlier values
      outlier.label.args = list(color = "red"), ## outlier point label color
      ## turn off messages
      ggtheme = ggplot2::theme_gray(), ## a different theme
      package = "yarrr", ## package from which color palette is to be take
      palette = "info2", ## choosing a different color palette
      title = paste("Wage for different",input$wage_factor),
      caption = "Source: VAST Challenge",
      x = !!input$wage_factor,
      y = Wage,
      type = "robust", ## type of statistics
      xlab = "Education Level", ## label for the x-axis
      ylab = "Wage", ## label for the y-axis
      plot.type = "boxviolin", ## type of plot
    )
       })
  ## End of Section 1
  
  
  ## Start of Section 2
  #output$social_act_table <- renderDataTable(mtcars)
  output$network_plot <- renderPlot({
    if (input$yearMonth == "2022-03"){
      set.seed(1234)
      network_plot<- ggraph(network_graph_2022,layout = "nicely") + 
        geom_edge_link(aes(width=Weight), alpha=0.25) +
        scale_edge_width(range = c(0.1, 1)) +
        geom_node_point()
      network_plot + theme_graph()
    }
   else if (input$yearMonth == "2023-03"){
      network_plot<- ggraph(network_graph_2023,layout = "nicely") + 
        geom_edge_link(aes(width=Weight), alpha=0.25) +
        scale_edge_width(range = c(0.1, 1)) +
        geom_node_point() 
      network_plot + theme_graph()
   }
  })
  
  
  output$network_group <- renderPlot({
    if(input$yearMonth =="2022-03"){
    if (input$groups =="educationLevel"){
      network_group <- ggraph(network_graph_2022, layout = "nicely") + 
        geom_edge_link(aes(width=Weight), alpha=0.2) +
        scale_edge_width(range = c(0.1, 1)) +
        geom_node_point(aes(colour = educationLevel), size = 1)
      network_group + theme_graph()
    }
    else if (input$groups =="interestGroup"){
      network_group <- ggraph(network_graph_2022, layout = "nicely") + 
        geom_edge_link(aes(width=Weight), alpha=0.2) +
        scale_edge_width(range = c(0.1, 1)) +
        geom_node_point(aes(colour = interestGroup), size = 1)
      network_group + theme_graph()
    }
    else if (input$groups =="joviality"){
      network_group <- ggraph(network_graph_2022, layout = "nicely") + 
        geom_edge_link(aes(width=Weight), alpha=0.2) +
        scale_edge_width(range = c(0.1, 1)) +
        geom_node_point(aes(colour = joviality), size = 1)
      network_group + theme_graph()
    }
    }
    else if (input$yearMonth =="2023-03"){
    if (input$groups =="educationLevel" ){
        network_group <- ggraph(network_graph_2023, layout = "nicely") + 
          geom_edge_link(aes(width=Weight), alpha=0.2) +
          scale_edge_width(range = c(0.1, 1)) +
          geom_node_point(aes(colour = educationLevel), size = 1)
        network_group + theme_graph()
      }
      else if (input$groups =="interestGroup"){
        network_group <- ggraph(network_graph_2023, layout = "nicely") + 
          geom_edge_link(aes(width=Weight), alpha=0.2) +
          scale_edge_width(range = c(0.1, 1)) +
          geom_node_point(aes(colour = interestGroup), size = 1)
        network_group + theme_graph()
      }
      else if (input$groups =="joviality"){
        network_group <- ggraph(network_graph_2023, layout = "nicely") + 
          geom_edge_link(aes(width=Weight), alpha=0.2) +
          scale_edge_width(range = c(0.1, 1)) +
          geom_node_point(aes(colour = joviality), size = 1)
        network_group + theme_graph()
      }
    }
  })
  
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
  buildings_summary <- valueBoxSpark(
    value = paste0("", nrow(buildings)),
    title = toupper("Total no. of buildings in the city"),
    subtitle = "",
    icon = NULL,
    #width = 2,
    color = "blue",
    href = NULL
  )
  apartments_summary <- valueBoxSpark(
    value = paste0("", nrow(apartments)),
    title = toupper("Total no. of apartments in the city"),
    subtitle = "",
    icon = NULL,
    color = "teal",
    href = NULL
  )
  employers_summary <- valueBoxSpark(
    value = paste0("", nrow(employers)),
    title = toupper("Total no. of employers in the city"),
    subtitle = "",
    icon = NULL,
    color = "teal",
    href = NULL
  )
  restaurants_summary <- valueBoxSpark(
    value = paste0("", nrow(restaurants)),
    title = toupper("Total no. of restaurants in the city"),
    subtitle = "",
    icon = NULL,
    color = "teal",
    href = NULL
  )
  pubs_summary <- valueBoxSpark(
    value = paste0("", nrow(pubs)),
    title = toupper("Total no. of pubs in the city"),
    subtitle = "",
    icon = NULL,
    color = "teal",
    href = NULL
  )
  schools_summary <- valueBoxSpark(
    value = paste0("", nrow(schools)),
    title = toupper("Total no. of schools in the city"),
    subtitle = "",
    icon = NULL,
    color = "teal",
    href = NULL
  )
  output$s3_buildings <- renderValueBox(buildings_summary)
  output$s3_apartments <- renderValueBox(apartments_summary)
  output$s3_employers <- renderValueBox(employers_summary)
  output$s3_restaurants <- renderValueBox(restaurants_summary)
  output$s3_pubs <- renderValueBox(pubs_summary)
  output$s3_schools <- renderValueBox(schools_summary)
  
  output$mapPlotAllDetails <- renderTmap({
    tmap_mode("view")
    mapAll <- tm_shape(buildings)+
      tm_polygons(
        palette="Accent",
        border.col = "black",
        border.alpha = .5,
        border.lwd = 0.5)
    if (input$venueTypeSelected == "All"){
      mapAll <- tm_shape(buildings)+
        tm_polygons(
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) + 
        tm_shape(restaurants) +
        tm_dots(col = "blue") +
        tm_shape(employers) +
        tm_dots(col = "red") +
        tm_shape(apartments) +
        tm_dots(col = "lightblue") +
        tm_shape(pubs) +
        tm_dots(col = "green") +
        tm_shape(schools) +
        tm_dots(col = "yellow")
    }
    
    if (input$venueTypeSelected == "Restaurants"){
      mapAll <- tm_shape(buildings)+
        tm_polygons(
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) + 
        tm_shape(restaurants) +
        tm_dots(col = "blue")
    }
    
    if (input$venueTypeSelected == "Employers"){
      mapAll <- tm_shape(buildings)+
        tm_polygons(
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) + 
        tm_shape(employers) +
        tm_dots(col = "red")
    }
    
    if (input$venueTypeSelected == "Apartments"){
      mapAll <- tm_shape(buildings)+
        tm_polygons(
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) + 
        tm_shape(apartments) +
        tm_dots(col = "lightblue")
    }
    
    if (input$venueTypeSelected == "Pubs"){
      mapAll <- tm_shape(buildings)+
        tm_polygons(
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) + 
        tm_shape(pubs) +
        tm_dots(col = "green")
    }
    
    if (input$venueTypeSelected == "Schools"){
      mapAll <- tm_shape(buildings)+
        tm_polygons(
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) + 
        tm_shape(schools) +
        tm_dots(col = "yellow")
    }
    mapAll
    
  })
  
  output$mapPlotAll <- renderTmap({
    tmap_mode("view")
    tm_shape(buildings)+
      tm_polygons(col = "buildingType",
                  palette="Accent",
                  border.col = "black",
                  border.alpha = .5,
                  border.lwd = 0.5)
  })
  
  output$mapPlotAreas <- renderPlotly({
    mapAreas <- ggplot(buildings_shp)+
      geom_sf(aes(fill = region),
              color = "black",
              size = 0.1,
              show.legend = TRUE) +
      coord_sf()+
      theme(
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_line(colour = "transparent")
      )
    #theme_bw()
    ggplotly(mapAreas)
    
  })
  
  output$mapPlotbyType <- renderTmap({
    if (input$locationType == "Apartments"){
      tmap_mode("view")
      tm_shape(buildings_shp)+
        tm_polygons(col = "region",
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
      tm_shape(buildings_shp)+
        tm_polygons(col = "region",
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
      tm_shape(buildings_shp)+
        tm_polygons(col = "region",
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
      tm_shape(buildings_shp)+
        tm_polygons(col = "region",
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
      tm_shape(buildings_shp)+
        tm_polygons(col = "region",
                    palette="Accent",
                    border.col = "black",
                    border.alpha = .5,
                    border.lwd = 0.5)+
        tm_shape(jobs_employers)+
        tm_bubbles(col = "hourlyRate",
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
  
  output$costTestPlot <-renderPlot({
    set.seed(1234)
    buildings_shp <- buildings_shp %>%
      rename(buildingId = bldngId)
    buildings_df <- buildings_shp%>% as.data.frame()
    
    if (input$s3_yvariable == "rentalCost"){
      apartments_df <- apartments%>% as.data.frame()
      joined_data <- left_join(apartments_df, buildings_df, by = "buildingId")
      ggbetweenstats(
        data = joined_data,
        x = "region",
        y = "rentalCost",
        title = "Statistical test on apartment rental cost and region",
        type = input$s3_test,
        plot.type = input$s3_plotType,
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE)
    }
    
    else if (input$s3_yvariable == "pubCost"){
      pubs_df <- pubs%>% as.data.frame()
      joined_data <-  left_join(pubs_df, buildings_df, by = "buildingId")
      ggbetweenstats(
        data = joined_data,
        x = "region",
        y = "hourlyCost",
        title = "Statistical test on pub cost and region",
        type = input$s3_test,
        plot.type = input$s3_plotType,
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE)
    }
    
    else if (input$s3_yvariable == "foodCost"){
      restaurants_df <- restaurants%>% as.data.frame()
      joined_data <-  left_join(restaurants_df, buildings_df, by = "buildingId")
      ggbetweenstats(
        data = joined_data,
        x = "region",
        y = "foodCost",
        title = "Statistical test on restaurant food cost and region",
        type = input$s3_test,
        plot.type = input$s3_plotType,
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE)
    }
    
    else if (input$s3_yvariable == "schoolCost"){
      schools_df <- schools%>% as.data.frame()
      joined_data <-  left_join(schools_df, buildings_df, by = "buildingId")
      ggbetweenstats(
        data = joined_data,
        x = "region",
        y = "monthlyCost",
        title = "Statistical test on school cost and region",
        type = input$s3_test,
        plot.type = input$s3_plotType,
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE)
    }
    
    else if (input$s3_yvariable == "jobRate"){
      jobs_employers_df <- jobs_employers%>% as.data.frame()
      joined_data <-  left_join(jobs_employers_df, buildings_df, by = "buildingId")
      ggbetweenstats(
        data = joined_data,
        x = "region",
        y = "hourlyRate",
        title = "Statistical test on job hourly rate and region",
        type = input$s3_test,
        plot.type = input$s3_plotType,
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE)
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