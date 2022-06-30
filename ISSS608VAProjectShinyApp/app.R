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
library(ggrepel)
library(formattable)
library(magrittr)
library(ggpol)
library(ggridges)
library(zoo)

# packages = c('tidyverse', 'sf', 'tmap', 'lubridate', 'clock', 'sftime', 'rmarkdown', 'plotly')
# 
# for (p in packages){
#   if(!require(p, character.only = T)){
#     install.packages(p)
#   }
# }
true_false_formatter <-
  formatter("span",
            style = x ~ style(
              font.weight = "bold",
              color = ifelse(x == TRUE, "forestgreen", ifelse(x == FALSE, "red", "black"))
            ))


## build ui.R -----------------------------------
## 1. header -------------------------------
header <- 
  dashboardHeader( title = HTML("City of Engagament"))



## 2. siderbar ------------------------------
siderbar <- 
  dashboardSidebar(
    sidebarMenu(id = 'sidebarmenu',
                menuItem("Demographics Analysis", 
                         tabName = "demographics_tab", startExpanded = FALSE, icon = icon("tachometer-alt"),
                         menuSubItem("Overall Demographics", tabName = "overall_demo_analysis"),
                         menuSubItem("Wage Analysis", tabName = "wage_analysis"), # TO-DO
                         menuSubItem("Expenditure Analysis", tabName = "expend_analysis") # TO-DO
                         #menuSubItem("Consume analysis", tabName = "consume_analysis")
                ),
                menuItem("Social Activity", tabName = "social_activity_tab", startExpanded = FALSE, icon = icon("users"),
                         menuSubItem("Overall Social Network", tabName = "network_tab"),
                         menuSubItem("Network by Group", tabName = "group_tab"),
                         menuSubItem("Network by Individual", tabName = "vis_tab")
                ),
                menuItem("Predominant Business", tabName = "predominant_business_tab", startExpanded = FALSE, icon = icon("building"),  
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
      h2("The overall view of demographics in City of Engagement, Ohio USA"),
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
    tabItem( # TO-DO'
      tabName = "wage_analysis",
      fluidPage(
        h2("Wage Analysis"),
        tabsetPanel(
          tabPanel("Exploratory Data Analysis (EDA)", 
                   fluidRow(
                     box(
                       title = "Wage Distribution in Different Groups", 
                       solidHeader = TRUE, 
                       status = "primary",
                       width = 6, # Half of the body
                       selectInput(
                         inputId = "wage_boxplot_var",
                         label = "Group by",
                         choices = c(
                           "Education Level" = "educationLevel",
                           "Interest Group" = "interestGroup",
                           "Joviality Status" = "jovialityGroup",
                           "Age Group" = "ageGroup",
                           "Have Kids" = "haveKids"
                         ),
                         multiple = FALSE),
                       plotlyOutput("wage_distribution",width="90%")
                     ),
                     box(width = 6, # Half of the body
                         title='What is influencing people\'s wage',
                         solidHeader = TRUE, 
                         status = "primary",
                         sliderInput(inputId = "wageDate",
                                     label = "Date Range",
                                     min = as.Date("2022-03-01","%Y-%m-%d"),
                                     max = as.Date("2023-05-25","%Y-%m-%d"),
                                     value=c(as.Date("2022-03-01"), as.Date("2023-05-25")),
                                     timeFormat="%Y-%m-%d"),
                         selectInput(inputId = "wage_factor", 
                                     label = "Factor that may influence Wage",
                                     choices = c(
                                       "Education Level" = "educationLevel",
                                       "Interest Group" = "interestGroup",
                                       "Age Group" = "ageGroup",
                                       "Joviality Status" = "Joviality_Group"),
                                     multiple = FALSE),
                         plotOutput("wage_ana_plot",width="100%")
                     )
                   ),
                   fluidRow(
                     box(width = 12,
                         title = "Predict participant\'s wage",
                         solidHeader = TRUE,
                         status='primary',
                         column(width = 4,
                                selectInput(inputId = "par_educationLevel", 
                                            label = "Education Level:",
                                            choices = c(
                                              #"Any Status" = "Any Status",
                                              "Low" = "Low",
                                              "High School or College" = "HighSchoolOrCollege",
                                              "Bachelors" = "Bachelors",
                                              "Graduate" = "Graduate"),
                                            multiple = FALSE),
                                selectInput(inputId = "par_ageGroup", 
                                            label = "Age Group:",
                                            choices = c(
                                              "26-35" = "26-35",
                                              "36-45" = "36-45",
                                              "46-55" = "46-55"),
                                            multiple = FALSE),
                                selectInput(inputId = "par_haveKids", 
                                            label = "Have Kids:",
                                            choices = c(
                                              "True" = "TRUE",
                                              "False" = "FALSE"),
                                            multiple = FALSE)),
                         column(width = 8,
                                plotlyOutput("wageScatterPlot")
                         )
                     )
                   )
          ), 
          tabPanel("Crutial factors", 
                   fluidRow(
                     column(width = 4,valueBoxOutput(width = 12,"blue_value_box")) ,
                     column(width = 4,valueBoxOutput(width = 12,"red_value_box")) ,
                     column(width = 4,valueBoxOutput(width = 12,"purple_value_box"))
                   ),
                   fluidRow(
                     box(
                       width = 12,
                       status = 'primary',
                       title = "Wage Distribution Through Time",
                       solidHeader = TRUE,
                       plotOutput("wageByMonth")
                     )
                   ),
                   fluidRow(
                     box(
                       width = 12,
                       status = 'primary',
                       title = "Wage By Education Level",
                       solidHeader = TRUE,
                       plotlyOutput("wageByEducationLevel")
                     ),
                   ),
                   fluidRow(
                     box(
                       width = 12,
                       title = "Wage By Joviality Status",
                       status = 'primary',
                       solidHeader = TRUE,
                       plotlyOutput("wageByJov")
                     )
                   ),
                   fluidRow(
                     box(
                       width = 12,
                       title = "Wage By Age Group",
                       status = 'primary',
                       solidHeader = TRUE,
                       plotlyOutput("wageByAgeGroup")
                     )
                   ),
                   fluidRow(
                     box(
                       width = 12,
                       title = "Wage By Have Kids or not",
                       status = 'primary',
                       solidHeader = TRUE,
                       plotlyOutput("wageByKids")
                     )
                   ),
                   
          ), 
          tabPanel("Data Table", fluidRow(DT::dataTableOutput("par_wage_table")))
        ),
      )
    ),
    tabItem(tabName ="expend_analysis",
            fluidPage(
              h2("Expenditure Analysis"),
              box(
                width = 12,
                title = "Daily Expenses",
                status = 'primary',
                solidHeader = TRUE,
                h4('The residents spend more on Saturdays and Sundays.'),
                girafeOutput(outputId = "exp_plot",width = 800, height = 400)
              ),
              box(
                width = 12,
                title = "Income and Expenses",
                status = 'primary',
                solidHeader = TRUE,
                selectInput(inputId = "in_ex_factor", 
                            label = "Group by",
                            choices = c(
                              "Education Level" = "educationLevel",
                              "Interest Group" = "interestGroup",
                              "Age Group" = "ageGroup",
                              "Joviality Status" = "Joviality_Group"),
                            multiple = FALSE),
                girafeOutput("income_spend_scatterplot"),
                DT::dataTableOutput("consume_table"))
            )
    ),
    tabItem(tabName = "network_tab",
            fluidPage(
              titlePanel("Development of Social Network Over Time"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "yearMonth", 
                              label = "Please select year and month:",
                              choices = c("2022-03" = "2022-03",
                                          "2023-03" = "2023-03"),
                              multiple = FALSE)
                ),
                
                fluidRow(
                  box(title = "Overall Social Network for the Selected Month",
                      solidHeader = TRUE, 
                      status = "primary", 
                      h5("Notes: Participants with higher centrality betweenness are highlighted"), plotOutput(outputId = "network_plot",width = 800, height = 600)),
                )
              )
            )
    ),
    tabItem(tabName = "group_tab",
            fluidPage(
              titlePanel("Social Network by Groups"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "yearMonth2", 
                              label = "Please select year and month:",
                              choices = c("2022-03" = "2022-03",
                                          "2023-03" = "2023-03"),
                              multiple = FALSE),
                  selectInput(inputId = "groups", 
                              label = "Please select participant group:",
                              choices = c("Education Level" = "educationLevel",
                                          "Interest Group" = "interestGroup",
                                          "Joviality" = "joviality"),
                              multiple = FALSE)
                ),
                fluidRow(
                  box(title = "Social Network for the Selected Group and Period",
                      solidHeader = TRUE, 
                      status = "primary", 
                      h5("Notes: Participants with higher centrality betweenness are highlighted"), plotOutput(outputId = "network_group",width = 800, height = 600)),
                )
              )
            )
    ),
    tabItem(tabName = "vis_tab",
            fluidPage(
              titlePanel("Social Network for Individual Participant"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "yearMonth3", 
                              label = "Please select year and month:",
                              choices = c("2022-03" = "2022-03",
                                          "2023-03" = "2023-03"),
                              multiple = FALSE)
                ),
                
                fluidRow(
                  box(title = "Social Network for the Selected Participant or Interest Group", 
                      solidHeader = TRUE, 
                      status = "primary", visNetworkOutput(outputId="visplot", width = 800, height = 600)),
                )
              )
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
              box( title = "Geographical region of the city", 
                   solidHeader = TRUE, 
                   status = "primary",  
                   plotlyOutput(outputId = "mapPlotAreas", height = 300)),
              #NEWSTART
              box(title = "Building type in the city", 
                  solidHeader = TRUE, 
                  status = "primary", tmapOutput(outputId = "mapPlotAll", height = 300)),
              
            ),
            fluidRow(
              box(title = "Select Venue type in the city", 
                  solidHeader = TRUE, 
                  status = "primary",
                  selectInput(inputId = "venueTypeSelected", 
                              label = "",
                              choices = c("All" = "All",
                                          "Apartments" = "Apartments",
                                          "Employers" = "Employers",
                                          "Pubs" = "Pubs",
                                          "Restaurants" = "Restaurants",
                                          "Schools" = "Schools"),
                              multiple = FALSE),
                  radioButtons(inputId="radioForMap", label = "Please select additional details to be shown in the map:",
                               choices = list("Geographical Region" = "region", "Building Type" = "bldngTy", "None" = ""), 
                               selected = ""),
              ),
              
              box(title = "Venue type in the city", 
                  solidHeader = TRUE, 
                  status = "primary", tmapOutput(outputId = "mapPlotAllDetails", height = 300))
              #NEWEND
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
                  textInput(inputId = "s3_plotTitle",
                            label = "Plot title",
                            placeholder = "Enter text to be used as plot title"),
                  actionButton(inputId = "s3_goButton", 
                               "Update Plot Title!")
                  
                ),
                fluidRow(
                  box(title = "Map for selected venue type with cost details", 
                      solidHeader = TRUE, 
                      status = "primary", tmapOutput(outputId = "mapPlotbyType", width = 780, height = 360)),
                  box(title = "Statistical test for the selected variables", 
                      solidHeader = TRUE, 
                      status = "primary", plotOutput(outputId = "costTestPlot",width = 780, height = 360)),
                )
              )
            )
    ),
    
    tabItem(tabName = "checkin_tab",
            fluidPage(
              #NEWSTART
              titlePanel("Check-in Analysis for Pubs and Restaurants"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "checkinType", 
                              label = "Check-in Venue Type:",
                              choices = c("Pubs" = "Pubs",
                                          "Restaurants" = "Restaurants"),
                              multiple = FALSE),
                  sliderInput(inputId = "checkinDate",
                              label = "Date Range",
                              min = as.Date("2022-03-01","%Y-%m-%d"),
                              max = as.Date("2023-05-25","%Y-%m-%d"),
                              value=c(as.Date("2022-03-01"), as.Date("2022-04-01")),
                              timeFormat="%Y-%m-%d"),
                  HTML('<hr style="color: black;">'),
                  helpText("Please select below fields for a correlation test on selected variables."),
                  selectInput(inputId = "s3_xyvariable",
                              label = "Select x and y variables:",
                              choices = c("Mean of check-ins VS. Pub Cost" = "pubMean",
                                          "Median of check-ins VS. Pub Cost" = "pubMedian",
                                          "Mean of check-ins VS. Restaurant Food Cost" = "restMean",
                                          "Median of check-ins VS. Restaurant Food Cost" = "restMedian"
                              ),
                              selected = "pubMean"),
                  selectInput(inputId = "s3_test1",
                              label = "Type of statistical test:",
                              choices = c("parametric" = "p",
                                          "nonparametric" = "np",
                                          "robust" = "r",
                                          "Bayes Factor" = "bf"),
                              selected = "p"),
                  checkboxInput(inputId = "s3_marginal", 
                                label = "Display marginal graphs", 
                                value = TRUE),
                  textInput(inputId = "s3_plotTitle1",
                            label = "Plot title",
                            placeholder = "Enter text to be used as plot title"),
                  actionButton(inputId = "s3_goButton1", 
                               "Update Plot Title!")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Plots", 
                             fluidRow(
                               box(title = "Check-in trends for selected venue type", 
                                   solidHeader = TRUE, 
                                   status = "primary",
                                   plotlyOutput("checkinPlot",width = "90%", height = "340px"), width = 10, height = 400),
                             ),
                             fluidRow(
                               box(title = "Total no. of check-ins for the selected period and venue type on the map", 
                                   solidHeader = TRUE, 
                                   status = "primary", 
                                   tmapOutput(outputId = "checkinMap", height = 300), width = 10),
                             )    
                    ), 
                    tabPanel("Correlation Test", 
                             fluidRow(
                               box(title = "Correlation test for the selected variables", 
                                   solidHeader = TRUE, 
                                   status = "primary",  
                                   plotOutput(outputId = "corrPlot",width = "90%", height = "340px"), width = 10, height = 400),
                             )    
                    ), 
                  ),
                )
              )
              #NEWEND
            )
    ),
    tabItem(tabName = "revenue_tab",
            fluidPage(
              titlePanel("Revenue Analysis for Pubs and Restaurants"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "revVenueType", 
                              label = "Select Venue Type:",
                              choices = c("Pubs" = "Pubs",
                                          "Restaurants" = "Restaurants"),
                              multiple = FALSE),
                  sliderInput(inputId = "revDate",
                              label = "Period",
                              min = as.Date("2022-03-01"),
                              max = as.Date("2023-05-01"),
                              value=c(as.Date("2022-03-01"), as.Date("2022-09-01")),
                              timeFormat="%Y-%m"),
                  selectInput(inputId = "revPlotType", 
                              label = "Select Plot Type:",
                              choices = c("Monthly Revenue Plot" = "revenue",
                                          "Monthly Customers Plot" = "customers",
                                          "Monthly Revenue/Customer Plot" = "RevenuePerCustomer"),
                              multiple = FALSE),
                  HTML('<hr style="color: black;">'),
                  helpText("Please select below fields for a statistical test on selected variables."),
                  selectInput(inputId = "s3_revxvariable",
                              label = "Select x-variable:",
                              choices = c("Region" = "Region"),
                              selected = "Region"),
                  selectInput(inputId = "s3_revyvariable",
                              label = "Select y-variable:",
                              choices = c("Monthly Revenue" = "revenue",
                                          "Monthly Customers" = "customers",
                                          "Monthly Revenue/Customer" = "RevenuePerCustomer"),
                              selected = "revenue"),
                  selectInput(inputId = "s3_revtest",
                              label = "Type of statistical test:",
                              choices = c("nonparametric" = "np",
                                          "parametric" = "p",
                                          "robust" = "r",
                                          "Bayes Factor" = "bf"),
                              selected = "p"),
                  selectInput(inputId = "s3_revplotType",
                              label = "Type of plot:",
                              choices = c("boxviolin" = "boxviolin",
                                          "box" = "box",
                                          "violin" = "violin"),
                              selected = "boxviolin"),
                  textInput(inputId = "s3_revStatsPlotTitle",
                            label = "Plot title",
                            placeholder = "Enter text to be used as plot title"),
                  actionButton(inputId = "s3_revStatsGoButton", 
                               "Update Plot Title!"),
                  
                  HTML('<hr style="color: black;">'),
                  helpText("Please select below fields for a correlation test on selected variables."),
                  selectInput(inputId = "s3_revxyvariable",
                              label = "Select x and y variables:",
                              choices = c("Revenue VS. Pub Cost" = "pubRev",
                                          "Customers VS. Pub Cost" = "pubCust",
                                          "Revenue VS. Restaurant Food Cost" = "restRev",
                                          "Customers VS. Restaurant Food Cost" = "restCust"
                              ),
                              selected = "pubRev"),
                  selectInput(inputId = "s3_revCorrtest",
                              label = "Type of statistical test:",
                              choices = c("parametric" = "p",
                                          "nonparametric" = "np",
                                          "robust" = "r",
                                          "Bayes Factor" = "bf"),
                              selected = "p"),
                  checkboxInput(inputId = "s3_revMarginal",
                                label = "Display marginal graphs",
                                value = TRUE),
                  textInput(inputId = "s3_revCorrPlotTitle",
                            label = "Plot title",
                            placeholder = "Enter text to be used as plot title"),
                  actionButton(inputId = "s3_revCorrGoButton", 
                               "Update Plot Title!"),
                  
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Plots", 
                             fluidRow(
                               box(title = "Monthly revenue and customers for selected venue type", 
                                   solidHeader = TRUE, 
                                   status = "primary",
                                   plotlyOutput("revPlot",width = "90%", height = "340px"), width = 10, height = 400),
                             ),
                             fluidRow(
                               box(title = "Revenue and customers for selected venue type on map", 
                                   solidHeader = TRUE, 
                                   status = "primary", 
                                   tmapOutput(outputId = "revMap", height = 300), width = 10),
                             )    
                    ), 
                    
                    tabPanel("Statistical Test", 
                             fluidRow(
                               box(title = "Statistical test for the selected variables", 
                                   solidHeader = TRUE, 
                                   status = "primary",  
                                   plotOutput(outputId = "revStatsPlot",width = "90%", height = "340px"), width = 10, height = 400),
                               box(title = "Correlation test for the selected variables", 
                                   solidHeader = TRUE, 
                                   status = "primary",  
                                   plotOutput(outputId = "revCorrPlot",width = "90%", height = "340px"), width = 10, height = 400),
                             )    
                    ), 
                  ),
                )
              )
              #NEWEND
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
participant_fin <- read_rds("data/participant_fin.rds")
Education_fin <- read_rds("data/Education_fin.rds")
participants_data <- read_rds('data/participants.rds')
consume_report <- read_rds('data/consume_report.rds')
financeJ <- read_rds(file = "data/financeJ.rds")
monthlyFinancial <- read_rds(file = 'data/monthlyFinance.rds')
expense_part_month_data <- read_rds(file = 'data/expense_part_month_data.rds')

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
#NEWSTART
checkin_journal <- read_rds("data/checkin_journal_selected.rds")
checkin_journal$timestamp <- as.Date(checkin_journal$timestamp, "%Y-%m-%d")
pub_cust_rev <- read_rds("data/pub_cust_rev.rds")
rest_cust_rev <- read_rds("data/rest_cust_rev.rds")
#NEWEND

network_nodes <- read_csv("data/Participants.csv")

network_edges_aggregated_2022 <- read_rds("data/network_edges_aggregated_2022.rds")
network_edges_aggregated_2023 <- read_rds("data/network_edges_aggregated_2023.rds")

buildings_shp <- read_sf("data/buildings.shp", 
                         options = "GEOM_POSSIBLE_NAMES=location")

## End of Data Import

## Start of data processing
max_wage <- round(max(participants_data$wage),2)
min_wage <- round(min(participants_data$wage),2)
avg_wage <- round(mean(participants_data$wage),2)

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
  wage_edu_plot <- Education_fin %>%
    ggplot(aes(x=date, y = income,col =educationLevel))+
    geom_line(size = 0.75)+
    ylab("Income")+
    xlab("Month, Year")+
    theme(axis.title.y=element_text(angle =0),
          axis.title.x=element_text(margin = margin(t=-10)),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_line(colour = "grey90"),
          panel.grid.major.x = element_line(colour = "grey90"),
          panel.grid.major.y = element_line(colour = "grey90"),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 45, margin = margin(t = 30)),
          #axis.text.y = element_text(),
          axis.line = element_line(color="grey25", size = 0.02))+
          #axis.title = element_text(size=16),
          #legend.title = element_text(size =16),
          #legend.text = element_text(size = 16),
          #plot.title = element_text(size =20,hjust = 0.5))+
    ggtitle("Average Wage by Education Level")
  wage_age_plot <- ggplot(participants_data, aes(x = wage, fill = ageGroup)) + 
    geom_histogram(data=participants_data, alpha=.5) +
    geom_histogram() +
    labs(x = "Wage", title = "Participants'wage wih different Age Group")+
    facet_wrap(~ ageGroup,scales = "free_y") + 
    guides(fill = "none") + 
    theme_bw()
  wage_kid_plot <- ggplot(participants_data, aes(x = wage, fill = haveKids)) + 
    geom_histogram(data=participants_data, alpha=.5) +
    geom_histogram() +
    labs(x = "Wage", title = "Participants'wage wih Kids or not")+
    guides(fill = "none") + 
    theme_bw()
  wage_jov_plot <- ggplot(participants_data, aes(x = wage, 
                                                 fill = Joviality_Group)) + 
    geom_histogram(data=participants_data, alpha=.5) +
    geom_histogram() +
    labs(x = "Wage", title = "Participants'wage wih different Joviality Status")+
    facet_wrap(~ Joviality_Group,scales = "free_y") + 
    guides(fill = "none") + 
    theme_bw()
  kids_plot <- participants_data %>%
    group_by(ageGroup, haveKids) %>%
    summarise(n = n()) %>%
    mutate(perc = paste(round(100*round(n / sum(n),3), 2), "%", sep="")) %>%
    ggplot(aes(fill=haveKids, x=ageGroup, y=perc)) + 
    geom_col() +
    geom_text(aes(label = perc), position = position_stack(vjust = 0.5)) +
    labs(x="Age group", y="Percentage",
         title = "Percentage of people with kids in different age groups", fill = "Have Kids?") +
    theme_minimal()
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
  wage_month_ridge_plot <- ggplot(monthlyFinancial, 
                                  aes(x=joviality, 
                                      y=monthlyFinancial$yearmonth, 
                                      fill = factor(stat(quantile)))) +
    stat_density_ridges(geom = "density_ridges_gradient", 
                        calc_ecdf = TRUE,
                        quantiles = 4, 
                        quantile_lines = TRUE) +
    scale_fill_viridis_d(name = "Quartiles") +
    labs(x= "Wage",
         y= "Time",
         title="Distribution of Residents' =Wage")
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Timeline"),
      Value = as.character(c(input$Month),
                           stringsAsFactors = FALSE)
    )
    
  })
  output$exp_plot <- renderGirafe({
    p3 <- expense_part_month_data%>%
      ggplot(aes(day,month,fill=AvgExpense))+
      geom_tile_interactive(aes(tooltip = tooltip),color = "white", size = 0.1)+
      theme_tufte(base_family = "Helvetica")+
      coord_equal() +
      scale_fill_gradient(name = "Expense",
                          low = "sky blue", 
                          high = "dark blue")+
      labs(x = "Day of the month", 
           y = NULL, 
           title = "Average Daily Expense of Residents") +
      theme(axis.text = element_text(size = 12,margin = margin(r = -60)),
            axis.ticks.y= element_blank(),
            legend.title = element_text(size =16),
            legend.text = element_text(size = 16),
            plot.title = element_text(size =18),
            axis.title.x = element_text(size = 14))
    
    girafe(
      ggobj = p3,
      width_svg = 12,
      height_svg = 12*0.618
    )
  })
  output$income_spend_scatterplot <- renderGirafe({
    if (!!input$in_ex_factor == "educationLevel"){
      p2 <- participant_fin %>%
        filter(date == 'Apr 2022') %>%
        ggplot(aes(x=income, y = abs(expense), size = savings, color = educationLevel))+
        geom_point_interactive(aes(tooltip = tooltip), alpha=0.7) +
        ggtitle(paste("Income vs Expense by different"),"Education Level") +
        ylab("Expense") +
        xlab("Income")+
        theme_minimal() +
        theme(axis.line = element_line(size = 0.5),
              axis.text = element_text(size = 16),
              axis.title = element_text(size=16),
              axis.title.y = element_text(angle = 0),
              legend.title = element_text(size =16),
              legend.text = element_text(size = 16),
              plot.title = element_text(size =20,hjust = 0.5))
    }
    else if (!!input$in_ex_factor == "interestGroup"){
      p2 <- participant_fin %>%
        filter(date == 'Apr 2022') %>%
        ggplot(aes(x=income, y = abs(expense), size = savings, color = interestGroup))+
        geom_point_interactive(aes(tooltip = tooltip), alpha=0.7) +
        ggtitle(paste("Income vs Expense by different"),"Interest Group") +
        ylab("Expense") +
        xlab("Income")+
        theme_minimal() +
        theme(axis.line = element_line(size = 0.5),
              axis.text = element_text(size = 16),
              axis.title = element_text(size=16),
              axis.title.y = element_text(angle = 0),
              legend.title = element_text(size =16),
              legend.text = element_text(size = 16),
              plot.title = element_text(size =20,hjust = 0.5))
    }
    else if (!!input$in_ex_factor == "ageGroup"){
      p2 <- participant_fin %>%
        filter(date == 'Apr 2022') %>%
        ggplot(aes(x=income, y = abs(expense), size = savings, color = ageGroup))+
        geom_point_interactive(aes(tooltip = tooltip), alpha=0.7) +
        ggtitle(paste("Income vs Expense by different"),"Age Group") +
        ylab("Expense") +
        xlab("Income")+
        theme_minimal() +
        theme(axis.line = element_line(size = 0.5),
              axis.text = element_text(size = 16),
              axis.title = element_text(size=16),
              axis.title.y = element_text(angle = 0),
              legend.title = element_text(size =16),
              legend.text = element_text(size = 16),
              plot.title = element_text(size =20,hjust = 0.5))
    }
    else if (!!input$in_ex_factor == "Joviality_Group"){
      p2 <- participant_fin %>%
        filter(date == 'Apr 2022') %>%
        ggplot(aes(x=income, y = abs(expense), size = savings, color = Joviality_Group))+
        geom_point_interactive(aes(tooltip = tooltip), alpha=0.7) +
        ggtitle(paste("Income vs Expense by different"),"Joviality Status") +
        ylab("Expense") +
        xlab("Income")+
        theme_minimal() +
        theme(axis.line = element_line(size = 0.5),
              axis.text = element_text(size = 16),
              axis.title = element_text(size=16),
              axis.title.y = element_text(angle = 0),
              legend.title = element_text(size =16),
              legend.text = element_text(size = 16),
              plot.title = element_text(size =20,hjust = 0.5))
    }
    
    
    girafe(
      ggobj = p2,
      width_svg = 16,
      height_svg = 16*0.618
    )
  })
  output$wageByMonth <- renderPlot({
    wage_month_ridge_plot
  })
  output$wageByEducationLevel <- renderPlotly({
    wage_edu_plot
  })
  output$wageByAgeGroup <- renderPlotly({
    wage_age_plot
  })
  output$wageByKids <- renderPlotly({
    wage_kid_plot
  })
  output$wageByJov <- renderPlotly({
    wage_jov_plot
  })
  output$consume_table <- DT::renderDataTable({
    dt <-  DT::datatable(as.data.frame(consume_report %>%
                                         group_by(category) %>%
                                         summarize('Monthly Expense' = list(consume), 
                                                   .groups = "drop") %>%
                                         gt() %>%
                                         gt_plt_sparkline('Monthly Expense')),  rownames = FALSE)
  })
  output$wage_ana_plot <- renderPlotly({
    wage_ana_plot
  })
  output$par_wage_table <- DT::renderDataTable({
    ## Colour and values for table colour formatting
    brks <- seq(min_wage, max_wage, 10)
    clrs <- colorRampPalette(c("white", "#6baed6"))(length(brks) + 1)
    dt <-  DT::datatable(as.data.frame(participants_data %>%
                                         #filter((educationLevel == input$par_educationLevel) &
                                         #(ageGroup == input$par_ageGroup) &
                                         #(haveKids == input$par_haveKids))%>%
                                         select(participantId, age, householdSize, joviality, interestGroup, wage) %>%
                                         gt() %>%
                                         fmt_number(columns = 2,
                                                    decimals = 2)
    )) %>%
      formatStyle(c("wage"), backgroundColor = styleInterval(brks, clrs))
  })
  output$wage_distribution <- renderPlotly({
    if(input$wage_boxplot_var == "educationLevel"){
      p <- ggplot(data=participants_data, aes(x = educationLevel, 
                                              y = wage))+
        geom_boxplot(position="dodge",aes(x = educationLevel, y = wage)) +
        stat_summary(geom = "point",
                     fun="mean",
                     colour ="red",
                     size=2) +
        stat_summary(aes(label = round(..y.., 0)), fun=mean, geom = "label_repel", size=3, angle=150) +
        labs(y= 'Wage', x= 'Education Level',
             title = paste("Wage Distribution by","Education Level"))
      ggplotly(p)
    }
    else if(input$wage_boxplot_var == "interestGroup"){
      p <- ggplot(data=participants_data, aes(x = interestGroup, 
                                              y = wage))+
        geom_boxplot(position="dodge",aes(x = interestGroup, y = wage)) +
        stat_summary(geom = "point",
                     fun="mean",
                     colour ="red",
                     size=2) +
        stat_summary(aes(label = round(..y.., 0)), fun=mean, geom = "label_repel", size=3, angle=150) +
        labs(y= 'Wage', x= 'Interest Group',
             title = paste("Wage Distribution by","Interest Group"))
      ggplotly(p)
    }
    else if(input$wage_boxplot_var == "jovialityGroup"){
      p <- ggplot(data=participants_data, aes(x = Joviality_Group, 
                                              y = wage))+
        geom_boxplot(position="dodge",aes(x = Joviality_Group, y = wage)) +
        stat_summary(geom = "point",
                     fun="mean",
                     colour ="red",
                     size=2) +
        stat_summary(aes(label = round(..y.., 0)), fun=mean, geom = "label_repel", size=3, angle=150) +
        labs(y= 'Wage', x= 'Joviality Group',
             title = paste("Wage Distribution by","Joviality Group"))
      ggplotly(p)
    }
    else if(input$wage_boxplot_var == "ageGroup"){
      p <- ggplot(data=participants_data, aes(x = ageGroup, 
                                              y = wage))+
        geom_boxplot(position="dodge",aes(x = ageGroup, y = wage)) +
        stat_summary(geom = "point",
                     fun="mean",
                     colour ="red",
                     size=2) +
        stat_summary(aes(label = round(..y.., 0)), fun=mean, geom = "label_repel", size=3, angle=150) +
        labs(y= 'Wage', x= 'Age Group',
             title = paste("Wage Distribution by","Age Group"))
      ggplotly(p)
    }
    else if(input$wage_boxplot_var == "haveKids"){
      p <- ggplot(data=participants_data, aes(x = haveKids, 
                                              y = wage))+
        geom_boxplot(position="dodge",aes(x = haveKids, y = wage)) +
        stat_summary(geom = "point",
                     fun="mean",
                     colour ="red",
                     size=2) +
        stat_summary(aes(label = round(..y.., 0)), fun=mean, geom = "label_repel", size=3, angle=150) +
        labs(y= 'Wage', x= 'Have Kids',
             title = paste("Wage Distribution by","Whether They Have Kids"))
      ggplotly(p)
    }
  })
  output$wageScatterPlot <- renderPlotly({
    p <- ggplot(participants_data %>%
                  filter((educationLevel == input$par_educationLevel) &
                           (ageGroup == input$par_ageGroup) &
                           (haveKids == input$par_haveKids)
                  ), aes (x=participantId,
                          y = wage,
                          color=wage,
                          text = paste('</br>participantId: ', participantId,
                                       '</br>Wage: ', round(wage,2),
                                       '</br>Age: ', age,
                                       '</br>Have Kids:', haveKids,
                                       '</br>Interest Group:', interestGroup,
                                       '</br>Joviality:', round(joviality,2),
                                       '</br>Education Level:', educationLevel
                          )
                  )
    ) +
      geom_point() +
      geom_hline(aes(yintercept=mean(wage)), color='red', linetype='dashed')+
      geom_text(aes(0, mean(wage), label = "Avg", hjust = -1, vjust=-1),color='red') +
      scale_y_continuous(name = "Wage",limits = c(1800, 16240))+
      labs(title = "Participants' wage")+
      theme_bw()
    ggplotly(p,tooltip="text")
  })
  output$blue_value_box <- renderValueBox({
    valueBox(
      value = max_wage,
      subtitle = "Max Wage",
      icon = icon("area-chart"),
      color =  "aqua"
    )
  })
  output$red_value_box <- renderValueBox({
    valueBox(
      value = min_wage,
      subtitle = "Min Wage",
      icon = icon("area-chart"),
      color =  "red"
    )
  })
  output$purple_value_box <- renderValueBox({
    valueBox(
      value = avg_wage,
      subtitle = "Mean Wage",
      icon = icon("area-chart"),
      color =  "purple"
    )
  })
  output$wage_edu_plot <- renderPlotly({
    wage_edu_plot
  })
  output$wage_age_plot <- renderPlotly({
    wage_age_plot
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
  myvars <- names(participants_data) %in% c("wage")
  output$wage_ana_plot <- renderPlot({
    ggbetweenstats(
      data = financeJ %>%
        filter(`timestamp` >= input$wageDate[1] & `timestamp` <= input$wageDate[2],category=='Wage') %>%
        group_by(participantId) %>%
        summarise(Wage = mean(amount)) %>%
        ungroup() %>%
        left_join(participants_data[!myvars],by='participantId'),
      outlier.tagging = FALSE, ## whether outliers should be flagged
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
      network_graph_2022 <- network_graph_2022  %>%
        mutate(Centrality = centrality_betweenness())
      network_plot <-ggraph(network_graph_2022,layout = "nicely") + 
        geom_edge_link(aes(width=Weight), alpha=0.25) +
        scale_edge_width(range = c(0.1, 1)) +
        geom_node_point(color="lightblue", aes( alpha=0.05,size = centrality_betweenness()))+
        geom_node_text(color="red", aes( filter=Centrality > 35000, label = name),
                       repel = TRUE)
      network_plot + theme_graph()
    }
    else if (input$yearMonth == "2023-03"){
      network_graph_2023 <- network_graph_2023  %>%
        mutate(Centrality = centrality_betweenness())
      network_plot<- ggraph(network_graph_2023,layout = "nicely") + 
        geom_edge_link(aes(width=Weight), alpha=0.25) +
        scale_edge_width(range = c(0.1, 1)) +
        geom_node_point(color="lightblue", aes( alpha=0.05, size = centrality_betweenness()))+
        geom_node_text(color="red", aes( filter=Centrality > 15000, label = name),repel = TRUE)
      network_plot + theme_graph()
    }
  })
  
  output$network_group <- renderPlot({
    if(input$yearMonth2 =="2022-03"){
      set.seed(1234)
      network_graph_2022 <- network_graph_2022  %>%
        mutate(Centrality = centrality_betweenness())
      if (input$groups =="educationLevel"){
        network_group <- ggraph(network_graph_2022, layout = "nicely") + 
          geom_edge_link(aes(width=Weight), alpha=0.2) +
          scale_edge_width(range = c(0.1, 1)) +
          geom_node_point(aes(colour = educationLevel,alpha=0.05,size = centrality_betweenness()))+
          geom_node_text(color="red",aes(filter=Centrality > 35000, label = name),repel = TRUE)
        network_group + theme_graph()
      }
      else if (input$groups =="interestGroup"){
        network_group <- ggraph(network_graph_2022, layout = "nicely") + 
          geom_edge_link(aes(width=Weight), alpha=0.2) +
          scale_edge_width(range = c(0.1, 1)) +
          geom_node_point(aes(colour = interestGroup,alpha=0.05,size = centrality_betweenness()))+
          geom_node_text(color="red",aes(filter=Centrality > 35000, label = name),repel = TRUE)
        network_group + theme_graph()
      }
      else if (input$groups =="joviality"){
        network_group <- ggraph(network_graph_2022, layout = "nicely") + 
          geom_edge_link(aes(width=Weight), alpha=0.2) +
          scale_edge_width(range = c(0.1, 1)) +
          geom_node_point(aes(colour = joviality,alpha=0.05,size = centrality_betweenness()))+
          geom_node_text(color="red",aes(filter=Centrality > 35000, label = name),repel = TRUE)
        network_group + theme_graph()
      }
    }
    else if (input$yearMonth2 =="2023-03"){
      set.seed(1234)
      network_graph_2023 <- network_graph_2023  %>%
        mutate(Centrality = centrality_betweenness())
      if (input$groups =="educationLevel"){
        network_group <- ggraph(network_graph_2023, layout = "nicely") + 
          geom_edge_link(aes(width=Weight), alpha=0.2) +
          scale_edge_width(range = c(0.1, 1)) +
          geom_node_point(aes(colour = educationLevel,alpha=0.05,size = centrality_betweenness()))+
          geom_node_text(color="red",aes(filter=Centrality > 15000, label = name),repel = TRUE)
        network_group + theme_graph()
      }
      else if (input$groups =="interestGroup"){
        network_group <- ggraph(network_graph_2023, layout = "nicely") + 
          geom_edge_link(aes(width=Weight), alpha=0.2) +
          scale_edge_width(range = c(0.1, 1)) +
          geom_node_point(aes(colour = interestGroup,alpha=0.05,size = centrality_betweenness()))+
          geom_node_text(color="red",aes(filter=Centrality > 15000, label = name),repel = TRUE)
        network_group + theme_graph()
      }
      else if (input$groups =="joviality"){
        network_group <- ggraph(network_graph_2023, layout = "nicely") + 
          geom_edge_link(aes(width=Weight), alpha=0.2) +
          scale_edge_width(range = c(0.1, 1)) +
          geom_node_point(aes(colour = joviality,alpha=0.05,size = centrality_betweenness()))+
          geom_node_text(color="red",aes(filter=Centrality > 15000, label = name),repel = TRUE)
        network_group + theme_graph()
      }
    }
  })
  
  output$visplot <- renderVisNetwork({
    network_nodes_2022 <-network_nodes_2022 %>%
      rename(id = participantId)%>%
      rename(group = educationLevel)
    
    network_nodes_2023 <-network_nodes_2023 %>%
      rename(id = participantId)%>%
      rename(group = educationLevel)
    if(input$yearMonth3 =="2022-03"){
      visNetwork(network_nodes_2022,
                 network_edges_aggregated_2022) %>%
        visIgraphLayout(layout = "layout_with_fr") %>%
        visEdges(arrows = "to", 
                 smooth = list(enabled = TRUE, 
                               type = "curvedCW")) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE,
                   selectedBy = "interestGroup") %>%
        visLegend() %>%
        visLayout(randomSeed = 123)
    }
    else if (input$yearMonth3=="2023-03"){
      visNetwork(network_nodes_2023,
                 network_edges_aggregated_2023) %>%
        visIgraphLayout(layout = "layout_with_fr") %>%
        visEdges(arrows = "to", 
                 smooth = list(enabled = TRUE, 
                               type = "curvedCW")) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE,
                   selectedBy = "interestGroup") %>%
        visLegend() %>%
        visLayout(randomSeed = 123)
      
    }
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
  #NEWSTART
  output$mapPlotAllDetails <- renderTmap({
    tmap_mode("view")
    mapAll <- tm_shape(buildings_shp)+
      tm_polygons(
        col = ifelse(input$radioForMap == "", NA, input$radioForMap),
        palette="Accent",
        border.col = "black",
        border.alpha = .5,
        border.lwd = 0.5)
    if (input$venueTypeSelected == "All"){
      mapAll <- tm_shape(buildings_shp)+
        tm_polygons(
          col = ifelse(input$radioForMap == "", NA, input$radioForMap),
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
      mapAll <- tm_shape(buildings_shp)+
        tm_polygons(
          col = ifelse(input$radioForMap == "", NA, input$radioForMap),
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) + 
        tm_shape(restaurants) +
        tm_dots(col = "blue")
    }
    
    if (input$venueTypeSelected == "Employers"){
      mapAll <- tm_shape(buildings_shp)+
        tm_polygons(
          col = ifelse(input$radioForMap == "", NA, input$radioForMap),
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) + 
        tm_shape(employers) +
        tm_dots(col = "red")
    }
    
    if (input$venueTypeSelected == "Apartments"){
      mapAll <- tm_shape(buildings_shp)+
        tm_polygons(
          col = ifelse(input$radioForMap == "", NA, input$radioForMap),
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) + 
        tm_shape(apartments) +
        tm_dots(col = "lightblue")
    }
    
    if (input$venueTypeSelected == "Pubs"){
      mapAll <- tm_shape(buildings_shp)+
        tm_polygons(
          col = ifelse(input$radioForMap == "", NA, input$radioForMap),
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) + 
        tm_shape(pubs) +
        tm_dots(col = "green")
    }
    
    if (input$venueTypeSelected == "Schools"){
      mapAll <- tm_shape(buildings_shp)+
        tm_polygons(
          col = ifelse(input$radioForMap == "", NA, input$radioForMap),
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) + 
        tm_shape(schools) +
        tm_dots(col = "yellow")
    }
    mapAll
    
  })
  #NEWEND
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
  #NEWSTART
  output$costTestPlot <-renderPlot({
    input$s3_goButton
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
        title = input$s3_plotTitle,
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
        title = input$s3_plotTitle,
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
        title = input$s3_plotTitle,
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
        title = input$s3_plotTitle,
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
        title = input$s3_plotTitle,
        type = input$s3_test,
        plot.type = input$s3_plotType,
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE)
    }
    #NEWEND
  })
  #NEWSTART
  output$checkinPlot <- renderPlotly({
    if (input$checkinType == "Restaurants"){
      checkin_journal_rest <- checkin_journal %>%
        filter(`venueType` == "Restaurant" & `timestamp` >= input$checkinDate[1] & `timestamp` <= input$checkinDate[2])
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
  
  output$checkinMap <- renderTmap({
    tmap_mode("plot")
    checkintm <- tm_shape(buildings_shp)+
      tm_polygons(
        col = "region",
        palette="Accent",
        border.col = "black",
        border.alpha = .5,
        border.lwd = 0.5)
    
    if (input$checkinType == "Restaurants"){
      checkin_journal_rest <- checkin_journal %>%
        filter(`venueType` == "Restaurant" & `timestamp` >= input$checkinDate[1] & `timestamp` <= input$checkinDate[2])
      checkin_rest <- checkin_journal_rest %>%
        group_by(`venueId`) %>%
        summarise('checkins' = n()) %>%
        ungroup()
      checkin_rest$venueId <- as.character(checkin_rest$venueId)
      restaurants_df <- restaurants %>% 
        as.data.frame()%>%
        rename(venueId = restaurantId)
      checkin_rest <-  left_join(checkin_rest, restaurants_df, by = "venueId")
      checkin_rest <- st_as_sf(checkin_rest )
      tmap_mode("plot")
      checkintm <- tm_shape(buildings_shp)+
        tm_polygons(
          col = "region",
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) +
        tm_shape(checkin_rest)+
        tm_bubbles(col = "checkins",
                   alpha = 0.8,
                   n = 6,
                   style = "jenks",
                   palette="Reds",
                   size = "checkins",
                   scale = 0.8,
                   border.col = "black",
                   border.lwd = 0.5)
      
    }
    
    else if (input$checkinType == "Pubs"){
      checkin_journal_pub <- checkin_journal %>%
        filter(`venueType` == "Pub" & `timestamp` >= input$checkinDate[1] & `timestamp` <= input$checkinDate[2])
      checkin_pub <- checkin_journal_pub %>%
        group_by(`venueId`) %>%
        summarise('checkins' = n()) %>%
        ungroup()
      checkin_pub$venueId <- as.character(checkin_pub$venueId)
      pubs_df <- pubs %>% 
        as.data.frame()%>%
        rename(venueId = pubId)
      checkin_pub <-  left_join(checkin_pub, pubs_df, by = "venueId")
      checkin_pub <- st_as_sf(checkin_pub)
      tmap_mode("plot")
      checkintm <- tm_shape(buildings_shp)+
        tm_polygons(
          col = "region",
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) +
        tm_shape(checkin_pub)+
        tm_bubbles(col = "checkins",
                   alpha = 0.8,
                   n = 6,
                   style = "jenks",
                   palette="Blues",
                   size = "checkins",
                   scale = 0.8,
                   border.col = "black",
                   border.lwd = 0.5)
      
    }
    tmap_mode("plot")
    checkintm
  })
  
  output$corrPlot <- renderPlot({
    input$s3_goButton1
    set.seed(1234)
    if (grepl("rest", input$s3_xyvariable, fixed=TRUE)){
      checkin_rest <- checkin_journal %>%
        filter(`venueType` == "Restaurant" & `timestamp` >= input$checkinDate[1] & `timestamp` <= input$checkinDate[2])
      if (input$s3_xyvariable == "restMean"){
        checkin_rest <- checkin_rest %>%
          group_by(`venueId`, `timestamp`) %>%
          summarise('checkins' = n()) %>%
          ungroup()%>%
          group_by(`venueId`) %>%
          summarise('checkins' = mean(checkins, na.rm = TRUE)) %>%
          ungroup()
      }
      else if (input$s3_xyvariable == "restMedian"){
        checkin_rest <- checkin_rest %>%
          group_by(`venueId`, `timestamp`) %>%
          summarise('checkins' = n()) %>%
          ungroup()%>%
          group_by(`venueId`) %>%
          summarise('checkins' = median(checkins,na.rm = TRUE)) %>%
          ungroup()
      }
      checkin_rest$venueId <- as.character(checkin_rest$venueId)
      restaurants_df <- restaurants %>% 
        as.data.frame()%>%
        rename(venueId = restaurantId)
      checkin_rest <-  left_join(checkin_rest, restaurants_df, by = "venueId")
      ggscatterstats(
        data = checkin_rest,
        x = "checkins", 
        y = "foodCost",
        type = input$s3_test1,
        marginal = input$s3_marginal,
        title = input$s3_plotTitle1,
        conf.level = 0.95,
        bf.prior = 0.707)
    }
    
    else if (grepl("pub", input$s3_xyvariable, fixed=TRUE)){
      checkin_pub <- checkin_journal %>%
        filter(`venueType` == "Pub" & `timestamp` >= input$checkinDate[1] & `timestamp` <= input$checkinDate[2])
      if (input$s3_xyvariable == "pubMean"){
        checkin_pub <- checkin_pub %>%
          group_by(`venueId`, `timestamp`) %>%
          summarise('checkins' = n()) %>%
          ungroup()%>%
          group_by(`venueId`) %>%
          summarise('checkins' = mean(checkins, na.rm = TRUE)) %>%
          ungroup()
      }
      else if (input$s3_xyvariable == "pubMedian"){
        checkin_pub <- checkin_pub %>%
          group_by(`venueId`, `timestamp`) %>%
          summarise('checkins' = n()) %>%
          ungroup()%>%
          group_by(`venueId`) %>%
          summarise('checkins' = median(checkins, na.rm = TRUE)) %>%
          ungroup()
      }
      
      checkin_pub$venueId <- as.character(checkin_pub$venueId)
      pubs_df <- pubs %>% 
        as.data.frame()%>%
        rename(venueId = pubId)
      checkin_pub <-  left_join(checkin_pub, pubs_df, by = "venueId")
      ggscatterstats(
        data = checkin_pub,
        x = "checkins", 
        y = "hourlyCost",
        type = input$s3_test1,
        marginal = input$s3_marginal,
        title = input$s3_plotTitle1,
        conf.level = 0.95,
        bf.prior = 0.707)
      
    }
    
  })
  
  output$revPlot <- renderPlotly({
    #print(input$revDate[1])
    #print(input$revDate[2])
    if (input$revVenueType == "Restaurants"){
      rest_cust_rev <- rest_cust_rev %>%
        filter(`monthYear` >= format(input$revDate[1], "%Y-%m") & `monthYear` <= format(input$revDate[2], "%Y-%m"))
      rest_cust_rev$venueId <- as.character(rest_cust_rev$venueId)
      #print(rest_cust_rev)
      rest_cust_rev$monthYear <- as.yearmon(rest_cust_rev$monthYear)
      revP <- ggplot(data=rest_cust_rev, 
                     aes_string(x = "monthYear",
                                y = input$revPlotType, 
                                color="venueId"
                     )) +
        labs(x="", y="", title = paste0("Monthly ", input$revPlotType, " for ", input$revVenueType)) +
        geom_line()
      ggplotly(revP)
    }
    
    else if (input$checkinType == "Pubs"){
      pub_cust_rev <- pub_cust_rev %>%
        filter(`monthYear` >= format(input$revDate[1], "%Y-%m") & `monthYear` <= format(input$revDate[2], "%Y-%m"))
      pub_cust_rev$venueId <- as.character(pub_cust_rev$venueId)
      #print(pub_cust_rev)
      pub_cust_rev$monthYear <- as.yearmon(pub_cust_rev$monthYear)
      revP <- ggplot(data=pub_cust_rev, 
                     aes_string(x = "monthYear",
                                y = input$revPlotType, 
                                color="venueId"
                     )) +
        labs(x = "", y="", title = paste0("Monthly ", input$revPlotType, " for ", input$revVenueType))+
        geom_line()
      ggplotly(revP)
      
    }
  })
  
  output$revMap <- renderTmap({
    tmap_mode("plot")
    revtm <- tm_shape(buildings_shp)+
      tm_polygons(
        col = "region",
        palette="Accent",
        border.col = "black",
        border.alpha = .5,
        border.lwd = 0.5)
    
    if (input$revVenueType == "Restaurants"){
      rest_cust_rev <- rest_cust_rev %>%
        filter(`monthYear` >= format(input$revDate[1], "%Y-%m") & `monthYear` <= format(input$revDate[2], "%Y-%m"))
      rest_cust_rev$venueId <- as.character(rest_cust_rev$venueId)
      rest_cust_rev$monthYear <- as.yearmon(rest_cust_rev$monthYear)
      restaurants_df <- restaurants %>% 
        as.data.frame()%>%
        rename(venueId = restaurantId)
      rev_rest <-  left_join(rest_cust_rev, restaurants_df, by = "venueId")
      rev_rest <- st_as_sf(rev_rest )
      tmap_mode("plot")
      revtm <- tm_shape(buildings_shp)+
        tm_polygons(
          col = "region",
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) +
        tm_shape(rev_rest)+
        tm_bubbles(col = input$revPlotType,
                   alpha = 0.8,
                   n = 6,
                   style = "jenks",
                   palette="Reds",
                   size = input$revPlotType,
                   scale = 0.8,
                   border.col = "black",
                   border.lwd = 0.5)
      
    }
    
    else if (input$checkinType == "Pubs"){
      pub_cust_rev <- pub_cust_rev %>%
        filter(`monthYear` >= format(input$revDate[1], "%Y-%m") & `monthYear` <= format(input$revDate[2], "%Y-%m"))
      pub_cust_rev$venueId <- as.character(pub_cust_rev$venueId)
      pub_cust_rev$monthYear <- as.yearmon(pub_cust_rev$monthYear)
      pubs_df <- pubs %>% 
        as.data.frame()%>%
        rename(venueId = pubId)
      rev_pub <-  left_join(pub_cust_rev, pubs_df, by = "venueId")
      rev_pub <- st_as_sf(rev_pub)
      tmap_mode("plot")
      revtm <- tm_shape(buildings_shp)+
        tm_polygons(
          col = "region",
          palette="Accent",
          border.col = "black",
          border.alpha = .5,
          border.lwd = 0.5) +
        tm_shape(rev_pub)+
        tm_bubbles(col = input$revPlotType,
                   alpha = 0.8,
                   n = 6,
                   style = "jenks",
                   palette="Blues",
                   size = input$revPlotType,
                   scale = 0.8,
                   border.col = "black",
                   border.lwd = 0.5)
      
    }
    tmap_mode("plot")
    revtm
  })
  
  output$revStatsPlot <-renderPlot({
    input$s3_revStatsGoButton
    set.seed(1234)
    buildings_shp <- buildings_shp %>%
      rename(buildingId = bldngId)
    buildings_df <- buildings_shp%>% as.data.frame()
    if (input$revVenueType == "Restaurants"){
      rest_cust_rev <- rest_cust_rev %>%
        filter(`monthYear` >= format(input$revDate[1], "%Y-%m") & `monthYear` <= format(input$revDate[2], "%Y-%m"))
      rest_cust_rev$venueId <- as.character(rest_cust_rev$venueId)
      #rest_cust_rev$monthYear <- as.yearmon(rest_cust_rev$monthYear)
      rest_df <- restaurants%>% 
        as.data.frame()%>%
        rename(venueId = restaurantId)
      joined_data <- left_join(rest_cust_rev, rest_df, by = "venueId")
      joined_data <- left_join(joined_data, buildings_df, by = "buildingId")
      #print(joined_data)
      ggbetweenstats(
        data = joined_data,
        x = "region",
        y = !!input$s3_revyvariable,
        title = input$s3_revStatsPlotTitle,
        type = input$s3_revtest,
        plot.type = input$s3_revplotType,
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE)
    }
    else if (input$revVenueType == "Pubs"){
      pub_cust_rev <- pub_cust_rev %>%
        filter(`monthYear` >= format(input$revDate[1], "%Y-%m") & `monthYear` <= format(input$revDate[2], "%Y-%m"))
      pub_cust_rev$venueId <- as.character(pub_cust_rev$venueId)
      #pub_cust_rev$monthYear <- as.yearmon(pub_cust_rev$monthYear)
      pub_df <- pubs%>% 
        as.data.frame()%>%
        rename(venueId = pubId)
      joined_data <- left_join(pub_cust_rev, pub_df, by = "venueId")
      joined_data <- left_join(joined_data, buildings_df, by = "buildingId")
      #print(joined_data)
      ggbetweenstats(
        data = joined_data,
        x = "region",
        y = !!input$s3_revyvariable,
        title = input$s3_revStatsPlotTitle,
        type = input$s3_revtest,
        plot.type = input$s3_revplotType,
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE)
    }
    
  })
  
  output$revCorrPlot <- renderPlot({
    input$s3_revCorrGoButton
    set.seed(1234)
    if (grepl("rest", input$s3_revxyvariable, fixed=TRUE)){
      rest_cust_rev <- rest_cust_rev %>%
        filter(`monthYear` >= format(input$revDate[1], "%Y-%m") & `monthYear` <= format(input$revDate[2], "%Y-%m"))
      rest_cust_rev$venueId <- as.character(rest_cust_rev$venueId)
      restaurants_df <- restaurants %>% 
        as.data.frame()%>%
        rename(venueId = restaurantId)
      rev_rest <-  left_join(rest_cust_rev, restaurants_df, by = "venueId")
      if (input$s3_revxyvariable == "restRev"){
        ggscatterstats(
          data = rev_rest,
          x = "revenue", 
          y = "foodCost",
          type = input$s3_revCorrtest,
          marginal = input$s3_revMarginal,
          title = input$s3_revCorrPlotTitle,
          conf.level = 0.95,
          bf.prior = 0.707)
      }
      else if (input$s3_revxyvariable == "restCust"){
        ggscatterstats(
          data = rev_rest,
          x = "customers", 
          y = "foodCost",
          type = input$s3_revCorrtest,
          marginal = input$s3_revMarginal,
          title = input$s3_revCorrPlotTitle,
          conf.level = 0.95,
          bf.prior = 0.707)
      }
      
    }
    
    else if (grepl("pub", input$s3_revxyvariable, fixed=TRUE)){
      pub_cust_rev <- pub_cust_rev %>%
        filter(`monthYear` >= format(input$revDate[1], "%Y-%m") & `monthYear` <= format(input$revDate[2], "%Y-%m"))
      pub_cust_rev$venueId <- as.character(pub_cust_rev$venueId)
      pubs_df <- pubs %>% 
        as.data.frame()%>%
        rename(venueId = pubId)
      rev_pub <-  left_join(pub_cust_rev, pubs_df, by = "venueId")
      if (input$s3_revxyvariable == "pubRev"){
        ggscatterstats(
          data = rev_pub,
          x = "revenue", 
          y = "hourlyCost",
          type = input$s3_revCorrtest,
          marginal = input$s3_revMarginal,
          title = input$s3_revCorrPlotTitle,
          conf.level = 0.95,
          bf.prior = 0.707)
      }
      else if (input$s3_revxyvariable == "pubCust"){
        ggscatterstats(
          data = rev_pub,
          x = "customers", 
          y = "hourlyCost",
          type = input$s3_revCorrtest,
          marginal = input$s3_revMarginal,
          title = input$s3_revCorrPlotTitle,
          conf.level = 0.95,
          bf.prior = 0.707)
      }
      
    }
    
  })
  #NEWEND
  ## End of Section 3
  
}

shinyApp(ui = ui, server = server)