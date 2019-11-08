#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(htmltools)
library(htmlwidgets)
library(shinyWidgets)
library(shinyglide)
library(lubridate)
library(DT)
library(kableExtra)
library(data.table)
library(plotly)
library(dplyr)
library(wordcloud2)
library(leaflet)
library(leaflet.extras)


load("recs.RData")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Crime Analysis"),
    dashboardSidebar(disable = T),
    dashboardBody(
        fluidPage(
            fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Baltimore Data",
                                     fluidRow(
                                         infoBoxOutput("TotalAgency", width = 2),
                                         infoBoxOutput("TotalCases", width = 2),
                                         infoBoxOutput("CasesOpen", width = 2),
                                         infoBoxOutput("CasesClosed", width = 2),
                                         infoBoxOutput("Warrents", width = 2),
                                         infoBoxOutput("Victims", width = 2)
                                     ),
                                     fluidRow(
                                         column(width = 4,
                                                plotOutput("vctm")
                                                ),
                                         column(width = 4,
                                                plotOutput("vctmAge")
                                         ),
                                         column(width = 4,
                                                plotOutput("suspects")
                                         )
                                     ),
                                     br(),
                                     fluidRow(
                                         box(width = 12, title = "High crime rate areas",solidHeader = TRUE,
                                             status = "info",
                                             leafletOutput("mp", height = "500")
                                             )
                                     )
                                     ),
                            tabPanel(title = "Federal Data",
                                     fluidRow(
                                         infoBoxOutput("TotalAgency_F", width = 3),
                                         infoBoxOutput("TotalPop", width = 3),
                                         infoBoxOutput("TotalVictims", width = 3),
                                         infoBoxOutput("DomesticViol", width = 3)
                                         
                                     ),
                                     fluidRow(
                                         column(width = 3,
                                                plotOutput("vctmdet")
                                                ),
                                         column(width = 3,
                                                plotOutput("vctmAgeDist")
                                                ),
                                         column(width = 6,
                                                plotOutput("crimeGrowth")
                                         )
                                     ),
                                     br(),
                                     fluidRow(
                                         column(width = 3,
                                                plotOutput("weapons")
                                                ),
                                         column(width = 3,
                                                plotOutput("casesByYear")
                                                ),
                                         column(width = 6,
                                                plotOutput("agencyPerform")
                                                )
                                     )
                                     )
                            )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$TotalAgency <- renderInfoBox({
        infoBox(title = "Total Agency",
                length(unique(b_Addr$agency_ori)), icon = icon("building"),
                color = "aqua"
        )
    })
    
    output$TotalCases <- renderInfoBox({
        infoBox(title = "Total Cases",
                length(unique(b_Shoot$CASE_NUMBER)), icon = icon("pen"),
                color = "blue"
        )
    })
    
    output$CasesOpen <- renderInfoBox({
        infoBox(title = "Open Cases",
                length(b_Shoot[which(b_Shoot$CASE_STATUS == "OPEN"),"CASE_STATUS"]), icon = icon("clock"),
                color = "yellow"
        )
    })

    output$CasesClosed <- renderInfoBox({
        infoBox(title = "Closed Cases",
                length(b_Shoot[which(b_Shoot$CASE_STATUS == "CLOSED"),"CASE_STATUS"]), icon = icon("check"),
                color = "green"
        )
    })
    
    output$Warrents <- renderInfoBox({
        infoBox(title = "Warrents Issued",
                length(b_Shoot[which(b_Shoot$CASE_STATUS == "OPEN/WARRANT"),"CASE_STATUS"]), icon = icon("warning"),
                color = "orange"
        )
    })
    
    output$Victims <- renderInfoBox({
        infoBox(title = "Victims",
                length(b_Shoot[which(b_Shoot$DESCRIPTION == "VICTIM"),"DESCRIPTION"]), icon = icon("address-card"),
                color = "maroon"
        )
    })
    
    
    #Plots
    
    output$vctm = renderPlot({
        ggplot(data = b_Shoot) + 
            geom_bar(aes(x = RACE, fill = DESCRIPTION), stat = "count") + 
            ggtitle("Race and Gender wise victims") + 
            facet_grid(.~SEX) + 
            theme(axis.text.x = element_text(angle = 90)) + 
            xlab("Race of Victim") + 
            ylab("Number of Victims")
    })
    
    output$vctmAge = renderPlot({
        ggplot(b_Shoot[which(b_Shoot$DESCRIPTION =="VICTIM"),]) + 
            geom_density(aes(x = age_at_case, fill = RACE), stat = "density", alpha = .6) + 
            facet_grid(SEX~.) + 
            ggtitle("Age distribution of victims", subtitle = "By gender and age of victim") + 
            xlab("Age of Victim") + 
            ylab("Density")
            
    })
    
    output$suspects = renderPlot({
        ggplot(b_Shoot[which(b_Shoot$DESCRIPTION =="SUSPECT"),]) + 
            geom_density(aes(x = age_at_case, fill = RACE), stat = "density", alpha = .6) + 
            facet_grid(SEX~.) + 
            ggtitle("Age distribution of suspects", subtitle = "By gender and age of suspect") + 
            xlab("Age of Suspect") + 
            ylab("Density")
        
    })
    
    
    # Plotting high crime rate areas
    cArea = count_(b_Addr,c("lat","lng"))
    
    output$mp = renderLeaflet({
        leaflet(cArea) %>%
            addProviderTiles(provider = providers$CartoDB) %>%
            addHeatmap(lng = ~lng, lat = ~lat, intensity = ~n, blur = 20, max = 0.5,radius = 10)
    })
    
    
    
    
    ####### TAb second
    
    output$TotalAgency_F <- renderInfoBox({
        infoBox(title = "Total Agencies",
                length(unique(n_agency$agency_ori_9)), icon = icon("building"),
                color = "aqua"
        )
    })
    
    output$TotalPop = renderInfoBox({
      meanPop = apply(n_agency[,4:8],2,function(x){
          round(mean(x),0)
      })
      sMean = sum(meanPop)
      
      infoBox(title = "Total Population",
              sMean, icon = icon("users"),
              color = "blue"
      )
    })
    
    output$TotalVictims <- renderInfoBox({
        infoBox(title = "Total Victims",
                sum(n_victim$victim_count), icon = icon("user-friends"),
                color = "maroon"
        )
    })
    
    output$DomesticViol <- renderInfoBox({
        infoBox(title = "Domestic Violence",
                sum(n_victim$domestic_violence_ind), icon = icon("home"),
                color = "yellow"
        )
    })
    
    output$vctmdet = renderPlot({
        ggplot(data = n_victim) + 
            geom_bar(aes(x = victim_race,fill = victim_injury_category), stat = "count") +
            facet_wrap(victim_sex~.) + 
            xlab("Race of Victim") + 
            ylab("Count of Victims") + 
            labs(fill = "Injury Type")
    })
    
    output$vctmAgeDist = renderPlot({
        
       #age_null =  length(n_victim[which(is.na(n_victim$victim_age)),"victim_age"])
       
       #if(age_null > 0 & (!is.null(age_null))){
           
        #   n_victim = n_victim[-which(is.na(n_victim$victim_age)),]
       #}
        n_victim$victim_age = as.numeric(n_victim$victim_age)
        ggplot(data = n_victim) + 
            geom_density(aes(x = victim_age, fill = victim_race), stat = "density", alpha = 0.6) + 
            facet_wrap(victim_injury_category~.) + 
            xlab("Race of Victim") + 
            ylab("Age distribution of victims") + 
            labs(fill = "Race") + 
            ggtitle("Age Distribution if victims", subtitle = "By Race and Injury type")
    })
    
    output$crimeGrowth = renderPlot({
        
        countPerYear_cat = dplyr::count_(n_victim,c("year","victim_injury_category","victim_race"))
        
        ggplot(data = countPerYear_cat) + 
            geom_smooth(aes(x = year, y = n, col = victim_injury_category), se = F) + 
            facet_wrap(victim_race ~.) + 
            xlab("Year of Incidence") + 
            ylab("Number of Incidences") + 
            labs(col = "Injury") + 
            ggtitle("Crime Growth Rate Per Year", subtitle = "Growth by unjury type and the race of victim")
    })
    
    output$weapons = renderPlot({
        ggplot(data = n_victim) + 
            geom_bar(aes(x = victim_injury_category, fill = primary_weapon_category)) + 
            xlab("Injury Type") + 
            ylab("Number of time a weapon is used") + 
            labs(fill = "Weapon") + 
            ggtitle("Commonly used weapons", subtitle = "Used in different type of injuries")
    })
    
    agencyMerge = dplyr::inner_join(r_agency,r_annual_count,by = "agency_ori_7") 
    
    output$casesByYear = renderPlot({
      
      
      ggplot(data = agencyMerge) + 
          geom_smooth(aes(x = year.y,y = actual, col = as.factor("actual")), se = F) +
          geom_smooth(aes(x = year.y,y = cleared, col = as.factor("cleared")), se = F) + 
          ggtitle("Cases Per Year", subtitle = "Cases actually orrcured v/s cases solved") + 
          xlab("Year") + 
          ylab("Number of Cases") + 
          labs(col = "Cases")
    })
    
    output$agencyPerform = renderPlot({
        agencyPerform_sum = agencyMerge %>%
            group_by(agency_name,offense) %>%
            summarise(Tot = sum(cleared))
        
        ggplot(data = agencyPerform_sum) + 
            geom_bar(aes(x = agency_name,y = Tot,fill = offense), stat = "identity") + 
            theme(axis.text.x = element_text(angle = 90)) + 
            xlab("Agency") + 
            ylab("Total Number of Cleared Cases") + 
            ggtitle("Performance of agencies", subtitle = "Performance of agencies categorised by weapon used") + 
            labs(fill = "Offense")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
