library(shiny)
library(tidyverse)
library(ggplot2)
library(tools)
library(shinydashboard)
library(plotly)
require(rgdal)
require(leaflet)

# load data----------------------------------
data <- readOGR("municipal_disso.shp",layer = "municipal_disso", GDAL1_integer64_policy = TRUE)
load("PA_M_2006_2018_full.RData")

#data@data <- merge(data@data, PA_M_2006_2018_full, by = "GEOID")

# get number of years
year_min <- min(PA_M_2006_2018_full$Reporting_Year)
year_max <- max(PA_M_2006_2018_full$Reporting_Year)

# get list of county names
county <- unique(c(PA_M_2006_2018_full$county_name, PA_M_2006_2018_full$county_name_2a))

# create date variable
PA_M_2006_2018_full$date <- as.Date(
    paste(PA_M_2006_2018_full$Reporting_Year, 1, 1, sep="-"))

# get names of indicators 

indi_name <- colnames(PA_M_2006_2018_full)[9:19]

# Avoid plotly issues -----------------------------------
pdf(NULL)



# Define UI for application that draws a histogram
ui <- dashboardPage(
    # change dashboard color
    skin = "black",
    
    # create header--------------------------------------
    dashboardHeader(
        title = "PA Municipalities Public Finance Data (2006-2018)",
        titleWidth = 500
    ),
    
    # create sidebar ----------------------------------------
    dashboardSidebar(
        sidebarMenu(
            menuItem("Revenue Analysis", 
                     tabName = "RA", 
                     icon = icon("money-bill-wave")),
            
            menuItem("Sources and Download", 
                     tabName = "Source", 
                     icon = icon("download")),
            
            # reset zoom button
            actionButton(
                inputId = "reset",
                label = "Return to Default Zoom"
            ),
            
            # create filters
            sliderInput(
                inputId = "year",
                label = "Year",
                min = year_min,
                max = year_max,
                step = 1,
                value = c(year_min, year_max),
                dragRange = T,
                sep = "",
                ticks = F
            ),
            
            # check box for municipal type
            checkboxGroupInput(
                inputId = "type",
                label = "Municipality Type",
                choices = c("City",
                            "Borough",
                            "First Class Township",
                            "Second Class Township"
                ),
                selected = c("City",
                             "Borough",
                             "First Class Township",
                             "Second Class Township"
                )
            ),
            
            # create dropdown box for indicator choice
            selectInput(
                inputId = "indicator",
                label = "Indicator For Map",
                choices = indi_name,
                selected = indi_name[3]
            ),
            
            # create first county comparison filters
            selectInput(
                inputId = "county_1",
                label = "County Group 1 (Type to search, backspace to delete)",
                choices = county,
                selectize = T ,
                multiple =  T
            ),
            
            # create second county comparison group
            selectInput(
                inputId = "county_2",
                label = "County Group 2 (Type to search, backspace to delete)",
                choices = county,
                selectize = T ,
                multiple =  T
            )
            
            )
        ),
    # create body--------------------------------
    dashboardBody(
        tabItems(
            ### first page------------------------
            tabItem(tabName = "RA",
                    h2("Revenue Breakdown"),
                    h4("Is there enough population to create revenue?"),
                    # fluidRow(
                    #     "Group 1",
                    #     valueBoxOutput("YearOfDeficit", width = 3 ),
                    #     valueBoxOutput("Numberofdeficits", width = 3),
                    #     valueBoxOutput("DeficitSize", width = 3)
                    # ),
                        
                    fluidRow(    
                        box(
                            leafletOutput("map"), 
                            width = 12)
                    )
    )
)
)
)

            
            


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # subset data -----------------------------------
    # reactive subsetting for single year charts and map 
    PA_subset_d <- reactive({
        req(input$year)
        req(input$type)
        a <- PA_M_2006_2018_full %>% 
            filter((Reporting_Year == max(input$year)) & 
                       (Municipality_Type %in% input$type))
        
        if (length(input$county_1) > 0) {
            a <- a %>% 
                filter((county_name %in% input$county_1) | 
                           (county_name_2a %in% input$county_1)) 
        }
        return(a)
    })
    
    # create reative subset for mutiple years data
    PA_subset_time_d <- reactive({
        req(input$year)
        req(input$type) 
        a <- PA_M_2006_2018_full %>% 
            filter((Reporting_Year <= max(input$year)) &
                       (Reporting_Year >= min(input$year)) &
                       (Municipality_Type %in% input$type))
        if (length(input$county_1) > 0) {
            a <- a %>% 
                filter((county_name %in% input$county_1) | 
                           (county_name_2a %in% input$county_1)) 
        }
        return(a)
    })
    
    # add debounce
    PA_subset <- debounce(PA_subset_d, 5000)
    
    PA_subset_time <- debounce(PA_subset_time_d, 5000)
    
    
    # Create leaflet map------------------------------
    output$map <- renderLeaflet({
        
        # create polygons with data
        data@data <- merge(data@data, PA_subset(), by = "GEOID")
        
        mapdata <- data
        
        # pal <- reactive({
        # <- input$indicator
        # colorNumeric(
        #     palette = "Purples",
        #     NULL)
        #     })
        
        Rmap <- leaflet(mapdata) %>%
                addProviderTiles(providers$CartoDB.Positron) %>% 
                addPolygons()
        
        observeEvent(input$reset,{
            
        Rmap <- Rmap %>% 
            clearBounds()
        })
        
        return(Rmap)
        
        
        
        # %>% 
        #         addPolygons(color = ~pal(input$indicator))
        
        

        
        # leaflet(data = cds) %>%
        #     addProviderTiles("Stamen.Toner") %>%
        #     addPolygons(color = ~pal(`Life Expectancy at Birth (years)`)
            # addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
            # setView(-74.0060, 40.7128, 9) %>%
            # addLayersControl(baseGroups = c("Google", "Wiki"))
    })
        


    }

# Run the application 
shinyApp(ui = ui, server = server)
