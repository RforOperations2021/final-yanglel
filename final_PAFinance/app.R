library(shiny)
library(tidyverse)
library(ggplot2)
library(tools)
library(shinydashboard)
library(plotly)
require(rgdal)
require(leaflet)

# load data----------------------------------
data <- readOGR(".", "municipal_disso")
PA_M_2006_2018_full <- read_csv("PA_M_2006_2018_full.csv",
                 col_types = cols(county_name_2 = col_character(),
                                  county_name_type_2 = col_character(),
                                  GEOID = col_character()))

data@data <- merge(data@data, PA_M_2006_2018_full, by = "GEOID")

year_min <- min(PA_M_2006_2018_full$Reporting_Year)
year_max <- max(PA_M_2006_2018_full$Reporting_Year)
# get list of county names
county <- unique(c(PA_M_2006_2018_full$county_name, PA_M_2006_2018_full$county_name_2))

# create date variable
PA_M_2006_2018_full$date <- as.Date(
    paste(PA_M_2006_2018_full$Reporting_Year, 1, 1, sep="-"))

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
            
            # create radio button for indicator choice
            radioButtons(
                inputId = "indicator",
                label = "Revenue Indicator",
                choices = c("Surplus/Deficit",
                            "Revenue Per Capita",
                            "Total Revenue"),
                selected = "Surplus/Deficit"
                
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
    )
)
            
            


# Define server logic required to draw a histogram
server <- function(input, output) {


    }

# Run the application 
shinyApp(ui = ui, server = server)
