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

data_1 <- subset(data, GEOID != 4204900000)

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
            )
            
            )
        ),
    # create body--------------------------------
    dashboardBody(
        tabItems(
            ### first page------------------------
            tabItem(tabName = "RA",
                    h2("Revenue Breakdown"),
                        
                    fluidRow(    
                        box(
                            leafletOutput("map"), 
                            width = 12)
                    ),
                    fluidRow(
                        box(plotlyOutput("line"), 
                            height= "450px"),
                        
                        box(plotlyOutput("bar"),
                            height= "450px")
                    ),
                    
                    fluidRow(
                        box(DT::dataTableOutput(outputId = "table2"), width = 12)
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
    
    # add debounce---------------------------------
    PA_subset <- debounce(PA_subset_d, 2000)
    
    PA_subset_time <- debounce(PA_subset_time_d, 2000)
    
    # create polygons with data----------------------------
    mapdata_1 <- reactive({
        
        # creating long table for filtering
        pivot_measure <- colnames(PA_subset())[c(9:19)]

        u2 <- PA_subset() %>%
            pivot_longer(
                cols = all_of(pivot_measure),
                names_to = "Indicator",
                values_to = "Amount"
            ) %>% 
            filter(Indicator == input$indicator) %>% 
            filter(!is.na(Amount))
        
        data_2 <- data_1[data_1$GEOID %in% u2$GEOID,]
        
    # merging of spatial with data
    data_2@data <- merge(data_2@data,u2, by.x = "GEOID", by.y = "GEOID") 
    

    return(data_2)
    })
    
    # Create leaflet map------------------------------
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>% 
            setView(lng = -77.5000, lat = 41.2033, zoom = 7)
        })
    
    
    # mapping changes to input---------------------
    observe({
        mapdata <- mapdata_1()
        
        # create color palette
        pal <- colorQuantile("RdBu", domain = mapdata$Amount, n = 5)
    
        leafletProxy("map", data = mapdata) %>% 
            # clear plot  
            clearGroup("all") %>%
             
            # clear legend
            clearControls() %>%
            
            addPolygons(fillColor = ~pal(Amount),
                            fillOpacity = 0.7,
                            weight = 1,
                            highlight = highlightOptions(
                                weight = 5,
                                color = "black",
                                fillOpacity = 0.7,
                                bringToFront = TRUE),
                            popup = ~paste(
                                full_municipal_name, "<br/>",
                                Reporting_Year, "<br/>",
                                paste("$", (scales::comma_format()(round(Amount,0) )))),
                            group = "all") %>%
            
            addLegend(pal = pal, 
                      values = ~Amount, 
                      opacity = 0.7, 
                      title = paste(max(input$year), input$indicator,"<br/>", "Quantile"), position = "bottomright")
        })

    
    # reset zoom level
    observeEvent(input$reset,{
        mapdata <- mapdata_1()
        leafletProxy("map", data = mapdata) %>% 
            setView(lng = -77.5000, lat = 41.2033, zoom = 7)
    })
    
    ### charts for second page------------------------------
    ### line charts
    output$line <- renderPlotly({
        
        # change to longer to get indicator
        pivot_measure <- colnames(PA_subset_time())[c(9:19)]
        
        u3 <- PA_subset_time() %>%
            pivot_longer(
                cols = all_of(pivot_measure),
                names_to = "Indicator",
                values_to = "Amount"
            ) %>% 
            filter(Indicator %in% c("Taxes_Per_Capita", "charges_and_fees_per_capita", "intergovernmental_per_capita", "other_Revenues_per_capita" )) %>% 
            filter(!is.na(Amount))
        
        # create table of median
        med_subset_time_1 <- u3 %>%
            group_by(Indicator,date) %>%
            summarize(median = median(Amount, na.rm = T))
        
        ggplotly(
            ggplot(data = med_subset_time_1, aes(x = date, y = median)) +
                geom_line(aes(color = Indicator)) +
                geom_point(aes(color = Indicator)) +
                scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
                xlab("Year") +
                ylab("Revenue Per Capita") +
                labs(title = paste(
                    "Types of Revenue Per Capita (Median)", "\nFrom",min(input$year), "To", max(input$year)))+ 
                scale_color_discrete(name = "Indicator"),
            tooltip = c("x", "y")
        )
    })
    
    ### bar chart---------------------------
    output$bar <- renderPlotly({

        # create table of median percentage share
        med_subset_1 <- PA_subset() %>%
            mutate(
                total_share = Total_Revenues/Total_Revenues,
                tax_share = Total_Taxes_Revenues/Total_Revenues,
                cf_share = total_charges_and_fees_Revenues/Total_Revenues,
                ig_share = total_intergovernmental_Revenues/Total_Revenues,
                other_share = total_other_Revenues/Total_Revenues) %>% 
            summarize(
                Total = round(((median(total_share, na.rm = T))*100),0),
                Tax = round(((median(tax_share, na.rm = T))*100),0),
                "Charges & Fees" = round(((median(cf_share, na.rm = T))*100),0),
                Intergovernmental = round(((median(ig_share, na.rm = T))*100),0),
                Others = round(((median(other_share, na.rm = T))*100),0)
            ) %>% 
            pivot_longer(c("Total","Tax","Charges & Fees","Intergovernmental", "Others"), names_to = "Type", values_to = "share")
        
        med_subset_1$Type <- factor(med_subset_1$Type, levels = c("Others","Intergovernmental","Charges & Fees","Tax", "Total"))
        
        ### plot
        ggplotly(
            ggplot(data = med_subset_1, aes(x = Type, y = share)) +
                geom_bar(stat = "identity", position = "dodge") +
                xlab("Revenue Type") + 
                ylab("Median Share of Total Revenue(%)") +
                labs(title = (paste("Types of Revenues In", max(input$year))))+ 
                coord_flip(),
            tooltip = c("y")    
        )
    })
        


    }

# Run the application 
shinyApp(ui = ui, server = server)
