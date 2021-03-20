library(shiny)
library(tidyverse)
library(ggplot2)
library(tools)
library(shinydashboard)
library(plotly)
library(rgdal)
library(leaflet)
library(spdplyr)



# load data----------------------------------
load("PA_M_2006_2018_full.RData")

data <- readOGR("municipal_disso.shp",layer = "municipal_disso", GDAL1_integer64_policy = TRUE)

county <- readOGR("tl_2019_us_county.shp",layer = "tl_2019_us_county", GDAL1_integer64_policy = TRUE) 

county_bound <- subset(county, STATEFP == 42) 


PA_M_2006_2018_full <- subset(PA_M_2006_2018_full, full_municipal_name != "County Subdivisions Not Defined, Erie County, PA")
data_1 <- subset(data, NAME_x != "County subdivisions not defined")

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
            
            # create county filters
            selectInput(
                inputId = "county_1",
                label = "County Group(Type to search, backspace to delete)",
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
                    
                    # download button
                    fluidRow(
                        box(downloadButton(
                        outputId = "download",
                        label = "Download Data Table")
                        )),
                        
                        br(),
                    
                    fluidRow(
                        box(DT::dataTableOutput(outputId = "table1"), width = 12
                        )
                    )
            ),
            # Second page---------------------
            tabItem(tabName = "Source",
                    h2("Source"),
                    
                    fluidRow(
                        box(uiOutput("source_1"),
                            width = 12
                        )
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
            filter((Reporting_Year == max(input$year))) %>% 
            filter((Municipality_Type %in% input$type))
        
        if (length(input$county_1) > 0) {
            a <- a %>% 
                filter((county_name %in% input$county_1) | 
                           (county_name_2a %in% input$county_1)) 
        }
        return(a)
    })
    
    # create reactive subset for multiple years data
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
            filter(Indicator == input$indicator) 
        
        # remove NA shapes
        data_2 <- subset(data_1, GEOID %in% unique(u2$GEOID))
        
    # merging of spatial with data
    data_2@data <- left_join(data_2@data,u2, by = "GEOID") 
    

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
                            fillOpacity = 1,
                            weight = 1,
                            highlight = highlightOptions(
                                weight = 5,
                                color = "black",
                                fillOpacity = 1,
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

    
    # reset zoom level------------------------------
    observeEvent(input$reset,{
        mapdata <- mapdata_1()
        leafletProxy("map", data = mapdata) %>% 
            setView(lng = -77.5000, lat = 41.2033, zoom = 7)
    })
    
    # creating county subset
    county_a <- reactive({
        if (length(input$county_1) > 0) {
            county_1 <- subset(county_bound, NAME %in% PA_subset()$county_name)
        } else{
        county_1 <- county_bound
        }
        return(county_1)
    })
    
    # add debounce---------------------------------
    # added to make sure the creation of the lines are done last when county selection changes
    county_1 <- debounce(county_a, 3000)
    
    # add lines for county boundaries 
    observe({
    leafletProxy("map", data = county_1()) %>%
        # clear plot  
        clearGroup("county") %>%
        
        addPolygons(weight = 3,
                    fill = FALSE,
                    color = "black",
                    group = "county")
    })
    
    ### line charts----------------------------------
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
                # create line chart
                geom_line(aes(color = Indicator)) +
                # create points on line chart
                geom_point(aes(color = Indicator)) +
                # pick the year portion of the date and axis will be seperated by 3 years
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
        
        ### relevel type
        med_subset_1$Type <- factor(med_subset_1$Type, levels = c("Others","Intergovernmental","Charges & Fees","Tax", "Total"))
        
        ### plot
        ggplotly(
            ggplot(data = med_subset_1, aes(x = Type, y = share)) +
                geom_bar(stat = "identity", position = "dodge") +
                xlab("Revenue Type") + 
                ylab("Median Share of Total Revenue(%)") +
                labs(title = (paste("Types of Revenues In", max(input$year))))+ 
                # flip x and y axis
                coord_flip(),
            tooltip = c("y")    
        )
    })
    
    
    ### Create data table page 1-------------------------------
    output$table1 <- DT::renderDataTable({
        a <- DT::datatable(data = PA_subset_time()[,c(2:3,7:9,11,12,14,16,18)], 
                           options = list(pageLength = 10), 
                           rownames = FALSE,
                           colnames = c("Year" = "Reporting_Year",
                                        "Municipal" = "full_municipal_name",
                                        "Type" = "Municipality_Type",
                                        "Revenues" = "Total_Revenues",
                                        "Surplus\nOr Deficits" = "Revenues_Over_Expenditures",
                                        "Tax" = "Total_Taxes_Revenues",
                                       "Charges\n& Fees" = "total_charges_and_fees_Revenues",  
                                        "Intergovernmental" = "total_intergovernmental_Revenues",
                                       "Others" = "total_other_Revenues",
                                       "Pop" = "Population"
                           ),
                           style = 'bootstrap',
                           caption = 'Table 1: Revenue Breakdown',
                           filter = 'top'
        ) %>% 
            DT::formatCurrency(5:10, digits = 0)
        
        return(a)
    })
    
    # download data table -----------------------------
    
    output$download <- downloadHandler(
        filename = function() {
            paste("PA_From", min(input$year), "To", max(input$year), ".csv", 
                  sep = "")
        },
        content = function(file) {
            b <- PA_subset_time() %>% 
                select(-c(date,county_name,county_name_2a,deficit))
            write.csv(b, file, row.names = FALSE)
        }
    )
    
    # source text -----------------------------
    output$source_1 <- renderUI({
        url_1 <- tags$a(href="http://munstats.pa.gov/Reports/ReportInformation2.aspx?report=StatewideMuniAfr", "PA Department of Community & Economic Development")
        url_2 <- tags$a(href="https://www.census.gov/cgi-bin/geo/shapefiles/index.php", "Census Shapefile")
        
        tags$div(
            h1("This Data Set was cleaned and merged using the following sources:"),
            br(),
            tagList("1. Municipal Data:", url_1 ),
            br(),
            tagList("2. Municipal Names:", url_2 ),
            br(),
            p("To get accurate municipal names and its corresponding counties, I have to refer to census data. However, some municipalites which are under two counties will be split into two parts in the census shapefile data. Thus, some processing is needed to combine them back into one municipalities. Indicators were derived from the financial numbers of DCED.")
        )
    })
        


    }

# Run the application 
shinyApp(ui = ui, server = server)
