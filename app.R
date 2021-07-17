# R Shiny App For ACS Census Data Viewing
##############################################################################
# Libraries
library(shiny)
library(shinyjs) #For javascript collapse box button
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(plotly)
library(tidytransit) # For read_gtfs
library(shinyWidgets) # For pickerInput
library(data.table)
library(leaflet)
library(sf)
library(gtools) # For mixed sort
library(DT)
library(stringr) # For string formatting
library(tidycensus)   # For downloading Census data
library(tmap)         # For creating tmap
library(tmaptools)    # For reading and processing spatial data related to tmap
library(tigris) # Get zip code list
library(scales)
library(gtfsFunctions) #devtools::install_github("b-tomhave/gtfsFunctions", force = TRUE)
library(plyr) # For round_any

# Install these to use the aggregate_map function that was previously in tmaptools
# library(devtools)
# install_github("mtennekes/oldtmaptools")
library(oldtmaptools)

# Allow input zip file to be up to 200mb in size
options(shiny.maxRequestSize = 200*1024^2)

# Load tigris as sf object
options(tigris_class = "sf")
# Cache acs data for quicker loading
options(tigris_use_cache = TRUE)

Sys.getenv("CENSUS_KEY")

# Set Inputs
acsYear = 2016
palletBinNumber = 5

# Quintile Percent Bins
percentBins <- c(0,.05,0.1,0.25,0.5,0.75,1)
labels_PctBins <- c("< 5%", "5% - 10%", "10% - 25%", "25% - 50%","50% - 75%", "> 75%")


# define a little helper function to format dollars for map
make_dollar <- function(x, digits = 0) {
    paste0("$", formatC(x, digits = digits, format = "f", big.mark = ","))
}


fips_codes <- tidycensus::fips_codes

# Get MSAs
msas <- tigris::core_based_statistical_areas(cb = TRUE)

# Create Table of Potential Variables/ACS Tables to Search
possibleTablesStatic <- load_variables(year = acsYear, dataset = "acs5")%>%
                                select(tableName ="name", concept, label)



# Tab 1 ACS Filter Map Variables
filterMapVars <- list(list("B01003_001", "[B01003_001] Total Population"),
                      list("B01002_001", "[B01002_001] Median Age"),
                      list("B01001_011", "[B01001_011] Male 25-29"),
                      list("B01001_012", "[B01001_012] Male 30-34"),
                      list("B01001_013", "[B01001_013] Male 35-39"),
                      list("B01001_014", "[B01001_014] Male 40-44"),
                      list("B01001_015", "[B01001_015] Male 45-49"),
                      list("B01001_035", "[B01001_011] Female 25-29"),
                      list("B01001_036", "[B01001_012] Female 30-34"),
                      list("B01001_037", "[B01001_013] Female 35-39"),
                      list("B01001_038", "[B01001_014] Female 40-44"),
                      list("B01001_039", "[B01001_015] Female 45-49"),
                      list("B02001_002", "[B02001_002] Population (White Alone)"),
                      list("B02001_003", "[B02001_003] Population (Black Alone)"),
                      list("B02001_004", "[B02001_004] Population (American Indian/Alaska Native Alone)"),
                      list("B02001_005", "[B02001_005] Population (Asian Alone)"),
                      list("B02001_006", "[B02001_006] Population (Hawaiian/Pacific Islander Alone)"),
                      list("B02001_007", "[B02001_007] Population (Other Race Alone)"),
                      list("B02001_008", "[B02001_008] Population (2+ Race)"),
                      list("B03001_003", "[B03001_003] Population (Hispaic/Latino)"),
                      list("B08101_025", "[B08101_025] Population Taking Transit To Work"),
                      list("B08101_033", "[B08101_033] Population Walking To Work"),
                      list("B17001_002", "[B17001_002] Population with below poverty-level income"),
                      list("B17001_031", "[B17001_031] Population with at or above poverty-level income"),
                      list("B19013_001", "[B19013_001] Median Household income in past 12 months (in selected year $)"),
                      list("B25064_001", "[B25064_001] Median Gross Rent"),
                      list("B08201_002", "[B08201_002] Zero-Vehicle Households"))

filterMapVarName <- unlist(lapply(filterMapVars, `[[`, 1))
filterMapVarDescription <- unlist(lapply(filterMapVars, `[[`, 2))
names(filterMapVarName) <- filterMapVarDescription



# Tab 2 Key Vars 
keyVarList <- list(list("B01003_001", "[B01003_001] Total Population"),
                   list("B01001_002", "[B01001_002] Total Male Population (All Race)"),
                   list("B01001_026", "[B01001_026] Total Female Population (All Race)"),
                   list("B01002_001", "[B01002_001] Median Age"),
                   list("B01002_002", "[B01002_002] Median Age (Male-All Races)"),
                   list("B01002_003", "[B01002_003] Median Age (Female-All Races)"),
                   list("B02001_002", "[B02001_002] Population (White Alone)"),
                   list("B02001_003", "[B02001_003] Population (Black Alone)"),
                   list("B02001_004", "[B02001_004] Population (American Indian/Alaska Native Alone)"),
                   list("B02001_005", "[B02001_005] Population (Asian Alone)"),
                   list("B02001_006", "[B02001_006] Population (Hawaiian/Pacific Islander Alone)"),
                   list("B02001_007", "[B02001_007] Population (Other Race Alone)"),
                   list("B02001_008", "[B02001_008] Population (2+ Race)"),
                   list("B03001_003", "[B03001_003] Population (Hispaic/Latino)"),
                   list("B08101_009", "[B08101_009] Population Driving Alone To Work"),
                   list("B08101_025", "[B08101_025] Population Taking Transit To Work"),
                   list("B08101_033", "[B08101_033] Population Walking To Work"),
                   list("B08201_002", "[B08201_002] Zero-Vehicle Households"),
                   list("B09001_001", "[B09001_001] Under-18 population"),
                   list("B17001_002", "[B17001_002] Population with below poverty-level income"),
                   list("B17001_031", "[B17001_031] Population with at or above poverty-level income"),
                   list("B19013_001", "[B19013_001] Median Household income in past 12 months (in selected year $)"),
                   list("B25064_001", "[B25064_001] Median Gross Rent"))

keyVarName <- unlist(lapply(keyVarList, `[[`, 1))
keyVarDescription <- unlist(lapply(keyVarList, `[[`, 2))
names(keyVarName) <- keyVarDescription


##############################################################################
# UI Side of App
##############################################################################
ui <-navbarPage("Routes & Stops Viewer", id="nav",
                # Map Page
                
                # Tab 1: Filtered ACS Mapping ----------------------------------
                tabPanel("Filtered ACS Mapping",
                         div(class="outer",
                             
                             tags$head(
                                 # Include custom CSS
                                 includeCSS("styles.css")
                             ),
                             tmapOutput("filteredAcsMap", width="100%", height="100%"),
                             # Input Selections for Tab 2
                             absolutePanel(id = "controls2", class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                           width = 330, height = "auto",
                                           
                                           radioButtons("inputGeomMethod", label = h4("Select ACS Geometry Method"),
                                                        choices = list("State Abbreviations" = 1,
                                                                       "GTFS Service Area Estimation" = 2), 
                                                        selected = 1),
                                           
                                           # If State Abbreviations Input Selected Show That Option
                                           conditionalPanel(
                                               "input.inputGeomMethod == 1",
                                               selectInput(
                                                   "acsStateSelect2",
                                                   label = h4("Select State(s) To View Census Tracts"),
                                                   choices = unique(fips_codes$state),
                                                   selected = NULL,
                                                   multiple = TRUE,
                                                   selectize = TRUE
                                               )
                                           ),
                                           
                                           # If GTFS Input Selection Show That Option
                                           conditionalPanel(
                                               "input.inputGeomMethod == 2",
                                               fileInput("selectInputFile", h5("Select GTFS Zip File:"),
                                                         multiple = FALSE,
                                                         accept = ".zip")
                                           ),
                                           actionButton("loadDataButton2", "Map Data"),
                                           br(),br(),
                                           
                                           # Map Filters
                                           h4('Adjust Sliders to Filter Tracts'),
                                           uiOutput("formattedPctAge25_50Slider"),
                                           uiOutput("formattedPctBipocSlider"),
                                           uiOutput("formattedHHIncomeSlider"),
                                           uiOutput("formattedPctBelowPovertySlider"),
                                           br(),
                                           uiOutput("acsTableSelect2UI"),
                                           paste("Data From:", acsYear, "ACS")
                             )
                         )),
                
                
                
                # Tab 2: Explore ACS Variables ----------------------------------
                tabPanel("Explore ACS Variables",
                         div(class="outer",
                             
                             tags$head(
                                 # Include custom CSS
                                 includeCSS("styles.css")
                             ),
                             
                             # If not using custom CSS, set height of leafletOutput to a number instead of percent
                             tmapOutput("acsMap", width="100%", height="100%"),
                             
                             # Shiny versions prior to 0.11 should use class = "modal" instead.
                             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                           width = 330, height = "auto",
                                           
                                           h2("ACS Viewer"),
                                           helpText("Initial load time of 5-10 seconds."),
                                           selectInput(
                                               "acsStateSelect",
                                               label = h4("Select State(s) To View Census Tracts"),
                                               choices = unique(fips_codes$state),
                                               selected = NULL,
                                               multiple = TRUE,
                                               selectize = TRUE
                                           ),
                                           selectInput(
                                               "keyVarSwitch",
                                               label = h4("ACS Table/Variable Filter"),
                                               choices = c("Key Variables Only", "All Variables"),
                                               selected = "Key Variables Only", 
                                               multiple = F,
                                               selectize = TRUE
                                           ),
                                           selectizeInput('acsTableSelect',
                                                          label = 'Select Table/Variable Name',
                                                          choices = NULL),
                                           prettySwitch(
                                               inputId = "pctTotalPopSwitch",
                                               label   = "Scale Variable by Total Tract Population",
                                               fill    = TRUE,
                                               status  = "primary",
                                               value = F),
                                           actionButton("loadDataButton", "Map Data"),
                                           br(),br(),
                                           paste("Data From:", acsYear, "ACS")
                             )
                         )
                ),
        
                
                
                
                
                # Tab 3: ACS Lookup Table of Variables ---------------------------
                tabPanel("ACS Lookup Table",
                         h2("Possible ACS Tables for Mapping"),
                         br(),
                         DT::dataTableOutput("possibleTables")
                )
)

##############################################################################
# Server Side of App
##############################################################################
server <- function(input, output, session, ...) {
    
    # Tab 1: ACS Map Filtering ---------------------------------
    # Create Render UI for GTFS Input box
    #inputGeomMethod
    

    # Update acs data selection based on file selection and get all variables specified in filterMapVars
    observeEvent(input$loadDataButton2,{
        # Load Data Differently Depending on if by state abbreviation of GTFS
        # If by state abbreviation...
        if (input$inputGeomMethod == 1){
            filteredACSTracts <- get_acs(
                geography = "tract",
                variables = as.character(filterMapVarName),
                state = input$acsStateSelect2,
                year = acsYear,
                survey = "acs5",
                geometry = TRUE,
                output = "wide" # get data in wide format for easier mapping
            )
        }
        else if(input$inputGeomMethod == 2){
            filteredACSTracts <- gtfsFunctions::getServiceAreaACS(gtfsFunctions::formatGTFSObject(input$selectInputFile$datapath),
                                                                  variables = as.character(filterMapVarName),
                                                                  geography = "tract",
                                                                  year = acsYear,
                                                                  survey = "acs5")
        }
        

        # Create Custom ACS Columns
        filteredACSTracts$TotalPop <- filteredACSTracts$B01003_001E
        # Remove Tracts with No Pop
        filteredACSTracts <- filteredACSTracts[!(is.na(filteredACSTracts$TotalPop) | filteredACSTracts$TotalPop ==0),]
        
        # Race Vars as Pct of Total Tract Pop
        filteredACSTracts$PctWhite        <- round(100*(filteredACSTracts$B02001_002E/filteredACSTracts$TotalPop),1)
        filteredACSTracts$PctBIPOC        <-  100 - filteredACSTracts$PctWhite 
        filteredACSTracts$PctBlack        <- round(100*(filteredACSTracts$B02001_003E/filteredACSTracts$TotalPop),1)
        filteredACSTracts$PctNative       <- round(100*(filteredACSTracts$B02001_004E/filteredACSTracts$TotalPop),1)
        filteredACSTracts$PctAsian        <- round(100*(filteredACSTracts$B02001_005E/filteredACSTracts$TotalPop),1)
        filteredACSTracts$PctPacIsland    <- round(100*(filteredACSTracts$B02001_006E/filteredACSTracts$TotalPop),1)
        filteredACSTracts$PctOtherRace    <- round(100*(filteredACSTracts$B02001_007E/filteredACSTracts$TotalPop),1)
        filteredACSTracts$PctTwoPlusRace  <- round(100*(filteredACSTracts$B02001_008E/filteredACSTracts$TotalPop),1)
        filteredACSTracts$PctHispLatino   <- round(100*(filteredACSTracts$B03001_003E/filteredACSTracts$TotalPop),1)
        
        # 25-50 Age Pct of Total Tract Pop
        filteredACSTracts$Male_Age25_50   <- filteredACSTracts$B01001_011E + filteredACSTracts$B01001_012E + filteredACSTracts$B01001_013E + filteredACSTracts$B01001_014E +filteredACSTracts$B01001_015E
        filteredACSTracts$Female_Age25_50   <- filteredACSTracts$B01001_035E + filteredACSTracts$B01001_036E + filteredACSTracts$B01001_037E + filteredACSTracts$B01001_038E +filteredACSTracts$B01001_039E
        filteredACSTracts$Total_Age25_50   <- filteredACSTracts$Male_Age25_50 + filteredACSTracts$Female_Age25_50 
        
        filteredACSTracts$Pct_Male_Age25_50    <- round(100*(filteredACSTracts$Male_Age25_50 / filteredACSTracts$TotalPop),1)
        filteredACSTracts$Pct_Female_Age25_50  <- round(100*(filteredACSTracts$Female_Age25_50 / filteredACSTracts$TotalPop),1)
        filteredACSTracts$Pct_Total_Age25_50   <- round(100*(filteredACSTracts$Total_Age25_50 / filteredACSTracts$TotalPop),1)
        
        
        # Other Vars as Pct of Total Tract Pop
        filteredACSTracts$PctTransit2Wrk   <- round(100*(filteredACSTracts$B08101_025E/filteredACSTracts$TotalPop),1)
        filteredACSTracts$PctWalk2Wrk      <- round(100*(filteredACSTracts$B08101_033E/filteredACSTracts$TotalPop),1)
        filteredACSTracts$PctBelowPoverty     <- round(100*(filteredACSTracts$B17001_002E/filteredACSTracts$TotalPop),1)
        filteredACSTracts$PctAtOrAbvPoverty   <- round(100*(filteredACSTracts$B17001_031E/filteredACSTracts$TotalPop),1)
        filteredACSTracts$PctZeroCarHH        <- round(100*(filteredACSTracts$B08201_002E/filteredACSTracts$TotalPop),1)
        
        # Other Non Pct Vars
        filteredACSTracts$MedianHHInc   <- filteredACSTracts$B19013_001E
        filteredACSTracts$MedianGrossRent   <- filteredACSTracts$B25064_001E
        
        finalFilteredVarsList <- list(list("TotalPop", "Total Population"),
                                      list("Pct_Total_Age25_50", "% Population Ages 25-50"),
                                      list("PctWhite","% Population (White)"),
                                      list("PctBIPOC","% Population (BIPOC)"),
                                      list("PctBlack","% Population (Black)"),
                                      list("PctAsian","% Population (Asian)"),
                                      list("PctHispLatino","% Population (Hispanic/Latino)"),
                                      list("PctZeroCarHH","% 0-Car HH"),
                                      list("PctTransit2Wrk","% Transit to Work"),
                                      list("PctWalk2Wrk","% Walk to Work"),
                                      list("PctBelowPoverty","% Below Poverty Line"),
                                      list("PctAtOrAbvPoverty","% At or Above Poverty Line"),
                                      list("MedianHHInc","Median HH Income (2016$)"),
                                      list("MedianGrossRent","Median Gross Rent (2016$)"))
        
        formatted_finalFilteredVarsList <- unlist(lapply(finalFilteredVarsList, `[[`, 1))
        formattedDescription <- unlist(lapply(finalFilteredVarsList, `[[`, 2))
        names(formatted_finalFilteredVarsList) <- formattedDescription
        
        # Filter Filtered Df By Column 
        filteredACSTracts <- filteredACSTracts[as.character(formatted_finalFilteredVarsList)]
        
        # Set Min & Max Values for Each Slider
        minHHIncome = round_any(min(na.omit(filteredACSTracts$MedianHHInc)),
                                10000, f = floor)
        
        maxHHIncome = round_any(max(na.omit(filteredACSTracts$MedianHHInc)),
                                10000, f = ceiling)
        
        minPctBipoc = 0
        maxPctBipoc = round_any(max(na.omit(filteredACSTracts$PctBIPOC)),
                                5, f = ceiling)
        
        minPctAge25_50 = 0
        maxPctAge25_50 = round_any(max(na.omit(filteredACSTracts$Pct_Total_Age25_50)),
                                   5, f = ceiling)
        
        minPctBelowPov = 0
        maxPctBelowPov = round_any(max(na.omit(filteredACSTracts$PctBelowPoverty)),
                                   5, f = ceiling)
    
    
        # Formatted Slider Inputs
        output$formattedPctAge25_50Slider <- renderUI({
            shinyWidgets::noUiSliderInput(
                inputId = "formattedPctAge25_50Slider_raw",
                label = "Tract % Aged 25-50",
                min = minPctAge25_50, max = maxPctAge25_50,
                margin = 10,
                value = c(minPctAge25_50, maxPctAge25_50),
                step = 5,
                behaviour =  c("snap"),
                format = wNumbFormat(decimals = 0,
                                     thousand = ",",
                                     suffix = "%")
            )
        })
        
        
        output$formattedPctBipocSlider <- renderUI({
            shinyWidgets::noUiSliderInput(
                inputId = "formattedPctBipocSlider_raw",
                label = "Tract % BIPOC",
                min = minPctBipoc, max = maxPctBipoc,
                margin = 10,
                value = c(minPctBipoc, maxPctBipoc),
                step = 5,
                behaviour =  c("snap"),
                format = wNumbFormat(decimals = 0,
                                     thousand = ",",
                                     suffix = "%")
            )
        })
        
        output$formattedHHIncomeSlider <- renderUI({
            shinyWidgets::noUiSliderInput(
                inputId = "formattedHHIncomeSlider_raw",
                label = paste0("Median Annual HH Income (",acsYear, " dollars)"),
                min = minHHIncome, max = maxHHIncome,
                margin = 20000,
                value = c(minHHIncome, maxHHIncome),
                step = 5000,
                behaviour =  c("snap"),
                format = wNumbFormat(decimals = 0,
                                     thousand = ",",
                                     prefix = "$")
            )
        })
        
        
        output$formattedPctBelowPovertySlider <- renderUI({
            shinyWidgets::noUiSliderInput(
                inputId = "formattedPctBelowPovertySlider_raw",
                label = "Tract % Below Poverty Line",
                min = minPctBelowPov, max = maxPctBelowPov,
                margin = 2,
                value = c(minPctBelowPov, maxPctBelowPov),
                step = 1,
                behaviour =  c("snap"),
                format = wNumbFormat(decimals = 0,
                                     thousand = ",",
                                     suffix = "%")
            )
        })
        
        
        
        # Set Variable to Color Cloropleth
        output$acsTableSelect2UI <- renderUI({
            selectizeInput('acsTableSelect2',
                           label = h5('Select Cloropleth Fill Color'),
                           choices = formatted_finalFilteredVarsList,
                           selected = "TotalPop")
        })
        
       
        
        # Variables To Listen For Change to Update Map
        mapVarChange <- reactive({
            list(input$acsTableSelect2,
                 input$formattedPctAge25_50Slider_raw,
                 input$formattedPctBipocSlider_raw,
                 input$formattedHHIncomeSlider_raw,
                 input$formattedPctBelowPovertySlider_raw)
        })
        

        # UPDATE MAP BASED ON INPUT VARIABLES --------------------
        observeEvent(mapVarChange(), {
            # Ensure both inputs have values
            req(input$formattedHHIncomeSlider_raw)
            req(input$acsTableSelect2)
            
            # Create Copy of Initial Table For Mapping
            mappingTable <- filteredACSTracts
            
            # Set Color Column Values Before Filtering
            mappingTable$colorCol = mappingTable[[input$acsTableSelect2]]
            
            # Subset Table By Filters
            mappingTable <- mappingTable%>%filter(MedianHHInc >= input$formattedHHIncomeSlider_raw[1] & MedianHHInc <= input$formattedHHIncomeSlider_raw[2]  & PctBIPOC >= input$formattedPctBipocSlider_raw[1] & PctBIPOC <= input$formattedPctBipocSlider_raw[2] & Pct_Total_Age25_50 >= input$formattedPctAge25_50Slider_raw[1] & Pct_Total_Age25_50 <= input$formattedPctAge25_50Slider_raw[2] & PctBelowPoverty >= input$formattedPctBelowPovertySlider_raw[1] & PctBelowPoverty <= input$formattedPctBelowPovertySlider_raw[2])
            
  
            # Set Style for Cloropleth Filtered Map
            popupText2 = c("Total Tract Population: " = "TotalPop",
                           "% BIPOC: " = "PctBIPOC",
                           "% Aged 25-50: " = "Pct_Total_Age25_50",
                           "% Taking Transit to Work: " = "PctTransit2Wrk",
                           "% Below Poverty Line: " = "PctBelowPoverty",
                           "Median HH Income: " = "MedianHHInc",
                           "Selected Variable: " = "colorCol")
            
            # Set Cloropleth/Map Formatting
            cloroplethStyle2 = "fisher"
            colorPal2 = "Greens"
            breakVals2 = NULL
            fillLabels2 = NULL
            legendTitle2 = names(formatted_finalFilteredVarsList)[formatted_finalFilteredVarsList == input$acsTableSelect2]
            
            # Remove records with empty geometry/units and plot
            updatedMap2 <-  tm_shape(mappingTable[!st_is_empty(mappingTable), ], unit = "mi") +
                tmap_options(max.categories = palletBinNumber) +  # Set Max Number of levels
                tm_fill(
                    group = "ACS Data Layer",
                    col = "colorCol",
                    n = palletBinNumber, # 5 colors
                    labels = fillLabels2,
                    palette = colorPal2,
                    style = cloroplethStyle2,
                    breaks = breakVals2,
                    contrast = c(0.3, 1),
                    title = legendTitle2,
                    textNA = "Not Available",
                    colorNA = "gray",
                    id = "NAME",
                    popup.vars = popupText2
                ) +
                tm_borders(col = "darkgray") +
                tm_view(
                    alpha = 0.5,
                    view.legend.position = c("left", "bottom"),
                    leaflet.options = 
                )+
              tm_basemap(c(leaflet::providers$Stamen.Toner,
                           leaflet::providers$Esri.WorldImagery,
                           leaflet::providers$OpenStreetMap, leaflet::providers$CartoDB.Positron))
            # Update Map
            output$filteredAcsMap <- renderTmap({updatedMap2})
        })
    })
    
    
  # c(leaflet::providers$OpenStreetMap, leaflet::providers$Stamen.Toner,
  #   leaflet::providers$Stamen.Terrain, leaflet::providers$Esri.WorldImagery,
  #   leaflet::providers$Esri.NatGeoWorldMap, leaflet::providers$CartoDB.Positron)
  
    
    
    
    
    
  # Info/Code for Tab 2: General ACS Overview ---------------------------------
  observeEvent(input$keyVarSwitch,{
    if (input$keyVarSwitch == "Key Variables Only"){
      variableSelectionList = keyVarName
    }else{
      variableSelectionList = unique(possibleTablesStatic$tableName)
    }
    
    # Update Dropdown for possible variables/tables
    updateSelectizeInput(session, 'acsTableSelect', choices = variableSelectionList, server = TRUE)
    
    
  })
  
  
  # Update acs data selection based on file selection for Tab 2
  observeEvent(input$loadDataButton,{
    req(input$acsStateSelect)
    currentSelectedStateTracts <- get_acs(
      geography = "tract",
      variables = c(input$acsTableSelect, "B01003_001"),
      state = input$acsStateSelect,
      year = acsYear,
      survey = "acs5",
      geometry = TRUE,
      output = "wide" # get data in wide format for easier mapping
    )
    
    # Alter Values and Labels if Scaled to Pct Total Pop
    
    # Set Style for Pct of Total Pop
    if (input$pctTotalPopSwitch == TRUE){
      currentSelectedStateTracts$colorCol = round(currentSelectedStateTracts[[paste0(input$acsTableSelect,"E")]] / currentSelectedStateTracts[[paste0("B01003_001","E")]],2)
      currentSelectedStateTracts$pctColorCol = label_percent(0.1)(currentSelectedStateTracts[[paste0(input$acsTableSelect,"E")]] / currentSelectedStateTracts[[paste0("B01003_001","E")]])
      popupText = c("Selected Variable: " = paste0(input$acsTableSelect,"E"),
                    "% of Total Tract Pop: " = "pctColorCol",
                    "Total Tract Population: " = "B01003_001E")
      
      # Set Cloropleth Formatting
      cloroplethStyle = "fixed"
      breakVals = percentBins# quantile(currentSelectedStateTracts$colorCol, percentBins, na.rm = T)
      fillLabels = labels_PctBins
      colorPal = "Greens" #"-RdBu"
      
      legendTitle = paste(names(keyVarName)[which(keyVarName == input$acsTableSelect)], "% of Total Tract Pop.")
    }else{
      currentSelectedStateTracts$colorCol = currentSelectedStateTracts[[paste0(input$acsTableSelect,"E")]]
      popupText = c("Selected Variable: " = "colorCol",
                    "Total Tract Population: " = "B01003_001E")
      
      # Set Cloropleth Formatting
      cloroplethStyle = "fisher"
      colorPal = "Greens"
      breakVals = NULL
      fillLabels = NULL
      legendTitle = names(keyVarName)[which(keyVarName == input$acsTableSelect)]
    }
    
    #percentBins
    # Remove records with empty geometry/units and plot
    updatedMap <-  tm_shape(currentSelectedStateTracts[!st_is_empty(currentSelectedStateTracts), ], unit = "mi") +
      tmap_options(max.categories = palletBinNumber) +  # Set Max Number of levels
      tm_fill(
        group = "ACS Data Layer",
        col = "colorCol",
        n = palletBinNumber, # 5 colors
        labels = fillLabels,
        palette = colorPal,
        style = cloroplethStyle,
        breaks = breakVals,
        contrast = c(0.3, 1),
        title = legendTitle,
        textNA = "Not Available",
        colorNA = "gray",
        id = "NAME",
        popup.vars = popupText
      ) +
      # tm_layout(basemaps = leaflet::providers$OpenStreetMap)+
      tm_borders(col = "darkgray") +
      tm_view(
        alpha = 0.5,
        #basemaps = "Stamen.TonerLite",
        view.legend.position = c("right", "bottom")
      )
    # Update Map
    output$acsMap <- renderTmap({updatedMap})
  })
  
  
  
  
  
  
  
  
    
    
    
    
    
    # Tab 3 (ACS Data Table Possibilities) ----------
    output$possibleTables <- DT::renderDataTable(DT::datatable(possibleTablesStatic,
                                                            filter = 'top',
                                                            options = list(pageLength = 25, ordering=F),
                                                            rownames= FALSE,
                                                            escape = FALSE,
                                                            selection = 'none')) # Set Strings as Factor so that filter is a dropdown not typing


} # End of Server

##############################################################################
shinyApp(ui, server)
##############################################################################
