#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
# source('./functions.r')
options(scipen=12)

getLolliPeaks <- function(df, subset.County=NULL){
    # df <- peakData[c(1,2,4)] pass this to function from the peak data for either SEIR or SEIcIscR
    
    colnames(df) <- c("County", "Peak", "Size")
    # Given data a random direction
    df$Month <- format(df$Peak, '%b')
    
    # Assign a Direction
    if(is.null(subset.County)){
        directions = c(1,-1)
    } else {
        directions = c(1)
    }
    df$dir <- sample(directions, length(df$County), replace=TRUE)
    
    # Offset days as well for labels.
    directions = c(-2,0,2)
    df$Peak = df$Peak + sample(directions, length(df$County), replace=TRUE)
    
    # Offset Labels
    text_offset <- 0.5
    df$month_count <- ave(df$Peak==df$Peak, df$Peak, FUN=cumsum)
    df$text_position <- (df$month_count * text_offset * df$dir) + ((df$Size+100)*df$dir)
    
    # Prepare Timeline Range and Date Positioning
    month_buffer <- 1
    month_date_range <- seq(min(df$Peak) - months(month_buffer), max(df$Peak) + months(month_buffer), by='month')
    month_format <- format(month_date_range, '%b')
    month_df <- data.frame(Date=month_date_range-10, Month=month_format)
    
    if(!is.null(subset.County)){
        df <- subset(df, df$County==subset.County)
    }
    
    # Plot It
    timeline_plot<-ggplot(data=df, 
                          aes(x=Peak,y=0)) +
        labs(col="Peak Date") + theme_classic() + geom_hline(yintercept=0, 
                                                             color = "black", size=0.5)
    # Plot Lollipops
    timeline_plot<-timeline_plot+geom_segment(data=df,
                                              aes(x=Peak, y=Size*dir,yend=0,xend=Peak),
                                              size=0.2)
    
    # Plot scatter points at zero and date
    timeline_plot<-timeline_plot+geom_point(aes(y=0, color=Month), size=2) + scale_color_brewer(palette="Set1")
    
    # Don't show axes, appropriately position legend
    timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                       axis.text.y=element_blank(),
                                       axis.title.x=element_blank(),
                                       axis.title.y=element_blank(),
                                       axis.ticks.y=element_blank(),
                                       axis.text.x =element_blank(),
                                       axis.ticks.x =element_blank(),
                                       axis.line.x =element_blank(),
                                       legend.position = "bottom") + guides(color=F, fill=F)
    
    timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=Date,y=-1,label=Month),
                                           size=2.5,hjust=1, color='black', angle=90)
    
    timeline_plot<-timeline_plot + geom_text(data=df, aes(y=text_position,label=County),size=2.5, angle=45) + ggtitle("Peak Cases")
    
    return(timeline_plot)
}


# Map Information and Dropdown Info
countyColorMap <- colorFactor(c("red","darkgreen"), NULL)
flMapCounties <- rgdal::readOGR("data/Florida_Counties.geojson")
flMapCounties$COUNTYNAME <- sapply(as.vector(flMapCounties$COUNTYNAME), FUN=function(x) gsub("DADE","MIAMI-DADE",x) )

# Loda Model Specific Info and Identifiers
load('data/FloridaCountyNames.rdata') # loads into namespace as counties
load('data/AgeVector.rdata')
ageVector <- c("All Ages", ageVector)
load('data/peakData.rdata')
load('data/incidenceTotals.rdata')

ui <- bootstrapPage(
    div(class="outer",
        tags$head(
            includeCSS("styles.css")
        ),
        # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = FALSE, top = 60, left = 50, bottom = "auto",
                      width = 330, height = "auto",
                      selectInput("County", "County",
                                  c("All Counties",sort(as.vector(counties$printablecounty))), "All Counties"
                      ),
                      selectInput("Age", "Age",
                                  ageVector, "All Ages"
                      ),
                      selectInput("Strategy", "Intervention",
                                  c("Strategy 1","Strategy 2", "Strategy 3","Strategy 4"), "Strategy 1"
                      ),
                      selectInput("Model", "Model",
                                  c("SEIR","SEIcIscR"), "SEIR"
                      ),
                      checkboxInput("legend", "Show legend", TRUE)
                      # plotOutput("lolliPeaks", height = 200)
        ),
        # absolutePanel(id = "plots", class = "panel panel-default", fixed = TRUE,
        #               draggable = FALSE, top = "auto", right = 50, bottom = "auto",
        #               width = 330, height = "auto",
        #               plotOutput("lolliPeaks", height = 200)
        # )
        absolutePanel(id = "controls", class = "panel panel-default", bottom = 20, left = 60, width = "90%", fixed=TRUE, draggable = FALSE, height = "auto",
                      fluidRow(
                          splitLayout(cellWidths = c("75%", "25%"), plotOutput("lolliPeaks", height = 200), plotOutput("cumsums", height = 200))
                      )
        )
    )
)

server <- function(input, output, session) {
    
    # Reactive expression for the county that is chosen
    chosenCounty <- reactive({
        counties[counties$printablecounty == input$County,]
    })
    
    output$map <- renderLeaflet({
        leaflet(flMapCounties) %>%
            addTiles() %>%
            setView(lat = 27.845806, lng = -84.145044, zoom=7) %>%
            addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.3,
                        fillColor = countyColorMap(flMapCounties$COUNTYNAME), popup = ~paste(flMapCounties$COUNTYNAME))
    })
    
    output$lolliPeaks <- renderPlot({
        # If no zipcodes are in view, don't plot
        # if (nrow(zipsInBounds()) == 0)
        #     return(NULL)
        if(input$Model=="SEIR"){
            df <- peakData[c(1,2,4)]
        } else {
            df <- peakData[c(1,3,5)]
        }
        if(input$County!="All Counties"){
            subset.County=input$County
        } else {
            subset.County=NULL
        }
        getLolliPeaks(df, subset.County=subset.County)
    })
    
    output$cumsums <- renderPlot({
        # If no zipcodes are in view, don't plot
        # if (nrow(zipsInBounds()) == 0)
        #     return(NULL)
        
        df <- subset(incidenceTotals, incidenceTotals$County==input$County)
        
        ggplot(df, aes(x=Time, y=CumSum, color=County)) + geom_line() + guides(fill=F, color=F) + ylab("Cases") + xlab("") +
            theme_bw() +  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Cumulative Cases")
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
