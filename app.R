library(shiny)

library(raster)
library(rasterVis)
library(DT)
library(rhandsontable)
library(stringr)
library(leaflet)
library(utils)
library(RColorBrewer)
library(colorRamps)
library(colorspace)
library(shinyBS)
library(rmarkdown)
library(shinyjs)
library(shinycssloaders)

library(shinymanager)

source('appConfig.R')

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


credentials <- data.frame(
    user = c("Landscapes"),
    password = c("1"),
    stringsAsFactors = FALSE
)

configInfo <- read.csv(paste0(dataStorePath, '/DataStoreConfig.csv'), stringsAsFactors = F)




ui <- 
      secure_app( theme = 'journal', tags_top=tags$div(
        #tags$h4("TERN Landscapes Review", style = "align:center"),
        tags$img(
          src = "Logos/TERN-NCRIS.jpg", width = 400
        ),
        tags$p(paste0("The draft TERN Landscapes products are currently only available to collaborators for review purposes.") )
        ),  head_auth = NULL, 
        tags_bottom=tags$div(
          tags$html("For Information about obtaining a login contact "), tags$a(adminName, href=paste0("mailto:", adminEmail,"?Subject=TERN Review Access"))
        ),
        
        fillPage(

    tagList(
        tags$head(
            tags$link(rel="stylesheet", type="text/css",href="style.css"),
            tags$script(type="text/javascript", src = "googleAnalytics.js"),
            tags$head(tags$link( rel="icon", type="image/png", href="favicon-32x32.png", sizes="32x32" ),
                      tags$title("NAWRA Draft Suitability Products")
            )
        )
    ),
    
    
    HTML("<p> <img src='Logos/TERN-NCRIS.jpg' height=100 width=200>
            
            <font size='6'><b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            TERN Landscapes Draft Map Products</b></font></p>"),
    
    
    sidebarLayout(
        sidebarPanel(width=2, HTML('The information contained in this site is only for the use of the TERN Landscapes team. <b>Do not distribute information outside of the project team.</b><br><br>'),
                     
                     htmlOutput("debugtext"),
                     
                     bsAlert("alert"),
                     
                     fluidRow(selectInput("wProduct", "Product", choices = NULL)),
                     fluidRow(selectInput("wProductType", "Product Type", choices = productTypes)),
                     fluidRow(selectInput("wProductDepth", "Product Deph", choices = NULL)),
                     
                    
                     HTML('<br><b>Data Downloads</b>'),
                     wellPanel( 
                         selectInput("dataset", "Choose product to download:", choices = c("")),
                         bsTooltip(id = "dataset", title = "Select the type of data set you want to download", placement = "top", trigger = "hover"),
                         
                         downloadButton('downloadData', 'Download Data'),
                         bsTooltip(id = "downloadData", title = "Click here to download the select data sets", placement = "top", trigger = "hover"),
                         
                         bsAlert("downloadalert")
                         
                     )
        ),
        mainPanel(
            tabsetPanel(
                #tabPanel("Suitability Maps", leafletOutput("mymap", width = "600px", height = "400px"),
                tabPanel("Map Viewer", 
                         bsAlert("waitalert"), 
                         withSpinner(leafletOutput("wMainMap", height = "850"))
                         
                        
                ), 
                
                tabPanel("Stats report", uiOutput("pdfStatsview")),
                
                tabPanel("Methodology", uiOutput("pdfMethodsview"))
                
                
            )
        )
    )
    )  
        
    
)


server <- function(input, output, session) {
    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    
    RV <- reactiveValues()
    RV$currentProductRecord <- NULL
    
    #########   Productivity Map Combos   ###########################
    observe({
        
        req(configInfo)
            products <- as.character(configInfo$Product)
            updateSelectInput(session, "wProduct", choices =  products)
    })
    
    observe({
        req(input$wProduct)
            RV$currentProductRecord <- configInfo[configInfo$Product == input$wProduct]
            dps <- str_split(RV$currentProductRecord$Depths, ';')
            updateSelectInput(session, "wProductDepth", choices =  dps[[1]])
    })
 
    output$pdfStatsview <- renderUI({
        tags$iframe(style="height:600px; width:100%", src="Stats/2020BebrasParentalConsentForm - James Searle.pdf")
    })
    
    output$pdfMethodsview <- renderUI({
        tags$iframe(style="height:600px; width:100%", src="Stats/2020BebrasParentalConsentForm - James Searle.pdf")
    })
    
    
    output$wMainMap <- renderLeaflet({
        
            # pal <- colorNumeric(c("brown", "lightgreen",  "darkgreen"), values(r),na.color = "transparent")
            # CPCOLS <- c("burlywood4", "burlywood3", "cornsilk", "darkseagreen1", "chartreuse", "chartreuse3", "chartreuse4")
            # pal <- colorNumeric(CPCOLS, values(r),na.color = "transparent")
            # 
            leaflet() %>% setView(lng = 140, lat = -26, zoom = 4) %>% addTiles() %>%
                addWMSTiles(
                    "http://localhost/cgi-bin/mapserv.exe?map=C:/ms4w/SLGA/SLGA.map&SERVICE=WMS&VERSION=1.0.0",
                    layers = "test1",
                    options = WMSTileOptions(format = "image/png", transparent = T)
                )
            
            
            
            # leaflet() %>%
            #     
            #     addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
            #     addTiles(group = "Map") 
                # fitBounds( e@xmin, e@ymin, e@xmax, e@ymax) %>%
                # addRasterImage(r, colors = pal, opacity = 0.8 ,  group = "Farm Data")  %>%
                
                # 
                # addLayersControl(
                #     baseGroups = c("Satelite Image", "Map"),
                #     overlayGroups = c("Farm Data"),
                #     options = layersControlOptions(collapsed = FALSE)
                # ) %>%
                # leaflet::addLegend(pal = pal, values = values(r), title = "Yield - T/Ha")
            
        
    
})
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
