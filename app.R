library(shiny)
library(sf)
library(raster)
library(rasterVis)
library(DT)
library(rhandsontable)
library(stringr)
library(leaflet.extras)
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
library(shinyalert)
library(shinybusy)

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
        
        fillPage( useShinyalert(), shinyjs::useShinyjs(),

    tagList(
        tags$head(
            tags$link(rel="stylesheet", type="text/css",href="style.css"),
            tags$script(type="text/javascript", src = "googleAnalytics.js"),
            tags$head(tags$link( rel="icon", type="image/png", href="favicon-32x32.png", sizes="32x32" ),
                      tags$title("TERN Landscapes Review")
                      
            )
        )
    ),
    
    tags$style(
        
        # Colorize the actionButton.
        HTML(".alert {
                         background-color: #F5F5F5;
                         padding: 0px; margin-bottom: 10px;
                         color: black;
                         height: 50px;
                     } 
             
             .alert-info {
             border-color: #F5F5F5;
             }"
        )
    ),
    HTML("<p> <img src='Logos/TERN-NCRIS.jpg' height=100 width=200>
            
            <font size='6'><b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            TERN Landscapes Draft Map Products</b></font></p>"),
    
    
    sidebarLayout(
        sidebarPanel(width=2, HTML('The information contained in this site is only for the use of the TERN Landscapes team. <b>Do not distribute information outside of the project team.</b><br><br>'),
                     
                     #htmlOutput("debugtext"),
                     
                    
                     
                     fluidRow(div(style = "valign:top; height:50px;background-color: #F5F5F5;", bsAlert("alert"))),
                     
                     fluidRow(div(style = "text-align:center;", uiOutput("wSendTo"))),
                     
                     
                     
                     fluidRow(selectInput("wProduct", "Product", choices = NULL)),
                     fluidRow(selectInput("wProductType", "Product Type", choices = NULL)),
                     fluidRow(selectInput("wProductDepth", "Product Depth", choices = NULL)),
                     
                     HTML('<b>Info</b>'),
                     wellPanel( 
                       fluidRow(div(style = "text-align:left;", uiOutput("wStatsLink"))),
                       fluidRow(div(style = "text-align:left;", uiOutput("wMethodLink"))),
                       
                     ),
                     
                     HTML('<br><b>Data Downloads</b>'),
                     wellPanel( 
                         downloadButton('downloadData', 'Download Current RasterData'),
                         bsTooltip(id = "downloadData", title = "Click here to download the currently displayed raster", placement = "top", trigger = "hover"),
                         bsAlert("downloadalert")
                         
                     )
        ),
        mainPanel(
            tabsetPanel(
                
                
                #tabPanel("Suitability Maps", leafletOutput("mymap", width = "600px", height = "400px"),
                tabPanel("Map Viewer", 
                         
                         leafletOutput("wMainMap", height = "700")
                )
                         
                        
                # ), 
                # 
                # tabPanel("Stats report", uiOutput("pdfStatsview")),
                # 
                # tabPanel("Methodology", uiOutput("pdfMethodsview"))
                # 
                
            )
        )
    )
    )  
        
    
)


server <- function(input, output, session) {
    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    
    RV <- reactiveValues()
    RV$currentProductRecord <- NULL
    RV$currentSites <- NULL
    RV$currentSiteLabels <- NULL
    RV$currentSiteData<- NULL
    RV$currentRaster <- NULL
    RV$productTypes <- NULL
    RV$productDepths <- NULL
    RV$sendCommentsTo <- NULL
    
    output$wSendTo <- renderUI({
      tags$a("Send Comments", href=paste0("mailto:", RV$currentProductRecord$CommentsTo,"?Subject=TERN Review Comments : ", input$wProduct))
    })
    
    output$wStatsLink <- renderUI({
      tags$a("Show Stats Summary", href=paste0(RV$currentProductRecord$StatsFile), target="_blank")
    })
    
    output$wMethodLink <- renderUI({
      tags$a("Show Methods Summary", href=paste0(RV$currentProductRecord$MethodsFile), target="_blank")
    })
    
    
    #########   Productivity Map Combos   ###########################
    observe({
        
        req(configInfo)
            products <- as.character(configInfo$Product)
            updateSelectInput(session, "wProduct", choices =  products)
    })
    
    observe({
        req(input$wProduct)
        
            updateSelectInput(session, "wProductDepth", choices =  NULL)
            RV$currentProductRecord <- configInfo[configInfo$Product == input$wProduct, ]
            
            dps <- str_split(RV$currentProductRecord$Depths, ';')
            if(length(dps[[1]]) > 0 & dps != ''){
              RV$productDepths <- dps[[1]]
              updateSelectInput(session, "wProductDepth", choices = RV$productDepths)
              shinyjs::enable("wProductDepth")
            }else{
              RV$productDepths <- NULL
              updateSelectInput(session, "wProductDepth", choices = c('None'), selected = 'None')
              #shinyjs::disable("wProductDepth")
            }
            
            dtypes <- str_split(RV$currentProductRecord$Types, ';')
            if(length(dtypes[[1]]) > 0 & dtypes != ''){
              RV$productTypes <- dtypes[[1]]
              updateSelectInput(session, "wProductType", choices = RV$productTypes)
              shinyjs::enable("wProductType")
             
            }else{
              RV$productTypes <- NULL
              updateSelectInput(session, "wProductType", choices =c('None'), selected = 'None')
             # shinyjs::disable("wProductType")
            }
    })
    
    
    getLayer <- reactive({
      p <- input$wProduct
      if(input$wProductType!='None'){p <- paste0(p, '_', input$wProductType)}
      if(input$wProductDepth!='None'){p <- paste0(p, '_', input$wProductDepth)}
      p
    })
    

    
    
    observe({
         req(input$wProduct)
         shinyBS::closeAlert(session, "waitalert")
         shinyBS::createAlert(session, "alert", "waitalert", title = "", content = paste0("<div id='zs1'><img src=wait.gif> Drawing map", " .....</div>"), append = FALSE, dismiss = F)
         
         RV$currentProductRecord <- configInfo[configInfo$Product == input$wProduct, ]
         dps <- str_split(RV$currentProductRecord$Depths, ';')

            if(length(dps[[1]]) > 1){RV$isMultiLayer=T}else{RV$isMultiLayer=F}

            RV$currentSites <- st_read(paste0(dataStorePath, "/Clay/Sites/Clay.shp"))
            #df <- st_drop_geometry(RV$currentSites )

            # #uri = paste0(OGCserver, '&SERVICE=WMS&VERSION=1.1.1&layer=', layer, '&REQUEST=getlegendgraphic&FORMAT=image/png')
            # RV$currentSiteLabels <- lapply(seq(nrow(df)), function(i) {
            #     paste0( '<li>Site Name : ', df[i, "ID"], '</li>',
            #             '<li>0-5cm : ', if(!is.na(df[i, "GSM1"])){format(round(df[i, "GSM1"], 2), nsmall = 2)}else{'NA'}, '</li>',
            #             '<li>5-15cm : ',  if(!is.na(df[i, "GSM2"])){format(round(df[i, "GSM2"], 2), nsmall = 2)}else{'NA'}, '</li>',
            #             '<li>15-30cm : ',  if(!is.na(df[i, "GSM3"])){format(round(df[i, "GSM3"], 2), nsmall = 2)}else{'NA'}, '</li>',
            #             '<li>30-60cm : ',  if(!is.na(df[i, "GSM4"])){format(round(df[i, "GSM4"], 2), nsmall = 2)}else{'NA'}, '</li>',
            #             '<li>60-100cm : ',  if(!is.na(df[i, "GSM5"])){format(round(df[i, "GSM5"], 2), nsmall = 2)}else{'NA'}, '</li>',
            #             '<li>100-200cm : ',  if(!is.na(df[i, "GSM6"])){format(round(df[i, "GSM6"], 2), nsmall = 2)}else{'NA'}, '</li>'
            # 
            #     )
            # })

            shinyBS::closeAlert(session, "waitalert")
            shinyBS::createAlert(session, "alert", "waitalert", title = "", content = NULL, append = FALSE, dismiss = F)

    })
 
    output$pdfStatsview <- renderUI({
        #tags$iframe(style="height:600px; width:100%", src="Stats/2020BebrasParentalConsentForm - James Searle.pdf")
      tags$iframe(style="height:600px; width:100%", src=paste0(RV$currentProductRecord$StatsFile))
      
    })
    
    output$pdfMethodsview <- renderUI({
        tags$iframe(style="height:600px; width:100%", src="Stats/2020BebrasParentalConsentForm - James Searle.pdf")
    })
    
    
   
    
    
    output$wMainMap <- renderLeaflet({
        
        req( input$wProduct, input$wProductType, input$wProductDepth)
      srv <-str_replace(OGCserver, 'XXXX',  str_to_lower( input$wProduct))
      

      layer = getLayer()
      lnum <- WMSMappings[WMSMappings$Name == layer, ]$LayerNum
      
      print(layer)
      print(lnum)
      print(srv)
      
       print(layer)
            leaflet()  %>% leaflet::removeTiles('wmsl') %>% setView(lng = 134, lat = -26, zoom = 4) %>% addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = F), group = "Satelite Image") %>%
             addTiles(group = "Map")  %>%
                 addWMSTiles(
                    layerId = 'wmsl',
                    baseUrl = srv,
                    layers = lnum,
                    options = WMSTileOptions(format = "image/png", transparent = T),
                    group = "SLGA"
                )   %>%
                      addWMSLegend(uri = paste0(OGCserver, '&SERVICE=WMS&VERSION=1.1.1&layer=', lnum, '&REQUEST=getlegendgraphic&FORMAT=image/png'),  position =  "bottomright") %>%
           addLayersControl(
                baseGroups = c("Satelite Image", "Map"),
                overlayGroups = c( 'Sites', "SLGA"),
                options = layersControlOptions(collapsed = FALSE)
            ) %>%
              
            #  addMarkers( data=RV$currentSites, clusterOptions = markerClusterOptions(), group = 'Sites', label = lapply(RV$currentSiteLabels, HTML) )  %>%
            
            #  addMarkers( data=RV$currentSites, clusterOptions = markerClusterOptions(), group = 'Sites', label =  popupTable(breweries91[1, ])) %>%
             
                
                addFullscreenControl() %>%
                leafem::addMouseCoordinates() %>%
                leaflet.extras::addSearchOSM(options = searchOptions(autoCollapse = T, minLength = 2)) 
})
    
    
    observe({
        
    req(input$wProductDepth, input$wProductType, input$wProductDepth)

    layer <- getLayer()

    rPath <- paste0(dataStorePath, '/',  input$wProduct, '/Rasters/', layer, '.tif')
    if(file.exists(rPath)){
      RV$currentRaster <- raster(rPath)
    }else{
      RV$currentRaster <- NULL
    }
    
    })
    
    
    observe({
        
        req(input$wProductDepth, input$wProductType, input$wProductDepth)
        
      layer <- getLayer()
        proxy <- leafletProxy("wMainMap")
            # proxy %>%  addWMSTiles(
            #     OGCserver,
            #     layers = layer,
            #     options = WMSTileOptions(format = "image/png", transparent = T),
            #     group = "SLGA"
            # ) 
        
        if( !is.null(RV$currentSites)){
            #proxy %>% addMarkers( data=RV$currentSites, clusterOptions = markerClusterOptions(), group = 'Sites', label = lapply( RV$currentSiteLabels, HTML) ) 
          
          
          proxy %>% addMarkers( data=RV$currentSites, clusterOptions = markerClusterOptions(), group = 'Sites', layerId=paste0(RV$currentSites$ID)) 
          
          
        }  
          proxy %>%  addWMSLegend(uri = paste0(OGCserver, '&SERVICE=WMS&VERSION=1.1.1&layer=', layer, '&REQUEST=getlegendgraphic&FORMAT=image/png'),  position =  "bottomright")
    })
    
    observe({
        
        req(input$wProduct, input$wProductType, input$wProductDepth)
        
        p <- input$wProduct
        t <- input$wProductType
        d <- input$wProductDepth
        layer <- paste0(p, '_', t, '_', d)
        proxy2 <- leafletProxy("wMainMap")
               proxy2 %>%  addWMSLegend(uri = paste0(OGCserver, '&SERVICE=WMS&VERSION=1.1.1&layer=', layer, '&REQUEST=getlegendgraphic&FORMAT=image/png'),  position =  "bottomright")
        
        
    })
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
                    fname <- paste0(input$wProduct,'_',  input$wProductType,'_', input$wProductDepth, '.tif')
            fname
        },
        content = function(file) {

            Rname <- 'E:/ReviewDataStore/Clay/Rasters/Clay_Model-Value_0-5.tif'
                    createAlert(session, "downloadalert", "downloadingalert", title = "", content = "<img src=wait.gif> Extracting the requested data .....", append = FALSE)
                    file.copy(Rname, file)
                    closeAlert(session, "downloadingalert")
        })
 
    
    observe({
        
        req(RV$currentRaster)
        
        click<-input$wMainMap_click
        if(is.null(click))
            return()

       if(!is.null(RV$productDepths) ){
           
           RV$currentProductRecord <- configInfo[configInfo$Product == input$wProduct, ]
           dps <- str_split(RV$currentProductRecord$Depths, ';')
           valstr = ''
           for (i in 1:length(RV$productDepths)) {
             rPath <- paste0(dataStorePath, '/', input$wProduct, '/Rasters/', input$wProduct,'_',  input$wProductType,'_', RV$productDepths[i], '.tif')
             if(file.exists(rPath)) { 
                r <- raster(rPath)
                ptVal <- extract(r, data.frame(click$lng, click$lat))
                valstr <- paste0(valstr, '<li>',dps[[1]][i],' : ', format(round(ptVal, 2), nsmall = 2), '</li>')
                print(i)
             }
           }
       }else{
           ptVal <- extract(RV$currentRaster, data.frame(click$lng, click$lat))
           valstr = paste0('<b>', format(round(ptVal, 2), nsmall = 2), '</br>')

       }
        shinyalert(input$wProduct,  valstr, type = "info", html=T, animation = F)
        
    })
    
    
    observe({

      req(RV$currentRaster)

      click<-input$wMainMap_marker_click
      if(is.null(click))
        return()
      
      print(click)
      
      df <-  st_drop_geometry( RV$currentSites[RV$currentSites$ID==click$id, ] )
     
     lab <- paste0( '<Table style="border:0px solid black;margin-left:auto;margin-right:auto;">
                    <tr><td>Site Name</td><td>', df[1, "ID"], '</td></td>',
                    '<tr><td>S0-5cm</td><td>', if(!is.na(df[1, "GSM1"])){format(round(df[1, "GSM1"], 2), nsmall = 2)}else{'NA'}, '</td></td>',
                    '<tr><td>S5-15cm</td><td>',  if(!is.na(df[1, "GSM2"])){format(round(df[1, "GSM2"], 2), nsmall = 2)}else{'NA'}, '</td></td>',
                    '<tr><td>S15-30cm</td><td>',  if(!is.na(df[1, "GSM3"])){format(round(df[1, "GSM3"], 2), nsmall = 2)}else{'NA'}, '</td></td>',
                    '<tr><td>S30-60cm</td><td>',  if(!is.na(df[1, "GSM4"])){format(round(df[1, "GSM4"], 2), nsmall = 2)}else{'NA'}, '</td></td>',
                    '<tr><td>S60-100cm</td><td>',  if(!is.na(df[1, "GSM5"])){format(round(df[1, "GSM5"], 2), nsmall = 2)}else{'NA'}, '</td></td>',
                    '<tr><td>S100-200cm</td><td>',  if(!is.na(df[1, "GSM6"])){format(round(df[1, "GSM6"], 2), nsmall = 2)}else{'NA'}, '</td></td></table>')
      
      shinyalert(paste0('Observed Data : ', input$wProduct),  lab, type = "info", html=T, animation = F)
    })
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)
