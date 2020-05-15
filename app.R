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
library(readr)


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
                     
                     fluidRow(div(style = "valign:top; height:50px;background-color: #F5F5F5;", bsAlert("alert"))),
                     fluidRow(div(style = "text-align:center;", uiOutput("wSendTo"))),
                     fluidRow(selectInput("wProduct", "Product", choices = NULL)),
                     fluidRow(selectInput("wProductType", "Product Type", choices = NULL)),
                     fluidRow(selectInput("wProductDepth", "Product Depth", choices = NULL)),
                     
                     HTML('<b>Info</b>'),
                     wellPanel( 
                       fluidRow(div(style = "text-align:left;", uiOutput("wStatsLink"))),
                       fluidRow(div(style = "text-align:left;", uiOutput("wMethodLink")))
                     ),
                     
                     HTML('<br><b>Data Downloads</b>'),
                     wellPanel( 
                         fluidRow(downloadLink('downloadData', 'Download Entire DataSet')),
                         fluidRow(div(style = "text-align:left;", uiOutput("wWCS"))),
                     )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Map Viewer", 
                         leafletOutput("wMainMap", height = "700")
                ),
                 tabPanel("Help", 
                          #div(style = "valign:top; height:1000px;background-color: #F5F5F5; overflow: scroll height: 90vh; overflow-y: auto;",  includeHTML("Help.html"))
                          div(style = "valign:top; height: 90vh; overflow-y: auto;",  includeHTML("Help.html"))
               
                 )
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
    
    
    output$wHelp <- renderUI({
      input$wProduct
     htm <- read_file('Help.html')
    # print(htm)
      htm
    })
    
    output$wSendTo <- renderUI({
      tags$a("Send Comments", href=paste0("mailto:", RV$currentProductRecord$CommentsTo,"?Subject=TERN Review Comments : ", input$wProduct))
    })
    
    output$wStatsLink <- renderUI({
      req(RV$currentProductRecord)
      if(RV$currentProductRecord$StatsFile != ''){
      tags$a(paste0("Show ",input$wProduct, " Stats Summary"), href=paste0(RV$currentProductRecord$StatsFile), target="_blank")
      }else{
        tags$html("Stats Info Not Available")
      }
    })
    
    
    output$wMethodLink <- renderUI({
      req(RV$currentProductRecord)
      if(RV$currentProductRecord$MethodsFile != ''){
        tags$a(paste0("Show ",input$wProduct, " Methods Summary"), href=paste0(RV$currentProductRecord$MethodsFile), target="_blank")
      }else{
        tags$html("Methods Info Not Available")
      }
    
    })
    
    output$wWCS <- renderUI({
      
      req(RV$currentProductRecord, input$wMainMap_bounds)
      
      pres <- 0.0008333333333467680612
      rows <- (input$wMainMap_bounds$north - input$wMainMap_bounds$south) / pres
      cols <- (input$wMainMap_bounds$east - input$wMainMap_bounds$west) / pres
      pixels <- rows*cols
      
      if(pixels < 30000000){
      res <- paste0('&RESX=', pres, '&RESY=', pres)
      bbox <- paste0(input$wMainMap_bounds$west, ',', input$wMainMap_bounds$south, ',',input$wMainMap_bounds$east, ',',input$wMainMap_bounds$north )
      print(input$wMainMap_bounds)
      tags$a(paste0("Download ",input$wProduct, " Current Extent"), href=paste0('http://www.asris.csiro.au/arcgis/services/SLGApReview/clay/MapServer/WcsServer?REQUEST=GetCoverage&SERVICE=WCS&VERSION=1.0.0&COVERAGE=1&CRS=EPSG:4326&BBOX=', bbox, '&FORMAT=GeoTIFF', res))
      }else{
        tags$html("")
      }
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
            print(RV$currentProductRecord)
            
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
         


            sp <- paste0(dataStorePath, "/", input$wProduct, "/Sites/", input$wProduct, ".shp")
            if(file.exists(sp)){
              RV$currentSites = st_read(paste0(dataStorePath, "/", input$wProduct, "/Sites/", input$wProduct, ".shp"))
            }else{
              RV$currentSites = NULL
            }

            shinyBS::closeAlert(session, "waitalert")
            shinyBS::createAlert(session, "alert", "waitalert", title = "", content = NULL, append = FALSE, dismiss = F)

    })
 
    
    
    
   
    
    
    output$wMainMap <- renderLeaflet({
        
      req( input$wProduct, input$wProductType, input$wProductDepth)
      srv <-str_replace(OGCserver, 'XXXX',  str_to_lower( input$wProduct))
      V1Server <-str_replace(V1Server, 'XXXX',  RV$currentProductRecord$V1Code)
      
      layer = getLayer()
      rec <- WMSMappings[WMSMappings$Name == layer, ]
      lnum <- rec$LayerNum
      
     if( RV$currentProductRecord$V1Code != '' ){
       grps <- c( 'Sites',"SLGA_V1", "SLGA_V2")
     }else{
       grps <- c( 'Sites', "SLGA_V2")
     }
            leaflet()  %>% setView(lng = 134, lat = -26, zoom = 4) %>% addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = F), group = "Satelite Image") %>%
               
                addFullscreenControl() %>%
                leafem::addMouseCoordinates() %>%
                leaflet.extras::addSearchOSM(options = searchOptions(autoCollapse = T, minLength = 2)) %>% 
              addLayersControl(
              baseGroups = c("Satelite Image", "Map"),
              overlayGroups = grps,
              options = layersControlOptions(collapsed = FALSE),
              
            ) %>%
              addWMSLegend(uri = paste0(srv, '?VERSION=1.3.0&layer=', lnum, '&REQUEST=GetLegendGraphic&FORMAT=image/png'),  position =  "bottomright")
})
    
    
   
    
    observe({
        
      req(input$wProductDepth, input$wProductType, input$wProductDepth)
      
      srv <-str_replace(OGCserver, 'XXXX',  str_to_lower( input$wProduct))
      V1Server <-str_replace(V1Server, 'XXXX',  RV$currentProductRecord$V1Code)
      
      layer = getLayer()
      rec <- WMSMappings[WMSMappings$Name == layer, ]
      lnum <- rec$LayerNum
      V1L <- rec$V1
      
      print(rec)
      
     
        proxy <- leafletProxy("wMainMap")
        
         if( !is.null(RV$currentSites)){
           proxy %>% addMarkers( data=RV$currentSites, clusterOptions = markerClusterOptions(), group = 'Sites', layerId=paste0(RV$currentSites$ID)) 
         }  
       
        proxy %>%  leaflet::removeTiles('wmsl')
        proxy %>% addTiles(group = "Map")
        proxy %>% addWMSTiles(
            layerId = 'wmsl',
            baseUrl = srv,
            layers = lnum,
            options = WMSTileOptions(format = "image/png", transparent = T),
            group = "SLGA_V2",
            attribution = "TERN"
          ) 
        if(RV$currentProductRecord$V1Code != ''){
          
         
        proxy %>% addWMSTiles(
            layerId = 'wmsl2',
            baseUrl = V1Server,
            layers = V1L,
            options = WMSTileOptions(format = "image/png", transparent = T),
            group = "SLGA_V1",
            attribution = "TERN"
        )}else{
          proxy %>%  removeTiles('wmsl2')
          
        }
        
        proxy %>% hideGroup("SLGA_V1")
          
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
    
    
    output$downloadData <- downloadHandler(
     
        filename = function() {
                    fname <- paste0(getLayer(), '.tif')
            fname
        },
        content = function(file) {

            layer <- getLayer()
            rPath <- paste0(dataStorePath, '/',  input$wProduct, '/Rasters/', layer, '.tif')
            
            if(file.exists(rPath)){
              file.copy(rPath, file)
            }else{
             
            }
                   # createAlert(session, "downloadalert", "downloadingalert", title = "", content = "<img src=wait.gif> Extracting the requested data .....", append = FALSE)
                    #writeRaster(RV$currentRaster, filename = file)
                    #closeAlert(session, "downloadingalert")
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
