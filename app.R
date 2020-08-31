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
library(shinyWidgets)

library(shinymanager)
library(shinyalert)
library(shinybusy)
library(readr)
library(leafgl)

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
    user = c(authUsr),
    password = c(authPwd),
    stringsAsFactors = FALSE
)





ui <- 
      secure_app( theme = 'journal', tags_top=tags$div(
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
            #tags$script(type="text/javascript", src = "cursors.js"),
            
            tags$link( rel="icon", type="image/png", href="favicon-32x32.png", sizes="32x32" ),
                      tags$title("TERN Landscapes Review"),
        )
    ),
    
    tags$style(
        
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
                       #fluidRow(div(style = "text-align:left;", uiOutput("wStatsLink"))),
                       fluidRow(div(style = "text-align:left;", uiOutput("wMethodLink"))),
                       fluidRow(div(style = "text-align:left;", HTML('<BR>'))),
                       fluidRow(div(style = "text-align:left;", uiOutput("wCodeRepoLink")))
                     ),
                     
                     HTML('<br><b>Data Downloads</b>'),
                     wellPanel( 
                         fluidRow(downloadLink('downloadRasterData', 'Download entire raster dataset')),
                         fluidRow(downloadLink('downloadSiteData', 'Download site dataset')),
                         fluidRow(div(style = "text-align:left;", uiOutput("wWCS"))),
                     )
        ),
        mainPanel(
         
          
          singleton(
            tags$head(tags$script('Shiny.addCustomMessageHandler("cursorCrossHair",
                                    function(message) {
                                     document.getElementById("wMainMap").style.cursor = "help"; 
                                    }
                                  );'))
          ),
          
          singleton(
            tags$head(tags$script('Shiny.addCustomMessageHandler("cursorDef",
                                    function(message) {
                                     document.getElementById("wMainMap").style.cursor = "grab"; 
                                  }
                                  );'))
          ),
          
            tabsetPanel(
                tabPanel("Map Viewer", leafletOutput("wMainMap", height = "700"),
                         
                         HTML('<div style="position:absolute;top: 130px; color: white; height:260px;  left: 30px; z-index:1001; outline-style: double">'), 
                         radioGroupButtons(
                           inputId = "btnMapAction",
                           label = HTML("&nbsp;&nbsp;Map Operations&nbsp;&nbsp;"),
                           choices = c('Navigate', 'Info' ),
                           selected = 'Navigate',
                           status = "primary",
                           direction = 'vertical',
                           individual = F,
                           justified = T,
                           size = 'sm',
                           checkIcon = list(
                             yes = icon("check-square"),
                             no = icon("square-o")
                           )
                         ), 
                         absolutePanel(top = 100, left = 20, HTML('<B>Map Opacity</B>')),
                         
                         absolutePanel(top = 140, left = 30, 
                                       noUiSliderInput(
                                         inputId = "wgtMapOpacity", label = NULL,
                                         min = 0, max = 100, step = 5,
                                         value = c(100), margin = 150,
                                         orientation = "vertical",
                                         width = "50px", height = "100px",
                                         direction = "rtl",
                                         color = 'blue',
                                         tooltips = F
                                       )
                         ),
                         
                         HTML(  '</div>'),
                         
                        
                         
                        
                ),
                 tabPanel("Help", div(style = "valign:top; height: 90vh; overflow-y: auto;",  includeHTML("Help.html"))
               
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
    RV$currentRaster <- NULL
    RV$productTypes <- NULL
    RV$productDepths <- NULL
    RV$sendCommentsTo <- NULL
    RV$MapMode <- 'Navigate'
    
    observe({
      cdata <- session$clientData
      cnames <- names(cdata)
      
      allvalues <- lapply(cnames, function(name) {
        paste(name, cdata[[name]], sep = " = ")
      })
    })
   
    
    
    observe({
      req(input$btnMapAction)
     
      if(input$btnMapAction == 'Info'){
       
        session$sendCustomMessage(type = 'cursorCrossHair', message = list(a = 1))
      }else{
        # RV$lastclick
        session$sendCustomMessage(type = 'cursorDef', message = list(a = 2))
      }
    })
    
    
    output$wHelp <- renderUI({
      input$wProduct
     htm <- read_file('Help.html')

      htm
    })
    
    output$wSendTo <- renderUI({
      tags$a("Send Comments", href=paste0("mailto:", RV$currentProductRecord$CommentsTo,"?Subject=TERN Review Comments : ", input$wProduct))
    })
    
    
    output$wMethodLink <- renderUI({
      req(RV$currentProductRecord)
      if(RV$currentProductRecord$MethodsFile != ''){
        tags$a(paste0("Show ",input$wProduct, " Methods Summary"), href=paste0(RV$currentProductRecord$MethodsFile), target="_blank")
      }else{
        tags$html("Methods info not available")
      }
    })
    
    output$wCodeRepoLink <- renderUI({
      req(RV$currentProductRecord)
      if(RV$currentProductRecord$CodeRepository != ''){
        tags$a(paste0("Show ",input$wProduct, " code repository"), href=paste0(RV$currentProductRecord$CodeRepository), target="_blank")
      }else{
        tags$html("Code repository not available")
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
      tags$a(paste0("Download ",input$wProduct, " Current Extent"), href=paste0('http://www.asris.csiro.au/arcgis/services/SLGApReview/clay/MapServer/WcsServer?REQUEST=GetCoverage&SERVICE=WCS&VERSION=1.0.0&COVERAGE=1&CRS=EPSG:4326&BBOX=', bbox, '&FORMAT=GeoTIFF', res))
      }else{
        tags$html("")
      }
    })
    
    
    #########    Map Combos   ###########################
    observe({
        
        req(configInfo)
            products <- configInfo[configInfo$Available == T,]$Product
            selProd <- configInfo[configInfo$DefaultDisplay == T,]$Product
            updateSelectInput(session, "wProduct", choices =  products, selected = selProd)
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
         
            sp <- paste0(dataStorePath, "/", input$wProduct, "/Sites/", RV$currentProductRecord$SiteDataFileName  )
 
            if(file.exists(sp)){
              sdf <- read.csv(sp, stringsAsFactors = F)
              RV$currentSites = st_as_sf(sdf, coords = c("Longitude", "Latitude"), crs = 4326)
            }else{
              RV$currentSites = NULL
            }

            shinyBS::closeAlert(session, "waitalert")
            shinyBS::createAlert(session, "alert", "waitalert", title = "", content = NULL, append = FALSE, dismiss = F)

    })
 
    
    
    
    output$wMainMap <- renderLeaflet({
        
      req( input$wProduct, input$wProductType, input$wProductDepth)
      srv <-str_replace(OGCserver, 'XXXX',   RV$currentProductRecord$Code)
      V1Server <-str_replace(V1Server, 'XXXX',  RV$currentProductRecord$V1Code)

      layer = getLayer()
      

      rec <- WMSMappings[WMSMappings$Name == layer, ]

      lnum <- rec$LayerNum
      
     if( RV$currentProductRecord$V1Code != '' ){
       grps <- c( "Bio", 'Sites',"SLGA_V1", "SLGA_V2")
     }else{
       grps <- c( "Bio", 'Sites', "SLGA_V2")
     }
            leaflet(options = leafletOptions(dragging = T)) %>%
            
              addWMSTiles(
                layerId = 'wmsg',
                baseUrl = 'https://www.asris.csiro.au/arcgis/services/ASRIS/physiographicRegions2011/MapServer/WMSServer',
                layers = '3',
                options = WMSTileOptions(format = "image/png", transparent = T),
                group = "Bio",
                attribution = ""
              )  %>%
            
             setView(lng = 134, lat = -26, zoom = 4) %>% addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = F), group = "Satelite Image") %>%

               # addFullscreenControl() %>%
                leafem::addMouseCoordinates() %>%
               # leaflet.extras::addSearchOSM(options = searchOptions(autoCollapse = T, minLength = 2)) %>%
              addLayersControl(
              baseGroups = c("Satelite Image", "Map"),
              overlayGroups = grps,
              options = layersControlOptions(collapsed = FALSE),

            ) %>%
           
               addWMSLegend(uri = paste0(srv, '?VERSION=1.3.0&layer=', lnum, '&REQUEST=GetLegendGraphic&FORMAT=image/png'),  position =  "bottomright")
})
    
    
   
    
    observe({
        
      req(input$wProductDepth, input$wProductType, input$wProductDepth)
     
      srv <-str_replace(OGCserver, 'XXXX',   str_trim( RV$currentProductRecord$Code))


      V1Server <-str_replace(V1Server, 'XXXX',  str_trim( RV$currentProductRecord$V1Code))

      
      layer = getLayer()
      rec <- WMSMappings[WMSMappings$Name == layer, ]
      lnum <- rec$LayerNum
      V1L <- rec$V1
      
        proxy <- leafletProxy("wMainMap" )
        
         if( !is.null(RV$currentSites)){
           proxy %>% addGlPoints(data = RV$currentSites, group = 'Sites', layerId=paste0(RV$currentSites$ID)) 
           #proxy %>% addMarkers( data=RV$currentSites, clusterOptions = markerClusterOptions(), group = 'Sites', layerId=paste0(RV$currentSites$ID)) 
         }  
       
        proxy %>%  leaflet::removeTiles('wmsl')
        proxy %>% addTiles(group = "Map")
        proxy %>% addWMSTiles(
            layerId = 'wmsl',
            baseUrl = srv,
            layers = lnum,
            options = WMSTileOptions(format = "image/png", transparent = T, opacity = input$wgtMapOpacity/100),
            group = "SLGA_V2",
            attribution = "TERN"
          ) 
        if(RV$currentProductRecord$V1Code != ''){
          

        proxy %>% addWMSTiles(
            layerId = 'wmsl2',
            baseUrl = V1Server,
            layers = V1L,
            options = WMSTileOptions(format = "image/png", transparent = T, opacity = input$wgtMapOpacity/100),
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
    
    
    output$downloadRasterData <- downloadHandler(
     
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
        })
    
    output$downloadSiteData <- downloadHandler(
      
      filename = function() {
        fname <- paste0(input$wProduct, '_SiteDataRaw.csv')
        fname
      },
      content = function(file) {
        
        layer <- getLayer()
        rPath <- paste0(dataStorePath, '/',  input$wProduct, '/Sites/', input$wProduct, '_SiteDataRaw.csv')

        
        if(file.exists(rPath)){
          file.copy(rPath, file)
        }else{
          
        }
      })
    
 
 
####  Point query the Map  ##############  
    
    observe({
      req(input$btnMapAction)
      RV$MapMode = input$btnMapAction
     
    })
    
    observe({
      
      click<-input$wMainMap_click
      mapMode <- isolate(RV$MapMode)
      print(paste0("Map mode is - ", mapMode))
      

       if(mapMode == 'Info'){


      #   if(is.null(click))
      #     return()
      #
      #
        pt <- st_sfc(st_point(c(click$lng, click$lat)), crs = 4326)
print('MMMMMMMMMMMMMMMMMMMMMMMMMMM')
        mwidth <- (input$wMainMap_bounds$north - input$wMainMap_bounds$south) / 100
        circle <- st_buffer(pt,mwidth)
        if(!is.null(RV$currentSites)){
        selectedPt <- which(st_contains(circle, RV$currentSites, sparse = FALSE))
        }else{selectedPt <- NULL}

        siteDataHTML <- ''
        if(!is.null(selectedPt)){
            if(length(selectedPt) > 0){
              sdf <- st_drop_geometry( RV$currentSites[selectedPt[1],])
              siteDataHTML <- '<div style="display: inline-block;white-space: nowrap"><table align="left" style="border:0px solid black;margin-left:auto;margin-right:auto;">'
              siteDataHTML <- paste0(siteDataHTML,  '<th colspan="3">Observed Site Data</th>')
              for (j in 1:ncol(sdf)) {
                if(colnames(sdf)[j] != 'Latitude' & colnames(sdf)[j] != 'Longitude' ){
                  siteDataHTML <- paste0(siteDataHTML,  '<tr><td style="text-align:left">',  colnames(sdf)[j], '</td><td style="text-align:center">=</td><td  style="text-align:right">', sdf[j], '</td></tr>' )
                }
              }
              siteDataHTML <- paste0(siteDataHTML,  '</table></div><BR><BR><div style="display: inline-block;white-space: nowrap">')
            }
        }


        siteDataHTML <- paste0(siteDataHTML,'<table align="left" style="border:0px solid black;margin-left:auto;margin-right:auto;">')
        siteDataHTML <- paste0(siteDataHTML,  '<th colspan="3">Modelled Values</th>')

        if(!is.null(RV$productDepths) ){

          RV$currentProductRecord <- configInfo[configInfo$Product == input$wProduct, ]
          dps <- str_split(RV$currentProductRecord$Depths, ';')
          valstr = ''

          for (i in 1:length(RV$productDepths)) {
            rPath <- paste0(dataStorePath, '/', input$wProduct, '/Rasters/', input$wProduct,'_',  input$wProductType,'_', RV$productDepths[i], '.tif')
            if(file.exists(rPath)) {
              r <- raster(rPath)
              ptVal <- extract(r, data.frame(click$lng, click$lat))
              siteDataHTML <- paste0(siteDataHTML,  '<tr><td style="text-align:left">', dps[[1]][i], '</td><td style="text-align:center">=</td><td  style="text-align:right">', format(round(ptVal, 2), nsmall = 2), '</td></tr>' )
            }
          }
        }else{
          ptVal <- extract(RV$currentRaster, data.frame(click$lng, click$lat))
          valstr = paste0('<b>', format(round(ptVal, 2), nsmall = 2), '</br>')

          siteDataHTML <- paste0(siteDataHTML,  '<tr><td style="text-align:left"> Modelled ', input$wProduct, '</td><td style="text-align:center">=</td><td  style="text-align:right">', format(round(ptVal, 2), nsmall = 2), '</td></tr>' )
        }

        siteDataHTML <- paste0(siteDataHTML,  '</table></div><BR>')
        htmlOut = paste0(siteDataHTML)
        
        showModal(modalDialog( title = paste0("Attribute Data for ", input$wProduct), HTML( htmlOut), easyClose = T, fade = F, size = 's' ))

        #shinyalert(input$wProduct, htmlOut, type = "info", html=T, animation = F,  closeOnEsc = TRUE, closeOnClickOutside = T)

       }else{}
    })
    
    
  
}

shinyApp(ui = ui, server = server)
