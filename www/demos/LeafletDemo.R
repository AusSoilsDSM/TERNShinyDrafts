library(leaflet)
library(leafem)
library(leaflet.extras)
library(httr)


url <- paste0('http://esoil.io/thredds/wcs/SMIPSall/SMIPSv0.5.nc?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage&FORMAT=GeoTIFF&COVERAGE=', att, '&CRS=OGC:CRS84&TIME=', d, 'T00:00:00Z')

####  Download a png file using the WMS endpoint - not that useful really
url <- 'https://www.asris.csiro.au/arcgis/services/SLGApReview/clay/MapServer/WMSServer?SERVICE=WMS&VERSION=1.0.0&REQUEST=GetMap&LAYERS=11&SRS=EPSG:4326&WIDTH=400&HEIGHT=300&FORMAT=image/png&BBOX=110,-44,155,-10&Styles='
download.file(url, 'c:/temp/wfs.png', mode = 'wb', quiet = T)

####  Download a png file - not that useful really
url <- 'https://www.asris.csiro.au/arcgis/services/SLGApReview/clay/MapServer/WCSServer?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage&CRS=EPSG:4326&COVERAGE=1&RESX=0.00083333=&RESY=0.00083333&FORMAT=GeoTIFF&BBOX=140,-26,141,-25'
download.file(url, 'c:/temp/wfc.tif', mode = 'wb', quiet = T)
r <- raster('c:/temp/wfc.tif')
plot(r)


##### View the data using Leaflet

srv = 'https://www.asris.csiro.au/arcgis/services/SLGApReview/clay/MapServer/WMSServer'
V1Server = 'http://www.asris.csiro.au/arcgis/services/TERN/CLY_ACLEP_AU_NAT_C/MapServer/WMSServer'

leaflet()  %>% setView(lng = 134, lat = -26, zoom = 4) %>% addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = F), group = "Satelite Image") %>%
  
  leafem::addMouseCoordinates() %>%
  leaflet.extras::addSearchOSM(options = searchOptions(autoCollapse = T, minLength = 2)) %>% 
  addLayersControl(
    baseGroups = c("Satelite Image", "Map"),
    overlayGroups = c( "SLGA_V1", "SLGA_V2"),
    options = layersControlOptions(collapsed = FALSE),
    
  ) %>%
 addWMSTiles(
    layerId = 'wmsl',
    baseUrl = srv,
    layers = 11,
    options = WMSTileOptions(format = "image/png", transparent = T),
    group = "SLGA_V2",
    attribution = "TERN"
  ) %>%
  addWMSLegend(uri = paste0(srv, '?VERSION=1.3.0&layer=', 11, '&REQUEST=GetLegendGraphic&FORMAT=image/png'),  position =  "bottomright") %>%

  addWMSTiles(
  layerId = 'wmsl2',
  baseUrl = V1Server,
  layers = 'CLY_000_005_EV_N_P_AU_NAT_C',
  options = WMSTileOptions(format = "image/png", transparent = T),
  group = "SLGA_V1",
  attribution = "TERN"
)
