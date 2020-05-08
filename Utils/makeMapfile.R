library(stringr)

source('C:/Users/sea084/Dropbox/RossRCode/Git/Shiny/TERNShinyDrafts/appConfig.R')

settingsPath <- 'E:/ReviewDataStore/DataStoreConfig.csv'
outFile <- 'C:/ms4w/SLGA/SLGA.map'

settings <- read.csv(settingsPath, stringsAsFactors = F)

M <- writeMapFileHeader(mapName, mapExtent, serverPath)

for (i in 1:nrow(settings)) {
  
  attribute <- settings$Product[i]
  dps <- str_split(settings$Depths[i], ';')[[1]]
  
  for (j in 1:length(dps)) {
    
    for (k in 1:length(productTypes)) {
      
      L <-  writeMapLayer(attribute=attribute, depth=dps[j], type=productTypes[k], lextent=mapExtent, lStyle=settings$Style[i])
      M <- paste0(M, L)
      
    }
  }
}

M <- paste0(M, '\n\nEND')
cat(M, file = outFile, append = F)





writeMapLayer <- function(attribute, depth, type, lextent, lStyle){
  
  L <- paste0("LAYER
    NAME '", attribute, "_", type, "_", depth,"'
    TYPE RASTER
    EXTENT ", mapExtent, "
    DATA '", paste0(attribute, "/Rasters/", attribute, "_", type, "_", depth,".tif"), "'
    
    METADATA
      'wcs_label'           'Bob'  ### required
      'wcs_rangeset_name'   'Mary'  ### required to support DescribeCoverage request
      'wcs_rangeset_label'  'Fred' ### required to support DescribeCoverage request
      'ows_size' '1995 4086'
      'wcs_bandcount' '1'
    END
    STATUS On

    PROJECTION
    'init=epsg:4326'
    END
    
    INCLUDE '", lStyle, ".txt'

  END # Layer
              \n\n")
              
  return(L)
}

writeMapFileHeader <- function(mapName, mapExtent, serverPath){
  
  h <- paste0("MAP   
  NAME '", mapName, "'
  SIZE 900 900 
  UNITS meters

  EXTENT ", mapExtent, " 
  
  MAXSIZE 15000 
 
   IMAGECOLOR 255 255 255 
 
   SHAPEPATH '", dataStorePath, "' 
  
  OUTPUTFORMAT  
    NAME GEOTIFF_16 
    DRIVER 'GDAL/GTiff' 
    MIMETYPE 'image/tiff' 
    IMAGEMODE FLOAT32
    EXTENSION 'tif'  
  END  
   
  PROJECTION  
      'init=epsg:4326'  
  END  
    
  WEB  
  
  IMAGEPATH 'c:/temp'  
  IMAGEURL '/tmp/'  
 
  # OGC server settings  
  METADATA\n  
      'ows_title'           '", mapName, "'  
      'ows_onlineresource'  '", OGCserver, "'  
      'ows_srs'             'EPSG:4326  EPSG:3857' 
      'ows_label'           'TERN Landscapes Review'  
      'ows_description'     'Web Services for viewing TERN Landscapes draft products'     
      'ows_fees'            'none' 
      'ows_accessconstraints'    'TERN Collaborators only'  
      'ows_keywordlist'          'wcs wms TERN SLGA'  
      'ows_enable_request' '*'  
  	    
  END  
  
  END\n 
")
  
  return(h)
}