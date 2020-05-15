
machineName <- Sys.info()['nodename'] 
if(machineName == 'soils-discovery'){
  dataStorePath <- '/OSM/CBR/LW_SOILDATAREPO/work/TERNLandscapes/ReviewDataStore'
  #OGCserver = "http://ternsoil2:8080/cgi-bin/mapserv.exe?map=C:/ms4w/SLGA/SLGA.map"
  OGCserver =  "https://www.asris.csiro.au/arcgis/services/SLGApReview/XXXX/MapServer/WMSServer"
  V1Server = "http://www.asris.csiro.au/arcgis/services/TERN/XXXX_ACLEP_AU_NAT_C/MapServer/WMSServer"
}else{
  dataStorePath <- 'E:/ReviewDataStore'
  #OGCserver = "http://localhost/cgi-bin/mapserv.exe?map=C:/ms4w/SLGA/SLGA.map"
  OGCserver =  "https://www.asris.csiro.au/arcgis/services/SLGApReview/XXXX/MapServer/WMSServer"
  V1Server = "http://www.asris.csiro.au/arcgis/services/TERN/XXXX_ACLEP_AU_NAT_C/MapServer/WMSServer"
}

adminEmail <- 'ross.searle@csiro.au'
adminName <- 'Ross Searle'

mapExtent = '112.9995833330000039 -43.9995833340000075 153.9995833339999933 -9.9995833330000004'

configInfo <- read.csv(paste0(dataStorePath, '/DataStoreConfig.csv'), stringsAsFactors = F)
WMSMappings <- read.csv(paste0(dataStorePath, '/WMSmappings.csv'), stringsAsFactors = F)
