
machineName <- Sys.info()['nodename'] 
if(machineName == 'soils-discovery'){
  dataStorePath <- '/OSM/CBR/LW_SOILDATAREPO/work/TERNLandscapes/ReviewDataStore'
  #OGCserver = "http://ternsoil2:8080/cgi-bin/mapserv.exe?map=C:/ms4w/SLGA/SLGA.map"
  OGCserver =  "https://www.asris.csiro.au/arcgis/services/TERNL/XXXX/MapServer/WMSServer"
  V1Server = "http://www.asris.csiro.au/arcgis/services/TERN/XXXX_ACLEP_AU_NAT_C/MapServer/WMSServer"
}else{
  dataStorePath <- 'D:/ReviewDataStore'
  #OGCserver = "http://localhost/cgi-bin/mapserv.exe?map=C:/ms4w/SLGA/SLGA.map"
              #  https://www.asris.csiro.au/arcgis/services/TERNL/AP_N_P_AU_NAT_N_20200810/MapServer/WmsServer?
  OGCserver =  "https://www.asris.csiro.au/arcgis/services/TERNL/XXXX/MapServer/WMSServer"
  V1Server = "http://www.asris.csiro.au/arcgis/services/TERN/XXXX_ACLEP_AU_NAT_C/MapServer/WMSServer"
}

authUsr<- "Landscapes"
authPwd <- "Sodosol"

adminEmail <- 'ross.searle@csiro.au'
adminName <- 'Ross Searle'

mapExtent = '112.9995833330000039 -43.9995833340000075 153.9995833339999933 -9.9995833330000004'

configInfo <- read.csv(paste0( 'Configs/DataStoreConfig.csv'), stringsAsFactors = F)
WMSMappings <- read.csv(paste0( 'Configs/WMSmappings.csv'), stringsAsFactors = F)


selectRadius <- 0.001

#print(str(configInfo))