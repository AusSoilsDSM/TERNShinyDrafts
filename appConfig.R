
machineName <- Sys.info()['nodename'] 
if(machineName == 'soils-discovery'){
  dataStorePath <- '/OSM/CBR/LW_SOILDATAREPO/work/TERNLandscapes/ReviewDataStore'
  #OGCserver = "http://ternsoil2:8080/cgi-bin/mapserv.exe?map=C:/ms4w/SLGA/SLGA.map"
  OGCserver =  "https://www.asris.csiro.au/arcgis/services/draft/CLY_000_005_05_N_P_AU_NAT_C_20140801/MapServer/WMSServer"
}else{
  dataStorePath <- 'E:/ReviewDataStore'
  OGCserver = "http://localhost/cgi-bin/mapserv.exe?map=C:/ms4w/SLGA/SLGA.map"
}

adminEmail <- 'ross.searle@csiro.au'
adminName <- 'Ross Searle'

#dataStorePath <- 'E:/ReviewDataStore'

productTypes <-c('Model-Value', '5-percent-CI', '95-percent-CI')

#OGCserver = "http://localhost/cgi-bin/mapserv.exe?map=C:/ms4w/SLGA/SLGA.map"

mapExtent = '112.9995833330000039 -43.9995833340000075 153.9995833339999933 -9.9995833330000004'



