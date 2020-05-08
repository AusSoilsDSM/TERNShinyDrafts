rootDir <- '/OSM/CBR/LW_SLGA/work/Projects/NAWRA/Production'

machineName <- Sys.info()['nodename'] 
if(machineName == 'TERNSOILS'){
  rootDir <- '//OSM-09-CDC.it.csiro.au/OSM_CBR_LW_SLGA_work/Projects/NAWRA/Production'
}else{
  rootDir <- '/OSM/CBR/LW_SLGA/work/Projects/NAWRA/Production'
}

adminEmail <- 'ross.searle@csiro.au'
adminName <- 'Ross Searle'

dataStorePath <- 'E:/ReviewDataStore'

productTypes <-c('Model-Value', '5-percent-CI', '95-percent-CI')

OGCserver = "http://localhost/cgi-bin/mapserv.exe?map=C:/ms4w/SLGA/SLGA.map&SERVICE=WMS&VERSION=1.0.0"

mapExtent = '112.9995833330000039 -43.9995833340000075 153.9995833339999933 -9.9995833330000004'



