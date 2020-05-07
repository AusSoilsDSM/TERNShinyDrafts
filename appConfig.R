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

productTypes <-c('Model Value', '5% CI', '95% CI', 'V1 V2 Difference')




