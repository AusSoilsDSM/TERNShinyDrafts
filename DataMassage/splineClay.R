library(ithir)
library(aqp)
library(mpspline2)




df <- read.csv('D:/ReviewDataStore/Sand/Sites/Sand_SiteData.csv')

df2 <- na.omit(df[(df$DataType == 'Morphological' & df$UpperDepth < 10.0 & df$LowerDepth < 10.0 & df$UpperDepth >= 0 & df$LowerDepth >= 0), c(4, 7,8,9,10,11)])
#summary(df2)
#hist(df2$UpperDepth)

df2$UpperDepth <- df2$UpperDepth * 100
df2$LowerDepth <- df2$LowerDepth * 100

idxs = which(df2$UpperDepth != df2$LowerDepth)
df2 <- df2[idxs,]

# head(df2)
# depths(df2) <- Observation_ID ~ UpperDepth + LowerDepth
# site(df2) <- ~ Longitude + Latitude
# #proj4string(df2) <- CRS("+proj=longlat +datum=WGS84")
# 
# class(df2)
# sp.fit <- ea_spline(df2, var.name="clayPercent")
# saveRDS(sp.fit, 'c:/temp/sdr/clay.rds')
# 
# 
# 
# df3 <- df2[c(1,4,5,6)]
# df3$UpperDepth <- df3$UpperDepth * 100
# df3$LowerDepth <- df3$LowerDepth * 100
# 
# idxs = which(df3$UpperDepth != df3$LowerDepth)
# df3 <- df3[idxs,]
# 
# write.csv(df3, 'c:/temp/df3.csv', row.names = F)
# 
# df3 <- read.csv('c:/temp/df3.csv')
# m1 <- mpspline(obj = df3, var_name = 'clayPercent', vlow = 0, vhigh = 100)


outF <- cat('ID, Longitude, Latitude, 000_005_cm, 005_015_cm, 015_030_cm, 030_060_cm, 060_100_cm, 100_200_cm\n', file='c:/temp/SandOut.csv', append = F)
ids <- unique(df2$Observation_ID)
for (i in 1:length(ids)) {
 try( doSpline(i, df2))
}

doSpline <- function(i, df2){
  print(paste0(i, ' of ', length(ids)))
  df <- df2[df2$Observation_ID == ids[i],]
  dfs <- df[, c(1,4,5,6)]
  m1 <- mpspline(obj = dfs,  var_name = 'sandPercent', vlow = 0, vhigh = 100)
  odf <- cat(df[1,1], df[1,2], df[1,3],  m1[[1]]$est_dcm, '\n', file='c:/temp/SandtOut.csv', sep = ',' , append = T)
}
