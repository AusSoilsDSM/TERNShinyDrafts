library(ithir)
library(aqp)


df <- read.csv('D:/ReviewDataStore/Silt/Sites/Silt_SiteData.csv')

df2 <- na.omit(df[(df$DataType == 'Morphological' & df$UpperDepth < 10.0 & df$LowerDepth < 10.0 & df$UpperDepth >= 0 & df$LowerDepth >= 0), c(4, 7,8,9,10,11)])
summary(df2)
hist(df2$UpperDepth)

df2$UpperDepth <- df2$UpperDepth * 100
df2$LowerDepth <- df2$LowerDepth * 100

head(df2)
depths(df2) <- Observation_ID ~ UpperDepth + LowerDepth
site(df2) <- ~ Longitude + Latitude
#proj4string(df2) <- CRS("+proj=longlat +datum=WGS84")

class(df2)
sp.fit <- ea_spline(df2, var.name="siltPercent")
saveRDS(sp.fit, 'c:/temp/sdr/Silt.rds')