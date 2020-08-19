df <- read.csv('D:/ReviewDataStore/DataPrep/combined_morph_PSA_data.csv')
df2 <- read.csv('D:/ReviewDataStore/DataPrep/combined_lab_PSA_data.csv')
head(df2)


unique(df$Provider)

####   Clay  #######
clydf<- df[, c(2,3,4,5,6,7,8,9,10,11, 14, 12, 13)]
clydf$DataType <- 'Morphological'
head(clydf)
str(df)
colnames(clydf) <- c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate", "Longitude", "Latitude", "UpperDepth", "LowerDepth", "clayPercent", "Texture1", "Texture2", "DataType"   )
head(clydf)


clydf2<- df2[, c(1,2,3,4,5,6,7,8,9,10, 11 )]
clydf2$Texture1 <- NA_character_
clydf2$Texture2 <- NA_character_
clydf2$DataType <- 'Laboratory'
head(clydf2)
str(df2)
colnames(clydf2) <- c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate", "Longitude", "Latitude", "UpperDepth", "LowerDepth", "clayPercent", "Texture1", "Texture2", "DataType"   )
head(clydf2)

outdf <- rbind(clydf, clydf2)
ids <- which(outdf$Provider != 'LawsonGrains')
outdf2 <- outdf[ids,]
write.csv(outdf2, file='D:/ReviewDataStore/DataPrep/clay_Site_data.csv', row.names = F)

# df1 <- outdf2[outdf2$UpperDepth == 0, c(1,2,3,4,5,6,7,8, 11 ) ]
# df2 <- outdf2[outdf2$UpperDepth == 5, c(4,11 ) ]
# df3 <- outdf2[outdf2$UpperDepth == 15, c(4,11 ) ]
# m1 <- merge(df1, df2, by='Observation_ID', all.x=T)
# head(m1)

####   Silt  #######
siltdf<- df[, c(2,3,4,5,6,7,8,9,10,11, 15, 12, 13)]
siltdf$DataType <- 'Morphological'
head(siltdf)
str(siltdf)
colnames(siltdf) <- c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate", "Longitude", "Latitude", "UpperDepth", "LowerDepth", "siltPercent", "Texture1", "Texture2", "DataType"   )
head(siltdf)

siltdf2<- df2[, c(1,2,3,4,5,6,7,8,9,10, 12 )]
siltdf2$Texture1 <- NA_character_
siltdf2$Texture2 <- NA_character_
siltdf2$DataType <- 'Laboratory'
head(siltdf2)
str(siltdf2)
colnames(siltdf2) <- c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate", "Longitude", "Latitude", "UpperDepth", "LowerDepth", "siltPercent", "Texture1", "Texture2", "DataType"   )
head(siltdf2)

outdf <- rbind(siltdf, siltdf2)
ids <- which(outdf$Provider != 'LawsonGrains')
outdf2 <- outdf[ids,]
head(outdf2)
write.csv(outdf2, file='D:/ReviewDataStore/DataPrep/silt_Site_data.csv', row.names = F)



####   Sand  #######
sanddf<- df[, c(2,3,4,5,6,7,8,9,10,11, 16, 12, 13)]
sanddf$DataType <- 'Morphological'
head(sanddf)
str(sanddf)
colnames(sanddf) <- c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate", "Longitude", "Latitude", "UpperDepth", "LowerDepth", "sandPercent", "Texture1", "Texture2", "DataType"   )
head(sanddf)

sanddf2<- df2[, c(1,2,3,4,5,6,7,8,9,10, 13 )]
sanddf2$Texture1 <- NA_character_
sanddf2$Texture2 <- NA_character_
sanddf2$DataType <- 'Laboratory'
head(sanddf2)
str(sanddf2)
colnames(sanddf2) <- c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate", "Longitude", "Latitude", "UpperDepth", "LowerDepth", "sandPercent", "Texture1", "Texture2", "DataType"   )
head(sanddf2)

outdf <- rbind(sanddf, sanddf2)
ids <- which(outdf$Provider != 'LawsonGrains')
outdf2 <- outdf[ids,]
write.csv(outdf2, file='D:/ReviewDataStore/DataPrep/sand_Site_data.csv', row.names = F)



library(ithir)
library(aqp)


df <- read.csv('D:/ReviewDataStore/Silt/Sites/Silt_SiteData.csv')
df <- read.csv('D:/ReviewDataStore/Silt/Sites/Silt_SiteData.csv')
df2 <- na.omit(df[df$DataType == 'Morphological', c(4, 7,8,9,10,11)])

head(df2)
depths(df2) <- Observation_ID ~ UpperDepth + LowerDepth
site(df2) <- ~ Longitude + Latitude
proj4string(df2) <- CRS("+proj=longlat +datum=WGS84")

class(df2)
sp.fit<- ea_spline(df2, var.name="siltPercent")




