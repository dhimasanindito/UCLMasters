#######################################
#### Algorithms of My Dissertation ####
#######################################

# Dhimas Anindito
# MSc Smart Cities and Urban Analytics, University College London

# The algorithms are divided into several sections: data cleaning and preparation, Spatial Autocorrelation,
# Temporal data visualisation, K-means clustering, DBSCAN, Structural Equation Modelling,  
# Geographically Weighted Regression (GWR), and Multiscale GWR (MGWR)

############################
##### Library Settings #####
############################

library(gdata)
library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(sf)
library(geojsonio)
library(tmaptools)
library(tmap)
library(raster)
library(fpc)
library(plyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(classInt)
library(spdep) 
library(ggmap) # For basemap
library(lubridate) # For calculating age from DOB
library(chron) # For assigning weekends as Boolean value
library(corrplot) # For correlation analysis
library(SciViews)
library(vegan) # For Shannon's Index
library(broom)
library(dplyr)
library(spatstat)
library(raster)

setwd("/Users/dhimasbayuanindito/Google Drive/Dissertation")
getwd()

########################################
##### Data Reading and Preparation #####
########################################

#•••••••••••••••••••••••
#•••• Boundary Data ••••
#•••••••••••••••••••••••

#-------------------------------
#--------- Kecamatan -----------
#-------------------------------

# Reading the shapefile of kelurahan into a simple features object
KecamatanSF <- st_read(kec_shp_directory)
summary(KecamatanSF)

# Plotting it to check it has been read in correctly
qtm(KecamatanSF)
print(KecamatanSF)

# Transforming Kecamatan into SF
KecamatanSP <- sf:::as_Spatial(KecamatanSF$geom)
KecamatanSP

# Reprojecting kecamatan layer
WGS = "+init=epsg:23834" # For Indonesia
Kecamatan <- spTransform(KecamatanSP, WGS)
summary(Kecamatan)

#-------------------------------
#--------- Kelurahan -----------
#-------------------------------


# Reading the shapefile of kelurahan into a simple features object
KelurahanSF <- st_read("Jakarta Shapefile/dki_kelurahan/dki_kelurahan.shp")
summary(KelurahanSF)

# Plotting it to check it has been read in correctly
qtm(KelurahanSF)
print(KelurahanSF)

# Transforming kelurahan into SF
KelurahanSP <- sf:::as_Spatial(KelurahanSF$geom)
KelurahanSP

# Reprojecting kelurahan layer
WGS = "+init=epsg:23834" # For Indonesia
Kelurahan <- spTransform(KelurahanSP, WGS)
summary(KelurahanSF)

#•••••••••••••••••••••
#•••• Report Data ••••
#•••••••••••••••••••••

# Reading the geoJSON file of reports
Reports <- geojson_read("Reports.geojson", what = "sp")
summary(Reports)

# Reprojecting the SPDF of reports
Reports <- spTransform(Reports, WGS)
summary(Reports)

# Visualising the points
tm_shape(Reports) + tm_dots(col='blue')

# Removing any reports with the same grid reference
Reports <- remove.duplicates(Reports)
Reports

# Omitting null values
Reports <- na.omit(Reports)
Reports

# Selecting the points inside Jakarta only
ReportsSub <- Reports[Kelurahan,]
ReportsSub

# Visualising the point layer over kelurahan layer
tm_shape(Kelurahan) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(ReportsSub) +
  tm_scale_bar(position = c("left", "bottom"), size=0.75) + # Putting the scale bar 
  tm_dots(col = "maroon")

# Visualising the point layer over kecamatan layer
tm_shape(Kecamatan) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(ReportsSub) +
  tm_dots(col = "blue")

# Counting the reports per kelurahan
res <- poly.counts(ReportsSub, Kelurahan)
res
sum(res)

# Adding the report count as a column in SPDF
Kelurahan$ReportsCount<-res

# Assigning day to each report
ReportsSub@data$Day <- weekdays(as.Date(ReportsSub@data$Date))

# Making a dataframe based on the day when the report is submitted
ReportDay <- data.frame(ReportsSub@data$Day)

# Assigning date of each report as weekend (in Boolean)
ReportsSub@data$Weekend <- chron::is.weekend(ReportsSub@data$Date)

# Formatting report time as time
time <- as.POSIXct(ReportsSub@data$Time, format = "%H:%M") %>% format("%H:%M:%S")

# Assigning type of time to each report (Morning starts from 6am to 12pm, Afternoon starts from 12pm to 6pm, Night starts from 6pm to 12am, Late evening starts from 12am to 6am)
timecategory <- cut(chron::times(time) , breaks = (1/24) * c(0,6,12,18,24))
ReportsSub@data$TimeCategory <- c("Midnight","Morning","Afternoon","Evening")[as.numeric(timecategory)]

# Assigning reports to hourly frame
hourcategory <- cut(chron::times(time) , breaks = (1/24) * c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24))
ReportsSub@data$HourCategory <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)[as.numeric(hourcategory)]

# -------------------------------------
# Subsetting report based on categories
# -------------------------------------

# Flood-related reports
Flood <- ReportsSub[ReportsSub@data$Category == "Cegah Banjir" |
                      ReportsSub@data$Category == "Potensi Banjir" |
                      ReportsSub@data$Category == "Bencana Banjir",]
plot(Flood)

# Assigning point count of flood report to each kelurahan
KelurahanSF$Flood <- poly.counts(Flood,Kelurahan)

# Assigning point count of flood report to each kecamatan
KecamatanSF$Flood <- poly.counts(Flood,Kecamatan)

# Traffic-related reports
Traffic <- ReportsSub[ReportsSub@data$Category == "Jalan Rusak" |
                        ReportsSub@data$Category == "Kaki Lima Liar" |
                        ReportsSub@data$Category == "Kemacetan" |
                        ReportsSub@data$Category == "Macet" |
                        ReportsSub@data$Category == "Mobilitas dan Akses" |
                        ReportsSub@data$Category == "Parkir Liar" |
                        ReportsSub@data$Category == "Pelanggaran Lalu Lintas" |
                        ReportsSub@data$Category == "PJU Rusak" |
                        ReportsSub@data$Category == "Rambu Jalan" |
                        ReportsSub@data$Category == "Pungutan Liar" |
                        ReportsSub@data$Category == "Pengemis",]
plot(Traffic)

# Assigning point count of traffic report to each kelurahan
KelurahanSF$Traffic <- poly.counts(Traffic,Kelurahan)

# Assigning point count of traffic report to each kecamatan
KecamatanSF$Traffic <- poly.counts(Traffic,Kecamatan)

# Transportation-related reports
Transport <- ReportsSub[ReportsSub@data$Category == "Transportasi" |
                          ReportsSub@data$Category == "Transportasi Umum",]

# Assigning point count of transport report to each kelurahan
KelurahanSF$Transport <- poly.counts(Transport,Kelurahan)

# Assigning point count of transportation report to each kecamatan
KecamatanSF$Transport <- poly.counts(Transport,Kecamatan)

# Smoking-related reports
Smoking <- ReportsSub[ReportsSub@data$Category == "Dilarang Merokok" |
                        ReportsSub@data$Category == "Larangan Merokok",]
plot(Smoking)

# Assigning point count of smoking report to each kelurahan
KelurahanSF$Smoking <- poly.counts(Smoking,Kelurahan)

# Assigning point count of smoking report to each kecamatan
KecamatanSF$Smoking <- poly.counts(Smoking,Kecamatan)

# Reports related to public facilities
PF <- ReportsSub[ReportsSub@data$Category == "Fasilitas Anak" | 
                   ReportsSub@data$Category == "Fasilitas Umum" |
                   ReportsSub@data$Category == "RPTRA" |
                   ReportsSub@data$Category == "Iklan Liar" |
                   ReportsSub@data$Category == "Tanaman Bermasalah" |
                   ReportsSub@data$Category == "Pohon Tumbang" |
                   ReportsSub@data$Category == "Pelanggaran" |
                   ReportsSub@data$Category == "Pelanggaran Bangunan" |
                   ReportsSub@data$Category == "Pelanggaran IMB" |
                   ReportsSub@data$Category == "Pelanggaran Ketertiban Umum" |
                   ReportsSub@data$Category == "Pilkada" |
                   ReportsSub@data$Category == "Rumah Tangga" |
                   ReportsSub@data$Category == "Sampah" |
                   ReportsSub@data$Category == "Pelayana Pemerintah",]
plot(PF)

# Assigning point count of public facilities report to each kelurahan
KelurahanSF$PF <- poly.counts(PF,Kelurahan)

# Assigning point count of public facilities report to each kecamatan
KecamatanSF$PF <- poly.counts(PF,Kecamatan)

# Crime-related reports
Crime <- ReportsSub[ReportsSub@data$Category == "Kriminal" |
                      ReportsSub@data$Category == "Narkoba",]

# Assigning point count of crime report to each kelurahan
KelurahanSF$PF <- poly.counts(Crime,Kelurahan)

# Assigning point count of crime report to each kecamatan
KecamatanSF$PF <- poly.counts(Crime,Kecamatan)


#•••••••••••••••••••
#•••• User Data ••••
#•••••••••••••••••••

### Reading the user data
QlueUser <- read_csv("Data Pelapor Dari Januari 2018 - Juni 2018.csv")
### Omitting null values
QlueUser <- na.omit(QlueUser)
QlueUser

### Finding the aggregate data of user attributes by the boundary units
## Age
# Calculating the age of users
QlueUser$Age <- as.period(interval(start = as.Date(QlueUser$DOB), end = as.Date(Sys.Date())))$year

# Plotting the number of QLue users based on age
FigUserAge <- ggplot(data=QlueUser, aes(x=Age)) + 
  labs(x = "Age", y = "Count") + 
  geom_bar(stat="count") + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"), aes(fill = Age == 28)) + 
  theme(legend.position="none") +
  ggtitle("Number of Qlue users based on age") 
FigUserAge

# Eliminating people age <15 and beyond >65
User15 <-subset(QlueUser, Age >= 15)
User1565 <-subset(QlueUser, Age <= 66)

# Making a matrix of kecamatan based on the users' age
Kecamatan_Age <- table(User1565$Kecamatan, User1565$Age)

# Making a matrix of kelurahan based on the users' age
Kelurahan_Age <- table(User1565$Kelurahan, User1565$Age)

# Creating the number of users per administrative unit
QlueUser_Kelurahan_Gender <- table(QlueUser$Kelurahan, QlueUser$Gender)
QlueUser_Kelurahan_Total <- table(QlueUser$Kelurahan)

QlueUser_Kecamatan_Gender <- table(QlueUser$Kecamatan, QlueUser$Gender)
QlueUser_Kecamatan_Total <- table(QlueUser$Kecamatan)


############################
#### Kelurahan Analysis ####
############################

## CALCULATING DENSITY OF REPORTS PER AREA
# Density of reports per area
Kelurahan$ReportsDensityArea <- Kelurahan$ReportsCount/poly.areas(Kelurahan)
Kelurahan$ReportsDensityArea

# Visualising the density of reports based on the area
tm_shape(Kelurahan) +
  tm_polygons("ReportsDensityArea",
              style="jenks",
              palette="Blues",
              midpoint=NA,
              title="Reports Density - Area")

## CALCULATING DENSITY OF REPORTS PER POPULATION
# Density of reports per population
Kelurahan$ReportsDensityPop <- Kelurahan$ReportsCount/KelurahanSF$Pop
Kelurahan$ReportsDensityPop

# Visualising the density of reports based on the population
tm_shape(Kelurahan) +
  tm_polygons("ReportsDensityPop",
              style="jenks",
              palette="Blues",
              midpoint=NA,
              title="Reports Density - Population")

## CALCULATING DENSITY OF REPORTS PER USERS
# Density of reports per users
Kelurahan$ReportsDensityUsers <- Kelurahan$ReportsCount/KelurahanSF$Total_User
Kelurahan$ReportsDensityUsers

# Visualising the density of reports based on the users
tm_shape(Kelurahan) +
  tm_polygons("ReportsDensityUsers",
              style="jenks",
              palette="Blues",
              midpoint=NA,
              title="Reports Density - Users")

#----------------------------------
#---- Spatial Autocorrelation -----
#----------------------------------

## Making the spatial weights matrix
# Calculating the centroids of all kelurahan in Jakarta
coordsKel <- coordinates(Kelurahan)
plot(coordsKel)
# Creating a neighbours list based on The Queen's Weight
Kelurahan_nb <- poly2nb(Kelurahan, queen=T, snap=0.1) 

# Plotting them
plot(Kelurahan_nb, coordinates(coordsKel), col="red")
# Adding the kelurahan map underneath
plot(Kelurahan, add=T)

# Creating a spatial weights object from these weights
Kelurahan.lw <- nb2listw(Kelurahan_nb, style="C", zero.policy = TRUE)
head(Kelurahan.lw$neighbours)

# •••••••••••••••••••••••••
# •••• DENSITY BY AREA ••••
# •••••••••••••••••••••••••

## GLOBAL SPATIAL AUTOCORRELATION
# Moran's I test 
I_Kelurahan_Global_Density <- moran.test(Kelurahan$ReportsDensityArea, Kelurahan.lw, zero.policy = TRUE)
I_Kelurahan_Global_Density

# Geary's C 
C_Kelurahan_Global_Density <- geary.test(Kelurahan$ReportsDensityArea, Kelurahan.lw, zero.policy = TRUE)
C_Kelurahan_Global_Density

# Getis Ord General G
G_Kelurahan_Global_Density <- globalG.test(Kelurahan$ReportsDensityArea, Kelurahan.lw, zero.policy = TRUE)
G_Kelurahan_Global_Density

## LOCAL SPATIAL AUTOCORRELATION
# Local Moran's I
I_Kelurahan_Local <- localmoran(Kelurahan$ReportsCount, Kelurahan.lw, zero.policy = TRUE)
I_Kelurahan_Local_Density <- localmoran(Kelurahan$ReportsDensityArea, Kelurahan.lw, zero.policy = TRUE)
# The first 10 of the result
head(I_Kelurahan_Local_Density)

# Copying the Moran's I score and z-score of standard deviation
Kelurahan$BLocI <- I_Kelurahan_Local[,1]
Kelurahan$BLocIz <- I_Kelurahan_Local[,4]
Kelurahan$BLocIR <- I_Kelurahan_Local_Density[,1]
Kelurahan$BLocIRz <- I_Kelurahan_Local_Density[,4]

# Setting the breaks based on the rule that data points >2.58 or <-2.58 standard deviations away from the mean are significant at the 99% level (<1% chance that autocorrelation not present); >1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
# Creating a colour palette based on the results
MoranColours<- rev(brewer.pal(8, "RdGy"))

# Plotting the map of Moran's I
tm_shape(Kelurahan) +
  tm_polygons("BLocIRz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Reports-Area")

## Getis Ord G* statistics
Gi_Kelurahan_Local_Density <- localG(Kelurahan$ReportsDensityArea, Kelurahan.lw, zero.policy = TRUE)
# The results
head(Gi_Kelurahan_Local_Density)

Kelurahan$BLocGiRz <- Gi_Kelurahan_Local_Density

GIColours<- rev(brewer.pal(8, "RdBu"))

# Plotting the map of Gi*
tm_shape(Kelurahan) +
  tm_polygons("BLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi* Reports-Area")

# •••••••••••••••••••••••••••••••
# •••• DENSITY BY POPULATION ••••
# •••••••••••••••••••••••••••••••

## GLOBAL SPATIAL AUTOCORRELATION
# Moran's I test 
I_Kelurahan_Global_Density_Pop <- moran.test(Kelurahan$ReportsDensityPop, Kelurahan.lw, zero.policy = TRUE)
I_Kelurahan_Global_Density_Pop

# Geary's C 
C_Kelurahan_Global_Density_Pop <- geary.test(Kelurahan$ReportsDensityPop, Kelurahan.lw, zero.policy = TRUE)
C_Kelurahan_Global_Density_Pop

# Getis Ord General G
G_Kelurahan_Global_Density_Pop <- globalG.test(Kelurahan$ReportsDensityPop, Kelurahan.lw, zero.policy = TRUE)
G_Kelurahan_Global_Density_Pop

## LOCAL SPATIAL AUTOCORRELATION
# Local Moran's I
I_Kelurahan_Local_Pop <- localmoran(Kelurahan$ReportsCount, Kelurahan.lw, zero.policy = TRUE)
I_Kelurahan_Local_DensityPop <- localmoran(Kelurahan$ReportsDensityPop, Kelurahan.lw, zero.policy = TRUE)
# The first 10 of the result
head(I_Kelurahan_Local_DensityPop)

# Copying the Moran's I score and z-score of standard deviation
Kelurahan$PopBLocI <- I_Kelurahan_Local_Pop[,1]
Kelurahan$PopBLocIz <- I_Kelurahan_Local_Pop[,4]
Kelurahan$PopBLocIR <- I_Kelurahan_Local_DensityPop[,1]
Kelurahan$PopBLocIRz <- I_Kelurahan_Local_DensityPop[,4]

# Setting the breaks based on the rule that data points >2.58 or <-2.58 standard deviations away from the mean are significant at the 99% level (<1% chance that autocorrelation not present); >1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
# Creating a colour palette based on the results
MoranColours<- rev(brewer.pal(8, "RdGy"))

# Plotting the map of Moran's I
tm_shape(Kelurahan) +
  tm_polygons("PopBLocIRz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Reports-Population")

## Getis Ord G* statistics
Gi_Kelurahan_Local_DensityPop <- localG(Kelurahan$ReportsDensityPop, Kelurahan.lw, zero.policy = TRUE)
# The results
head(Gi_Kelurahan_Local_DensityPop)

Kelurahan$PopBLocGiRz <- Gi_Kelurahan_Local_DensityPop

GIColours<- rev(brewer.pal(8, "RdBu"))

# Plotting the map of Gi*
tm_shape(Kelurahan) +
  tm_polygons("PopBLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi* Reports-Population")

# ••••••••••••••••••••••••••
# •••• DENSITY BY USERS ••••
# ••••••••••••••••••••••••••

## GLOBAL SPATIAL AUTOCORRELATION
# Moran's I test 
I_Kelurahan_Global_Density_Users <- moran.test(Kelurahan$ReportsDensityUsers, Kelurahan.lw, zero.policy = TRUE)
I_Kelurahan_Global_Density_Users

# Geary's C 
C_Kelurahan_Global_Density_Users <- geary.test(Kelurahan$ReportsDensityUsers, Kelurahan.lw, zero.policy = TRUE)
C_Kelurahan_Global_Density_Users

# Getis Ord General G
G_Kelurahan_Global_Density_Users <- globalG.test(Kelurahan$ReportsDensityUsers, Kelurahan.lw, zero.policy = TRUE)
G_Kelurahan_Global_Density_Users

## LOCAL SPATIAL AUTOCORRELATION
# Local Moran's I
I_Kelurahan_Local_Users <- localmoran(Kelurahan$ReportsCount, Kelurahan.lw, zero.policy = TRUE)
I_Kelurahan_Local_Density_Users <- localmoran(Kelurahan$ReportsDensityUsers, Kelurahan.lw, zero.policy = TRUE)
# The first 10 of the result
head(I_Kelurahan_Local_Density_Users)

# Copying the Moran's I score and z-score of standard deviation
Kelurahan$UsersBLocI <- I_Kelurahan_Local_Users[,1]
Kelurahan$UsersBLocIz <- I_Kelurahan_Local_Users[,4]
Kelurahan$UsersBLocIR <- I_Kelurahan_Local_Density_Users[,1]
Kelurahan$UsersBLocIRz <- I_Kelurahan_Local_Density_Users[,4]

# Setting the breaks based on the rule that data points >2.58 or <-2.58 standard deviations away from the mean are significant at the 99% level (<1% chance that autocorrelation not present); >1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
# Creating a colour palette based on the results
MoranColours<- rev(brewer.pal(8, "RdGy"))

# Plotting the map of Moran's I
tm_shape(Kelurahan) +
  tm_polygons("UsersBLocIRz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Reports-Users")

## Getis Ord G* statistics
Gi_Kelurahan_Local_Density_Users <- localG(Kelurahan$ReportsDensityUsers, Kelurahan.lw, zero.policy = TRUE)
# The results
head(Gi_Kelurahan_Local_Density_Users)

Kelurahan$UsersBLocGiRz <- Gi_Kelurahan_Local_Density_Users

GIColours<- rev(brewer.pal(8, "RdBu"))

# Plotting the map of Gi*
tm_shape(Kelurahan) +
  tm_polygons("UsersBLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi* Reports-User")

############################
#### Kecamatan Analysis ####
############################

#----------------------------------
#----------- Data Viz -------------
#----------------------------------

# Choropleth map of reports density
res1 <- poly.counts(ReportsSub, Kecamatan)
res1
#and add this as a column in our spatialPolygonsDataframe
Kecamatan$ReportsCount<-res1

## CALCULATING DENSITY OF REPORTS PER AREA
# Density of reports per area
Kecamatan$ReportsDensityArea <- Kecamatan$ReportsCount/poly.areas(Kecamatan)
#let's just check the data to see if the calculations have worked
Kecamatan$ReportsCount
Kecamatan

# Visualising the density of reports based on the area
 tm_shape(Kecamatan) +
  tm_polygons("ReportsDensityArea",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Reports Density - Area")


 ## CALCULATING DENSITY OF REPORTS PER POPULATION
 # Density of reports per population
 Kecamatan$ReportsDensityPopulation <- Kecamatan$ReportsCount/KecamatanSF$Population
 Kecamatan$ReportsDensityPopulation

 
 # Visualising the density of reports based on the population
 tm_shape(Kecamatan) +
   tm_polygons("ReportsDensityPopulation",
               style="jenks",
               palette="YlOrRd",
               midpoint=NA,
               title="Reports Density per Population")
 
 ## CALCULATING DENSITY OF REPORTS PER POOR HOUSEHOLD
 # Density of reports per low income household
 Kecamatan$ReportsDensityPoorHousehold <- Kecamatan$ReportsCount/KecamatanSF$Poor_House
 #let's just check the data to see if the calculations have worked
 Kecamatan$ReportsDensityPoorHousehold
 
 
 # Visualising the density of reports based on the area
 tm_shape(Kecamatan) +
   tm_polygons("ReportsDensityPoorHousehold",
               style="jenks",
               palette="YlOrRd",
               midpoint=NA,
               title="Reports Density per Poor Household")
 
 
 ## CALCULATING DENSITY OF REPORTS PER USERS
 # Density of reports per users
 Kecamatan$ReportsDensityUsers <- Kecamatan$ReportsCount/KecamatanSF$Total_User
 #let's just check the data to see if the calculations have worked
 
 # Visualising the density of reports based on the area
 tm_shape(Kecamatan) +
   tm_polygons("ReportsDensityUsers",
               style="jenks",
               palette="YlOrRd",
               midpoint=NA,
               title="Reports Density - User")
 
#----------------------------------
#---- Spatial Autocorrelation -----
#----------------------------------

## Making the spatial weights matrix

# Calculating the centroids of all kelurahan in Jakarta
coordsKec<- coordinates(Kecamatan)
plot(coordsKec)
# Creating a neighbours list based on The Queen's Weight
Kecamatan_nb <- poly2nb(Kecamatan, queen=T, snap=0.1)
# Plotting the graph of neighbours 
plot(Kecamatan_nb, coordinates(coordsKec), col="red")
# Adding a map underneath
plot(Kecamatan, add=T)

# Creating a spatial weight object from these weights
Kecamatan.lw <- nb2listw(Kecamatan_nb, style="C")
head(Kecamatan.lw$neighbours)


# •••••••••••••••••••••••••
# •••• DENSITY BY AREA ••••
# •••••••••••••••••••••••••

## GLOBAL SPATIAL AUTOCORRELATION
# Moran's I 
I_Kecamatan_Global_Density_Area <- moran.test(Kecamatan$ReportsDensityArea, Kecamatan.lw)
I_Kecamatan_Global_Density_Area

# Geary's C
C_Kecamatan_Global_Density_Area <- geary.test(Kecamatan$ReportsDensityArea, Kecamatan.lw)
C_Kecamatan_Global_Density_Area

# Getis Ord General G
G_Kecamatan_Global_Density_Area <- globalG.test(Kecamatan$ReportsDensityArea, Kecamatan.lw)
G_Kecamatan_Global_Density_Area

## LOCAL SPATIAL AUTOCORRELATION
# Local Moran's I
I_Kecamatan_Local_Area <- localmoran(Kecamatan$ReportsCount, Kecamatan.lw)
I_Kecamatan_Local_Density_Area <- localmoran(Kecamatan$ReportsDensityArea, Kecamatan.lw)
# The result
head(I_Kecamatan_Local_Density_Area)

# Copying the Moran's I score and z-score of standard deviation
Kecamatan$BLocI <- I_Kecamatan_Local_Area[,1]
Kecamatan$BLocIz <- I_Kecamatan_Local_Area[,4]
Kecamatan$BLocIR <- I_Kecamatan_Local_Density_Area[,1]
Kecamatan$BLocIRz <- I_Kecamatan_Local_Density_Area[,4]

# Setting the breaks based on the rule that data points >2.58 or <-2.58 standard deviations away from the mean are significant at the 99% level (<1% chance that autocorrelation not present); >1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
# Creating a new diverging colour brewer palette and reverse the order using rev so higher values correspond to red
MoranColours<- rev(brewer.pal(8, "RdGy"))

# Plotting a map based on Moran's I score
tm_shape(Kecamatan) +
  tm_polygons("BLocIRz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Reports-Area")

## Getis Ord G* statistics
Gi_Kecamatan_Local_Density_Area <- localG(Kecamatan$ReportsDensityArea, Kecamatan.lw)
head(Gi_Kecamatan_Local_Density_Area)

Kecamatan$BLocGiRz <- Gi_Kecamatan_Local_Density_Area

GIColours<- rev(brewer.pal(8, "RdBu"))

# Plotting a choropleth map based on Gi* score
tm_shape(Kecamatan) +
  tm_polygons("BLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Reports-Area")


# •••••••••••••••••••••••••••••••
# •••• DENSITY BY POPULATION ••••
# •••••••••••••••••••••••••••••••

## GLOBAL SPATIAL AUTOCORRELATION
# Moran's I
I_Kecamatan_Global_Density_Pop <- moran.test(Kecamatan$ReportsDensityPopulation, Kecamatan.lw)
I_Kecamatan_Global_Density_Pop

# Geary's C
C_Kecamatan_Global_Density_Pop <- geary.test(Kecamatan$ReportsDensityPopulation, Kecamatan.lw)
C_Kecamatan_Global_Density_Pop

# Getis Ord General G
G_Kecamatan_Global_Density_Pop <- globalG.test(Kecamatan$ReportsDensityPopulation, Kecamatan.lw)
G_Kecamatan_Global_Density_Pop

## LOCAL SPATIAL AUTOCORRELATION
# Moran's I 
I_Kecamatan_Local_Pop <- localmoran(Kecamatan$ReportsCount, Kecamatan.lw)
I_Kecamatan_Local_Density_Pop <- localmoran(Kecamatan$ReportsDensityPopulation, Kecamatan.lw)
# The result
head(I_Kecamatan_Local_Density_Pop)

# Copying the Moran's I score and z-score of standard deviation
Kecamatan$PopBLocI <- I_Kecamatan_Local_Pop[,1]
Kecamatan$PopBLocIz <- I_Kecamatan_Local_Pop[,4]
Kecamatan$PopBLocIR <- I_Kecamatan_Local_Density_Pop[,1]
Kecamatan$PopBLocIRz <- I_Kecamatan_Local_Density_Pop[,4]

# Setting the breaks based on the rule that data points >2.58 or <-2.58 standard deviations away from the mean are significant at the 99% level (<1% chance that autocorrelation not present); >1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
# Creating a new diverging colour brewer palette and reverse the order using rev so higher values correspond to red
MoranColours<- rev(brewer.pal(8, "RdGy"))

# Plotting a map based on Moran's I 
tm_shape(Kecamatan) +
  tm_polygons("PopBLocIRz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Reports-Population")

## Getis Ord G* statistics
Gi_Kecamatan_Local_Density_Pop <- localG(Kecamatan$ReportsDensityPopulation, Kecamatan.lw)
#Check the help file  (?localG) to see what a localG object looks like - it is a bit different from a localMoran object as it only contains just a single value - the z-score (standardised value relating to whether high values or low values are clustering together)
head(Gi_Kecamatan_Local_Density_Pop)
Kecamatan$PopBLocGiRz <- Gi_Kecamatan_Local_Density_Pop

GIColours<- rev(brewer.pal(8, "RdBu"))

# Plotting a choropleth map based on Gi*
tm_shape(Kecamatan) +
  tm_polygons("PopBLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Reports-Population")

# •••••••••••••••••••••••••••••••••
# •••• DENSITY BY ACTIVE USERS ••••
# •••••••••••••••••••••••••••••••••

## GLOBAL SPATIAL AUTOCORRELATION
# Moran's I 
I_Kecamatan_Global_Density_Users <- moran.test(Kecamatan$ReportsDensityUsers, Kecamatan.lw)
I_Kecamatan_Global_Density_Users

# Geary's C
C_Kecamatan_Global_Density_Users <- geary.test(Kecamatan$ReportsDensityUsers, Kecamatan.lw)
C_Kecamatan_Global_Density_Users

# Getis Ord General G
G_Kecamatan_Global_Density_Users <- globalG.test(Kecamatan$ReportsDensityUsers, Kecamatan.lw)
G_Kecamatan_Global_Density_Users

## LOCAL SPATIAL AUTOCORRELATION
# Local Moran's I
I_Kecamatan_Local_Area <- localmoran(Kecamatan$ReportsCount, Kecamatan.lw)
I_Kecamatan_Local_Density_Area <- localmoran(Kecamatan$ReportsDensityArea, Kecamatan.lw)
# The result
head(I_Kecamatan_Local_Density_Area)

# Copying the Moran's I score and z-score of standard deviation
Kecamatan$BLocI <- I_Kecamatan_Local_Area[,1]
Kecamatan$BLocIz <- I_Kecamatan_Local_Area[,4]
Kecamatan$BLocIR <- I_Kecamatan_Local_Density_Area[,1]
Kecamatan$BLocIRz <- I_Kecamatan_Local_Density_Area[,4]

# Setting the breaks based on the rule that data points >2.58 or <-2.58 standard deviations away from the mean are significant at the 99% level (<1% chance that autocorrelation not present); >1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
# Creating a new diverging colour brewer palette and reverse the order using rev so higher values correspond to red
MoranColours<- rev(brewer.pal(8, "RdGy"))

# Plotting a map based on Moran's I score
tm_shape(Kecamatan) +
  tm_polygons("BLocIRz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Reports-Users")

## Getis Ord G* statistics
Gi_Kecamatan_Local_Density_Area <- localG(Kecamatan$ReportsDensityArea, Kecamatan.lw)
head(Gi_Kecamatan_Local_Density_Area)

Kecamatan$BLocGiRz <- Gi_Kecamatan_Local_Density_Area

GIColours<- rev(brewer.pal(8, "RdBu"))

# Plotting a choropleth map based on Gi* score
tm_shape(Kecamatan) +
  tm_polygons("BLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Reports-Users")


#######################
#### User Analysis ####
#######################

# CALCULATING THE DENSITY OF USER PER AREA
Kelurahan$UsersDensityArea <- KelurahanSF$Total_User/poly.areas(Kelurahan)
Kecamatan$UsersDensityArea <- KecamatanSF$Total_User/poly.areas(Kecamatan)

# CALCULATING THE DENSITY OF USER PER POPULATION
Kelurahan$UserRatio <- KelurahanSF$Total_User / KelurahanSF$Pop
Kecamatan$UserRatio <- KecamatanSF$Total_User / KecamatanSF$Population


### Visualising the user ratio for kelurahan and kecamatan
## Kelurahan
# User density per area
tm_shape(Kelurahan) +
  tm_polygons("UsersDensityArea",
              style="jenks",
              palette="Blues",
              midpoint=NA,
              title="User Density in Area")


# User density per population
tm_shape(Kelurahan) +
  tm_polygons("UserRatio",
              style="jenks",
              palette="Blues",
              midpoint=NA,
              title="User Density - Population")

## Kecamatan
# User density per area
tm_shape(Kecamatan) +
  tm_polygons("UsersDensityArea",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="User Density in Area")

# User density per population
tm_shape(Kecamatan) +
  tm_polygons("UserRatio",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="User Density - Population")


#----------------------------------
#---- Spatial Autocorrelation -----
#----------------------------------

### Making the spatial weights matrix
## Kelurahan
# Calculating the centroids of all kelurahan in Jakarta
coordsW <- coordinates(Kelurahan)
plot(coordsW)
# Creating a neighbours list based on The Queen's Weight
Kelurahan_nb <- poly2nb(Kelurahan, queen=T, snap=0.1) 

# Plotting them
plot(Kelurahan_nb, coordinates(coordsW), col="red")
# Adding the kelurahan map underneath
plot(Kelurahan, add=T)

# Creating a spatial weights object from these weights
Kelurahan.lw <- nb2listw(Kelurahan_nb, style="C", zero.policy = TRUE)
head(Kelurahan.lw$neighbours)

## Kecamatan
# Calculating the centroids of all kelurahan in Jakarta
coordsKec<- coordinates(Kecamatan)
plot(coordsKec)

# Creating a neighbours list based on The Queen's Weight
Kecamatan_nb <- poly2nb(Kecamatan, queen=T, snap=0.1)
# Plotting the graph of neighbours 
plot(Kecamatan_nb, coordinates(coordsKec), col="red")
# Adding a map underneath
plot(Kecamatan, add=T)

# Creating a spatial weight object from these weights
Kecamatan.lw <- nb2listw(Kecamatan_nb, style="C")
head(Kecamatan.lw$neighbours)

# •••••••••••••••••••••••••
# •••• DENSITY BY AREA ••••
# •••••••••••••••••••••••••
### KELURAHAN
## GLOBAL SPATIAL AUTOCORRELATION
# Moran's I test 
I_Kelurahan_Global_Density_User_Area <- moran.test(Kelurahan$UsersDensityArea, Kelurahan.lw, zero.policy = TRUE)
I_Kelurahan_Global_Density_User_Area

# Geary's C 
C_Kelurahan_Global_Density_User_Area <- geary.test(Kelurahan$UsersDensityArea, Kelurahan.lw, zero.policy = TRUE)
C_Kelurahan_Global_Density_User_Area

# Getis Ord General G
G_Kelurahan_Global_Density_User_Area <- globalG.test(Kelurahan$UsersDensityArea, Kelurahan.lw, zero.policy = TRUE)
G_Kelurahan_Global_Density_User_Area

## LOCAL SPATIAL AUTOCORRELATION
# Local Moran's I
I_Kelurahan_Local_User <- localmoran(KelurahanSF$Total_User, Kelurahan.lw, zero.policy = TRUE)
I_Kelurahan_Local_Density_User_Area <- localmoran(Kelurahan$UsersDensityArea, Kelurahan.lw, zero.policy = TRUE)
# The first 10 of the result
head(I_Kelurahan_Local_Density_User_Area)

# Copying the Moran's I score and z-score of standard deviation
Kelurahan$UserAreaBLocI <- I_Kelurahan_Local_User[,1]
Kelurahan$UserAreaBLocIz <- I_Kelurahan_Local_User[,4]
Kelurahan$UserAreaBLocIR <- I_Kelurahan_Local_Density_User_Area[,1]
Kelurahan$UserAreaBLocIRz <- I_Kelurahan_Local_Density_User_Area[,4]

# Setting the breaks based on the rule that data points >2.58 or <-2.58 standard deviations away from the mean are significant at the 99% level (<1% chance that autocorrelation not present); >1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
# Creating a colour palette based on the results
MoranColours<- rev(brewer.pal(8, "RdGy"))

# Plotting the map of Moran's I
tm_shape(Kelurahan) +
  tm_polygons("UserAreaBLocIRz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Users-Area")

## Getis Ord G* statistics
Gi_Kelurahan_Local_Density_User_Area <- localG(Kelurahan$UsersDensityArea, Kelurahan.lw, zero.policy = TRUE)
# The results
head(Gi_Kelurahan_Local_Density_User_Area)

Kelurahan$UserAreaBLocGiRz <- Gi_Kelurahan_Local_Density_User_Area

GIColours<- rev(brewer.pal(8, "RdBu"))

# Plotting the map of Gi*
tm_shape(Kelurahan) +
  tm_polygons("UserAreaBLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi* Users-Area")

# •••••••••••••••••••••••••••••••
# •••• DENSITY BY POPULATION ••••
# •••••••••••••••••••••••••••••••

## GLOBAL SPATIAL AUTOCORRELATION
# Moran's I test 
I_Kelurahan_Global_Density_User_Pop <- moran.test(Kelurahan$UserRatio, Kelurahan.lw, zero.policy = TRUE)
I_Kelurahan_Global_Density_User_Pop

# Geary's C 
C_Kelurahan_Global_Density_User_Pop <- geary.test(Kelurahan$UserRatio, Kelurahan.lw, zero.policy = TRUE)
C_Kelurahan_Global_Density_User_Pop

# Getis Ord General G
G_Kelurahan_Global_Density_User_Pop <- globalG.test(Kelurahan$UserRatio, Kelurahan.lw, zero.policy = TRUE)
G_Kelurahan_Global_Density_User_Pop

## LOCAL SPATIAL AUTOCORRELATION
# Local Moran's I
I_Kelurahan_Local_User_Pop <- localmoran(KelurahanSF$Total_User, Kelurahan.lw, zero.policy = TRUE)
I_Kelurahan_Local_Density_User_Pop <- localmoran(Kelurahan$UserRatio, Kelurahan.lw, zero.policy = TRUE)
# The first 10 of the result
head(I_Kelurahan_Local_Density_User_Pop)

# Copying the Moran's I score and z-score of standard deviation
Kelurahan$UserPopBLocI <- I_Kelurahan_Local_User_Pop[,1]
Kelurahan$UserPopBLocIz <- I_Kelurahan_Local_User_Pop[,4]
Kelurahan$UserPopBLocIR <- I_Kelurahan_Local_Density_User_Pop[,1]
Kelurahan$UserPopBLocIRz <- I_Kelurahan_Local_Density_User_Pop[,4]

# Setting the breaks based on the rule that data points >2.58 or <-2.58 standard deviations away from the mean are significant at the 99% level (<1% chance that autocorrelation not present); >1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
# Creating a colour palette based on the results
MoranColours<- rev(brewer.pal(8, "RdGy"))

# Plotting the map of Moran's I
tm_shape(Kelurahan) +
  tm_polygons("UserPopBLocIRz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Users-Population")

## Getis Ord G* statistics
Gi_Kelurahan_Local_Density_User_Pop <- localG(Kelurahan$UserRatio, Kelurahan.lw, zero.policy = TRUE)
# The results
head(Gi_Kelurahan_Local_Density_User_Pop)

Kelurahan$UserPopBLocGiRz <- Gi_Kelurahan_Local_Density_User_Pop

GIColours<- rev(brewer.pal(8, "RdBu"))

# Plotting the map of Gi*
tm_shape(Kelurahan) +
  tm_polygons("UserPopBLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi* Users-Population")

## KECAMATAN
## GLOBAL SPATIAL AUTOCORRELATION
# Moran's I test 
I_Kecamatan_Global_Density_User_Area <- moran.test(Kecamatan$UsersDensityArea, Kecamatan.lw, zero.policy = TRUE)
I_Kecamatan_Global_Density_User_Area

# Geary's C 
C_Kecamatan_Global_Density_User_Area <- geary.test(Kecamatan$UsersDensityArea, Kecamatan.lw, zero.policy = TRUE)
C_Kecamatan_Global_Density_User_Area

# Getis Ord General G
G_Kecamatan_Global_Density_User_Area <- globalG.test(Kecamatan$UsersDensityArea, Kecamatan.lw, zero.policy = TRUE)
G_Kecamatan_Global_Density_User_Area

## LOCAL SPATIAL AUTOCORRELATION
# Local Moran's I
I_Kecamatan_Local_User <- localmoran(KecamatanSF$Total_User, Kecamatan.lw, zero.policy = TRUE)
I_Kecamatan_Local_Density_User_Area <- localmoran(Kecamatan$UsersDensityArea, Kecamatan.lw, zero.policy = TRUE)
# The first 10 of the result
head(I_Kecamatan_Local_Density_User_Area)

# Copying the Moran's I score and z-score of standard deviation
Kecamatan$UserAreaBLocI <- I_Kecamatan_Local_User[,1]
Kecamatan$UserAreaBLocIz <- I_Kecamatan_Local_User[,4]
Kecamatan$UserAreaBLocIR <- I_Kecamatan_Local_Density_User_Area[,1]
Kecamatan$UserAreaBLocIRz <- I_Kecamatan_Local_Density_User_Area[,4]

# Setting the breaks based on the rule that data points >2.58 or <-2.58 standard deviations away from the mean are significant at the 99% level (<1% chance that autocorrelation not present); >1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
# Creating a colour palette based on the results
MoranColours<- rev(brewer.pal(8, "RdGy"))

# Plotting the map of Moran's I
tm_shape(Kecamatan) +
  tm_polygons("UserAreaBLocIRz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Users-Area")

## Getis Ord G* statistics
Gi_Kecamatan_Local_Density_User_Area <- localG(Kecamatan$UsersDensityArea, Kecamatan.lw, zero.policy = TRUE)
# The results
head(Gi_Kecamatan_Local_Density_User_Area)

Kecamatan$UserAreaBLocGiRz <- Gi_Kecamatan_Local_Density_User_Area

GIColours<- rev(brewer.pal(8, "RdBu"))

# Plotting the map of Gi*
tm_shape(Kecamatan) +
  tm_polygons("UserAreaBLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi* Users-Area")

# •••••••••••••••••••••••••••••••
# •••• DENSITY BY POPULATION ••••
# •••••••••••••••••••••••••••••••

## GLOBAL SPATIAL AUTOCORRELATION
# Moran's I test 
I_Kecamatan_Global_Density_User_Pop <- moran.test(Kecamatan$UserRatio, Kecamatan.lw, zero.policy = TRUE)
I_Kecamatan_Global_Density_User_Pop

# Geary's C 
C_Kecamatan_Global_Density_User_Pop <- geary.test(Kecamatan$UserRatio, Kecamatan.lw, zero.policy = TRUE)
C_Kecamatan_Global_Density_User_Pop

# Getis Ord General G
G_Kecamatan_Global_Density_User_Pop <- globalG.test(Kecamatan$UserRatio, Kecamatan.lw, zero.policy = TRUE)
G_Kecamatan_Global_Density_User_Pop

## LOCAL SPATIAL AUTOCORRELATION
# Local Moran's I
I_Kecamatan_Local_User_Pop <- localmoran(KecamatanSF$Total_User, Kecamatan.lw, zero.policy = TRUE)
I_Kecamatan_Local_Density_User_Pop <- localmoran(Kecamatan$UserRatio, Kecamatan.lw, zero.policy = TRUE)
# The first 10 of the result
head(I_Kecamatan_Local_Density_User_Pop)

# Copying the Moran's I score and z-score of standard deviation
Kecamatan$UserPopBLocI <- I_Kecamatan_Local_User_Pop[,1]
Kecamatan$UserPopBLocIz <- I_Kecamatan_Local_User_Pop[,4]
Kecamatan$UserPopBLocIR <- I_Kecamatan_Local_Density_User_Pop[,1]
Kecamatan$UserPopBLocIRz <- I_Kecamatan_Local_Density_User_Pop[,4]

# Setting the breaks based on the rule that data points >2.58 or <-2.58 standard deviations away from the mean are significant at the 99% level (<1% chance that autocorrelation not present); >1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
# Creating a colour palette based on the results
MoranColours<- rev(brewer.pal(8, "RdGy"))

# Plotting the map of Moran's I
tm_shape(Kecamatan) +
  tm_polygons("UserPopBLocIRz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Users-Population")

## Getis Ord G* statistics
Gi_Kecamatan_Local_Density_User_Pop <- localG(Kecamatan$UserRatio, Kecamatan.lw, zero.policy = TRUE)
# The results
head(Gi_Kecamatan_Local_Density_User_Pop)

Kecamatan$UserPopBLocGiRz <- Gi_Kecamatan_Local_Density_User_Pop

GIColours<- rev(brewer.pal(8, "RdBu"))

# Plotting the map of Gi*
tm_shape(Kecamatan) +
  tm_polygons("UserPopBLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi* Users-Population")


# ••••••••••••••••••••••
# •••• USER DENSITY ••••
# ••••••••••••••••••••••
### KELURAHAN
## GLOBAL SPATIAL AUTOCORRELATION
# Moran's I test 
I_Kelurahan_Global_Density_User_Density <- moran.test(Kelurahan$UsersDensityArea, Kelurahan.lw, zero.policy = TRUE)
I_Kelurahan_Global_Density_User_Area

# Geary's C 
C_Kelurahan_Global_Density_User_Area <- geary.test(Kelurahan$UsersDensityArea, Kelurahan.lw, zero.policy = TRUE)
C_Kelurahan_Global_Density_User_Area

# Getis Ord General G
G_Kelurahan_Global_Density_User_Area <- globalG.test(Kelurahan$UsersDensityArea, Kelurahan.lw, zero.policy = TRUE)
G_Kelurahan_Global_Density_User_Area

## LOCAL SPATIAL AUTOCORRELATION
# Local Moran's I
I_Kelurahan_Local_User <- localmoran(KelurahanSF$Total_User, Kelurahan.lw, zero.policy = TRUE)
I_Kelurahan_Local_Density_User_Area <- localmoran(Kelurahan$UsersDensityArea, Kelurahan.lw, zero.policy = TRUE)
# The first 10 of the result
head(I_Kelurahan_Local_Density_User_Area)

# Copying the Moran's I score and z-score of standard deviation
Kelurahan$UserAreaBLocI <- I_Kelurahan_Local_User[,1]
Kelurahan$UserAreaBLocIz <- I_Kelurahan_Local_User[,4]
Kelurahan$UserAreaBLocIR <- I_Kelurahan_Local_Density_User_Area[,1]
Kelurahan$UserAreaBLocIRz <- I_Kelurahan_Local_Density_User_Area[,4]

# Setting the breaks based on the rule that data points >2.58 or <-2.58 standard deviations away from the mean are significant at the 99% level (<1% chance that autocorrelation not present); >1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
# Creating a colour palette based on the results
MoranColours<- rev(brewer.pal(8, "RdGy"))

# Plotting the map of Moran's I
tm_shape(Kelurahan) +
  tm_polygons("UserAreaBLocIRz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Users-Area")

## Getis Ord G* statistics
Gi_Kelurahan_Local_Density_User_Area <- localG(Kelurahan$UsersDensityArea, Kelurahan.lw, zero.policy = TRUE)
# The results
head(Gi_Kelurahan_Local_Density_User_Area)

Kelurahan$UserAreaBLocGiRz <- Gi_Kelurahan_Local_Density_User_Area

GIColours<- rev(brewer.pal(8, "RdBu"))

# Plotting the map of Gi*
tm_shape(Kelurahan) +
  tm_polygons("UserAreaBLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi* Users-Area")

#####################################
#### Temporal Pattern of Reports ####
#####################################
# Making a dataframe for the temporal analysis
ReportTotal <- data.frame(ReportsSub)
FloodTotal <- data.frame(Flood)
TrafficTotal <- data.frame(Traffic)
TransportTotal <- data.frame(Transport)
SmokingTotal <- data.frame(Smoking)
PFTotal <- data.frame(PF)
CrimeTotal <- data.frame(Crime)

# Arranging the order of the day
ReportTotal$Day <- factor(ReportTotal$Day,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


# Plotting a histogram based on the total daily frequency of reports 
FigTotMths <- ggplot(data=ReportTotal, aes(x=Date)) + 
  labs(x = "Day", y = "Count") + 
  geom_bar(stat="count") + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none" ) +
  ggtitle("Daily frequency of Qlue Reports") 
FigTotMths

# Plotting a histogram based on the total daily frequency of reports
FigTotDay <- ggplot(data=ReportTotal, aes(x=Day)) + 
  labs(x = "Day", y = "Count") + 
  geom_bar(stat="count", bins=7,  aes(fill = Day == "Friday")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none" ) +
  ggtitle("Daily frequency of Qlue Reports") 
FigTotDay

# Plotting a histogram based on the total hourly frequency of reports
FigTotal <- ggplot(data=ReportTotal, aes(x=HourCategory)) + 
  labs(x = "Hour", y = "Count") + 
  geom_bar(stat="count", bins=24, aes(fill = HourCategory == 2)) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none") +
  ggtitle("Hourly frequency of Qlue Reports") 
FigTotal


# Total reports per date
TotDate <- data.frame(count(ReportTotal, Date))
TotDate

# Average reports per day
TotDay <- count(ReportTotal, Day)

# Average reports per date
TotHour <- data.frame(count(ReportTotal,HourCategory))
TotHour[3]<- TotHour[2]/181 # Reflecting the number of days
TotHour2 <- TotHour[1:24,]

# Barplot for average daily frequency
FigDayAvg <- ggplot(data=TotHour2, aes(x = Day, y = n.1)) + 
  labs(x = "Hour", y = "Average Count") + 
  geom_bar(stat="identity", aes(fill = Day == "Friday")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + 
  theme(legend.position="none") +
  ggtitle("Average daily frequency of Qlue reports") 
FigDayAvg

# Barplot for average hourly frequency
FigHourAvg <- ggplot(data=TotHour2, aes(x=HourCategory, y=n.1)) + 
  labs(x = "Hour", y = "Count") + 
  geom_bar(stat="identity", aes(fill = HourCategory == 2)) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none") +
  ggtitle("Average hourly frequency of Qlue reports") 
FigHourAvg


# Subsetting report based on weekend - weekdays
ReportWeekend <- data.frame(ReportsSub[ReportsSub@data$Weekend == TRUE,])
ReportWeekday <- data.frame(ReportsSub[ReportsSub@data$Weekend == FALSE,])

# Plotting hourly frequency reports histogram on weekend - weekdays dataframe
FigWeekday <- ggplot(data=ReportWeekday, aes(x=HourCategory)) + 
  labs(x = "Hour", y = "Count") + 
  geom_bar(stat="count", bins=24, aes(fill = HourCategory == 2)) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none", axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  ggtitle("Hourly frequency of Qlue Reports on Weekdays") 
FigWeekday

FigWeekend <- ggplot(data=ReportWeekend, aes(x=HourCategory)) + 
  labs(x = "Hour", y = "Count") + 
  geom_bar(stat="count", bins=24, aes(fill = HourCategory == 10)) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none", axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  ggtitle("Hourly frequency of Qlue Reports on Weekend") 
FigWeekend

# Morning - Afternoon - Evening - Midnight dataframe
ReportMorning <- ReportsSub[!is.na(ReportsSub@data$TimeCategory) & ReportsSub@data$TimeCategory == "Morning" & ReportsSub@data$Day == "Saturday" ,]
ReportAfternoon <- ReportsSub[!is.na(ReportsSub@data$TimeCategory) & ReportsSub@data$TimeCategory == "Afternoon" & ReportsSub@data$Day == "Saturday",]
ReportEvening <- ReportsSub[!is.na(ReportsSub@data$TimeCategory) & ReportsSub@data$TimeCategory == "Evening" & ReportsSub@data$Day == "Saturday",]
ReportMidnight <- ReportsSub[!is.na(ReportsSub@data$TimeCategory) & ReportsSub@data$TimeCategory == "Midnight" & ReportsSub@data$Day == "Saturday",]
plot(ReportMorning)
plot(ReportAfternoon)
plot(ReportEvening)
plot(ReportMidnight)

# Assigning point count of report based on time category to each kelurahan
KelurahanSF$Morning <- poly.counts(ReportMorning,Kelurahan)
KelurahanSF$Afternoon <- poly.counts(ReportAfternoon,Kelurahan)
KelurahanSF$Evening <- poly.counts(ReportEvening,Kelurahan)
KelurahanSF$Midnight <- poly.counts(ReportMidnight,Kelurahan)

# Assigning point count of report based on time category to each kecamatan
KecamatanSF$Morning <- poly.counts(ReportMorning,Kecamatan)
KecamatanSF$Afternoon <- poly.counts(ReportAfternoon,Kecamatan)
KecamatanSF$Evening <- poly.counts(ReportEvening,Kecamatan)
KecamatanSF$Midnight <- poly.counts(ReportMidnight,Kecamatan)

# ----------------------------
# Topic-based temporal pattern
# ----------------------------
FloodTotal <- data.frame(Flood)
TrafficTotal <- data.frame(Traffic)
Transport <- data.frame(Transport)
Smoking <- data.frame(Smoking)
PFTotal <- data.frame(PF)
CrimeTotal <- data.frame(Crime)

# Total
FigFloodMths <- ggplot(data=FloodTotal, aes(x=Date)) + 
  labs(x = "Day", y = "Count") + 
  geom_bar(stat="count") + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none", axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  ggtitle("Daily frequency of Flood reports") 
FigFloodMths

FigTrafficMths <- ggplot(data=TrafficTotal, aes(x=Date)) + 
  labs(x = "Day", y = "Count") + 
  geom_bar(stat="count") + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none", axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  ggtitle("Daily frequency of Traffic reports") 
FigTrafficMths

FigTransportMths <- ggplot(data=TransportTotal, aes(x=Date)) + 
  labs(x = "Day", y = "Count") + 
  geom_bar(stat="count") + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none", axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  ggtitle("Daily frequency of Transport reports") 
FigTransportMths

FigSmokingMths <- ggplot(data=SmokingTotal, aes(x=Date)) + 
  labs(x = "Day", y = "Count") + 
  geom_bar(stat="count") + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none", axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  ggtitle("Daily frequency of Smoking reports") 
FigSmokingMths

FigPFMths <- ggplot(data=PFTotal, aes(x=Date)) + 
  labs(x = "Day", y = "Count") + 
  geom_bar(stat="count") + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none", axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  ggtitle("Daily frequency of Public facilities reports") 
FigPFMths

FigCrimeMths <- ggplot(data=CrimeTotal, aes(x=Date)) + 
  labs(x = "Day", y = "Count") + 
  geom_bar(stat="count") + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none", axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  ggtitle("Daily frequency of Crime reports") 
FigCrimeMths

############################
#### Index of Diversity ####
############################

# Preparing the dataframe
KelurahanGeom <- st_set_geometry(KelurahanSF,NULL)
KecamatanGeom <- st_set_geometry(KecamatanSF,NULL)

TypeSumKel <- colSums(KelurahanGeom[12:16])
TypeSumKec <- colSums(KecamatanGeom[21:24])

# ••••••••••••••••••••••••••••
# •••• Index of Diversity ••••
# ••••••••••••••••••••••••••••

# ---------
# Kelurahan
# ---------

# Diversity Index of reports based on type in kelurahan
KelurahanGeom[23] <- (KelurahanGeom[,12]/rowSums(KelurahanGeom[,12:16]))^2 
KelurahanGeom[24] <- (KelurahanGeom[,13]/rowSums(KelurahanGeom[,12:16]))^2 
KelurahanGeom[25] <- (KelurahanGeom[,14]/rowSums(KelurahanGeom[,12:16]))^2 
KelurahanGeom[26] <- (KelurahanGeom[,15]/rowSums(KelurahanGeom[,12:16]))^2 
KelurahanGeom[27] <- (KelurahanGeom[,16]/rowSums(KelurahanGeom[,12:16]))^2 


# Assigning Diversity Index back to spatial dataframe
Kelurahan$DivType <- 1-(rowSums(KelurahanGeom[,23:27]))

# Plotting choropleth map based on Diversity Index
tm_shape(Kelurahan) +
  tm_polygons("DivType",
              style="jenks",
              breaks=breaks1,
              palette="YlGnBu",
              midpoint=NA,
              title="Diversity Index - Report Type")

# Diversity Index of reports based on time in kelurahan
KelurahanGeom[28] <- (KelurahanGeom[,17]/rowSums(KelurahanGeom[,17:20]))^2 
KelurahanGeom[29] <- (KelurahanGeom[,18]/rowSums(KelurahanGeom[,17:20]))^2 
KelurahanGeom[30] <- (KelurahanGeom[,19]/rowSums(KelurahanGeom[,17:20]))^2 
KelurahanGeom[31] <- (KelurahanGeom[,20]/rowSums(KelurahanGeom[,17:20]))^2 

# Assigning Diversity Index back to spatial dataframe
Kelurahan$DivTime <- 1-(rowSums(KelurahanGeom[,28:31]))

# Plotting choropleth map based on Diversity Index
tm_shape(Kelurahan) +
  tm_polygons("DivTime",
              style="jenks",
              breaks=breaks1,
              palette="YlGnBu",
              midpoint=NA,
              title="Diversity Index - Report Time")

# ---------
# Kecamatan
# ---------

# Diversity Index of reports based on type in kecamatan
KecamatanGeom[27] <- (KecamatanGeom[,16]/rowSums(KecamatanGeom[,16:20]))^2 
KecamatanGeom[28] <- (KecamatanGeom[,17]/rowSums(KecamatanGeom[,16:20]))^2 
KecamatanGeom[29] <- (KecamatanGeom[,18]/rowSums(KecamatanGeom[,16:20]))^2 
KecamatanGeom[30] <- (KecamatanGeom[,19]/rowSums(KecamatanGeom[,16:20]))^2 
KecamatanGeom[31] <- (KecamatanGeom[,20]/rowSums(KecamatanGeom[,16:20]))^2 

KecamatanGeomSub <- KecamatanGeom[,16:20]

# Assigning Diversity Index back to spatial dataframe
Kecamatan$DivType <- 1-(rowSums(KecamatanGeom[,27:31]))

# Plotting choropleth map based on Diversity Index
tm_shape(Kecamatan) +
  tm_polygons("DivType",
              style="jenks",
              breaks=breaks1,
              palette="RdPu",
              midpoint=NA,
              title="Diversity Index - Report Type")

# Diversity Index of reports based on time in kecamatan
KecamatanGeom[32] <- (KecamatanGeom[,21]/rowSums(KecamatanGeom[,21:24]))^2 
KecamatanGeom[33] <- (KecamatanGeom[,22]/rowSums(KecamatanGeom[,21:24]))^2 
KecamatanGeom[34] <- (KecamatanGeom[,23]/rowSums(KecamatanGeom[,21:24]))^2 
KecamatanGeom[35] <- (KecamatanGeom[,24]/rowSums(KecamatanGeom[,21:24]))^2 


# Assigning Diversity Index back to spatial dataframe
Kecamatan$DivTime <- 1-(rowSums(KecamatanGeom[,32:35]))

# Plotting choropleth map based on report time
tm_shape(Kecamatan) +
  tm_polygons("DivTime",
              style="jenks",
              breaks=breaks1,
              palette="RdPu",
              midpoint=NA,
              title="Diversity Index - Report Time")

# •••••••••••••••••••••••
# •••• ENTROPY INDEX ••••
# •••••••••••••••••••••••

# ---------------------
# Kelurahan & Kecamatan
# ---------------------

# Subsetting dataset for analysis
KelurahanGeomType <- KelurahanGeom[,12:16]
KelurahanGeomTime <- KelurahanGeom[,17:20]
KecamatanGeomType <- KecamatanGeom[,16:20]
KecamatanGeomTime <- KecamatanGeom[,21:24]

# Shannon Index for Kelurahan
Kelurahan$ShannonType <- diversity(KelurahanGeomType, index="shannon", MARGIN = 1, base = exp(1))
Kelurahan$ShannonTime <- diversity(KelurahanGeomTime, index="shannon", MARGIN = 1, base = exp(1))

# Plotting choropleth map based on Shannon Index
# Report type
tm_shape(Kelurahan) +
  tm_polygons("ShannonType",
              style="jenks",
              breaks=breaks1,
              palette="YlGnBu",
              midpoint=NA,
              title="Shannon Index - Report Type")
# Report time
tm_shape(Kelurahan) +
  tm_polygons("ShannonTime",
              style="jenks",
              breaks=breaks1,
              palette="YlGnBu",
              midpoint=NA,
              title="Shannon Index - Report Time")

# Shannon Index for Kecamatan
Kecamatan$ShannonType <- diversity(KecamatanGeomType, index="shannon", MARGIN = 1, base = exp(1))
Kecamatan$ShannonTime <- diversity(KecamatanGeomTime, index="shannon", MARGIN = 1, base = exp(1))

# Plotting choropleth map based on Shannon Index
# Report type
tm_shape(Kecamatan) +
  tm_polygons("ShannonType",
              style="jenks",
              breaks=breaks1,
              palette="RdPu",
              midpoint=NA,
              title="Shannon Index - Report Type")
# Report time
tm_shape(Kecamatan) +
  tm_polygons("ShannonTime",
              style="jenks",
              breaks=breaks1,
              palette="RdPu",
              midpoint=NA,
              title="Shannon Index - Report Time")

# --------
# User Age
# --------

Kelurahan$ShannonUserAge <- diversity(Kelurahan_Age, index="shannon", MARGIN = 1, base = exp(1))
Kecamatan$ShannonUserAge <- diversity(Kecamatan_Age, index="shannon", MARGIN = 1, base = exp(1))


# Plotting choropleth map based on Shannon Index
# Kelurahan
tm_shape(Kelurahan) +
  tm_polygons("ShannonUserAge",
              style="jenks",
              breaks=breaks1,
              palette="YlGnBu",
              midpoint=NA,
              title="Shannon Index - User Age")

# Kecamatan
tm_shape(Kecamatan) +
  tm_polygons("ShannonUserAge",
              style="jenks",
              breaks=breaks1,
              palette="RdPu",
              midpoint=NA,
              title="Shannon Index - User Age")


#######################################
#### Geodemographics of Qlue Users ####
#######################################

# Preparing the library needed
library(tidyverse)
library(downloader)
library(rgdal)
library(sf)
library(ggplot2)
library(reshape2)
library(plotly)
library(highcharter)

# Checking the numeric data
list1 <- as.data.frame(cbind(lapply(KelurahanSF, class)))
list1
list1 <- cbind(list1, seq.int(nrow(list1)))

# Excluding non-numeric columns 
KelurahanSub1 <- KelurahanSF[,c(1:12)]

# Dropping geometry
KelurahanSub1 <- st_set_geometry(KelurahanSub1,NULL)

# Winsorize the outliers
KelurahanSub1$PctUnemplo <- Winsorize(KelurahanSub1$PctUnemplo,maxval = 63.78)

# Transforming the total user counts into log
KelurahanSub1$Total_User_log <- log(KelurahanSub1$Total_User)

# Seeing the correlation among variables
cormat <- cor(KelurahanSub1[,4:11], use="complete.obs", method="pearson")
corrplot(cormat)

# Visualising the variables based on distribution
KelurahanMelt <- melt(KelurahanSub1, id.vars = 1:3)
attach(KelurahanMelt)

hist <- ggplot(KelurahanMelt, aes(x=value)) + geom_histogram(aes(y = ..density..)) + geom_density(colour="red", size=1, adjust=1)
hist + facet_wrap(~ variable, scales="free")

## %UNEMPLOYMENT VS TOTAL USER
# Creating a new data frame
Cluster1 <- as.data.frame(KelurahanSub1[,c("Total_User_log","PctUnemplo")])
attach(Cluster1)

# Checking variable distributions 
histplot <- ggplot(data=Cluster1, aes(x=PctUnemplo))
histplot + geom_histogram() 

histplot <- ggplot(data=Cluster1, aes(x= Total_User_log))
histplot + geom_histogram()

# Running a K-means analysis to find 3 clusters – using 25 iterations
fit <- kmeans(Cluster1, 3, nstart=100) 

# Getting cluster means
centroid <- aggregate(Cluster1,by=list(fit$cluster),FUN=mean)

# Getting the results of the cluster groupings
centroid

# Plotting the variables based on clusters
p <- ggplot(Cluster1,aes(Total_User_log, PctUnemplo))
p + geom_point(aes(colour=factor(fit$cluster)))+geom_point(data=centroid[,1:3],aes(Total_User_log, PctUnemplo), size=7, shape=18)+ theme(legend.position="none")

Cluster1$cluster <- fit$cluster

# Assigning kelurahan to related cluster
KelurahanSF$cluster<-Cluster1$cluster

# Mapping the geodeomographic classification
map <- ggplot(KelurahanSF) + geom_sf(mapping = aes(fill=cluster))+scale_fill_continuous(breaks=c(1,2,3))
map

####################################
#### Topic-based report pattern ####
####################################

#---------------------------
#---- Data Preparation -----
#---------------------------

## Subsetting the report data based on topic
PF

## Subsetting dataframes based on weekend/weekdays
# Morning - Afternoon - Evening - Midnight dataframe
PFMorning <- PF[!is.na(PF$TimeCategory) & PF$TimeCategory == "Morning",]
PFAfternoon <- PF[!is.na(PF@data$TimeCategory) & PF@data$TimeCategory == "Afternoon",]
PFEvening <- PF[!is.na(PF@data$TimeCategory) & PF@data$TimeCategory == "Evening",]
PFMidnight <- PF[!is.na(PF@data$TimeCategory) & PF@data$TimeCategory == "Midnight",]
plot(PFMorning)
plot(PFAfternoon)
plot(PFEvening)
plot(PFMidnight)

# Weekend - weekdays dataframe
PFWeekdays <- PF[PF@data$Weekend == FALSE,]
PFWeekend <- PF[PF@data$Weekend == TRUE,]

# Weekend - weekdays & time category dataframe
PFWeekdays_Morning <- PFWeekdays[!is.na(PFWeekdays@data$TimeCategory) & PFWeekdays@data$TimeCategory == "Morning",]
PFWeekdays_Afternoon <- PFWeekdays[!is.na(PFWeekdays@data$TimeCategory) & PFWeekdays@data$TimeCategory == "Afternoon",]
PFWeekdays_Evening <- PFWeekdays[!is.na(PFWeekdays@data$TimeCategory) & PFWeekdays@data$TimeCategory == "Evening",]
PFWeekdays_Midnight <- PFWeekdays[!is.na(PFWeekdays@data$TimeCategory) & PFWeekdays@data$TimeCategory == "Midnight",]

PFWeekend_Morning <- PFWeekend[!is.na(PFWeekend@data$TimeCategory) & PFWeekend@data$TimeCategory == "Morning",]
PFWeekend_Afternoon <- PFWeekend[!is.na(PFWeekend@data$TimeCategory) & PFWeekend@data$TimeCategory == "Afternoon",]
PFWeekend_Evening <- PFWeekend[!is.na(PFWeekend@data$TimeCategory) & PFWeekend@data$TimeCategory == "Evening",]
PFWeekend_Midnight <- PFWeekend[!is.na(PFWeekend@data$TimeCategory) & PFWeekend@data$TimeCategory == "Midnight",]

## Assigning point count of PF report based on time category to each kelurahan
KelurahanSF$PFMorning <- poly.counts(PFMorning,Kelurahan)
KelurahanSF$PFAfternoon <- poly.counts(PFAfternoon,Kelurahan)
KelurahanSF$PFEvening <- poly.counts(PFEvening,Kelurahan)
KelurahanSF$PFMidnight <- poly.counts(PFMidnight,Kelurahan)

KelurahanSF$PF_WD_Morning <- poly.counts(PFWeekdays_Morning,Kelurahan)
KelurahanSF$PF_WD_Afternoon <- poly.counts(PFWeekdays_Afternoon,Kelurahan)
KelurahanSF$PF_WD_Evening <- poly.counts(PFWeekdays_Evening,Kelurahan)
KelurahanSF$PF_WD_Midnight <- poly.counts(PFWeekdays_Midnight,Kelurahan)

KelurahanSF$PF_WE_Morning <- poly.counts(PFWeekend_Morning,Kelurahan)
KelurahanSF$PF_WE_Afternoon <- poly.counts(PFWeekend_Afternoon,Kelurahan)
KelurahanSF$PF_WE_Evening <- poly.counts(PFWeekend_Evening,Kelurahan)
KelurahanSF$PF_WE_Midnight <- poly.counts(PFWeekend_Midnight,Kelurahan)

## Assigning point count of PF report based on time category to each kecamatan
KecamatanSF$PFMorning <- poly.counts(PFMorning,Kecamatan)
KecamatanSF$PFAfternoon <- poly.counts(PFAfternoon,Kecamatan)
KecamatanSF$PFEvening <- poly.counts(PFEvening,Kecamatan)
KecamatanSF$PFMidnight <- poly.counts(PFMidnight,Kecamatan)

KecamatanSF$PF_WD_Morning <- poly.counts(PFWeekdays_Morning,Kecamatan)
KecamatanSF$PF_WD_Afternoon <- poly.counts(PFWeekdays_Afternoon,Kecamatan)
KecamatanSF$PF_WD_Evening <- poly.counts(PFWeekdays_Evening,Kecamatan)
KecamatanSF$PF_WD_Midnight <- poly.counts(PFWeekdays_Midnight,Kecamatan)

KecamatanSF$PF_WE_Morning <- poly.counts(PFWeekend_Morning,Kecamatan)
KecamatanSF$PF_WE_Afternoon <- poly.counts(PFWeekend_Afternoon,Kecamatan)
KecamatanSF$PF_WE_Evening <- poly.counts(PFWeekend_Evening,Kecamatan)
KecamatanSF$PF_WE_Midnight <- poly.counts(PFWeekend_Midnight,Kecamatan)

Kelurahan$PFDensityArea <- KelurahanSF$PF/poly.areas(Kelurahan)
Kelurahan$PFDensityPop <- KelurahanSF$PF/KelurahanSF$Pop
Kelurahan$PFDensityUser <- KelurahanSF$PF/KelurahanSF$Total_User
Kecamatan$PFDensityArea <- KecamatanSF$PF/poly.areas(Kecamatan)
Kecamatan$PFDensityUser <- KecamatanSF$PF/KecamatanSF$Total_User

#---------------------------
#---- Temporal Pattern -----
#---------------------------

# Making a dataframe for the temporal analysis
PF1 <- data.frame(PF)
PF1

# Arranging the order of the day
PF1$Day <- factor(PF1$Day,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plotting a histogram based on the daily frequency of reports
FigDay1 <- ggplot(data=PF1, aes(x=Day)) + 
  labs(x = "Day", y = "Count") + 
  geom_bar(stat="count", bins=7, aes(fill = Day == "Friday")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none" ) +
  ggtitle("Daily frequency of PF Reports") 
FigDay1

# Plotting a histogram based on the hourly frequency of reports
FigTotalPF <- ggplot(data=PF1, aes(x=HourCategory)) + 
  labs(x = "Hour", y = "Count") + 
  geom_bar(stat="count", bins=24, aes(fill = HourCategory == 2)) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none") +
  ggtitle("Hourly frequency of PF Reports") 
FigTotalPF

# Subsetting report based on weekend - weekdays
PFWeekend <- data.frame(PF[PF@data$Weekend == TRUE,])
PFWeekday <- data.frame(PF[PF@data$Weekend == FALSE,])

# Plotting hourly frequency reports histogram on weekend - weekdays dataframe
FigPFWeekday <- ggplot(data=PFWeekday, aes(x=HourCategory)) + 
  labs(x = "Hour", y = "Count") + 
  geom_bar(stat="count", bins=24, aes(fill = HourCategory == 2)) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none", axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  ggtitle("Hourly frequency of PF Reports on Weekdays") 
FigPFWeekday

FigPFWeekend <- ggplot(data=PFWeekend, aes(x=HourCategory)) + 
  labs(x = "Hour", y = "Count") + 
  geom_bar(stat="count", bins=24, aes(fill = HourCategory == 10)) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07") ) + 
  theme(legend.position="none", axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  ggtitle("Hourly frequency of PF Reports on Weekend") 
FigPFWeekend

## Calculating density of reports per user
# Kelurahan
Kelurahan$ReportsDensityUsers_WD_Morning <- KelurahanSF$PF_WD_Morning/KelurahanSF$Total_User
Kelurahan$ReportsDensityUsers_WD_Afternoon <- KelurahanSF$PF_WD_Afternoon/KelurahanSF$Total_User
Kelurahan$ReportsDensityUsers_WD_Evening <- KelurahanSF$PF_WD_Evening/KelurahanSF$Total_User
Kelurahan$ReportsDensityUsers_WD_Midnight <- KelurahanSF$PF_WD_Midnight/KelurahanSF$Total_User

Kelurahan$ReportsDensityUsers_WE_Morning <- KelurahanSF$PF_WE_Morning/KelurahanSF$Total_User
Kelurahan$ReportsDensityUsers_WE_Afternoon <- KelurahanSF$PF_WE_Afternoon/KelurahanSF$Total_User
Kelurahan$ReportsDensityUsers_WE_Evening <- KelurahanSF$PF_WE_Evening/KelurahanSF$Total_User
Kelurahan$ReportsDensityUsers_WE_Midnight <- KelurahanSF$PF_WE_Midnight/KelurahanSF$Total_User

## Visualising the density of reports based on the users
tm_shape(Kelurahan) +
  tm_polygons("ReportsDensityUsers_WD_Midnight",
              style="jenks",
              palette="Blues",
              midpoint=NA,
              title="Reports Density - Users")


#----------------------------------
#---- Spatial Autocorrelation -----
#----------------------------------

## Making the spatial weights matrix
# Calculating the centroids of all kelurahan in Jakarta
coordsW <- coordinates(Kelurahan)
plot(coordsW)
# Creating a neighbours list based on The Queen's Weight
Kelurahan_nb <- poly2nb(Kelurahan, queen=T, snap=0.1) 


# Plotting them
plot(Kelurahan_nb, coordinates(coordsW), col="red")
# Adding the kelurahan map underneath
plot(Kelurahan, add=T)

# Creating a spatial weights object from these weights
Kelurahan.lw <- nb2listw(Kelurahan_nb, style="C", zero.policy = TRUE)
head(Kelurahan.lw$neighbours)

# •••••••••••••••••••••••••
# •••• DENSITY BY USER ••••
# •••••••••••••••••••••••••

## GLOBAL SPATIAL AUTOCORRELATION
# Moran's I test 
I_Kelurahan_Global_Density_PF_WD_Morning <- moran.test(Kelurahan$ReportsDensityUsers_WD_Afternoon, Kelurahan.lw, zero.policy = TRUE)
I_Kelurahan_Global_Density_PF_WD_Morning

# Geary's C 
C_Kelurahan_Global_Density_PF_WD_Morning <- geary.test(Kelurahan$ReportsDensityUsers_WD_Afternoon, Kelurahan.lw, zero.policy = TRUE)
C_Kelurahan_Global_Density_PF_WD_Morning

# Getis Ord General G
G_Kelurahan_Global_Density_PF_WD_Morning <- globalG.test(Kelurahan$ReportsDensityUsers_WD_Afternoon, Kelurahan.lw, zero.policy = TRUE)
G_Kelurahan_Global_Density_PF_WD_Morning

## LOCAL SPATIAL AUTOCORRELATION
# Local Moran's I
I_Kelurahan_Local <- localmoran(Kelurahan$ReportsCount, Kelurahan.lw, zero.policy = TRUE)
I_Kelurahan_Local_Density <- localmoran(Kelurahan$ReportsDensityUsers_WE_Evening, Kelurahan.lw, zero.policy = TRUE)
# The first 10 of the result
head(I_Kelurahan_Local_Density)

# Copying the Moran's I score and z-score of standard deviation
Kelurahan$BLocI <- I_Kelurahan_Local[,1]
Kelurahan$BLocIz <- I_Kelurahan_Local[,4]
Kelurahan$BLocIR <- I_Kelurahan_Local_Density[,1]
Kelurahan$BLocIRz <- I_Kelurahan_Local_Density[,4]

# Setting the breaks based on the rule that data points >2.58 or <-2.58 standard deviations away from the mean are significant at the 99% level (<1% chance that autocorrelation not present); >1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
# Creating a colour palette based on the results
MoranColours<- rev(brewer.pal(8, "RdGy"))

# Plotting the map of Moran's I
tm_shape(Kelurahan) +
  tm_polygons("BLocIRz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Public Facilities Report - Weekend Evening")

## Getis Ord G* statistics
Gi_Kelurahan_Local_Density <- localG(Kelurahan$ReportsDensityUsers_WE_Evening, Kelurahan.lw, zero.policy = TRUE)
# The results
head(Gi_Kelurahan_Local_Density)

Kelurahan$BLocGiRz <- Gi_Kelurahan_Local_Density

GIColours<- rev(brewer.pal(8, "RdBu"))

# Plotting the map of Gi*
tm_shape(Kelurahan) +
  tm_polygons("BLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi* Public Facilities Report - Weekend Evening")

#---------------------------------
#---- Point Pattern Analysis -----
#---------------------------------

# Loading the libraries
library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojsonio)
library(tmaptools)

# Subsetting sample of kelurahan
KelSlipi <- Kelurahan[KelurahanSF$KEL_NAME== "SLIPI",]

# Clipping the reports that fall within kelurahan sample
ReportSlipi <- ReportsSub[KelSlipi,]
ReportSlipi

# Subsetting the reports based on the preferred topic
PFSlipi <- ReportSlipi[ReportSlipi@data$Category == "Fasilitas Anak" | 
                         ReportSlipi@data$Category == "Fasilitas Umum" |
                         ReportSlipi@data$Category == "RPTRA" |
                         ReportSlipi@data$Category == "Iklan Liar" |
                         ReportSlipi@data$Category == "Tanaman Bermasalah" |
                         ReportSlipi@data$Category == "Pohon Tumbang" |
                         ReportSlipi@data$Category == "Pelanggaran" |
                         ReportSlipi@data$Category == "Pelanggaran Bangunan" |
                         ReportSlipi@data$Category == "Pelanggaran IMB" |
                         ReportSlipi@data$Category == "Pelanggaran Ketertiban Umum" |
                         ReportSlipi@data$Category == "Pilkada" |
                         ReportSlipi@data$Category == "Rumah Tangga" |
                         ReportSlipi@data$Category == "Sampah" |
                         ReportSlipi@data$Category == "Pelayana Pemerintah",]

# Checking to see that the correct borough has been pulled out
tm_shape(KelSlipi) +
  tm_polygons(col = NA, alpha = 0.5) + 
  tm_shape(PFSlipi) +
  tm_dots(col = "navy", size=0.1)

# Subsetting the data based on weekday - weekend
PFWeekdaySlipi <- PFSlipi[PFSlipi$Weekend == FALSE,]
PFWeekendSlipi <- PFSlipi[PFSlipi$Weekend == TRUE,]

# Subsetting the data based on time category
# Weekday
PFWeekdaySlipi_Morning <- PFWeekdaySlipi[PFWeekdaySlipi$TimeCategory=="Morning",]
PFWeekdaySlipi_Afternoon <- PFWeekdaySlipi[PFWeekdaySlipi$TimeCategory=="Afternoon",]
PFWeekdaySlipi_Evening <- PFWeekdaySlipi[PFWeekdaySlipi$TimeCategory=="Evening",]
PFWeekdaySlipi_Midnight <- PFWeekdaySlipi[PFWeekdaySlipi$TimeCategory=="Midnight",]

# Weekend
PFWeekendSlipi_Morning <- PFWeekendSlipi[PFWeekendSlipi$TimeCategory=="Morning",]
PFWeekendSlipi_Afternoon <- PFWeekendSlipi[PFWeekendSlipi$TimeCategory=="Afternoon",]
PFWeekendSlipi_Evening <- PFWeekendSlipi[PFWeekendSlipi$TimeCategory=="Evening",]
PFWeekendSlipi_Midnight <- PFWeekendSlipi[PFWeekendSlipi$TimeCategory=="Midnight",]

## Performing the Ripley's K and DBSCAN
# Setting a window as the borough boundary
window <- as.owin(KelSlipi)
plot(window)

# Making ppp objects
PFSlipiWeekday.ppp <- ppp(x=PFWeekdaySlipi@coords[,1],y=PFWeekdaySlipi@coords[,2],window=window)
PFSlipiWeekdayMorning.ppp <- ppp(x=PFWeekdaySlipi_Morning@coords[,1],y=PFWeekdaySlipi_Morning@coords[,2],window=window)
PFSlipiWeekdayAfternoon.ppp <- ppp(x=PFWeekdaySlipi_Afternoon@coords[,1],y=PFWeekdaySlipi_Afternoon@coords[,2],window=window)
PFSlipiWeekdayEvening.ppp <- ppp(x=PFWeekdaySlipi_Evening@coords[,1],y=PFWeekdaySlipi_Evening@coords[,2],window=window)
PFSlipiWeekdayMidnight.ppp <- ppp(x=PFWeekdaySlipi_Midnight@coords[,1],y=PFWeekdaySlipi_Midnight@coords[,2],window=window)

PFSlipiWeekend.ppp <- ppp(x=PFWeekendSlipi@coords[,1],y=PFWeekendSlipi@coords[,2],window=window)
PFSlipiWeekendMorning.ppp <- ppp(x=PFWeekendSlipi_Morning@coords[,1],y=PFWeekendSlipi_Morning@coords[,2],window=window)
PFSlipiWeekendAfternoon.ppp <- ppp(x=PFWeekendSlipi_Afternoon@coords[,1],y=PFWeekendSlipi_Afternoon@coords[,2],window=window)
PFSlipiWeekendEvening.ppp <- ppp(x=PFWeekendSlipi_Evening@coords[,1],y=PFWeekendSlipi_Evening@coords[,2],window=window)
PFSlipiWeekendMidnight.ppp <- ppp(x=PFWeekendSlipi_Midnight@coords[,1],y=PFWeekendSlipi_Midnight@coords[,2],window=window)

# Performing Ripley's K analysis
K_Weekday <- Kest(PFSlipiWeekday.ppp, correction="border")
K_Weekday_Morning <- Kest(PFSlipiWeekdayMorning.ppp, correction="border")
K_Weekday_Afternoon <- Kest(PFSlipiWeekdayAfternoon.ppp, correction="border")
K_Weekday_Evening <- Kest(PFSlipiWeekdayEvening.ppp, correction="border")
K_Weekday_Midnight <- Kest(PFSlipiWeekdayMidnight.ppp, correction="border")
K_Weekend <- Kest(PFSlipiWeekend.ppp, correction="border")
K_Weekend_Morning <- Kest(PFSlipiWeekendMorning.ppp, correction="border")
K_Weekend_Afternoon <- Kest(PFSlipiWeekendAfternoon.ppp, correction="border")
K_Weekend_Evening <- Kest(PFSlipiWeekendEvening.ppp, correction="border")
K_Weekend_Midnight <- Kest(PFSlipiWeekendMidnight.ppp, correction="border")

# Plotting the result of Ripley's K
plot(K_Weekday)
plot(K_Weekday_Morning) #
plot(K_Weekday_Afternoon)
plot(K_Weekday_Evening)
plot(K_Weekday_Midnight)
plot(K_Weekend)
plot(K_Weekend_Morning) #
plot(K_Weekend_Afternoon)
plot(K_Weekend_Evening)
plot(K_Weekend_Midnight)

# Heatmaps
plot(density(PFSlipiWeekend.ppp, sigma = 50))
plot(density(PFSlipiWeekday.ppp, sigma = 50))

# Extracting the points from the spatial points data frame
SlipiWeekdayMorning <- data.frame(PFWeekdaySlipi_Morning@coords[,1:2])
SlipiWeekendMorning <- data.frame(PFWeekendSlipi_Morning@coords[,1:2])

# Running the DBSCAN
db_weekday_morning <- fpc::dbscan(SlipiWeekdayMorning, eps = 175, MinPts = 4) #
db_weekend_morning <- fpc::dbscan(SlipiWeekendMorning, eps = 25, MinPts = 4)

# Plotting the results
plot(db_weekday_morning, SlipiWeekdayMorning, main = "DBSCAN Output of PF reports on Weekday Morning", frame = F)
plot(KelSlipi, add=T)

plot(db_weekend_morning, SlipiWeekendMorning, main = "DBSCAN Output of Weekend Morning", frame = F)
plot(KelSlipi, add=T)

# Adding the cluster into the dataframe
SlipiWeekdayMorning$cluster <- db_weekday_morning$cluster

# Making convex hull polygons to wrap around the points in clusters

# Getting the convex hull coordinates from the cluster groups in the dataframe
chulls <- ddply(SlipiWeekdayMorning, .(cluster), function(df) df[chull(df$coords.x1, df$coords.x2), ])
# Dropping 0 as it is not a cluster from the dataframe
chulls <- subset(chulls, cluster>=1)

# Creating a ggplot2 object from  data
dbplot <- ggplot(data=SlipiWeekdayMorning, aes(coords.x1,coords.x2, colour=cluster, fill=cluster)) 
# Adding the points in
dbplot <- dbplot + geom_point()
# Adding the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, aes(coords.x1,coords.x2, group=cluster), alpha = 0.5) 
# Plotting the result in black and white and setting the coordinates to scale correctly
dbplot + theme_bw() + coord_equal()

## Adding a basemap
# Getting the bbox in lat long
WGS <- "+init=epsg:4326"
KelSlipiWGS <-spTransform(KelSlipi, WGS)
KelSlipiWGS@bbox

# Creating a basemap
basemap<-openmap(c(-6.201415,106.797020),c(-6.189014,106.806730), zoom=NULL,"osm")

# Converting the basemap to WGS
basemap_Slipi<-openproj(basemap, projection=WGS)
basemap_Slipi

## Plotting the map
autoplot(basemap_Slipi) + geom_point(data=SlipiWeekdayMorning, aes(coords.x1,coords.x2, colour=cluster, fill=cluster)) + geom_polygon(data = chulls, aes(coords.x1,coords.x2, group=cluster, fill=cluster), alpha = 0.5)  


#######################################
#### Structural Equation Modelling ####
#######################################

# ••••••••••••••••••••••••••••
# Loading the libraries needed
# ••••••••••••••••••••••••••••

library(lavaan)
library(semPlot)
library(OpenMx)
library(ggpubr)
library(tidyverse)
library(knitr)
library(standardize)
library(GGally)
library(kableExtra)
library(ggplot2)
library(reshape2)
library(MVN) # Functions for checking multivariate and univariate normality
library(BBmisc)
library(DescTools) # Functions for winsorizing the variables

# ••••••••••••••••••
# Preparing the data
# ••••••••••••••••••
# ----------------
# Reading the data
# ----------------
data_kelurahan <- read.csv("Kelurahan socio-economic data2.csv")
data_kecamatan <- read.csv("Kecamatan socio-economic data.csv")

# ------------------------------------------------------
# Adding the report-related variables into the dataframe
# ------------------------------------------------------
# Kelurahan variables
data_kelurahan$pct_maleuser <- data_kelurahan$Male_User/data_kelurahan$Male_Pop
data_kelurahan$pct_femaleuser <- data_kelurahan$Female_User/data_kelurahan$Female_Pop
data_kelurahan$reportdensityarea <- Kelurahan$ReportsDensityArea
data_kelurahan$reportdensitypop <- Kelurahan$ReportsDensityPop
data_kelurahan$reportdensityuser <- Kelurahan$ReportsDensityUsers
data_kelurahan$entropy_type <- Kelurahan$ShannonType
data_kelurahan$entropy_time <- Kelurahan$ShannonTime
data_kelurahan$entropy_age <- Kelurahan$ShannonUserAge
data_kelurahan$ReportsCount <- Kelurahan$ReportsCount

# Kecamatan variables
data_kecamatan$pct_maleuser <- data_kecamatan$Male_user/data_kecamatan$Male_Pop
data_kecamatan$pct_femaleuser <- data_kecamatan$Female_user/data_kecamatan$Female_Pop
data_kecamatan$reportdensityarea <- Kecamatan$ReportsDensityArea
data_kecamatan$reportdensitypop <- Kecamatan$ReportsDensityPopulation
data_kecamatan$reportdensityuser <- Kecamatan$ReportsDensityUsers
data_kecamatan$entropy_type <- Kecamatan$ShannonType
data_kecamatan$entropy_time <- Kecamatan$ShannonTime
data_kecamatan$entropy_age <- Kecamatan$ShannonUserAge
data_kecamatan$ReportsCount <- Kecamatan$ReportsCount

# Dropping the ID column
data_kelurahansub <- data_kelurahan[,2:20]
data_kecamatansub <- data_kecamatan[,5:26]

# Making a faceted boxplot
df.kel <- melt(data_kelurahansub)
a <- ggplot(data = df.kel, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=value))
a <- a + facet_wrap( ~ variable, scales="free")
a <- a + xlab("Kelurahan variables") + ylab("Value") + ggtitle("Boxplots for kelurahan attributes")
a

df.kec <- melt(data_kecamatansub)
b <- ggplot(data = df.kec, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=value))
b <- b + facet_wrap( ~ variable, scales="free")
b <- b + xlab("Kecamatan variables") + ylab("Value") + ggtitle("Boxplots for kecamatan attributes")
b

# -----------------------------------
# Checking for multivariate normality
# -----------------------------------
kel.result <- mvn(data_kelurahansub, mvnTest = "mardia") 
kel.result$multivariateNormality

kec.result <- mvn(data_kecamatansub, mvnTest = "mardia") 
kec.result$multivariateNormality

# ----------------------------------------------------------
# Winsorizing the variables to assume multivariate normality
# ----------------------------------------------------------
# Kelurahan variables
data_kelurahansub$Male_Pop <- Winsorize(data_kelurahansub$Male_Pop)
data_kelurahansub$Female_Pop <- Winsorize(data_kelurahansub$Female_Pop)
data_kelurahansub$Pop <- Winsorize(data_kelurahansub$Pop)
data_kelurahansub$Unemployed_Pct <- Winsorize(data_kelurahansub$Unemployed_Pct)
data_kelurahansub$Mean_Age_Active_Users <- Winsorize(data_kelurahansub$Mean_Age_Active_Users)
data_kelurahansub$Female_User <- Winsorize(data_kelurahansub$Female_User)
data_kelurahansub$Male_User <- Winsorize(data_kelurahansub$Male_User)
data_kelurahansub$Total_User <- Winsorize(data_kelurahansub$Total_User)
data_kelurahansub$User_Ratio <- Winsorize(data_kelurahansub$User_Ratio)
data_kelurahansub$pct_maleuser <- Winsorize(data_kelurahansub$pct_maleuser)
data_kelurahansub$pct_femaleuser <- Winsorize(data_kelurahansub$pct_femaleuser)
data_kelurahansub$reportdensityarea <- Winsorize(data_kelurahansub$reportdensityarea)
data_kelurahansub$reportdensitypop <- Winsorize(data_kelurahansub$reportdensitypop)
data_kelurahansub$reportdensityuser <- Winsorize(data_kelurahansub$reportdensityuser)
data_kelurahansub$entropy_type <- Winsorize(data_kelurahansub$entropy_type)
data_kelurahansub$entropy_time <- Winsorize(data_kelurahansub$entropy_time)
data_kelurahansub$entropy_age <- Winsorize(data_kelurahansub$entropy_age)
data_kelurahansub$ReportsCount <- Winsorize(data_kelurahansub$ReportsCount)

# Kecamatan variables
data_kecamatansub$Male_Pop <- Winsorize(data_kecamatansub$Male_Pop)
data_kecamatansub$Female_Pop <- Winsorize(data_kecamatansub$Female_Pop)
data_kecamatansub$Pop <- Winsorize(data_kecamatansub$Pop)
data_kecamatansub$Pct_Poor_Household <- Winsorize(data_kecamatansub$Pct_Poor_Household)
data_kecamatansub$Mean_Age_active_user <- Winsorize(data_kecamatansub$Mean_Age_active_user)
data_kecamatansub$Female_user <- Winsorize(data_kecamatansub$Female_user)
data_kecamatansub$Male_user <- Winsorize(data_kecamatansub$Male_user)
data_kecamatansub$Total_User <- Winsorize(data_kecamatansub$Total_User)
data_kecamatansub$Ratio_of_user <- Winsorize(data_kecamatansub$Ratio_of_user)
data_kecamatansub$pct_maleuser <- Winsorize(data_kecamatansub$pct_maleuser)
data_kecamatansub$pct_femaleuser <- Winsorize(data_kecamatansub$pct_femaleuser)
data_kecamatansub$reportdensityarea <- Winsorize(data_kecamatansub$reportdensityarea)
data_kecamatansub$reportdensitypop <- Winsorize(data_kecamatansub$reportdensitypop)
data_kecamatansub$reportdensityuser <- Winsorize(data_kecamatansub$reportdensityuser)
data_kecamatansub$entropy_type <- Winsorize(data_kecamatansub$entropy_type)
data_kecamatansub$entropy_time <- Winsorize(data_kecamatansub$entropy_time)
data_kecamatansub$entropy_age <- Winsorize(data_kecamatansub$entropy_age)
data_kecamatansub$ReportsCount <- Winsorize(data_kecamatansub$ReportsCount)

# ---------------------------------
# Checking for univariate normality
# ---------------------------------
# Kelurahan variables
mvn(data_kelurahansub$Male_Pop)
mvn(data_kelurahansub$Female_Pop)
mvn(data_kelurahansub$Pop)
mvn(data_kelurahansub$Unemployed_Pct)
mvn(data_kelurahansub$Mean_Age_Active_Users)
mvn(data_kelurahansub$Female_User)
mvn(data_kelurahansub$Male_User)
mvn(data_kelurahansub$Total_User)
mvn(data_kelurahansub$User_Ratio)
mvn(data_kelurahansub$pct_maleuser)
mvn(data_kelurahansub$pct_femaleuser)
mvn(data_kelurahansub$reportdensityarea)
mvn(data_kelurahansub$reportdensitypop)
mvn(data_kelurahansub$reportdensityuser)
mvn(data_kelurahansub$ReportsCount)
mvn(data_kelurahansub$entropy_type)
mvn(data_kelurahansub$entropy_time)
mvn(data_kelurahansub$entropy_age)

# Kecamatan variables
mvn(data_kecamatansub$Male_Pop)
mvn(data_kecamatansub$Female_Pop)
mvn(data_kecamatansub$Pop)
mvn(data_kecamatansub$Pct_Poor_Household)
mvn(data_kecamatansub$Mean_Age_active_user) #
mvn(data_kecamatansub$Female_user) #
mvn(data_kecamatansub$Male_user)
mvn(data_kecamatansub$Total_user) #
mvn(data_kecamatansub$Ratio_of_user) 
mvn(data_kecamatansub$pct_maleuser)
mvn(data_kecamatansub$pct_femaleuser)
mvn(data_kecamatansub$reportdensityarea)
mvn(data_kecamatansub$reportdensitypop)
mvn(data_kecamatansub$reportdensityuser) #
mvn(data_kecamatansub$ReportsCount)
mvn(data_kecamatansub$entropy_type)
mvn(data_kecamatansub$entropy_time) #
mvn(data_kecamatansub$entropy_age) #


# --------------------------------------------------
# Transforming the value of variables to to z-scores
# --------------------------------------------------
# Kelurahan variables
data_kelurahansub1 <- scale(data_kelurahansub, center = TRUE, scale = TRUE)

# Kecamatan variables
data_kecamatansub1 <- scale(data_kecamatansub, center = TRUE, scale = TRUE)

# -----------------------------------
# Checking for multivariate normality
# -----------------------------------
kel.result <- mvn(data_kelurahansub1, mvnTest = "mardia") 
kel.result$multivariateNormality

kec.result <- mvn(data_kecamatansub1, mvnTest = "mardia") 
kec.result$multivariateNormality


# ••••••••••••••••••••
# Developing the model
# ••••••••••••••••••••
# -----------
# Model Tested
# -----------
# Model 1 (Two latent variables)
kecamatan_model <-'
# Latent Variables
DIV =~ entropy_time + entropy_age
DEM =~ Total_user + Mean_Age_active_user + Female_user
# Regression
reportdensityuser ~ DEM + DIV
'

# Model 2 (One latent variable)
kecamatan_model1 <-'
# Latent Variables
DIV =~ entropy_time + entropy_age
# Regression
reportdensityuser ~ Female_user + Total_user + Mean_Age_active_user + DIV
'

# Model 3 (No latent variable)
kecamatan_model2 <-'
# Regression
reportdensityuser ~ Total_user + Mean_Age_active_user +  entropy_time + entropy_age
'

# •••••••••••
# The results
# •••••••••••
fit_kec <- cfa(kecamatan_model, data = data_kecamatansub1)
varTable(fit_kec)
parameterEstimates(fit_kec)
y <- summary(fit_kec, fit.measures=T, standardized=T, rsquare=T)
inspect(fit_kec, 'r2')

# •••••••••••••••••••••••••
# Making the diagram of SEM
# •••••••••••••••••••••••••
semPaths(fit_kec, 'est', layout = 'tree', sizeMan = 10, nCharNodes = 2, edge.label.cex = 3)
?semPaths

############################################
#### Geographically Weighted Regression ####
############################################
# •••••••••••••••••••
# Loading the library
# •••••••••••••••••••
library(car)
library(spgwr) # For GWR
library(ggplot2)
library(maptools)
library(GWmodel) # For MGWR

# •••••••••••••••••••••
# Preparing the dataset
# •••••••••••••••••••••
# Adding the attribute data to kelurahan SF
KelurahanSF$Shannon_Type <- Kelurahan@data$ShannonType
KelurahanSF$Shannon_Time <- Kelurahan@data$ShannonTime
KelurahanSF$Shannon_Age <- Kelurahan@data$ShannonUserAge
KelurahanSF$ReportsCount <- Kelurahan@data$ReportsCount
KelurahanSF$ReportsDensityArea <- Kelurahan@data$ReportsDensityArea
KelurahanSF$ReportsDensityPop <- Kelurahan@data$ReportsDensityPop
KelurahanSF$ReportsDensityUsers <- Kelurahan@data$ReportsDensityUsers

# Normalise the kelurahan variables into z-score
KelurahanSF$Male_Pop_Sc <- scale(KelurahanSF$Male_Pop, center = TRUE, scale = TRUE)
KelurahanSF$Female_Pop_Sc <- scale(KelurahanSF$Female_Pop, center = TRUE, scale = TRUE)
KelurahanSF$Pop_Sc <- scale(KelurahanSF$Pop, center = TRUE, scale = TRUE)
KelurahanSF$Female_User_Sc <- scale(KelurahanSF$Female_Use, center = TRUE, scale = TRUE)
KelurahanSF$Male_User_Sc <- scale(KelurahanSF$Male_User, center = TRUE, scale = TRUE)
KelurahanSF$Total_User_Sc<- scale(KelurahanSF$Total_User, center = TRUE, scale = TRUE)
KelurahanSF$PctUnemployed_Sc <- scale(KelurahanSF$PctUnemplo, center = TRUE, scale = TRUE)
KelurahanSF$Mean_Age_Sc <- scale(KelurahanSF$Mean_Age, center = TRUE, scale = TRUE)
KelurahanSF$Shannon_Type_Sc <- scale(KelurahanSF$Shannon_Type, center = TRUE, scale = TRUE)
KelurahanSF$Shannon_Time_Sc <- scale(KelurahanSF$Shannon_Time, center = TRUE, scale = TRUE)
KelurahanSF$Shannon_Age_Sc <- scale(KelurahanSF$Shannon_Age, center = TRUE, scale = TRUE)
KelurahanSF$Reports_Count_Sc <- scale(KelurahanSF$ReportsCount, center = TRUE, scale = TRUE)
KelurahanSF$Reports_Density_Area_Sc <- scale(KelurahanSF$ReportsDensityArea, center = TRUE, scale = TRUE)
KelurahanSF$Reports_Density_Pop_Sc <- scale(KelurahanSF$ReportsDensityPop, center = TRUE, scale = TRUE)
KelurahanSF$Reports_Density_User_Sc <- scale(KelurahanSF$ReportsDensityUsers, center = TRUE, scale = TRUE)

# Adding the kelurahan variables into SPDF
Kelurahan$Male_Pop_Sc <- scale(KelurahanSF$Male_Pop, center = TRUE, scale = TRUE)
Kelurahan$Female_Pop_Sc <- scale(KelurahanSF$Female_Pop, center = TRUE, scale = TRUE)
Kelurahan$Pop_Sc <- scale(KelurahanSF$Pop, center = TRUE, scale = TRUE)
Kelurahan$Female_User_Sc <- scale(KelurahanSF$Female_Use, center = TRUE, scale = TRUE)
Kelurahan$Male_User_Sc <- scale(KelurahanSF$Male_User, center = TRUE, scale = TRUE)
Kelurahan$Total_User_Sc<- scale(KelurahanSF$Total_User, center = TRUE, scale = TRUE)
Kelurahan$PctUnemployed_Sc <- scale(KelurahanSF$PctUnemplo, center = TRUE, scale = TRUE)
Kelurahan$Mean_Age_Sc <- scale(KelurahanSF$Mean_Age, center = TRUE, scale = TRUE)
Kelurahan$Shannon_Type_Sc <- scale(KelurahanSF$Shannon_Type, center = TRUE, scale = TRUE)
Kelurahan$Shannon_Time_Sc <- scale(KelurahanSF$Shannon_Time, center = TRUE, scale = TRUE)
Kelurahan$Shannon_Age_Sc <- scale(KelurahanSF$Shannon_Age, center = TRUE, scale = TRUE)
Kelurahan$Reports_Count_Sc <- scale(KelurahanSF$ReportsCount, center = TRUE, scale = TRUE)
Kelurahan$Reports_Density_Area_Sc <- scale(KelurahanSF$ReportsDensityArea, center = TRUE, scale = TRUE)
Kelurahan$Reports_Density_Pop_Sc <- scale(KelurahanSF$ReportsDensityPop, center = TRUE, scale = TRUE)
Kelurahan$Reports_Density_User_Sc <- scale(KelurahanSF$ReportsDensityUsers, center = TRUE, scale = TRUE)

a <- Kelurahan[,cbind(48:62)]
KelurahanPython <- as(Kelurahan, "SpatialPolygonsDataFrame")

writeOGR(KelurahanPython, "KelurahanPython", layer='newstuff', driver="ESRI Shapefile")
writeOGR(KelurahanPython, "Kelurahan_geojson", layer="Kelurahan", driver="GeoJSON")

# •••••••••••••••••••••••••••••••••
# Ordinary Least Squares Regression
# •••••••••••••••••••••••••••••••••

KelurahanSF$coord.x <- coordsKel[,1]
KelurahanSF$coord.y <- coordsKel[,2]

# -------------------------------------
# Checking the VIF of related variables
# -------------------------------------
# Kelurahan variables
vif(lm(Reports_Count_Sc ~ Female_User_Sc + Male_User_Sc + PctUnemployed_Sc + Shannon_Type_Sc + Shannon_Time_Sc + Shannon_Age_Sc, data=Kelurahan))
summary(Kelurahan)

# --------------
# OLS Regression
# --------------
kel_regression <- lm(Reports_Count_Sc ~ Female_User_Sc + Male_User_Sc + PctUnemployed_Sc + Shannon_Type_Sc + Shannon_Time_Sc + Shannon_Age_Sc, data=Kelurahan)
summary(kel_regression)

# ••••••••••••••••••••••••••••••••••
# Geographically Weighted Regression
# ••••••••••••••••••••••••••••••••••
# ----------------------------------
# Spatial heterogeneity of residuals
# ----------------------------------
# Residuals of kelurahan model
resids_kel <- residuals(kel_regression)
colours <- c("dark blue", "blue", "red", "dark red") 
resids_kel.df <- data.frame(resids_kel, KelurahanSF$coord.x, KelurahanSF$coord.y)

# Making a SPDF of residuals
map.resids_kel <- SpatialPointsDataFrame(data=resids_kel.df[1], coords=cbind(resids_kel.df[2], resids_kel.df[3])) 

# Plotting the residual map
spplot(map.resids_kel, cuts=quantile(resids_kel), col.regions=colours, cex=2)

# ----------------------------------
# Geographically Weighted Regression
# ----------------------------------
# Calculating kernel bandwidth
GWRbandwidth_kel <- gwr.sel(Reports_Count_Sc ~ Female_User_Sc + Male_User_Sc + Shannon_Time_Sc + Shannon_Age_Sc, data=Kelurahan, adapt=T, method = 'aic') 

# Running the analysis
gwr.model_kel = gwr(Reports_Count_Sc ~ Female_User_Sc + Male_User_Sc + Shannon_Time_Sc + Shannon_Age_Sc, data=Kelurahan, adapt=GWRbandwidth_kel, hatmatrix=TRUE, se.fit=TRUE) 
?gwr
# Printing the results
gwr.model_kel

results_kel <- as.data.frame(gwr.model_kel$SDF)
summary(results_kel)

# ------------------------
# Interpreting the results
# ------------------------
# Assigning variable estimates into SPDF
Kelurahan$Est_Female_User <- gwr.model_kel$SDF$Female_User_Sc
Kelurahan$Est_Male_User <- gwr.model_kel$SDF$Male_User_Sc
Kelurahan$Est_Shannon_Time <- gwr.model_kel$SDF$Shannon_Time_Sc
Kelurahan$Est_Shannon_Age <- gwr.model_kel$SDF$Shannon_Age_Sc

# Plotting the variable estimates against kelurahan shapefiles
SigColours<- rev(brewer.pal(3, "YlGnBu"))

Est_Female_User_Map <- tm_shape(Kelurahan) + tm_polygons("Est_Female_User",
                                                         style="jenks",
                                                         palette=SigColours,
                                                         midpoint=NA,
                                                         title="Female User estimates") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)

Est_Male_User_Map <- tm_shape(Kelurahan) + tm_polygons("Est_Male_User",
                                                       style="jenks",
                                                       palette=SigColours,
                                                       midpoint=NA,
                                                       title="Male User estimates") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)

Est_Shannon_Time_Map <- tm_shape(Kelurahan) + tm_polygons("Est_Shannon_Time",
                                                          style="jenks",
                                                          palette=SigColours,
                                                          midpoint=NA,
                                                          title="Shannon Index of Report Time estimates") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)

Est_Shannon_Age_Map <- tm_shape(Kelurahan) + tm_polygons("Est_Shannon_Age",
                                                         style="jenks",
                                                         palette=SigColours,
                                                         midpoint=NA,
                                                         title="Shannon Index of User Age estimates") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)
Est_Female_User_Map
Est_Male_User_Map
Est_Shannon_Age_Map
Est_Shannon_Time_Map

# Looking for t-value of all variables 
Kelurahan$Sig_Female_User <- gwr.model_kel$SDF$Female_User_Sc / gwr.model_kel$SDF$Female_User_Sc_se
Kelurahan$Sig_Male_User <- gwr.model_kel$SDF$Male_User_Sc / gwr.model_kel$SDF$Male_User_Sc_se
Kelurahan$Sig_Shannon_Time <- gwr.model_kel$SDF$Shannon_Time_Sc / gwr.model_kel$SDF$Shannon_Time_Sc_se
Kelurahan$Sig_Shannon_Age <- gwr.model_kel$SDF$Shannon_Age_Sc / gwr.model_kel$SDF$Shannon_Age_Sc_se


# Plotting the distribution of t-values
Sig_Female_User_map <- tm_shape(Kelurahan) + tm_polygons("Sig_Female_User",
                                                         style="jenks",
                                                         palette=SigColours,
                                                         midpoint=NA,
                                                         title="t-value Female User") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)


Sig_Male_User_map <- tm_shape(Kelurahan) + tm_polygons("Sig_Male_User",
                                                       style="jenks",
                                                       palette=SigColours,
                                                       midpoint=NA,
                                                       title="t-value Male User") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)

Sig_Shannon_Age_map <- tm_shape(Kelurahan) + tm_polygons("Sig_Shannon_Age",
                                                         style="jenks",
                                                         palette=SigColours,
                                                         midpoint=NA,
                                                         title="t-value Shannon Index of Users' Age") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)

Sig_Shannon_Time_map <- tm_shape(Kelurahan) + tm_polygons("Sig_Shannon_Time",
                                                          style="jenks",
                                                          palette=SigColours,
                                                          breaks=SigColours,
                                                          midpoint=NA,
                                                          title="t-value Shannon Index of Report Times") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)

# Plotting the result
Sig_Female_User_map
Sig_Male_User_map 
Sig_Shannon_Time_map
Sig_Shannon_Age_map

# Plotting the map of local R2
Kelurahan$LocalR2 <- results_kel$localR2
Local_R2_map <- tm_shape(Kelurahan) + tm_polygons("LocalR2",
                                                  style="jenks",
                                                  palette="RdBu",
                                                  midpoint=NA,
                                                  title="Local R2") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)
Local_R2_map

# Plotting the residuals of GWR
Kelurahan$Residuals <- results_kel$gwr.e
GWR_Residual_map <- tm_shape(Kelurahan) + tm_polygons("Residuals",
                                                      style="jenks",
                                                      palette="PuBuGn",
                                                      midpoint=NA,
                                                      title="GWR Residuals") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)
GWR_Residual_map

########################################################
#### Multi-scale Geographically Weighted Regression ####
########################################################
# --------
# Analysis
# --------
mgwr.model_kel <- gwr.multiscale(Reports_Count_Sc ~ Female_User_Sc + Male_User_Sc + Shannon_Time_Sc + Shannon_Age_Sc, data=Kelurahan, kernel="gaussian", adapt=T, hatmatrix=TRUE, bws0 = GWRbandwidth_kel, approach="AIC", dMats)
print(mgwr.model_kel)
mgwr.model_kel$SDF$residual
?gwr.multiscale

# --------------------
# Plotting the results
# --------------------
# Residuals
Kelurahan$MGWR_Residuals <- mgwr.model_kel$SDF@data$residual
MGWR_Residual_map <- tm_shape(Kelurahan) + tm_polygons("MGWR_Residuals",
                                                       style="jenks",
                                                       palette="PuBuGn",
                                                       midpoint=NA,
                                                       title="MGWR Residuals") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)
MGWR_Residual_map

# Plotting the variable estimates against kelurahan shapefiles
SigColours<- rev(brewer.pal(3, "YlGnBu"))


# Assigning variable estimates into SPDF
Kelurahan$MGWR_Est_Female_User <- mgwr.model_kel$SDF$Female_User_Sc
Kelurahan$MGWR_Est_Male_User <- mgwr.model_kel$SDF$Male_User_Sc
Kelurahan$MGWR_Est_Shannon_Time <- mgwr.model_kel$SDF$Shannon_Time_Sc
Kelurahan$MGWR_Est_Shannon_Age <- mgwr.model_kel$SDF$Shannon_Age_Sc
Kelurahan$MGWR_Residuals <- mgwr.model_kel$SDF$residual

# Plotting the variable estimates against kelurahan shapefiles
SigColours<- rev(brewer.pal(3, "YlGnBu"))

MGWR_Est_Female_User_Map <- tm_shape(Kelurahan) + tm_polygons("MGWR_Est_Female_User",
                                                              style="jenks",
                                                              palette=SigColours,
                                                              midpoint=NA,
                                                              title="Female User estimates") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)

MGWR_Est_Male_User_Map <- tm_shape(Kelurahan) + tm_polygons("MGWR_Est_Male_User",
                                                            style="jenks",
                                                            palette=SigColours,
                                                            midpoint=NA,
                                                            title="Male User estimates") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)

MGWR_Est_Shannon_Time_Map <- tm_shape(Kelurahan) + tm_polygons("MGWR_Est_Shannon_Time",
                                                               style="jenks",
                                                               palette=SigColours,
                                                               midpoint=NA,
                                                               title="Shannon Index of Report Time estimates") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)

MGWR_Est_Shannon_Age_Map <- tm_shape(Kelurahan) + tm_polygons("MGWR_Est_Shannon_Age",
                                                              style="jenks",
                                                              palette=SigColours,
                                                              midpoint=NA,
                                                              title="Shannon Index of User Age estimates") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)
MGWR_Residual_map <- tm_shape(Kelurahan) + tm_polygons("MGWR_Residuals",
                                                       style="jenks",
                                                       palette=SigColours,
                                                       midpoint=NA,
                                                       title="Residuals") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)



MGWR_Est_Female_User_Map
MGWR_Est_Male_User_Map
MGWR_Est_Shannon_Age_Map
MGWR_Est_Shannon_Time_Map
MGWR_Residual_map

# Looking for t-value of all variables 
SE.Female <- sqrt(var(mgwr.model_kel$SDF$Female_User_Sc)/length(mgwr.model_kel$SDF$Female_User_Sc))
SE.Male <- sqrt(var(mgwr.model_kel$SDF$Male_User_Sc)/length(mgwr.model_kel$SDF$Male_User_Sc))
SE.Time <- sqrt(var(mgwr.model_kel$SDF$Shannon_Time_Sc)/length(mgwr.model_kel$SDF$Shannon_Time_Sc))
SE.Age <- sqrt(var(mgwr.model_kel$SDF$Shannon_Age_Sc)/length(mgwr.model_kel$SDF$Shannon_Age_Sc))

Kelurahan$MGWR_Sig_Female_User <- mgwr.model_kel$SDF$Female_User_Sc/ SE.Female
Kelurahan$MGWR_Sig_Male_User <- mgwr.model_kel$SDF$Male_User_Sc / SE.Male
Kelurahan$MGWR_Sig_Shannon_Time <- mgwr.model_kel$SDF$Shannon_Time_Sc / SE.Time
Kelurahan$MGWR_Sig_Shannon_Age <- mgwr.model_kel$SDF$Shannon_Age_Sc / SE.Age

# Plotting the distribution of t-values
MGWR_Sig_Female_User_map <- tm_shape(Kelurahan) + tm_polygons("Sig_Female_User",
                                                              style="jenks",
                                                              palette=SigColours,
                                                              midpoint=NA,
                                                              title="t-value Female User") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)


MGWR_Sig_Male_User_map <- tm_shape(Kelurahan) + tm_polygons("Sig_Male_User",
                                                            style="jenks",
                                                            palette=SigColours,
                                                            midpoint=NA,
                                                            title="t-value Male User") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)

MGWR_Sig_Shannon_Age_map <- tm_shape(Kelurahan) + tm_polygons("Sig_Shannon_Age",
                                                              style="jenks",
                                                              palette=SigColours,
                                                              midpoint=NA,
                                                              title="t-value Shannon Index of Users' Age") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)

MGWR_Sig_Shannon_Time_map <- tm_shape(Kelurahan) + tm_polygons("Sig_Shannon_Time",
                                                               style="jenks",
                                                               palette=SigColours,
                                                               breaks=SigColours,
                                                               midpoint=NA,
                                                               title="t-value Shannon Index of Report Times") + tm_layout(legend.text.size = 1.3, legend.title.size = 1.3)

# Plotting the result
MGWR_Sig_Female_User_map
MGWR_Sig_Male_User_map 
MGWR_Sig_Shannon_Time_map
MGWR_Sig_Shannon_Age_map

