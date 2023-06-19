## Analyses: Map for "Protecting the Vote? Peacekeeping Presence And The Risk Of Electoral Violence"
## Authors: Fjelde and Smidt
## Last modified: 26.02.2021

# The DECO dataset was collected by Hanne Fjelde and Kristine Höglund (2021) and is not published yet.
# Once it is published, we will update this file.


# Remove all objects from working space
rm(list=ls())

# Install packages
#install.packages("GADMTools")
#install.packages("raster")
#install.packages("plyr")
#install.packages("haven")
#install.packages("states")
#install.packages("dplyr")
#install.packages("readxl")

# Libraries
library(raster)
library(plyr)
library(haven)
library(states)
library(dplyr)
library(readxl)
library(readstata13)


# Set path 
setwd("YOUR DIRECTORY")

# Load GEOPKO for PKO personnel per base 
data <- read.dta13("replicationData.dta")

# Load DECO
DECO <- read_excel("USE THE DECO DATASET HERE")

# Drop events which we cannot place temporally or spatially
DECO1 <- DECO[DECO$where_prec<4,] # 3211 from 4238 obs.

# Only keep events that are identified as related to elections
DECO2 <- DECO1[DECO1$electoral_vio==1,] # 3211 out of 3211 obs.

DECO2$longitude <- as.numeric(DECO2$longitude)
DECO2$latitude <- as.numeric(DECO2$latitude)
DECO2 <- DECO2[!is.na(DECO2$longitude),] # One with missing long.
DECO2 <- DECO2[!is.na(DECO2$latitude),] # Zero missing after missing in long. dropped.

attach(DECO2)
coordinates(DECO2) = ~ longitude + latitude


# Load all shapefiles for countries hosting a PKO (downloaded from gadm.org)
BDI <- readRDS("./GADM/gadm36_BDI_2_sp.rds")
CAF <- readRDS("./GADM/gadm36_CAF_2_sp.rds")
CIV <- readRDS("./GADM/gadm36_CIV_2_sp.rds")
COD <- readRDS("./GADM/gadm36_COD_2_sp.rds")
MLI <- readRDS("./GADM/gadm36_MLI_2_sp.rds")
MOZ <- readRDS("./GADM/gadm36_MOZ_2_sp.rds")
SLE <- readRDS("./GADM/gadm36_SLE_2_sp.rds")
SDN <- readRDS("./GADM/gadm36_SDN_2_sp.rds")

# Chad and Liberia not in DECO

# Bin all shapefules into one
# do not keep rownames (keepnames) but assign new ones for identifying polygons (admin units)
shapefiles <- bind(BDI,CAF,CIV,COD,MLI,MOZ,SLE,SDN, keepnames=F)

# Correct a mistake
shapefiles@data$NAME_2[shapefiles@data$NAME_2=="Kipushi (ville) "] <- "Kipushi (ville)"

# Make a spatial polygon dataframe from the shapefiles
map_poly <- SpatialPolygons(shapefiles@polygons)

# Reduce dataset
data2 <- data[,c("country","gwcode", "name_1", "name_2", "PKO_presNoPol","collapsed_time_si_el","collapsed_time_to_el", "year", "month",  "un_military_base2")] 

# Create indicator for military base
data2$un_military_base_du <- ifelse(data2$un_military_base2>0,1,0)

# Only keep relevant observations (that are also in the analyses)
data2a <- subset(data2, PKO_presNoPol==1)
data2b <- subset(data2a, year>=1994 & year<=2017)
data2c <- data2b[-which(data2b$collapsed_time_si_el>6 & data2b$collapsed_time_to_el>6),]

# Aggregate to gwcode, name_1 name2
data3a <- data2c[,c("gwcode", "name_1", "name_2","un_military_base2")]
data3a$un_military_base2 <- as.numeric(as.character(data3a$un_military_base2))
data3b <- data3a %>% dplyr::group_by(gwcode, name_1, name_2) %>% dplyr::summarize(un_military_base2 = sum(un_military_base2, na.rm=T)) %>% as.data.frame()
data3b$un_military_base2 <- ifelse(data3b$un_military_base2>0,1,0)

# Add gwcode to shapefiles
shapefiles@data$gwcode <- shapefiles@data$NAME_0
shapefiles@data$gwcode <- mapvalues(shapefiles@data$gwcode, from = c("Burundi", "Central African Republic"
                                                   ,"Côte d'Ivoire", "Democratic Republic of the Congo"
                                                   ,"Mali","Mozambique","Sierra Leone","Sudan")
                            ,to = c(516,482,437,490,432,541,451,625) )


# Merge datasets
spatial_data3 <- sp::merge(x=shapefiles, y=data3b[data3b$un_military_base2==1,], by.x = c("gwcode", "NAME_1", "NAME_2"), by.y=c("gwcode", "name_1", "name_2"),  all.x=F, all.y=T)

# Make a spatial polygon dataframe from the shapefiles
map_poly_pk <- SpatialPolygons(spatial_data3@polygons)

# Add to "shapefiles" the rownumber of the data stored in "shapefiles" 
#   The rownumber is an identifier for each polygon (i.e. second administrative unit)
shapefiles@data$OBJECTID <- as.numeric(rownames(shapefiles@data))

# Overlay the polygons for the second admin units over the spatialPointsDataFrame "GEOPKO"
# Each spatial point gets assigned an OBJECTID, which is a number that identified the 
# polygon for the second administrative unit in the map_poly and in the shapefiles@data
OBJECTID <- over(DECO2, map_poly)

# Bind the OBJECT ID togather with the GEOPKO2@data and also the coordinates of GEOPKO2 to keep the lon and lat of PKO bases
DECO2@data <- cbind(DECO2@data, coordinates(DECO2), OBJECTID)


# Merge the spatial points dataframe with the OBJECTID for the administrative unit of a spatial point
#   to the data of the shapefiles.
#   all.y means here that we only want to keep those spatial points from
#   the GEOPKO that fall within the shapesfiles
DECO3 <- merge(DECO2@data, shapefiles@data, by="OBJECTID", all.y=T)



#######################
### PLOT FOR PAPER ####
#######################
test <- DECO3[complete.cases(DECO3[,c("longitude", "latitude")]),]

spdf <- SpatialPointsDataFrame(coords = test[,c("longitude", "latitude")], data = test,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

library(rworldmap)
newmap <- getMap()

png(file="./Figures/mapDECO.png",width=800,height=800,res=550)

par(mar=c(1,1,1,1))
plot(newmap
     , col="gray85", border="gray85"
     , xlim = c(-15, 50), ylim = c(1, 4) )
plot(map_poly, lwd=0.1, add=T, border="gray50")
plot(map_poly_pk, lwd=0.1, add=T, border="gray50", col="lightblue")

plot(readRDS("./GADM/countries/gadm36_SDN_0_sp.rds")
     , border="black", add=T, lwd=0.1)
plot(readRDS("./GADM/countries/gadm36_SLE_0_sp.rds")
     , border="black", add=T, lwd=0.1)
plot(readRDS("./GADM/countries/gadm36_MOZ_0_sp.rds")
     , border="black", add=T, lwd=0.1)
plot(readRDS("./GADM/countries/gadm36_COD_0_sp.rds")
     , border="black", add=T, lwd=0.1)
plot(readRDS("./GADM/countries/gadm36_CIV_0_sp.rds")
     , border="black", add=T, lwd=0.1)
plot(readRDS("./GADM/countries/gadm36_CAF_0_sp.rds")
     , border="black", add=T, lwd=0.1)
plot(readRDS("./GADM/countries/gadm36_BDI_0_sp.rds")
     , border="black", add=T, lwd=0.1)
plot(readRDS("./GADM/countries/gadm36_MLI_0_sp.rds")
      , border="black", add=T, lwd=0.1) 

points(spdf, col="black", pch=18, cex=0.1)

dev.off()

png(file="./Figures/mapDECO_legend.png", res=150)
par(mar=c(2.1, 2.1, 4.1, 2.1))
plot(x=c(0,1), y=c(0,1), type="n", xlim=c(0,1), ylim=c(0,1), axes=F, xlab="", ylab="")
legend(x=0, y=1, legend = c("Electoral violence", "Peacekeeping base"), pch = c(18,15), col=c("black", "lightblue"), cex = c(1,1))
dev.off()

