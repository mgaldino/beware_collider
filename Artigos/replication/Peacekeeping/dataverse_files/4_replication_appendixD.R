## Analyses: Appendix D Spatial Models for "Protecting the Vote? Peacekeeping Presence And The Risk Of Electoral Violence"
## Authors: Fjelde and Smidt
## Last modified: 26.02.2021

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
library(splm)
library(plm)

# Set path 
setwd("C:/Users/hanna/Dropbox/PC/Documents/PaperProjects/Paper-PKO and election-related violence/Submissions/BJPS/BJPS_R&R2/Replication Materials")

# Load GEOPKO for PKO personnel per base 
data <- read.dta13("replicationData.dta")

# Only keep relevant observations (that are also in the analyses)
data2 <- subset(data, PKO_presNoPol==1)
data2 <- subset(data2, year>=1994 & year<=2017)
data2 <- data2[-which(data2$collapsed_time_si_el>6 & data2$collapsed_time_to_el>6),]

# Drop Chad and Liberia which are not in VECO
data2 <- subset(data2, country!="Liberia")
data2 <- subset(data2, country!="Chad")

# Load all shapefiles for countries hosting a PKO (downloaded from gadm.org)
BDI <- readRDS("./GADM/gadm36_BDI_2_sp.rds")
CAF <- readRDS("./GADM/gadm36_CAF_2_sp.rds")
CIV <- readRDS("./GADM/gadm36_CIV_2_sp.rds")
COD <- readRDS("./GADM/gadm36_COD_2_sp.rds")
MLI <- readRDS("./GADM/gadm36_MLI_2_sp.rds")
MOZ <- readRDS("./GADM/gadm36_MOZ_2_sp.rds")
SDN <- readRDS("./GADM/gadm36_SDN_2_sp.rds")
SLE <- readRDS("./GADM/gadm36_SLE_2_sp.rds")

# Bind all shapefules into one
shapefiles <- bind(BDI,CAF,CIV,COD,MLI,MOZ,SLE,SDN, keepnames=F)

# Correct a mistake
shapefiles@data$NAME_2[shapefiles@data$NAME_2=="Kipushi (ville) "] <- "Kipushi (ville)"


# Add gwcode to shapefiles
shapefiles@data$gwcode <- shapefiles@data$NAME_0
shapefiles@data$gwcode <- mapvalues(shapefiles@data$gwcode, from = c("Burundi", "Central African Republic"
                                                                     ,"Côte d'Ivoire"
                                                                     , "Democratic Republic of the Congo"
                                                                     ,"Mali","Mozambique","Sierra Leone","Sudan")
                                    ,to = c(516,482,437,490,432,541,451,625) )


# Reduce dataset
data3 <- data2[,c("country","gwcode" ,"name_1", "name_2", "year", "month"
                 ,"un_military_base2"
                 ,"viol_stateBased_preDep","viol_stateBased_preDep_trend"
                 , "viol_OneSided_preDep", "viol_OneSided_preDep_trend"
                 , "pop_gpw_sum", "roadDensity"
                 , "imr_mean", "bdist1", "capdist", "un_military_base_du2Lag1" 
                 , "un_military_base_du_spL", "veco_viol_du","veco_viol","veco_viol_post_du" ,"veco_viol_pre_du")] 

data4 <- data3[complete.cases(data3),]


# Dataset of analysis
data_splm <- data4 %>% mutate(  id = as.numeric(factor(paste0(gwcode, name_1, name_2)))
                               , yearmo = format(as.Date(paste0(year,"/",month,"/", 1)), "%Y/%m") ) %>% 
                       dplyr::select(id,yearmo, everything()) %>% as.data.frame()

data_splm <- data_splm %>% arrange(id, yearmo) %>% group_by(id) %>% dplyr::mutate(yearmo = seq( n() ) )
 
# Keep only first election in country
data_splm <- subset(data_splm, yearmo<=13)

# Make panel and check balance
pdata_splm <- plm::pdata.frame(data_splm, index = c("id", "yearmo") )
plm::is.pbalanced(pdata_splm)


# Merge with shape file
shapefiles2 <- shapefiles
shapefiles2@data <- sp::merge(pdata_splm, shapefiles2
                             , by.y = c("gwcode", "NAME_1", "NAME_2")
                             , by.x = c("gwcode", "name_1", "name_2")
                             , all.y=T)


shapefiles2@data$id
pdata_splm$id

# Make spatial weights matrix
nb <- spdep::poly2nb(shapefiles2)
w <-  spdep::nb2mat(nb, style="B", zero.policy=TRUE)
wlist <- spdep::nb2listw(nb, style="B", zero.policy=TRUE)

# Plot to check
plot(polygons(shapefiles2))
coords <- coordinates(polygons(shapefiles2))
plot(nb, coords, add=TRUE, col="red")


## Spatial error models

m1 <- spml(formula = (veco_viol_du ~ un_military_base2)
          , data = data_splm, listw = wlist, model = "within", spatial.error=c("b") )
summary(m1)


m2 <- spml(formula = (veco_viol_du ~ un_military_base2 + viol_stateBased_preDep + viol_stateBased_preDep_trend + viol_OneSided_preDep + viol_OneSided_preDep_trend + pop_gpw_sum + un_military_base_du_spL)
           , data = data_splm, listw = wlist, model = "within", spatial.error=c("b") )
summary(m2)


m3 <- spml(formula = (veco_viol_du ~ un_military_base2 + viol_stateBased_preDep + viol_stateBased_preDep_trend + viol_OneSided_preDep + viol_OneSided_preDep_trend + pop_gpw_sum + un_military_base_du_spL + as.factor(yearmo))
           , data = data_splm, listw = wlist, model = "within", spatial.error=c("b") )
summary(m3)

pdata_splm$veco_viol_trend1 = ( plm::lag(pdata_splm$veco_viol, k=2) + plm::lag(pdata_splm$veco_viol, k=3) )/2 - plm::lag(pdata_splm$veco_viol, k=1)
pdata_splm$veco_viol_trend2 = ( plm::lag(pdata_splm$veco_viol, k=3) ) - ( plm::lag(pdata_splm$veco_viol, k=2) + plm::lag(pdata_splm$veco_viol, k=1) )


m4 <- spml(formula = (veco_viol_du ~ un_military_base2 + viol_stateBased_preDep + viol_stateBased_preDep_trend + viol_OneSided_preDep + viol_OneSided_preDep_trend + pop_gpw_sum + un_military_base_du_spL + veco_viol_trend1 )
           , data = subset(pdata_splm, !is.na(veco_viol_trend1)) , listw = wlist, model = "within", spatial.error=c("b") )
summary(m4)


m5 <- spml(formula = (veco_viol_du ~ un_military_base2 + viol_stateBased_preDep + viol_stateBased_preDep_trend + viol_OneSided_preDep + viol_OneSided_preDep_trend + pop_gpw_sum + un_military_base_du_spL + veco_viol_trend2 )
           , data = subset(pdata_splm, !is.na(veco_viol_trend2)) , listw = wlist, model = "within", spatial.error=c("b") )
summary(m5)


library(xtable)


coef <- rbind.fill( as.data.frame(t(m1$coefficients)), as.data.frame(t(m2$coefficients))
                    , as.data.frame(t(m3$coefficients[1:8])) , as.data.frame(t(m4$coefficients))
                    , as.data.frame(t(m5$coefficients)))[2:10]

se1 <- as.data.frame(t( sqrt(diag(m1$vcov))[2:2] ))
colnames(se1) <- colnames(coef)[c(1)]
se2 <- as.data.frame(t( sqrt(diag(m2$vcov))[2:8] ) )
colnames(se2) <- colnames(coef)[c(1:7)]
se3 <- as.data.frame(t( sqrt(diag(m3$vcov))[2:8] ))
colnames(se3) <- colnames(coef)[c(1:7)]
se4 <- as.data.frame( t(sqrt(diag(m4$vcov))[2:9] ))
colnames(se4) <- colnames(coef)[c(1:8)]
se5 <- as.data.frame( t(sqrt(diag(m5$vcov))[2:9] ))
colnames(se5) <- colnames(coef)[c(1:7,9)]

se <- plyr::rbind.fill(se1, se2, se3, se4, se5)

models <- matrix(NA, nrow=18, ncol=5)
z <- matrix(NA, nrow=9, ncol=5)
pval <- matrix(NA, nrow=9, ncol=5)


for(i in 1:9){
  models[i*2-1,]  <- round(coef[,i],3)
  models[i*2,]  <- paste0("(",round(se[,i],3),")")
  z[i,] <-   coef[,i]/se[,i]
  pval[i,] <- 2*pnorm(-abs(z[i,]))
}


for(j in 1:5){
 for(i in seq(1,17,2) ) {
  models[i,j] <-           if(!is.na(pval[(i+1)/2,j]) & pval[(i+1)/2,j]<0.01){ 
    paste0(models[i,j], "***" ) 
  } else if(!is.na(pval[(i+1)/2,j]) & pval[(i+1)/2,j]<0.05){
    paste0(models[i,j], "**" )
  } else if(!is.na(pval[(i+1)/2,j]) & pval[(i+1)/2,j]<0.1){
    paste0(models[i,j], "*" )
  } else {paste0(models[i,j], "" ) }
 }
}


xtable(models)

nrow(summary(m1)$model)
nrow(summary(m3)$model)
nrow(summary(m3)$model)
nrow(summary(m4)$model)
nrow(summary(m5)$model)


