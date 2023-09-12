#packages
library(sf)
library(spdep)
library(tmap)
library(rgeos)
library(maptools)
library(dplyr)
library(sp)
library(plotKML)
library(rasterVis)
library(raster)
library(tidyr)
library(ggplot2)

#----------------------------------------------------------------------------------
#data
survey2023_INSPQ <- read_csv("data/survey2023 INSPQ.csv")
geo <- st_read("data/geo_recensement_2021_QC_harmonise_ISQ_format_SHP/AD_2021_QC_LimCarto_harmo.shp")


#----------------------------------------------------------------------------------
#Moran I test - have to remove NAs first
map_data <- map_data[!is.na(map_data$risk_impact) ,]

#----------------------------------------------------------------------------------

nb <- poly2nb(map_data, queen=TRUE) #queen vs rook local Moran's I test
nb[1]
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

lw$weights[1]

I <- moran(map_data$risk_impact, lw, length(nb), Szero(lw))[1]
I

moran.test(map_data$risk_impact,lw, alternative="greater")
moran.test(map_data$risk_impact,lw, alternative="less")


MC<- moran.mc(map_data$risk_impact, lw, nsim=999, alternative="greater")
#----------------------------------------------------------------------------------
# View results (including p-value)
MC


I <- moran(map_data$risk_likely, lw, length(nb), Szero(lw))[1]
I

moran.test(map_data$risk_likely,lw, alternative="greater")



MC<- moran.mc(map_data$risk_likely, lw, nsim=999, alternative="greater")

#----------------------------------------------------------------------------------
# View results (including p-value)
MC


tm_shape(map_data) + tm_fill(col="risk_per", style="quantile", n=3, palette="Greens") +
  tm_legend(outside=TRUE)



I <- moran(map_data$risk_per, lw, length(nb), Szero(lw))[1]
I

moran.test(map_data$risk_per,lw, alternative="greater")


MC<- moran.mc(map_data$risk_per, lw, nsim=999, alternative="greater")
#----------------------------------------------------------------------------------
# View results (including p-value)
MC
