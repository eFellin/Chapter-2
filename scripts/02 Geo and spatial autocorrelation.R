#load packages

library(rgeos)
library(maptools)
library(dplyr)
library(sp)
library(sf)
library(ggpubr)
library(plotKML)
library(rasterVis)
library(raster)
library(tidyr)
library(ggplot2)
library(spdep)
library(readr)

#----------------------------------------------------------------------------------
#data
surveys <- read.csv("data/survey2023 INSPQ.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1",na.strings=c("","NA"))

#----------------------------------------------------------------------------------
#update names of regions to not be messy
table(surveys$RegionOFF)
surveys[which(surveys$RegionOFF=="LanaudiÃ¨re"),]$RegionOFF <- "Lanaudière"
surveys[which(surveys$RegionOFF=="MontÃ©rÃ©gie"),]$RegionOFF <- "Montérégie"
surveys[which(surveys$RegionOFF=="GaspÃ©sieâ\u0080\u0093Ã\u008eles-de-la-Madeleine"),]$RegionOFF <- "Gaspésie–Îles-de-la-Madeleine"
surveys[which(surveys$RegionOFF=="Capitale Nationale"),]$RegionOFF <- "Capitale-Nationale"
surveys[which(surveys$RegionOFF=="ChaudiÃ¨re-Appalaches"),]$RegionOFF <- "Chaudière-Appalaches"
surveys[which(surveys$RegionOFF=="MontrÃ©al"),]$RegionOFF <- "Montréal"
surveys[which(surveys$RegionOFF=="Saguenayâ\u0080\u0093Lac-Saint-Jean"),]$RegionOFF <- "Saguenay–Lac-Saint-Jean"
surveys[which(surveys$RegionOFF=="Abitibi-TÃ©miscamingue"),]$RegionOFF <- "Abitibi-Témiscamingue"
surveys[which(surveys$RegionOFF=="Mauricie et Centre-du-QuÃ©bec"),]$RegionOFF <- "Centre-du-Québec"

table(surveys$RegionOFF) ### 14 regions plus "multiple"
#----------------------------------------------------------------------------------
#create spatial data for spatial visual and correlation
geo <- st_read("data/geo_recensement_2021_QC_harmonise_ISQ_format_SHP/AD_2021_QC_LimCarto_harmo.shp")
geo #### NAD83 UTM
table(geo$NOM_RA)
geo <- geo %>% dplyr::rename(RegionOFF = NOM_RA)  ### rename the field with the region name to match name in survey data
geo <- geo[25]  #### keep only the region name field to extract the centroid coordinates

geo_aggregated <- geo %>%  #### aggregate by region   
  dplyr::group_by(RegionOFF) %>%
  dplyr::summarise()

geo_aggregated  ### sf object with 17 features (regions)
#plot(geo_aggregated)

crsNAD83 <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"   ### NAD83 lat long projection
geo_aggregated<- st_transform(geo_aggregated, crs=crsNAD83)

#### aggregate risk by region ####
risk_likely=aggregate(INSPQriskOFF ~ RegionOFF, data=surveys, mean, na.rm=TRUE)   #### average likelihood risk by fsa
risk_impact=aggregate(RiskScore ~ RegionOFF, data=surveys, mean, na.rm=TRUE)   #### average likelihood risk by fsa

#scale scores to be same scale
risk_impact$RiskScore <- risk_impact$RiskScore-2
risk_impact$RiskScore <- risk_impact$RiskScore/25  ### to make the 2 risks on the same scale one is from 2-27, the other 1-3, subtract 27-2, 3-1, divide by remainder
risk_likely$INSPQriskOFF <- risk_likely$INSPQriskOFF-1
risk_likely$INSPQriskOFF <- risk_likely$INSPQriskOFF/2

#join the data together for easier comparison
map_data <- left_join(geo_aggregated, risk_impact, by = "RegionOFF")
map_data <- left_join(map_data, risk_likely, by = "RegionOFF")
names(map_data) <- c("RegionOFF", "geometry", "risk_impact", "risk_likely")
map_data

#----------------------------------------------------------------------------------
#visuals without scale limit
map_impact <- ggplot() +
  geom_sf(data = map_data, aes(fill = risk_impact)) +
  scale_fill_continuous(low="#FAFAD2",high="red") +
  #scale_x_continuous(limits = c(-80, -62)) +
  #scale_y_continuous(limits = c(44.5, 50)) +
  theme(axis.text = element_text(size = 8, colour = "#333333"), panel.background = element_rect(fill = "white", colour = "grey50")) +
  scale_colour_discrete(na.translate = F) 
#map_impact

map_likely <- ggplot() +
  geom_sf(data = map_data, aes(fill = risk_likely)) +
  scale_fill_continuous(low="#FAFAD2",high="red") +
  #scale_x_continuous(limits = c(-80, -62)) +
  #scale_y_continuous(limits = c(44.5, 50)) +
  theme(axis.text = element_text(size = 8, colour = "#333333"), panel.background = element_rect(fill = "white", colour = "grey50")) +
  scale_colour_discrete(na.translate = F) 
#map_likely

#----------------------------------------------------------------------------------
#summary of data so far
summary(map_data$risk_impact)
length(map_data$risk_impact)
length(which(!is.na(map_data$risk_impact)))  ### 14

summary(map_data$risk_likely)
length(map_data$risk_likely)
length(which(!is.na(map_data$risk_likely)))  ### 14
#----------------------------------------------------------------------------------
#### overall correlation between the two risks #####
cor.test(map_data$risk_impact,map_data$risk_likely, test = T) #####   R = 0.505, p = 0.065
pearson <- (cor.test(map_data$risk_impact,map_data$risk_likely))$estimate
p.value <- (cor.test(map_data$risk_impact,map_data$risk_likely))$p.value

bbox <- extent(map_data)
res <- res(raster(map_data))/10
raster_template <- raster(bbox, res=res, crs=crsNAD83)

impact_raster <- rasterize(map_data, raster_template, field = "risk_impact")
likely_raster <- rasterize(map_data, raster_template, field = "risk_likely")

length(impact_raster@data@values)
summary(impact_raster@data@values)
length(which(!is.na(impact_raster@data@values))) #### 1060

length(likely_raster@data@values)
summary(likely_raster@data@values)
length(which(!is.na(likely_raster@data@values))) #### 1060

#### calculate correlation between the two risks #####
correlation <- corLocal(impact_raster, likely_raster, nbg = 10, na.omit = TRUE, test=TRUE )  ### local correlation, changing nbg value did not matter
correlation
length(correlation@data@values)
summary(correlation@data@values)
length(which(!is.na(correlation@data@values))) #### 978

correlation_pearson <- correlation[[1]]   #### extract layer from raster brick
correlation_pvalue <- correlation[[2]]
summary(correlation_pearson@data@values)
summary(correlation_pvalue@data@values)

correlation_pearson_sf <- st_as_sf(data.frame(rasterToPoints(correlation_pearson)), coords = c("x", "y"), crs = crsNAD83)
correlation_pvalue_sf <- st_as_sf(data.frame(rasterToPoints(correlation_pvalue)), coords = c("x", "y"), crs = crsNAD83)

correlation_sf <- st_join(correlation_pearson_sf,correlation_pvalue_sf)  ### join the two sf objects
summary(correlation_sf$pearson)
length(which(!is.na(correlation_sf$pearson))) ### 426
length(which(is.na(correlation_sf$pearson))) #### no NAs
hist(correlation_sf$pearson)  ### clusters at -1 and at +1

#plot(correlation_sf)

st_crs(correlation_sf)   #### +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs 
st_crs(map_data) #### +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs 

#sf::sf_use_s2(FALSE)  needed to run this because of some errors with "non spherical geometries"
map_correlation_sf <- st_join(map_data, correlation_sf)

pdf(file="map_correlation.pdf")
map_correlation <- ggplot() +
  geom_sf(data = map_correlation_sf, aes(fill = pearson)) +
  scale_fill_continuous(low="#FAFAD2",high="red") +
  #scale_x_continuous(limits = c(-80, -62)) +
  #scale_y_continuous(limits = c(44.5, 50)) +
  theme(axis.text = element_text(size = 8, colour = "#333333"), panel.background = element_rect(fill = "white", colour = "grey50")) +
  scale_colour_discrete(na.translate = F) 
dev.off()
map_correlation  #### This map shows all Pearson values (including non-significant)

map_correlation_sign_sf <- map_correlation_sf[which(map_correlation_sf$p.value<0.05),]  ### 329 features out of the 374 original ones (43 pearson corr. coef are not significant)


#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#Moran I test - have to remove NAs first
map_data <- map_data[!is.na(map_data$risk_impact) ,]

#----------------------------------------------------------------------------------

nb <- poly2nb(map_data, queen=TRUE) #queen vs rook local Moran's I test
nb[1]
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

lw$weights[1]

#behaviour score

I <- moran(map_data$risk_impact, lw, length(nb), Szero(lw))[1]
I

moran.test(map_data$risk_impact,lw, alternative="greater")
moran.test(map_data$risk_impact,lw, alternative="less")


MC<- moran.mc(map_data$risk_impact, lw, nsim=999, alternative="greater")
MC # View results (including p-value)
#----------------------------------------------------------------------------------
#INSPQ risk score

I <- moran(map_data$risk_likely, lw, length(nb), Szero(lw))[1]
I

moran.test(map_data$risk_likely,lw, alternative="greater")



MC<- moran.mc(map_data$risk_likely, lw, nsim=999, alternative="greater")
MC
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------