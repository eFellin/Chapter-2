require(rgeos)
require(maptools)
require(dplyr)
require(sp)
require(sf)
require(ggpubr)
require(plotKML)
require(rasterVis)
require(raster)
require(tidyr)
require(ggplot2)

setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-McGillUniversity/Erica & Virginie_Group - General/Chapter 2/RiskMaps")
load("~/Library/CloudStorage/OneDrive-SharedLibraries-McGillUniversity/Erica & Virginie_Group - General/Chapter 2/RiskMaps/MapRisk.RData")

#### load survey data #####
surveys <- read.csv("survey2.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1",na.strings=c("","NA"))
surveys$out1region #### outdoor work location (region) - duplicate lines when more than one region
### fixing names to match the ones in the geo data
table(surveys$out1region)
surveys[which(surveys$out1region=="LanaudiÃ¨re"),]$out1region <- "Lanaudière"
surveys[which(surveys$out1region=="MontÃ©rÃ©gie"),]$out1region <- "Montérégie"
surveys[which(surveys$out1region=="GaspÃ©sieâ\u0080\u0093Ã\u008eles-de-la-Madeleine"),]$out1region <- "Gaspésie–Îles-de-la-Madeleine"
surveys[which(surveys$out1region=="Capitale Nationale"),]$out1region <- "Capitale-Nationale"
surveys[which(surveys$out1region=="ChaudiÃ¨re-Appalaches"),]$out1region <- "Chaudière-Appalaches"
surveys[which(surveys$out1region=="MontrÃ©al"),]$out1region <- "Montréal"
surveys[which(surveys$out1region=="Monteregie"),]$out1region <- "Montérégie"

table(surveys$out1region) ### 8 regions, so drop the ones that we do not need in geo_aggregated


#### WORK LOCATION #####
#### data from statistiques Canada to get the region polygons #####
geo <- st_read("geo_recensement_2021_QC_harmonise_ISQ_format_SHP/AD_2021_QC_LimCarto_harmo.shp")
geo #### NAD83 UTM
table(geo$NOM_RA)
geo <- geo %>% dplyr::rename(out1region = NOM_RA)  ### rename the field with the region name to match name in survey data
geo <- geo[25]  #### keep only the region name field to extract the centroid coordinates

geo_aggregated <- geo %>%  #### aggregate by region   
  dplyr::group_by(out1region) %>%
  dplyr::summarise()

geo_aggregated  ### sf object with 17 features (regions)
plot(geo_aggregated)  ### don't...takes forever

crsNAD83 <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"   ### NAD83 lat long
geo_aggregated<- st_transform(geo_aggregated, crs=crsNAD83)


#### aggregate risk by region ####
risk_impact=aggregate(Risk.Score ~ out1region, data=surveys, mean, na.rm=TRUE)   #### average impact risk by fsa
risk_likely=aggregate(outlikelihood ~ out1region, data=surveys, mean, na.rm=TRUE)   #### average likelihood risk by fsa
risk_per=aggregate(OUTRS ~ out1region, data=surveys, mean, na.rm=TRUE)   #### average likelihood risk by fsa

risk_impact$Risk.Score <- risk_impact$Risk.Score/5  ### to make the 2 risks on the same scale


map_data <- left_join(geo_aggregated, risk_impact, by = "out1region")
map_data <- left_join(map_data, risk_likely, by = "out1region")
map_data <- left_join(map_data, risk_per, by = "out1region")
names(map_data) <- c("out1region", "geometry", "risk_impact", "risk_likely", "risk_per")
map_data

map_impact <- ggplot() +
  geom_sf(data = map_data, aes(fill = risk_impact)) +
  scale_fill_continuous(low="#FAFAD2",high="red") +
  #scale_x_continuous(limits = c(-80, -62)) +
  #scale_y_continuous(limits = c(44.5, 50)) +
  theme(axis.text = element_text(size = 8, colour = "#333333"), panel.background = element_rect(fill = "white", colour = "grey50")) +
  scale_colour_discrete(na.translate = F) 
#map_impact

map_impact1 <- ggplot() +
  geom_sf(data = map_data, aes(fill = risk_impact)) +
  scale_fill_continuous(low="#FAFAD2",high="red") +
  scale_x_continuous(limits = c(-80, -62)) +
  scale_y_continuous(limits = c(44.5, 50)) +
  theme(axis.text = element_text(size = 8, colour = "#333333"), panel.background = element_rect(fill = "white", colour = "grey50")) +
  scale_colour_discrete(na.translate = F) 



map_likely <- ggplot() +
  geom_sf(data = map_data, aes(fill = risk_likely)) +
  scale_fill_continuous(low="#FAFAD2",high="red") +
  #scale_x_continuous(limits = c(-80, -62)) +
  #scale_y_continuous(limits = c(44.5, 50)) +
  theme(axis.text = element_text(size = 8, colour = "#333333"), panel.background = element_rect(fill = "white", colour = "grey50")) +
  scale_colour_discrete(na.translate = F) 
#map_likely

map_likely1 <- ggplot() +
  geom_sf(data = map_data, aes(fill = risk_likely)) +
  scale_fill_continuous(low="#FAFAD2",high="red") +
  scale_x_continuous(limits = c(-80, -62)) +
  scale_y_continuous(limits = c(44.5, 50)) +
  theme(axis.text = element_text(size = 8, colour = "#333333"), panel.background = element_rect(fill = "white", colour = "grey50")) +
  scale_colour_discrete(na.translate = F) 
#map_likely

pdf(file="map_impact_likely.pdf")
map_impact_likely <- ggarrange(map_impact,map_likely + rremove("x.text"),
          labels = c("Impact", "Likelihood"),
          ncol = 1, nrow = 2)
map_impact_likely
dev.off()

summary(map_data$risk_impact)
length(map_data$risk_impact)
length(which(!is.na(map_data$risk_impact)))  ### 12

summary(map_data$risk_likely)
length(map_data$risk_likely)
length(which(!is.na(map_data$risk_likely)))  ### 12

#### overall correlation between the two risks #####
cor.test(map_data$risk_impact,map_data$risk_likely, test = T) #####   R = 0.21, p = 0.49
pearson <- (cor.test(map_data$risk_impact,map_data$risk_likely))$estimate
p.value <- (cor.test(map_data$risk_impact,map_data$risk_likely))$p.value

bbox <- extent(map_data)
res <- res(raster(map_data))/10
raster_template <- raster(bbox, res=res, crs=crsNAD83)

impact_raster <- rasterize(map_data, raster_template, field = "risk_impact")
likely_raster <- rasterize(map_data, raster_template, field = "risk_likely")

length(impact_raster@data@values)
summary(impact_raster@data@values)
length(which(!is.na(impact_raster@data@values))) #### 623

length(likely_raster@data@values)
summary(likely_raster@data@values)
length(which(!is.na(likely_raster@data@values))) #### 623

#### calculate correlation between the two risks #####
correlation <- corLocal(impact_raster, likely_raster, nbg = 10, na.omit = TRUE, test=TRUE )  ### local correlation, changing nbg value did not matter
correlation
length(correlation@data@values)
summary(correlation@data@values)
length(which(!is.na(correlation@data@values))) #### 852

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

pdf(file="map_correlation_sign.pdf")
map_correlation_sign <- ggplot() +
  geom_sf(data = map_correlation_sign_sf, aes(fill = pearson)) +
  scale_fill_continuous(low="#FAFAD2",high="red") +
  #scale_x_continuous(limits = c(-80, -62)) +
  #scale_y_continuous(limits = c(44.5, 50)) +
  theme(axis.text = element_text(size = 8, colour = "#333333"), panel.background = element_rect(fill = "white", colour = "grey50")) +
  scale_colour_discrete(na.translate = F) 
dev.off()

map_correlation_sign  #### This map shows only significant Pearson values

centroid_coords <- st_centroid(map_data)
centroid_coords_df <- data.frame(cbind(centroid_coords$out1region, st_coordinates(centroid_coords)))
names(centroid_coords_df) <- c("out1region", "Centroid_X", "Centroid_Y")
centroid_coords_df

surveys <- left_join(surveys, centroid_coords_df, by = "out1region")   ### add centroid coordinates to survey data
surveys  ### 114 observations
table(surveys$out1region)
length(which(is.na(surveys$out1region))) ### 20 NAs

head(surveys)  #### all data in the df, ready to use for next analyses


save.image("~/Library/CloudStorage/OneDrive-SharedLibraries-McGillUniversity/Erica & Virginie_Group - General/Chapter 2/RiskMaps/MapRisk.RData")



### old stuff, when working with zip codes #####
#### load map with Zip codes #####
#fsas <- maptools::readShapeSpatial("gfsa000b11a_e/gfsa000b11a_e.shp")
#fsas <- st_read("gfsa000b11a_e/gfsa000b11a_e.shp")
#names(fsas)[1] <- "fsa"
#QC.fsas <- fsas %>% filter(startsWith(as.character(PRNAME), 'Q'))  ##### select Quebec


#### fixing the Work Location column
#table(surveys$worklocation)
#surveys$worklocation[surveys$worklocation=="Ange-Gardien (Outaouais)"] <- "G0A"
#surveys$worklocation[surveys$worklocation=="Chelsea"] <- "J9B"
#surveys$worklocation[surveys$worklocation=="GaspÃ©"] <- "G4X"
#surveys$worklocation[surveys$worklocation=="Mont Saint Hilaire"] <- "J3G"
#surveys$worklocation[surveys$worklocation=="Shefford"] <- "J2M"

#### copying work location into fsa column
#surveys$fsa <- surveys$worklocation
#table(surveys$fsa)
#surveys$fsa[surveys$fsa=="J0J1C0"] <- "J0J"   ### delete last three digits
#surveys$fsa[surveys$fsa=="J2H 0Y6"] <- "J2H"
#surveys$fsa[surveys$fsa=="J3G 4S6"] <- "J3G"
#surveys$fsa[surveys$fsa=="J8Y 4S1"] <- "J8Y"
#surveys$fsa[surveys$fsa=="J2G 3V3"] <- "J2G"
#surveys$fsa[surveys$fsa=="J3G 0A4"] <- "J3G"
#surveys$fsa[surveys$fsa=="j3g4s6"] <- "J3G"
#surveys$fsa[surveys$fsa=="J3H4E3 "] <- "J3H"
#surveys$fsa[surveys$fsa=="H7E 5G8"] <- "H7E"
#surveys$fsa[surveys$fsa=="J0E 2K0"] <- "J0E"
#surveys$fsa[surveys$fsa=="J0X 2A0"] <- "J0X"
#surveys$fsa[surveys$fsa=="J1N 1H4"] <- "J1N"
#surveys$fsa[surveys$fsa=="J2G 5P3"] <- "J2G"
#surveys$fsa[surveys$fsa=="J3g 4s6"] <- "J3G"

#surveys$fsa[surveys$fsa=="J2n"] <- "J2N"  #### fix typos
#surveys$fsa[surveys$fsa=="J3h"] <- "J3H"
#surveys$fsa[surveys$fsa=="j2m"] <- "J2M"
#surveys$fsa[surveys$fsa=="j3g"] <- "J3G"
#surveys$fsa[surveys$fsa=="j4k"] <- "J4K"
#surveys$fsa[surveys$fsa=="J6e"] <- "J6E"

#surveys$fsa[surveys$fsa=="Eastern Townships "] <- "NA"   ### replace with NA when too wide area
#surveys$fsa[surveys$fsa=="J4B 6K2 (tÃ©lÃ©travail...)"] <- "NA" 
#surveys$fsa[surveys$fsa=="MontrÃ©al"] <- "NA" 
#surveys$fsa[surveys$fsa=="Estrie"] <- "NA" 
#surveys$fsa[surveys$fsa=="MontÃ©rÃ©gie"] <- "NA" 

#table(surveys$fsa)


#### aggregate risk by fsa (first three digits of zip codes) ####
#risk_total=aggregate(OUTRS ~ fsa, data=surveys, mean, na.rm=TRUE)   #### average total risk by fsa
#risk_impact=aggregate(Risk.Score ~ fsa, data=surveys, mean, na.rm=TRUE)   #### average impact risk by fsa
#risk_likely=aggregate(outlikelihood ~ fsa, data=surveys, mean, na.rm=TRUE)   #### average likelihood risk by fsa

#risk_impact$Risk.Score <- risk_impact$Risk.Score/5  ### to make the 2 risks on the same scale

