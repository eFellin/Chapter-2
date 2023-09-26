#testing predictors for spatial autocorrelation across regions

#using base code from 02 Geo and spatial autocorrelation (up to line 51)
#using variable formatting from 03 model selection (up to line 39)
#----------------------------------------------------------------------------------

####Sex
#### aggregate variable by region ####
sexf=aggregate(sexf ~ RegionOFF, data=survey2023_INSPQ, mean, na.rm=TRUE) 
#join the data together for easier comparison
map_data <- left_join(geo_aggregated, sexf, by = "RegionOFF")
map_data <- left_join(map_data, sexf, by = "RegionOFF")
names(map_data) <- c("RegionOFF", "geometry", "sexf")

#Moran I test - have to remove NAs first
map_data <- map_data[!is.na(map_data$sexf) ,]

#neighbour list
nb <- poly2nb(map_data, queen=TRUE) #getting surrounding neighbours of polygons 
nb[1]

# Create a spatial weights matrix
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[1]

moran.test(map_data$sexf,lw, alternative="greater")
moran.test(map_data$sexf,lw, alternative="less")


MC<- moran.mc(map_data$sexf, lw, nsim=999, alternative="greater")
MC # View results (including p-value)

I <- moran(map_data$sexf, lw, length(nb), Szero(lw))[1]
I

#no spatial autocorrelation


#----------------------------------------------------------------------------------
####Age
#### aggregate variable by region ####
agef=aggregate(agef ~ RegionOFF, data=survey2023_INSPQ, mean, na.rm=TRUE) 
#join the data together for easier comparison
map_data <- left_join(geo_aggregated, agef, by = "RegionOFF")
map_data <- left_join(map_data, agef, by = "RegionOFF")
names(map_data) <- c("RegionOFF", "geometry", "agef")

#Moran I test - have to remove NAs first
map_data <- map_data[!is.na(map_data$agef) ,]

#neighbour list
nb <- poly2nb(map_data, queen=TRUE) #getting surrounding neighbours of polygons 
nb[1]

# Create a spatial weights matrix
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[1]

moran.test(map_data$agef,lw, alternative="greater")
moran.test(map_data$agef,lw, alternative="less")


MC<- moran.mc(map_data$agef, lw, nsim=999, alternative="greater")
MC # View results (including p-value)

I <- moran(map_data$agef, lw, length(nb), Szero(lw))[1]
I

#----------------------------------------------------------------------------------
####Education
#### aggregate variable by region ####
eduf=aggregate(eduf ~ RegionOFF, data=survey2023_INSPQ, mean, na.rm=TRUE) 
#join the data together for easier comparison
map_data <- left_join(geo_aggregated, eduf, by = "RegionOFF")
map_data <- left_join(map_data, eduf, by = "RegionOFF")
names(map_data) <- c("RegionOFF", "geometry", "eduf")

#Moran I test - have to remove NAs first
map_data <- map_data[!is.na(map_data$eduf) ,]

#neighbour list
nb <- poly2nb(map_data, queen=TRUE) #getting surrounding neighbours of polygons 
nb[1]

# Create a spatial weights matrix
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[1]

moran.test(map_data$eduf,lw, alternative="greater")
moran.test(map_data$eduf,lw, alternative="less")


MC<- moran.mc(map_data$eduf, lw, nsim=999, alternative="greater")
MC # View results (including p-value)

I <- moran(map_data$eduf, lw, length(nb), Szero(lw))[1]
I

#no spatial autocorrelation


#----------------------------------------------------------------------------------
####Industry
#### aggregate variable by region ####
indf=aggregate(indf ~ RegionOFF, data=survey2023_INSPQ, mean, na.rm=TRUE) 
#join the data together for easier comparison
map_data <- left_join(geo_aggregated, indf, by = "RegionOFF")
map_data <- left_join(map_data, indf, by = "RegionOFF")
names(map_data) <- c("RegionOFF", "geometry", "indf")

#Moran I test - have to remove NAs first
map_data <- map_data[!is.na(map_data$indf) ,]

#neighbour list
nb <- poly2nb(map_data, queen=TRUE) #getting surrounding neighbours of polygons 
nb[1]

# Create a spatial weights matrix
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[1]

moran.test(map_data$indf,lw, alternative="greater")
moran.test(map_data$indf,lw, alternative="less")


MC<- moran.mc(map_data$indf, lw, nsim=999, alternative="greater")
MC # View results (including p-value)

I <- moran(map_data$indf, lw, length(nb), Szero(lw))[1]
I

#no spatial autocorrelation