#Mathieu Varin 02-2019 #projet 19-0830
#test de l'indice de Moran pour verifier l'autocorrelation spatiale des echantillons
#hypothese nulle : il n'y a pas de grappes spatiales (agrégation) des valeurs associees avec leur distribution; la distribution est aleatoire
#alpha = .05

#installation et lecture des packages
install.packages("sp")
install.packages("spdep")
install.packages("rgdal")
install.packages("tmap")
install.packages("ape")
install.packages("BBmisc")
install.packages("maptools")
install.packages("ncf")
install.packages("mclust")
install.packages("caret")

#lecture des packages

Packages <- c("caret","ncf", "sp", "rgdal", "raster","tmap","ape","BBmisc","stats","maptools","spdep","mclust")

lapply(Packages, library, character.only = TRUE)

##preparation des polygones shapefile pour l'autocorrelation spatiale et la selection validation-entraintement
#definition du fichier ROI de polygones
chemin_shapef = "M:/2019/19-0830-AUTRE-MFFP-Carto Ess/Donnees/Analyse_stat/R/Donnees_geospatiales"
nom_shapef = "ROI"
ID_ROI = "ROIs_ID"
Ess = "ESS_PUR"
#Pour information, la projection MTM 9 a un CRS de 32189, pour l'utiliser, on prendra la variable proj = "+init=espg:32189"


#lecture des polygones dans R
polygones <- readOGR(dsn=chemin_shapef,layer=nom_shapef)

#verification des polygones lus
summary(polygones@data)
head(polygones@data)

##Autocorrelation spatiale de tous les polygones sans egard a la classe
latlong <- coordinates(polygones)

inf.dists <- as.matrix(dist(cbind(lon=latlong[,1], lat=latlong[,2])))
inf.dists.inv <- 1/inf.dists
diag(inf.dists.inv) <- 0

listew <- mat2listw(inf.dists.inv)
moran <- moran.test(polygones$Shape_Area,listew)
moran.plot(polygones$Shape_Area,listew)

#message de recommandation
if (moran$estimate[1]>0) {
  print(paste0("Une valeur de Moran positive: ",round(moran$estimate[1],3)," indique une presence d'agregation"))
} else if (moran$estimate<0) {
  print(paste0("Une valeur de Moran negative: ",round(moran$estimate[1],3)," indique une presence de dispersion"))
} else {
  print(paste0("Une valeur de Moran proche de 0: ",round(moran$estimate[1],3)," indique une distribution aleatoire parfaite"))
}

#si pvalue < 0,05 : difference entre observ et expect donc on rejette H0 = on ne peut pas dire que c'est aleatoirement ditribue
if (moran$p.value<0.05) {
  print(paste0("mais le p-value ", round(moran$p.value,2), " est inferieur a 0.05 permet de rejeter H0; on ne peut pas dire que la distribution est aleatoire. Il est recommande d'ajouter des ROI ou il y a absence de polygones"))
} else if (moran$p.value>0.05) {
  print(paste0("mais le p-value ", round(moran$p.value,2), " est superieur a 0.05 permet d'accepter H0; la distribution est aleatoire. Le jeu de donnees peut etre utilise tel quel"))
} 


###############################################
###############################################
###############################################

##sortir la valeur moyenne par ESS_PUR de la distance minimale des voisins
distance_matrix <- as.data.frame(inf.dists)
for (i in 1:length(distance_matrix)){
  distance_matrix[i,i] <- 100000000
}

distance_matrix$min <- apply(distance_matrix,1,min)
table_dist_ess <- data.frame(matrix(nrow=length(distance_matrix$min)))
table_dist_ess <- cbind.data.frame(dist=distance_matrix$min,ess=polygones$ESS_PUR)
liste_ess <- list(table_dist_ess$ess)
table_dist_moy <- stats::aggregate(table_dist_ess$dist,by=liste_ess,FUN=mean)
table_dist_std <- stats::aggregate(table_dist_ess$dist,by=liste_ess,FUN=sd)

##Autocorrelation spatiale de tous les polygones par classe

#Normaliser les valeurs de superficies
tm_shape(polygones) + tm_polygons(style="cat", col = Ess) +
  tm_legend(outside = TRUE, text.size = .8) 

polygones$Shape_Area <- round(normalize(polygones$Shape_Area,method="range",range=c(1,10)),2)

summary(polygones$Shape_Area)

##Boucle d'autocorrelation
nb_ess=length(levels(polygones$ESS_PUR))
table_moran_ess <- data.frame(matrix(ncol=3,nrow=0))
par(mfrow=c(2,2))

for (i in levels(polygones$ESS_PUR)){
  poly_select <- polygones[polygones$ESS_PUR==i,]
  latlong <- coordinates(poly_select)
  
  inf.dists <- as.matrix(dist(cbind(lon=latlong[,1], lat=latlong[,2])))
  inf.dists.inv <- 1/inf.dists
  diag(inf.dists.inv) <- 0
  listew <- mat2listw(inf.dists.inv)
  
  table_moran_ess[i,2] <- moran.test(poly_select$Shape_Area,listew)$estimate[1]
  table_moran_ess[i,3] <- moran.test(poly_select$Shape_Area,listew)$p.value
  table_moran_ess[i,1] <- i
  
  #ce graphique permet de voir la distance (lag) entre les polygones en fonction de leur superficie par ESS_PUR
  #la pente de la courbe correspond a l'indice de Moran
  #les lignes pointilles divise le graphique en 4 type d'autocorrelation spatiale (haut-droit et bas-gauche: correlation positive), le centre est la moyenne
  moran.plot(poly_select$Shape_Area,listew,main=i,xlab="Area",ylab="Spatially lagged polygon")
  
  #regarder la correlation spatiale visuellement selon la distance des voisins et la superficie des polygones
  graph_corr <- spline.correlog(x=latlong[,1], y=latlong[,2], z=poly_select$Shape_Area, resamp=100, quiet=TRUE)
  plot(graph_corr, main=i)
}

##Preparation du tableau final de Moran

#Ajout des noms de colonnes
names(table_moran_ess) <- c("ess_pure","observed","pvalue")

#Assignation de la valeur pour la colonne de distribution aleatoire ; aleatoire ou non
table_moran_ess[table_moran_ess$pvalue>=0.05,"dist_aleatoire"] <- "oui"
table_moran_ess[!table_moran_ess$pvalue>=0.05,"dist_aleatoire"] <- "non"

#Assignation de la valeur pour la colonne de distribution ; agregee ou dispersee
table_moran_ess[table_moran_ess$observed>0,"distribution"] <- "Agrege"
table_moran_ess[table_moran_ess$observed<0,"distribution"] <- "Disperse"
table_moran_ess[table_moran_ess$observed==0,"distribution"] <- "Aleatoire parfait"
table_moran_ess

#Ajouter les moyennes de distance par ESS_PUR dans le tableau
table_moran_ess$dist_moy <- table_dist_moy$x
table_moran_ess$ecart_type <- table_dist_std$x
table_moran_ess$nb_polygones <- summary(polygones@data$ESS_PUR)
table_moran_ess[,c(1,8,2,3,5,4,6,7)]

#Creation des talbeaux pour les groupes de polygones en fonction des classes et de leur distribution
attach(table_moran_ess)
table_ess_aleatoire <- table_moran_ess[(dist_aleatoire=="oui"&distribution=="Agrege")|(dist_aleatoire=="oui"&distribution=="Disperse")|(dist_aleatoire=="oui"&distribution=="Aleatoire parfait"),1]
table_ess_aleatoire
table_ess_stratifie <- table_moran_ess[(dist_aleatoire=="non"&distribution=="Agrege")|(dist_aleatoire=="non"&distribution=="Disperse"),1]
table_ess_stratifie
detach(table_moran_ess)


###############################################
###############################################
###############################################

##selection aleatoire des polygones par classe selectionne dans table_ess_aleatoire

total_aleatoire <- as.data.frame(polygones[polygones$ESS_PUR%in%table_ess_aleatoire,])
total_aleatoire$ESS_PUR <- factor(total_aleatoire$ESS_PUR)

reference_index_aleatoire <-  createDataPartition(total_aleatoire$ESS_PUR,p=0.8,list=FALSE,times=1)

reference_aleatoire <- total_aleatoire [reference_index_aleatoire,1:2]


validation_aleatoire <- total_aleatoire [-reference_index_aleatoire,1:2]

dim(reference_aleatoire)
dim(validation_aleatoire)

##selection stratifiee des polygones par classe selectionne dans table_ess_stratifie

#boucle pour creer les clusters par ESS_PURs voulues (table_ess_stratifie)
par(mfrow=c(2,2))
total <- as.data.frame(matrix(ncol=2, nrow = 0))

for (i in table_ess_stratifie){
  poly_select <- polygones[polygones$ESS_PUR==i,]
  latlong <- coordinates(poly_select)
    
    #cbind(as.data.frame(poly_select),coordinates(poly_select))
 #colnames(latlong) <- c("ESS_PUR", "ROIs_ID", "Shape_Leng", "Shape_Area", "X", "Y")

  #Elbow méthode pour voir les k en fonction de la variance
  set.seed(123)
  #Calculer et plot wss pour k = 1 a kmax, qui est le nombre d'echantillons. Utiliser 20 echantillons pour les 15 iterations
  k.max <- length(poly_select)-1
  wss <- sapply(1:k.max, 
                function(k){kmeans(latlong, k, nstart=20,iter.max = 15 )$tot.withinss})
  wss
  
  #graphique montrant le nombre de k avec le ratio qui varie
  plot(1:k.max, wss,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares",main=i)  
  
  #"Bayesian Inference Criterion for k means" - choix de méthodes pour trouver le k max et optimal, visuellement
  #Trouve le nombre max de k
  d_clust <- Mclust(as.matrix(latlong), G=1:10, 
                    modelNames = mclust.options("emModelNames"))
  
  d_clust$BIC
  print("Selectionner 0 pour sortir du choix")
  plot(d_clust,main=i)

  #k optimal en choisissant la methode de classification pour separer les echantillons
  k_optimal <- max(d_clust$classification)
  print (paste0("La valeur maximale du k est de ", k_optimal," clusters selon une methode de classification pour la classe ",i))
  
  #clustering avec le k optimal
  #faire une boucle pour s'assurer qu'aucun cluster n'ait moins de 3 echantillons
  kmm = kmeans(latlong,k_optimal,nstart = 20,iter.max = 15) #we keep number of iter.max=15 to ensure the algorithm converges and nstart=50 to #ensure that atleat 50 random sets are choosen  

  while(prod(table(kmm$cluster)>3)==0){
    k_optimal <- k_optimal-1
    kmm = kmeans(latlong,k_optimal,nstart = 20,iter.max = 15)
   
  }
  
  #print un nouveau graphique en fonction du k optimal et affiche les echantillons
  print (paste0("La valeur optimale du k est de ", k_optimal," clusters selon une methode de classification pour la classe ",i))
  print("selectionner le nouveau graphique avec le k optimal")
  d_clust <- Mclust(as.matrix(latlong), G=k_optimal, 
                    modelNames = mclust.options("emModelNames"))
  
  plot(d_clust,main=i)
  
  plot(latlong[,1], latlong[,2], col = kmm$cluster, pch = 20,main=i) #"between_SS/total_SS" ratio actually accounts for the amount of total sum of squares of the data points which is between the clusters
  
  table_strat_finale <- data.frame(matrix(ncol=0,nrow=length(kmm$cluster)))
  table_strat_finale$ESS_PUR <- i
  table_strat_finale$ROIs_ID <- poly_select$ROIs_ID
  table_strat_finale$cluster <- kmm$cluster
 
  
  #populer un tableau avec les clusters pour toutes les ESS_PURs voulues
  total <- rbind(total,table_strat_finale)
}  

##selection de 20% de ROI dans total par cluster

#faire boucle par ESS_PUR

reference_stratifiee <- as.data.frame(matrix(ncol=3, nrow = 0))
validation_stratifiee <- as.data.frame(matrix(ncol=3, nrow = 0))
for (i in unique(total$ESS_PUR)){
  set.seed(10)
  reference_index_stratifiee <-  createDataPartition(as.factor(total[total$ESS_PUR==i,"cluster"]),p=0.8,list=FALSE,times=1)
  subset_Ess <- total[total$ESS_PUR==i,]
  reference_Ess_stratifiee <- subset_Ess[reference_index_stratifiee,]
  reference_stratifiee <- rbind(reference_stratifiee,reference_Ess_stratifiee)
  valid_Ess_stratifiee <- subset_Ess[-reference_index_stratifiee,]
  validation_stratifiee <- rbind(validation_stratifiee,valid_Ess_stratifiee)
}

validation_stratifiee <- validation_stratifiee [,-3]
reference_stratifiee <- reference_stratifiee [,-3]

##combiner les deux tableaux (selection aleatoire et stratifiee)

reference <- rbind(reference_aleatoire,reference_stratifiee )
validation <- rbind(validation_aleatoire,validation_stratifiee)


##references
#https://www.r-bloggers.com/finding-optimal-number-of-clusters/
#https://mgimond.github.io/Spatial/spatial-autocorrelation-in-r.html
#http://resources.esri.com/help/9.3/arcgisengine/java/gp_toolref/spatial_statistics_tools/how_spatial_autocorrelation_colon_moran_s_i_spatial_statistics_works.htm
