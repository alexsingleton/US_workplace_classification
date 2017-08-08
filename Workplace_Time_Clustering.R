#Set Working Directory and Options
setwd("~/US_workplace_classification")
options(scipen=999)

#Load Packages
packages <- c("fmsb","radarchart","R.utils","data.table","RCurl","bit64","maptools","rgdal","rgeos","cluster","plyr")
for (package in packages){
  if(paste(package) %in% rownames(installed.packages()) == FALSE) {install.packages(paste(package))}
  library(paste(package),character.only=TRUE)
}


#Load required data

load("./Census_Files/MSA_WRK_CENSUS.Rdata")


################################################################################
# Clustering using H2O
################################################################################


# H20 Setup...
#h2o.shutdown()

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("statmod","RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-vajda/4/R")

# Finally, let's load H2O and start up an H2O cluster
library(h2o)

h2o.init(max_mem_size = "10g",nthreads = -1)


#Convert dataframe to H20 dataframe

limit <- 40



MSA_WRK_2004.h2o <- as.h2o(MSA_WRK_2004[TC > limit], destination_frame="MSA_WRK_2004.h2o")
MSA_WRK_2005.h2o <- as.h2o(MSA_WRK_2005[TC > limit], destination_frame="MSA_WRK_2005.h2o")
MSA_WRK_2006.h2o <- as.h2o(MSA_WRK_2006[TC > limit], destination_frame="MSA_WRK_2006.h2o")
MSA_WRK_2007.h2o <- as.h2o(MSA_WRK_2007[TC > limit], destination_frame="MSA_WRK_2007.h2o")
MSA_WRK_2008.h2o <- as.h2o(MSA_WRK_2008[TC > limit], destination_frame="MSA_WRK_2008.h2o")
MSA_WRK_2009.h2o <- as.h2o(MSA_WRK_2009[TC > limit], destination_frame="MSA_WRK_2009.h2o")
MSA_WRK_2010.h2o <- as.h2o(MSA_WRK_2010[TC > limit], destination_frame="MSA_WRK_2010.h2o")
MSA_WRK_2011.h2o <- as.h2o(MSA_WRK_2011[TC > limit], destination_frame="MSA_WRK_2011.h2o")
MSA_WRK_2012.h2o <- as.h2o(MSA_WRK_2012[TC > limit], destination_frame="MSA_WRK_2012.h2o")
MSA_WRK_2013.h2o <- as.h2o(MSA_WRK_2013[TC > limit], destination_frame="MSA_WRK_2013.h2o")
MSA_WRK_2014.h2o <- as.h2o(MSA_WRK_2014[TC > limit], destination_frame="MSA_WRK_2014.h2o")


#Column list
i_cols <- c("CA01","CA02","CA03","CE01","CE02","CE03","CNS01", "CNS02", "CNS03", "CNS04", "CNS05", "CNS06", "CNS07", "CNS08", "CNS09","CNS10","CNS11","CNS12","CNS13","CNS14","CNS15","CNS16","CNS17","CNS18","CNS19","CNS20")


######################
#Scree Plot Function
######################

cluster_plot <- function(min,max,df){
  wss <- list()
  tmp <- NA
  for (k in min:max){
    print(paste("Starting",k,"clusters"))
    for (i in 1:100) {
      tmp[i] <- h2o.kmeans(training_frame = df, k = k, x = i_cols,max_iterations=1000,standardize=FALSE,init="Random")@model$model_summary["within_cluster_sum_of_squares"]
    }
    wss <- rbind(wss,c(mean(unlist(tmp)),median(unlist(tmp)),min(mean(unlist(tmp))),max(unlist(tmp))))
  }
  return(wss)
}


wss_2014 <- cluster_plot(2,15,MSA_WRK_2014.h2o)
wss_2013 <- cluster_plot(2,15,MSA_WRK_2013.h2o)
wss_2012 <- cluster_plot(2,15,MSA_WRK_2012.h2o)
wss_2011 <- cluster_plot(2,15,MSA_WRK_2011.h2o)
wss_2010 <- cluster_plot(2,15,MSA_WRK_2010.h2o)
wss_2009 <- cluster_plot(2,15,MSA_WRK_2009.h2o)
wss_2008 <- cluster_plot(2,15,MSA_WRK_2008.h2o)
wss_2007 <- cluster_plot(2,15,MSA_WRK_2007.h2o)
wss_2006 <- cluster_plot(2,15,MSA_WRK_2006.h2o)
wss_2005 <- cluster_plot(2,15,MSA_WRK_2005.h2o)
wss_2004 <- cluster_plot(2,15,MSA_WRK_2004.h2o)

save(wss_2004,wss_2005,wss_2006,wss_2007,wss_2008,wss_2009,wss_2010,wss_2011,wss_2012,wss_2013,wss_2014, file="./MSA_RES_WSS_RESULTS.Rdata")

#Plot All WSS
#Range standardisation function
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

test <- data.frame(Y2004=unlist(wss_2004[,1]),Y2005=unlist(wss_2005[,1]),Y2006=unlist(wss_2006[,1]),Y2007=unlist(wss_2007[,1]),Y2008=unlist(wss_2008[,1]),Y2009=unlist(wss_2009[,1]),Y2010=unlist(wss_2010[,1]),Y2011=unlist(wss_2011[,1]),Y2012=unlist(wss_2012[,1]),Y2013=unlist(wss_2013[,1]),Y2014=unlist(wss_2014[,1]))
test <- data.frame(apply(test,2,range01))
test$K <- 2:15
test <- melt(test, id.vars = "K")

ggplot(data=test, aes(K, value, colour = variable)) +
  geom_line() +
  geom_point()
ggsave("WSS_2004_2014_WRK.pdf")

# Test Clustergram 2014

source("https://gist.githubusercontent.com/hadley/439761/raw/a7027786f34e4819dc411496b61a34c02f7c1586/clustergram-had.r")

dat <- data.frame(MSA_WRK_2014[TC > limit]) #Get DF
dat <- dat[,i_cols] #Cut to data

k_2014 <- many_kmeans(dat, 2:15,nstart = 100)
pr <- prcomp(dat)
pr1 <- predict(pr)[, 1]
pr2 <- predict(pr)[, 2]
plot(clustergram(k_2014, pr1))



# Clustergrams

source("https://gist.githubusercontent.com/hadley/439761/raw/a7027786f34e4819dc411496b61a34c02f7c1586/clustergram-had.r")

clustergram_YYYY <- function(year){

dat <- data.frame(get(paste0("MSA_WRK_",year))[TC > limit]) #Get DF
dat <- dat[,i_cols] #Cut to data
tmp <- many_kmeans(dat, 2:15,nstart = 100)
pr <- prcomp(dat)
pr1 <- predict(pr)[, 1]
pr2 <- predict(pr)[, 2]

pdf(paste0("Clustergram_MSA_",year,"_WRK.pdf"))
print(plot(clustergram(tmp, pr1)))
dev.off()
return(tmp)
}

k_2004 <- clustergram_YYYY(2004)
k_2005 <- clustergram_YYYY(2005)
k_2006 <- clustergram_YYYY(2006)
k_2007 <- clustergram_YYYY(2007)
k_2008 <- clustergram_YYYY(2008)
k_2009 <- clustergram_YYYY(2009)
k_2010 <- clustergram_YYYY(2010)
k_2011 <- clustergram_YYYY(2011)
k_2012 <- clustergram_YYYY(2012)
k_2013 <- clustergram_YYYY(2013)
k_2014 <- clustergram_YYYY(2014)



#####################







######################
#Optimal Cluster Function
######################



optimal_cluster <- function(k,df,year,runs){
  
  clusters <- list()
  fit <- list()
  for (i in 1:runs){
    print(paste("starting run", i, sep=" "))
    tmp <- h2o.kmeans(training_frame = df, k = k, x = i_cols,max_iterations=1000,init="Random")
    fit[i] <- tmp@model$model_summary["within_cluster_sum_of_squares"]
    
    if (length(fit) == 1){clusters <- tmp } #Initially sets the cluster object on first run
    
    if (fit[i] < min(unlist(fit)[1:(i-1)])){ #Keep the cluster object where wss is smaller
      clusters <- tmp
    }
  }
  
  cluster_results <- h2o.predict(clusters,df)
  
  #Objects to return
  return(list("cluster_object" = clusters,"fit"=fit,"lookup" = as.data.frame(h2o.cbind(df[,"block_workplace"],cluster_results))))
  
}


results_WRK_WP_04 <- optimal_cluster(3,MSA_WRK_2004.h2o,2004,1000)
results_WRK_WP_05 <- optimal_cluster(3,MSA_WRK_2005.h2o,2005,1000)
results_WRK_WP_06 <- optimal_cluster(3,MSA_WRK_2006.h2o,2006,1000)
results_WRK_WP_07 <- optimal_cluster(3,MSA_WRK_2007.h2o,2007,1000)
results_WRK_WP_08 <- optimal_cluster(3,MSA_WRK_2008.h2o,2008,1000)
results_WRK_WP_09 <- optimal_cluster(3,MSA_WRK_2009.h2o,2009,1000)
results_WRK_WP_10 <- optimal_cluster(3,MSA_WRK_2010.h2o,2010,1000)
results_WRK_WP_11 <- optimal_cluster(3,MSA_WRK_2011.h2o,2011,1000)
results_WRK_WP_12 <- optimal_cluster(3,MSA_WRK_2012.h2o,2012,1000)
results_WRK_WP_13 <- optimal_cluster(3,MSA_WRK_2013.h2o,2013,1000)
results_WRK_WP_14 <- optimal_cluster(3,MSA_WRK_2014.h2o,2014,1000)



# Save Results
save(results_WRK_WP_04,results_WRK_WP_05,results_WRK_WP_06,results_WRK_WP_07,results_WRK_WP_08,results_WRK_WP_09,results_WRK_WP_10,results_WRK_WP_11,results_WRK_WP_12,results_WRK_WP_13,results_WRK_WP_14,file="./Results/Work/Clusters_SuperG/MSA_WRK_CENSUS_3_Clusters.Rdata")




######################
#Compare cluster solutions & create lookup table
######################
# Read blocks for MSA
poly.data <- readRDS("poly.data.rds")

x <- results_WRK_WP_04$lookup

#Setup lookup table...
final_lookup <- as.data.frame(poly.data@data$GEOID10)
colnames(final_lookup) <- c("block_workplace")
final_lookup <- merge(final_lookup,results_WRK_WP_04$lookup,by="block_workplace",all.x=TRUE)
colnames(final_lookup) <- c("block_workplace","results_WRK_WP_04")


compare_clusters <- function (x,y){
  
  tmp <- merge(x,y, by="block_workplace",all.x=TRUE)
  tmp2 <- as.data.frame.matrix(table(tmp[,2],tmp[,3])) #2004 as cols
  tmp3 <- list()
  for (n in 1:length(rownames(tmp2))){
    tmp3[n] <- colnames(tmp2[which.max(tmp2[n,])])
  }
  print(tmp2)
  return(unlist(tmp3))
}



ys <- c("results_WRK_WP_05","results_WRK_WP_06","results_WRK_WP_07","results_WRK_WP_08","results_WRK_WP_09","results_WRK_WP_10","results_WRK_WP_11","results_WRK_WP_12","results_WRK_WP_13","results_WRK_WP_14")

#Loop to create lookup
for (i in 1:length(ys)){
  tmp <-  compare_clusters(x,get(ys[i])$lookup)#make comparison and return list
  tmp2 <- get(ys[i])$lookup#get comparison year
  tmp2$predict <- mapvalues(tmp2$predict, from = tmp, to = as.character(rep(0:2)))#use plyr to match new numbering
  colnames(tmp2) <- c("block_workplace",ys[i]) #change column names to reflect year
  final_lookup <- merge(final_lookup,tmp2,by="block_workplace",all.x=TRUE)
}

write.csv(final_lookup,"./Results/Work/Clusters_SuperG/MSA_WRK_CENSUS_3_Clusters_Lookup.csv")

save(final_lookup,file="./Results/Work/Clusters_SuperG/MSA_WRK_CENSUS_3_Clusters_Lookup.Rdata")



##########################
# Describe Clusters ######
##########################

# Append classification
clusters_04 <- data.frame(block_workplace=final_lookup$block_workplace,results_WRK_WP_04=final_lookup$results_WRK_WP_04) #Use 2004 data to create profile
data_04 <- subset(MSA_WRK_2004, select = c("block_workplace",i_cols), TC > limit)
data_04 <- merge(data_04,clusters_04,by.x="block_workplace",by.y="block_workplace",all.x=TRUE)

# Cluster mean
group_means <- (data_04[,-"block_workplace"] %>% 
  group_by(results_WRK_WP_04) %>%
  summarise_all(funs(mean)))
# Overall mean
all_means <- (data_04[,-"block_workplace"] %>% 
               summarise_all(funs(mean)))
# Combine cluster means and overall means
group_means <- rbind(group_means,all_means)[-1] 

# Calculate index scores
index_scores <- data.frame(lapply(group_means, function(X) X/X[4]*100))[1:3,]
labs <-c("Workers age 29 or younger","workers age 30 to 54","workers age 55 or older", "Earnings $1250/month or less","Earnings $1251/month to $3333/month","earnings greater than $3333/month",
         "Agriculture","Mining","Utilities","Construction","Manufacturing","Wholesale","Retail","Transportation and Warehousing","Information",
         "Finance and Insurance","Real Estate","Professional...","Management...","Administrative and Support and Waste","Education","Health","Arts, Entertainment, and Recreation",
         "Accommodation and Food Services","Other Services","Public Administration")

# Transpose and assign SuperGroup code
index_scores <- data.frame(t(index_scores))
colnames(index_scores) <- c("Retail & Leisure","Blue Collar","White Collar")
rownames(index_scores) <- labs

#Plot chart
cols <-col2rgb(c("#94BA65","#2790B0","#C7919D"))
chartJSRadar(scores = as.list(index_scores), labs = labs, maxScale = 350,labelSize = 8,addDots = FALSE,colMatrix = cols)
########################################################################################################


































###################################################################################################
## Create sub groups
###################################################################################################




###
#Group 3
###

#Create a split input
input_split <- split(data.frame(input),cluster_results$cluster)
#Cluster
cluster_results_G3 <- kmeans(input_split[[3]],3,nstart=1000,iter.max=500)


input_split <- split(workplace_PCT[,1:20],cluster_results$cluster)

clusters <- data.frame(cluster_results_G3$cluster)
colnames(clusters) <- "Cluster"
index_split <- split(input_split[[3]], clusters)
#Creating index score relative to the global mean
df <- round((do.call("rbind", lapply(index_split, function(x) apply(x,2,mean))) / apply(workplace_PCT[,1:20],2,mean)) * 100)

#Create Plot
colnames(df) <- c("Agriculture, Forestry, Fishing and Hunting","Mining, Quarrying, and Oil and Gas Extraction","Utilities","Construction","Manufacturing","Wholesale Trade","Retail Trade","Transportation and Warehousing","Information","Finance and Insurance","Real Estate and Rental and Leasing","Professional, Scientific, and Technical Services","Management of Companies and Enterprises","Admin. / Support / Waste / Remediation","Educational Services","Health Care and Social Assistance","Arts, Entertainment, and Recreation","Accommodation and Food Services","Other Services","Public Administration")
melted_df <- melt(df)
cs <- c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7","#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", "#D14285", "#6DDE88", "#652926", "#7FDCC0")

p <- ggplot(as.data.frame(melted_df), aes(melted_df$Var1,log(melted_df$value)))
p + geom_point(aes(colour = factor(melted_df$Var2),shape = factor(melted_df$Var2)), size = 4) +
  scale_shape_manual(values=1:nlevels(melted_df$Var2)) +
  geom_hline(yintercept=log(100))  + scale_colour_manual(values = cs) +
  geom_hline(yintercept=log(200), color="black", linetype="dashed") + 
  geom_hline(yintercept=log(50), color="black", linetype="dashed") + 
  scale_x_discrete(1:length(unique(melted_df$Var1)),name="Cluster") + 
  theme(legend.title=element_blank()) +
  labs( y= "log(index score)")
ggsave("index_G3_graph.pdf", width = 29.7, height = 21, units = "cm")


###
#Group 6
###

#Create a split input
input_split <- split(data.frame(input),cluster_results$cluster)
#Cluster
cluster_results_G6 <- kmeans(input_split[[6]],3,nstart=500,iter.max=500)


input_split <- split(workplace_PCT[,1:20],cluster_results$cluster)

clusters <- data.frame(cluster_results_G6$cluster)
colnames(clusters) <- "Cluster"
index_split <- split(input_split[[6]], clusters)
#Creating index score relative to the global mean
df <- round((do.call("rbind", lapply(index_split, function(x) apply(x,2,mean))) / apply(workplace_PCT[,1:20],2,mean)) * 100)

#Create Plot
colnames(df) <- c("Agriculture, Forestry, Fishing and Hunting","Mining, Quarrying, and Oil and Gas Extraction","Utilities","Construction","Manufacturing","Wholesale Trade","Retail Trade","Transportation and Warehousing","Information","Finance and Insurance","Real Estate and Rental and Leasing","Professional, Scientific, and Technical Services","Management of Companies and Enterprises","Admin. / Support / Waste / Remediation","Educational Services","Health Care and Social Assistance","Arts, Entertainment, and Recreation","Accommodation and Food Services","Other Services","Public Administration")
melted_df <- melt(df)
cs <- c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7","#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", "#D14285", "#6DDE88", "#652926", "#7FDCC0")

p <- ggplot(as.data.frame(melted_df), aes(melted_df$Var1,log(melted_df$value)))
p + geom_point(aes(colour = factor(melted_df$Var2),shape = factor(melted_df$Var2)), size = 4) +
  scale_shape_manual(values=1:nlevels(melted_df$Var2)) +
  geom_hline(yintercept=log(100))  + scale_colour_manual(values = cs) +
  geom_hline(yintercept=log(200), color="black", linetype="dashed") + 
  geom_hline(yintercept=log(50), color="black", linetype="dashed") + 
  scale_x_discrete(1:length(unique(melted_df$Var1)),name="Cluster") + 
  theme(legend.title=element_blank()) +
  labs( y= "log(index score)")
ggsave("index_G6_graph.pdf", width = 29.7, height = 21, units = "cm")





#Create Single set of sub groups

lapply(input_split,nrow)

split_order <- as.numeric(unlist(lapply(input_split,row.names))) #creates a numeric list that the rows appear in the split inputs

#Creates a sub group column (only clusters 3 and 6)
all_subs <- c(rep(1,lapply(input_split,nrow)[1]),rep(2,lapply(input_split,nrow)[2]),cluster_results_G3$cluster,rep(4,lapply(input_split,nrow)[4]),rep(5,lapply(input_split,nrow)[5]),cluster_results_G6$cluster)

#Create Lookup
lookup <- data.frame(do.call('rbind', strsplit(as.character(workplace_PCT$geocode),'_',fixed=TRUE)),cluster_results$cluster)
lookup$X1 <- paste0("0",as.character(lookup$X1))
lookup <- lookup[split_order,] #Reorders to match the splits
lookup$SubCluster <- all_subs
colnames(lookup) <- c("geocode","Year","Cluster","SubCluster")
lookup$SubCluster <- paste0(lookup$Cluster,".",lookup$SubCluster)
lookup$SubCluster <- ifelse(lookup$SubCluster == 1.1, 1, ifelse(lookup$SubCluster == 2.2, 2, ifelse(lookup$SubCluster == 3.1, 3, ifelse(lookup$SubCluster == 3.2, 4, ifelse(lookup$SubCluster == 3.3, 5, ifelse(lookup$SubCluster == 4.4, 6, ifelse(lookup$SubCluster == 5.5, 7, ifelse(lookup$SubCluster == 6.1, 8, ifelse(lookup$SubCluster == 6.2, 9, ifelse(lookup$SubCluster == 6.3, 10, NA) ) ) ) ) ) ) ) ) ) 
lookup_split <- split(lookup,lookup$Year)


#Append lookup to spatial polygons
lookup_final <- lookup_split[[1]]
colnames(lookup_final) <- c("Block","Year","Cluster_2002","ClusterSub_2002")
lookup_final$Year <- NULL

for (i in 2:length(lookup_split)) {
  tmp <- lookup_split[[i]]
  colnames(tmp) <- c("Block","Year",paste0("Cluster_",lookup_split[[i]][1,2]),paste0("ClusterSub_",lookup_split[[i]][1,2]))
  tmp$Year <- NULL
  lookup_final <- merge(lookup_final,tmp, by = "Block",all.x=TRUE,all.y=TRUE)
}

#Join Spatial Polygons
sp.2014_Block_Cluster <- sp.2014_Block
sp.2014_Block_Cluster@data$Block <- paste0(sp.2014_Block_Cluster@data[,"STATEFP"],sp.2014_Block_Cluster@data[,"COUNTYF"],sp.2014_Block_Cluster@data[,"TRACTCE"],sp.2014_Block_Cluster@data[,"BLOCKCE"])
sp.2014_Block_Cluster@data <- sp.2014_Block_Cluster@data[,c("Block","NAME10")]
all_blk <- data.frame(all_blk)

#Append residential workplace scores
sp.2014_Block_Cluster@data = data.frame(sp.2014_Block_Cluster@data, all_blk[match(sp.2014_Block_Cluster@data[, "Block"], all_blk[, "Block"]), ])
sp.2014_Block_Cluster@data <- sp.2014_Block_Cluster@data[,!colnames(sp.2014_Block_Cluster@data) %in% c("NAME10","V1","Block.1")]

#Append cluster
sp.2014_Block_Cluster@data = data.frame(sp.2014_Block_Cluster@data, lookup_final[match(sp.2014_Block_Cluster@data[, "Block"], lookup_final[, "Block"]), ])

writeOGR(sp.2014_Block_Cluster,".","block_workplace_jobs_class",driver = "ESRI Shapefile")






###################################################################################################
#Maps - Classification
###################################################################################################

library(RColorBrewer)
my_colour <- brewer.pal(10,"Set3")
roads <- readOGR(".", "stclines_streets")
sp.2014_Block_Cluster = spTransform(sp.2014_Block_Cluster, CRS("+init=epsg:3857"))
roads = spTransform(roads, CRS("+init=epsg:3857"))

#Cluster Names
c_names <- c("Food & Leisure","Agriculture","Service Workers","Warehousing & Waterfront","Manufacturing","Education & Campus","Health and Social Care","Tech Infill","Public Services","Financial & Business Services")


sp.2014_Block_Cluster_Simp <- gSimplify(sp.2014_Block_Cluster,0.00001,topologyPreserve=TRUE)
sp.2014_Block_Cluster_Simp <- SpatialPolygonsDataFrame(sp.2014_Block_Cluster_Simp,sp.2014_Block_Cluster@data)
sp.2014_Block_Cluster_Simp = spTransform(sp.2014_Block_Cluster_Simp, CRS("+init=epsg:3857"))



for (i in 1:length(seq(2002, 2014, 1))) {
  yr <- seq(2002, 2014, 1)[i]
  #Initiate plot for SF
  pdf(paste0("map_",yr,".pdf"))
  plot(roads, col = NA, axes = FALSE)
  plot(sp.2014_Block_Cluster_Simp, col = "#eeeeee", border = NA,add=TRUE)
  plot(sp.2014_Block_Cluster_Simp, col = my_colour[sp.2014_Block_Cluster@data[,paste0("ClusterSub_",yr)]], border = NA,add=TRUE)
  legend("topleft", legend = c_names, fill = my_colour, bty = "n", cex=0.7, title=paste(yr))
  dev.off()
  print(yr)
}


###################################################################################################










##############################################################################################################################
#OD Flows
##############################################################################################################################

#OD
download.file("http://lehd.ces.census.gov/data/lodes/LODES7/ca/od/ca_od_main_JT00_2014.csv.gz","ca_od_main_JT00_2014.csv.gz") #CA - All Jobs - 2012 OD Flow
gunzip("ca_od_main_JT00_2014.csv.gz")
OD<- fread("ca_od_main_JT00_2014.csv",colClasses = c("character","character",rep("numeric",10),"character"))
#Create Tract code
OD[,h_geocode_T:=substr(OD$h_geocode,1,11)]
#Cut down
OD <- subset(OD,select=c("h_geocode_T","w_geocode","S000"))
#Aggregate
OD <- OD[, lapply(.SD,sum), by=list(h_geocode_T,w_geocode)]

#Limit OD flows to only those within the Bay Area
OD <- subset(OD, (substr(OD[,h_geocode_T],3,5) %in% SFOH_FIPS) & substr(OD[,w_geocode],3,5) %in% SFOH_FIPS)




#Load Tract Classification
load("~/Dropbox/Projects/US_Flow_Classification/OpenUSA/tract_data_with_classes_063013.Rdata")
usa.trt.cl <- usa.trt.cl[,c("FIPS","X10")]
usa.trt.cl$FIPS <- paste0("0",usa.trt.cl$FIPS)



#Append Classifications to flows & create X tab
lookup_tmp <- data.table(lookup_final[,c("Block","ClusterSub_2014")]) #Create a 2014 lookup
OD <- merge(OD, lookup_tmp,by.x="w_geocode",by.y="Block",all.x=TRUE)#Workplace
OD <- merge(OD, usa.trt.cl,by.x="h_geocode_T",by.y="FIPS",all.x=TRUE)#Residential

tmp <- subset(OD,select = c(S000,ClusterSub_2014,X10))
tmp <- tmp[, lapply(.SD,sum), by=list(ClusterSub_2014,X10)]
Flows <- dcast(tmp,formula=X10~ClusterSub_2014,value.var="S000")

tract_names <- c("A: Hispanic and Kids",
                 "B: Wealthy Nuclear Families",
                 "C: Middle Income, Single Family Homes",
                 "E: Wealthy Urbanites",
                 "F: Low Income and Diverse",
                 "G: Old, Wealthy White",
                 "H: Low Income Minority Mix",
                 "I: Poor, African-American",
                 "J: Residential Institutions, Young People","NA")

Flows <- cbind(tract_names,round(Flows[2:ncol(Flows)]/rowSums(Flows[2:ncol(Flows)])*100,digits = 1))
colnames(Flows) <- c("",c_names)
write.csv(Flows,"flows_OD.csv")



##############################################################################################################################
#OD Co-ordinates
##############################################################################################################################
SP_Tracts <- readOGR(".", "cb_2014_06_tract_500k")
SP_Tracts <- SP_Tracts[SP_Tracts@data$COUNTYFP %in% SFOH_FIPS,]
SP_Tracts@data = data.frame(SP_Tracts@data, usa.trt.cl[match(SP_Tracts@data[, "GEOID"], usa.trt.cl[, "FIPS"]), ])
SP_Tracts = spTransform(SP_Tracts, CRS("+init=epsg:3857"))
writeOGR(SP_Tracts,".","SP_Tracts_3857",driver = "ESRI Shapefile")



Tract_CoOrds <- data.frame(SP_Tracts@data$FIPS,coordinates(SP_Tracts))
colnames(Tract_CoOrds) <- c("FIPS","long","lat")
Block_CoOrds <- data.frame(sp.2014_Block_Cluster_Simp@data$Block,coordinates(sp.2014_Block_Cluster_Simp))
colnames(Block_CoOrds) <- c("Block","block_long","block_lat")

OD_LINESTRING <- merge(OD,Tract_CoOrds,by.x="h_geocode_T",by.y="FIPS",all.x=TRUE)
OD_LINESTRING <- merge(OD_LINESTRING,Block_CoOrds,by.x="w_geocode",by.y="Block",all.x=TRUE)
OD_LINESTRING$geom <- paste("LINESTRING (",OD_LINESTRING$long,OD_LINESTRING$lat,",",OD_LINESTRING$block_long,OD_LINESTRING$block_lat,")")
OD_LINESTRING <- OD_LINESTRING[!is.na(long) & !is.na(lat) & !is.na(block_long) & !is.na(block_long),]
write.csv(OD_LINESTRING,"OD_LINESTRING.csv")

save.image("~/Dropbox/Projects/US_Flow_Classification/Data.RData")
