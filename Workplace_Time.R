#Set Working Directory
library(R.utils)
library(data.table)
library(RCurl)
setwd("~/US_workplace_classification")
options(scipen=999)
library(bit64)

library(rgdal)
library(rgeos)

#San Francisco–Oakland–Hayward, CA Metropolitan Statistical Area (plus extras)
#Alameda County (001), Contra Costa County, San Francisco, San Mateo County, Marin County,Napa (055), Solano (095), Santa Clara (085), Sonoma County (097)

SFOH_FIPS <- c("001","013","075","081", "041","055","095","085","097")


#Read Blocks
sp.2014_Block <- readOGR(".", "blocks")
sp.2014_Block <- sp.2014_Block[sp.2014_Block@data$COUNTYF %in% SFOH_FIPS,]
sp.2014_Block <- sp.2014_Block[sp.2014_Block@data$ALAND10 > 0,]
block_list <- unique(sp.2014_Block@data$GEOID10)


##################
# Download WAC Files
##################


#Create State list

state_list <- c('ak', 'al', 'ar', 'az', 'ca', 'co', 'ct', 'dc', 'de', 'fl', 'ga', 'hi', 'ia', 'id', 'il', 'in', 'ks', 'ky', 'la', 'ma', 'md', 'me', 'mi', 'mn', 'mo', 'ms', 'mt', 'nc', 'nd', 'ne', 'nh', 'nj', 'nm', 'nv', 'ny', 'oh', 'ok', 'or', 'pa', 'ri', 'sc', 'sd', 'tn', 'tx', 'ut', 'va', 'vt', 'wa', 'wi', 'wv', 'wy') #Removed pr - Puerto Rico; vi - U.S. Virgin Islands - no WAC or OD available

output_log <- NA

#Download Files - workplace - 2002 - 2014

for (n in 1:length(state_list)) { #state loop

  #assign(paste0("all_blk_",state_list[n]),list())
  assign(paste0("all_blk_",state_list[n]),data.table())
  
for (i in 1:length(seq(2002, 2014, 1))) { #download loop
    dl_file <- paste0(state_list[n],"_wac_S000_JT00_",seq(2002, 2014, 1)[i],".csv.gz")
    
    if (url.exists((paste0("http://lehd.ces.census.gov/data/lodes/LODES7/",state_list[n],"/wac/",dl_file)))) { #Checks if the remote file exists
    
        download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/",state_list[n],"/wac/",dl_file),dl_file) #All Jobs
        gunzip(dl_file)
        assign("temp",fread(paste0(state_list[n],"_wac_S000_JT00_",seq(2002, 2014, 1)[i],".csv"))) #read text
        temp[,w_geocode:=paste0(temp[,w_geocode],"_",seq(2002, 2014, 1)[i])]#append year
        
        assign(paste0("all_blk_",state_list[n]),rbind(get(paste0("all_blk_",state_list[n])),temp)) #Append data to the output data table
        
        rm(temp)#remove temp object
        
        file.remove(paste0(state_list[n],"_wac_S000_JT00_",seq(2002, 2014, 1)[i],".csv"))#remove csv
        
    } else {
        output_log <- c(output_log,paste0(state_list[n],"_",seq(2002, 2014, 1)[i]))
    }
}#end download loop
} #end state loop

save.image("data.Rdata")



#Todo;
# Build typology sep. for workplaces by top ten MSA
# Build RAC for MSA
# Join by OD flow


################################################
#Create a lookup for MSA - County and State
################################################

#List of MSA codes (top 15 pop - https://en.wikipedia.org/wiki/List_of_Metropolitan_Statistical_Areas)
CBSAFP <- c('35620','31080','16980','19100','26420','47900','37980','33100','12060','14460','41860','38060','40140','19820','42660')


#Download CBSA Boundaries
download.file("http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_cbsa_500k.zip","cb_2015_us_cbsa_500k.zip")
unzip("cb_2015_us_cbsa_500k.zip")
CBSA <- readOGR(".", "cb_2015_us_cbsa_500k")
CBSA <- CBSA[CBSA@data$CBSAFP %in% CBSAFP,] #Limit to top ten MSA

# #Download State Boundaries
# download.file("http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_state_500k.zip","cb_2015_us_state_500k.zip")
# unzip("cb_2015_us_state_500k.zip")
# STATE <- readOGR(".", "cb_2015_us_state_500k")

#Download County Boundaries
download.file("http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_county_500k.zip","cb_2015_us_county_500k.zip")
unzip("cb_2015_us_county_500k.zip")
COUNTY <- readOGR(".", "cb_2015_us_county_500k")

#Create a county list within MSA
COUNTY_Points <- SpatialPointsDataFrame(coords = coordinates(COUNTY), data = data.frame(COUNTY@data), proj4string = CRS(proj4string(COUNTY)))#Create Point version County
o <- over(COUNTY_Points, CBSA) #Point in Polygon
COUNTY_Points@data <- cbind(COUNTY_Points@data, o)# Add the attributes back county
COUNTY_Points <- COUNTY_Points[!is.na(COUNTY_Points@data$CBSAFP), ]# Use the NA values to remove those points not within MSA definitions





#Create MSA Tables
FIPS_USPS <- fread("FIPS_USPS_CODE.csv") #Lookup from: https://www.census.gov/geo/reference/ansi_statetables.html
MSA_list <- unique(COUNTY_Points@data$CBSAFP)#Create a list of MSA





for (i in 1:length(MSA_list)){
  
  t <- unique(COUNTY_Points@data[COUNTY_Points@data$CBSAFP == MSA_list[i],"STATEFP"]) #List the state that the MSA are within
  s <- tolower(FIPS_USPS[FIPS %in% t,USPS_CODE]) #Get the state list as USPS format codes
  print(s)
  
  assign(paste0("MSA_",MSA_list[i]),data.table()) #Creates an empty MSA data table
  
      for (j in 1:length(s)){#loop to pull in data from state files
        
        u <- unique(COUNTY_Points@data[COUNTY_Points@data$CBSAFP == MSA_list[i] & COUNTY_Points@data$STATEFP == t[j],"COUNTYFP"]) #List the counties within state
        cnty_tmp <- paste0(t[j],u[j]) #Get county code
        
        if (substring(cnty_tmp,1,1) == "0"){#loop to check for the missing 0 issue you get on the state codes
          cnty_tmp <- substr(cnty_tmp,2,nchar(cnty_tmp))
        }
        
      tmp <- get(paste0("all_blk_",s[j]))[substring(w_geocode,1,nchar(cnty_tmp)) == cnty_tmp,] #Get rows that match the county
      
      assign(paste0("MSA_",MSA_list[i]),rbind(get(paste0("MSA_",MSA_list[i])),tmp)) #Add data to MSA table
  
      }
      
  
}


















#Download Files - residential - 2002 - 2014

for (i in 1:length(seq(2002, 2014, 1))) {
  dl_file <- paste0("ca_rac_S000_JT00_",seq(2002, 2014, 1)[i],".csv.gz")
  download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/ca/rac/",dl_file),dl_file) #CA - All Jobs - 2012 OD Flow
  gunzip(dl_file)
  assign(paste0("residential_",seq(2002, 2014, 1)[i]),fread(paste0("ca_rac_S000_JT00_",seq(2002, 2014, 1)[i],".csv")))
  
  all_blk <- c(all_blk,as.numeric(get(paste0("residential_",seq(2002, 2014, 1)[i]))[,h_geocode]))
}

all_blk <- data.table(unique(all_blk))[-1]



#Create workplace - residential files for each year
for (i in 1:length(seq(2002, 2014, 1))) {
  #Temp_Res
  assign("temp_res",subset(get(paste0("residential_",seq(2002, 2014, 1)[i])),select = c("h_geocode","C000")))
  colnames(temp_res) <- c("Block","Residential")
  #Temp_wrk
  assign("temp_wrk",subset(get(paste0("workplace_",seq(2002, 2014, 1)[i])),select = c("w_geocode","C000")))
  colnames(temp_wrk) <- c("Block","Workplace")
  #create combined work - residence
  assign(paste0("residence_workplace",seq(2002, 2014, 1)[i]),merge(temp_res,temp_wrk, by="Block"))
  # Code add R, W, M
  get(paste0("residence_workplace",seq(2002, 2014, 1)[i]))[,Type:=ifelse(Residential > Workplace,"R",ifelse(Residential == Workplace,"M","W"))]
  
  tmp <-  get(paste0("residence_workplace",seq(2002, 2014, 1)[i]))
  
  colnames(tmp) <- c("Block",paste0("res_",seq(2002, 2014, 1)[i]),paste0("wrk_",seq(2002, 2014, 1)[i]),paste0("type_",seq(2002, 2014, 1)[i]))
    
  all_blk <- merge(all_blk,get(paste0("residence_workplace",seq(2002, 2014, 1)[i])),by.x="V1",by.y="Block",all.x=TRUE)
  
  }


#Limit to SF Area

all_blk <- all_blk[,Block:=paste0("0",as.character(V1))]
all_blk <-  all_blk[Block %in% block_list,]
#Limit to where at least
#block_list2 <- all_blk[all_blk$wrk_2002 >= 40 | all_blk$wrk_2003 >= 40 | all_blk$wrk_2004 >= 40 | all_blk$wrk_2005 >= 40 | all_blk$wrk_2006 >= 40 | all_blk$wrk_2007 >= 40 | all_blk$wrk_2008 >= 40 | all_blk$wrk_2009 >= 40 | all_blk$wrk_2010 >= 40 | all_blk$wrk_2011 >= 40 | all_blk$wrk_2012 >= 40 | all_blk$wrk_2013 >= 40 | all_blk$wrk_2014 >= 40,Block]
#all_blk <-  all_blk[Block %in% block_list2,]




#Build unified file for clustering

unified_input <- list()

for (i in 1:length(seq(2002, 2014, 1))) {
#Just job type variables
assign("temp_wrk",subset(get(paste0("workplace_",seq(2002, 2014, 1)[i])),select=c("w_geocode","C000",colnames(get(paste0("workplace_",seq(2002, 2014, 1)[i])))[grepl("CN",colnames(get(paste0("workplace_",seq(2002, 2014, 1)[i]))))])))
temp_wrk$w_geocode <- paste0(temp_wrk$w_geocode,"_",seq(2002, 2014, 1)[i])#create year version of Block code
temp_wrk <- subset(temp_wrk,C000 > 40)#Remove 40 counts
unified_input<-rbindlist(list(unified_input,temp_wrk))#rbind years
}


#Calculate Percentages
workplace <- as.data.frame(unified_input)#convert back to data frame
workplace_PCT <- workplace[,3:ncol(workplace)]/workplace[,2]
workplace_PCT$geocode <- workplace$w_geocode


#workplace_Index <- ((workplace[,3:ncol(workplace)]/workplace[,2]) / (sum(workplace[,3:ncol(workplace)])/sum(workplace[,2]))) *100
#workplace_Index$geocode <- workplace$w_geocode


#input <- workplace_PCT[,1:20]




input <- scale(workplace_PCT[,1:20])
#input2 <- scale(workplace_Index[,1:20])


#Scree Plot
#wss <- (nrow(input)-1)*sum(apply(input,2,var))
#for (i in 2:8) wss[i] <- sum(kmeans(input, centers=i,nstart=3)$withinss)
#plot(2:8, wss[-1], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


#Clustergram
#source("https://raw.github.com/talgalili/R-code-snippets/master/clustergram.r")
#source("clustergram_ADS.r")
clustergram(input2, k.range = 4:10, line.width = 0.0004)


#Explore input histograms
pdf("histograms.pdf")
par(mfrow = c(2, 3))
for (i in 1:(ncol(workplace_PCT)-1)){
  hist(workplace_PCT[,i], main=paste(colnames(workplace_PCT)[i]))
}
dev.off()

#Create Clusters
cluster_results <- kmeans(input,6,nstart=1000,iter.max=500)


#cluster_results_index <- kmeans(input2,6,nstart=500,iter.max=500)


#Viz Clusters
#source("http://pcwww.liv.ac.uk/~william/Geodemographic%20Classifiability/func%20CreateRadialPlot.r")
#viz <- data.frame(group=1:6,cluster_results$centers)
#colnames(viz) <- c("group","Agriculture, Forestry, Fishing and Hunting","Mining, Quarrying, and Oil and Gas Extraction","Utilities","Construction","Manufacturing","Wholesale Trade","Retail Trade","Transportation and Warehousing","Information","Finance and Insurance","Real Estate and Rental and Leasing","Professional, Scientific, and Technical Services","Management of Companies and Enterprises","Admin. / Support / Waste / Remediation","Educational Services","Health Care and Social Assistance","Arts, Entertainment, and Recreation","Accommodation and Food Services","Other Services","Public Administration")
#CreateRadialPlot(viz, plot.extent.x = 1.8,grid.max = 10,grid.min = -0.4,grid.label.size=2,axis.label.size=2)




####################################################################################################
# Test K
####################################################################################################
input_test <- input_split[[5]]
kmax = 5
totwss = list() # will be filled with total sum of within group sum squares
kmfit = list() # create and empty list
for (i in 2:kmax){
  kclus = kmeans(input_test,centers=i,iter.max=200,nstart=5)
  totwss[i] = kclus$tot.withinss
  kmfit[[i]] = kclus
}

kmeansAIC = function(fit){
  
  m = ncol(fit$centers)
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(D + 2*m*k)
}
aic=sapply(kmfit,kmeansAIC)
plot(seq(1,kmax),aic,xlab="Number of clusters",ylab="AIC",pch=20,cex=2)


v = -diff(unlist(aic))
nv = length(v)
fom = v[1:(nv-1)]/v[2:nv]
nclus = which.max(fom)+1
cat("The apparent number of clusters is: ",nclus,"\n")
points(nclus,aic[nclus],col=2,pch=20,cex=2)




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
#Create Index Scores - Super Groups
###################################################################################################

clusters <- data.frame(cluster_results$cluster)
colnames(clusters) <- "Cluster"
index_split <- split(workplace_PCT[,1:20], clusters)
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
ggsave("index_graph.pdf", width = 29.7, height = 21, units = "cm")







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
