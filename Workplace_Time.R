#Set Working Directory and Options
setwd("~/US_workplace_classification")
options(scipen=999)

#Load Packages
packages <- c("R.utils","data.table","RCurl","bit64","maptools","rgdal","rgeos","cluster")
for (package in packages){
if(paste(package) %in% rownames(installed.packages()) == FALSE) {install.packages(paste(package))}
library(paste(package),character.only=TRUE)
}


##################
# Download WAC Files
##################

setwd("~/US_workplace_classification")

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


####################################################################################
#Create MSA Tables
####################################################################################

MSA_list <- as.numeric(CBSAFP)#Create a list of MSA

for (i in 1:length(MSA_list)){
  
  t <- unique(COUNTY_Points@data[COUNTY_Points@data$CBSAFP == MSA_list[i],"STATEFP"]) #List the state that the MSA are within
  s <- tolower(FIPS_USPS[FIPS %in% t,USPS_CODE]) #Get the state list as USPS format codes
  print(s)

  assign(paste0("MSA_",MSA_list[i]),data.table()) #Creates an empty MSA data table
    
    u <- COUNTY_Points@data[COUNTY_Points@data$CBSAFP == MSA_list[i] ,c("STATEFP","COUNTYFP")] #List the counties within state
    u <- unique(paste0(u$STATEFP,u$COUNTYFP))#Create list of counties
    
    for (k in 1:length(u)) {  
      if (substring(u[k],1,1) == "0"){#loop to check for the missing 0 issue you get on the state codes
        u[k] <- substr(u[k],2,nchar(u[k]))
      }
    }
    
    for (j in 1:length(s)){#loop to extract counties from appropriate state table
      tmp <- get(paste0("all_blk_",s[j]))[substring(w_geocode,1,nchar(u)) %in% u,] #Get rows that match the county  
      assign(paste0("MSA_",MSA_list[i]),rbind(get(paste0("MSA_",MSA_list[i])),tmp)) #Add data to MSA table
    }

}

######################################################################
#Create combined MSA input per year
######################################################################

for (i in 1:length(paste0("MSA_",CBSAFP))) { #loop through each MSA
  
  MSA <- paste0("MSA_",CBSAFP)[i]#get MSA code in the format of the data table
  tmp <- get(MSA)
  tmp[, c("block_workplace", "year") := tstrsplit(w_geocode, "_", fixed=TRUE)]#create a block and year column
  tmp[,MSA:= paste0(CBSAFP[i])]#Add MSA code
  
  tmp$block_workplace[nchar(tmp$block_workplace) < 15] <- paste0("0",tmp$block_workplace) #Corrects the missing 0 issue where needed
  assign(MSA,tmp)
  rm(tmp)
}

#Create combined MSA object
MSA_All <- do.call(rbind, list(MSA_35620, MSA_31080, MSA_16980, MSA_19100, MSA_26420, MSA_37980, MSA_33100, MSA_12060, MSA_41860, MSA_38060, MSA_40140, MSA_19820, MSA_42660))

#remove individual MSA objects
rm(list=c("MSA_35620","MSA_31080","MSA_16980","MSA_19100","MSA_26420","MSA_37980","MSA_33100","MSA_12060","MSA_41860","MSA_38060","MSA_40140","MSA_19820","MSA_42660"))

######################################################################
# Create year files and calc rates
######################################################################

out_tab <- MSA_All[,colnames(MSA_All)[3:52],with=FALSE]/MSA_All[,C000] * 100 #convert to percent
out_tab[,block_workplace:= MSA_All[,block_workplace]] #Add block code
out_tab[,year:= MSA_All[,year]] #Add year ID
out_tab[,MSA:= MSA_All[,MSA]] #Add year ID


yr_list <- unique(out_tab$year)

for (i in 1:length(yr_list)){
  
  assign(paste0("MSA_",yr_list[i]),out_tab[year==yr_list[i]])
}

################################################################################
# Code to check block matches and identify those blocks with no data for 2004-2014
################################################################################

length(unique(poly.data@data$GEOID10)) #number of unique block ID
nrow(poly.data@data) #table rows

all_block_IDs <- data.table(poly.data@data$GEOID10) 

#Create a data table showing which blocks have data by year

for (i in 1:length(2004:2014)){
  
  yr <- c(2004:2014)[i]#Get year
  
  tmp <- data.table(get(paste0("MSA_",yr))[,block_workplace])
  tmp[,yr := 1]
  setnames(tmp, old=c("V1","yr"), new=c("V1",paste0("Yr_",yr)))
  
  all_block_IDs <- merge(all_block_IDs,tmp,by = "V1", all.x=TRUE)
  
  tmp2 <- data.table(all_block_IDs[,V1])
  tmp2[,In_All_Blocks := 1]
  
  tmp <- merge(data.table(tmp[,V1]),tmp2,by = "V1", all.x=TRUE)#Checks to ensure all blocks in a year are found in the all_block_IDs object
  
  assign(paste0("blocks_in_",yr),tmp)
  rm(list=c("tmp","tmp2"))
    
}

all_blocks_no_data <- all_block_IDs[rowSums(is.na(all_block_IDs))==11, V1]

#write.csv(all_block_IDs,"missing_blocks")


################################################################################
# Remove blocks with no data for 2004 - 2014 / calc rates again
################################################################################

out_tab <- MSA_All[!block_workplace %in% all_blocks_no_data,]#remove blocks with no data
out_tab <- out_tab[,colnames(out_tab)[3:52],with=FALSE]/out_tab[,C000] * 100 #convert to percent
out_tab[,block_workplace:= MSA_All[!block_workplace %in% all_blocks_no_data,block_workplace]] #Add block code
out_tab[,TC:= MSA_All[!block_workplace %in% all_blocks_no_data,C000]] #Add total count
out_tab[!block_workplace %in% all_blocks_no_data,year:= MSA_All[!block_workplace %in% all_blocks_no_data,year]] #Add year ID

yr_list <- unique(out_tab$year)
yr_list <- yr_list[!yr_list %in% c("2002","2003")]#2004 - 2014 only - 2002 & 2003 don't have full coverage

for (i in 1:length(yr_list)){ 
  
  assign(paste0("MSA_",yr_list[i]),out_tab[year==yr_list[i]])
  #write.csv(assign(paste0("MSA_",yr_list[i]),out_tab[year==yr_list[i]]),paste0("MSA_",yr_list[i],".csv"),row.names = FALSE) Optional write csv
  
}

save.image("data.Rdata")


################################################################################
# Clustering using H2O
################################################################################

#h2o.shutdown()

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("methods","statmod","stats","graphics","RCurl","jsonlite","tools","utils")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Next, we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-turing/6/R")))
library(h2o)
h2o.init(max_mem_size = "30g",nthreads = -1)


#Convert dataframe to H20 dataframe
MSA_2004.h2o <- as.h2o(MSA_2004, destination_frame="MSA_2004.h2o")

i_cols <- c("CA01","CA02","CA03","CE01","CE02","CE03","CNS01", "CNS02", "CNS03", "CNS04", "CNS05", "CNS06", "CNS07", "CNS08", "CNS09","CNS10","CNS11","CNS12","CNS13","CNS14","CNS15","CNS16","CNS17","CNS18","CNS19","CNS20")

#gap <- clusGap(as.data.frame(subset(MSA_2004,select=i_cols)), kmeans, K.max=10, B=500)


K_10_results <- h2o.kmeans(training_frame = MSA_2004.h2o, k = 10, x = i_cols,max_iterations=1000,init="Random")
K_10_results_out <- h2o.predict(K_10_results,MSA_2004.h2o) #Get the cluster ID



#Scree Plot
wss <- list()
tmp <- NA

for (k in 2:25){
for (i in 1:100) {
  tmp[i] <- h2o.kmeans(training_frame = MSA_2004.h2o, k = k, x = i_cols,max_iterations=1000,init="Random")@model$model_summary["within_cluster_sum_of_squares"]
  }
wss <- rbind(wss,c(mean(unlist(tmp)),median(unlist(tmp)),min(mean(unlist(tmp))),max(unlist(tmp))))
}


plot(2:25, wss[,2], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")








h2o.downloadCSV(h2o.cbind(MSA_2004.h2o[,"block_workplace"],K_10_results_out), "K_10_lookup.csv")























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
