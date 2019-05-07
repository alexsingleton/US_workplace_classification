#Set Working Directory and Options
setwd("/home/alex/US_workplace_classification")
options(scipen=999)

#Load Packages
packages <- c("R.utils","devtools","data.table","RCurl","bit64","maptools","rgdal","rgeos","cluster","plyr","httr","foreign","RMySQL","dplyr","rio","readxl","tidyr","stringr","igraph")
for (package in packages){
if(paste(package) %in% rownames(installed.packages()) == FALSE) {install.packages(paste(package))}
library(paste(package),character.only=TRUE)
}


####################
# Download OD Files
##################

#Create State list

state_list <- c('ak', 'al', 'ar', 'az', 'ca', 'co', 'ct', 'dc', 'de', 'fl', 'ga', 'hi', 'ia', 'id', 'il', 'in', 'ks', 'ky', 'la', 'ma', 'md', 'me', 'mi', 'mn', 'mo', 'ms', 'mt', 'nc', 'nd', 'ne', 'nh', 'nj', 'nm', 'nv', 'ny', 'oh', 'ok', 'or', 'pa', 'ri', 'sc', 'sd', 'tn', 'tx', 'ut', 'va', 'vt', 'wa', 'wi', 'wv', 'wy') #Removed pr - Puerto Rico; vi - U.S. Virgin Islands - no WAC or OD available

output_log <- NA

#Download Files - OD - 2002 - 2015

for (n in 1:length(state_list)) { #state loop

  assign(paste0("all_blk_",state_list[n]),data.table())
  
for (i in 1:length(seq(2002, 2015, 1))) { #download loop aux
    dl_file <- paste0(state_list[n],"_od_aux_JT00_",seq(2002, 2015, 1)[i],".csv.gz")
  #AUX  
    if (!http_error((paste0("https://lehd.ces.census.gov/data/lodes/LODES7/",state_list[n],"/od/",dl_file)))) { #Checks if the remote file exists
    
        download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/",state_list[n],"/od/",dl_file),dl_file) #All Jobs
        gunzip(dl_file)
        #assign("temp",fread(paste0(state_list[n],"_od_aux_JT00_",seq(2002, 2015, 1)[i],".csv"))) #read text
        assign("temp",fread(paste0(state_list[n],"_od_aux_JT00_",seq(2002, 2015, 1)[i],".csv"),colClasses=c( w_geocode="character",h_geocode="character",S000="integer"),drop=c("SA01","SA02","SA03","SE01","SE02","SE03","SI01","SI02","SI03","createdate"))) #read text
        
        #temp[,w_geocode:=paste0(temp[,w_geocode],"_",seq(2002, 2015, 1)[i])]#append year
        temp[,Yr:=seq(2002, 2015, 1)[i]]#append year
        
        assign(paste0("all_blk_",state_list[n]),rbind(get(paste0("all_blk_",state_list[n])),temp)) #Append data to the output data table
        
        rm(temp)#remove temp object
        
        file.remove(paste0(state_list[n],"_od_aux_JT00_",seq(2002, 2015, 1)[i],".csv"))#remove csv
        
    } else {
        output_log <- c(output_log,paste0(state_list[n],"_aux_",seq(2002, 2014, 1)[i]))
    }
   
}#end downloa  d loop (aux)
     
    #MAIN
    for (i in 1:length(seq(2002, 2015, 1))) { #download loop main
      dl_file <- paste0(state_list[n],"_od_main_JT00_",seq(2002, 2015, 1)[i],".csv.gz")
      
      if (!http_error((paste0("https://lehd.ces.census.gov/data/lodes/LODES7/",state_list[n],"/od/",dl_file)))) { #Checks if the remote file exists
        
        download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/",state_list[n],"/od/",dl_file),dl_file) #All Jobs
        gunzip(dl_file)
        #assign("temp",fread(paste0(state_list[n],"_od_main_JT00_",seq(2002, 2015, 1)[i],".csv"))) #read text
        assign("temp",fread(paste0(state_list[n],"_od_main_JT00_",seq(2002, 2015, 1)[i],".csv"),colClasses=c( w_geocode="character",h_geocode="character",S000="integer"),drop=c("SA01","SA02","SA03","SE01","SE02","SE03","SI01","SI02","SI03","createdate"))) #read text
        
        
        #temp[,w_geocode:=paste0(temp[,w_geocode],"_",seq(2002, 2015, 1)[i])]#append year
        temp[,Yr:=seq(2002, 2015, 1)[i]]#append year
        
        assign(paste0("all_blk_",state_list[n]),rbind(get(paste0("all_blk_",state_list[n])),temp)) #Append data to the output data table
        
        rm(temp)#remove temp object
        
        file.remove(paste0(state_list[n],"_od_main_JT00_",seq(2002, 2015, 1)[i],".csv"))#remove csv
        
      } else {
        output_log <- c(output_log,paste0(state_list[n],"_main_",seq(2002, 2014, 1)[i]))
      }
      
    
}#end download loop (main)

  saveRDS(get(paste0("all_blk_",state_list[n])), file = paste0("./OD/all_blk_",state_list[n],".Rdata"))
  rm(list=paste0("all_blk_",state_list[n]))

} #end state loop

write.csv(output_log,"missing_files_from_series.csv")





####################################################################################
# 
####################################################################################


#setwd("/home/alex/Documents/M_DRIVE/Data/")


#Read Block Lookup
#block_lookup <- import("/home/alex/US_workplace_classification/Shapefiles/Blocks/All_Blocks_2018_CSA.dbf")

# Core based statistical areas (CBSAs), metropolitan divisions, and combined statistical areas (CSAs) (https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html)

"https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2018/delineation-files/list1_Sep_2018.xls"

# 

load("~/US_workplace_classification/Data.RData")




# CSA Lookup
download.file("https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2018/delineation-files/list1_Sep_2018.xls","list1_Sep_2018.xls")
CSA_lookup <- read_xls("list1_Sep_2018.xls",skip=2)
CSA_lookup <- CSA_lookup[,c("CSA Code","CSA Title", "FIPS State Code","FIPS County Code")]
CSA_lookup <- unite(CSA_lookup, CountyCD, c(`FIPS State Code`, `FIPS County Code`), remove=TRUE,sep="")
CSA_lookup <- as.data.table(CSA_lookup)
CSA_lookup <- CSA_lookup[!is.na(`CSA Code`),]

setindex(CSA_lookup,CountyCD)


#

all_blk_ca[,w_CountyCD:=substring(w_geocode,1,5)]
all_blk_ca[,h_CountyCD:=substring(h_geocode,1,5)]

all_blk_ca[,w_TractCD:=substring(w_geocode,1,11)]
all_blk_ca[,h_TractCD:=substring(h_geocode,1,11)]



setindex(all_blk_ca,w_CountyCD,  w_TractCD,h_CountyCD, h_TractCD)
setindex(CSA_lookup ,CountyCD)

setnames(CSA_lookup, c('CSA Code', 'CSA Title'), c('W_CSA_Code', 'W_CSA_Title'))


all_blk_ca <- merge(all_blk_ca,CSA_lookup,by.x="w_CountyCD",by.y="CountyCD",all.x=TRUE)


setnames(CSA_lookup, c('W_CSA_Code', 'W_CSA_Title'), c('H_CSA_Code', 'H_CSA_Title'))


all_blk_ca <- merge(all_blk_ca,CSA_lookup,by.x="h_CountyCD",by.y="CountyCD",all.x=TRUE)


#Group by tract, year and subset for San Jose-San Francisco-Oakland CSA (internal flows)

CSA_2002 <- all_blk_ca[ Yr == "2002" & W_CSA_Code == "488" & H_CSA_Code == "488",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_2003 <- all_blk_ca[ Yr == "2003" & W_CSA_Code == "488" & H_CSA_Code == "488",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_2004 <- all_blk_ca[ Yr == "2004" & W_CSA_Code == "488" & H_CSA_Code == "488",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_2005 <- all_blk_ca[ Yr == "2005" & W_CSA_Code == "488" & H_CSA_Code == "488",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_2006 <- all_blk_ca[ Yr == "2006" & W_CSA_Code == "488" & H_CSA_Code == "488",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_2007 <- all_blk_ca[ Yr == "2007" & W_CSA_Code == "488" & H_CSA_Code == "488",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_2008 <- all_blk_ca[ Yr == "2008" & W_CSA_Code == "488" & H_CSA_Code == "488",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_2009 <- all_blk_ca[ Yr == "2009" & W_CSA_Code == "488" & H_CSA_Code == "488",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_2010 <- all_blk_ca[ Yr == "2010" & W_CSA_Code == "488" & H_CSA_Code == "488",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_2011 <- all_blk_ca[ Yr == "2011" & W_CSA_Code == "488" & H_CSA_Code == "488",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_2012 <- all_blk_ca[ Yr == "2012" & W_CSA_Code == "488" & H_CSA_Code == "488",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_2013 <- all_blk_ca[ Yr == "2013" & W_CSA_Code == "488" & H_CSA_Code == "488",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_2014 <- all_blk_ca[ Yr == "2014" & W_CSA_Code == "488" & H_CSA_Code == "488",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_2015 <- all_blk_ca[ Yr == "2015" & W_CSA_Code == "488" & H_CSA_Code == "488",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]


save(CSA_2002, CSA_2003,CSA_2004,CSA_2005,CSA_2006,CSA_2007,CSA_2008,CSA_2009,CSA_2010,CSA_2011,CSA_2012,CSA_2013,CSA_2014,CSA_2015,file = "CSA_DATA.RData")




#GET TRACT CENTROIDS (2018 DATA)
All_Tracts_Centroids_2018_WGS84 <-  readOGR(dsn="./Tracts", layer="All_Tracts_Centroids_2018_WGS84")
Tract_Centroids <- data.frame(GEOID=All_Tracts_Centroids_2018_WGS84@data$GEOID,coordinates(All_Tracts_Centroids_2018_WGS84))
names(Tract_Centroids) <- c("GEOID","lon","lat")

#Create Graph

CSA_Yr <- c("CSA_2002", "CSA_2003","CSA_2004","CSA_2005","CSA_2006","CSA_2007","CSA_2008","CSA_2009","CSA_2010","CSA_2011","CSA_2012","CSA_2013","CSA_2014","CSA_2015")
sink("diam_log.txt",split=FALSE,append = FALSE)


for (i in 1:length(CSA_Yr)){

#GET TEMP TABLE
tmp_table <- as.matrix(get(CSA_Yr[i])[All_Flow > 0 & (w_TractCD != h_TractCD),])

#CREATE NETWORK
g <- graph.edgelist(tmp_table[,1:2], directed=FALSE)

#ADD EDGE WEIGHTS
E(g)$weight=as.numeric(tmp_table[,3])


#ADD LOCATION TO EACH VERTEX
#tmp_coord <- Tract_Centroids[Tract_Centroids$GEOID %in% get.vertex.attribute(g, "name"),]#Gets the lat lon for the vertex
#tmp_coord <- tmp_coord[match(get.vertex.attribute(g, "name"), tmp_coord$GEOID),] #Order correctly
#g <- set.vertex.attribute(g, "latitude", value=tmp_coord$lat)
#g <- set.vertex.attribute(g, "longitude", value=tmp_coord$lon)


#FIND STRUCTURE
#dg <- diameter(g)
dg <- 10
#log diam.
cat(paste0(CSA_Yr[i],": D=",dg))

cluster_res <-  walktrap.community(g, steps=dg) #Find community structure

#ADD MEMBERSHIP TO EACH VERTEX
g <- set.vertex.attribute(g, "membership", value=membership(cluster_res))


assign(paste0(CSA_Yr[i],"_Graph_with_clusters"), g)#Add clusters to graph object


# Map
# Flows
names(Tract_Centroids) <- c("GEOID","w_lon","w_lat")
tmp_flow <- get(CSA_Yr[i])
Flow_Out <- data.frame(tmp_flow[All_Flow > 0 & (w_TractCD != h_TractCD),])
Flow_Out <- merge(Flow_Out,Tract_Centroids,by.x="w_TractCD",by.y="GEOID",all.x=TRUE)
names(Tract_Centroids) <- c("GEOID","h_lon","h_lat")
Flow_Out <- merge(Flow_Out,Tract_Centroids,by.x="h_TractCD",by.y="GEOID",all.x=TRUE)

# membership
names(Tract_Centroids) <- c("GEOID","lon","lat")
Tract <- data.frame(GEOID=get.vertex.attribute(g,"name"),membership=get.vertex.attribute(g,"membership"))
Tract <- merge(Tract,Tract_Centroids,by="GEOID",all.x=TRUE)


#Write CSV
write.csv(Tract,paste0(CSA_Yr[i],"_Tract_Membership.csv"))
write.csv(Flow_Out,paste0(CSA_Yr[i],"_Flows.csv"))

remove(list=c("g","dg","cluster_res","Tract","Flow_Out","tmp_table","tmp_flow"))

}


#write_graph(CSA_2002_graph,file="CSA_2002_graph.net",format="pajek")







