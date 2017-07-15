#Set Working Directory and Options
setwd("~/US_workplace_classification")
options(scipen=999)

#Load Packages
packages <- c("R.utils","data.table","RCurl","bit64","maptools","rgdal","rgeos")
for (package in packages){
  if(paste(package) %in% rownames(installed.packages()) == FALSE) {install.packages(paste(package))}
  library(paste(package),character.only=TRUE)
}


##################
# Download blocks
##################


FIPS_USPS <- fread("FIPS_USPS_CODE.csv") #Lookup from: https://www.census.gov/geo/reference/ansi_statetables.html

setwd("~/US_workplace_classification/blocks")

#List of the states which MSA overlap
state_list <- c("nj", "ny", "pa", "ca", "il", "in", "wi", "tx", "de", "md", "fl", "ga", "az", "mi", "wa")
state_list <- FIPS_USPS[USPS_CODE %in% toupper(state_list),FIPS] #Get state numbers

#Download the block shapefiles
for (i in 1:length(state_list)){
  
  URL_file <- paste0("tl_2015_",state_list[i],"_tabblock10")
  URL <- paste0("ftp://ftp2.census.gov/geo/tiger/TIGER2015/TABBLOCK/",URL_file,".zip")
  download.file(URL,paste0(URL_file,".zip")) #Download
  unzip (paste0(URL_file,".zip"), exdir = ".") #Unzip
  file.remove(paste0(URL_file,".zip"))
}

#Create a US Block File from the states
#List files
files <- list.files(pattern=".shp$", recursive=TRUE,full.names=TRUE) #Generate list of shapefiles
system(paste0("ogr2ogr blocks.shp"," ",files[1]," -simplify 0.00009 -overwrite")) #Simplify and copy

#Loop to merge shapefiles / simplify
for (i in 2:length(files)){
  system(paste0("ogr2ogr tmp.shp"," ",files[i]," -simplify 0.00009 -overwrite")) #Simplify and copy
  system("ogr2ogr -f 'ESRI Shapefile' -update -append blocks.shp tmp.shp") #Merge blocks
  file.remove(list.files(pattern="tmp"))
  print(i)
}

poly.data <- readOGR(".","blocks")


################################################
#Create a lookup for MSA - County and State
################################################

setwd("~/US_workplace_classification/msa")

#List of MSA codes (top 15 pop - https://en.wikipedia.org/wiki/List_of_Metropolitan_Statistical_Areas)
CBSAFP <- c('35620','31080','16980','19100','26420','47900','37980','33100','12060','14460','41860','38060','40140','19820','42660')

#Limit the list to those MSA where there are data available from 2004 onwards
CBSAFP_remove <- c('47900','14460','38060') #DC,Boston
CBSAFP <- setdiff(CBSAFP,CBSAFP_remove)

#Download CBSA Boundaries
download.file("http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_cbsa_500k.zip","cb_2015_us_cbsa_500k.zip")
unzip("cb_2015_us_cbsa_500k.zip")
file.remove("cb_2015_us_cbsa_500k.zip")
CBSA <- readOGR(".", "cb_2015_us_cbsa_500k")
CBSA <- CBSA[CBSA@data$CBSAFP %in% CBSAFP,] #Limit to top ten MSA

# #Download State Boundaries
# download.file("http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_state_500k.zip","cb_2015_us_state_500k.zip")
# unzip("cb_2015_us_state_500k.zip")
# STATE <- readOGR(".", "cb_2015_us_state_500k")

setwd("~/US_workplace_classification/county")

#Download County Boundaries
download.file("http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_county_500k.zip","cb_2015_us_county_500k.zip")
unzip("cb_2015_us_county_500k.zip")
file.remove("cb_2015_us_county_500k.zip")
COUNTY <- readOGR(".", "cb_2015_us_county_500k")

#Create a county list within MSA
COUNTY_Points <- SpatialPointsDataFrame(coords = coordinates(COUNTY), data = data.frame(COUNTY@data), proj4string = CRS(proj4string(COUNTY)))#Create Point version County
o <- over(COUNTY_Points, CBSA) #Point in Polygon
COUNTY_Points@data <- cbind(COUNTY_Points@data, o)# Add the attributes back county
COUNTY_Points <- COUNTY_Points[!is.na(COUNTY_Points@data$CBSAFP), ]# Use the NA values to remove those points not within MSA definitions

setwd("~/US_workplace_classification")
saveRDS(COUNTY_Points, file="COUNTY_Points.rds")
saveRDS(CBSA, file="CBSA.rds")

####################################
# Create Block MSA Shapefiles
####################################

setwd("~/US_workplace_classification/blocks")

BLOCK_Points <- SpatialPointsDataFrame(coords = coordinates(poly.data), data = data.frame(poly.data@data), proj4string = CRS(proj4string(poly.data)))#Create Point version County

o <- over(BLOCK_Points, CBSA) #Point in Polygon
BLOCK_Points@data <- cbind(BLOCK_Points@data, o)# Add the attributes back to blocks
BLOCK_Points <- BLOCK_Points[!is.na(BLOCK_Points@data$CBSAFP), ]# Use the NA values to remove those points not within MSA definitions

MSA_list <- unique(BLOCK_Points@data$GEOID) #Create MSA list

for (i in 1:length(MSA_list)){
  
  MSA_Blocks <- BLOCK_Points@data[BLOCK_Points@data$GEOID == MSA_list[i],"GEOID10"]
  
  tmp <- poly.data[poly.data@data$GEOID10 %in% MSA_Blocks,] #Get the polygons within MSA
  tmp <- tmp[tmp@data$AWATER10 != 0,]
  tmp@data <- tmp@data[c("GEOID10","AWATER10")]
  assign(paste0("BLOCKS_MSA_",paste(MSA_list[i])),tmp) #Save MSA blocks
  writeOGR(tmp, ".", paste0("Block_MSA_",MSA_list[i]), driver="ESRI Shapefile")
  rm(tmp)
  
}

rm(poly.data) #Remove integrated file
rm(BLOCK_Points) #Remove integrated points file

#Create MSA Block list

MSA_BLOCK_LIST <- NA

for (i in 1:length(MSA_list)){
  tmp <- get(paste0("BLOCKS_MSA_",paste(MSA_list[i])))
  tmp <- as.character(tmp@data[,"GEOID10"])
  MSA_BLOCK_LIST <-c(MSA_BLOCK_LIST,tmp)
  rm(tmp)
}




save.image("data.Rdata")





