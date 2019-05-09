#Set Working Directory and Options
setwd("~/US_workplace_classification")
options(scipen=999)

#Load Packages
packages <- c("R.utils","data.table","RCurl","bit64","maptools","rgdal","rgeos","cluster","plyr")
for (package in packages){
  if(paste(package) %in% rownames(installed.packages()) == FALSE) {install.packages(paste(package))}
  library(paste(package),character.only=TRUE)
}


####################
# Download WAC Files
##################

setwd("~/US_workplace_classification")


# Read shapefiles
COUNTY_Points <- readRDS("COUNTY_Points.rds")
# FIPS
FIPS_USPS <- fread("FIPS_USPS_CODE.csv") #Lookup from: https://www.census.gov/geo/reference/ansi_statetables.html


#Create State list

state_list <- 'ca'


output_log <- NA

#Download Files - workplace - 2002 - 2015

for (n in 1:length(state_list)) { #state loop
  
  #assign(paste0("all_blk_",state_list[n]),list())
  assign(paste0("all_blk_",state_list[n]),data.table())
  
  for (i in 1:length(seq(2002, 2015, 1))) { #download loop
    dl_file <- paste0(state_list[n],"_wac_S000_JT00_",seq(2002, 2015, 1)[i],".csv.gz")
    
    if (url.exists((paste0("http://lehd.ces.census.gov/data/lodes/LODES7/",state_list[n],"/wac/",dl_file)))) { #Checks if the remote file exists
      
      download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/",state_list[n],"/wac/",dl_file),dl_file) #All Jobs
      gunzip(dl_file)
      assign("temp",fread(paste0(state_list[n],"_wac_S000_JT00_",seq(2002, 2015, 1)[i],".csv"))) #read text
      temp[,w_geocode:=paste0(temp[,w_geocode],"_",seq(2002, 2015, 1)[i])]#append year
      
      assign(paste0("all_blk_",state_list[n]),rbind(get(paste0("all_blk_",state_list[n])),temp)) #Append data to the output data table
      
      rm(temp)#remove temp object
      
      file.remove(paste0(state_list[n],"_wac_S000_JT00_",seq(2002, 2015, 1)[i],".csv"))#remove csv
      
    } else {
      output_log <- c(output_log,paste0(state_list[n],"_",seq(2002, 2015, 1)[i]))
    }
  }#end download loop
} #end state loop



# Split WAC Data


all_blk_ca[, c("block_workplace", "year") := tstrsplit(w_geocode, "_", fixed=TRUE)]#create a block and year column

all_blk_ca[,w_geocode:=NULL]

ca_WAC_2002_2015 <- all_blk_ca

save(ca_WAC_2002_2015, file = "./Final_Output/WAC_Data/ca_WAC_2002_2015.RData")



