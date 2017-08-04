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

#List of MSA codes (top 15 pop - https://en.wikipedia.org/wiki/List_of_Metropolitan_Statistical_Areas)
CBSAFP <- c('35620','31080','16980','19100','26420','47900','37980','33100','12060','14460','41860','38060','40140','19820','42660')
CBSAFP_remove <- c('47900','14460','38060') #DC,Boston
CBSAFP <- setdiff(CBSAFP,CBSAFP_remove)

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
MSA_All <- do.call(rbind, list(MSA_35620, MSA_31080, MSA_16980, MSA_19100, MSA_26420, MSA_37980, MSA_33100, MSA_12060, MSA_41860, MSA_40140, MSA_19820, MSA_42660))

#remove individual MSA objects
rm(list=c("MSA_35620","MSA_31080","MSA_16980","MSA_19100","MSA_26420","MSA_37980","MSA_33100","MSA_12060","MSA_41860","MSA_40140","MSA_19820","MSA_42660"))

######################################################################
# Create year files and calc rates
######################################################################

out_tab <- MSA_All[,colnames(MSA_All)[2:52],with=FALSE]/MSA_All[,C000] * 100 #convert to percent
out_tab[,block_workplace:= MSA_All[,block_workplace]] #Add block code
out_tab[,year:= MSA_All[,year]] #Add year ID
out_tab[,MSA:= MSA_All[,MSA]] #Add year ID


yr_list <- unique(out_tab$year)

for (i in 1:length(yr_list)){
  
  assign(paste0("MSA_WRK_",yr_list[i]),out_tab[year==yr_list[i]])
}

################################################################################
# Code to check block matches and identify those blocks with no data for 2004-2014
################################################################################


poly.data <- readRDS("poly.data.rds")
all_block_IDs <- data.table(poly.data@data$GEOID10) 
rm(poly.data)# Remove - as large


#Create a data table showing which blocks have data by year

for (i in 1:length(2004:2014)){
  
  yr <- c(2004:2014)[i]#Get year
  
  tmp <- data.table(get(paste0("MSA_WRK_",yr))[,block_workplace])
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



################################################################################
# Remove blocks with no data for 2004 - 2014 / calc rates again
################################################################################

out_tab <- MSA_All[!block_workplace %in% all_blocks_no_data,]#remove blocks with no data
out_tab <- out_tab[,colnames(out_tab)[2:52],with=FALSE]/out_tab[,C000] * 100 #convert to percent
out_tab[,block_workplace:= MSA_All[!block_workplace %in% all_blocks_no_data,block_workplace]] #Add block code
out_tab[,TC:= MSA_All[!block_workplace %in% all_blocks_no_data,C000]] #Add total count
out_tab[!block_workplace %in% all_blocks_no_data,year:= MSA_All[!block_workplace %in% all_blocks_no_data,year]] #Add year ID

yr_list <- unique(out_tab$year)
yr_list <- yr_list[!yr_list %in% c("2002","2003")]#2004 - 2014 only - 2002 & 2003 don't have full coverage

for (i in 1:length(yr_list)){ 
  
  assign(paste0("MSA_WRK_",yr_list[i]),out_tab[year==yr_list[i]])
  #write.csv(assign(paste0("MSA_",yr_list[i]),out_tab[year==yr_list[i]]),paste0("MSA_",yr_list[i],".csv"),row.names = FALSE) Optional write csv
  
}

save(MSA_WRK_2004,MSA_WRK_2005,MSA_WRK_2006,MSA_WRK_2007,MSA_WRK_2008,MSA_WRK_2009,MSA_WRK_2010,MSA_WRK_2011,MSA_WRK_2012,MSA_WRK_2013,MSA_WRK_2014,file="./Census_Files/MSA_WRK_CENSUS.Rdata")






