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
all_blk_ca <- readRDS("./OD/all_blk_ca.Rdata")






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


#Group by tract, year and subset for San Jose-San Francisco-Oakland CSA

CSA_388_2002 <- all_blk_ca[W_CSA_Code == "488" & Yr == "2002",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_388_2003 <- all_blk_ca[W_CSA_Code == "488" & Yr == "2003",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_388_2004 <- all_blk_ca[W_CSA_Code == "488" & Yr == "2004",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_388_2005 <- all_blk_ca[W_CSA_Code == "488" & Yr == "2005",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_388_2006 <- all_blk_ca[W_CSA_Code == "488" & Yr == "2006",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_388_2007 <- all_blk_ca[W_CSA_Code == "488" & Yr == "2007",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_388_2008 <- all_blk_ca[W_CSA_Code == "488" & Yr == "2008",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_388_2009 <- all_blk_ca[W_CSA_Code == "488" & Yr == "2009",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_388_2010 <- all_blk_ca[W_CSA_Code == "488" & Yr == "2010",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_388_2011 <- all_blk_ca[W_CSA_Code == "488" & Yr == "2011",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_388_2012 <- all_blk_ca[W_CSA_Code == "488" & Yr == "2012",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_388_2013 <- all_blk_ca[W_CSA_Code == "488" & Yr == "2013",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_388_2014 <- all_blk_ca[W_CSA_Code == "488" & Yr == "2014",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]
CSA_388_2015 <- all_blk_ca[W_CSA_Code == "488" & Yr == "2015",.(All_Flow = sum(S000)),by=list(w_TractCD, h_TractCD)]

#Create Graph
CSA_388_2002_graph <- graph_from_data_frame(CSA_388_2002[All_Flow > 100 & (w_TractCD != h_TractCD),],directed = FALSE)

test <- cluster_infomap(CSA_388_2002_graph)


write_graph(CSA_388_2002_graph,file="CSA_388_2002_graph.net",format="pajek")


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
  s <- tolower(FIPS_USPS[FIPS %in% t,STUSAB]) #Get the state list as USPS format codes
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






