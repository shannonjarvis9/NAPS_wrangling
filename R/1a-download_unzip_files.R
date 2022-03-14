library(WriteXLS)
library(pracma)
library(readxl)
library(openxlsx)
library(gdata)

# In this script, the NAPS data files are downloaded and unzipped 

# Downloads the following folders for each year: IntegratedPM2.5-10-PM2.5-10Ponctuelles.zip
# PMPART25.zip, IntegratedPM2.5-PM2.5Ponctuelles.zip, IntegratedPM2.5-10.zip, 
# IntegratedPM2.5.zip, PMDICHOT.zip

# The start_year and end_year can be modified 



setwd(paste0(wd$data ))

#--------------------------------------------------------------------------------
# Get the files from data.ec.gc.ca
# want 2010+ _IntegratedPM2.5-10-PM2.5-10Ponctuelles.zip, _IntegratedPM2.5-PM2.5Ponctuelles.zip
# want 2003-2009: PMSPECIATION.zip, PMPART25.zip, PMDICHOT.zip 
# want hourly data too 
#--------------------------------------------------------------------------------
start_year <- 2003 # must be earlier than 2010
end_year <- 2019


# Integrated data 
url_wo_year <- c("http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/", 
                 "/IntegratedData-DonneesPonctuelles/?lang=en")


mapply(function(url) 
  system(paste("wget -r -np -t1 -nd -APMSPECIATION.zip,_IntegratedPM2.5-10-PM2.5-10Ponctuelles.zip,PMPART25.zip,_IntegratedPM2.5-PM2.5Ponctuelles.zip,_IntegratedPM2.5-10.zip,_IntegratedPM2.5.zip,PMDICHOT.zip", 
               url)),url = paste0(url_wo_year[1], start_year:end_year, url_wo_year[2]))

# Rename zip files that are written in french - for consistent naming 
file.rename(list.files(pattern="PM2.5-10Ponctuelles"), 
            paste0(substring(list.files(pattern="PM2.5-10Ponctuelles"),1, 4), "_IntegratedPM2.5-10.zip"))

file.rename(list.files(pattern="PM2.5Ponctuelles"),
            paste0(substring(list.files(pattern="PM2.5Ponctuelles"),1, 4), "_IntegratedPM2.5.zip"))


# Hourly data 
dir.create(paste0(wd$data,"Hourly/" ))
setwd(paste0(wd$data,"Hourly/" ))

# url_wo_year <- c("http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/", 
#                  "/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/?lang=en")
# 
# 
# mapply(function(url) 
#   system(paste("wget -r -np -t1 -nd -A.csv", 
#                url)),url = paste0(url_wo_year[1], start_year:end_year, url_wo_year[2]))

types <- c("PM10", "O3", "CO", "NOX", "NO2", "NO", "SO2", "PM25")
years <- start_year:end_year

for(t in types){
  for(y in years){
    if(!(y == "2013" & t == "NOX")){ # Missing NOX for 2013, so we skip so loop continues
      download.file(paste0("https://data-donnees.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/", y, "/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/", t, "_", y, ".csv"), 
                    destfile = paste0(getwd(),"/", t, "_", y, ".csv"), method = "wget", quiet = TRUE, mode = "wb")
    }
  }
}



setwd(paste0(wd$data ))

#--------------------------------------------------------------------------------
# Unzip the files
#--------------------------------------------------------------------------------
dirs <- dir(path = getwd(), recursive = FALSE) 
unique_type <- c()
for(i in dirs){  unique_type <- unique(c(unique_type, sub("^_","", substring(i,5, nchar(i) - 4)))) } 
for(i in unique_type){ dir.create(paste0(getwd(), "/", i))}


for(i in dirs){ 
  year <- as.numeric(substring(i,1,4))
  type <- sub("^_","", substring(i,5, nchar(i) - 4))
  file_names <- unzip(i, list= TRUE)$Name
  
  #might be better to find indicies that i want instead of those to remove 
  file_names <- file_names[- grep('FR|4hr|change|csv|[/]$', file_names, 
                                  ignore.case = TRUE, value = FALSE)]
  
  if(isempty(file_names)){print(sprintf("%s is empty", i)); next}
  
  # unzip, rename, relocate the desired files
  unzip(i,  files = file_names, exdir = paste0("temp/") )
  lapply(1:length(file_names), function(i){file.rename(from = paste0(getwd(),"/temp/", file_names[i]) , 
                                                       to = paste0(getwd(),"/", type, "/", year,"_", sub(".*/", "", file_names[i])))}) 
  
  print(sprintf("Unzipped %i files from %s into %s", length(file_names), i, type)) 
  unlink("temp", recursive = TRUE) # delete the temp folder
}



#--------------------------------------------------------------------------------
# Rename some sheets 
#--------------------------------------------------------------------------------
# Sheet Elements_EDXRF becomes Elements_EXDXRF (for consistency)
setwd(paste0(wd$data, "IntegratedPM2.5"))

for(file in list.files(pattern = '.xlsx')){
  if("Elements_EDXRF" %in% excel_sheets(file)){
    wb <- loadWorkbook(file, xlsxFile = NULL)
    names(wb)[names(wb) == "Elements_EDXRF"] <- 'Elements_EXDXRF'
    saveWorkbook(wb, file, overwrite = TRUE)
  }
}

setwd(paste0(wd$data, "IntegratedPM2.5-10"))

for(file in list.files(pattern = '.xlsx')){
  if("Elements_EDXRF" %in% excel_sheets(file)){
    wb <- loadWorkbook(file, xlsxFile = NULL)
    names(wb)[names(wb) == "Elements_EDXRF"] <- 'Elements_EXDXRF'
    saveWorkbook(wb, file, overwrite = TRUE)
  }
}

#--------------------------------------------------------------------------------
# Manual modification of some files  
#--------------------------------------------------------------------------------
# Remove file
#   File is empty: says Sampling  relocated to 090228 in April 2008
file.remove( paste0(wd$data, "PMPART25/2009_S90227_PART25.XLS"))
file.remove( paste0(wd$data, "PMDICHOT/2009_S90227_DICH.XLS")) 


# Date changes format - fix here so it can be read easily later
# manual manipulation may create problems! be careful!!! 
file_path <- paste0(wd$data, "PMPART25/2009_S31001_PART25.XLS")
tmp <- read.xls(file_path, na.strings = c("NA","", " ", "-", "-999","-999.000"))
tmp[8:30,1] <- as.character(as.Date(as.character(tmp[8:30,1] ), format = c("%m/%d/%y")))

WriteXLS(tmp, paste0(wd$data,"PMPART25/2009_S31001_PART25.XLS"))









