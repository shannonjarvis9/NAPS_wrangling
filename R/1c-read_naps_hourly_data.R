# 1c-read_naps_hourly_data.R
# In this script, the files within the hourly directory (created in 1a) are read into
# a data frame
library(stringr)
source('~/NAPS_project/NAPS_wrangling/R/0-setup_project.R') # setup the working dir





## Function: getFile_csv
## -----------------------------------------------------
## Inputs: file_path: path of file to be read
## Output: a data frame with the appropriately formatted data frame column names  
getFile_csv <- function(file_path){

  ncol <- min(max(count.fields(file_path, sep = ","), na.rm = TRUE), 32L)
  df <- read.csv(file_path, header = FALSE, na.strings = c("NA","", " ", "-", "-999","-999.000"), 
                 fileEncoding="latin1" , check.names = FALSE, col.names=paste0('V', seq_len(ncol)),
                 fill=TRUE, quote = "", sep = "," ) 
  df <- as.data.frame(sapply(df, function(x){gsub("\"", "", x, fixed = TRUE)}))
  
  # Try to find the header row 
  headerRow <- max(grep("Pollut", df$V1)) 
  
  # Expect there to be 31 rows - some files have an additional 
  # 'Method" column - this needs to be removed 
  if("Method" == df[headerRow,2] |  df[headerRow,2] == "Method Code//Code Méthode"
     | df[headerRow,2] == "Method Code // Code Méthode"){ df <- select(df, -2)} 
  if(ncol(df) > 31){df <- select(df, -c(32:ncol(df)))}
  
  # Later years have english name // french name  --> I just want the english part 
  names(df) <- c("Pollutant", "NAPSID", "City", "P/T", "Latitude", "Longitude", 
                 "Date", "H01", "H02", "H03", "H04", "H05", "H06", "H07", "H08", 
                 "H09", "H10", "H11", "H12", "H13", "H14", "H15", "H16", "H17", 
                 "H18", "H19", "H20", "H21", "H22", "H23", "H24")
  
  
  # keep only rows with valid pollutants 
  df <- df[which(df$Pollutant %in% c("CO",   "NO" ,  "NO2",  "NOX" , "O3",  "PM10", "PM25","PM2.5", "SO2" )), ]
  df$dates2 <- df$Date
  
  df %>% 
    clean_names() %>%
    mutate(datea = as.Date(as.character(date),format = "%Y%m%d")) %>% 
    mutate(dateb = as.Date(as.character(dates2),format = "%Y-%m-%d")) %>% #tryFormats=c("%Y-%m-%d", "%Y%m%d"))) %>% 
    mutate(date = coalesce(datea, dateb)) %>%
    select(-dates2) %>% 
    mutate(year = year(date), month = month(date), day = day(date)) %>%
    mutate(across(starts_with("h"), as.numeric)) 
}


## Function: check_df
## -----------------------------------------------------
## Inputs: df: data frame to be checked 
##         stations: list of valid station numbers 
##         i: index of file being read from the files vector 
##         year: year of the file being read 
## Output: boolean if data frame passed checks & a message if checks fail 
check_df <- function(df, stations, i, year){
  successful <- TRUE
  
  # Remove observations where year doesn't match the file name's year 
  df <- df[which(df$year == year),]
  
  # Check the dates are valid 
  numInvalid <- length(which(is.na(df$month)))
  if(numInvalid != 0){
    print(sprintf("Warning: %i Invalid dates, file %s",numInvalid, files[i]))
    successful <- FALSE
  }
  
  # Check the stations are valid 
  invalidStn <- df$NAPSID[! df$NAPSID %in% stations]
  if(length(invalidStn) != 0){
    print(sprintf("Warning: Invalid station id, file %s, invalid station %s",files[i], invalidStn))
    successful <- FALSE
  }
  
  return(successful)
}


# Lets load the data
dir <- paste0(wd$data, "Hourly")
NAPS_raw_metadata <- read.csv(paste0(wd$data, "edited_StationsNAPS-StationsSNPA.csv"))
stations <- unique(NAPS_raw_metadata$NAPS)

# issue with the format of all 2019 files (R reads them as having 2 columns)
#files <- grep("2019", list.files(dir, full.names = TRUE), invert = TRUE, value = TRUE)
files <- list.files(dir, full.names = TRUE)
file_years <- unlist(lapply(strsplit(files, "_|\\."), "[[", 4)) 

# Initalze the data frame 
hourly_df <- getFile_csv(files[1])

# Itterate through the files, adding to the data frame 
for(i in 2:length(files)){
  tmp <- getFile_csv(files[i])
  valid <- check_df(tmp, stations, i, file_years[i])
  
  if(valid){
    hourly_df <- merge(hourly_df, tmp, all.x = TRUE, all.y = TRUE,
                       by = names(hourly_df))
  }else {
  print(sprintf("Unsuccseefully read %s",files[i]))
  }
  
}

#  summarise(across(h01:h24, ~mean(.x, na.rm = TRUE))) 



# There are some issues with the files & imcomplete observations, remove by 
# using date criteria (was manually checked)
# Example of such an issue is in the SO2_2015.csv file at bottom
invalidObsv <- hourly_df[which(is.na(hourly_df$month)), ] #75 invalid obsv - lots of half filled/empty
hourly_df  <- hourly_df[- which(is.na(hourly_df$month)), ] %>%
  
hourly_df <- hourly_df %>% 
  select(-c("datea", "dateb")) %>% 
  relocate(c("year", "month", "day"), .after = "date")

hourly_df$napsid <- str_remove(hourly_df$napsid, "^0+")

save(file = paste0(wd$output, "naps_hourly.rda"), hourly_df)
