# 2c-hourly_data.R
# In this script, the hourly PM10 and PM25 data is read and aggregated to a 
# single daily observation

# Still in progress - issues transforming the hourly data to numerical 
library(stringr)

# Setup/load the necessary files 
# ------------------------------------------------------------------------------

source('~/NAPS_project/NAPS_wrangling/R/0-setup_project.R') # setup the working dir
setwd(paste0(wd$data,"Hourly/" ))



# Define some functions  
# ------------------------------------------------------------------------------


## Function: getHeader
# Finds row index of the header
## -----------------------------------------------------
## Inputs: file:  file to be read
## Output: i, the row which contains the header
##         or 0 if something went wrong 
## Process: Looks for a row that contains "NAPS" or "Date", returns the index of
##          that row
getHeader <- function(file){
  found_header <- i <- 0L  # found header is FALSE
  while(found_header == FALSE & i < nrow(file)){
    i <- i+1
    if(any(grep(paste0( c("NAPS", "Date") , sep = "", collapse = "|"), file[i,], 
                ignore.case = TRUE, value = FALSE))){
      found_header <- TRUE
    }
  }
  
  if(found_header == FALSE){
    warning("Header cannot be found in the file")
    return(0)
  } else {
    return(i)
  }
}


## Function: removeEmptyRowsCols
## -----------------------------------------------------
## Process: Removes rows whose loaction info (Col 2-6) is NA
removeEmptyRowsCols <- function(df){
  df[rowSums(is.na(df[,2:6]))<= 4,colSums(is.na(df))<nrow(df)]
}


## Function: format_csv
## -----------------------------------------------------
## Process: Reads and re-formats the specified file 
format_csv <- function(file){

  i <- getHeader(read.csv(file, fileEncoding="latin1", header = FALSE,
                          na.strings = c("NA","", " ", "-", "-999","-999.000")))
  
  df <- read.csv(file, fileEncoding="latin1", skip = i, check.names = FALSE,
                   na.strings = c("NA","", " ", "-", "-999","-999.000", "-9999")) 
  
  # Later years have english name // french name  --> I just want the english part 
  names(df) <- str_replace(unlist(lapply(strsplit(names(df), "//"), "[[", 1)), " ", "")
  
  if("napsid" %in% names(df)){names(df)[,]} # Inconsistent naming 
  
  # Format the df
  df %>%
    clean_names()  %>%
    mutate(date = as.Date(as.character(date), format = "%Y%m%d"), 
           year = year(date), month = month(date), day = day(date)) %>%
    mutate(across(starts_with("h"), as.numeric))
}

all_files <- list.files(paste0(wd$data,"Hourly" ), full.names = TRUE)

##--------------------------------------------
##--------------------------------------------
## PM10
##--------------------------------------------
##--------------------------------------------


# Start by reading all the csv files 
# ------------------------------------------------------------------------------
pm10_files <- grep("PM10", all_files, value = TRUE)[-17] # Issue with 2019 csv file! 

pm10_df <- format_csv(pm10_files[1]) # inititalze the df 

for(i in 2:length(pm10_files)){
  tmp <- format_csv(pm10_files[i])
  pm10_df <- rbind(pm10_df, tmp)
}

# Now with the large df, aggregate to one daily observation 
# ------------------------------------------------------------------------------
pm10_df2 <- pm10_df %>%
  select(-c( "pollutant")) %>% 
  rowwise() %>%
  mutate(mean = mean(c_across(h01:h24), na.rm = TRUE)) # replace mean in future 
  
  summarise(across(h01:h24, ~mean(.x, na.rm = TRUE))) 
  

# Transform aggregrated data to a list 
# ------------------------------------------------------------------------------
pm10_hourly <- split(pm10_df , f = pm10_df$napsid )



##--------------------------------------------
##--------------------------------------------
## PM25
##--------------------------------------------
##--------------------------------------------


# Start by reading all the csv files 
# ------------------------------------------------------------------------------
pm25_files <- grep("PM25", all_files, value = TRUE) # Issue with 2019 csv file! 

pm25_df <- format_csv(pm25_files[1]) # inititalze the df 
# Issue here with the date 
for(i in 2:length(pm25_files)){
  tmp <- format_csv(pm25_files[i])
  pm25_df <- rbind(pm25_df, tmp)
}

# Now with the large df, aggregate to one daily observation 
# ------------------------------------------------------------------------------
pm25_df2 <- pm25_df %>%
  select(-c( "pollutant")) %>% 
  rowwise() %>%
  mutate(mean = mean(c_across(h01:h24), na.rm = TRUE)) # replace mean in future 

summarise(across(h01:h24, ~mean(.x, na.rm = TRUE))) 


# Transform aggregrated data to a list 
# ------------------------------------------------------------------------------
pm10_hourly <- split(pm25_df , f = pm25_df$napsid )















