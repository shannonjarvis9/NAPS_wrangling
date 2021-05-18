# In this script, the files within the hourly directory (created in 1a) are read into
# a data frame

source('~/NAPS_project/NAPS_wrangling/R/0-setup_project.R') # setup the working dir





## Function: getFile_csv
## -----------------------------------------------------
## Inputs: file_path: path of file to be read
##         type: type of PM tht is being read 
## Output: a data frame with the appropriately formatted data frame column names  
##          for 2003-2010 NAPS xls data
##          read_excel() has issues reading .xls 
getFile_csv <- function(file_path){
  # tmp <- read.csv(file_path, header = FALSE, na.strings = c("NA","", " ", "-", "-999","-999.000"),
  #                 fileEncoding="latin1", nrows = 20)
  # i <- getHeader(tmp)
  
  # if(i == 0){
  #   return(stop(paste0("Header cannot be found in ", file_path, ", stopping exectution.")))
  # }
  
  df <- read.csv(file_path, header = FALSE, na.strings = c("NA","", " ", "-", "-999","-999.000"), 
                 fileEncoding="latin1", check.names = FALSE)
  
  if("Method" == df[1,2] | ncol(df) != 31){ df <- select(df, -2)} 
  
  # Later years have english name // french name  --> I just want the english part 
  #names(df) <- str_replace(unlist(lapply(strsplit(names(df), "//"), "[[", 1)), " ", "")
  names(df) <- c("Pollutant", "NAPSID", "City", "P/T", "Latitude", "Longitude", 
                 "Date", "H01", "H02", "H03", "H04", "H05", "H06", "H07", "H08", 
                 "H09", "H10", "H11", "H12", "H13", "H14", "H15", "H16", "H17", 
                 "H18", "H19", "H20", "H21", "H22", "H23", "H24")
  
  df %>% 
    filter(Pollutant %in% c("CO",   "NO" ,  "NO2",  "NOX" , "O3" ,  "PM10", "PM25", "SO2" )) %>% 
    clean_names() %>%
    mutate(date = as.Date(as.character(date),format="%Y%m%d")) %>% 
    mutate(year = year(date), month = month(date), day = day(date)) %>%
    mutate(across(starts_with("h"), as.numeric))
  
}


# Lets load the data

dir <- paste0(wd$data, "Hourly")

# issue with the format of all 2019 files (R reads them as having 2 columns)
files <- grep("2019", list.files(dir, full.names = TRUE), invert = TRUE, value = TRUE)
hourly_df <- getFile_csv(files[1])

for(i in 112:length(files)){
  tmp <- getFile_csv(files[i])
  hourly_df <- merge(hourly_df, tmp, all.x = TRUE, all.y = TRUE,
                             by = names(hourly_df))
  print(sprintf("Read %s",files[i]))
}

#  summarise(across(h01:h24, ~mean(.x, na.rm = TRUE))) 

save(file = paste0(wd$output, "naps_hourly.rda"), hourly_df)

