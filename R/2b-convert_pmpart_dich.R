# 2b-convert_pmpart_dich.R
# In this script, the list of pmpart and dichot data is converted to the 
# appropriate format for merging in subsequent scripts 

# Data is divided into coarse and fine, columns are renamed using a conversion
# scheme s.t. it matches the post 2010 data 

# Pmpart and dich are all teflon filters (info from NAPS 2003-2008 paper)

# Setup/load the necessary files 
# ------------------------------------------------------------------------------

source('~/NAPS_project/NAPS_wrangling/R/0-setup_project.R') # setup the working dir

load(paste0(wd$output, "pmpart_read.rda"))
load(paste0(wd$output, "dichot_read.rda"))



# Read the conversion file: a scheme for how the pre-2010 columns are converted 
# to 2010+ format
  # 1st row: column names in the pre 2010 data
  # 2nd row: corresponding col names for the 2010+ data
  # 3rd row: corresponding mdl (minimum detection limit) column name
  # 4th row: new file type name/location
# Conversion file is slightly modified for the dichot conversion (towards end script)
##----------------------------------------------------------------------------
pmpart_conv <- read_excel(paste0(wd$header, "pmpart_headers.xlsx"), 
                               col_names = FALSE)[,-1]



##------------------------------------------------------------------------------
# Separate the data into coarse and fine 
##------------------------------------------------------------------------------
pmpart_fine <- lapply(pmpart, function(x){x %>% filter(c_f == "F") %>%
    dplyr::mutate(spec_pm2_5 = mass, .after = day) %>%
    select(-c("c_f", "mass"))})


dichot_fine <- lapply(dichot, function(x){x %>% filter(c_f == "F") %>%
    dplyr::mutate(dich_pm2_5 = mass, .after = day) %>%
    select(-c("c_f", "mass")) %>%
    distinct(date, .keep_all = TRUE)}) #manually checked distinct 


dichot_coarse <- lapply(dichot, function(x){x %>% filter(c_f == "C") %>%
    dplyr::mutate(dich_pm2_5_10 = mass, .after = day) %>%
    select(-c("c_f", "mass"))  %>%
    distinct(date, .keep_all = TRUE)}) #manually checked distinct 


##------------------------------------------------------------------------------
# Convert the data to 2010+ format
# Current structure: Station -> data
# New structure: Station -> File type -> data 
##------------------------------------------------------------------------------

## Current data has column followed by its detection limit 
## order of cols: date, year, month, day, mass (pm2_5 or pm2_5_10) then elem & DL


##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
## Lets start by defining some functions  
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

## Function: get_names
##---------------------------------
## Inputs: col_name: column name to match with 2010+ col names 
##         next_col_name: name of the adjacent column (corresponds to the dl col)
##         conversion: df of the conversion structure 
## Outputs: vector containing the new column names and the new file type
get_names <- function(col_name, next_col_name, conversion) {
  if (grep('d_l|mdl', next_col_name) == 0L)
    stop("Second argument to function must contain `d_l`")
  
  col <- which(col_name == conversion[1,])
  
  if (is.integer(col) && length(col) == 0L) {
    return(c(col_name, paste0(col_name, "_mdl"), "No file"))
  }else if (is.na(conversion[2, col])) {
    return(c(col_name, paste0(col_name, "_mdl"), "No file"))
  }else {
    return(c(conversion[2, col], conversion[3, col], conversion[4, col]))
  }
}


#Function: get_empty_list
#------------------------------------------------
## Inputs: original_df: contains the stations /data of interest
##         conv_scheme: contains the conversion scheme/header info 
# Creates a new df with the 2010+ format - uses the stations in the 
# original_df and file names in the conversion scheme 
get_empty_list <- function(original_df, conv_scheme){
  lst <- vector("list", length(original_df))
  names(lst) <- names(original_df)
  possible_types <- na.omit(unique(as.character(conv_scheme[4, ])))
  
  for (i in 1:length(lst)) {
    lst[[i]] <- vector("list", length = length(possible_types))
    names(lst[[i]]) <- possible_types
  }
  lst
}



# Function: rename_reformat_pmpart
#-------------------------------------------------
## Input: orig_df: list of the data by station
##        new_df: empty df with the 2010+ format 
##        conv_scheme: header dictionary with col names and new location
# Fills the new_df with the orig_df using the conv_scheme
rename_reformat_pmpart <- function(orig_df, new_df, conv_scheme, type) {
  for (s in names(new_df)) {
    
    # Start by adding the dates/pm mass 
    for (t in names(new_df[[s]])) {
      if (grepl("PM", t, fixed = TRUE)) {
        new_df[[s]][[t]] <- orig_df[[s]][, 1:5] # Add pm mass if pm file type
      } else {
        new_df[[s]][[t]] <- orig_df[[s]][, 1:4] # Add just date info 
      }
    }
    
    # Now add rest of cols & detection limit to the correct file type (sublist)
    if (ncol(orig_df[[s]]) > 5) {
      for (i in seq(6, ncol(orig_df[[s]]), 2)) {
        colNam <-
          get_names(names(orig_df[[s]])[i], names(orig_df[[s]])[i + 1],
                    conv_scheme)
        names(orig_df[[s]])[c(i, i + 1)] <- paste0(type,"_", colNam[c(1, 2)])
        
        new_df[[s]][[paste(colNam[3])]] <-
          cbind(new_df[[s]][[paste(colNam[3])]], orig_df[[s]][, c(i, i + 1)]) 
      }
    }
  }
  new_df
}



# Function: convert_pmpart
#---------------------------------
## Inputs: pmpart_df: the pmpart_df that is to be converted (Station -> data)
##         pmpart_conversion_file: contains the pmpart column names, new column 
##                            names and location/list element in the new format 
## Output: re-formatted data 
##            station -> file type -> data 
## Combines the helper functions to convert the pmpart (pre-2010) data into the 
## 2010 + format 
convert_pmpart <- function(pmpart_df, pmpart_conversion_file, type) {
  new_pm_fine_df <- get_empty_list(pmpart_df, pmpart_conversion_file)
  
  df <- rename_reformat_pmpart(pmpart_df, new_pm_fine_df, pmpart_conversion_file, type)
  
  # If a df has 4 columns - remove it (it just has the date columns)
  lapply(df, function(a) {
    list.clean(a, function(x)
      length(x) == 4L, recursive = FALSE)
  })
}




##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
## Lets use the functions to convert the data!
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
 

# modify the conversion scheme for dich fine data
pmpart_conv_dich_fine <- pmpart_conv
pmpart_conv_dich_fine[2,] <- as.list(gsub("spec_pm2_5",  "dich_pm2_5", pmpart_conv_dich_fine[2,]))



# modify the conversion scheme for dich coarse data
pmpart_conv_coarse <- pmpart_conv %>% 
  mutate_all(str_replace_all, "PM2.5", "PM2.5-10") %>% 
  mutate_all(str_replace_all, "spec_pm2_5", "dich_pm2_5_10") 


# Let's call our functions! 
pm_fine <- convert_pmpart(pmpart_fine, pmpart_conv, "spec")
dich_fine <- convert_pmpart(dichot_fine, pmpart_conv_dich_fine, "dich")
dich_coarse <- convert_pmpart(dichot_coarse, pmpart_conv_coarse, "dich")  



##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
## Save the files 
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
save(file = paste0(wd$output, "pmpart_fine_2010_format.rda"), pm_fine)
save(file = paste0(wd$output, "dichot_coarse_2010_format.rda"), dich_coarse)
save(file = paste0(wd$output, "dichot_fine_2010_format.rda"), dich_fine)






