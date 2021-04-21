# In this script, the pmpart data is renamed s.t. it matches the post 2010 data 

load(paste0(wd$output, "pmpart_read.rda"))
load(paste0(wd$output, "dichot_read.rda"))



# Read the conversion file: a scheme for how the pre-2010 columns are converted 
# to 2010+ format
## Contains the old column name, new column name, new detection limit column name
## and column location 
##----------------------------------------------------------------------------
pmpart_conv_fine <- read_excel(paste0(wd$header, "pmpart_pm25_headers.xlsx"), 
                               col_names = FALSE)[,-1]


pmpart_conv_coarse <- read_excel(paste0(wd$header,"pmpart_pm10_headers.xlsx"), 
                                 col_names = FALSE)[,-1]


# Separate the data into coarse and fine 
##----------------------------------------------------------------------------
pmpart_fine <- lapply(pmpart, function(x){x %>% filter(c_f == "F") %>%
    dplyr::mutate(pm2_5 = mass, .after = day) %>%
    select(-c("c_f", "mass"))})

dichot_fine <- lapply(dichot, function(x){x %>% filter(c_f == "F") %>%
    dplyr::mutate(pm2_5 = mass, .after = day) %>%
    select(-c("c_f", "mass")) %>%
    distinct(date, .keep_all = TRUE)})


dichot_coarse <- lapply(dichot, function(x){x %>% filter(c_f == "C") %>%
    dplyr::mutate(pm2_5_10 = mass, .after = day) %>%
    select(-c("c_f", "mass"))  %>%
    distinct(date, .keep_all = TRUE)})



## data is structured such that it has one column followed by its detection 
## limit 
## order of cols: date, year, month, day, mass (pm2_5 or pm2_5_10) then elem & DL


## Function: get_names
##---------------------------------
## Inputs: col_name: column name to match with 2010+ col names 
##         next_col_name: name of the adjacent column (corresponds to the dl col)
##         pmpart_conv: df with the conversion structure 
## Outputs: vector containing the new column names and the new file type
get_names <- function(col_name, next_col_name, pmpart_conv){
  if (grep('d_l|mdl', next_col_name) == 0L) stop("Second argument to function must contain `d_l`")
  
  col <- which(col_name == pmpart_conv[1,])
  
  if(is.integer(col) && length(col) == 0L){
    return(c(col_name, paste0(col_name, "_mdl"), "No file")) 
  }else if(is.na(pmpart_conv[2,col])){
    return(c(col_name, paste0(col_name, "_mdl"), "No file")) 
  }else {
    c(pmpart_conv[2,col], pmpart_conv[3,col], pmpart_conv[4,col])
  }
  
}

#Function: get_empty_list
#------------------------------------------------
## Inputs: original_df: contains the stations /data of interest
##         conv_scheme: contains the conversion scheme/header info 
# Create a new df to hold the newly formatted data
get_empty_list <- function(original_df, conv_scheme){
  lst <- vector("list", length(original_df))
  names(lst) <- names(original_df)
  possible_types <- na.omit(unique(as.character(conv_scheme[4,])))
  for(i in 1:length(lst)){
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
# Lets rename the columns and reformat to the 2010+ form
rename_reformat_pmpart <- function(orig_df,new_df, conv_scheme){
  for(s in names(orig_df)){
    for (t in names(new_df[[s]])) {
      if (grepl("PM", t, fixed = TRUE)) {
        new_df[[s]][[t]] <- orig_df[[s]][, 1:5]
      } else {
        new_df[[s]][[t]] <- orig_df[[s]][, 1:4]
      }
    }
    
    # rest of col has the particle followed by its detection limit 
    if(ncol(orig_df[[s]]) > 5){
      for(i in seq(6, ncol(orig_df[[s]]), 2)){
        colNam <- get_names(names(orig_df[[s]])[i], names(orig_df[[s]])[i+1],
                            conv_scheme)
        names(orig_df[[s]])[c(i,i+1)] <- colNam[c(1,2)]
  
        new_df[[s]][[paste(colNam[3])]] <- 
          cbind(new_df[[s]][[paste(colNam[3])]], orig_df[[s]][,c(i, i+1)]) # there must be a better way to do this!!
      
      }
    }
  }
  new_df
}


# Function: convert_pmpart
#---------------------------------
## Inputs: pmpart_df: the pmpart_df that is to be converted
##         pmpart_conversion_file: contains the pmpart column names, new column 
##          names and location/list element in the new format 
## Output: re-fromated data 
##            station -> location/variable type -> data 
## Combines the helper functions to convert the pmpart (pre-2010) data into the 
## 2010 + format 
convert_pmpart <- function(pmpart_df, pmpart_conversion_file){
  new_pm_fine_df <- get_empty_list(pmpart_df, pmpart_conversion_file)
  df <- rename_reformat_pmpart(pmpart_df, new_pm_fine_df, pmpart_conversion_file)
  
  # If a df has 4 columns - remove it (it just has the date columns)
  lapply(df, function(a){list.clean(a, function(x) length(x) == 4L, recursive = FALSE)})
}





# Lets convert! 
pm_fine <- convert_pmpart(pmpart_fine, pmpart_conv_fine)
 
dich_fine <- convert_pmpart(dichot_fine, pmpart_conv_fine)
dich_coarse <- convert_pmpart(dichot_coarse, pmpart_conv_coarse)  



save(file = paste0(wd$output, "pmpart_fine_2010_format.rda"), pm_fine)
save(file = paste0(wd$output, "dichot_coarse_2010_format.rda"), dich_coarse)
save(file = paste0(wd$output, "dichot_fine_2010_format.rda"), dich_fine)




