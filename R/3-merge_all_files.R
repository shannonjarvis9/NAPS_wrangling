# So far we have converted the pre 2010 data into the updated format 
# Now we need to combine everything together 

# We have;
# spec_2010_format.rda
# pmpart_fine_2010_format.rda
# dichot_coarse_2010_format.rda
# dichot_fine_2010_format.rda

# Need to merge with 
# pm10_read.rda
# pm25_read.rda

# lets load all the data 
load(paste0(wd$output, "pm25_read.rda"))
load(paste0(wd$output, "spec_2010_format.rda"))
load(paste0(wd$output, "pmpart_fine_2010_format.rda"))
load(paste0(wd$output, "dichot_fine_2010_format.rda"))

load(paste0(wd$output, "pm10_read.rda"))
load(paste0(wd$output, "dichot_coarse_2010_format.rda"))
load(paste0(wd$output, "spec_coarse_2010_format.rda"))

# Function: sublist_names
#-------------------------------------------------------------------------------
# Get the names of all sublists in mylist 
sublist_names <- function(mylist){unique(unlist(lapply(mylist, names)))}


#Function: remove_empty_list 
#-------------------------------------------------------------------------------
# Remove empty elements in the list 
remove_empty_list <- function(mylist){
  list.clean(lapply(mylist, function(x){Filter(Negate(is.null), x)}) , 
             function(x) length(x) == 0L, recursive = FALSE)
}

## pm10 and pm25 have duplicate dates - need to fix! 


## Remove field blanks for 2010+ data
## Remove the field/text blank samples 
pm10dat <- lapply(pm10dat, lapply, function(x){if(length(x) != 0){
  if("sample_type" %in% names(x)){x %>% filter(! sample_type %in% c("TB", "FB")) %>% select(-sample_type)}
  else if("sampling_type" %in% names(x)){x %>% filter(! sampling_type %in% c("TB", "FB")) %>% select(-sampling_type)} 
  else {x}}})


pm25dat <- lapply(pm25dat, lapply, function(x){if(length(x) != 0){
  if("sample_type" %in% names(x)){x %>% filter(! sample_type %in% c("TB", "FB")) %>% select(-sample_type)}
  else if("sampling_type" %in% names(x)){x %>% filter(! sampling_type %in% c("TB", "FB")) %>% select(-sampling_type)} 
  else {x}}})


##------------------------------------------------------------------------------
## First step: need to make sure all variables that should be numeric are 
##------------------------------------------------------------------------------

# function: getChar_to_Numeric
## ----------------------------------------------------------
## Input: data_list: a list of a list of data frames 
## Output:  data_list with relevant character col converted to numeric 
## Process: filters the name and type of all column to get the names of variables 
##          that should be double based on criteria (col is char & not called 
##          flag, cartridge, media (can be media_t, media_N), sampl
convert_Char_to_Numeric <- function(data_list){
  df <- data.frame(Station = c(), Colname = c(), type = c())
  for(i in names(data_list)){
    for(j in names(data_list[[i]])){
      char_col <- names(data_list[[i]][[j]])[which(sapply(data_list[[i]][[j]], class) == "character")]
      
      convert <- c(grep("flag|cart|media|sampl", char_col, invert = TRUE, value = TRUE),
                   grep("media_T|media_N", char_col, value = TRUE))
      
      data_list[[i]][[j]] <- dplyr::mutate_at(data_list[[i]][[j]], .vars = convert, .funs = as.numeric)
    }
  }
  data_list
}

# apply our function to all the data frames 
pm25dat <- pm25dat %>% convert_Char_to_Numeric()
pm10dat <- pm10dat %>% convert_Char_to_Numeric()
dich_coarse <- dich_coarse %>% convert_Char_to_Numeric()
dich_fine <- dich_fine %>% convert_Char_to_Numeric()
pm_fine <- pm_fine %>% convert_Char_to_Numeric()
spec <- spec %>% convert_Char_to_Numeric()
spec_coarse <- spec_coarse %>% convert_Char_to_Numeric()



#-------------------------------------------------------------------------------
# Now lets combine all fine PM the data frames!!
#-------------------------------------------------------------------------------

stations <- unique(c(names(dich_fine), names(pm_fine), names(pm25dat), names(spec)))
df_types <- unique(c(sublist_names(pm25dat), sublist_names(spec), sublist_names(dich_fine), 
                     sublist_names(pm_fine)))

NAPS_fine <- vector("list", length(stations))
names(NAPS_fine) <- stations

for(s in names(NAPS_fine)){
  for(t in df_types){ 
    
    lst <- remove_empty_list(list(dich_fine[[s]][[t]], pm_fine[[s]][[t]], 
                                  pm25dat[[s]][[t]], spec[[s]][[t]]))
    if(! is_empty(lst)){
      common_names <- data.frame(table(unlist(sapply(lst, names)))) %>%
        dplyr::filter(Freq == length(lst)) %>%
        select(- Freq)
      
      NAPS_fine[[s]][[t]] <- lst %>% reduce(full_join, by = as.vector(common_names[,1]))
    }
  }
}


#-------------------------------------------------------------------------------
# Now lets combine all coarse PM the data frames!!
#-------------------------------------------------------------------------------

stations <- unique(c(names(dich_coarse), names(pm10dat), names(spec_coarse)))
df_types <- unique(c(sublist_names(dich_coarse), sublist_names(pm10dat), 
                     sublist_names(spec_coarse)))

NAPS_coarse <- vector("list", length(stations))
names(NAPS_coarse) <- stations

for(s in names(NAPS_coarse)){
  for(t in df_types){ 
    
    lst <- remove_empty_list(list(dich_coarse[[s]][[t]], pm10dat[[s]][[t]], 
                                  spec_coarse[[s]][[t]]))
    if(! is_empty(lst)){
      common_names <- data.frame(table(unlist(sapply(lst, names)))) %>%
        dplyr::filter(Freq == length(lst)) %>%
        select(- Freq)
      
      NAPS_coarse[[s]][[t]] <- lst %>% reduce(full_join, by = as.vector(common_names[,1]))
    }
  }
}




# So now, we have two lists of lists; 
# NAPS_fine: Station -> File type -> All data 
# NAPS_coarse: Station -> File type -> All data 


#-------------------------------------------------------------------------------
# Now lets rename column using the file type and convert the data to a list;
# Station -> data 
#-------------------------------------------------------------------------------

file_type_conv <- read_excel(paste0(wd$header, "file_type_conv.xlsx"), 
                             col_names = TRUE)

# function: renameDupl
##------------------------------------------------
## Input: df: data frame
##        s: name of the df in the list / file type 
## Output:  renamed df
## Process: renames the columns 
renameCols <- function(df, s){
  row <- which(file_type_conv[,1] == s)
  if(!is_empty(row)){
    names(df)[-c(1:4)] <- paste0(file_type_conv[row,2],"_", names(df)[-c(1:4)])
  }
  df
}


NAPS_fine <- lapply(NAPS_fine, function(a) lapply(seq_along(a),
                                                    function(i) renameCols(a[[i]], names(a)[i])))
NAPS_coarse <- lapply(NAPS_coarse, function(a) lapply(seq_along(a), 
                                                    function(i) renameCols(a[[i]], names(a)[i])))


# Now merge all sublists 
NAPS_coarse <- lapply(NAPS_coarse, function(x){Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("date", "year", "month", "day"), 
                                                                                  all.x = TRUE, all.y = TRUE), x)})

NAPS_fine <- lapply(NAPS_fine, function(x){Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("date", "year", "month", "day"), 
                                                                                 all.x = TRUE, all.y = TRUE), x)})


check_dates <- lapply(NAPS_coarse, function(x){if(length(x) != 0){
  dupl_dates <- nrow(x[duplicated(x$date),])
  if(dupl_dates != 0){print(sprintf("Warning: %i dupl dates", dupl_dates))}}})

check_dates <- lapply(NAPS_fine, function(x){if(length(x) != 0){
  dupl_dates <- nrow(x[duplicated(x$date),])
  if(dupl_dates != 0){print(sprintf("Warning: %i dupl dates", dupl_dates))}}})


# station_to_fix <-  list.clean(lapply(check_dates, function(x){Filter(Negate(is.null), x)}), 
#                              function(x) length(x) == 0L, recursive = FALSE)





## Save the data frames 
save(file = paste0(wd$output, "NAPS_fine.rda"), NAPS_fine)
save(file = paste0(wd$output, "NAPS_coarse.rda"), NAPS_coarse)
























