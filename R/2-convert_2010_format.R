# Need to develop a scheme to convert the 2010 and before data to the 
# format used after 2010  
library(tidyr)
library(stringr)
library(openxlsx)
library(plyr)
library(dplyr)
library(rlist)

# Load the data from the previous step
#-------------------------------------------------------------------------------
# 2003-2010 data 
load(paste0(wd$output, "spec_read.rda"))
load(paste0(wd$output, "pmpart_read.rda"))
load(paste0(wd$output, "dichot_read.rda"))

# 2010+ data 
load(paste0(wd$output, "pm10_read.rda"))
load(paste0(wd$output, "pm25_read.rda"))


# Need to make some manual fixes to the spec data - certain stations had 
# formatting issues 
##----------------------------------------------------------------------------
spec[["S60427"]][["CARB"]] <- spec[["S60427"]][["CARB"]] %>% 
  mutate(cart = x0) %>%
  select(-matches('x0'))

spec[["S60211"]][["CARB"]] <- spec[["S60211"]][["CARB"]] %>% 
  mutate(speciation_mass_ug_m3 =  mas_sp) %>%
  select(-matches('mas_sp'))

for(s in c("S31001", "S70301", "S105001", "S106600")){
  spec[[s]][["WICPMS"]] <- spec[[s]][["WICPMS"]] %>% 
    mutate(cartridge =  f_c) %>%
    select(-matches('f_c'))
}

for(s in c("S70301", "S105001", "S106600")){
  spec[[s]][["WICPMS"]] <- spec[[s]][["WICPMS"]] %>% 
    mutate(mass_ug_m3 = mass) %>%
    select(-"mass")
  
}

spec[["S100702"]][["ICPMS"]] <- spec[["S100702"]][["ICPMS"]]%>% 
  select(-starts_with('na'))

for(s in names(spec)){
  if(length(spec[[s]][["NA"]]) != 0){
    spec[[s]][["NA"]] <- spec[[s]][["NA"]]%>% 
      mutate(media = ifelse(media == "y", "C", media))
  }
  
  if(length(spec[[s]][["ICPMS"]]) != 0){
    spec[[s]][["ICPMS"]] <- spec[[s]][["ICPMS"]]%>% 
      select(-c(contains('id')))
  }
}




##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
# Prepare 2003-2009 data to combine with 2010+ data 
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------


# Remove field blank observations to a different list
##----------------------------------------------------------------------------
field_blanks <- lapply(spec, lapply, function(x){if(length(x) != 0){
  if("cart" %in% names(x)){x %>% filter(cart %in% c("TB", "FB", "F"))}
  else if("cartridge" %in% names(x)){x %>% filter(cartridge %in% c("TB", "FB", "F"))} 
  else if("media" %in% names(x)){x %>% filter(media %in% c("TB", "FB", "F"))}}})

spec <- lapply(spec, lapply, function(x){if(length(x) != 0){
  if("cart" %in% names(x)){x %>% filter(! cart %in% c("TB", "FB", "F"))}
  else if("cartridge" %in% names(x)){x %>% filter(! cartridge %in% c("TB", "FB", "F"))} 
  else if("media" %in% names(x)){x %>% filter(! media %in% c("TB", "FB", "F"))} 
  else {x}}})



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



# Dealing with duplicate dates from each data frame
# ICPMS has pm25 and pm10 and total data 
# --------------------------------------------------------------------
##----------------------------------------------------------------------------
icpms_coarse <- vector("list", length(spec))
names(icpms_coarse) <- names(spec)
icpms_fine <- icpms_total <- icpms_coarse

for(i in 1:length(spec)){
  # Rename carb using cart 
  if(length(spec[[i]][["CARB"]] ) != 0){
    spec[[i]][["CARB"]] <-  pivot_wider(spec[[i]][["CARB"]], names_from = "cart", 
                            names_glue = "cart_{cart}_{.value}", names_prefix = "cart_",
                            values_from = c("speciation_mass_ug_m3", "oc1", "oc2",
                            "oc3", "oc4", "poc_r", "poc_t","oc_r", "oc_t","ec1", 
                            "ec2","ec3", "ec_t" ,"ec_r","tc", "oc_dl" ,"ec_dl")) 
  }

  # Rename IC using media 
  if(length(spec[[i]][["IC"]] ) != 0){
    spec[[i]][["IC"]] <-  pivot_wider(spec[[i]][["IC"]], names_from = "media", 
                                        names_glue = "media_{media}_{.value}", names_prefix = "media_",
                                        values_from = names(spec[[i]][["IC"]])[7:ncol(spec[[i]][["IC"]])])
  }
  
  # Remove the coarse and total ICPMS data - keep the fine data 
  if(length(spec[[i]][["ICPMS"]] ) != 0){
    icpms_coarse[[i]] <- spec[[i]][["ICPMS"]] %>% 
                        filter(fraction == "C") %>% 
                        select(-"fraction")
    
    icpms_total[[i]] <- spec[[i]][["ICPMS"]] %>% 
      filter(fraction == "T") %>% 
      select(-"fraction")
    
    icpms_fine[[i]] <- spec[[i]][["ICPMS"]] %>% 
      filter(fraction == "F") %>% 
      select(-"fraction")
    

    spec[[i]][["ICPMS"]] <- spec[[i]][["ICPMS"]] %>% 
                            filter(fraction == "F") %>% 
                            select(-"fraction")
  }
}




# Check that each date in the spec file appears only once in a data frame
check_dates <- lapply(spec, lapply, function(x){if(length(x) != 0){
        dupl_dates <- nrow(x[duplicated(x$date),])
        if(dupl_dates != 0){print(sprintf("Error: %i dupl dates", dupl_dates))}}})


# Now spec has one observation per date 
# It is organized as spec -> station -> file type 
##----------------------------------------------------------------------------




# Rename pmpart & dichot s.t. it matches the PM10 data 
##----------------------------------------------------------------------------
pmpart_conv_fine <- read_excel(paste0(wd$header, "pmpart_pm25_headers.xlsx"), 
                               col_names = FALSE)
pmpart_conv_coarse <- read_excel(paste0(wd$header,"pmpart_pm10_headers.xlsx"), 
                                 col_names = FALSE)

pmpart_fine <- lapply(pmpart, function(x){x %>% filter(c_f == "F") %>%
                                          dplyr::mutate(pm2_5 = mass, .after = day) %>%
                                          select(-c("c_f", "mass"))})

pmpart_coarse <- lapply(pmpart, function(x){x %>% filter(c_f == "C") %>%
                                            dplyr::mutate(pm2_5_10 = mass, .after = day) %>%
                                            select(-c("c_f", "mass"))})

check_dates <- lapply(pmpart_coarse, function(x){if(length(x) != 0){
  dupl_dates <- nrow(x[duplicated(x$date),])
  if(dupl_dates != 0){print(sprintf("Error: %i dupl dates", dupl_dates))}}})


chect_dates <- lapply(pmpart_fine,function(x){if(length(x) != 0){
  dupl_dates <- nrow(x[duplicated(x$date),])
  if(dupl_dates != 0){print(sprintf("Error: %i dupl dates", dupl_dates))}}})


# now check dichot
##----------------------------------------------------------------------------
# Remove duplicate dates- manually checked distinct can be used here
# The duplicate date( second instance) has same & less info than the first instance
dichot_fine <- lapply(dichot, function(x){x %>% filter(c_f == "F") %>%
    dplyr::mutate(pm2_5 = mass, .after = day) %>%
    select(-c("c_f", "mass")) %>%
    distinct(date, .keep_all = TRUE)})


dichot_coarse <- lapply(dichot, function(x){x %>% filter(c_f == "C") %>%
    dplyr::mutate(pm2_5_10 = mass, .after = day) %>%
    select(-c("c_f", "mass"))  %>%
    distinct(date, .keep_all = TRUE)})


check_dates <- lapply(dichot_fine, function(x){if(length(x) != 0){
  dupl_dates <- nrow(x[duplicated(x$date),])
  if(dupl_dates != 0){print(sprintf("Error: %i dupl dates", dupl_dates))}}})


chect_dates <- lapply(dichot_coarse,function(x){if(length(x) != 0){
  dupl_dates <- nrow(x[duplicated(x$date),])
  if(dupl_dates != 0){print(sprintf("Error: %i dupl dates", dupl_dates))}}})



##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
## Merge pmpart_coarse and pm10 -> all_pm10
## Merge pmpart_fine and pm25 -> all_pm25
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
# Iterate through the df, renaming columns and merging to combine all dates 
# in all_pm10

#pm part has detection limit columns - each d_l column corresponds
# to the detection limit of the previous col 



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
    return(c(col_name, paste0(col_name, "mdl"), "No file")) 
  }else if(is.na(pmpart_conv[2,col])){
    return(c(col_name, paste0(col_name, "mdl"), "No file")) 
  }else {
    c(pmpart_conv[2,col], pmpart_conv[3,col], pmpart_conv[4,col])
  }
  
}


all_pm25 <- pm25dat

for(s in names(pmpart_fine)){
  elem_df <- ic_df <- pmpart_fine[[s]][,1:4]
  other <- pmpart_fine[[s]][,1:5]
  
  if(ncol(pmpart_fine[[s]]) > 5){
    for(i in seq(6, ncol(pmpart_fine[[s]]), 2)){
      colNam <- get_names(names(pmpart_fine[[s]])[i], names(pmpart_fine[[s]])[i+1],
                          pmpart_conv_fine)
      names(pmpart_fine[[s]])[c(i,i+1)] <- colNam[c(1,2)]
      
      if(is.na(colNam[3])){
        other[paste(colNam[1])] <- pmpart_fine[[s]][i] 
        other[paste(colNam[2])] <- pmpart_fine[[s]][i+1]
      } else if(colNam[3] == "Elements_EXDXRF"){
        elem_df[paste(colNam[1])] <- pmpart_fine[[s]][i] 
        elem_df[paste(colNam[2])] <- pmpart_fine[[s]][i+1] 
      }
      else if(colNam[3] == "Ions-Spec_IC"){
        ic_df[paste(colNam[1])] <-pmpart_fine[[s]][i] 
        ic_df[paste(colNam[2])] <-pmpart_fine[[s]][i+1] 
      }
    }
  }
  
  all_pm25[[s]][["PM2.5"]] <-  merge(all_pm25[[s]][["PM2.5"]], other,
                                        all.x = TRUE, all.y = TRUE, 
                                        by = intersect(names(all_pm25[[s]][["PM2.5"]]), names(other)))
  if(ncol(elem_df) > 4){
    if(length(all_pm25[[s]][["Elements_EXDXRF"]]) == 0){
      all_pm25[[s]][["Elements_EXDXRF"]] <- elem_df
    } else {
      all_pm25[[s]][["Elements_EXDXRF"]] <-  merge(all_pm25[[s]][["Elements_EXDXRF"]], elem_df,
                                                 all.x = TRUE, all.y = TRUE, 
                                                 by = intersect(names(all_pm25[[s]][["Elements_EXDXRF"]]), names(elem_df)))
    }
  }
  if(ncol(ic_df) > 4){
    if(length(all_pm25[[s]][["Ions-Spec_IC"]]) == 0){
      all_pm25[[s]][["Ions-Spec_IC"]] <- ic_df
    }else{
      all_pm25[[s]][["Ions-Spec_IC"]] <-  merge(all_pm25[[s]][["Ions-Spec_IC"]], ic_df,
                                              all.x = TRUE, all.y = TRUE, 
                                              by = intersect(names(all_pm25[[s]][["Ions-Spec_IC"]]), names(ic_df)))
    }
  }
  
  
  print(sprintf("Successfully added pre-2010 data from station %s.",s))
}



# currently expect duplicates as pmpart_fine contains duplicates 
check_dates <- lapply(all_pm25, lapply, function(x){if(length(x) != 0){
  dupl_dates <- nrow(x[duplicated(x$date),])
  if(dupl_dates != 0){print(sprintf("Error: %i dupl dates", dupl_dates))}}})




all_pm10 <- pm10dat

for(s in names(pmpart_coarse)){
  elem_df <- ic_df <- pmpart_coarse[[s]][,1:4]
  
  if(ncol(pmpart_coarse[[s]]) > 5){
    for(i in seq(6, ncol(pmpart_coarse[[s]]), 2)){
      colName <- get_names(names(pmpart_coarse[[s]])[i], names(pmpart_coarse[[s]])[i+1],
                         pmpart_conv_coarse)
      names(pmpart_coarse[[s]])[c(i,i+1)] <- colName[c(1,2)]
   
      if(colName[3] == "Elements_EXDXRF"){
        elem_df[paste(colName[1])] <- pmpart_coarse[[s]][i] 
        elem_df[paste(colName[2])] <- pmpart_coarse[[s]][i+1] 
      }
      if(colName[3] == "Ions-Spec_IC"){
        ic_df[paste(colName[1])] <-pmpart_coarse[[s]][i] 
        ic_df[paste(colName[2])] <-pmpart_coarse[[s]][i+1] 
      }
    }
  }
  
  all_pm10[[s]][["PM2.5-10"]] <-  merge(all_pm10[[s]][["PM2.5-10"]], pmpart_coarse[[s]][,1:5],
                                        all.x = TRUE, all.y = TRUE, 
                                        by = intersect(names(all_pm10[[s]][["PM2.5-10"]]), names(pmpart_coarse[[s]][,1:5])))
  if(ncol(elem_df) > 4){
    if(length(all_pm10[[s]][["Elements_EXDXRF"]]) == 0){
      all_pm10[[s]][["Elements_EXDXRF"]] <- elem_df
    } else {
      all_pm10[[s]][["Elements_EXDXRF"]] <-  merge(all_pm10[[s]][["Elements_EXDXRF"]], elem_df,
                                               all.x = TRUE, all.y = TRUE, 
                                               by = intersect(names(all_pm10[[s]][["Elements_EXDXRF"]]), names(elem_df)))
    }
  }
  if(ncol(ic_df) > 4){
    if(length(all_pm10[[s]][["Ions-Spec_IC"]]) == 0){
      all_pm10[[s]][["Ions-Spec_IC"]] <- ic_df
    }else{
      all_pm10[[s]][["Ions-Spec_IC"]] <-  merge(all_pm10[[s]][["Ions-Spec_IC"]], ic_df,
                                              all.x = TRUE, all.y = TRUE, 
                                          by = intersect(names(all_pm10[[s]][["Ions-Spec_IC"]]), names(ic_df)))
    }
  }

  
  print(sprintf("Successfully added pre-2010 data from station %s.",s))
}


check_dates <- lapply(all_pm10, lapply, function(x){if(length(x) != 0){
  dupl_dates <- nrow(x[duplicated(x$date),])
  if(dupl_dates != 0){print(sprintf("Error: %i dupl dates", dupl_dates))}}})


##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
## Rename dichot cols
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

# now lets rename the variab 
for(s in names(dichot_fine)){
  if(ncol(dichot_fine[[s]]) > 5){
    for(i in seq(6, ncol(dichot_fine[[s]]), 2)){
      colNam <- get_names(names(dichot_fine[[s]])[i], names(dichot_fine[[s]])[i+1],
                          pmpart_conv_fine)
      #print(sprintf("Exisitng names %s, new names %s ", names(dichot_fine[[s]])[c(i,i+1)] , colNam[c(1,2)]))
      names(dichot_fine[[s]])[c(i,i+1)] <- colNam[c(1,2)]
      
    }
  }
  #all_pm25[[s]][["dichot"]] <- dichot_fine[[s]]
  #print(sprintf("Successfully added pre-2010 data from station %s.",s))
}

for(s in names(dichot_coarse)){
  if(ncol(dichot_coarse[[s]]) > 5){
    for(i in seq(6, ncol(dichot_coarse[[s]]), 2)){
      colNam <- get_names(names(dichot_coarse[[s]])[i], names(dichot_coarse[[s]])[i+1],
                          pmpart_conv_coarse)
      names(dichot_coarse[[s]])[c(i,i+1)] <-  colNam[c(1,2)]
      
    }
  }
  #all_pm10[[s]][["dichot"]] <- dichot_coarse[[s]]
  #print(sprintf("Successfully added pre-2010 data from station %s.",s))
}


##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
## Merge spec and pm25 -> all_pm25
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

filename <-paste0(wd$header, "spec_header_fixicpms.xlsx")
sheets <- getSheetNames(filename)
spec_header_dict <- lapply(sheets, read.xlsx, xlsxFile = filename)
names(spec_header_dict) <- sheets


for(s in names(spec)){
  for(t in names(spec[[s]])){
    new_file_loc <- na.omit(unique(as.character(spec_header_dict[[t]][3,])))
    
    if(length(spec[[s]][[t]]) != 0){
      if(ncol(spec[[s]][[t]]) > 5){

        df <- spec[[s]][[t]][,1:4]
        
        for(i in 5:ncol(spec[[s]][[t]])){
          colNam <- get_names(names(spec[[s]][[t]])[i], "Null_mdl", spec_header_dict[[t]])
          names(spec[[s]][[t]])[i] <- colNam[1]
          df[paste(colNam[1])] <- spec[[s]][[t]][i]
        }
        df <- Filter(function(x)!all(is.na(x)), df)
        
        if(ncol(df) > 4){
          if(length(all_pm25[[s]][[new_file_loc]]) == 0){
            all_pm25[[s]][[new_file_loc]] <- df
          }else{
            all_pm25[[s]][[new_file_loc]] <-  merge(all_pm25[[s]][[new_file_loc]], df,
                                                      all.x = TRUE, all.y = TRUE, 
                                                      by = unique(c("date", "day", "month", "year", intersect(names(all_pm25[[s]][[new_file_loc]]), names(df)))))
          }
        }
      }
    }
  }
  print(sprintf("Successfulled added Station %s",s))
}


check_dates <- lapply(all_pm25, lapply, function(x){if(length(x) != 0){
  dupl_dates <- nrow(x[duplicated(x$date),])
  if(dupl_dates != 0){print(sprintf("Error: %i dupl dates", dupl_dates))}}})

# for some stations, there is issue with merging (due to cols being NA), lets fix 
station_to_fix <-  list.clean(lapply(check_dates, function(x){Filter(Negate(is.null), x)}), 
                        function(x) length(x) == 0L, recursive = FALSE)

compress <- function(x) c(na.omit(x), NA)[1]

for(s in names(station_to_fix)){
  df <- all_pm25[[s]][["PM2.5"]]
  mass_df <- aggregate(df$pm2_5, df[1:4], compress) # issue is due to the mass column 
  names(mass_df)[5] <-  "pm2_5"
  rest_df <- aggregate(df[-c(1:4,7)], df[1:4], compress)
  all_pm25[[s]][["PM2.5"]] <- merge(mass_df, rest_df, by = c("date", "day", "month", "year"))
}



check_dates <- lapply(all_pm25, lapply, function(x){if(length(x) != 0){
  dupl_dates <- nrow(x[duplicated(x$date),])
  if(dupl_dates != 0){print(sprintf("Error: %i dupl dates", dupl_dates))}}})



## Now lets deal with icpms_coarse - use header dict from spec to merge with all_pm10

for(s in names(icpms_coarse)){
    new_file_loc <- na.omit(unique(as.character(spec_header_dict[["ICPMS"]][3,])))
    
    if(length(icpms_coarse[[s]]) != 0){
      if(ncol(icpms_coarse[[s]]) > 5){
        df <- icpms_coarse[[s]][,1:4]
        
        for(i in 6:ncol(icpms_coarse[[s]])){
          colNam <- get_names(names(icpms_coarse[[s]])[i], "NA_mdl", spec_header_dict[["ICPMS"]])
          names(icpms_coarse[[s]])[i] <- colNam[1]
          df[paste(colNam[1])] <- icpms_coarse[[s]][i]
        }
        if(ncol(df) > 4){
          if(length(all_pm10[[s]][[new_file_loc]]) == 0){
            all_pm10[[s]][[new_file_loc]] <- df
          }else{
            all_pm10[[s]][[new_file_loc]] <-  merge(all_pm10[[s]][[new_file_loc]], df,
                                                    all.x = TRUE, all.y = TRUE, 
                                                    by = intersect(names(all_pm10[[s]][[new_file_loc]]), names(df)))
          }
        }
        print(sprintf("Successfulled added Station %s",s))
      }
    }
}



check_dates <- lapply(all_pm10, lapply, function(x){if(length(x) != 0){
  dupl_dates <- nrow(x[duplicated(x$date),])
  if(dupl_dates != 0){print(sprintf("Error: %i dupl dates", dupl_dates))}}})


# currently expect duplicates as pmpart_fine contains duplicates 
check_dates <- lapply(all_pm25, lapply, function(x){if(length(x) != 0){
  dupl_dates <- nrow(x[duplicated(x$date),])
  if(dupl_dates != 0){print(sprintf("Error: %i dupl dates", dupl_dates))}}})


for(i in names(all_pm10)){
  for(j in names(all_pm10[[i]])){
    if(nrow(all_pm10[[i]][[j]]) == 0){
      all_pm10[[i]][[j]] <- NULL
    }
  }
}

for(i in names(all_pm25)){
  for(j in names(all_pm25[[i]])){
    if(nrow(all_pm25[[i]][[j]]) == 0){
      all_pm25[[i]][[j]] <- NULL
    }
  }
}

# Now we have two data frames - all_pm10 and all_pm25 
##----------------------------------------------------------------------------
all_pm10 <- lapply(all_pm10, function(x){Filter(Negate(is.null), x)}) 
all_pm25 <- lapply(all_pm25, function(x){Filter(Negate(is.null), x)}) 

save(file = "all_pm10.rda", all_pm10)
save(file = "all_pm25.rda", all_pm25)




# Now lets check the data frames before merging
## Check columns named appropriately 
## Check columns with the same name are the same type 
##------------------------------------------------------------------------------

#get a list of the column names 
pm10names <-  list.clean(lapply(all_pm10 , function(a){lapply(a, names)}),
                             function(x) length(x) < 2, recursive = FALSE)
pm25names <- list.clean(lapply(all_pm25 , function(a){lapply(a, names)}),
                             function(x) length(x) < 2, recursive = FALSE)



# function: getDupl
##----------------------------------------------------
## Input: df: list of data frames 
## output: vector of duplicate names
## Process: extracts all duplicate column names from a list 
getDupl <- function(df){
dupl <- c()
  for(i in 1:length(df)){
    vec <- c()
    for(j in 1:length(df[[i]])){vec <- c(vec, df[[i]][[j]])}
    dupl <- c(dupl, unique(vec[duplicated(vec)]))
  }
  dupl <- unique(dupl)
  dupl[!(dupl %in% c("date", "year", "month", "day"))]
}

duplicate_names_pm10 <- getDupl(pm10names)
duplicate_names_pm25 <- getDupl(pm25names)


# function: renameDupl
##------------------------------------------------
## Input: df: data frame
##        duplicate_vect: contains names of duplicate rows in the list 
##        s: name of the df in the list 
## Output:  renamed df
## Process: renames the duplicate columns for each station using the 
##          file names/name of list index 
renameDupl <- function(df, duplicate_vect, s){
  renameCols <- which(names(df) %in% duplicate_vect)
  names(df)[renameCols] <- paste0(s,"_", names(df)[renameCols])
  df
}
# rename the duplicate cols (except for date, day, month, year)
pm10_renamed <- lapply(all_pm10, function(a) lapply(seq_along(a),
                                                    function(i) renameDupl(a[[i]], duplicate_names_pm10, names(a)[i])))
pm25_renamed <- lapply(all_pm25, function(a) lapply(seq_along(a), 
                                                    function(i) renameDupl(a[[i]], duplicate_names_pm25, names(a)[i])))



combin_pm10 <- lapply(pm10_renamed, function(x){Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("date", "year", "month", "day"), 
                                                                              all.x = TRUE, all.y = TRUE), x)})


combin_pm25 <- lapply(pm25_renamed, function(x){Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("date", "year", "month", "day"), 
                                                                              all.x = TRUE, all.y = TRUE), x)})


# check if there are same names of different type in the df 
##----------------------------------------------------------- 

# function: getChar_to_Numeric
## ----------------------------------------------------------
## Input: data_list: a list of data frames 
## Output: a data frame containing the column name and list index of 
##         variables that should be converted from numeric to double 
## Process: obtains the name and type of all column names and filters
##          to get the names of variables that should be double 
getChar_to_Numeric <- function(data_list){
  df <- data.frame(Station = c(), Colname = c(), type = c())
  for(i in names(data_list)){
    df2 <- data.frame(Station =  rep(i, length(names(data_list[[i]]))) , 
                      Colname = names(data_list[[i]]),
                      type = sapply(data_list[[i]], class)) 
    if(nrow(df) == 0){ df <- df2} else {
    df <- merge(df, df2, all = TRUE) }
  }

  df_conv <- df %>%
    filter(type == "character") 
  
  # we don't want media_T or media_N but want all other instances of media 
  rows <- which(grepl("flag|cartridge|media",df_conv$Colname))[! which(grepl("flag|cartridge|media",df_conv$Colname)) %in%
               which(grepl("media_T|media_N",df_conv$Colname))] 
  
  df_conv[-rows,]
}


df_conv_pm10 <- getChar_to_Numeric(combin_pm10)
df_conv_pm25 <- getChar_to_Numeric(combin_pm25)


for(x in 1:nrow(df_conv_pm10)){combin_pm10[[df_conv_pm10[x,1]]][,df_conv_pm10[x,2]] <- 
                        as.numeric(combin_pm10[[df_conv_pm10[x,1]]][,df_conv_pm10[x,2]])}

for(x in 1:nrow(df_conv_pm25)){combin_pm25[[df_conv_pm25[x,1]]][,df_conv_pm25[x,2]] <-
                        as.numeric(combin_pm25[[df_conv_pm25[x,1]]][,df_conv_pm25[x,2]])}





# Save the output! 
save(file = paste0(wd$output, "all_pm10_dat.rda"), combin_pm10)
save(file = paste0(wd$output, "all_pm25_dat.rda"), combin_pm25)
