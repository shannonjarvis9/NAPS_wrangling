library(tidyr)
library(rlist)
library(janitor)

# 2a-convert_spec.R
# In this script, the list of spec data (2003-2010 data) is converted to the 
# appropriate format (2010+ data format) for merging in subsequent scripts 

# Manual fixes are preformed, field blanks are removed, columns are renamed using
# cartridges and the header dictionary 


# Setup/load the necessary files 
# ------------------------------------------------------------------------------
source('~/NAPS_project/NAPS_wrangling/R/0-setup_project.R') # setup the working dir

load(paste0(wd$output, "spec_read.rda"))



# Load the header dictionary (sheets correspond to names of file types in the spec data)
# The header dictionary was manually created, its format is as follows;
  # A sheet for each file type (i.e. CARB, IC, NH4...)
  # Within each sheet, 1st row: column names in the pre 2010 data
  #                    2nd row: corresponding col names for the 2010+ data
  #                    3rd row: new file type name (may be missing for some - that's okay!)
  #                    4th row: column type (i.e. numeric, char...) not used, may be missing 
filename <-paste0(wd$header, "spec_header.xlsx")
sheets <- getSheetNames(filename)
spec_header_dict <- lapply(sheets, read.xlsx, xlsxFile = filename)
names(spec_header_dict) <- sheets



##----------------------------------------------------------------------------
# Need to make some manual fixes to the spec data - certain stations had 
# formatting issues 
##----------------------------------------------------------------------------
spec[["S60427"]][["CARB"]] <- spec[["S60427"]][["CARB"]] %>% 
  mutate(cart = x0) %>%
  select(-matches('x0'))

spec[["S60211"]][["CARB"]] <- spec[["S60211"]][["CARB"]] %>% 
  mutate(speciation_mass_ug_m3 =  mas_sp) %>%
  select(-matches('mas_sp'))


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
  
  if(length(spec[[s]][["WICPMS"]]) != 0){ # media col is always T or NA 
    spec[[s]][["WICPMS"]] <- spec[[s]][["WICPMS"]]%>% 
      select(-c(contains('media')))
  }
  
  if(length(spec[[s]][["LEV"]]) != 0){
    spec[[s]][["LEV"]] <- spec[[s]][["LEV"]]%>% 
      select(-c(contains('media')))
  }
  
}


# Remove field blank observations to a different list
##----------------------------------------------------------------------------
field_blanks <- lapply(spec, lapply, function(x){if(length(x) != 0){
  if("cart" %in% names(x)){x %>% filter(cart %in% c("TB", "FB"))}
  else if("cartridge" %in% names(x)){x %>% filter(cartridge %in% c("TB", "FB"))} 
  else if("media" %in% names(x)){x %>% filter(media %in% c("TB", "FB"))}}})

spec <- lapply(spec, lapply, function(x){if(length(x) != 0){
  if("cart" %in% names(x)){x %>% filter(! cart %in% c("TB", "FB"))}
  else if("cartridge" %in% names(x)){x %>% filter(! cartridge %in% c("TB", "FB"))} 
  else if("media" %in% names(x)){x %>% filter(! media %in% c("TB", "FB")) } 
  else {x}}})




# Dealing with duplicate dates from each data frame
# Renaming CARB using cartridge, SPEC using media/filter
# ICPMS, WICPMS has fine and coarse data 
##----------------------------------------------------------------------------
icpms_coarse <- vector("list", length(spec))
names(icpms_coarse) <- names(spec)
wicpms_coarse <- icpms_coarse


for(i in 1:length(spec)){
  
  # Rename carb using cartridge 
  if(length(spec[[i]][["CARB"]] ) != 0){
    spec[[i]][["CARB"]] <-  pivot_wider(spec[[i]][["CARB"]], names_from = "cart", 
                                        names_glue = "cart_{cart}_{.value}", names_prefix = "cart_",
                                        values_from = c("speciation_mass_ug_m3", "oc1", "oc2",
                                                        "oc3", "oc4", "poc_r", "poc_t","oc_r", "oc_t","ec1", 
                                                        "ec2","ec3", "ec_t" ,"ec_r","tc", "oc_dl" ,"ec_dl")) 
  }
  
  # Rename IC using media (nylon/teflon filter)
  if(length(spec[[i]][["IC"]] ) != 0){
    spec[[i]][["IC"]] <-  pivot_wider(spec[[i]][["IC"]], names_from = "media", 
                                      names_glue = "{media}F_{.value}",
                                      values_from = names(spec[[i]][["IC"]])[7:ncol(spec[[i]][["IC"]])])
  }
  if(length(field_blanks[[i]][["IC"]] ) != 0){
    field_blanks[[i]][["IC"]] <-  pivot_wider(field_blanks[[i]][["IC"]], names_from = "media", 
                                              names_glue = "{media}F_{.value}",
                                              values_from = names(field_blanks[[i]][["IC"]])[7:ncol(field_blanks[[i]][["IC"]])])
  }
  
  
  # ICPMS has fine, coarse and fine data
  if(length(spec[[i]][["ICPMS"]] ) != 0){
    icpms_coarse[[i]] <- spec[[i]][["ICPMS"]] %>% 
      filter(fraction == "C") %>% 
      select(-"fraction")
    
    spec[[i]][["ICPMS"]] <- spec[[i]][["ICPMS"]] %>% 
      filter(fraction == "F") %>% 
      select(-"fraction")
  }
  
  # WICPMS has fine and coarse data
  if(length(spec[[i]][["WICPMS"]] ) != 0 & "f_c" %in% names(spec[[i]][["WICPMS"]])){
    wicpms_coarse[[i]] <- spec[[i]][["WICPMS"]] %>% 
      filter(f_c == "C") %>% 
      select(-"f_c")
    
    
    spec[[i]][["WICPMS"]] <- spec[[i]][["WICPMS"]] %>% 
      filter(f_c == "F") %>% 
      select(-"f_c")
  }
}



# clean up the lists (remove empty elements)
wicpms_coarse <- list.clean(wicpms_coarse, fun = function(x) nrow(x) == 0L || length(x) == 0L) 
icpms_coarse <- list.clean(icpms_coarse, fun = function(x) nrow(x) == 0L || length(x) == 0L) 


# Remove the cartridge/media columns - all important info from these cols has been used
spec <- lapply(spec, lapply, function(x){if(length(x) != 0){
  if("cart" %in% names(x)){x %>%  select(-cart)}
  else if("cartridge" %in% names(x)){x %>% select(-cartridge)} 
  else if("media" %in% names(x)){x %>% select(-media)} 
  else {x}}})

field_blanks <- lapply(field_blanks, lapply, function(x){if(length(x) != 0){
  if("cart" %in% names(x)){x %>%  select(-cart)}
  else if("cartridge" %in% names(x)){x %>% select(-cartridge)} 
  else if("media" %in% names(x)){x %>% select(-media)} 
  else {x}}})


##----------------------------------------------------------------------------
# Now spec has one observation per date 
# It is organized as spec -> station -> file type 

# Lets rename so it can be merged with the 2010+ data
##----------------------------------------------------------------------------





##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
## Rename and reformat spec using the header dictionary/conversion file 
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

## Function: get_names
##---------------------------------
## Inputs: col_name: column name to match with 2010+ col names 
##         conversion_df: df with the conversion structure 
## Outputs: the new column names if it exists
get_names <- function(col_name, conversion_df) {
  col <- which(col_name == conversion_df[1,])
  
  if ((is.integer(col) &&
       length(col) == 0L) | is.na(conversion_df[2, col])) {
    return(tolower(col_name))
  } else {
    return(tolower(conversion_df[2, col]))
  }
}


## Function: rename_df
##---------------------------------
## Inputs: df- list of list whose names are being converted
##         header_dict- name conversion scheme, a list
## Process: iterates through the input list (df) to rename each column and list
##          to match the format of the header dictionary 
rename_df <- function(df, header_dict, type) {
  for (s in names(df)) {
    for (t in names(df[[s]])) {
      if (length(df[[s]][[t]]) != 0) {
        for (i in 1:ncol(df[[s]][[t]])) {
          names(df[[s]][[t]])[i] <- 
            get_names(names(df[[s]][[t]])[i], header_dict[[t]])
        }
        # Add the sampler type to the names
        names(df[[s]][[t]])[5:ncol(df[[s]][[t]])] <- paste0(type, "_",
                                                            names(df[[s]][[t]])[5:ncol(df[[s]][[t]])])
      }
      # Rename the file type for the list 
      type_idx <- which(names(df[[s]]) == t)
      names(df[[s]])[[type_idx]] <-
        na.omit(unique(as.character(header_dict[[t]][3, ])))
    }
  }
  df 
}




spec <- rename_df(spec, spec_header_dict, "spec")
field_blanks <- rename_df(field_blanks, spec_header_dict, "spec")


##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
## Create, combine, rename and reformat the coarse data  
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
# Create a new list to hold the data (will only contain WICPMS, ICPMS)
spec_coarse <- vector("list")

# add wicpms coarse to the coarse spec data 
for(s in names(wicpms_coarse)){
    for(i in 1:ncol(wicpms_coarse[[s]])){
      names(wicpms_coarse[[s]])[i] <- get_names(names(wicpms_coarse[[s]])[i], spec_header_dict[["WICPMS"]])
    }
    spec_coarse[[s]][[na.omit(unique(as.character(spec_header_dict[["WICPMS"]][3,])))]] <- wicpms_coarse[[s]]
}

# add icpms
for(s in names(icpms_coarse)){
  for(i in 1:ncol(icpms_coarse[[s]])){
    names(icpms_coarse[[s]])[i] <- get_names(names(icpms_coarse[[s]])[i], spec_header_dict[["ICPMS"]])
  }
  spec_coarse[[s]][[na.omit(unique(as.character(spec_header_dict[["ICPMS"]][3,])))]] <- icpms_coarse[[s]]
}



##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
## Save the files 
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
save(file = paste0(wd$output, "spec_2010_format.rda"), spec)
save(file = paste0(wd$output, "spec_coarse_2010_format.rda"), spec_coarse)
save(file = paste0(wd$output, "spec_fieldblanks_2010_format.rda"), field_blanks)



