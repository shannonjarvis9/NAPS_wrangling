load(paste0(wd$output, "spec_read.rda"))



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
  
  if(length(spec[[s]][["LEV"]]) != 0){
    spec[[s]][["LEV"]] <- spec[[s]][["LEV"]]%>% 
      select(-c(contains('media')))
  }
}






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





# Dealing with duplicate dates from each data frame
# ICPMS has pm25 and pm10 and total data 
# --------------------------------------------------------------------
##----------------------------------------------------------------------------
#try making this code nicer 
icpms_coarse <- vector("list", length(spec))
names(icpms_coarse) <- names(spec)


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
                                      names_glue = "media_{media}_{.value}", names_prefix = "media_",
                                      values_from = names(spec[[i]][["IC"]])[7:ncol(spec[[i]][["IC"]])])
  }
  
  # Remove the coarse and total ICPMS data - keep the fine data 
  if(length(spec[[i]][["ICPMS"]] ) != 0){
    icpms_coarse[[i]] <- spec[[i]][["ICPMS"]] %>% 
      filter(fraction == "C") %>% 
      select(-"fraction")
    
    # icpms_total[[i]] <- spec[[i]][["ICPMS"]] %>% 
    #   filter(fraction == "T") %>% 
    #   select(-"fraction")
    
    
    spec[[i]][["ICPMS"]] <- spec[[i]][["ICPMS"]] %>% 
      filter(fraction == "F") %>% 
      select(-"fraction")
  }
}



# Check that each date in the spec file appears only once in a data frame
check_dates <- lapply(spec, lapply, function(x){if(length(x) != 0){
  dupl_dates <- nrow(x[duplicated(x$date),])
  if(dupl_dates != 0){print(sprintf("Error: %i dupl dates", dupl_dates))}}})


##----------------------------------------------------------------------------
# Now spec has one observation per date 
# It is organized as spec -> station -> file type 
##----------------------------------------------------------------------------


## Now lets rename spec 

## Function: get_names
##---------------------------------
## Inputs: col_name: column name to match with 2010+ col names 
##         pmpart_conv: df with the conversion structure 
## Outputs: vector containing the new column names and the new file type
get_names <- function(col_name, conversion_df) {
  col <- which(col_name == conversion_df[1, ])
  
  if ((is.integer(col) &&
       length(col) == 0L) | is.na(conversion_df[2, col])) {
    return(col_name)
  } else {
    return(conversion_df[2, col])
  }
}


##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
## Rename and reformat spec using the header dictionary/conversion file 
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

filename <-paste0(wd$header, "spec_header_fixicpms.xlsx")
sheets <- getSheetNames(filename)
spec_header_dict <- lapply(sheets, read.xlsx, xlsxFile = filename)
names(spec_header_dict) <- sheets


# can replace loops with lapply 
for(s in names(spec)){
  for(t in names(spec[[s]])){
    
    if(length(spec[[s]][[t]]) != 0){
        for(i in 1:ncol(spec[[s]][[t]])){
          names(spec[[s]][[t]])[i] <- get_names(names(spec[[s]][[t]])[i], spec_header_dict[[t]])
      
      }
    }
    type_idx <- which(names(spec[[s]]) == t)
    names(spec[[s]])[[type_idx]] <- na.omit(unique(as.character(spec_header_dict[[t]][3,])))
  }
}





save(file = paste0(wd$output, "spec_2010_format.rda"), spec)

