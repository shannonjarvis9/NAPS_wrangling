# So far we have converted the pre 2010 data into the updated format 
# Now we need to combin everything together 

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


##------------------------------------------------------------------------------
## First step: need to make sure all variables that should be double are 
##------------------------------------------------------------------------------
# Now lets combine;
stations <- unique(c(names(dich_fine), names(pm_fine), names(pm25dat), names(spec)))
df_types <- unique(c(sublist_names(pm25dat), sublist_names(spec), sublist_names(dich_fine), 
              sublist_names(pm_fine)))

NAPS_fine <- vector("list", length(stations))
names(NAPS_fine) <- stations

for(s in names(NAPS_fine)){
  for(t in df_types){ 

    lst <- remove_empty_list(list(dich_fine[[s]][[t]], pm_fine[[s]][[t]], 
                                  pm25dat[[s]][[t]], spec[[s]][[t]]))
    lst_df_names <- unlist(sapply(lst, names))
    common_names <- lst_df_names[duplicated(lst_df_names)]
    NAPS_fine[[s]][[t]] <- lst %>% reduce(full_join, by = common_names)
  
  }
}


unique(a <- lapply(dich_fine, names))


list.merge(dich_fine[["S100119"]], pm_fine[["S100119"]])


list(x, y, z) %>% reduce(full_join, by = "i")
a <-  Map(c, pm25dat, dich_fine, pm_fine, spec)
a <- Map(c, spec, Map(c, dich_fine, pm_fine))
