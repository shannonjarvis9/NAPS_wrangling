source("~/NAPS_project/NAPS_wrangling/R/0-setup_project.R")
load(file = paste0(wd$output, "NAPS_fine.rda"))
load(file = paste0(wd$output, "NAPS_coarse.rda"))

names_Vec <- vector(length= 0L)
for(i in 1:length(NAPS_fine)){
  names_Vec <- unique(c(names_Vec, names(NAPS_fine[[i]])))
}
removed <- names_Vec[grep("_2", names_Vec)]
names_Vec <- names_Vec[-grep("flag", names_Vec)]
names_Vec <- names_Vec[-grep("_2", names_Vec)]
nms <- data.frame(old_name = names_Vec, new_name = vector(length = length(names_Vec)))



mutate_using_mdl <- function(df, col, colmdl){
  col <- as.character(col)
  colmdl <- as.character(colmdl)
  df %>% mutate(!!rlang::ensym(col) := ifelse(!is.na(!!rlang::ensym(col)) & (!!rlang::ensym(col)  < !!rlang::ensym(colmdl)) ,
                                              0.5*!!rlang::ensym(colmdl), !!rlang::ensym(col) ))
}



df_nonmdl_names <- grep("mdl", names_Vec, invert = TRUE, value = TRUE)
df_nonmdl_names <- df_nonmdl_names[-c(1:4)]
compon_mdl_corr <- names_Vec
df_mdl_names <- grep("mdl", names_Vec, invert = FALSE, value = TRUE) 

for(i in 1:length(df_nonmdl_names)){
  
  idx <- df_mdl_names[which(paste0(df_nonmdl_names[i], "_mdl") == df_mdl_names)]
  
  split_name <- strsplit(df_nonmdl_names[i], "_")[[1]]
  idx_other <- grep(paste0(split_name[1], "_", split_name[2], "_", split_name[4]), df_mdl_names, value = TRUE)
  idx_other2 <- grep(paste0(split_name[2], "_", split_name[3]), df_mdl_names, value = TRUE)
  idx_other2b <- grep(paste0(split_name[1], "_", split_name[3]), df_mdl_names, value = TRUE)
  idx_other2c <- grep(paste0(split_name[1], "_", split_name[2], "_", split_name[3], "_", split_name[5]), df_mdl_names, value = TRUE)
  idx_other3 <- grep(paste0(split_name[3], "_", split_name[4], "_", split_name[5]), df_mdl_names, value = TRUE)
  idx_other4 <- grep(paste0(split_name[3], "_", split_name[4]), df_mdl_names, value = TRUE)
  idx_other5 <- grep(paste0(split_name[5], "_", split_name[6]), df_mdl_names, value = TRUE)
  idx_other6 <- grep(paste0(split_name[5]), df_mdl_names, value = TRUE)
  idx_other7 <- grep(paste0("_", split_name[6], "_"), df_mdl_names, value = TRUE)
  idx_other8 <- grep(paste0(split_name[1], "_", split_name[2], "_", split_name[3], "_", split_name[4], "_", split_name[6]), df_mdl_names, value = TRUE)
  
  mdl_col <- ifelse(length(idx) == 1L, idx, 
                    ifelse(length(idx_other) == 1L, idx_other,
                           ifelse(length(idx_other2) == 1L, idx_other2, 
                                  ifelse(length(idx_other2b) == 1L, idx_other2b,
                                         ifelse(length(idx_other2c) == 1L, idx_other2c,
                                  ifelse(length(idx_other3) == 1L, idx_other3, 
                                         ifelse(length(idx_other4) == 1L, idx_other4, 
                                                ifelse(length(idx_other5) == 1L, idx_other5, 
                                                       ifelse(length(idx_other6) == 1L, idx_other6, 
                                                              ifelse(length(idx_other7) == 1L, idx_other7,
                                                                     ifelse(length(idx_other8) == 1L, idx_other8, NA)))))))))))
  
  if(df_nonmdl_names[i] == "EDXRF_dich_sulphur_s"){
    mdl_col <- "EDXRF_dich_s_mdl"
  } else if(df_nonmdl_names[i] == "Ions_IC_spec_nf_manganese"){
    mdl_col <- "Ions_IC_spec_nf_mn_mdl"
  }else if(df_nonmdl_names[i] == "Ions_IC_spec_tf_manganese"){
    mdl_col <- "Ions_IC_spec_tf_mn_mdl"
  }
  
  
  if(is.na(mdl_col)){
    print(sprintf("Unable to find mdl col, %s, %i", df_nonmdl_names[i], i))
  } else {
    #names_Vec <- mutate_using_mdl(names_Vec, df_nonmdl_names[i], mdl_col)
    mdl_idx <- which(names_Vec == mdl_col)
    nms[mdl_idx, "new_name"] <- paste0(df_nonmdl_names[i], "_mdl")
  }
}


nms[which(nms$old_name == "EDXRF_dich_ni_mdl"),2] <- "EDXRF_dich_nickel_ni_mdl"
nms[which(nms$old_name == "EDXRF_s_mdl"),2] <- "EDXRF_sulphur_s_mdl"
nms[which(nms$old_name == "EDXRF_spec_s_mdl"),2] <- "EDXRF_spec_sulphur_s_mdl"
nms[which(nms$old_name == "EDXRF_spec_ni_mdl"),2] <- "EDXRF_spec_nickel_ni_mdl"
nms[which(nms$old_name == "Acidic_IC_spec_sulphur_dioxide_mdl"),2] <- "Acidic_IC_spec_sulphur_dioxide_mdl"


which(is.na(nms[grep("mdl", nms$old_name), 2] )) # all MDL col are matched 

nms <- nms %>% 
  mutate(new_name = ifelse(new_name == FALSE, old_name, new_name))




# Now i want to reformat the new name using periods and filters 
nms$new_name2 <- nms$new_name


nms$new_name2[grep("Metals_ICPMS_WS_", nms$new_name)] <- sub("Metals_ICPMS_WS_", "na.ICPMS_WS.TF.", nms$new_name, ignore.case = TRUE)[grep("Metals_ICPMS_WS_", nms$new_name)] 
nms$new_name2[grep("Metals_ICPMS_WS_dich_", nms$new_name)] <- sub("Metals_ICPMS_WS_dich_", "dich.ICPMS_WS.TF.", nms$new_name, ignore.case = TRUE)[grep("Metals_ICPMS_WS_dich_", nms$new_name)]
nms$new_name2[grep("Metals_ICPMS_WS_spec_", nms$new_name)] <- sub("Metals_ICPMS_WS_spec_", "spec.ICPMS_WS.TF.", nms$new_name, ignore.case = TRUE)[grep("Metals_ICPMS_WS_spec_", nms$new_name)]

nms$new_name2[grep("Metals_ICPMS_NT_", nms$new_name)] <- sub("Metals_ICPMS_NT_", "na.ICPMS_NT.TF.", nms$new_name, ignore.case = TRUE)[grep("Metals_ICPMS_NT_", nms$new_name)]
nms$new_name2[grep("Metals_ICPMS_NT_dich_", nms$new_name)] <- sub("Metals_ICPMS_NT_dich_", "dich.ICPMS_NT.TF.", nms$new_name, ignore.case = TRUE)[grep("Metals_ICPMS_NT_dich_", nms$new_name)]
nms$new_name2[grep("Metals_ICPMS_NT_spec_", nms$new_name)] <- sub("Metals_ICPMS_NT_spec_", "spec.ICPMS_NT.TF.", nms$new_name, ignore.case = TRUE)[grep("Metals_ICPMS_NT_spec_", nms$new_name)]

nms$new_name2[grep("Ions_IC_", nms$new_name)] <- sub("Ions_IC_", "na.IC.na.", nms$new_name, ignore.case = TRUE)[grep("Ions_IC_", nms$new_name)] 
nms$new_name2[grep("Ions_IC_tf_", nms$new_name)] <- sub("Ions_IC_tf_", "na.IC.TF.", nms$new_name, ignore.case = TRUE)[grep("Ions_IC_tf_", nms$new_name)]
nms$new_name2[grep("Ions_IC_nf_", nms$new_name)] <- sub("Ions_IC_nf_", "na.IC.NF.", nms$new_name, ignore.case = TRUE)[grep("Ions_IC_nf_", nms$new_name)]


nms$new_name2[grep("Ions_IC_dich_", nms$new_name)] <- sub("Ions_IC_dich_", "dich.IC.na.", nms$new_name, ignore.case = TRUE)[grep("Ions_IC_dich_", nms$new_name)] 
nms$new_name2[grep("Ions_IC_dich_tf_", nms$new_name)]  <- sub("Ions_IC_dich_tf_", "dich.IC.TF.", nms$new_name, ignore.case = TRUE)[grep("Ions_IC_dich_tf_", nms$new_name)]
nms$new_name2[grep("Ions_IC_dich_nf_", nms$new_name)]  <- sub("Ions_IC_dich_nf_", "dich.IC.NF.", nms$new_name, ignore.case = TRUE)[grep("Ions_IC_dich_nf_", nms$new_name)]

nms$new_name2[grep("Ions_IC_spec_", nms$new_name)] <- sub("Ions_IC_spec_", "spec.IC.na.", nms$new_name, ignore.case = TRUE)[grep("Ions_IC_spec_", nms$new_name)] 
nms$new_name2[grep("Ions_IC_spec_tf_", nms$new_name)] <- sub("Ions_IC_spec_tf_", "spec.IC.TF.", nms$new_name, ignore.case = TRUE)[grep("Ions_IC_spec_tf_", nms$new_name)]
nms$new_name2[grep("Ions_IC_spec_nf_", nms$new_name)] <- sub("Ions_IC_spec_nf_", "spec.IC.NF.", nms$new_name, ignore.case = TRUE)[grep("Ions_IC_spec_nf_", nms$new_name)]




# EDXRF 
nms$new_name2[grep("EDXRF_", nms$new_name)] <- sub("EDXRF_", "na.EDXRF.TF.", nms$new_name, ignore.case = TRUE)[grep("EDXRF_", nms$new_name)] 
nms$new_name2[grep("EDXRF_dich_", nms$new_name)] <- sub("EDXRF_dich_", "dich.EDXRF.TF.", nms$new_name, ignore.case = TRUE)[grep("EDXRF_dich_", nms$new_name)]
nms$new_name2[grep("EDXRF_spec_", nms$new_name)] <- sub("EDXRF_spec_", "spec.EDXRF.TF.", nms$new_name, ignore.case = TRUE)[grep("EDXRF_spec_", nms$new_name)]


nms$new_name2[grep("Nitrate_IC_spec_", nms$new_name)] <- sub("Nitrate_IC_spec_", "spec.V_IC.NF.", nms$new_name, ignore.case = TRUE)[grep("Nitrate_IC_spec_", nms$new_name)]

nms$new_name2[grep("OCEC_spec_", nms$new_name)] <- sub("OCEC_spec_", "spec.TOR.QF.", nms$new_name, ignore.case = TRUE)[grep("OCEC_spec_", nms$new_name)]

nms$new_name2[grep("Biomass_IC_spec_", nms$new_name)] <- sub("Biomass_IC_spec_", "spec.IC.TF.", nms$new_name, ignore.case = TRUE)[grep("Biomass_IC_spec_", nms$new_name)]

nms$new_name2[grep("Ammonia_IC_spec_", nms$new_name)] <- sub("Ammonia_IC_spec_", "spec.IC.CAD.", nms$new_name, ignore.case = TRUE)[grep("Ammonia_IC_spec_", nms$new_name)]

nms$new_name2[grep("Acidic_IC_spec_", nms$new_name)] <- sub("Acidic_IC_spec_", "spec.IC.SCD.", nms$new_name, ignore.case = TRUE)[grep("Acidic_IC_spec_", nms$new_name)]


# now change the ones that didn't change 
idx <- which(nms$new_name == nms$new_name2)

nms$new_name2[idx] <- gsub(" ", "", nms$new_name2[idx])
nms$new_name2[idx] <- gsub("dich_", "dich.", nms$new_name2[idx])
nms$new_name2[idx] <- gsub("spec_", "spec.", nms$new_name2[idx])

nms[which(nms$old_name == "dich_pm2_5_mdl"),"new_name2"] <- "dich.pm2_5_mdl"


write.csv(nms[,c(1,3)], file = paste0(wd$header, "final_fine_names.csv"))

final_fine_names <- read_csv("header_files/final_fine_names.csv")

NAPS_renamed <- vector("list", length = length(NAPS_fine))
names(NAPS_renamed) <- names(NAPS_fine)
for(i in 1:length(NAPS_fine)){
  NAPS_fine[[i]] <- NAPS_fine[[i]] %>% 
         select(-contains("flag"))
  df <- data.frame(idx = 1:nrow(NAPS_fine[[i]]))
  col <- 1
  for(j in 1:ncol(NAPS_fine[[i]])){
    if(names(NAPS_fine[[i]])[j] %in% final_fine_names$old_name){
      df$new_col <- NAPS_fine[[i]][, j]
      names(df)[col+1] <- final_fine_names[which(final_fine_names$old_name == names(NAPS_fine[[i]])[j]), "new_name2"]
      col <- col+1
    }
  }
  NAPS_renamed[[i]] <- df %>% select(-1) %>% 
    mutate(date = as.Date(date))
  gc()
}

save(file = paste0(wd$output, "NAPS_renamed.rda"), NAPS_renamed)

