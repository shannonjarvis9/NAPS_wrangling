

## getting diff data frames for each tpye 
##----------------------------------------------------------------------------
## wb <- createWorkbook()
## 
## 
## for(t in 1:length(pm10dat[[1]])){
##   type <- pm10dat[[1]][[t]]
##   
##   for(s in 2:length(pm10dat)){
##     if(length(pm10dat[[s]][[t]] ) != 0){
##       #type <- merge(type, dat[[s]][[t]], all.x = TRUE, all.y = TRUE) 
##       type <- bind_rows(type, pm10dat[[s]][[t]])
##     }
##   }
##   
##   assign(paste0("all_", names(pm10dat)[t], "_station_merged"), type)
##   
##   addWorksheet(wb, paste0(names(pm10dat[[1]])[t]))
##   writeData(wb, paste0(names(pm10dat[[1]])[t]), t(names(type)), startRow = 1, startCol = 1)
## }
## 
## saveWorkbook(wb, file = "PM10_header.xlsx", overwrite = TRUE)

wb <- createWorkbook()


for(t in 1:length(pm25dat[[1]])){
  type <- pm25dat[[1]][[t]]
  
  for(s in 2:length(pm25dat)){
    if(length(pm25dat[[s]][[t]] ) != 0){
      #type <- merge(type, pm25dat[[s]][[t]], all.x = TRUE, all.y = TRUE) 
      type <- bind_rows(type, pm25dat[[s]][[t]])
    }
  }
  
  assign(paste0("all_", names(pm25dat)[t], "_station_merged"), type)
  
  addWorksheet(wb, paste0(names(pm25dat[[1]])[t]))
  writeData(wb, t, t(names(type)), startRow = 1, startCol = 1)
}

saveWorkbook(wb, file = "PM25_header_nocart.xlsx", overwrite = TRUE)




wb <- createWorkbook()


for(t in 1:length(spec[[1]])){
  type <- spec[[1]][[t]]
  
  for(s in 2:length(spec)){
    if(length(spec[[s]][[t]] ) != 0){
      type <- merge(type, spec[[s]][[t]], all.x = TRUE, all.y = TRUE) 
      #type <- bind_rows(type, spec[[s]][[t]])
    }
  }
  
  assign(paste0("all_", names(spec)[t], "_station_merged"), type)
  
  addWorksheet(wb, paste0(names(spec[[1]])[t]))
  writeData(wb, t, t(names(type)), startRow = 1, startCol = 1)
}

saveWorkbook(wb, file = "spec_header_fixicpms.xlsx", overwrite = TRUE)


# Generate pmpart header dict - match to PM10
nm <-c()
for(s in names(pmpart)){
  if("na" %in% names(pmpart[[s]])){
    print(sprintf("station %s", s))
  }
  nm <- c(nm, names(pmpart[[s]]))
}
nm <- str_subset(unique(nm), "d_l", negate = TRUE)
write.xlsx(t(nm), "pmpart_headers.xlsx")