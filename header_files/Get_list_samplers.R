# Reads smapler files to find the sampler name/type 

## Make my excel file 
getSampler <- function(dir){
  
  files <- list.files(path = paste0(wd$data, dir), full.names = TRUE)
  file_names <- list.files(path = paste0(wd$data, dir), full.names = FALSE)
  station <- unlist(lapply(strsplit(file_names, "_"), "[[", 2))
  year <- unlist(lapply(strsplit(file_names, "_|\\."), "[[", 1)) 
  
  sampler_dict <- data.frame(Description = c(), Sampler = c(), 
                             SamplerNum = c(), Station = c(), Year = c())
  
  for(i in 1:length(files)){
    tmp <- suppressMessages(read_excel(files[i], sheet = "Station Info", col_names = FALSE,
                                       na = c("NA","", " ", "-", "-999","-999.000"), trim_ws = TRUE))
    
    c1 <- tmp %>% pull(`...1`)
    row <- grep("Sampler #", c1)
    
    samplers<- tmp[row+1,]
    samplers$sample <- str_match_all(c1[row], "(?<=\\().+?(?=\\))")
    samplers$station <- rep(station[i], length(row))
    samplers$year <- rep(year[i], length(row))
    names(samplers) <- c("Description","Sampler", "SamplerNum", "Station", "Year") 
    
    
    sampler_dict <- rbind(sampler_dict, samplers)
    rm(samplers)
  }
  
  all_spec_dich <- filter(sampler_dict, grepl('dich|spec', sampler_dict$Sampler, ignore.case = TRUE))
  all_others <- filter(sampler_dict, !grepl('dich|spec', sampler_dict$Sampler, ignore.case = TRUE)) %>% 
    mutate(type = "PM")
  
  all_spec_dich <- mutate(all_spec_dich, type = ifelse(
    grepl('spec', all_spec_dich$Sampler, ignore.case = TRUE), "Spec", "Dichot"))
  
  all_info <- rbind(all_spec_dich, all_others)
  all_info$dir <- dir
  
  all_info %>%
    select(-c(Description))
  
}

## Directory 1 
dir <- "IntegratedPM2.5"
sampler_dict_pm25 <- getSampler(dir)
  
  

  
## Directory 2   
dir <- "IntegratedPM2.5-10"
sampler_dict_pm10 <- getSampler(dir)
  

dir <- "PMSPECIATION"
dir <- "PMPART25"
dir <- "PMDICHOT"

getSamplerType <- function(dir){
  files <- list.files(path = paste0(wd$data, dir), full.names = TRUE)
  file_names <- list.files(path = paste0(wd$data, dir), full.names = FALSE)
  station <- unlist(lapply(strsplit(file_names, "_"), "[[", 2))
  year <- unlist(lapply(strsplit(file_names, "_|\\."), "[[", 1)) 
  
  sampler_dict <- data.frame(Sampler = c(), Station = c(), Year = c()) 

  for(i in 1:length(files)){
    tmp <- data.frame(Sampler = read.xls(files[i], header = FALSE, nrows = 1L)[1,1],
                      Station = station[i],
                      Year = year[i])
    
    sampler_dict <- rbind(sampler_dict, tmp)

  }
  
  sampler_dict
}
  

## Directory 3 
pmpart_sampler <- getSamplerType("PMPART25")

pmpart_sampler$Sampler <- sub(" at .*", "", pmpart_sampler$Sampler)
pmpart_sampler$Sampler[70] <- "Partisol PM2.5 Sampler Concentrations (ug/m3)"
pmpart_sampler$dir <- "PMPART25"
pmpart_sampler$type <- "PM"


## Directory 4
  
pmdichot_sampler <- getSamplerType("PMDICHOT")

pmdichot_sampler$Sampler <- sub(" at .*", "", pmdichot_sampler$Sampler)
pmdichot_sampler$dir <- "PMDICHOT"
pmdichot_sampler$type <- "Dichot"

  
## Directory 5 
  
# for spec - we know th sampler is spec - files dont give much info 
# pmspec_sampler <- getSamplerType("PMSPECIATION") - doesn't have sampler info

file_names <- list.files(path = paste0(wd$data, "PMSPECIATION"), full.names = FALSE)
station <- unlist(lapply(strsplit(file_names, "_"), "[[", 2))
year <- unlist(lapply(strsplit(file_names, "_|\\."), "[[", 1)) 

pmspec_sampler <- data.frame(Sampler = "Spec",
                             Station = unlist(lapply(strsplit(file_names, "_"), "[[", 2)),
                             Year = unlist(lapply(strsplit(file_names, "_|\\."), "[[", 1)),
                             dir = "PMSPECIATION",
                             type = "Spec")

# have a df fro each sampler; sampler_dict_pm25, sampler_dict_pm10, pmpart_sampler,
# pmdichot_sampler, pmspec_sampler 

pre2010 <- rbind(pmpart_sampler, pmdichot_sampler, pmspec_sampler)
pre2010$SamplerNum <- NA

post2010 <- rbind(sampler_dict_pm25, sampler_dict_pm10)

all_info <- rbind(pre2010, post2010)

write.xlsx(all_info, paste0(wd$output, "all_sampler_info.xlsx"))
