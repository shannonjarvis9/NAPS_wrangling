library("ggplot2")

# Load the data we already created
# ------------------------------------------------------------------------------
load(paste0(wd$output, "all_pm10_dat.rda"))
load(paste0(wd$output, "all_pm25_dat.rda"))
load(paste0(wd$output, "dichot_fine.rda"))

# Get the station metadata
#-------------------------------------------------------------------------------
NAPS_metadata <- read.csv(paste0(wd$data, "edited_StationsNAPS-StationsSNPA.csv")) %>%
  mutate(station = paste0("S", NAPS)) %>%
  clean_names  %>%
  filter(naps %in% c(30113, 40801, 50104, 54401, 60211, 60427, 62601, 90132, 
                     103202, 101004, 100119)) %>%
  mutate(city = c("Halifax ", "Canterbury", "Montréal ", "Saint-Anicet" , 
                  "Windsor ", "Toronto ", "Simcoe ", "Edmonton ", "Burnaby", 
                  "Abbotsford", "Golden"))

# set city to factor so they will be displayed in the correct order 
NAPS_metadata$city <- factor(NAPS_metadata$city, levels=rev(c("Halifax ", "Canterbury", 
                             "Montréal ", "Saint-Anicet" , "Toronto ", "Simcoe ",
                             "Windsor ", "Edmonton ", "Golden", "Abbotsford", "Burnaby")))



pm25_bind <- bind_rows(combin_pm25, .id = "station") %>%
  clean_names()

# filter for the relevant sites and relevant years 
spec_R_03_08 <- pm25_bind %>%
  filter( 2003 <= year & year <= 2008) %>%
  filter( station %in% c("S30113", "S40801", "S50104", "S54401", "S60211", 
                         "S60427", "S62601", "S90132", "S103202", "S101004", "S100119")) 

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Table 2: Approx num obsv as number of spec obsv - numbers are diff 
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Lets get count of spec mass obsv by stations 
spec_R_03_08 %>%
  select("station", "speciation_mass_ug_m3")%>%
  group_by(station) %>%
  dplyr::summarise(n = n())

spec_data <- merge(spec_R_03_08, NAPS_metadata, by = "station")
spec_data <- spec_data[,which(unlist(lapply(spec_data, function(x) !all(is.na(x)))))]


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Figure 2 
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

ggplot(data = spec_data, mapping = aes(x = city, y = speciation_mass_ug_m3)) + 
  geom_boxplot() +
  ylim(0, 80) + 
  labs(y =   expression(PM[2.5] ~ Mass ~ (mu ~ g/m^3)), x = " ")  + 
  scale_x_discrete(labels = function(labels) {
    fixedLabels <- c()
    for (l in 1:length(labels)) {
      fixedLabels[l] <- paste0(ifelse(l %% 2 == 0, '', '\n'), labels[l])
    }
    return(fixedLabels)
  })

#outliers for burnaby & edmonton missing 


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Figure 3 
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

monthly_station <- spec_data %>%
  mutate(month_text = month(date, label = TRUE, abbr = TRUE)) %>%
  dplyr::select("city", "speciation_mass_ug_m3", "month", "month_text")%>%
  group_by(city, month, month_text) %>%
  dplyr::summarise(mean = mean(speciation_mass_ug_m3, na.rm = TRUE), n = n(),
                   sd = sd(speciation_mass_ug_m3, na.rm = TRUE)) %>%
  mutate(CI = qnorm(0.95)*(sd/sqrt(n)))



ggplot(data = monthly_station, mapping = aes(x = month_text, y = mean, group = 1)) + 
  geom_point( ) + 
  geom_line() +
  facet_wrap(~ city, nrow = 3) + 
  ylim(0, 35) +
  labs(y =   expression(Total ~ Mass ~ From ~ Spectation ~ Sampler ~ (mu ~ g/m^3)), x = "Month")  + 
  geom_errorbar(aes(x = month_text, ymin=mean-CI, ymax=mean+CI), colour="black", width=.1) +
  scale_x_discrete(labels= c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Figure 4
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


# need to get the field blank data 

oc_station <- spec_data %>%
  mutate(mean_oc_cart_a = cart_a_oc_a) %>%
  mutate(mean_oc_cart_b = cart_b_oc_b) %>%
  mutate(passiveTravel_fieldBlank = cart_a_poc_a + cart_A_poc_t) %>%
  dplyr::select("city", "mean_oc_cart_a", "mean_oc_cart_b",  
                "passiveTravel_fieldBlank") %>%
  group_by(city) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE)  %>%
  pivot_longer(!city, names_to = "type", values_to = "val")




ggplot(oc_station, mapping = aes(x = city, y = val, fill = type)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_discrete(labels = c("Mean OC (QA)", "Mean OC Active Blank (QB)", 
                                 "Passive Travel and Field Blank")) + 
  theme(legend.position = c(0.75, 0.825), legend.title = element_blank()) + 
  labs(y =   expression(Total ~ OC ~ Concentration ~ (mu ~ g/m^3)), x = "")  + 
  scale_x_discrete(labels = function(labels) {
    fixedLabels <- c()
    for (l in 1:length(labels)) {
      fixedLabels[l] <- paste0(ifelse(l %% 2 == 0, '', '\n'), labels[l])
    }
    return(fixedLabels)
  })



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Table 4: Compound composition 
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
names(spec_data)[grep("amm", names(spec_data))]
# Need to add dichot data - contains soil info for some sites 
# has warning message: duplicate cols & new names - but occurs for cols that don't matter 
dichot_data <- bind_rows(dichot_fine, .id = "station") %>%
  clean_names() %>%
  filter( 2003 <= year & year <= 2008) %>%
  filter(station %in% c("S30113", "S50104", "S60211", "S60427", "S62601", 
                        "S90132", "S103202", "S101004", "S100119")) %>%
  merge(NAPS_metadata, by = "station") %>%
  select('city','date', 'month', silicon_si, calcium_ca, iron_fe, potassium_k, titanium_ti) 

dichot_data$SOIL <- 3.48*dichot_data$silicon_si + 1.63*dichot_data$calcium_ca + 
  1.63*dichot_data$iron_fe + 1.41*dichot_data$potassium_k +
  1.94*dichot_data$titanium_ti

# now lets get the columns needed from spec data 
comp_station <- spec_data %>%
  select('city','date', 'month', media_t_nitrate, media_t_sulphate, media_t_ammonium,
         cart_b_oc_b, media_t_potassium, silicon_si, calcium_ca, elements_exdxrf_iron_fe,  
         elements_exdxrf_titanium_ti, cart_a_ec_a, media_t_sodium, media_t_chloride, 
         cart_c_ammonia)  


# Ammonium Nitrate (NH4NO3) = [ANO3] = 1.29[NO3 ] # should be teflon filter 
      # Hence, Teflon nitrate was used for mass reconstruction
      # and the Nylon nitrate was neglected for mass reconstruction
      # purposes.
comp_station$ANO3_dat <- 1.29*(comp_station$media_t_nitrate)

# Ammonium sulphates = [ASO4] = [SO4] + [NH4] - 0.29[NO3]
comp_station$ASO4_dat <- comp_station$media_t_sulphate +  
                         comp_station$media_t_ammonium - 
                         0.29*(comp_station$media_t_nitrate)


  
# Organic Matter = [OM] = k[OC] 
comp_station$OM_dat <-comp_station$cart_b_oc_b

# Elemental carbon = [EC] 
comp_station$EC_dat <- comp_station$cart_a_ec_a

# Crustal matter = [SOIL] = 3.48[Si] + 1.63[Ca] + 2.42[Fe] + 1.41[K] + 1.94[Ti]
comp_station$SOIL = 3.48*comp_station$silicon_si + 1.63*comp_station$calcium_ca + 
                   1.63*comp_station$elements_exdxrf_iron_fe + 1.41*comp_station$media_t_potassium +
                   1.94*comp_station$elements_exdxrf_titanium_ti

# soil info is only present for two sites - can be fond in the dich data - lets add
comp_station <- left_join(comp_station, dichot_data[, c("city", "date", "month", "SOIL")], 
                              by = c("city", "date", "month"))

comp_station$SOIL_dat <- coalesce(comp_station2$SOIL.x, comp_station2$SOIL.y)
comp_station <- comp_station %>% select(-c("SOIL.x", "SOIL.y"))

# Sodium chloride = [NaCl] = [Na] + [Cl]
comp_station$NaCl_dat <- comp_station$media_t_sodium + comp_station$media_t_chloride

#Particle-bound water = [PBW] = 0.32 ([SO4] + [NH4 ])
comp_station$PBW_dat <- 0.32*(comp_station$media_t_sulphate + comp_station$media_t_ammonium)


comp_station_april_sept <- comp_station %>%
  filter(4 <= month & month <= 9) %>% 
  dplyr::select("city", "NaCl", "SOIL", "EC", "OM", "ANO3", "ASO4", "PBW")%>%
  group_by(city) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  pivot_longer(!city, names_to = "type", values_to = "val")


comp_station_oct_mar <- comp_station %>%
  filter(month <= 3 | month >= 10) %>% 
  dplyr::select("city", "NaCl", "SOIL", "EC", "OM", "ANO3", "ASO4", "PBW") %>%
  group_by(city) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  pivot_longer(!city, names_to = "type", values_to = "val")




ggplot(comp_station_april_sept, mapping = aes(x = city, y = val, 
        fill = factor(type, levels = c("NaCl", "SOIL", "EC", "OM", "ANO3", "ASO4", "PBW")))) + 
  geom_bar(stat="identity", position=position_stack()) + 
  #scale_fill_discrete(labels = c("Mean OC (QA)", "Mean OC Active Blank (QB)", 
  #                               "Passive Travel and Field Blank")) + 
  ylim(0,20) + 
  theme(legend.position = c(0.8, 0.825), legend.title = element_blank()) + 
  labs(y =   expression(Total ~ OC ~ Concentration ~ (mu ~ g/m^3)), x = "")  + 
  scale_x_discrete(labels = function(labels) {
    fixedLabels <- c()
    for (l in 1:length(labels)) {
      fixedLabels[l] <- paste0(ifelse(l %% 2 == 0, '', '\n'), labels[l])
    }
    return(fixedLabels)
  })



ggplot(comp_station_oct_mar, mapping = aes(x = city, y = val, 
        fill = factor(type, levels = c("NaCl", "SOIL", "EC", "OM", "ANO3", "ASO4", "PBW")))) + 
  geom_bar(stat="identity", position=position_stack()) + 
  #scale_fill_discrete(labels = c("Mean OC (QA)", "Mean OC Active Blank (QB)", 
  #                               "Passive Travel and Field Blank")) + 
  scale_y_continuous(limits = c(0, 20), breaks = seq(0,20,2)) + 
  theme(legend.position = c(0.8, 0.825), legend.title = element_blank()) + 
  labs(y =   expression(Total ~ OC ~ Concentration ~ (mu ~ g/m^3)), x = "")  + 
  scale_x_discrete(labels = function(labels) {
    fixedLabels <- c()
    for (l in 1:length(labels)) {
      fixedLabels[l] <- paste0(ifelse(l %% 2 == 0, '', '\n'), labels[l])
    }
    return(fixedLabels)
  })




vars <- spec_data %>%
  select('city','date', 'month', media_t_nitrate, media_t_sulphate, media_t_ammonium,
         cart_b_oc_b,
         cart_c_ammonia,
         silicon_si, calcium_ca, elements_exdxrf_iron_fe, potassium_k,  elements_exdxrf_titanium_ti,
         "cart_a_ec_a", media_t_sodium, media_t_chloride)  %>%
  mutate(across(!city, as.numeric)) %>%
  group_by(city) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) 



"sodium" 

names(spec_data)[grep("so", names(spec_data))]

names(spec_10data)[grep("ch", names(spec_10data))]




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Figure 7: Ammonia Sulphate and Nitrate 
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# calculate our summary statisitcs 
ammon_compnd <- comp_station %>%
  mutate(month_text = month(date, label = TRUE, abbr = TRUE)) %>%
  select(city, ASO4_dat, ANO3_dat, month_text) %>%
  group_by(city, month_text) %>%
  dplyr::summarise(ASO4 = median(ASO4_dat, na.rm = TRUE),  
                   ANO3 = median(ANO3_dat, na.rm = TRUE),
                   lwr_quantile_aso4 = quantile(ASO4_dat, na.rm = TRUE, probs = 0.25),
                   upr_quantile_aso4 = quantile(ASO4_dat, na.rm = TRUE, probs = 0.75),
                   lwr_quantile_ano3 = quantile(ANO3_dat, na.rm = TRUE, probs = 0.25),
                   upr_quantile_ano3 = quantile(ANO3_dat, na.rm = TRUE, probs = 0.75)) 


# need to structure the df (city, month, compound, value, lower IQR, upper IQR) 
# for easy plotting - there is probably a much easier way to do this! 
#------------------------------------------------------------------------------
# start by putting the IQR in the correct format 
ano3_IQR <- ammon_compnd %>%
  select(c(city, month_text, lwr_quantile_ano3, upr_quantile_ano3)) %>%
  mutate(compound = "ANO3") %>%
  dplyr::rename(lower_iqr = lwr_quantile_ano3,  upper_iqr = upr_quantile_ano3)

aso4_IQR <- ammon_compnd %>%
  select(c(city, month_text, lwr_quantile_aso4, upr_quantile_aso4)) %>%
  mutate(compound = "ASO4") %>%
  dplyr::rename(lower_iqr = lwr_quantile_aso4,  upper_iqr = upr_quantile_aso4)

ammon_iqr <- bind_rows(ano3_IQR, aso4_IQR)

# Then merge the IQR back with the original df 
ammon_compnd_median <- ammon_compnd %>%
  select(c(city, month_text, ASO4, ANO3)) %>%
  pivot_longer(!c(city, month_text), names_to = "compound", values_to = "median") 

ammonia_data <- merge(ammon_compnd_median, ammon_iqr, by = c("city", "month_text", "compound"))



# lets plot! 
ggplot(data = ammonia_data, mapping = aes(x = month_text, y = median, colour = compound, linetype = compound,
                                          group = compound, ymin = lower_iqr, ymax = upper_iqr)) + 
  geom_point() + 
  geom_line() +
  geom_errorbar() +
  facet_wrap(~ city, nrow = 3) + 
  scale_y_continuous(limits = c(0, 12), breaks = seq(0,12,2)) + 
  scale_linetype_manual(values = c("ANO3" = "dashed", "ASO4" = "solid")) + 
  labs(y =   expression(paste("Ammonium Sulphate/Ammonium Nitrate (",mu ,"g/",m^3,")")), x = "Month")  + 
  scale_x_discrete(labels= c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))





#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Figure 8: EC and OC
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# calculate our summary statisitcs 
carb_compnd <- comp_station %>%
  mutate(month_text = month(date, label = TRUE, abbr = TRUE)) %>%
  select(city, EC_dat, OM_dat, month_text) %>%
  group_by(city, month_text) %>%
  dplyr::summarise(EC = median(EC_dat, na.rm = TRUE),  
                   OM = median(OM_dat, na.rm = TRUE),
                   lwr_quantile_EC = quantile(EC_dat, na.rm = TRUE, probs = 0.25),
                   upr_quantile_EC = quantile(EC_dat, na.rm = TRUE, probs = 0.75),
                   lwr_quantile_OM = quantile(OM_dat, na.rm = TRUE, probs = 0.25),
                   upr_quantile_OM = quantile(OM_dat, na.rm = TRUE, probs = 0.75)) 


# need to structure the df (city, month, compound, value, lower IQR, upper IQR) 
# for easy plotting - there is probably a much easier way to do this! 
#------------------------------------------------------------------------------
# start by putting the IQR in the correct format 
om_IQR <- carb_compnd %>%
  select(c(city, month_text, lwr_quantile_OM, upr_quantile_OM)) %>%
  mutate(compound = "OM") %>%
  dplyr::rename(lower_iqr = lwr_quantile_OM,  upper_iqr = upr_quantile_OM)

ec_IQR <- carb_compnd %>%
  select(c(city, month_text, lwr_quantile_EC, upr_quantile_EC)) %>%
  mutate(compound = "EC") %>%
  dplyr::rename(lower_iqr = lwr_quantile_EC,  upper_iqr = upr_quantile_EC)

carb_iqr <- bind_rows(om_IQR, ec_IQR)

# Then merge the IQR back with the original df 
carb_compnd_median <- carb_compnd %>%
  select(c(city, month_text, OM, EC)) %>%
  pivot_longer(!c(city, month_text), names_to = "compound", values_to = "median") 

carb_data <- merge(carb_compnd_median, carb_iqr, by = c("city", "month_text", "compound"))



# lets plot! 
ggplot(data = carb_data, mapping = aes(x = month_text, y = median, colour = compound, linetype = compound,
                                          group = compound, ymin = lower_iqr, ymax = upper_iqr)) + 
  geom_point() + 
  geom_line() +
  geom_errorbar() +
  facet_wrap(~ city, nrow = 3) + 
  #scale_y_continuous(limits = c(0, 12), breaks = seq(0,12,2)) + 
  scale_linetype_manual(values = c("EC" = "dashed", "OM" = "solid")) + 
  labs(y =   expression(paste("Ammonium Sulphate/Ammonium Nitrate (",mu ,"g/",m^3,")")), x = "Month")  + 
  scale_x_discrete(labels= c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Figure 9: SOIL and NaCl
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# calculate our summary statisitcs 
fig9_compnd <- comp_station %>%
  mutate(month_text = month(date, label = TRUE, abbr = TRUE)) %>%
  select(city, SOIL_dat, NaCl_dat, month_text) %>%
  group_by(city, month_text) %>%
  dplyr::summarise(SOIL = median(SOIL_dat, na.rm = TRUE),  
                   NaCl = median(NaCl_dat, na.rm = TRUE),
                   lwr_quantile_SOIL = quantile(SOIL_dat, na.rm = TRUE, probs = 0.25),
                   upr_quantile_SOIL = quantile(SOIL_dat, na.rm = TRUE, probs = 0.75),
                   lwr_quantile_NaCl = quantile(NaCl_dat, na.rm = TRUE, probs = 0.25),
                   upr_quantile_NaCl = quantile(NaCl_dat, na.rm = TRUE, probs = 0.75)) 



soil_IQR <- fig9_compnd %>%
  select(c(city, month_text, lwr_quantile_SOIL, upr_quantile_SOIL)) %>%
  mutate(compound = "SOIL") %>%
  dplyr::rename(lower_iqr = lwr_quantile_SOIL,  upper_iqr = upr_quantile_SOIL)

nacl_IQR <- fig9_compnd %>%
  select(c(city, month_text, lwr_quantile_NaCl, upr_quantile_NaCl)) %>%
  mutate(compound = "NaCl") %>%
  dplyr::rename(lower_iqr = lwr_quantile_NaCl,  upper_iqr = upr_quantile_NaCl)

fig9_iqr <- bind_rows(soil_IQR, nacl_IQR)

# Then merge the IQR back with the original df 
fig9_compnd_median <- fig9_compnd %>%
  select(c(city, month_text, SOIL, NaCl)) %>%
  pivot_longer(!c(city, month_text), names_to = "compound", values_to = "median") 

fig9_data <- merge(fig9_compnd_median, fig9_iqr, by = c("city", "month_text", "compound"))



# lets plot! 
ggplot(data = fig9_data, mapping = aes(x = month_text, y = median, colour = compound, linetype = compound,
                                       group = compound, ymin = lower_iqr, ymax = upper_iqr)) + 
  geom_point() + 
  geom_line() +
  geom_errorbar() +
  facet_wrap(~ city, nrow = 3) + 
  scale_y_continuous(limits = c(0, 3), breaks = seq(0,3,0.5)) + 
  scale_linetype_manual(values = c("NaCl" = "dashed", "SOIL" = "solid")) + 
  labs(y =   expression(paste("Ammonium Sulphate/Ammonium Nitrate (",mu ,"g/",m^3,")")), x = "Month")  + 
  scale_x_discrete(labels= c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Figure 10: Ammonia - cart c
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# calculate our summary statisitcs 
ammonia_compnd <- comp_station %>%
  mutate(month_text = month(date, label = TRUE, abbr = TRUE)) %>%
  select(city, cart_c_ammonia, month_text) %>%
  group_by(city, month_text) %>%
  dplyr::summarise(Ammonia = median(cart_c_ammonia, na.rm = TRUE),  
                   lwr_quantile = quantile(cart_c_ammonia, na.rm = TRUE, probs = 0.25),
                   upr_quantile = quantile(cart_c_ammonia, na.rm = TRUE, probs = 0.75)) 


# lets plot! 
ggplot(data = ammonia_compnd %>% filter(city != "Abbotsford"),
       mapping = aes(x = month_text, y = Ammonia,group = 1, ymin = lwr_quantile, ymax = upr_quantile)) + 
  geom_point() + 
  geom_line() +
  geom_errorbar() +
  facet_wrap(~ city, nrow = 3) + 
  scale_y_continuous(limits = c(0, 5), breaks = 1:5) + 
  labs(y = "Ammonia (ppb)", x = "Month")  + 
  scale_x_discrete(labels= c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))


ggplot(data = ammonia_compnd %>% filter(city == "Abbotsford"), 
       mapping = aes(x = month_text, y = Ammonia,group = 1, ymin = lwr_quantile, ymax = upr_quantile)) + 
  geom_point() + 
  geom_line() +
  geom_errorbar() +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0,50,10)) + 
  labs(y = "Ammonia (ppb)", x = "Month")  + 
  scale_x_discrete(labels= c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Figure 11: Sulphr dioxide, nitric acid 
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


 