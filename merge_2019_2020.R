## STEP 5


rm(list=ls())

library(dplyr)
library(tidyr)
library(plyr)
print("merging 2019 and 2020 data")
load("olas_2020_wide.rda")    
load("olas_2019_wide.rda")

##### Create data set for uploading onto the website #####
#### we need to merge the data in the old format because of how the site currently is formatted

olas_2020_wide_hh <- select(olas_2020_wide, 1:11, 46:79)                                           # Just select household data 
olas_2019_wide_hh <- select(olas_2019_wide, 1:11, 46:79)                                           # Just select household data



## making the names compatible with the current system. 

olas_2020_wide_hh  <- dplyr::rename(olas_2020_wide_hh, c(iso	= pais_c,
                                                         scope = zone,
                                                         quintile = quintileipc,
                                                         gender = sex,	
                                                         population = population,
                                                         population_perc = population_prc,
                                                         ave_income_pc	= ave_income_pc,
                                                         max_income_pc = max_income_pc,
                                                         ave_hh_income	= ave_hh_income,
                                                         max_hh_income	= max_hh_income,
                                                         access_water_piped_house	= access_water_piped_house__hh,
                                                         access_water_piped_plot	=  access_water_piped_plot__hh,
                                                         access_water_other_min = consume_other_min__hh,
                                                         access_water_other_max = consume_other_max__hh,
                                                         access_water_daily	= water_dist_daily__hh,
                                                         access_san_exclusive	= access_san_exclusive__hh,
                                                         access_sewer	= access_sewer__hh,
                                                         access_septic	= access_septic__hh,
                                                         access_latrine_min = access_latrine_min__hh ,
                                                         access_latrine_max = access_latrine_max__hh,
                                                         access_sewer_exclusive = access_sewer_exclusive__hh,
                                                         access_septic_exclusive	 = access_septic_exclusive__hh,
                                                         access_latrine_exclusive_min = access_latrine_exclusive_min__hh,
                                                         access_latrine_exclusive_max	= access_latrine_exclusive_max__hh , 
                                                         access_water_piped_house_daily	= access_water_piped_house_daily__hh,
                                                         access_water_piped_plot_daily	= access_water_piped_plot_daily__hh,
                                                         no_sanitation_access	= no_sanitation_access__hh,
                                                         access_latrines_unimproved_min	= access_latrines_unimproved_min__hh,
                                                         access_latrines_unimproved_exclusive_min	= access_latrines_unimproved_exclusive_min__hh,
                                                         improved_w_access_min = improved_w_access_min__hh,
                                                         improved_w_access_max = improved_w_access_max__hh,
                                                         water_trucked = water_trucked__hh,
                                                         water_on_premises = water_on_premises__hh,
                                                         water_distr = water_distr__hh,
                                                         consume_distr = consume_distr__hh,
                                                         consume_bottled = consume_bottled__hh,
                                                         water_dist_continuity_jmp = water_dist_continuity_jmp__hh,
                                                         improved_w_access_min = improved_w_access_min__hh,
                                                         treat_water = treat_water__hh,
                                                         water_meter = water_meter__hh,
                                                         improved_san_max = improved_san_max__hh,
                                                         improved_san_min_exclusive = improved_san_min_exclusive__hh,
                                                         improved_san_max_exclusive = improved_san_max_exclusive__hh,
                                                         access_latrines_unimproved_max = access_latrines_unimproved_max__hh))



arg_pop_2020 <- structure(list(iso = c("ARG", "ARG"), 
                               scope = c("country", "rural"), 
                               gender = c("all","all"),
                               quintile = c("total","total"),
                               population = c(45376763, 45376763),
                               population_perc = c(1,.08),
                               year_survey = c(2020,2020)), class = "data.frame", row.names = c(NA, -2L))


olas_2020_wide_hh <- rbind.fill(arg_pop_2020,olas_2020_wide_hh)

olas_2019_wide_hh  <- dplyr::rename(olas_2019_wide_hh, c(iso	= pais_c,
                                                         scope = zone,
                                                         quintile = quintileipc,
                                                         gender = sex,	
                                                         population = population,
                                                         population_perc = population_prc,
                                                         ave_income_pc	= ave_income_pc,
                                                         max_income_pc = max_income_pc,
                                                         ave_hh_income	= ave_hh_income,
                                                         max_hh_income	= max_hh_income,
                                                         access_water_piped_house	= access_water_piped_house__hh,
                                                         access_water_piped_plot	=  access_water_piped_plot__hh,
                                                         access_water_other_min = consume_other_min__hh,
                                                         access_water_other_max = consume_other_max__hh,
                                                         access_water_daily	= water_dist_daily__hh,
                                                         access_san_exclusive	= access_san_exclusive__hh,
                                                         access_sewer	= access_sewer__hh,
                                                         access_septic	= access_septic__hh,
                                                         access_latrine_min = access_latrine_min__hh ,
                                                         access_latrine_max = access_latrine_max__hh,
                                                         access_sewer_exclusive = access_sewer_exclusive__hh,
                                                         access_septic_exclusive	 = access_septic_exclusive__hh,
                                                         access_latrine_exclusive_min = access_latrine_exclusive_min__hh,
                                                         access_latrine_exclusive_max	= access_latrine_exclusive_max__hh , 
                                                         access_water_piped_house_daily	= access_water_piped_house_daily__hh,
                                                         access_water_piped_plot_daily	= access_water_piped_plot_daily__hh,
                                                         no_sanitation_access	= no_sanitation_access__hh,
                                                         access_latrines_unimproved_min	= access_latrines_unimproved_min__hh,
                                                         access_latrines_unimproved_exclusive_min	= access_latrines_unimproved_exclusive_min__hh,
                                                         improved_w_access_min = improved_w_access_min__hh,
                                                         improved_w_access_max = improved_w_access_max__hh,
                                                         water_trucked = water_trucked__hh,
                                                         water_on_premises = water_on_premises__hh,
                                                         water_distr = water_distr__hh,
                                                         consume_distr = consume_distr__hh,
                                                         consume_bottled = consume_bottled__hh,
                                                         water_dist_continuity_jmp = water_dist_continuity_jmp__hh,
                                                         improved_w_access_min = improved_w_access_min__hh,
                                                         treat_water = treat_water__hh,
                                                         water_meter = water_meter__hh,
                                                         improved_san_max = improved_san_max__hh,
                                                         improved_san_min_exclusive = improved_san_min_exclusive__hh,
                                                         improved_san_max_exclusive = improved_san_max_exclusive__hh,
                                                         access_latrines_unimproved_max = access_latrines_unimproved_max__hh))



arg_pop_2019 <- structure(list(iso = c("ARG", "ARG"), 
                               scope = c("country", "rural"), 
                               gender = c("all","all"),
                               quintile = c("total","total"),
                               population = c(44494502, 44494502),
                               population_perc = c(1,0.0813),
                               year_survey = c(2018,2018)), class = "data.frame", row.names = c(NA, -2L))


olas_2019_wide_hh <- rbind.fill(arg_pop_2019,olas_2019_wide_hh)

## Merge data sets

olas_update <- rbind(olas_2019_wide_hh, olas_2020_wide_hh)


olas_update[12:45] <- signif(olas_update[12:45],digits=3)

olas_update_upload <- olas_update[,c(1,2,4,3,5:ncol(olas_update))]


olas_update_upload$name <- paste0(olas_update_upload$iso,"_", olas_update_upload$year_survey)
z<-split(olas_update_upload,olas_update_upload$name)
m <- length(z)
for(i in 1:m){
  write.table(z[[i]], file = 
                paste0("output_data/country_files/",names(z[i]),".txt"), row.names = FALSE, sep = ",") 
}
z<-split(olas_update_upload,olas_update_upload$name)
m <- length(z)
for(i in 1:m){
  write.csv(z[[i]], file = 
              paste0("output_data/country_files/",names(z[i]),".csv"), row.names = FALSE)
}


##### create OLAS updated data in long format with pop and hh data #####


olas_update_full_data <- rbind(olas_2019_wide, olas_2020_wide)      
#bind



olas_update_full_data  <- dplyr::rename(olas_update_full_data,
                                    c( iso	= pais_c,
                                       scope = zone,
                                       quintile = quintileipc,
                                       gender = sex,	
                                       population = population,
                                       population_perc = population_prc))


write.csv(olas_update_full_data, file ="output_data/olas_harmonized_hh_surveys_wide.csv", row.names = FALSE)


olas_update_long <-olas_update_full_data %>%                                           ## Write final long file
  tidyr::pivot_longer(12:79, names_to="variable", values_to = "value" )%>% 
  separate(variable, c('variable','unit'), sep = '__')

#save(olas_update_long,file = "output_data/olas_update_long.rda") 
write.csv(olas_update_long, "output_data/olas_harmonized_hh_surveys_long.csv")

print("script finished")

## EXTRA

most_recent_year <- aggregate(olas_update_full_data$year, by = list(olas_update_full_data$iso), max)
colnames(most_recent_year) <- c("iso", "year_survey")

olas_update_ry<-merge(most_recent_year, olas_update_full_data,  by = c("iso", "year_survey"))   ## OLAS_update_upload is for upload to the site. 

countries_ry  <- olas_update_ry[olas_update_ry$quintile=="total" & olas_update_ry$scope == "country" & olas_update_ry$gender == "all",]


mean(countries_ry$improved_san_max__pop, rm.na = TRUE)
mean(countries_ry$improved_san_min__pop, na.rm = TRUE)


