#STEP 4: Generate dimensions and general data
print("create_summary_data.R starting") 
rm(list = ls())
library(hutils)
library(dplyr)
library(haven)

load("master_2020.rda")
data <- data_2020
rm(data_2020)

## Create dimension variables ####

## Create zone dimension
data$zone <- ifelse(data$zona_c == 1, "urban", ifelse(data$zona_c == 0, "rural",NA))


## Create sex dimension

    ## explore sex data 
    #table(data$pais_c,data$sexo_ci)
    #data %>%
    #  group_by(pais_c) %>%
    #  summarize(na = sum(is.na(sexo_ci)),
    #            female = sum(sexo_ci == 2, na.rm = T),
    #            male = sum(sexo_ci==1, na.rm = T))
    
    # remove NA sex
    
    data <- data[!is.na(data$sexo_ci),]                                               ## This is not a common occurrence but it causes issues with the final data set
    
    ## Create sex of head of household variable

    data$hh_id <- paste0(data$pais_c, data$idh_ch)                                      ## Create household ids
    data$fhh<- ifelse(data$jefe_ci == 1 & unclass(data$sexo_ci) == 2,1,0)               ## Generate population ratios for "living in female and male headed households
    data$mhh<- ifelse(data$jefe_ci == 1 & unclass(data$sexo_ci) == 1,1,0)
    
    data$sex <- ifelse(data$hh_id %in% unique(data$hh_id[data$fhh==1]), "female",
                       ifelse(data$hh_id %in% unique(data$hh_id[data$mhh==1]), "male",0)) ## There are cases where people marked both, in which case they will count in both categories (resulting % may not add to 100)

## Create quintiles and income data
    ### Income data is at household level. Here we calculate income, income per capita for households and equivalent quintiles, then join with each individual. 
    
    ## Validate income data 
        #validation_income<- data %>%
        #  group_by(pais_c) %>%
        #  dplyr::summarise(na_ylm_ci = mean(is.na(ylm_ci)),
        #                   na_ylnm_ci = mean(is.na(ylnm_ci)),
        #                   na_ynlm_ci = mean(is.na(ynlm_ci)),
        #                   na_ynlnm_ci = mean(is.na(ynlnm_ci)))
        
        #test <- select(data,pais_c, ylm_ci,ylnm_ci,ynlm_ci,ynlnm_ci)
        #test2 <- split(test, test$pais_c)
    
    income_data <- select(data, ylm_ci,ylnm_ci,ynlm_ci,ynlnm_ci)
    data$income<- ifelse((is.na(data$ylm_ci)& is.na(data$ylnm_ci) & is.na(data$ynlm_ci)& is.na(data$ynlnm_ci)),NA, rowSums(income_data, na.rm= TRUE)) #this is new
    
    ## Replace factor_ch with factor_ci when factor_ch is null
    data$factor_ch2 <- ifelse(is.na(data$factor_ch), data$factor_ci, 
                              data$factor_ch)
    data <- data[!is.na(data$factor_ch2),]   
    data <- data[data$nmiembros_ch != 0,]
    
    hh_income_table<- data %>%
      group_by(pais_c,idh_ch,nmiembros_ch,zona_c,region_c,factor_ch2) %>%
      dplyr::summarize(
        income_hh=sum(income, na.rm = TRUE),
        itpc = (sum(income, na.rm = TRUE)/nmiembros_ch))
    hh_income_table <- hh_income_table[!is.na(hh_income_table$pais_c),]
    
    
    ## Using income data to create quintiles
    hh_income_q_table<-mutate_ntile(hh_income_table, "itpc", n=5, weights = "factor_ch2", by = "pais_c",
                                    new.col = "quintileipc", character.only = TRUE, overwrite = TRUE,
                                    check.na = FALSE)
    hh_income_information <- as.data.frame(hh_income_q_table)
    hh_income_info <- select(hh_income_information, "pais_c", "idh_ch", "income_hh", "itpc", "quintileipc")


    ## The below uses a lot of memory, may need to check and reset the limit
    memory.limit()
    memory.limit(size=560000)
    
    ## join income data with main data set
    data<- left_join(data, hh_income_info, by=c("pais_c" = "pais_c", 
                                                "idh_ch" = "idh_ch"))
    data$factor_ch2 <- NULL 
    rm(hh_income_info, hh_income_information, hh_income_q_table, hh_income_table)
    
    print("Created quintiles, zone and head of household sex dimensions")

## Generate dummy variables ######
    ## For each dummy variable, countries that do not address the topic are given all NA values.
    ## These dummy variables are later used to generate population and household percentages of the population
    ## for each metric. In some cases, surveys only interview one person per vivienda while in others
    ## responses for the household or vivienda are extrapolated to all the family members or people living there.
    ## In the former case those not interviewed are given NA values so they do not affect the over percent generation.
    
      ## Connected to the public network and with piped water into the house
      
      #piped water access into house
          
      data$access_water_piped_house <- ifelse(data$pais_c == "ARG" & data$iv6 == 1 & data$iv7 == 1, 1,
                                              ifelse(data$pais_c == "BOL" & data$s07a_01 %in% c(1), 1, 
                                                     ifelse(data$pais_c == "CHL" & data$v20 == 1 & data$v22 ==1, 1,
                                                            ifelse(data$pais_c == "COL", NA, ## COL does not ask this
                                                                   ifelse(data$pais_c == "CRI" & data$v11 %in% c(1) & data$v12 %in% c(1,2,3,4),1, 
                                                                          ifelse(data$pais_c == "DOM" & data$donde_proviene_agua %in% c(1)& data$tiene_agua_red_publica ==1 , 1,
                                                                                 ifelse(data$pais_c == "ECU" & (data$vi10 == 1 & data$vi10a %in% c(1)), 1, 
                                                                                        ifelse(data$pais_c == "GTM" & data$p02a05a ==1 & data$p02b03 ==1,1, 
                                                                                               ifelse(data$pais_c == "HTI" & (data$sh102 %in% c(11)|data$sh101 %in% c(11)) ,1, 
                                                                                                      ifelse(data$pais_c == "JAM" & is.na(data$i27),NA, 
                                                                                                             ifelse(data$pais_c == "JAM" & data$i27 %in% c(1) ,1,
                                                                                                                    ifelse(data$pais_c == "MEX" & data$disp_agua %in% c(1),1,
                                                                                                                           ifelse(data$pais_c =="PER" & is.na(data$p110),NA,
                                                                                                                                  ifelse(data$pais_c == "PER" & data$p110 %in% c(1),1,
                                                                                                                                         ifelse(data$pais_c == "PRY"& is.na(data$v06),NA, 
                                                                                                                                                ifelse(data$pais_c == "PRY"& data$v06 %in% c(1,2,3,4) & data$v07a == 2,1, 
                                                                                                                                                       ifelse(data$pais_c == "SLV"& data$r312 %in% c(1,2),1,
                                                                                                                                                              ifelse(data$pais_c == "SUR"& data$q13_15 %in% c(1) & data$q13_16 %in% c(1,2,3),1,
                                                                                                                                                                     ifelse(data$pais_c == "TTO"& data$water %in% c(1,3),1,
                                                                                                                                                                            ifelse(data$pais_c == "VEN", NA,0)))))))))))))))))))) 
      
      # piped water access to plot or terrain    
      data$access_water_piped_plot <- ifelse(data$pais_c == "ARG" & data$iv6 == 2 & data$iv7 == 1, 1,
                                             ifelse(data$pais_c == "BOL" & data$s07a_01 %in% c(2), 1, 
                                                    ifelse(data$pais_c == "CHL" & data$v20 == 1 & data$v22 ==2, 1,
                                                           ifelse(data$pais_c == "COL" & is.na(data$p5050), NA,
                                                                  ifelse(data$pais_c == "COL" & data$p5050 %in% c(1,2), 1, 
                                                                         ifelse(data$pais_c == "CRI" & data$v11 %in% c(2) & data$v12 %in% c(1,2,3,4),1,
                                                                                ifelse(data$pais_c == "DOM" & data$donde_proviene_agua %in% c(2)& data$tiene_agua_red_publica ==1 , 1,
                                                                                       ifelse(data$pais_c == "ECU" & (data$vi10 == 1 & data$vi10a %in% c(2)), 1,
                                                                                              ifelse(data$pais_c == "GTM" & data$p02a05a ==1 & data$p02b03 ==2,1, 
                                                                                                     ifelse(data$pais_c == "HTI" & (data$sh102 %in% c(12)|data$sh101 %in% c(12)) ,1, 
                                                                                                            ifelse(data$pais_c == "JAM" & is.na(data$i27),NA,
                                                                                                                   ifelse(data$pais_c == "JAM" & data$i27 %in% c(2) ,1,
                                                                                                                          ifelse(data$pais_c == "MEX" & data$disp_agua %in% c(2),1,
                                                                                                                                 ifelse(data$pais_c =="PER" & is.na(data$p110),NA,
                                                                                                                                        ifelse(data$pais_c == "PER" & data$p110 %in% c(2),1,
                                                                                                                                               ifelse(data$pais_c == "PRY"& is.na(data$v06),NA,
                                                                                                                                                      ifelse(data$pais_c == "PRY"& data$v06 %in% c(1,2,3,4) & data$v07a == 1,1, 
                                                                                                                                                             ifelse(data$pais_c == "SLV"& data$r312 %in% c(3,4),1,
                                                                                                                                                                    ifelse(data$pais_c == "SUR"& data$q13_15 %in% c(2) & data$q13_16 %in% c(1,2,3),1, 
                                                                                                                                                                           ifelse(data$pais_c == "TTO"& data$water %in% c(2),1,
                                                                                                                                                                                  ifelse(data$pais_c == "VEN"&(data$s4q5__3 ==1| data$s5q13__3 == 1), 1,0)))))))))))))))))))))
      
      
      
      
      ### Delivered by Truck
      
      data$water_trucked <- ifelse(data$pais_c == "ARG", NA,
                                   ifelse(data$pais_c == "BOL" & data$s07a_01 == 12,1, 
                                          ifelse(data$pais_c == "CHL" & data$v20 == 6,1,
                                                 ifelse(data$pais_c == "COL" & is.na(data$p5050), NA, 
                                                        ifelse(data$pais_c == "COL" & data$p5050 %in% c(8,9), 1,
                                                               ifelse(data$pais_c == "CRI",NA,
                                                                      ifelse(data$pais_c == "DOM" & data$donde_proviene_agua %in% c(9), 1,
                                                                             ifelse(data$pais_c == "ECU" & data$vi10 == 4, 1,
                                                                                    ifelse(data$pais_c == "GTM" &  data$p02b03 ==6,1, 
                                                                                           ifelse(data$pais_c == "HTI" & (data$sh102 %in% c(71, 72) | data$sh101 %in% c(72, 71)),1, 
                                                                                                  ifelse(data$pais_c == "JAM" & is.na(data$i27),NA,
                                                                                                         ifelse(data$pais_c == "JAM" & data$i27 %in% c(8,9,10,11) ,1,
                                                                                                                ifelse(data$pais_c == "MEX" & data$disp_agua %in% c(6),1,
                                                                                                                       ifelse(data$pais_c =="PER" & is.na(data$p110),NA,
                                                                                                                              ifelse(data$pais_c == "PER"& data$p110 == 4,1,
                                                                                                                                     ifelse(data$pais_c == "PRY"& is.na(data$v06),NA,
                                                                                                                                            ifelse(data$pais_c == "PRY"& (data$v06 %in% c(11) | data$v07a == 6),1,
                                                                                                                                                   ifelse(data$pais_c == "SLV"& data$r313 %in% c(3),1, 
                                                                                                                                                          ifelse(data$pais_c == "SUR"& data$q13_15 %in% c(7),1,
                                                                                                                                                                 ifelse(data$pais_c == "TTO"& data$water %in% c(6),1,
                                                                                                                                                                        ifelse(data$pais_c == "VEN", NA,0))))))))))))))))))))) 
      
      
      
      ## consume bottled water
      
      data$consume_bottled <- ifelse(data$pais_c == "ARG", NA,
                                     ifelse(data$pais_c == "BOL" & data$s07a_01 %in% c(11), 1, 
                                            ifelse(data$pais_c == "CHL", NA, 
                                                   ifelse(data$pais_c == "COL" & is.na(data$p5050),NA,
                                                          ifelse(data$pais_c == "COL" & data$p5050 %in% c(10), 1, 
                                                                 ifelse(data$pais_c == "CRI",NA,
                                                                        ifelse(data$pais_c == "DOM", NA,
                                                                               ifelse(data$pais_c == "ECU", NA,
                                                                                      ifelse(data$pais_c == "GTM"& data$p02b04 == 5,1, 
                                                                                             ifelse(data$pais_c == "HTI" & data$sh101 %in% c(81) ,1, 
                                                                                                    ifelse(data$pais_c == "JAM" & is.na(data$i27),NA,
                                                                                                           ifelse(data$pais_c == "JAM" & data$i27 %in% c(12) ,1,
                                                                                                                  ifelse(data$pais_c == "MEX", NA, 
                                                                                                                         ifelse(data$pais_c == "PER", NA,
                                                                                                                                ifelse(data$pais_c == "PRY"& is.na(data$v06),NA,
                                                                                                                                       ifelse(data$pais_c == "PRY" & data$v08 == 11,1, 
                                                                                                                                              ifelse(data$pais_c == "SLV",NA,
                                                                                                                                                     ifelse(data$pais_c == "SUR", NA,
                                                                                                                                                            ifelse(data$pais_c == "TTO",NA,
                                                                                                                                                                   ifelse(data$pais_c == "VEN" & data$s5q13__7 == 1, NA,0))))))))))))))))))) )
      
      
      ## Household is connected to the water distribution network / has piped access
      # This field is generated using general use water variables unless only drinking water is available. 
      
      data$water_distr <- ifelse(data$pais_c == "ARG" & data$iv7 == 1 & data$iv6 %in% c(1,2), 1,
                                 ifelse(data$pais_c == "BOL" & data$s07a_01 %in% c(1,2), 1, 
                                        ifelse(data$pais_c == "CHL" & data$v20 == 1 & data$v22 %in% c(1,2), 1,
                                               ifelse(data$pais_c == "COL" & is.na(data$p4030s5), NA,
                                                      ifelse(data$pais_c == "COL" & data$p4030s5 %in% c(1)|data$p5050 %in%c(1), 1,
                                                             ifelse(data$pais_c == "CRI" & data$v11 %in% c(1,2) & data$v12 %in% c(1,2,3,4),1,
                                                                    ifelse(data$pais_c == "DOM" & data$donde_proviene_agua %in% c(1,2) & data$tiene_agua_red_publica ==1, 1,
                                                                           ifelse(data$pais_c == "ECU" & (data$vi10 == 1 & data$vi10a %in% c(1,2)), 1, 
                                                                                  ifelse(data$pais_c == "GTM" & data$p02a05a ==1 & data$p02b03 %in% c(1,2),1, 
                                                                                         ifelse(data$pais_c == "HTI" & (data$sh102 %in% c(11,12) | data$sh101 %in% c(11,12)),1, 
                                                                                                ifelse(data$pais_c == "JAM" & is.na(data$i27),NA,
                                                                                                       ifelse(data$pais_c == "JAM" & data$i27 %in% c(1,2) ,1,
                                                                                                              ifelse(data$pais_c == "MEX" & data$disp_agua %in% c(1,2),1,
                                                                                                                     ifelse(data$pais_c =="PER" & is.na(data$p110),NA,
                                                                                                                            ifelse(data$pais_c == "PER" & data$p110 %in% c(1,2),1,
                                                                                                                                   ifelse(data$pais_c == "PRY"& is.na(data$v06),NA,
                                                                                                                                          ifelse(data$pais_c == "PRY"& data$v06 %in% c(1,2,3,4)& data$v07a %in% c(1,2),1, 
                                                                                                                                                 ifelse(data$pais_c == "SLV"& data$r312 %in% c(1:4,4.1),1,
                                                                                                                                                        ifelse(data$pais_c == "SUR"& data$q13_15 %in% c(1,2) & data$q13_16 %in% c(1,2,3),1,
                                                                                                                                                               ifelse(data$pais_c == "TTO"& data$water %in% c(1,2,3),1,
                                                                                                                                                                      ifelse(data$pais_c == "VEN" & data$s4q5__1==1,1,0))))))))))))))))))))) 
      
      
      
      ## Primary drinking water source is piped
      # If a country ask about both drinking water and general use, this field is generated from the drinking water variable. 
      
      data$consume_distr <- ifelse(data$pais_c == "ARG" & data$iv7 == 1 & data$iv6 %in% c(1,2), 1,## Does not differentiate between water used for drinking and water for other purposes
                                   ifelse(data$pais_c == "BOL" & data$s07a_01 %in% c(1,2), 1, ## Specifically drinking water #1. Cañería de red dentro de la vivienda?	2. Cañería de red fuera de la vivienda, pero dentro del lote o terreno?	
                                          ifelse(data$pais_c == "CHL" & data$v20 == 1& data$v22 %in% c(1,2), 1, ## Does not differentiate between uses
                                                 ifelse(data$pais_c == "COL" & is.na(data$p5050),NA,
                                                        ifelse(data$pais_c == "COL" & data$p5050 %in% c(1), 1, ## asks about both, p5050 is about human consumption specifically
                                                               ifelse(data$pais_c == "CRI" & data$v11 %in% c(1,2) & data$v12 %in% c(1,2,3,4),1, ## # Does not differentiate between uses, included 4 empresa o colectiva
                                                                      ifelse(data$pais_c == "DOM" & data$donde_proviene_agua %in% c(1,2) & data$tiene_agua_red_publica ==1, 1,## Specifically NOT drinking water, asks about other purposes.
                                                                             ifelse(data$pais_c == "ECU" & (data$vi10 == 1 & data$vi10a %in% c(1,2)), 1, ## does not differentiate
                                                                                    ifelse(data$pais_c == "GTM" & data$p02a05a ==1& data$p02b03 %in% c(1,2)& !data$p02b04 == 5,1, ## Does not differentiate between uses 
                                                                                           ifelse(data$pais_c == "HTI" & data$sh101 %in% c(11,12) ,1, ##  Specifies both. 101 is drinking, 102 is other purposes
                                                                                                  ifelse(data$pais_c == "JAM" & is.na(data$i27),NA,
                                                                                                         ifelse(data$pais_c == "JAM" & data$i27 %in% c(1,2) ,1, ## specific to drinking water
                                                                                                                ifelse(data$pais_c == "MEX" & data$disp_agua %in% c(1,2),1,# Does not differentiate between uses
                                                                                                                       ifelse(data$pais_c =="PER" & is.na(data$p110),NA,
                                                                                                                              ifelse(data$pais_c == "PER" & data$p110 %in% c(1,2) & data$p110a1==1 ,1, ## asks generally and if water is potable
                                                                                                                                     ifelse(data$pais_c == "PRY"& is.na(data$v06),NA,
                                                                                                                                            ifelse(data$pais_c == "PRY"& data$v08 %in% c(1,2,3,4)& data$v07a %in% c(1,2),1, ## Asks both about drinking water and general
                                                                                                                                                   ifelse(data$pais_c == "SLV"& data$r312 %in% c(1:4,4.1),1, ## Does not differentiate between uses
                                                                                                                                                          ifelse(data$pais_c == "SUR"& data$q13_15 %in% c(1,2) & data$q13_16 %in% c(1,2,3),1, # Does not differentiate between uses
                                                                                                                                                                 ifelse(data$pais_c == "TTO"& data$water %in% c(1,2,3),1,# Does not differentiate between uses
                                                                                                                                                                        ifelse(data$pais_c == "VEN"& data$s5q13__1%in% c(1),1,0))))))))))))))))))))) # Asks both
      
      
      
      # data$comsume_other_max
        # Upper bound includes wells when no distinction is made
        # Upper bound includes other
        # If springs are lumped in with other surface water bodies it is always considered unimproved.
        # Some countries ask specifically about drinking water while some specifically do not ask about drinking water (DOM)
        # Includes getting water from a neighbor
        # If response options are not sufficiently precise, lower bound (min) is give a value of 0.
      
      
      data$consume_other_max <- ifelse(data$pais_c == "ARG" & data$iv7 %in% c(2,3), 1, 
                                       ifelse(data$pais_c == "BOL" & data$s07a_01 %in% c(3,4,5,6,7,9,11,12,13), 1,  
                                              ifelse(data$pais_c == "CHL" & data$v20 %in% c(4,6,7), 1, 
                                                     ifelse(data$pais_c == "COL" & is.na(data$p5050),NA,
                                                            ifelse(data$pais_c == "COL" & data$p5050 %in% c(3,4,5,7,8,9,10), 1,
                                                                   ifelse(data$pais_c == "CRI" &  data$v12 %in% c(5,7),1, 
                                                                          ifelse(data$pais_c == "DOM" & data$donde_proviene_agua %in% c(3,4,5,7,8,9,99), 1,
                                                                                 ifelse(data$pais_c == "ECU" & data$vi10 %in% c(2,3,4,5,7), 1, 
                                                                                        ifelse(data$pais_c == "GTM"& (data$p02b03 %in% c(3,4,6,7,98)|data$p02b04 == 5), 1,
                                                                                               ifelse(data$pais_c == "HTI" & data$sh101 %in% c(13,14,21,22,41,61,71,72,81,91,96),1,
                                                                                                      ifelse(data$pais_c == "JAM" & is.na(data$i27),NA,
                                                                                                             ifelse(data$pais_c == "JAM" & data$i27 %in% c(3,4,6,7,8,9,10,11,12,13),1, 
                                                                                                                    ifelse(data$pais_c == "MEX" & data$disp_agua %in% c(3,4,5,6,7),1, 
                                                                                                                           ifelse(data$pais_c =="PER" & is.na(data$p110),NA,
                                                                                                                                  ifelse(data$pais_c == "PER" & (data$p110 %in% c(4,5,6,7) | (data$p110 %in% c(3)& data$p110a1==1)),1,
                                                                                                                                         ifelse(data$pais_c == "PRY"& is.na(data$v06),NA,
                                                                                                                                                ifelse(data$pais_c == "PRY"& data$v08 %in% c(5,6,8,10,11,12,14),1,
                                                                                                                                                       ifelse(data$pais_c == "SLV"& data$r313 %in% c(1,2,3,4,4.1,5,5.1,8,10,11,12,13),1, 
                                                                                                                                                              ifelse(data$pais_c == "SUR"& data$q13_15 %in% c(4,5,7,8),1, 
                                                                                                                                                                     ifelse(data$pais_c == "TTO"& data$water %in% c(4,5,6,7),1, 
                                                                                                                                                                            ifelse(data$pais_c == "VEN" & is.na(data$s5q13__1) & is.na(data$s5q13__2) & is.na(data$s5q13__3) & is.na(data$s5q13__4) & is.na(data$s5q13__5) & is.na(data$s5q13__6) & is.na(data$s5q13__7)& is.na(data$s5q13__8), 0,
                                                                                                                                                                                   ifelse(data$pais_c == "VEN" & (data$s5q13__2==1|data$s5q13__3==1|data$s5q13__4%in% c(1)|data$s5q13__5%in% c(1)|data$s5q13__7 %in% c(1)|data$s5q13__8%in% c(1)),1,0 )))))))))))))))))))))) 
      
      
      
      data$consume_other_min <- ifelse(data$pais_c == "ARG", 0, 
                                       ifelse(data$pais_c == "BOL" & data$s07a_01 %in% c(3,4,5,6,7,9,11,12), 1, 
                                              ifelse(data$pais_c == "CHL" & data$v20 %in% c(6), 1,
                                                     ifelse(data$pais_c == "COL" & is.na(data$p5050),NA,
                                                            ifelse(data$pais_c == "COL" & data$p5050 %in% c(5,7,8,9,10), 1, 
                                                                   ifelse(data$pais_c == "CRI",0, 
                                                                          ifelse(data$pais_c == "DOM" & data$donde_proviene_agua %in% c(3,4,5,7,9), 1, 
                                                                                 ifelse(data$pais_c == "ECU" & data$vi10 %in% c(2,3,4), 1, 
                                                                                        ifelse(data$pais_c == "GTM" & (data$p02b03 %in% c(3,6,7)|data$p02b04 == 5),1,
                                                                                               ifelse(data$pais_c == "HTI" & data$sh101 %in%c(13,14,21,22,41,61,71,72, 81,91),1, 
                                                                                                      ifelse(data$pais_c == "JAM" & is.na(data$i27),NA,
                                                                                                             ifelse(data$pais_c == "JAM" & data$i27 %in% c(3,6,7,8,9,10,11,12),1,
                                                                                                                    ifelse(data$pais_c == "MEX" & data$disp_agua %in% c(3,4,5,6),1, 
                                                                                                                           ifelse(data$pais_c =="PER" & is.na(data$p110),NA,
                                                                                                                                  ifelse(data$pais_c == "PER" & (data$p110 %in% c(4) | (data$p110 %in% c(3)& data$p110a1==1)),1, 
                                                                                                                                         ifelse(data$pais_c == "PRY"& is.na(data$v06),NA,
                                                                                                                                                ifelse(data$pais_c == "PRY"& data$v08 %in% c(5,6,8,10,11,12,14),1, 
                                                                                                                                                       ifelse(data$pais_c == "SLV"& data$r313 %in% c(2,3,4,5,5.1,8,10,11),1,
                                                                                                                                                              ifelse(data$pais_c == "SUR"& data$q13_15 %in% c(4,7),1, 
                                                                                                                                                                     ifelse(data$pais_c == "TTO"& data$water %in% c(4,5,6),1, 
                                                                                                                                                                            ifelse(data$pais_c == "VEN" & is.na(data$s5q13__1) & is.na(data$s5q13__2) & is.na(data$s5q13__3) & is.na(data$s5q13__4) & is.na(data$s5q13__5) & is.na(data$s5q13__6) & is.na(data$s5q13__7)& is.na(data$s5q13__8), 0,
                                                                                                                                                                                   ifelse(data$pais_c == "VEN" & (data$s5q13__2==1|data$s5q13__3==1|data$s5q13__5==1|data$s5q13__7 %in% c(1)),1,0))))))))))))))))))))))
      
      
      
      
      
      
      
      
      ## Frequency of water access, reports no interruptions 
      ## This classification is different than the JMP because the way the questions are asked make determination via the JMP methodology impossible
      
      data$water_dist_daily <- ifelse(!data$pais_c %in% c("COL", "HTI", "JAM", "MEX" , "PER", "PRY", "SLV", "VEN") ,NA,
                                     ifelse(data$pais_c =="COL" & is.na(data$p4030s5), NA, 
                                            ifelse(data$pais_c =="COL" & is.na(data$p4040), 0,
                                                   ifelse(data$pais_c == "COL" & data$p4040 %in% c(1), 1, 
                                                        ifelse(data$pais_c == "HTI" & is.na(data$hv201a),0,
                                                               ifelse(data$pais_c == "HTI" & data$hv201a %in% c(0),1, 
                                                                      ifelse(data$pais_c == "JAM" & is.na(data$i28),0,
                                                                             ifelse(data$pais_c == "JAM" & data$i28 ==0,1, 
                                                                                    ifelse(data$pais_c == "MEX" & is.na(data$dotac_agua),0,
                                                                                           ifelse(data$pais_c == "MEX" & data$dotac_agua %in% c(1),1,
                                                                                                  ifelse(data$pais_c =="PER" & is.na(data$p110),NA,
                                                                                                         ifelse(data$pais_c == "PER" & is.na(data$p110c),0,
                                                                                                                ifelse(data$pais_c == "PER" & data$p110c %in% c(1),1,
                                                                                                                       ifelse(data$pais_c == "PRY"& is.na(data$v06),NA,
                                                                                                                              ifelse(data$pais_c == "PRY"& is.na(data$v07),0,
                                                                                                                                     ifelse(data$pais_c == "PRY"& data$v07 %in% c(1),1, 
                                                                                                                                            ifelse(data$pais_c == "SLV"& is.na(data$r312d),0,
                                                                                                                                                   ifelse(data$pais_c == "SLV"& data$r312d%in%c(0),1,
                                                                                                                                                          ifelse(data$pais_c == "VEN" & is.na(data$s4q6), 0,
                                                                                                                                                                 ifelse(data$pais_c == "VEN" & data$s4q6==1,1,0))))))))))))))))))))

      
      
      
      
      
      
      
      ## This is a continuity dummy variable matching the JMP classification (water arrives at least 4 days of the week and or 12 hours per day) 
      
      data$water_dist_continuity_jmp <- ifelse(data$pais_c == "JAM" & data$i28 <16,1,
                                               ifelse(data$pais_c == "JAM" & is.na(data$i28),0,
                                                      ifelse(data$pais_c =="PER" & is.na(data$p110),NA,
                                                             ifelse(data$pais_c =="PER" & is.na(data$p110c2),0,
                                                                    ifelse(data$pais_c == "PER" & data$p110c2 > 3 &  data$p110c3 >=12,1,
                                                                           ifelse(data$pais_c == "SLV"& is.na(data$r312h), 0,
                                                                                  ifelse(data$pais_c == "SLV"& data$r312h <13,1, 
                                                                                         ifelse(data$pais_c == "VEN" & is.na(data$s4q6),0,
                                                                                                ifelse(data$pais_c == "VEN" & data$s4q6 %in% c(1,2),1,
                                                                                                       ifelse(!data$pais_c %in% c("JAM", "PER", "SLV", "VEN"),NA, 0)))))))))) 
      
      ## Meter
      
      data$water_meter <- ifelse(data$pais_c == "CHL"& data$v20 %in% c(1,2),1,
                                 ifelse(data$pais_c == "ECU" & data$vi101 %in% c(1)|data$vi10 %in% c(1),1,
                                        ifelse(data$pais_c == "GTM" & data$p02a05e == 1, 1,
                                               ifelse(data$pais_c == "JAM" & data$i31 %in% c(1,2), 1,
                                                      ifelse(!data$pais_c %in% c("ECU", "GTM", "JAM"), NA,0)))))
      ## Treat water                                      
      
      data$treat_water <- ifelse(data$pais_c == "GTM" & data$p02b04 %in% c(2:4, 98),1,
                                 ifelse(data$pais_c == "HTI" & data$hv237 == 1, 1,
                                        ifelse(data$pais_c == "VEN" & is.na(data$s5q14), NA,
                                               ifelse(data$pais_c == "VEN" & data$s5q14 %in% c(1),1,
                                                      ifelse(!data$pais_c %in% c("GTM", "HTI", "VEN"), NA,0)))))
      
      
      ## Exclusive sanitation access
      
      data$access_san_exclusive <- ifelse(data$pais_c == "ARG" & data$ii9==1,1,
                                          ifelse(data$pais_c == "BOL",NA, ## BOL does not ask this year
                                                 ifelse(data$pais_c == "CHL", NA, 
                                                        ifelse(data$pais_c == "COL" & is.na(data$p5020),NA, 
                                                               ifelse(data$pais_c == "COL" & is.na(data$p5030),0, 
                                                                      ifelse(data$pais_c == "COL" & data$p5030 == 1,1,
                                                                             ifelse(data$pais_c == "CRI" & is.na(data$v13b),0, 
                                                                                    ifelse(data$pais_c == "CRI" & data$v13b == 1,1, 
                                                                                           ifelse(data$pais_c == "DOM" & data$tipo_sanitario %in% c(1,3),1,
                                                                                                  ifelse(data$pais_c == "ECU", NA, 
                                                                                                         ifelse(data$pais_c == "GTM", NA, 
                                                                                                                ifelse(data$pais_c == "HTI" & data$hv225 %in% c(0),1,
                                                                                                                       ifelse(data$pais_c == "JAM" & is.na(data$i5),NA, 
                                                                                                                              ifelse(data$pais_c == "JAM" & data$i6 ==1,1, 
                                                                                                                                     ifelse(data$pais_c == "MEX" & data$uso_compar %in% c(2),1,
                                                                                                                                            ifelse(data$pais_c == "PER", NA, 
                                                                                                                                                   ifelse(data$pais_c == "PRY", NA,
                                                                                                                                                          ifelse(data$pais_c == "SLV"& data$r316 %in% c(1,2,5,7,9),1, 
                                                                                                                                                                 ifelse(data$pais_c == "SUR",NA,
                                                                                                                                                                        ifelse(data$pais_c == "TTO",NA, 
                                                                                                                                                                               ifelse(data$pais_c == "VEN" & data$s5q2==1,1,0))))))))))))))))))))) ## specifies baño and ducha exclusive
      
      
      
      ## access to toilet connected to sewer system
      
      data$access_sewer <- ifelse(data$pais_c == "ARG" & data$iv11==1,1, 
                                  #ifelse(data$pais_c == "BLZ"& data$h3 == 1,1, 
                                  ifelse(data$pais_c == "BOL" & data$s07a_02==1 & is.na(data$s07a_03),0,
                                         ifelse(data$pais_c == "BOL" & data$s07a_02 == 1 & data$s07a_03 == 1,1, 
                                                ifelse(data$pais_c == "CHL" & is.na(data$v23_sistema), 0, 
                                                       ifelse(data$pais_c == "CHL" & data$v23_sistema == 1, 1,
                                                              ifelse(data$pais_c == "COL" & is.na(data$p5020),NA, 
                                                                     ifelse(data$pais_c=="COL" & data$p5020== 1,1, 
                                                                            ifelse(data$pais_c == "CRI" & data$v13a == 1,1,
                                                                                   ifelse(data$pais_c == "DOM" & data$se_encuentra_conectada_a %in% c(2),1, 
                                                                                          ifelse(data$pais_c == "ECU" & data$vi09 == 1, 1, 
                                                                                                 ifelse(data$pais_c == "GTM" & (data$p02b07 == 1 & data$p02a05b ==1), 1, 
                                                                                                        ifelse(data$pais_c == "HTI" & data$hv205 %in% c(11),1, 
                                                                                                               ifelse(data$pais_c == "JAM" & is.na(data$i5),NA,
                                                                                                                      ifelse(data$pais_c == "JAM" & data$i5 ==1,1,
                                                                                                                             ifelse(data$pais_c == "MEX" & data$drenaje %in% c(1),1,
                                                                                                                                    ifelse(data$pais_c =="PER" & is.na(data$p111a),NA,
                                                                                                                                           ifelse(data$pais_c == "PER"& data$p111a %in% c(1,2),1, 
                                                                                                                                                  ifelse(data$pais_c == "PRY"& is.na(data$v06),NA, 
                                                                                                                                                         ifelse(data$pais_c == "PRY" & is.na(data$v13),0, 
                                                                                                                                                                ifelse(data$pais_c == "PRY" & data$v13 == 1, 1,  
                                                                                                                                                                       ifelse(data$pais_c == "SLV"& data$r316 %in% c(1,3),1,
                                                                                                                                                                              ifelse(data$pais_c == "SUR",NA, 
                                                                                                                                                                                     ifelse(data$pais_c == "TTO" & data$toilet == 2,1, 
                                                                                                                                                                                            ifelse(data$pais_c == "VEN" & is.na(data$s4q9),0,
                                                                                                                                                                                                   ifelse(data$pais_c == "VEN" & data$s4q9==1,1,0)))))))))))))))))))))))))
      ## septic system access
      
      data$access_septic <- ifelse(data$pais_c == "ARG" & data$iv11==2,1, 
                                   #ifelse(data$pais_c == "BLZ"& data$h3 == 2,1,
                                   ifelse(data$pais_c == "BOL" & data$s07a_02==1 & is.na(data$s07a_03),0,
                                          ifelse(data$pais_c == "BOL" & data$s07a_02 == 1 & data$s07a_03 == 2,1,
                                                 ifelse(data$pais_c == "CHL" & is.na(data$v23_sistema), 0, 
                                                        ifelse(data$pais_c == "CHL" & data$v23_sistema == 2, 1,
                                                               ifelse(data$pais_c == "COL" & is.na(data$p5020),NA,
                                                                      ifelse(data$pais_c == "COL" & data$p5020== 2,1,
                                                                             ifelse(data$pais_c == "CRI" & data$v13a %in% c(2,3),1, 
                                                                                    ifelse(data$pais_c == "DOM" & data$se_encuentra_conectada_a %in% c(1),1, 
                                                                                           ifelse(data$pais_c == "ECU" & data$vi09 == 2, 1,
                                                                                                  ifelse(data$pais_c == "GTM" & data$p02b07 == 2, 1, 
                                                                                                         ifelse(data$pais_c == "HTI" & data$hv205 %in% c(12),1, 
                                                                                                                ifelse(data$pais_c == "JAM" & is.na(data$i5), NA,
                                                                                                                       ifelse(data$pais_c == "JAM" & data$i5 %in% c(3),1, 
                                                                                                                              ifelse(data$pais_c == "MEX" & data$drenaje %in% c(2),1, 
                                                                                                                                     ifelse(data$pais_c =="PER" & is.na(data$p111a),NA,
                                                                                                                                            ifelse(data$pais_c == "PER" & data$p111a %in% c(4),1,  
                                                                                                                                                   ifelse(data$pais_c == "PRY"& is.na(data$v06),NA,
                                                                                                                                                          ifelse(data$pais_c == "PRY" & is.na(data$v13),0,
                                                                                                                                                                 ifelse(data$pais_c == "PRY" & data$v13 == 2, 1,
                                                                                                                                                                        ifelse(data$pais_c == "SLV"& data$r316 %in% c(2,4),1, 
                                                                                                                                                                               ifelse(data$pais_c == "SUR" & data$q13_14 ==1,1, 
                                                                                                                                                                                      ifelse(data$pais_c == "TTO",NA, 
                                                                                                                                                                                             ifelse(data$pais_c == "VEN" & is.na(data$s4q9),0,
                                                                                                                                                                                                    ifelse(data$pais_c == "VEN" & data$s4q9==2,1,0)))))))))))))))))))))))))
      
      
      
      ## access latrine min
      ## This category identifies households that have access to improved sanitation facilities that are not flush toilets connected
      ## to the sewer system or a septic system, according to the JMP improved sanitation facilities definition
      ## for this variable, lower bound is 0 if none of the alternative facilities in the response options are clearly improved 
      
      data$access_latrine_min <- ifelse(data$pais_c == "ARG" & (data$iv10 %in% c(2) & data$iv11 %in% c(3)),1, 
                                        #ifelse(data$pais_c == "BLZ"& data$h3 %in% c(3,4,5),1, 
                                        ifelse(data$pais_c == "BOL" & (data$s07a_02 %in% c(2,4) | (data$s07a_02 %in% c(1) &  data$s07a_03 %in% c(3,5))),1, 
                                               ifelse(data$pais_c == "CHL" & (data$v23_sistema %in% c(7) | (data$v23_sistema %in% c(4) & data$v23_cajon %in% c(4))), 1, 
                                                      ifelse(data$pais_c == "COL" & is.na(data$p5020),NA,
                                                             ifelse(data$pais_c == "COL",0, 
                                                                    ifelse(data$pais_c == "CRI",0, 
                                                                           ifelse(data$pais_c == "DOM",0, 
                                                                                  ifelse(data$pais_c == "ECU" & data$vi09 %in% c(3), 1, 
                                                                                         ifelse(data$pais_c == "GTM",0, 
                                                                                                ifelse(data$pais_c == "HTI" & data$hv205 %in% c(13,15,21,22,41),1, 
                                                                                                       ifelse(data$pais_c == "JAM" & is.na(data$i5), NA,
                                                                                                              ifelse(data$pais_c == "JAM" & data$i5 %in% c(2),1,   
                                                                                                                     ifelse(data$pais_c == "MEX" ,0, 
                                                                                                                            ifelse(data$pais_c =="PER" & is.na(data$p111a),NA,
                                                                                                                                   ifelse(data$pais_c == "PER" & data$p111a %in% c(3,5),1, 
                                                                                                                                          ifelse(data$pais_c == "PRY"& is.na(data$v06),NA,
                                                                                                                                                 ifelse(data$pais_c == "PRY" & is.na(data$v13),0,
                                                                                                                                                        ifelse(data$pais_c == "PRY" & data$v13 %in% c(3,5,6), 1,
                                                                                                                                                               ifelse(data$pais_c == "SLV"& data$r316 %in% c(7:10),1, 
                                                                                                                                                                      ifelse(data$pais_c == "SUR" & data$q13_14 %in% c(2),1,
                                                                                                                                                                             ifelse(data$pais_c == "TTO",0,  
                                                                                                                                                                                    ifelse(data$pais_c == "VEN",0,0))))))))))))))))))))))
      
      
      
      ## access latrine max
      ## does not include "other" responses unless specified "other" responses are clearly improved facilities. 
      
      data$access_latrine_max <- ifelse(data$pais_c == "ARG" & (data$iv10 %in% c(2,3) & data$iv11 ==3),1,
                                        ifelse(data$pais_c == "BOL" & (data$s07a_02 %in% c(2,4) | (data$s07a_02 %in% c(1) &  data$s07a_03 %in% c(3,5))),1, 
                                               ifelse(data$pais_c == "CHL" & (data$v23_sistema %in% c(3,7) | (data$v23_sistema %in% c(4) & data$v23_cajon %in% c(4,6))), 1, 
                                                      ifelse(data$pais_c == "COL" & is.na(data$p5020),NA, 
                                                             ifelse(data$pais_c == "COL" & data$p5020 %in% c(3,4),1,
                                                                    ifelse(data$pais_c == "CRI" & data$v13a %in% c(4),1, 
                                                                           ifelse(data$pais_c == "DOM" & data$tipo_sanitario %in% c(3,4),1,
                                                                                  ifelse(data$pais_c == "ECU" & (data$vi09 %in% c(3,4) | data$vi09b %in% c(1:3)), 1, 
                                                                                         ifelse(data$pais_c == "GTM" & data$p02b07 %in% c(3,4), 1, 
                                                                                                ifelse(data$pais_c == "HTI" & data$hv205 %in% c(13,15,20,21,22,41),1,
                                                                                                       ifelse(data$pais_c == "JAM" & is.na(data$i5),NA,
                                                                                                              ifelse(data$pais_c == "JAM" & data$i5 %in% c(2,4),1, 
                                                                                                                     ifelse(data$pais_c == "MEX" & data$excusado ==1 & data$drenaje %in% c(5),1, 
                                                                                                                            ifelse(data$pais_c =="PER" & is.na(data$p111a),NA,
                                                                                                                                   ifelse(data$pais_c == "PER" & data$p111a %in% c(3,5),1, 
                                                                                                                                          ifelse(data$pais_c == "PRY"& is.na(data$v06),NA,
                                                                                                                                                 ifelse(data$pais_c == "PRY" & is.na(data$v13),0,      
                                                                                                                                                        ifelse(data$pais_c == "PRY"  & data$v13 %in% c(3,5,6), 1, 
                                                                                                                                                               ifelse(data$pais_c == "SLV"& data$r316 %in% c(5:10),1, 
                                                                                                                                                                      ifelse(data$pais_c == "SUR" & data$q13_14 %in% c(2),1,
                                                                                                                                                                             ifelse(data$pais_c == "TTO"& data$toilet %in% c(1,3,9),1,
                                                                                                                                                                                    ifelse(data$pais_c == "VEN" & data$s4q9 %in% c(3,4),1,0))))))))))))))))))))))
      
      ## no sanitation access
      
      data$no_san_access<- ifelse(data$pais_c == "ARG" & data$ii9 == 4,1,  
                                  #ifelse(data$pais_c == "BLZ", NA,
                                  ifelse(data$pais_c == "BOL" & data$s07a_02 ==5,1,
                                         ifelse(data$pais_c == "CHL" & data$v23 == 2, 1, 
                                                ifelse(data$pais_c == "COL" & is.na(data$p5020),NA,
                                                       ifelse(data$pais_c == "COL" & data$p5020 == 6,1,
                                                              ifelse(data$pais_c == "CRI" & data$v13a == 0,1,
                                                                     ifelse(data$pais_c == "DOM" & data$tipo_sanitario %in% c(5),1,
                                                                            ifelse(data$pais_c == "ECU" & data$vi09 %in% c(5), 1, 
                                                                                   ifelse(data$pais_c == "GTM" & data$p02b07 %in% c(5), 1,
                                                                                          ifelse(data$pais_c == "HTI" & data$hv205 %in% c(31),1,
                                                                                                 ifelse(data$pais_c == "JAM" & is.na(data$i5),NA,
                                                                                                        ifelse(data$pais_c == "JAM" & data$i5 %in% c(6),1, 
                                                                                                               ifelse(data$pais_c == "MEX" & data$excusado ==2,1, 
                                                                                                                      ifelse(data$pais_c == "PER" ,NA, 
                                                                                                                             ifelse(data$pais_c == "PRY"& is.na(data$v06),NA,
                                                                                                                                    ifelse(data$pais_c == "PRY" & is.na(data$v12),0, 
                                                                                                                                           ifelse(data$pais_c == "PRY"& data$v12 == 6, 1, 
                                                                                                                                                  ifelse(data$pais_c == "SLV" & data$r314 == 4,1, 
                                                                                                                                                         ifelse(data$pais_c == "SUR", NA,
                                                                                                                                                                ifelse(data$pais_c == "TTO" & data$toilet == 5,1,
                                                                                                                                                                       ifelse(data$pais_c == "VEN" & data$s4q9 == 5,1,0)))))))))))))))))))))
      
      ## Construct compound variables 
      
          ## water access on premises
          
          data_on_prem <- select(data, access_water_piped_house,access_water_piped_plot, water_trucked)
          data_on_prem$prem<- +(rowSums(data_on_prem, na.rm = TRUE) * NA ^ (rowSums(!is.na(data_on_prem)) == 0) > 0)
          data$water_on_premises <-data_on_prem$prem
          
          
          ## improved water access, lower bound
          
          data_improved_w_access_min <- select(data, consume_distr,consume_other_min)
          data_improved_w_access_min$improved  <- +(rowSums(data_improved_w_access_min, na.rm = TRUE) * NA ^ (rowSums(!is.na(data_improved_w_access_min)) == 0) > 0)
          data$improved_w_access_min <-data_improved_w_access_min$improved
          
          ## improved water access, upper bound
          
          data_improved_w_access_max <- select(data, consume_distr,consume_other_max)
          data_improved_w_access_max$improved  <- +(rowSums(data_improved_w_access_max, na.rm = TRUE) * NA ^ (rowSums(!is.na(data_improved_w_access_max)) == 0) > 0)
          data$improved_w_access_max <-data_improved_w_access_max$improved
          
          
          ## access_water_piped_house_daily
          
          
          data$access_water_piped_house_daily <- ifelse(is.na(data$water_dist_daily) | is.na(data$access_water_piped_house), NA,
                                                        ifelse(data$access_water_piped_house ==1 & data$water_dist_daily ==1, 1, 0))
          
          ## access_water_piped_plot_daily
          
          data$access_water_piped_plot_daily <- ifelse(is.na(data$water_dist_daily) | is.na(data$access_water_piped_plot), NA,
                                                       ifelse(data$access_water_piped_plot == 1 & data$water_dist_daily ==1, 1, 0))
          
          ## access_sewer_exclusive
          
          data$access_sewer_exclusive <- ifelse(is.na(data$access_san_exclusive | is.na(data$access_sewer)),NA, 
                                                ifelse(data$access_sewer == 1 & data$access_san_exclusive == 1, 1,
                                                       ifelse(data$access_sewer == 0 & is.na(data$access_san_exclusive),0,
                                                              ifelse(is.na(data$access_sewer) & data$access_san_exclusive ==0,0, 0))))
          
          ## access_septic_exclusive
          
          data$access_septic_exclusive <- ifelse(is.na(data$access_san_exclusive | is.na(data$access_septic)),NA,
                                                 ifelse(data$access_septic == 1 & data$access_san_exclusive == 1, 1,
                                                        ifelse(data$access_septic == 0 & is.na(data$access_san_exclusive),0,
                                                               ifelse(is.na(data$access_septic) & data$access_san_exclusive ==0,0, 0))))
          
          ## access_latrine_exclusive_min
          
          data$access_latrine_exclusive_min <- ifelse(is.na(data$access_latrine_min) | is.na(data$access_san_exclusive) , NA, 
                                                      ifelse(data$access_latrine_min == 1 & data$access_san_exclusive == 1, 1,0))
          
          
          ## access_latrine_exclusive_max
          
          data$access_latrine_exclusive_max <- ifelse(is.na(data$access_latrine_max) | is.na(data$access_san_exclusive) , NA, 
                                                      ifelse(data$access_latrine_max == 1 & data$access_san_exclusive == 1, 1,0))
          
          ## improved_san_min
          
          data_improved_san_min <- select(data, access_sewer,access_septic, access_latrine_min)
          data_improved_san_min$improved <- +(rowSums(data_improved_san_min, na.rm = TRUE) * NA ^ (rowSums(!is.na(data_improved_san_min)) == 0) > 0)
          data$improved_san_min <-data_improved_san_min$improved
          
          ## improved_san_max
          
          data_improved_san_max <- select(data,access_sewer,access_septic, access_latrine_max)
          data_improved_san_max$improved <- +(rowSums(data_improved_san_max, na.rm = TRUE) * NA ^ (rowSums(!is.na(data_improved_san_max)) == 0) > 0)
          data$improved_san_max <-data_improved_san_max$improved
          
          ## improved_san_min_exclusive
          
          data_improved_san_min_exclusive <- select(data, access_sewer_exclusive,access_septic_exclusive, access_latrine_exclusive_min)
          data_improved_san_min_exclusive$improved <- +(rowSums(data_improved_san_min_exclusive, na.rm = TRUE) * NA ^ (rowSums(!is.na(data_improved_san_min_exclusive)) == 0) > 0)
          data$improved_san_min_exclusive <-data_improved_san_min_exclusive$improved
          
          ## improved_san_max_exclusive
          
          data_improved_san_max_exclusive <- select(data, access_sewer_exclusive,access_septic_exclusive, access_latrine_exclusive_max)
          data_improved_san_max_exclusive$improved <- +(rowSums(data_improved_san_max_exclusive, na.rm = TRUE) * NA ^ (rowSums(!is.na(data_improved_san_max_exclusive)) == 0) > 0)
          data$improved_san_max_exclusive <-data_improved_san_max_exclusive$improved
          
          ## access_latrines_unimproved_min
          
          data$access_latrines_unimproved_min <-ifelse(data$improved_san_max %in% c(0),1,
                                                       ifelse(data$improved_san_max %in% c(1),0,NA))
          
          ## access_latrines_unimproved_max
          
          data$access_latrines_unimproved_max <-ifelse(data$improved_san_min %in% c(0),1,
                                                       ifelse(data$improved_san_max %in% c(1),0,NA))
          
          ## access_latrines_unimproved_exclusive_min
          
          data$access_latrines_unimproved_exclusive_min <-ifelse(data$improved_san_max_exclusive %in% c(0),1,
                                                                 ifelse(data$improved_san_max_exclusive %in% c(1),0,NA)) 
          
      print("Dummy variables created")
## Summarize data  (population) ####

    
    options(scipen = 999)
    ## list of dimensions to group data by
    dimensions <- list("pais_c", c("pais_c", "zone"), c("pais_c", "sex"), c("pais_c","quintileipc"), 
                       c("pais_c","zone", "sex"), c("pais_c","zone", "quintileipc"), c("pais_c","sex","quintileipc"), 
                       c("pais_c","zone", "sex", "quintileipc"))
    
    
    library(plyr)
    ## generate summary information 
    for (i in 1:length(dimensions)){
      
      print(paste0("Dimension: ",dimensions[i]," ", "Num"," ", i, "/",length(dimensions))) 
      summary_d <- data %>% 
        dplyr::group_by_at(vars(one_of(dimensions[[i]]))) %>%   
        dplyr::summarize(
          ave_income_pc	= weighted.mean(itpc, dplyr::coalesce(factor_ci,0), na.rm = T), 
          max_income_pc = max(itpc, na.rm= TRUE), 	
          ave_hh_income	= weighted.mean(income_hh, dplyr::coalesce(factor_ci,0), na.rm = T) , 
          max_hh_income	= max(income_hh, na.rm = TRUE),
          access_water_piped_house__pop = weighted.mean(access_water_piped_house, dplyr::coalesce(factor_ci,0), na.rm = T),
          access_water_piped_plot__pop = weighted.mean(access_water_piped_plot, dplyr::coalesce(factor_ci,0), na.rm = T),
          water_trucked__pop = weighted.mean(water_trucked, dplyr::coalesce(factor_ci,0), na.rm = T),
          water_on_premises__pop = weighted.mean(water_on_premises, dplyr::coalesce(factor_ci,0), na.rm = T),
          water_distr__pop = weighted.mean(water_distr, dplyr::coalesce(factor_ci,0), na.rm = T),
          consume_distr__pop = weighted.mean(consume_distr,  dplyr::coalesce(factor_ci,0), na.rm= TRUE),
          consume_other_max__pop =  weighted.mean(consume_other_max,  dplyr::coalesce(factor_ci,0), na.rm= TRUE),
          consume_other_min__pop = weighted.mean(consume_other_min,  dplyr::coalesce(factor_ci,0), na.rm = TRUE),
          consume_bottled__pop = weighted.mean(consume_bottled, dplyr::coalesce(factor_ci,0), na.rm = T),
          water_dist_daily__pop = weighted.mean(water_dist_daily,  dplyr::coalesce(factor_ci,0), na.rm = TRUE),
          water_dist_continuity_jmp__pop  = weighted.mean(water_dist_continuity_jmp,  dplyr::coalesce(factor_ci,0), na.rm = TRUE),
          improved_w_access_min__pop  = weighted.mean(improved_w_access_min,  dplyr::coalesce(factor_ci,0), na.rm = TRUE),
          improved_w_access_max__pop  = weighted.mean(improved_w_access_max,  dplyr::coalesce(factor_ci,0), na.rm = TRUE),
          access_water_piped_house_daily__pop = weighted.mean(access_water_piped_house_daily,  dplyr::coalesce(factor_ci,0), na.rm = TRUE),
          access_water_piped_plot_daily__pop = weighted.mean(access_water_piped_plot_daily,  dplyr::coalesce(factor_ci,0), na.rm = TRUE),
          treat_water__pop = weighted.mean(treat_water,  dplyr::coalesce(factor_ci,0), na.rm = TRUE),
          water_meter__pop = weighted.mean(water_meter,  dplyr::coalesce(factor_ci,0), na.rm = TRUE),
          
          access_san_exclusive__pop = weighted.mean(access_san_exclusive,  dplyr::coalesce(factor_ci,0), na.rm = TRUE),
          access_sewer__pop = weighted.mean(access_sewer,  dplyr::coalesce(factor_ci,0), na.rm = TRUE),
          access_septic__pop = weighted.mean(access_septic,  dplyr::coalesce(factor_ci,0), na.rm = TRUE),
          access_latrine_min__pop = weighted.mean(access_latrine_min, dplyr::coalesce(factor_ci,0), na.rm = T),
          access_latrine_max__pop = weighted.mean(access_latrine_max, dplyr::coalesce(factor_ci,0), na.rm = T),
          
          access_sewer_exclusive__pop = weighted.mean(access_sewer_exclusive, dplyr::coalesce(factor_ci,0), na.rm = T),
          access_septic_exclusive__pop = weighted.mean(access_septic_exclusive, dplyr::coalesce(factor_ci,0), na.rm = T),
          access_latrine_exclusive_min__pop = weighted.mean(access_latrine_exclusive_min, dplyr::coalesce(factor_ci,0), na.rm = T),
          access_latrine_exclusive_max__pop = weighted.mean(access_latrine_exclusive_max, dplyr::coalesce(factor_ci,0), na.rm = T),
          improved_san_min__pop = weighted.mean(improved_san_min, dplyr::coalesce(factor_ci,0), na.rm = T),
          improved_san_max__pop = weighted.mean(improved_san_max, dplyr::coalesce(factor_ci,0), na.rm = T),
          
          improved_san_min_exclusive__pop = weighted.mean(improved_san_min_exclusive, dplyr::coalesce(factor_ci,0), na.rm = T),
          improved_san_max_exclusive__pop = weighted.mean(improved_san_max_exclusive, dplyr::coalesce(factor_ci,0), na.rm = T),
          
          access_latrines_unimproved_min__pop = weighted.mean(access_latrines_unimproved_min, dplyr::coalesce(factor_ci,0), na.rm = T),
          access_latrines_unimproved_max__pop = weighted.mean(access_latrines_unimproved_max, dplyr::coalesce(factor_ci,0), na.rm = T),
          access_latrines_unimproved_exclusive_min__pop = weighted.mean(access_latrines_unimproved_exclusive_min, dplyr::coalesce(factor_ci,0), na.rm = T),
          
          
          no_sanitation_access__pop = weighted.mean(no_san_access, dplyr::coalesce(factor_ci,0), na.rm = T))
      
      
      
      if(i == 1){
        summary <-summary_d                        # Create summary data frame if it doesnt exist
      }else{
        summary <-rbind.fill(summary, summary_d)        # append the current data frame
      }
      
    }
    
    
    ## Distinct summary because some missing values create duplicates
    distinct_summary <- distinct(summary)
    distinct_summary$zone <-  ifelse(is.na(distinct_summary$zone), "country",distinct_summary$zone)
    distinct_summary$quintileipc <- ifelse(is.na(distinct_summary$quintile), "total", distinct_summary$quintileipc)
    distinct_summary$sex <- ifelse(is.na(distinct_summary$sex), "all", distinct_summary$sex)
    
    
    ## Argentina only has data for urban
    summary_pop_final <- filter(distinct_summary, !(distinct_summary$pais_c == "ARG" & distinct_summary$zone== "country"))
    summary_pop_final <- filter(summary_pop_final, !(summary_pop_final$pais_c %in% c("JAM", "HTI") & summary_pop_final$quintileipc !="total")) 
    
    summary_pop_final$ave_income_pc  <- ifelse(summary_pop_final$pais_c %in% c("JAM", "HTI"), NA, summary_pop_final$ave_income_pc)
    summary_pop_final$max_income_pc  <- ifelse(summary_pop_final$pais_c %in% c("JAM", "HTI"), NA, summary_pop_final$max_income_pc)
    summary_pop_final$ave_hh_income  <- ifelse(summary_pop_final$pais_c %in% c("JAM", "HTI"), NA, summary_pop_final$ave_hh_income)
    summary_pop_final$max_hh_income  <- ifelse(summary_pop_final$pais_c %in% c("JAM", "HTI"), NA, summary_pop_final$max_hh_income)
    
    
    #save(summary_pop_final,file = "data_pop_summary.rda")   

    rm(distinct_summary, summary_d, summary)
print("Indicators based on population percentage generated")

## Summary data (households) #####     

    data_hh <- data[data$jefe_ci ==1,] 
    
    ## list of dimensions to group data by
    dimensions <- list("pais_c", c("pais_c", "zone"), c("pais_c", "sex"), c("pais_c","quintileipc"), 
                       c("pais_c","zone", "sex"), c("pais_c","zone", "quintileipc"), c("pais_c","sex","quintileipc"), 
                       c("pais_c","zone", "sex", "quintileipc"))
    
    library(plyr)
    ## generate summary information 
    for (i in 1:length(dimensions)){
      
      print(paste0("Dimension: ",dimensions[i]," ", "Num"," ", i, "/",length(dimensions))) 
      summary_d <- data_hh %>% 
        dplyr::group_by_at(vars(one_of(dimensions[[i]]))) %>%   
        dplyr::summarize(
          access_water_piped_house__hh = weighted.mean(access_water_piped_house, dplyr::coalesce(factor_ch,0), na.rm = T),
          access_water_piped_plot__hh = weighted.mean(access_water_piped_plot, dplyr::coalesce(factor_ch,0), na.rm = T),
          water_trucked__hh = weighted.mean(water_trucked, dplyr::coalesce(factor_ch,0), na.rm = T),
          water_on_premises__hh = weighted.mean(water_on_premises, dplyr::coalesce(factor_ch,0), na.rm = T),
          water_distr__hh = weighted.mean(water_distr, dplyr::coalesce(factor_ch,0), na.rm = T),
          consume_distr__hh = weighted.mean(consume_distr,  dplyr::coalesce(factor_ch,0), na.rm= TRUE),
          consume_other_max__hh =  weighted.mean(consume_other_max,  dplyr::coalesce(factor_ch,0), na.rm= TRUE),
          consume_other_min__hh = weighted.mean(consume_other_min,  dplyr::coalesce(factor_ch,0), na.rm = TRUE),
          consume_bottled__hh = weighted.mean(consume_bottled, dplyr::coalesce(factor_ch,0), na.rm = T),
          water_dist_daily__hh = weighted.mean(water_dist_daily,  dplyr::coalesce(factor_ch,0), na.rm = TRUE),
          water_dist_continuity_jmp__hh  = weighted.mean(water_dist_continuity_jmp,  dplyr::coalesce(factor_ch,0), na.rm = TRUE),
          improved_w_access_min__hh  = weighted.mean(improved_w_access_min,  dplyr::coalesce(factor_ch,0), na.rm = TRUE),
          improved_w_access_max__hh  = weighted.mean(improved_w_access_max,  dplyr::coalesce(factor_ch,0), na.rm = TRUE),
          access_water_piped_house_daily__hh = weighted.mean(access_water_piped_house_daily,  dplyr::coalesce(factor_ch,0), na.rm = TRUE),
          access_water_piped_plot_daily__hh = weighted.mean(access_water_piped_plot_daily,  dplyr::coalesce(factor_ch,0), na.rm = TRUE),
          treat_water__hh = weighted.mean(treat_water,  dplyr::coalesce(factor_ch,0), na.rm = TRUE),
          water_meter__hh = weighted.mean(water_meter,  dplyr::coalesce(factor_ch,0), na.rm = TRUE),
          
          access_san_exclusive__hh = weighted.mean(access_san_exclusive,  dplyr::coalesce(factor_ch,0), na.rm = TRUE),
          access_sewer__hh = weighted.mean(access_sewer,  dplyr::coalesce(factor_ch,0), na.rm = TRUE),
          access_septic__hh = weighted.mean(access_septic,  dplyr::coalesce(factor_ch,0), na.rm = TRUE),
          access_latrine_min__hh = weighted.mean(access_latrine_min, dplyr::coalesce(factor_ch,0), na.rm = T),
          access_latrine_max__hh = weighted.mean(access_latrine_max, dplyr::coalesce(factor_ch,0), na.rm = T),
          
          access_sewer_exclusive__hh = weighted.mean(access_sewer_exclusive, dplyr::coalesce(factor_ch,0), na.rm = T),
          access_septic_exclusive__hh = weighted.mean(access_septic_exclusive, dplyr::coalesce(factor_ch,0), na.rm = T),
          access_latrine_exclusive_min__hh = weighted.mean(access_latrine_exclusive_min, dplyr::coalesce(factor_ch,0), na.rm = T),
          access_latrine_exclusive_max__hh = weighted.mean(access_latrine_exclusive_max, dplyr::coalesce(factor_ch,0), na.rm = T),
          improved_san_min__hh = weighted.mean(improved_san_min, dplyr::coalesce(factor_ch,0), na.rm = T),
          improved_san_max__hh = weighted.mean(improved_san_max, dplyr::coalesce(factor_ch,0), na.rm = T),
          
          improved_san_min_exclusive__hh = weighted.mean(improved_san_min_exclusive, dplyr::coalesce(factor_ch,0), na.rm = T),
          improved_san_max_exclusive__hh = weighted.mean(improved_san_max_exclusive, dplyr::coalesce(factor_ch,0), na.rm = T),
          
          access_latrines_unimproved_min__hh = weighted.mean(access_latrines_unimproved_min, dplyr::coalesce(factor_ch,0), na.rm = T),
          access_latrines_unimproved_max__hh = weighted.mean(access_latrines_unimproved_max, dplyr::coalesce(factor_ch,0), na.rm = T),
          access_latrines_unimproved_exclusive_min__hh = weighted.mean(access_latrines_unimproved_exclusive_min, dplyr::coalesce(factor_ch,0), na.rm = T),
          
          no_sanitation_access__hh = weighted.mean(no_san_access, dplyr::coalesce(factor_ch,0), na.rm = T))
      
      
      
      if(i == 1){
        summary_hh <-summary_d                        # Create summary data frame if it doesnt exist
      }else{
        summary_hh <-rbind.fill(summary_hh, summary_d)        # append the current data frame
      }
      
    }
    
    
    ##
    distinct_summary_hh <- distinct(summary_hh)
    distinct_summary_hh$zone <-  ifelse(is.na(distinct_summary_hh$zone), "country",distinct_summary_hh$zone)
    distinct_summary_hh$quintileipc <- ifelse(is.na(distinct_summary_hh$quintile), "total", distinct_summary_hh$quintileipc)
    distinct_summary_hh$sex <- ifelse(is.na(distinct_summary_hh$sex), "all", distinct_summary_hh$sex)
    
    
    
    
    rm(summary_hh, summary_d, data_hh, dimensions)
    
    ## Argentina only has data for urban
    summary_hh_final <- filter(distinct_summary_hh, !(distinct_summary_hh$pais_c == "ARG" & distinct_summary_hh$zone== "country"))
    summary_hh_final <- filter(summary_hh_final, !(summary_hh_final$pais_c == "JAM" & summary_hh_final$quintileipc !="total")) 
    
    #rm(distinct_summary_hh)
    
    #save(summary_hh_final,file = "data_hh_summary.rda")
    #load("data_hh_summary.rda")

print("Indicators based on household percentage generated")
    

## Add population percentages ####
### The population_year.csv file must be created for the new year. It must contain the ISO, year of survey, and population of each country. 
### This code creates population percentages living in each dimension. Refer to the methodology document for more details. 

    
    d <- select(data, c("pais_c","zona_c","zone", "sex", "quintileipc" ,"jefe_ci", "sexo_ci",    ## Isolate the necessary variables in smaller data frame, d
                        "factor_ci", "factor_ch", "idh_ch", "idp_ci"))
    #rm(data)
    
    
    d$rural <- ifelse(unclass(d$zona_c) == 0,1,0)                              ## Generate population ratios for rural urban
    d$urban <- ifelse(unclass(d$zona_c) == 1,1,0)
    
    
    d$hh_id <- paste0(d$pais_c, d$idh_ch)                                      ## Create household ids
    d$fhh<- ifelse(d$jefe_ci == 1 & unclass(d$sexo_ci) == 2,1,0)               ## Generate population ratios for "living in female and male headed households
    d$mhh<- ifelse(d$jefe_ci == 1 & unclass(d$sexo_ci) == 1,1,0)
    
    d$female <- ifelse(d$hh_id %in% unique(d$hh_id[d$fhh==1]), 1,0)            ## Classifies all in family as having a female or male head of household
    d$male <- ifelse(d$hh_id %in% unique(d$hh_id[d$mhh==1]), 1,0)              ## There are a few cases where people marked both a female and male hh, in which case they will count in both categories (resulting % may not add to 100)
    
    
    ## generate percentages for area data (% of pop in each income quintile and overall living in urban or rural areas)
        dimensions <- list("pais_c", c("pais_c","quintileipc"))
        
        library(plyr)
        ## generate summary information 
        for (i in 1:length(dimensions)){
          
          print(paste0("Dimension: ",dimensions[i]," ", "Num"," ", i, "/",length(dimensions))) 
          summary_d <- d %>% 
            dplyr::group_by_at(vars(one_of(dimensions[[i]]))) %>%   
            dplyr::summarize(
              urban = weighted.mean(urban,dplyr::coalesce(factor_ci,0),na.rm= TRUE),
              rural = weighted.mean(rural,dplyr::coalesce(factor_ci,0),na.rm= TRUE))
          
          
          
          if(i == 1){
            summary_area <-summary_d                        # Create summary data frame if it doesnt exist
          }else{
            summary_area <-rbind.fill(summary_area, summary_d)        # append the current data frame
          }
          
        }
        summary_area <- distinct(summary_area)
        
        summary_area$quintileipc <- ifelse(is.na(summary_area$quintile), "total", summary_area$quintileipc)
        summary_area2<- pivot_longer(summary_area, c(urban, rural), names_to = "zone", values_to = "area_prc")
    
    
    
    ## generate percentages for head of household data (% of pop in each income quintile and zone classification with female vs male head of household)
        
        dimensions <- list("pais_c", c("pais_c", "zone"), c("pais_c","quintileipc"),c("pais_c","zone", "quintileipc"))
        
        library(plyr)
        ## generate summary information 
        for (i in 1:length(dimensions)){
          
          print(paste0("Dimension: ",dimensions[i]," ", "Num"," ", i, "/",length(dimensions))) 
          summary_d <- d %>% 
            dplyr::group_by_at(vars(one_of(dimensions[[i]]))) %>%   
            dplyr::summarize(
              female = weighted.mean(female, dplyr::coalesce(factor_ci,0), na.rm= TRUE),
              male = weighted.mean(male, dplyr::coalesce(factor_ci,0), na.rm= TRUE))
          
          
          if(i == 1){
            summary_sex <-summary_d                        # Create summary data frame if it doesnt exist
          }else{
            summary_sex <-rbind.fill(summary_sex, summary_d)        # append the current data frame
          }
          
        }
        
        summary_sex <- distinct(summary_sex)
        summary_sex$zone <-  ifelse(is.na(summary_sex$zone), "country",summary_sex$zone)
        summary_sex$quintileipc <- ifelse(is.na(summary_sex$quintile), "total", summary_sex$quintileipc)
        
        summary_sex2<- pivot_longer(summary_sex, c(female, male), names_to = "sex", values_to = "sex_prc")
        
    ## join area and sex information
    pop_prc <- full_join(summary_sex2, summary_area2, by = c("pais_c" = "pais_c", "zone"="zone", "quintileipc" = "quintileipc"))
    pop_prc2 <- rbind.fill(pop_prc, summary_area2)
    pop_prc2 <- distinct(pop_prc2)
    
    ## fill empty sex values as "all"
    pop_prc2$sex <- ifelse(is.na(pop_prc2$sex), "all", pop_prc2$sex)
    
    
    #ARG exception handling
    ## ARG does not survey their rural population, so area prcs cannot be calculated for most dimensional breakdowns. 
    ## Data on overall rural and urban populations was taken from the national statistics agency, INDEC.
    pop_prc2$area_prc <- ifelse(pop_prc2$pais_c == "ARG" & pop_prc2$quintileipc== "total" & pop_prc2$zone == "urban", 0.92,pop_prc2$area_prc)
    pop_prc2$area_prc <-ifelse(pop_prc2$pais_c == "ARG" & pop_prc2$quintileipc == "total" & pop_prc2$zone == "rural",0.08 ,pop_prc2$area_prc)
    pop_prc2$area_prc<- ifelse(pop_prc2$pais_c == "ARG" &pop_prc2$quintileipc != "total", NA, pop_prc2$area_prc)
    
    
    
    
    olas_pop_draft <- left_join(summary_pop_final, pop_prc2,                       ## Join population information with summary data set.
                                by = c("pais_c" = "pais_c","sex" = "sex" , "zone"="zone", "quintileipc" = "quintileipc")) 
    
    
    total_population <- select(read.csv("population_2020.csv"), iso, population, year_survey)
    
    olas_pop_draft <- left_join(olas_pop_draft, total_population, by = c("pais_c" = "iso"))
    
    ## calculate population percentages
    olas_pop_draft$population_prc <- ifelse(olas_pop_draft$zone == "country" & olas_pop_draft$sex == "all" & olas_pop_draft$quintileipc == "total",1,
                                            ifelse(olas_pop_draft$zone != "country" & olas_pop_draft$sex == "all" & olas_pop_draft$quintileipc == "total", olas_pop_draft$area_prc, 
                                                   ifelse(olas_pop_draft$zone == "country" & olas_pop_draft$sex != "all" & olas_pop_draft$quintileipc == "total", olas_pop_draft$sex_prc,
                                                          ifelse(olas_pop_draft$zone=="country"& olas_pop_draft$sex== "all" & olas_pop_draft$quintileipc != "total", .2 ,
                                                                 ifelse(olas_pop_draft$zone != "country" & olas_pop_draft$sex == "all" & olas_pop_draft$quintileipc != "total", olas_pop_draft$area_prc*.2,
                                                                        ifelse(olas_pop_draft$zone == "country" & olas_pop_draft$sex != "all" & olas_pop_draft$quintileipc != "total", olas_pop_draft$sex_prc*.2,
                                                                               ifelse(olas_pop_draft$zone != "country" & olas_pop_draft$sex != "all" & olas_pop_draft$quintileipc == "total", olas_pop_draft$sex_prc*olas_pop_draft$area_prc,
                                                                                      ifelse(olas_pop_draft$zone != "country" & olas_pop_draft$sex != "all" & olas_pop_draft$quintileipc != "total", olas_pop_draft$sex_prc*olas_pop_draft$area_prc*.2,NA ))))))))
    
    
    ## multiply population percentages to get an estimated population
    olas_pop_draft$population_est <- as.numeric(olas_pop_draft$population)*olas_pop_draft$population_prc

print("Population data generated")

## Merge population and household data #### 

  olas_pop <- select(olas_pop_draft, 1,40:42,45:47,2:5,6:39)               ## rearrange rows
  
  summary_final <- merge(olas_pop, summary_hh_final)                      ## merging population percentages with household percentages

## Clean up ####   

  ##rm(pop,population,olas_pop,olas_pop_draft,summary_hh_final, summary_pop_final,d,i)
  
  is.nan.data.frame <- function(x)                                        ## create function to eliminate NaN
    do.call(cbind, lapply(x, is.nan))
  
  summary_final[is.nan.data.frame(summary_final)] <-NA                        ## eliminate NaN

## Write final files ####   
  library(tidyr)
  olas_2020_wide <- summary_final
  
  save(olas_2020_wide,file = "olas_2020_wide.rda")                         