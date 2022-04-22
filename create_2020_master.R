# STEP THREE
# Create master data set
print("create_2020_master_data_set.R starting") 
rm(list=ls())

library(tidyr)
library(dplyr)
library(plyr)
library(haven)

## Combine data sets with variables of interest####

files<- list.files("input_data_2020", pattern = "\\.dta$")

# Create merged database using all files, if new variables are needed add them to ref.csv

ColREF<-c("ARG","BOL","CHL","COL","CRI","DOM","ECU","GTM","HTI","JAM","MEX","PER","PRY",
          "SLV","SUR","TTO","VEN")


Keep_Var<-as.data.frame(read.csv("ref_2020.csv"))


## combine country data files with the desired variables. 
### Note:: Order of the files in the files list must correspond to the order of the countries in ColREF 

for (f in 1:length(files)){
  print(paste0("File Num: ",f,"/",length(ColREF), " File: ", files[f], " Completed: ", round((f/length(files)),2)*100,"%"))
  df1      <- read_dta(paste0("input_data_2020","/",files[f]))   # read the file
  Retainer <- as.character(Keep_Var[,ColREF[f]])
  Retainer <- Retainer[Retainer!=""]
  
  df2<-df1[,c(Retainer)]
  
  if(f == 1){
    DF <-df2                        # Create if it does not exists
  }else{
    DF <-rbind.fill(DF, df2)        # append the current file
  }
}
#Check all countries are loaded
table(DF$pais_c)
table(DF$pais_c,DF$sexo_ci)

## rename
data_2020 <- DF


# data check: total households surveyed by country
hh_per_pais <- data_2020 %>%
  group_by(pais_c) %>%
  dplyr::summarize(no_hh = length(unique(idh_ch)))

#write.csv(hh_per_pais, "hogares_encuestados_update.csv")

## Quick clean #####
    ## There are household ids with multiple heads of household
    data_2020$hh_id <- paste0(data_2020$pais_c, data_2020$idh_ch)
    multiple_jefes_per_hh <- data_2020 %>%
      group_by(hh_id) %>%
      summarize(hh = sum(jefe_ci, na.rm = T))
    multiple_jefes <- multiple_jefes_per_hh[multiple_jefes_per_hh$hh>1,]
    
    ## Some are duplicate records, some are households with multiple heads 
    
    # Identify duplicate records and deleted duplicates
    #sum(duplicated(data))
    data_2020 <- distinct(data_2020)
    rm(DF,df1,df2,multiple_jefes_per_hh)

## Save master data file ####

save(data_2020,file = "master_2020.rda")

print("create_2020_master.R finished") 
