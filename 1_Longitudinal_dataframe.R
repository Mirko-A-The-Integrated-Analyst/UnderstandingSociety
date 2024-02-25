# Understanding Society ----

# This readme file provides a guideline for importing and merging various data files, extracting variables, and creating a longitudinal data frame using an extensive UK Household Longitudinal survey called UKHLS (i.e., Understanding Society Survey).

# Project Showcase Notice ----
#This repository contains code and materials for the purpose of showcasing the project. Please note that sensitive and official parts of the project have been either partially or totally removed.
#Purpose
#The content provided here is intended to demonstrate the capabilities, design patterns, and methodologies employed in the project. It is not meant for production use and may lack certain functionalities that are part of the full, official version.
#Limitations
#- **Sensitive Information**: Any sensitive information, including but not limited to private keys, credentials, and personal data, has been removed or anonymized.
#- **Partial Functionality**: Some sections of the code may have been modified or omitted to ensure the security and privacy of the underlying system or data. As such, the repository may not represent the full functionality of the original project.
#- **Showcase Only**: The provided code and documents are intended for viewing and demonstration purposes only.


# 1. Load the library----
install.packages(c('tidyr', 'plyr','dplyr', 'labelled'))
library(tidyr) # Data Manipulation
library(plyr) # Data Manipulation
library(dplyr) # Data Manipulation

# 2. Dataset setup ----

## Set up input file folder path 
# folderS3_path <- "s3://InternalUseOnlyUKDA-6614-tab"

## Set up waves and dafiles - UKHLS dataset consists of single wave and single data file. You need to insert the waves and data files you are interested in.
### Set up waves and Set up data files
#waves_list <- c("l","k", "j", "i", "h", "g") #e.g., wave l (12) = 2020-21; # wave g (7) = 2015-2016
waves_list <- c("l","k","j", "i", "h", "g") #e.g., wave l (12) = 2020-21; # wave k (11) = 2019-2020
datafiles_list <- c("indresp", "hhresp") # indresp provides individual data and hhresp provides household data

# Create dataframe for each wave and each data files
for (w in waves_list) {
  for (d in datafiles_list){
    path <- paste0(folder_path, "/", w, "_", d, ".tab")
    
    # Check that the file exists
    if (file.exists(path)){
      cat(paste0(path," : File exists"), "\n")
    } else {
      cat(paste0(path," : Please check if the file is present locally"), "\n")
    }
    
    # Read data from S3
    data <- read.table(path, header=TRUE, 
                    # replace missing values (integers -10 to -1) with NA
                    na.string = c("NA", "-10", "-9", "-8", "-7", "-2", "-1"))
    
    assign(paste0(w, "_", d), data)
    # housekeeping
    rm(data)
  }}


# 3. Merge Individual and Households data frames ----
# We use household identifier (*hidp*) to identify members of the same households withing one wave
## From 2015/2016 to 2020/2021
l_wave<- join(l_indresp,l_hhresp, by=c("l_hidp"))
k_wave <- join(k_indresp, k_hhresp, by=c("k_hidp"))
j_wave<- join(j_indresp,j_hhresp, by=c("j_hidp"))
i_wave <- join(i_indresp, i_hhresp, by=c("i_hidp"))
h_wave<- join(h_indresp,h_hhresp, by=c("h_hidp"))
g_wave <- join(g_indresp, g_hhresp, by=c("g_hidp"))

## Check the data
# Wave: l
dim(l_indresp)
#dim(l_hhresp)
dim(l_wave)

# Wave: K
dim(k_indresp)
# dim(k_hhresp)
dim(k_wave)

# Wave: j
dim(j_indresp)
# dim(j_hhresp)
dim(j_wave)

# Wave: i
dim(i_indresp)
#dim(i_hhresp)
dim(i_wave)

# Wave: h
dim(h_indresp)
#dim(h_hhresp)
dim(h_wave)

# Wave: g
dim(g_indresp)
#dim(g_hhresp)
dim(g_wave)

# 4. Variables extraction  ----
## Variables info and selection methodologies, please see UKHLS_Variables_List on share point https://jInternalUseOnly
## or see the online documentation https://www.InternalUseOnly

# Selected variables

## a. Demographic (5 variables)
### Age: dvage; Gender: sex; legal marital status: marstat  {waves 1:12}
### Current labour force status: jbstat {waves 1:12}; Highest qualification: qfhigh {waves 1:12}

## b. Geography (2 variables)
### Country of residence: country , government office region: gor_dv  {waves 1:12}

## c. Income (2 variables) 
### Total net personal income - no deductions: fimnnet_dv {waves 1:12} 
### Total household net income - no deductions: fihhmnnet1_dv {waves 1:12}

## d. General household (3 variables)
### Number of people in household: hhsize {waves 1:12} 
### Modified OECD equivalence scale: ieqmoecd_dv {waves 1:12}
### Number of children in household: nkids_dv  {waves 1:12}

## e. Behavioural Sciences (3 variables) 
### Life satisfaction: sclfsato {waves 1:12}
### Job satisfaction:  jbsat {waves 1:12}
### Framming_bills: fuelduel {waves 1:12}

## f. Questions for Repossessions (9 variables) 
### Housing tenure: tenure_dv {waves 1:12}
### type of mortgage: mgtype {waves 1:12}
### monthly housing cost including mortgage principal payments: houscost1_dv {waves 1:12}
### monthly housing cost excluding mortgage principal payments houscost2_dv {waves 1:12}
### year mortgage began: hsyr04 {waves 1:12} 
### years left to pay mortgage: mglife {waves 1:12}
### monthly mortgage payment including imputations: xpmg_dv {waves 1:12}
### estimated interest in monthly mortgage payment: xpmgint_dv {waves 1:12}
### last rent payment, montly: rent_dv {waves 1:12}


## g. Subjective questions for Repossessions (9 variables) 
### Financial situation future: finfut {waves 1:12}
### Financial situation now: finnow {waves 1:12}
### General health: sf1 {waves 1:12}
### Satisfaction with health: sclfsat1 {waves 1:12}
### Expects to move in next year: xpmove {waves 1:12}
### Problem with paying: xphsdb {waves 1:12}
### Up to date bills: xphsdba {waves 1:12}
### Satisfaction with income: sclfsat2 {waves 1:12}
### Satisfaction with house/flat: sclfsat3 {waves: 12}

# Other: 
# Interview date (Year): intdaty_dv


# wave: 1=a, 2=b,3=c,4=d,5=e,6=f,7=g,8=h,9=i,10=j,11=k,12=l 
l_data <- plyr::rename(l_wave, c(l_dvage = "age", l_sex = "sex", l_marstat = "marstat", l_jbstat = "jbstat", l_qfhigh = "qfhigh", # Demographic
                                 l_country = "country", l_gor_dv = "gor_dv",  # Geography
                                 l_fimnnet_dv = "fimnnet_dv", l_fihhmnnet1_dv = "fihhmnnet1_dv",  # Income
                                 l_hhsize = "hhsize", l_ieqmoecd_dv = "ieqmoecd_dv", l_nkids_dv = "nkids_dv", # General household
                                 l_sclfsato = "sclfsato",  l_jbsat = "jbsat",  l_fuelduel = "fuelduel", # Behavioural Sciences
                                 l_houscost1_dv = "houscost1_dv", l_houscost2_dv = "houscost2_dv",  # Questions for Repossessions
                                 l_hsyr04 = "hsyr04", l_mglife = "mglife", l_xpmg_dv = "xpmg_dv",
                                 l_xpmgint_dv = "xpmgint_dv", l_mgtype = "mgtype", l_rent_dv = "rent_dv", l_tenure_dv = "tenure_dv", 
                                 l_finfut = "finfut", l_finnow = "finnow", l_sf1 = "sf1", # Subjective questions for Repossessions
                                 l_sclfsat1 = "sclfsat1", l_xpmove = "xpmove", l_xphsdb = "xphsdb",  
                                 l_xphsdba = "xphsdba", l_sclfsat2 = "sclfsat2", l_sclfsat3 = "sclfsat3", 
                                 l_intdaty_dv = "intdaty_dv" # date
))

k_data <- plyr::rename(k_wave, c(k_dvage = "age", k_sex = "sex", k_marstat = "marstat", k_jbstat = "jbstat", k_qfhigh = "qfhigh",  # Demographic
                                 k_country = "country", k_gor_dv = "gor_dv",  # Geography
                                 k_fimnnet_dv = "fimnnet_dv", k_fihhmnnet1_dv = "fihhmnnet1_dv",  # Income
                                 k_hhsize = "hhsize", k_ieqmoecd_dv = "ieqmoecd_dv", k_nkids_dv = "nkids_dv", # General household
                                 k_sclfsato = "sclfsato",  k_jbsat = "jbsat",  k_fuelduel = "fuelduel", # Behavioural Sciences
                                 k_houscost1_dv = "houscost1_dv", k_houscost2_dv = "houscost2_dv",  # Questions for Repossessions
                                 k_hsyr04 = "hsyr04", k_mglife = "mglife", k_xpmg_dv = "xpmg_dv",
                                 k_xpmgint_dv = "xpmgint_dv", k_mgtype = "mgtype", k_rent_dv = "rent_dv", k_tenure_dv = "tenure_dv", 
                                 k_finfut = "finfut", k_finnow = "finnow", k_sf1 = "sf1", # Subjective questions for Repossessions
                                 k_sclfsat1 = "sclfsat1", k_xpmove = "xpmove", k_xphsdb = "xphsdb", 
                                 k_xphsdba = "xphsdba", k_sclfsat2 = "sclfsat2" ,
                                 k_intdaty_dv = "intdaty_dv" # date
))

j_data <- plyr::rename(j_wave, c(j_dvage = "age", j_sex = "sex", j_marstat = "marstat", j_jbstat = "jbstat", j_qfhigh = "qfhigh", # Demographic
                                 j_country = "country", j_gor_dv = "gor_dv",  # Geography
                                 j_fimnnet_dv = "fimnnet_dv", j_fihhmnnet1_dv = "fihhmnnet1_dv", # Income
                                 j_hhsize = "hhsize", j_ieqmoecd_dv = "ieqmoecd_dv", j_nkids_dv = "nkids_dv", # General household
                                 j_sclfsato = "sclfsato",  j_jbsat = "jbsat",  j_fuelduel = "fuelduel", # Behavioural Sciences
                                 j_houscost1_dv = "houscost1_dv", j_houscost2_dv = "houscost2_dv",  # Questions for Repossessions
                                 j_hsyr04 = "hsyr04", j_mglife = "mglife", j_xpmg_dv = "xpmg_dv",
                                 j_xpmgint_dv = "xpmgint_dv", j_mgtype = "mgtype", j_rent_dv = "rent_dv", j_tenure_dv = "tenure_dv", 
                                 j_finfut = "finfut", j_finnow = "finnow", j_sf1 = "sf1", # Subjective questions for Repossessions
                                 j_sclfsat1 = "sclfsat1", j_xpmove = "xpmove", j_xphsdb = "xphsdb",  
                                 j_xphsdba = "xphsdba", j_sclfsat2 = "sclfsat2",
                                 j_intdaty_dv = "intdaty_dv" # date
))

i_data <- plyr::rename(i_wave, c(i_dvage = "age", i_sex = "sex", i_marstat = "marstat", i_jbstat = "jbstat", i_qfhigh = "qfhigh",  # Demographic
                                 i_country = "country", i_gor_dv = "gor_dv",  # Geography
                                 i_fimnnet_dv = "fimnnet_dv", i_fihhmnnet1_dv = "fihhmnnet1_dv",  # Income
                                 i_hhsize = "hhsize", i_ieqmoecd_dv = "ieqmoecd_dv", i_nkids_dv = "nkids_dv", # General household
                                 i_sclfsato = "sclfsato",  i_jbsat = "jbsat",  i_fuelduel = "fuelduel", # Behavioural Sciences
                                 i_houscost1_dv = "houscost1_dv", i_houscost2_dv = "houscost2_dv",  # Questions for Repossessions
                                 i_hsyr04 = "hsyr04", i_mglife = "mglife", i_xpmg_dv = "xpmg_dv",
                                 i_xpmgint_dv = "xpmgint_dv", i_mgtype = "mgtype", i_rent_dv = "rent_dv", i_tenure_dv = "tenure_dv", 
                                 i_finfut = "finfut", i_finnow = "finnow", i_sf1 = "sf1", # Subjective questions for Repossessions
                                 i_sclfsat1 = "sclfsat1", i_xpmove = "xpmove", i_xphsdb = "xphsdb", 
                                 i_xphsdba = "xphsdba", i_sclfsat2 = "sclfsat2",
                                 i_intdaty_dv = "intdaty_dv" # date
))

h_data <- plyr::rename(h_wave, c(h_dvage = "age", h_sex = "sex", h_marstat = "marstat", h_jbstat = "jbstat", h_qfhigh = "qfhigh",  # Demographic
                                 h_country = "country", h_gor_dv = "gor_dv",  # Geography
                                 h_fimnnet_dv = "fimnnet_dv", h_fihhmnnet1_dv = "fihhmnnet1_dv",  # Income
                                 h_hhsize = "hhsize", h_ieqmoecd_dv = "ieqmoecd_dv", h_nkids_dv = "nkids_dv", # General household
                                 h_sclfsato = "sclfsato",  h_jbsat = "jbsat",  h_fuelduel = "fuelduel", # Behavioural Sciences
                                 h_houscost1_dv = "houscost1_dv", h_houscost2_dv = "houscost2_dv",  # Questions for Repossessions
                                 h_hsyr04 = "hsyr04", h_mglife = "mglife", h_xpmg_dv = "xpmg_dv",
                                 h_xpmgint_dv = "xpmgint_dv", h_mgtype = "mgtype", h_rent_dv = "rent_dv", h_tenure_dv = "tenure_dv", 
                                 h_finfut = "finfut", h_finnow = "finnow", h_sf1 = "sf1", # Subjective questions for Repossessions
                                 h_sclfsat1 = "sclfsat1", h_xpmove = "xpmove", h_xphsdb = "xphsdb",  
                                 h_xphsdba = "xphsdba", h_sclfsat2 = "sclfsat2",
                                 h_intdaty_dv = "intdaty_dv" # date
))

g_data <- plyr::rename(g_wave, c(g_dvage = "age", g_sex = "sex", g_marstat = "marstat", g_jbstat = "jbstat", g_qfhigh = "qfhigh",  # Demographic
                                 g_country = "country", g_gor_dv = "gor_dv",  # Geography
                                 g_fimnnet_dv = "fimnnet_dv", g_fihhmnnet1_dv = "fihhmnnet1_dv", # Income
                                 g_hhsize = "hhsize", g_ieqmoecd_dv = "ieqmoecd_dv", g_nkids_dv = "nkids_dv", # General household
                                 g_sclfsato = "sclfsato",  g_jbsat = "jbsat",  g_fuelduel = "fuelduel", # Behavioural Sciences
                                 g_houscost1_dv = "houscost1_dv", g_houscost2_dv = "houscost2_dv",  # Questions for Repossessions
                                 g_hsyr04 = "hsyr04", g_mglife = "mglife", g_xpmg_dv = "xpmg_dv",
                                 g_xpmgint_dv = "xpmgint_dv", g_mgtype = "mgtype", g_rent_dv = "rent_dv", g_tenure_dv = "tenure_dv", 
                                 g_finfut = "finfut", g_finnow = "finnow", g_sf1 = "sf1", # Subjective questions for Repossessions
                                 g_sclfsat1 = "sclfsat1", g_xpmove = "xpmove", g_xphsdb = "xphsdb", 
                                 g_xphsdba = "xphsdba", g_sclfsat2 = "sclfsat2",
                                 g_intdaty_dv = "intdaty_dv" # date
))


# 5. Long format to create longitudinal data frame----
# We use a 'long format' approach, , where each row represents a particular year’s data for a particular respondent. 
# This happens to be the most suitable arrangement for more advanced statistical models of various kinds, and is also a more efficient means of storing the data, particularly when there are complex patterns of non-trivial attrition. By using functions that harvest data in the previous data row (or the one before the previous, or the next) it is straightforward to measure the extent of transitions of various kinds on an annual basis. More info: [link](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwj3k8Si27L7AhWSOcAKHRoSCZc4ChAWegQIBhAB&url=http%3A%2F%2Fdoc.ukdataservice.ac.uk%2Fdoc%2F8715%2Fmrdoc%2Fpdf%2F8715_user_guide.pdf&usg=AOvVaw26UHZTqfUutI1CoWsWrUsY)
# Filter selected variable create a column named 'wave'.
# The person identifier pidp is the only variable that does not include a wave prefix

# wave: 1=a, 2=b,3=c,4=d,5=e,6=f,7=g,8=h,9=i,10=j,11=k,12=l 
# Wave 12 - l
l_vars<- c("pidp", "age", "sex", "marstat", "country", "gor_dv", "jbstat", "qfhigh", # Demographic & Geography
           "fimnnet_dv", "fihhmnnet1_dv",  # Income
           "hhsize", "ieqmoecd_dv", "nkids_dv", # General household
           "sclfsato", "jbsat", "fuelduel", # Behavioural Sciences
           "houscost1_dv", "houscost2_dv", # Questions for Repossessions
           "hsyr04", "mglife","xpmg_dv",
           "xpmgint_dv", "mgtype", "rent_dv", "tenure_dv", 
           "finfut", "finnow", "sf1", # Subjective questions for Repossessions
           "sclfsat1", "xpmove", "xphsdb", "xphsdba", "sclfsat2", "sclfsat3", 
           "intdaty_dv") # Other
l_dataReposs <- l_data[,l_vars]
l_dataReposs$wave <- 12
# Wave 11 - k
k_vars<- c("pidp", "age", "sex", "marstat", "country", "gor_dv", "jbstat", "qfhigh", # Demographic & Geography
           "fimnnet_dv", "fihhmnnet1_dv",  # Income
           "hhsize", "ieqmoecd_dv", "nkids_dv", # General household
           "sclfsato", "jbsat", "fuelduel", # Behavioural Sciences
           "houscost1_dv", "houscost2_dv", # Questions for Repossessions
           "hsyr04", "mglife","xpmg_dv",
           "xpmgint_dv", "mgtype", "rent_dv", "tenure_dv", 
           "finfut", "finnow", "sf1", # Subjective questions for Repossessions
           "sclfsat1", "xpmove", "xphsdb", "xphsdba", "sclfsat2",
           "intdaty_dv") # Other
k_dataReposs <- k_data[,k_vars]
k_dataReposs$wave <- 11 
# Wave 10 - j
j_vars<- c("pidp", "age", "sex", "marstat", "country", "gor_dv", "jbstat", "qfhigh",  # Demographic & Geography
           "fimnnet_dv", "fihhmnnet1_dv",  # Income
           "hhsize", "ieqmoecd_dv", "nkids_dv", # General household
           "sclfsato", "jbsat", "fuelduel", # Behavioural Sciences
           "houscost1_dv", "houscost2_dv", # Questions for Repossessions
           "hsyr04", "mglife","xpmg_dv",
           "xpmgint_dv", "mgtype", "rent_dv", "tenure_dv", 
           "finfut", "finnow", "sf1", # Subjective questions for Repossessions
           "sclfsat1", "xpmove", "xphsdb", "xphsdba", "sclfsat2",
           "intdaty_dv") # Other
j_dataReposs <- j_data[,j_vars]
j_dataReposs$wave <- 10
# Wave 9 - i
i_vars<- c("pidp", "age", "sex", "marstat", "country", "gor_dv", "jbstat", "qfhigh",  # Demographic & Geography
           "fimnnet_dv", "fihhmnnet1_dv",  # Income
           "hhsize", "ieqmoecd_dv", "nkids_dv", # General household
           "sclfsato", "jbsat", "fuelduel", # Behavioural Sciences
           "houscost1_dv", "houscost2_dv", # Questions for Repossessions
           "hsyr04", "mglife","xpmg_dv",
           "xpmgint_dv", "mgtype", "rent_dv", "tenure_dv", 
           "finfut", "finnow", "sf1", # Subjective questions for Repossessions
           "sclfsat1", "xpmove", "xphsdb", "xphsdba", "sclfsat2",
           "intdaty_dv") # Other 
i_dataReposs <- i_data[,i_vars]
i_dataReposs$wave <- 9
# Wave 8 - h
h_vars<- c("pidp", "age", "sex", "marstat", "country", "gor_dv", "jbstat", "qfhigh", # Demographic & Geography
           "fimnnet_dv", "fihhmnnet1_dv",  # Income
           "hhsize", "ieqmoecd_dv", "nkids_dv", # General household
           "sclfsato", "jbsat", "fuelduel", # Behavioural Sciences
           "houscost1_dv", "houscost2_dv", # Questions for Repossessions
           "hsyr04", "mglife","xpmg_dv",
           "xpmgint_dv", "mgtype", "rent_dv", "tenure_dv", 
           "finfut", "finnow", "sf1", # Subjective questions for Repossessions
           "sclfsat1", "xpmove", "xphsdb", "xphsdba", "sclfsat2",
           "intdaty_dv") # Other
h_dataReposs <- h_data[,h_vars]
h_dataReposs$wave <- 8
# Wave 7 - g
g_vars<-c("pidp", "age", "sex", "marstat", "country", "gor_dv", "jbstat", "qfhigh", # Demographic & Geography
          "fimnnet_dv", "fihhmnnet1_dv",  # Income
          "hhsize", "ieqmoecd_dv", "nkids_dv", # General household
          "sclfsato", "jbsat", "fuelduel", # Behavioural Sciences
          "houscost1_dv", "houscost2_dv", # Questions for Repossessions
          "hsyr04", "mglife","xpmg_dv",
          "xpmgint_dv", "mgtype", "rent_dv", "tenure_dv", 
          "finfut", "finnow", "sf1", # Subjective questions for Repossessions
          "sclfsat1", "xpmove", "xphsdb", "xphsdba", "sclfsat2",
          "intdaty_dv") # Other 
g_dataReposs <- g_data[,g_vars]
g_dataReposs$wave <- 7 


# 6. Final longitudinal dataframe ----
# Combine all the final data waves into a single dataframe
# Note: the final data waves are created by running "2_Variable_Extraction_UKHLS.R"

dataReposs_all_reposs <- rbind.fill(l_dataReposs, k_dataReposs, j_dataReposs, 
                                    i_dataReposs, h_dataReposs, g_dataReposs) # check total observations

# dim(dataReposs_all_reposs)
table(dataReposs_all_reposs$wave)

# 7.  Reduce data frame to England and Wales ----
# 1 = England; 2 = Wales (The country's value can be checked online here https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/variable/country)
dataReposs <- subset(dataReposs_all_reposs, country == 1 | country == 2)
#Check the data
# dim(dataReposs)
# Check if the data contains all the waves and country
table(dataReposs$wave, dataReposs$country)

# Save dataframe
#save(list=c("dataReposs"), file = "dataReposs.Rdata")
#write.csv(dataReposs_Reposs, "~/BoldCivFamDrivers/dataReposs_Reposs.Rdata.csv", row.names=FALSE)

# Housekeeping
# Remove dataframe for each wave and each data files
for (w in waves_list) {
  for (d in datafiles_list){
    assign(paste0(w, "_", d), data)
    # housekeeping
    rm(data)
  }}
# Remove join data frame
# create a vector of variable names to be removed
vars_to_remove <- c("l_data", "k_data", "j_data","i_data", "h_data", "g_data",
                    "l_dataReposs", "k_dataReposs", "j_dataReposs","i_dataReposs", "h_dataReposs", "g_dataReposs", # data
                    "l_wave", "k_wave", "j_wave","i_wave", "h_wave", "g_wave") # wave
# loop and remove each variable
for (var in vars_to_remove) {
  if (exists(var)) {  # check if the variable exists in the workspace
    rm(list = var)  # remove the variable
  }
}



