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

# How to search variable's values ----
# To search and check variable's details, you can use:
# a) An Excel file in shared point: https://InternalUseOnly
# b) Search the variable's name on line: https://InternalUseOnly



# Load the library----
install.packages(c('tidyr', 'plyr','dplyr', 'labelled'))


library(tidyr) # Data Manipulation
library(plyr) # Data Manipulation
library(dplyr) # Data Manipulation
library(labelled) # Data Manipulation


## Wave: wave, wave_f ----
# Note that the value of the wave can be checked by looking at any variable online (see point b above), as each variable is grouped by wave with its related year.
# For example, you can go to the link: https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/variable/sex
table(dataReposs$wave)
class(dataReposs$wave)
var_label(dataReposs$wave) <- "Wave"

# Convert into factor
dataReposs$wave_f <- factor(dataReposs$wave, levels = c(7:12), 
                            labels = c("2015-2016", "2016-2017", "2017-2018",
                                       "2018-2019", "2019-2020", "2020-2021"), exclude = NA)
var_label(dataReposs$wave_f) <- "Year"
# check
table(dataReposs$wave, dataReposs$wave_f) 



# a. Demographic (5 variables) ----
### Age: dvage; Gender: sex; legal marital status: marstat  {waves 1:12}
### Current labour force status: jbstat {waves 1:12}; Highest qualification: qfhigh {waves 1:12}

## Age = age  ----
# Excel file: From UKHL_Variables_List, Uk_List = 1; Online: Variable's name: age
# check
table(dataReposs$age)
class(dataReposs$age)
summary(dataReposs$age)

# Convert into numeric
dataReposs$age <- as.numeric(dataReposs$age)

# Label variable
var_label(dataReposs$age) <- "Age"

# # Remove NA's
# dataReposs <- dataReposs[!is.na(dataReposs$age),]
# summary(dataReposs$age)

## Sex = gender (factor)  ----
# From UKHL_Variables_List (excel file): Uk_List = 2
# check
table(dataReposs$sex) 
class(dataReposs$sex)
summary(dataReposs$sex)
# Convert into factor
dataReposs$gender <- factor(dataReposs$sex, levels = c(1:2), labels = c("Male", "Female"), exclude = NA)
var_label(dataReposs$gender) <- "Gender"
# check
table(dataReposs$sex, dataReposs$gender) 
class(dataReposs$gender)
summary(dataReposs$gender)

# # Remove NA's
# dataReposs <- dataReposs[!is.na(dataReposs$gender),]
# summary(dataReposs$gender)
# 
# # Create variable for each gender
# dataReposs$male <- ifelse (dataReposs$gender == "Male", 1, 0)
# dataReposs$female <- ifelse (dataReposs$gender == "Female", 1, 0)
# # check
# table(dataReposs$gender)
# table(dataReposs$male)
# table(dataReposs$female)


## Marstat = marital_status and marstat_rec_f (factor) ----
# From UKHL_Variables_List (excel file): Uk_List = 5 
# check
table(dataReposs$marstat) 
class(dataReposs$marstat)

dataReposs$marital_status<- factor(dataReposs$marstat, levels = c(1:9), 
                                   labels = c("Single", "Married", "Civil", "Separated_Leg", "Divorced", 
                                              "Widowed", "Separated Civil", "Former Civil", "Surviving Civil"), exclude = NA)
table(dataReposs$marital_status) 
class(dataReposs$marital_status)

# Recode marstat
dataReposs$marstat_rec <- dataReposs$marstat
table(dataReposs$marstat_rec)
summary(dataReposs$marstat_rec)

dataReposs$marstat_rec[dataReposs$marstat_rec==1] <- 1
dataReposs$marstat_rec[dataReposs$marstat_rec==2] <- 2
dataReposs$marstat_rec[dataReposs$marstat_rec==3] <- 3
dataReposs$marstat_rec[dataReposs$marstat_rec==4] <- 4
dataReposs$marstat_rec[dataReposs$marstat_rec==5] <- 5
dataReposs$marstat_rec[dataReposs$marstat_rec==6] <- 6
dataReposs$marstat_rec[dataReposs$marstat_rec==7] <- 6
dataReposs$marstat_rec[dataReposs$marstat_rec==8] <- 6
dataReposs$marstat_rec[dataReposs$marstat_rec==9] <- 6

# Factor recode marsta
dataReposs$marstat_rec_f<- factor(dataReposs$marstat_rec, levels = c(1:6), 
                                  labels = c("Single", "Married", "Civil", "Separated_Leg", "Divorced", 
                                             "Widowed or SepFormerSurving Civil"), exclude = NA)
table(dataReposs$marstat_rec_f, dataReposs$marstat) 
class(dataReposs$marstat_rec_f)

# Label variable
var_label(dataReposs$marstat_rec_f) <- "Marital Status"


## Current labour force status: jbstat = jbstat_rec (recode factor)---- 
# From UKHL_Variables_List (excel file): Uk_List = 6 
table(dataReposs$jbstat) 
class(dataReposs$jbstat)

# Recode jbstat
dataReposs$jbstat_rec <- dataReposs$jbstat
table(dataReposs$jbstat_rec)
summary(dataReposs$jbstat_rec)

dataReposs$jbstat_rec[dataReposs$jbstat_rec==1] <- 1
dataReposs$jbstat_rec[dataReposs$jbstat_rec==2] <- 2
dataReposs$jbstat_rec[dataReposs$jbstat_rec==3] <- 3
dataReposs$jbstat_rec[dataReposs$jbstat_rec==4] <- 4
dataReposs$jbstat_rec[dataReposs$jbstat_rec==5] <- 5
dataReposs$jbstat_rec[dataReposs$jbstat_rec==6] <- 5
dataReposs$jbstat_rec[dataReposs$jbstat_rec==7] <- 5
dataReposs$jbstat_rec[dataReposs$jbstat_rec==8] <- 5
dataReposs$jbstat_rec[dataReposs$jbstat_rec==9] <- 5
dataReposs$jbstat_rec[dataReposs$jbstat_rec==10] <- 5
dataReposs$jbstat_rec[dataReposs$jbstat_rec==11] <- 5
dataReposs$jbstat_rec[dataReposs$jbstat_rec==12] <- 5
dataReposs$jbstat_rec[dataReposs$jbstat_rec==13] <- 5
dataReposs$jbstat_rec[dataReposs$jbstat_rec==97] <- 5

# Factor recode marsta
dataReposs$jbstat_rec<- factor(dataReposs$jbstat_rec, levels = c(1:5), 
                               labels = c("Self employed", "Paid employment(ft/pt)", "Unemployed", "Retired", "Other"), exclude = NA)
table(dataReposs$jbstat, dataReposs$jbstat_rec) 
class(dataReposs$jbstat_rec)

# Label variable
var_label(dataReposs$jbstat_rec) <- "Current labour force status"


## Highest qualification: qfhigh = qfhigh_rec (recode factor) ----
# From UKHL_Variables_List (excel file): Uk_List = 7 
table(dataReposs$qfhigh) 
class(dataReposs$qfhigh)

# Recode qfhigh
dataReposs$qfhigh_rec <- dataReposs$qfhigh
table(dataReposs$qfhigh_rec)
summary(dataReposs$qfhigh_rec)

dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==1] <- 1
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==2] <- 1
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==3] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==4] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==5] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==6] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==7] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==8] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==9] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==10] <-2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==11] <-2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==12] <-2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==13] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==14] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==15] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==16] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==17] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==18] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==19] <- 1
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==20] <- 1
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==21] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==22] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==23] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==24] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==25] <- 2
dataReposs$qfhigh_rec[dataReposs$qfhigh_rec==96] <- 2

# Factor recode marsta
dataReposs$qfhigh_rec<- factor(dataReposs$qfhigh_rec, levels = c(1:2), 
                               labels = c("University", "No University" ), exclude = NA)
table(dataReposs$qfhigh, dataReposs$qfhigh_rec) 
class(dataReposs$qfhigh_rec)

# Label variable
var_label(dataReposs$qfhigh_rec) <- "Highest qualification"


# b. Geography (2 variables)----

## Country = country_f (factor)---- 
# From UKHL_Variables_List (excel file): Uk_List = 14
# 1 = England; 2 = Wales 
table(dataReposs$country)
class(dataReposs$country)
summary(dataReposs$country)

# Convert into factor
dataReposs$country_f<- factor(dataReposs$country, levels = c(1:2), labels = c("England", "Wales"), exclude = NA)
# dataReposs <- within(dataReposs, rm(country.f))
# check
table(dataReposs$country, dataReposs$country_f)
summary(dataReposs$country_f)

# Label variable
var_label(dataReposs$country_f) <- "Country"

# # Create variable for each country
# dataReposs$england <- ifelse (dataReposs$country_f == "England", 1, 0)
# dataReposs$wales <- ifelse (dataReposs$country_f == "Wales", 1, 0)
# # check
# table(dataReposs$country_f)
# table(dataReposs$england)
# table(dataReposs$wales)


## Government office region: gor_dv = gor_dv_f  ---- 
# From UKHL_Variables_List (excel file): Uk_List = 70 
# 1=North East; 2=North West; 3= Yorkshire and the Humber
# 4=East Midlands; 5= West Midlands; 6= East of England; 7=London ; 8= South East; 9= South West ; 
# 10= Wales; 11=  Scotland; 12= Northern Ireland;
table(dataReposs$gor_dv)
class(dataReposs$gor_dv)
summary(dataReposs$gor_dv)

# Convert into factor
dataReposs$gor_dv_f<- factor(dataReposs$gor_dv, levels = c(1:10), 
                             labels = c("North East", "North West", "Yorkshire and the Humber",
                                        "East Midlands", "West Midlands", "East of England", "London", "South East", "South West", "Wales"), exclude = NA)
# dataReposs <- within(dataReposs, rm(country.f))
# check
table(dataReposs$gor_dv,dataReposs$gor_dv_f)
summary(dataReposs$gor_dv_f)

# Label variable
var_label(dataReposs$gor_dv_f) <- "Government office region"




# c. Income (2 variables) ----

## Monthly total net personal income - no deductions: fimnnet_dv  ----
# From UKHL_Variables_List (excel file): Uk_List = 26
table(dataReposs$fimnnet_dv)
class(dataReposs$fimnnet_dv)
summary(dataReposs$fimnnet_dv)


# Label variable
var_label(dataReposs$fimnnet_dv) <- "Monthly total net personal income (no deductions)"



## Monthly total household net income - no deductions: fihhmnnet1_dv ----
# From UKHL_Variables_List (excel file): Uk_List = 27
table(dataReposs$fihhmnnet1_dv)
class(dataReposs$fihhmnnet1_dv)
summary(dataReposs$fihhmnnet1_dv)

# Label variable
var_label(dataReposs$fihhmnnet1_dv) <- "Monthly total household net income (no deductions)"

# # Remove NA's
# dataReposs <- dataReposs[!is.na(dataReposs$fihhmnnet1_dv),]
# summary(dataReposs$fihhmnnet1_dv)


# d. General household (3 variables)----

## Number of people in household: hhsize ----
# From UKHL_Variables_List (excel file): Uk_List = 51
table(dataReposs$hhsize)
class(dataReposs$hhsize)
summary(dataReposs$hhsize)

# Convert into numeric
dataReposs$hhsize<- as.numeric(dataReposs$hhsize)

# Label variable
var_label(dataReposs$hhsize) <- "Number of people in household"

# # Remove NA's
# dataReposs <- dataReposs[!is.na(dataReposs$hhsize_n),]
# summary(dataReposs$hhsize_n)
# #dataReposs <- subset(dataReposs, select = -c(hhsize_n))


## Modified OECD equivalence scale: ieqmoecd_dv ----
# From UKHL_Variables_List (excel file): Uk_List = 84
table(dataReposs$ieqmoecd_dv)
class(dataReposs$ieqmoecd_dv)
summary(dataReposs$ieqmoecd_dv)

# Label variable
var_label(dataReposs$ieqmoecd_dv) <- "Modified OECD equivalence scale"

# # Remove NA's
# dataReposs$ieqmoecd_dv_n <- as.numeric(dataReposs$ieqmoecd_dv)
# dataReposs <- dataReposs[!is.na(dataReposs$ieqmoecd_dv_n),]
# summary(dataReposs$ieqmoecd_dv_n)
# class(dataReposs$ieqmoecd_dv_n)


## Number of children in household: nkids_dv ----
# From UKHL_Variables_List (excel file): Uk_List = 
###### check UKHL_ID
table(dataReposs$nkids_dv)
class(dataReposs$nkids_dv)
summary(dataReposs$nkids_dv)

# Convert into numeric
dataReposs$nkids_dv<- as.numeric(dataReposs$nkids_dv)

# Label variable
var_label(dataReposs$nkids_dv) <- "Number of children in household"

# e. Behavioural Science (3 variables) ----

## Life satisfaction: sclfsato, life_satisfaction, life_satisfaction_f ----- 
# From UKHL_Variables_List (excel file): Uk_List = 37
# How dissatisfied or satisfied are you with your life overall?
# 1:7 (completely dissatisfied: Completely satisfied) 
table(dataReposs$sclfsato)
class(dataReposs$sclfsato)
summary(dataReposs$sclfsato)

# Convert into numeric
dataReposs$life_satisfaction <- as.numeric(dataReposs$sclfsato)

# recode
dataReposs$life_satisfaction_f<- factor(dataReposs$life_satisfaction, levels = c(1:7), 
                                        labels = c("Completely dissatisfied", "Mostly dissatisfied", "Somewhat dissatisfied",
                                                   "Neither Sat nor Dissat", "Somewhat satisfied", "Mostly satisfied", "Completely satisfied"), exclude = NA)
table(dataReposs$sclfsato,dataReposs$life_satisfaction_f)
class(dataReposs$life_satisfaction_f)
summary(dataReposs$life_satisfaction_f)

# Label variable
var_label(dataReposs$life_satisfaction_f) <- "Life satisfaction"

# # Remove NA's
# dataReposs <- dataReposs[!is.na(dataReposs$life_satisfaction),]
# summary(dataReposs$life_satisfaction)


## Job satisfaction:  jbsat, job_satisfaction ----
# From UKHL_Variables_List (excel file): Uk_List = 38
# how dissatisfied or satisfied are you with your present job overall?
# 1:7 (completely dissatisfied: Completely satisfied) 
table(dataReposs$jbsat)
class(dataReposs$jbsat)
summary(dataReposs$jbsat)

# Convert into numeric
dataReposs$job_satisfaction <- as.numeric(dataReposs$jbsat)
class(dataReposs$job_satisfaction)

# recode
dataReposs$job_satisfaction_f<- factor(dataReposs$job_satisfaction, levels = c(1:7), 
                                       labels = c("Completely dissatisfied", "Mostly dissatisfied", "Somewhat dissatisfied",
                                                  "Neither Sat nor Dissat", "Somewhat satisfied", "Mostly satisfied", "Completely satisfied"), exclude = NA)
# Check
table(dataReposs$jbsat, dataReposs$job_satisfaction_f)
class(dataReposs$job_satisfaction_f)
summary(dataReposs$job_satisfaction_f)

# Label variable
var_label(dataReposs$job_satisfaction_f) <- "Job satisfaction"



## Framing bills: fuelduel, framing_bills, framing_bills_f ----
# From UKHL_Variables_List (excel file): Uk_List = 66
# Do you pay your gas and electric as one bill or separately?
table(dataReposs$fuelduel)
class(dataReposs$fuelduel)
summary(dataReposs$fuelduel)

# Convert into numeric
dataReposs$framing_bills <- as.numeric(dataReposs$fuelduel)
class(dataReposs$framing_bills)

# Convert into factor
dataReposs$framing_bills_f<- factor(dataReposs$framing_bills, levels = c(1:2), 
                                    labels = c("One bill", "Separately"), exclude = NA)
# check
table(dataReposs$fuelduel, dataReposs$framing_bills_f)

# Label variable
var_label(dataReposs$framing_bills_f) <- "Framing bills"




# f. Questions for Repossessions (9 variables) ----

## Housing tenure: tenure_dv, housing_tenure ----
# From UKHL_Variables_List (excel file): Uk_List = 42
table(dataReposs$tenure_dv)
class(dataReposs$tenure_dv)
summary(dataReposs$tenure_dv)

# recode tenure_dv
dataReposs$housing_tenure <- dataReposs$tenure_dv
table(dataReposs$housing_tenure)

dataReposs$housing_tenure[dataReposs$housing_tenure==1] <- 1
dataReposs$housing_tenure[dataReposs$housing_tenure==2] <- 2
dataReposs$housing_tenure[dataReposs$housing_tenure==3] <- 3
dataReposs$housing_tenure[dataReposs$housing_tenure==4] <- 4
dataReposs$housing_tenure[dataReposs$housing_tenure==5] <- 5
dataReposs$housing_tenure[dataReposs$housing_tenure==6] <- 5
dataReposs$housing_tenure[dataReposs$housing_tenure==7] <- 5
dataReposs$housing_tenure[dataReposs$housing_tenure==8] <- 6

# Convert into factor
dataReposs$housing_tenure<- factor(dataReposs$housing_tenure, levels = c(1:6),
                                   labels = c("Owned outright", "Owned with mortgage", "Local authority rent", "Housing assoc rented",
                                              "Rented", "Other"), exclude = NA)
table(dataReposs$tenure_dv, dataReposs$housing_tenure)
class(dataReposs$housing_tenure)
summary(dataReposs$housing_tenure)

# Label variable
var_label(dataReposs$housing_tenure) <- "Housing tenure"

# # # Create dummy variable and label it
# dataReposs$Owned_outright <- ifelse (dataReposs$housing_tenure == "Owned outright", 1, 0)
# var_label(dataReposs$Owned_outright) <- "Owned outright"
# dataReposs$Owned_mortgage <- ifelse (dataReposs$housing_tenure == "Owned with mortgage", 1, 0)
# var_label(dataReposs$Owned_mortgage) <- "Owned with mortgage"
# dataReposs$Local_rent <- ifelse (dataReposs$housing_tenure == "Local authority rent", 1, 0)
# var_label(dataReposs$Local_rent) <- "Local authority rent"
# dataReposs$Housing_assoc_rented <- ifelse (dataReposs$housing_tenure == "Housing assoc rented", 1, 0)
# var_label(dataReposs$Housing_assoc_rented) <- "Housing assoc rented"
# dataReposs$Rented <- ifelse (dataReposs$housing_tenure == "Rented", 1, 0)
# var_label(dataReposs$Rented) <- "Rented"
# dataReposs$Other <- ifelse (dataReposs$housing_tenure == "Other", 1, 0)
# var_label(dataReposs$Rented) <- "Other"
# table(dataReposs$housing_tenure)
# table(dataReposs$Owned_outright)
# table(dataReposs$Owned_mortgage) 
# table(dataReposs$Local_rent)
# table(dataReposs$Housing_assoc_rented) 
# table(dataReposs$Rented) 
# table(dataReposs$Other)


## Type of mortgage: mgtype, mgtype_rec ---- 
# From UKHL_Variables_List (excel file): Uk_List = 63
###### check UKHL_ID
# Is your mortgage or loan ...
table(dataReposs$mgtype)
class(dataReposs$mgtype)
summary(dataReposs$mgtype)
table(dataReposs$mgtype,dataReposs$wave)
# Recode mgtype
dataReposs$mgtype_rec <- dataReposs$mgtype
table(dataReposs$mgtype_rec)
summary(dataReposs$mgtype_rec)

dataReposs$mgtype_rec[dataReposs$mgtype_rec==1] <- 1
dataReposs$mgtype_rec[dataReposs$mgtype_rec==2] <- 2
dataReposs$mgtype_rec[dataReposs$mgtype_rec==3] <- 2
dataReposs$mgtype_rec[dataReposs$mgtype_rec==4] <- 2
dataReposs$mgtype_rec[dataReposs$mgtype_rec==5] <- 2
dataReposs$mgtype_rec[dataReposs$mgtype_rec==6] <- 2
dataReposs$mgtype_rec[dataReposs$mgtype_rec==97] <- 2

# Convert into factor
dataReposs$mgtype_rec<- factor(dataReposs$mgtype_rec, levels = c(1:2), 
                               labels = c("A repayment mortgage or loan", "Other"), exclude = NA)

table(dataReposs$mgtype,dataReposs$mgtype_rec)

# Label variable
var_label(dataReposs$mgtype_rec) <- "Type of mortgage"


## Monthly housing cost including mortgage principal payments: houscost1_dv ----
# From UKHL_Variables_List (excel file): Uk_List = 30
table(dataReposs$houscost1_dv)
class(dataReposs$houscost1_dv)
summary(dataReposs$houscost1_dv)

# Label variable
var_label(dataReposs$houscost1_dv) <- "Monthly Housing Cost (incl: mortgage)"



## Monthly housing cost excluding mortgage principal payments houscost2_dv ----
# From UKHL_Variables_List (excel file): Uk_List = 31
table(dataReposs$houscost2_dv)
class(dataReposs$houscost2_dv)
summary(dataReposs$houscost2_dv)

# Label variable
var_label(dataReposs$houscost2_dv) <- "Monthly Housing Cost (excl: mortgage)"


## Year mortgage began: hsyr04 ----
# From UKHL_Variables_List (excel file): Uk_List = 102
###### check UKHL_ID
table(dataReposs$hsyr04)
class(dataReposs$hsyr04)
summary(dataReposs$hsyr04)

# Convert into numeric
dataReposs$hsyr04 <- as.numeric(dataReposs$hsyr04)
class(dataReposs$hsyr04)

# Label variable
var_label(dataReposs$hsyr04) <- "Year mortgage began"


## Years left to pay mortgage: mglife ----
# From UKHL_Variables_List (excel file): Uk_List = 32
###### check UKHL_ID
table(dataReposs$mglife)
class(dataReposs$mglife)
summary(dataReposs$mglife)

# Label variable
var_label(dataReposs$mglife) <- "Years left to pay mortgage"


## Monthly mortgage payment including imputations: xpmg_dv ----
# From UKHL_Variables_List (excel file): Uk_List = 59
###### check UKHL_ID
table(dataReposs$xpmg_dv)
class(dataReposs$xpmg_dv)
summary(dataReposs$xpmg_dv)

# Label variable
var_label(dataReposs$xpmg_dv) <- "Monthly mortgage payment (incl. imputations)"


## Estimated interest in monthly mortgage payment: xpmgint_dv ---- 
# From UKHL_Variables_List (excel file): Uk_List = 60
table(dataReposs$xpmgint_dv)
class(dataReposs$xpmgint_dv)
summary(dataReposs$xpmgint_dv)

# Label variable
var_label(dataReposs$xpmgint_dv) <- "Estimated interest in monthly mortgage payment"


## last rent payment, montly: rent_dv ----
# From UKHL_Variables_List (excel file): Uk_List = 103
###### check UKHL_ID
table(dataReposs$rent_dv)
class(dataReposs$rent_dv)
summary(dataReposs$rent_dv)


# Label variable
var_label(dataReposs$rent_dv) <- "Last rent payment"

# g. Subjective questions for Repossessions (9 variables) ----

## Financial situation now: finnow, finnow_r, finnow_f ----
# From UKHL_Variables_List (excel file): Uk_List = 71
# How well would you say you yourself are managing financially these days? Would you say you are...
# 1:5 (living comfortable:finding it difficult)
table(dataReposs$finnow)
class(dataReposs$finnow)
summary(dataReposs$finnow)

# # Recode: positive direction
# dataReposs$finnow_r <- recode(dataReposs$finnow, "1"=5,"2"=4,"3"=3, "4"=2,"5"=1)
# table(dataReposs$finnow_r)
# table(dataReposs$finnow)
# class(dataReposs$finnow_r)

# # Remove NA's
# dataReposs <- dataReposs[!is.na(dataReposs$finfut_r),]
# summary(dataReposs$finfut_r)

# Convert into factor
dataReposs$finnow_f<- factor(dataReposs$finnow, levels = c(1:5), 
                             labels = c("Living comfortably", "Doing alright", "Just about getting by",
                                        "Finding it quite difficult", "Finding it very difficult"), exclude = NA)
# check
table(dataReposs$finnow, dataReposs$finnow_f)
class(dataReposs$finnow_f)
summary(dataReposs$finnow_f)

# Label variable
var_label(dataReposs$finnow_f) <- "Financial situation now"


## Financial situation future: finfut, finfut, finfut_r, finfut_f -----
# From UKHL_Variables_List (excel file): Uk_List = 10
# Looking ahead, how do you think you will be financially a year from now, will you be...
# 1:3 (better off, Worse, Same)
table(dataReposs$finfut)
class(dataReposs$finfut)
summary(dataReposs$finfut)

# Recode: positive direction
dataReposs$finfut_r <- recode(dataReposs$finfut, "1"=3,"2"=1,"3"=2)
table(dataReposs$finfut, dataReposs$finfut_r)
table(dataReposs$finfut)
class(dataReposs$finfut_r)

# # Remove NA's
# dataReposs <- dataReposs[!is.na(dataReposs$finfut_r),]
# summary(dataReposs$finfut_r)

# Convert into factor
dataReposs$finfut_f<- factor(dataReposs$finfut_r, levels = c(1:3), 
                             labels = c("Worse of than now", "About the same", "Better off"), exclude = NA)
# check
table(dataReposs$finfut, dataReposs$finfut_f)
class(dataReposs$finfut_f)
table(dataReposs$finfut)
# Label variable
var_label(dataReposs$finfut_f) <- "Financial situation future"

# # Create variable for each Subjective financial situation
# dataReposs$finfut_worse <- ifelse (dataReposs$finfut_f == "Worse of than now", 1, 0)
# dataReposs$finfut_same <- ifelse (dataReposs$finfut_f == "About the same", 1, 0)
# dataReposs$finfut_better <- ifelse (dataReposs$finfut_f == "Better off", 1, 0)
# # check
# table(dataReposs$finfut_f)
# table(dataReposs$finfut_worse)
# table(dataReposs$finfut_same)
# table(dataReposs$finfut_better)



## General health: sf1, general_health_f ----
# From UKHL_Variables_List (excel file): Uk_List = 11
# In general, would you say your health is...
#1:5 (exccelent:poor)
table(dataReposs$sf1)
class(dataReposs$sf1)
summary(dataReposs$sf1) 

# # Recode: positive direction
# dataReposs$general_health <- recode(dataReposs$sf1, "1"=5,"2"=4,"3"=3, "4"=2,"5"=1)
# table(dataReposs$sf1, dataReposs$general_health)
# Convert into factor
dataReposs$general_health_f<- factor(dataReposs$sf1, levels = c(1:5), 
                                     labels = c("Excellent", "Very good", "Good",
                                                "Fair", "Poor"), exclude = NA)
# check
table(dataReposs$sf1, dataReposs$general_health_f)

# Label variable
var_label(dataReposs$general_health_f) <- "General health"

## Satisfaction with health: sclfsat1, health_satisfaction ---- 
# From UKHL_Variables_List (excel file): Uk_List = 104
###### check UKHL_ID
# how dissatisfied or satisfied you are with the following aspects of your current situation: Your health
#1:7 (completely dissafisfied: completely satisfied)
table(dataReposs$sclfsat1)
class(dataReposs$sclfsat1)
summary(dataReposs$sclfsat1)

# Convert into factor
dataReposs$health_satisfaction<- factor(dataReposs$sclfsat1, levels = c(1:7), 
                                        labels = c("Completely dissatisfied", "Mostly dissatisfied", "Somewhat dissatisfied",
                                                   "Neither Sat nor Dissat", "Somewhat satisfied", "Mostly satisfied", "Completely satisfied"), exclude = NA)
# check
table(dataReposs$sclfsat1, dataReposs$health_satisfaction)
class(dataReposs$health_satisfaction)

# Label variable
var_label(dataReposs$health_satisfaction) <- "Satisfaction with health"


## Expects to move in next year: xpmove, expects_move ----
# From UKHL_Variables_List (excel file): Uk_List = 75
# Do you expect you will move in the coming year?
# 1:2 (yes, no)
table(dataReposs$xpmove) 
class(dataReposs$xpmove)
summary(dataReposs$xpmove)

# Convert into factor
dataReposs$expects_move <- factor(dataReposs$xpmove, levels = c(1:2), labels = c("Yes", "No"), exclude = NA)
# check
table(dataReposs$expects_move) 
class(dataReposs$expects_move)
table(dataReposs$expects_move,dataReposs$wave_f) 
# Label variable
var_label(dataReposs$expects_move) <- "Expects to move in next year"

## Problem with paying: xphsdb, bills_problem ---- 
# From UKHL_Variables_List (excel file): Uk_List = 67
# Many people find it hard to keep up with their housing payments. 
## In the last twelve months, have you ever found yourself behind with your rent/mortgage?
# 1:2 (yes, no)
table(dataReposs$xphsdb)
class(dataReposs$xphsdb)
summary(dataReposs$xphsdb)

# Convert into factor
dataReposs$bills_problem<- factor(dataReposs$xphsdb, levels = c(1:2), 
                                  labels = c("Yes", "No"), exclude = NA)
# check
table(dataReposs$xphsdb, dataReposs$bills_problem)
class(dataReposs$bills_problem)
table(dataReposs$bills_problem,dataReposs$wave_f)
# Label variable
var_label(dataReposs$bills_problem) <- "Problem with paying (rent/mortgage)"

# # Remove NA's
# dataReposs <- dataReposs[!is.na(dataReposs$xphsdb_f),]
# summary(dataReposs$xphsdb_f)

# # Create variable for each Up to date with all bills
# dataReposs$probpayh_yes <- ifelse (dataReposs$xphsdb_f == "Yes", 1, 0)
# dataReposs$probpayh_no <- ifelse (dataReposs$xphsdb_f == "No", 1, 0)
# 
# # check
# table(dataReposs$xphsdb_f)
# table(dataReposs$probpayh_yes)
# table(dataReposs$probpayh_no)


## Up to date bills: xphsdba, bills_uptodate ----
# From UKHL_Variables_List (excel file): Uk_List = 12
# Sometimes people are not able to pay every household bill when it falls due. 
# May we ask, are you up to date with all your household bills such as electricity, 
# gas, water rates, telephone and other bills or are you behind with any of them?
# 1:3 (up to date, hehind with some bills, behind with all bills)
table(dataReposs$xphsdba)
class(dataReposs$xphsdba)
summary(dataReposs$xphsdba)

# # Recode: positive direction

dataReposs$bills_uptodate <- recode(dataReposs$xphsdba, "1"=3,"2"=2,"3"=1)

# group level 1 and 2 into level 1
dataReposs$bills_uptodate <- ifelse(dataReposs$bills_uptodate %in% c("1", "2"), "1", "2")

# dataReposs$bills_uptodate <- recode(dataReposs$xphsdba, "1"=3,"2"=2,"3"=1)

# group level 1 and 2 into level 1
dataReposs$bills_uptodate <- ifelse(dataReposs$bills_uptodate %in% c("1", "2"), "1", "2")


# Convert into factor
dataReposs$bills_uptodate<- factor(dataReposs$bills_uptodate, levels = c(1:2), 
                                   labels = c("Behind with bills", "Up to date with all bills"), exclude = NA)
# Check
table(dataReposs$xphsdba,dataReposs$bills_uptodate)

# Label variable
var_label(dataReposs$bills_uptodate) <- "Up to date bills"

# # Create variable for each Up to date with all bills
# dataReposs$bills_uptodate <- ifelse (dataReposs$xphsdba_f == "Up to date with all bills", 1, 0)
# dataReposs$bills_behindsome <- ifelse (dataReposs$xphsdba_f == "Behind with some bills", 1, 0)
# dataReposs$bills_behindall <- ifelse (dataReposs$xphsdba_f == "Behind with all bills", 1, 0)
# # check
# table(dataReposs$xphsdba_f)
# table(dataReposs$bills_uptodate)
# table(dataReposs$bills_behindsome)
# table(dataReposs$bills_behindall)


## Satisfaction with income: sclfsat2, income_satisfaction ----
# From UKHL_Variables_List (excel file): Uk_List = 97
###### check UKHL_ID
# how dissatisfied or satisfied you are with the following aspects of your current situation: Your Income
#1:7 (completely dissafisfied: completely satisfied)
table(dataReposs$sclfsat2)
class(dataReposs$sclfsat2)
summary(dataReposs$sclfsat2)

# Convert into factor
dataReposs$income_satisfaction<- factor(dataReposs$sclfsat2, levels = c(1:7), 
                                        labels = c("Completely dissatisfied", "Mostly dissatisfied", "Somewhat dissatisfied",
                                                   "Neither Sat nor Dissat", "Somewhat satisfied", "Mostly satisfied", "Completely satisfied"), exclude = NA)
# check
table(dataReposs$sclfsat2, dataReposs$income_satisfaction)
class(dataReposs$income_satisfaction)

# Label variable
var_label(dataReposs$income_satisfaction) <- "Satisfaction with income"

## Satisfaction with house/flat: sclfsat3, houseflat_satisfaction {New - waves: 12} ----
# From UKHL_Variables_List (excel file): Uk_List = 105
###### check UKHL_ID
# how dissatisfied or satisfied you are with the following aspects of your current situation: Your house/flat
# 1:7 (completely dissatisfied: completely satisfied)
table(dataReposs$sclfsat3)
class(dataReposs$sclfsat3)
summary(dataReposs$sclfsat3)
table(dataReposs$sclfsat3, dataReposs$wave)

# Convert into factor
dataReposs$houseflat_satisfaction<- factor(dataReposs$sclfsat3, levels = c(1:7), 
                                           labels = c("Completely dissatisfied", "Mostly dissatisfied", "Somewhat dissatisfied",
                                                      "Neither Sat nor Dissat", "Somewhat satisfied", "Mostly satisfied", "Completely satisfied"), exclude = NA)
# check
table(dataReposs$sclfsat3, dataReposs$houseflat_satisfaction)
class(dataReposs$houseflat_satisfaction)

# Label variable
var_label(dataReposs$houseflat_satisfaction) <- "Satisfaction with house/flat"

# Save data ----
save(list=c("dataReposs"), file = "dataReposs_prep_na.Rdata")
# write data frame into csv format
#write.csv(dataReposs, "~/..../data...._prep_na.Rdata.csv", row.names=FALSE)

