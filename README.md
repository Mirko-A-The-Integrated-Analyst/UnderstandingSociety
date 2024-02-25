# Understanding Society

This readme file provides a guideline for importing and merging various data files, extracting variables, and creating a longitudinal data frame using an extensive UK Household Longitudinal survey called UKHLS (i.e., Understanding Society Survey).

# Project Showcase Notice
#This repository contains code and materials for the purpose of showcasing the project. Please note that sensitive and official parts of the project have been either partially or totally removed.
#Purpose
#The content provided here is intended to demonstrate the capabilities, design patterns, and methodologies employed in the project. It is not meant for production use and may lack certain functionalities that are part of the full, official version.
#Limitations
#- **Sensitive Information**: Any sensitive information, including but not limited to private keys, credentials, and personal data, has been removed or anonymized.
#- **Partial Functionality**: Some sections of the code may have been modified or omitted to ensure the security and privacy of the underlying system or data. As such, the repository may not represent the full functionality of the original project.
#- **Showcase Only**: The provided code and documents are intended for viewing and demonstration purposes only.

```{r setup, eval = FALSE, message = FALSE, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

## Contents

#### **Getting started** 

##### [a. InternalUseOnly Platform and AWS s3 access](#aps3) 

##### [b. Accessing the model from Github](#github) 

 


#### **User Instructions**

##### [c. Load the library](#library) 

##### [d. Import datafiles](#import) 

##### [e. Merge datafiles](#merge) 

##### [f. Variable extraction](#var_extr)

##### [g. Final longitudinal dataframe](#final_data)


   

## Getting started

<a name="aps3"></a>

### a. InternalUseOnly Platform and AWS s3 access

You will initially need to have access to the InternalUseOnly Platform. If you do not, guidance on how to set this up is found here:

<https:/InternalUseOnly/#content>

You will also need access to the 'InternalUseOnly' s3 bucket, and this can be setup by the admin for that bucket.

   

<a name="github"></a>  

### b. Accessing the model from Github

Firstly (if you haven't done so already) you should link your AP account to GitHub following these instructions: <https://InternalUseOnlyset-up-github.html>

You should then follow the instructions in step 1 at the link below to clone the `InternalUseOnly` GitHub repository to the local area of your platform account. You can read/write the 'InternalUseOnly' scripts from R-studio on your platform account.<https:/InternalUseOnly/rstudio-git.html>

Step 2 and beyond will be used should you want to amend code in the model scripts and push them back onto GitHub.

   



## User Instructions
<a name="library"></a> 

**c. Load the library**

Load the library and check all packages are loading correctly for you.

```{r, eval = FALSE, message = FALSE}
install.packages(c('rmarkdown', 'tidyverse','dplyr','plyr'))

library(botor) # To Access InternalUseOnly Amazon S3 data storage
library(rmarkdown) # To create dynamic documents that combine text, code, and visualizations 
library(tidyverse) # Data Manipulation
library(plyr) # Data Manipulation
library(dplyr) # Data Manipulation
```

### d. Import datafiles

<a name="import"></a> 



**1. Import datafiles**

Understanding Society Survey (UK Household Longitudinal Study - UKHLS) consists of various datasets. As an initial phase of the project, we use the main dataset, which can be downloaded on the following [link](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6614). The dataset has various datafile. We are interested in **indresp** and **hhresp** data files, which provide individual and household data. For the time being, we focus on three waves from 2017 to 2020: **wave 12 - l**, **Wave 11 - k** .

Data files are stored in an Amazon S3 bucket called **InternalUseOnly**. To upload the related data file to your work environment, you need to set input file name first as following:

**indresp** set input file name:
```{r, eval = FALSE}
l_indresp_path  <- "s3://InternalUseOnlyl_indresp.tab"
k_indresp_path  <- "s3://InternalUseOnly"
```
**hhresp** set input file name:
```{r, eval = FALSE}
l_hhresp_path  <- "s3://InternalUseOnly/l_hhresp.tab"
k_hhresp_path  <- "s3://InternalUseOnly/k_hhresp.tab"
```
To import data files automatically, please refer to the InternalUseOnly.R script

# Check that the file exists
```{r, eval = FALSE, message = FALSE}
# indresp
s3_exists(l_indresp_path)
s3_exists(k_indresp_path)
# hhresp
s3_exists(l_hhresp_path)
s3_exists(k_hhresp_path)
```

# 2. Load the data files

*Create Individuals dataframe for each wave- Indresp*
```{r, eval = FALSE, message = FALSE}
l_indresp <- s3_read(l_indresp_path, read.table, header=TRUE, na.string = c("NA", "-10", "-9", "-8", "-7", "-2", "-1"))
k_indresp <- s3_read(k_indresp_path, read.table, header=TRUE, na.string = c("NA", "-10", "-9", "-8", "-7", "-2", "-1"))
```

*Create Households dataframe for each wave - Hhresp*
```{r, eval = FALSE, message = FALSE}
l_hhresp <- s3_read(l_hhresp_path, read.table, header=TRUE, na.string = c("NA", "-10", "-9", "-8", "-7", "-2", "-1"))
k_hhresp <- s3_read(k_hhresp_path, read.table, header=TRUE, na.string = c("NA", "-10", "-9", "-8", "-7", "-2", "-1"))
```

<a name="merge"></a> 

### e. Merge datafiles

**1. Merge Individual and Houselds dataframes within each wave** 
We use household identifier (*hidp*) to identify members of the same households withing one wave

```{r, eval = FALSE,  message = FALSE}
l_wave<- join(l_indresp,l_hhresp, by=c("l_hidp"))
k_wave <- join(k_indresp, k_hhresp, by=c("k_hidp"))
```

Save the new dataset for each wave as R object
```{r, eval = FALSE, message = FALSE}
save(list=c("l_wave"), file="l_wave.RData")
save(list=c("k_wave"), file="k_wave.RData")
```

Check the data
```{r, eval = FALSE, message = FALSE}
dim(l_indresp)
dim(l_hhresp)
dim(l_wave)

head(l_indresp[,"l_gor_dv"])
head(l_hhresp[,"l_gor_dv"])
head(l_wave[,"l_gor_dv"])

table(l_indresp[,"l_country"])
table(l_hhresp[,"l_country"])
table(l_wave[, "l_country"])

table(k_indresp[,"k_gor_dv"])
table(k_hhresp[,"k_gor_dv"])
table(k_wave[, "k_gor_dv"])

sum(is.na(l_wave$l_gor_dv))
sum(is.na(l_indresp$l_gor_dv))

summary(na.omit(l_hhresp[,"l_fihhmnnet1_dv"]))
summary(na.omit(l_wave[, "l_fihhmnnet1_dv"])) 

dim(k_indresp)
dim(k_hhresp)
dim(k_wave)
```

<a name="var_extr"></a> 

### f. Variable Extraction

**1. Rename selected variables across waves**

The person identifier pidp is the only variable that does not include a wave prefix

```{r, eval = FALSE, message = FALSE}
l_data <- rename(l_wave, c(l_dvage = "age", l_sex = "sex", # Demographic
                              l_country = "country", l_gor_dv = "regional",  # Demographic - Geo
                             l_finfut = "finfut", l_fihhmnnet1_dv = "houseNetincome")) # Income

k_data <- rename(k_wave, c(k_dvage = "age", k_sex = "sex", # Demographic
                              k_country = "country", k_gor_dv = "regional",  # Demographic - Geo
                             k_finfut = "finfut", k_fihhmnnet1_dv = "houseNetincome")) # Income

```

**2. Selected variable from different waves and create a column named wave**

We use a 'long format' approach, , where each row represents a particular year’s data for a particular respondent. 

This happens to be the most suitable arrangement for more advanced statistical models of various kinds, and is also a more efficient means of storing the data, particularly when there are complex patterns of non-trivial attrition. By using functions that harvest data in the previous data row (or the one before the previous, or the next) it is straightforward to measure the extent of transitions of various kinds on an annual basis. More info: [link](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwj3k8Si27L7AhWSOcAKHRoSCZc4ChAWegQIBhAB&url=http%3A%2F%2Fdoc.ukdataservice.ac.uk%2Fdoc%2F8715%2Fmrdoc%2Fpdf%2F8715_user_guide.pdf&usg=AOvVaw26UHZTqfUutI1CoWsWrUsY)

Waves from 2019 to 2021: **wave 12 - l**, **Wave 11 - k**
```{r, eval = FALSE, message = FALSE }
# Wave 12 - l
l_vars<- c("pidp", "age", "sex", "country", "regional", "finfut", "houseNetincome" )
l_datafinal <- l_data[,l_vars]
l_datafinal$wave <- 12
# Wave 11 - k
k_vars<- c("pidp", "age", "sex", "country", "regional", "finfut", "houseNetincome" )
k_datafinal <- k_data[,k_vars]
k_datafinal$wave <- 11 
```

<a name="final_data"></a> 

### g. Final longitudinal dataframe

**1. Final longitudinal dataframe**

Combine all the final data waves into a single dataframe
```{r, eval = FALSE, message = FALSE}
dataFinal_all <- rbind.fill(l_datafinal, k_datafinal) # check total observations

# Reduce data frame to England and Wales: 1 = England; 2 = Wales
dataFinal <- subset(dataFinal_all, country == 1 | country == 2)

#Check the data
dim(dataFinal)

# Check if the data contains all the waves
table(dataFinal$wave)
table(dataFinal$country)

# Save dataframe
#save(list=c("dataFinal"), file = "dataFinal.Rdata")
```




<a name="rpack"></a>
&nbsp;



