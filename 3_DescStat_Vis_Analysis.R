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


# Load the library----
install.packages(c('tidyr', 'dplyr', 'gtsummary', 'ggplot2', 'ggthemes', 'nnet', 'openxlsx'))


library(tidyr) # Data Manipulation
library(dplyr) # Data Manipulation
library(gtsummary) # to create tables
library(ggplot2) # Data Visualization
library(ggthemes) # Additional themes and scales for the ggplot2
library(nnet) # Implementing neural networks such as classification and regression tasks
library(openxlsx) # To read, write and manipulate Excel files



# 3a. Descriptive statistics ----
## Select prepared variables ----
drivers_reposs <- dataReposs %>% 
  select(wave_f, 
         country_f, gor_dv_f, # Geography
         housing_tenure, mgtype_rec, houscost1_dv, houscost2_dv, # Question for Repossessions
         xpmg_dv, xpmgint_dv, rent_dv,
         finnow_f, finfut_f, general_health_f, health_satisfaction, # Subjective questions for Repossessions
         expects_move, bills_problem, bills_uptodate, income_satisfaction, houseflat_satisfaction,
         life_satisfaction_f, job_satisfaction_f, framing_bills_f, # Behavioural Science
         age, gender, marstat_rec_f, jbstat_rec, qfhigh_rec, # Demographic
         fimnnet_dv,fihhmnnet1_dv, # Income
         hhsize) # General household

var_label(drivers_reposs)
str(drivers_reposs)

## Descriptive statistics by housing tenure ----
ds_overview_house_tenure <-drivers_reposs %>% 
  # To include these missing values in the output table
  mutate(housing_tenure = forcats::fct_explicit_na(housing_tenure)) %>%
  tbl_summary(
    by = housing_tenure,
    type = c(age, houscost1_dv, houscost2_dv, xpmg_dv, xpmgint_dv, rent_dv, fimnnet_dv, fihhmnnet1_dv, hhsize)
    ~ "continuous2", 
    statistic = 
      list( c(age, houscost1_dv, houscost2_dv, xpmg_dv, xpmgint_dv, rent_dv, fimnnet_dv, fihhmnnet1_dv, hhsize) 
            ~ c("{mean}, {median}, ({sd})", "{min}, {max}")),
    missing_text = "Missing values"
  ) %>% 
  modify_header(label ~ "**Potential Drivers in Civil Court Demand**") %>% 
  bold_labels() %>% 

  modify_caption("**Descriptive statistics: Repossessions in Understanding Society from 2015 to 2021 grouped by housing tenure**")


ds_overview_house_tenure


# 3b. Visualisation ----

## House Tenure and Subjective questions ----

### Financial situation now plot across years ----

## Create a data frame with non-missing values of wave_f, finnow_f_fix,and house_tenure
df_tenure_finnow_fix <- na.omit(data.frame(wave_f = dataReposs$wave_f,
                                           housing_tenure = dataReposs$housing_tenure,
                                           finnow_f = dataReposs$finnow_f))
str(df_tenure_finnow_fix)

##### y= count; fill = Finnow 
tenure_finnow_plot_fill <- ggplot(df_tenure_finnow_fix, aes(x = wave_f, fill =  finnow_f)) +
  geom_bar(position = "dodge") +
  labs(title = "Housing Tenure Trend by Financial Situation Now",
       subtitle = "How well would you say you yourself are managing financially these days? Would you say you are...",
       x = "Year", y = "Observations", fill = "Financial situation now") +
  facet_wrap(~ housing_tenure, ncol = 3, scales = "free_y", strip.position = "top") +
  scale_y_continuous(expand = c(0, 0)) + # optional: remove space between bars and y-axis
  scale_fill_manual(values = govanal_colours) +
  theme_bw() +
  theme(legend.position = "right",
        title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic"),
        #axis.line = element_line(color = "black"),
        axis.title = element_text(size = 10, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"))

#To make the x-axis label more visible in the plot
tenure_finnow_plot_fill <- tenure_finnow_plot_fill +
  theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1, face = "bold"))

tenure_finnow_plot_fill
# save the graph as a PNG in your working directory.
ggsave("Housing tenure trend & Financial situation now (fill = finnow).png", plot = tenure_finnow_plot_fill, dpi = 300, width = 10, height = 6, units = "in")

# # Legend position: Top
# tenure_finnow_plot_fill_legend <- tenure_finnow_plot_fill + 
#   theme_bw() +
#   theme(legend.position = "top")
# 
# tenure_finnow_plot_fill_legend <- tenure_finnow_plot_fill_legend +
#   theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1, face = "bold"))
# 
# tenure_finnow_plot_fill_legend
# ggsave("Housing tenure trend & Financial situation now (fill = finnow, legend(top)).png", plot = tenure_finnow_plot_fill_legend, dpi = 300, width = 10, height = 6, units = "in")

# 
# ##### y= count; fill = wave 
# tenure_finnow_plot_wave <- ggplot(df_tenure_finnow_fix, aes(x = finnow_f, fill = wave_f )) +
#   geom_bar(position = "dodge") +
#   labs(title = "Housing Tenure Trend by Financial Situation Now",
#        subtitle = "How well would you say you yourself are managing financially these days? Would you say you are...",
#        x = "Financial situation now", y = "Observations", fill = "Year") +
#   facet_wrap(~ housing_tenure, ncol = 3, scales = "free_y", strip.position = "top") +
#   scale_y_continuous(expand = c(0, 0)) + # optional: remove space between bars and y-axis
#   scale_fill_manual(values = govanal_colours) +
#   theme_bw() +
#   theme(legend.position = "right",
#         title = element_text(size = 12, face = "bold"),
#         plot.subtitle = element_text(size = 10, face = "italic"),
#         #axis.line = element_line(color = "black"),
#         axis.title = element_text(size = 10, face = "bold"),
#         strip.text = element_text(size = 10, face = "bold"))
# 
# #To make the x-axis label more visible in the plot
# tenure_finnow_plot_wave <- tenure_finnow_plot_wave +
#   theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1, face = "bold"))
# 
# tenure_finnow_plot_wave
# # save the graph as a PNG in your working directory.
# ggsave("Housing tenure trend & Financial situation now (fill = wave).png", plot = tenure_finnow_plot_fill, dpi = 300, width = 10, height = 6, units = "in")
# 

### Financial situation future plot across years ----

## Create a data frame with non-missing values of wave_f, finfut_f,and house_tenure
df_tenure_finfut_fix <- na.omit(data.frame(wave_f = dataReposs$wave_f,
                                           housing_tenure = dataReposs$housing_tenure,
                                           finfut_f = dataReposs$finfut_f))
str(df_tenure_finfut_fix)

##### Plot
tenure_finfut_plot_fill <- ggplot(df_tenure_finfut_fix, aes(x = wave_f, fill =  finfut_f)) +
  geom_bar(position = "dodge") +
  labs(title = "Housing Tenure Trend by Financial Situation Future",
       subtitle = "Looking ahead, how do you think you will be financially a year from now, will you be...",
       x = "Year", y = "Observations", fill = "Financial situation future") +
  facet_wrap(~ housing_tenure, ncol = 3, scales = "free_y", strip.position = "top") +
  scale_y_continuous(expand = c(0, 0)) + # optional: remove space between bars and y-axis
  scale_fill_manual(values = govanal_colours) +
  theme_bw() +
  theme(legend.position = "right",
        title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic"),
        #axis.line = element_line(color = "black"),
        axis.title = element_text(size = 10, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"))

#To make the x-axis label more visible in the plot
tenure_finfut_plot_fill <- tenure_finfut_plot_fill +
  theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1, face = "bold"))

tenure_finfut_plot_fill
# save the graph as a PNG in your working directory.
ggsave("Housing tenure trend & Financial situation future (fill = finfut).png", plot = tenure_finfut_plot_fill, dpi = 300, width = 10, height = 6, units = "in")


### Satisfaction with health plot across years ----

## Create a data frame with non-missing values of wave_f, health_satisfaction,and house_tenure
df_tenure_sathealth_fix <- na.omit(data.frame(wave_f = dataReposs$wave_f,
                                              housing_tenure = dataReposs$housing_tenure,
                                              health_satisfaction = dataReposs$health_satisfaction))
str(df_tenure_sathealth_fix)

##### Plot
tenure_sathealth_plot_fill <- ggplot(df_tenure_sathealth_fix, aes(x = wave_f, fill =  health_satisfaction)) +
  geom_bar(position = "dodge") +
  labs(title = "Housing Tenure Trend by Satisfaction with health",
       subtitle = "How dissatisfied or satisfied you are with the following aspects of your current situation: Your health...",
       x = "Year", y = "Observations", fill = "Satisfaction with health") +
  facet_wrap(~ housing_tenure, ncol = 3, scales = "free_y", strip.position = "top") +
  scale_y_continuous(expand = c(0, 0)) + # optional: remove space between bars and y-axis
  scale_fill_manual(values = govanal_colours) +
  theme_bw() +
  theme(legend.position = "right",
        title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic"),
        #axis.line = element_line(color = "black"),
        axis.title = element_text(size = 10, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"))

#To make the x-axis label more visible in the plot
tenure_sathealth_plot_fill <- tenure_sathealth_plot_fill +
  theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1, face = "bold"))

tenure_sathealth_plot_fill
# save the graph as a PNG in your working directory.
ggsave("Housing tenure trend & Satisfaction with health (fill = health_satisfaction).png", plot = tenure_sathealth_plot_fill, dpi = 300, width = 10, height = 6, units = "in")

### Problem with paying plot across years ----

## Create a data frame with non-missing values of wave_f, bills_problem,and house_tenure
df_tenure_bills_problem_fix <- na.omit(data.frame(wave_f = dataReposs$wave_f,
                                                  housing_tenure = dataReposs$housing_tenure,
                                                  bills_problem = dataReposs$bills_problem))
str(df_tenure_sathealth_fix)

##### Plot
tenure_bills_problem_plot_fill <- ggplot(df_tenure_bills_problem_fix, aes(x = wave_f, fill =  bills_problem)) +
  geom_bar(position = "dodge") +
  labs(title = "Housing Tenure Trend by Problem with paying",
       subtitle = "In the last twelve months, have you ever found yourself behind with your rent/mortgage?",
       x = "Year", y = "Observations", fill = "Problem with paying") +
  facet_wrap(~ housing_tenure, ncol = 3, scales = "free_y", strip.position = "top") +
  scale_y_continuous(expand = c(0, 0)) + # optional: remove space between bars and y-axis
  scale_fill_manual(values = govanal_colours) +
  theme_bw() +
  theme(legend.position = "right",
        title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic"),
        #axis.line = element_line(color = "black"),
        axis.title = element_text(size = 10, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"))

#To make the x-axis label more visible in the plot
tenure_bills_problem_plot_fill <- tenure_bills_problem_plot_fill +
  theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1, face = "bold"))

tenure_bills_problem_plot_fill
# save the graph as a PNG in your working directory.
ggsave("Housing tenure trend & Problem with paying (fill = bills_problem).png", plot = tenure_bills_problem_plot_fill, dpi = 300, width = 10, height = 6, units = "in")


###  Up to date bills plot across years ----

## Create a data frame with non-missing values of wave_f, bills_uptodate,and house_tenure
df_tenure_bills_uptodate_fix <- na.omit(data.frame(wave_f = dataReposs$wave_f,
                                                   housing_tenure = dataReposs$housing_tenure,
                                                   bills_uptodate = dataReposs$bills_uptodate))
str(df_tenure_bills_uptodate_fix)

##### Plot
tenure_bills_uptodate_fix_plot_fill <- ggplot(df_tenure_bills_uptodate_fix, aes(x = wave_f, fill =  bills_uptodate)) +
  geom_bar(position = "dodge") +
  labs(title = "Housing Tenure Trend by Up to date bills",
       subtitle = "May we ask, are you up to date with all your household bills such as electricity, gas, 
                    water rates, telephone and other bills or are you behind with any of them?",
       x = "Year", y = "Observations", fill = "Up to date bills") +
  facet_wrap(~ housing_tenure, ncol = 3, scales = "free_y", strip.position = "top") +
  scale_y_continuous(expand = c(0, 0)) + # optional: remove space between bars and y-axis
  scale_fill_manual(values = govanal_colours) +
  theme_bw() +
  theme(legend.position = "right",
        title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic"),
        #axis.line = element_line(color = "black"),
        axis.title = element_text(size = 10, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"))

#To make the x-axis label more visible in the plot
tenure_bills_uptodate_fix_plot_fill <- tenure_bills_uptodate_fix_plot_fill +
  theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1, face = "bold"))

tenure_bills_uptodate_fix_plot_fill
# save the graph as a PNG in your working directory.
ggsave("Housing tenure trend & Up to date bills (fill = bills_uptodate).png", plot = tenure_bills_uptodate_fix_plot_fill, dpi = 300, width = 10, height = 6, units = "in")

## House Tenure and Behavioural Science ----

#### Life satisfaction plot across years ----
## Create a data frame with non-missing values of wave_f, life_satisfaction_f,and house_tenure
df_tenure_life_satisfaction_fix <- na.omit(data.frame(wave_f = dataReposs$wave_f,
                                                      housing_tenure = dataReposs$housing_tenure,
                                                      life_satisfaction = dataReposs$life_satisfaction_f))
str(df_tenure_life_satisfaction_fix)

##### Plot
tenure_life_satisfaction_plot_fill <- ggplot(df_tenure_life_satisfaction_fix, aes(x = wave_f, fill =  life_satisfaction)) +
  geom_bar(position = "dodge") +
  labs(title = "Housing Tenure Trend by Life satisfaction",
       subtitle = "How dissatisfied or satisfied are you with your life overall?",
       x = "Year", y = "Observations", fill = "Life satisfaction") +
  facet_wrap(~ housing_tenure, ncol = 3, scales = "free_y", strip.position = "top") +
  scale_y_continuous(expand = c(0, 0)) + # optional: remove space between bars and y-axis
  scale_fill_manual(values = govanal_colours) +
  theme_bw() +
  theme(legend.position = "right",
        title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic"),
        #axis.line = element_line(color = "black"),
        axis.title = element_text(size = 10, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"))

#To make the x-axis label more visible in the plot
tenure_life_satisfaction_plot_fill <- tenure_life_satisfaction_plot_fill +
  theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1, face = "bold"))

tenure_life_satisfaction_plot_fill
# save the graph as a PNG in your working directory.
ggsave("Housing tenure trend & Life satisfaction (fill = life_satisfaction).png", plot = tenure_life_satisfaction_plot_fill, dpi = 300, width = 10, height = 6, units = "in")



### Job satisfaction plot across years-----
## Create a data frame with non-missing values of wave_f, finfut_f,and house_tenure
df_tenure_job_satisfactionn_fix <- na.omit(data.frame(wave_f = dataReposs$wave_f,
                                                      housing_tenure = dataReposs$housing_tenure,
                                                      job_satisfaction = dataReposs$job_satisfaction_f))
str(df_tenure_job_satisfactionn_fix)

##### Plot
tenure_job_satisfaction_plot_fill <- ggplot(df_tenure_job_satisfactionn_fix, aes(x = wave_f, fill =  job_satisfaction)) +
  geom_bar(position = "dodge") +
  labs(title = "Housing Tenure Trend by Job satisfaction",
       subtitle = "How dissatisfied or satisfied are you with your present job overall?",
       x = "Year", y = "Observations", fill = "Job satisfaction") +
  facet_wrap(~ housing_tenure, ncol = 3, scales = "free_y", strip.position = "top") +
  scale_y_continuous(expand = c(0, 0)) + # optional: remove space between bars and y-axis
  scale_fill_manual(values = govanal_colours) +
  theme_bw() +
  theme(legend.position = "right",
        title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic"),
        #axis.line = element_line(color = "black"),
        axis.title = element_text(size = 10, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"))

#To make the x-axis label more visible in the plot
tenure_job_satisfaction_plot_fill <- tenure_job_satisfaction_plot_fill +
  theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1, face = "bold"))

tenure_job_satisfaction_plot_fill
# save the graph as a PNG in your working directory.
ggsave("Housing tenure trend & Job satisfaction (fill = job_satisfaction).png", plot = tenure_job_satisfaction_plot_fill, dpi = 300, width = 10, height = 6, units = "in")


### Framing bills plot across years-----
## Create a data frame with non-missing values of wave_f, framing_bills_f,and house_tenure
df_tenure_framing_bills_fix <- na.omit(data.frame(wave_f = dataReposs$wave_f,
                                                  housing_tenure = dataReposs$housing_tenure,
                                                  framing_bills = dataReposs$framing_bills_f))
str(df_tenure_framing_bills_fix)

##### Plot
tenure_framing_bills_plot_fill <- ggplot(df_tenure_framing_bills_fix, aes(x = wave_f, fill =  framing_bills)) +
  geom_bar(position = "dodge") +
  labs(title = "Housing Tenure Trend by Framing bills",
       subtitle = "Do you pay your gas and electric as one bill or separately?",
       x = "Year", y = "Observations", fill = "Framing bills") +
  facet_wrap(~ housing_tenure, ncol = 3, scales = "free_y", strip.position = "top") +
  scale_y_continuous(expand = c(0, 0)) + # optional: remove space between bars and y-axis
  scale_fill_manual(values = govanal_colours) +
  theme_bw() +
  theme(legend.position = "right",
        title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic"),
        #axis.line = element_line(color = "black"),
        axis.title = element_text(size = 10, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"))

#To make the x-axis label more visible in the plot
tenure_framing_bills_plot_fill <- tenure_framing_bills_plot_fill +
  theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1, face = "bold"))

tenure_framing_bills_plot_fill
# save the graph as a PNG in your working directory.
ggsave("Housing tenure trend & Framing bills (fill = framing_bills).png", plot = tenure_framing_bills_plot_fill, dpi = 300, width = 10, height = 6, units = "in")



# 3c. Analysis ----

# ## Create a contingency table with row and column percentages 
xtabs(~ housing_tenure + finfut_f, data = dataReposs) %>%
  addmargins() %>%
  prop.table(margin = 1) %>%
  round(digits = 2)
# 
# ## cross table using gtsummary (better)
dataReposs %>%
  tbl_cross(
  row = finfut_f,
  col = housing_tenure,
  percent = "row",
  missing_text = "Missing values")
# 
# 
# ### Association Analysis 
# ### Correalation 
# # select the variables of interest
vars <- c("age", "fimnnet_dv", "fihhmnnet1_dv", "hhsize", "nkids_dv", "mglife",
           "houscost1_dv", "houscost2_dv", "xpmgint_dv", "rent_dv")
# # compute the correlation matrix
cor_table <- cor(dataReposs[, vars], use = "pairwise.complete.obs")
# 


## Multinomial  logistic regression model to predict housing tenure ----
# The goal is to predict the probability of each category of the housing tenure given the values of the independent variables.
# This function uses a maximum likelihood estimation method to find the coefficients that maximize the likelihood of the observed data. 

# Check reference category
levels(dataReposs$housing_tenure) # "Owned outright" is the reference category.
#levels(dataReposs$finnow_f) # "Living comfortably" is the reference category.
levels(dataReposs$job_satisfaction_f) # "Completely dissatisfied" is the reference category.
levels(dataReposs$framing_bills_f) # "One bill" is the reference category.
# levels(dataReposs$gender) # "Male" is the reference category.
#levels(dataReposs$marstat_rec_f) # "Single" is the reference category.
#levels(dataReposs$jbstat_rec) # "Self employed" is the reference category.

####  Job satisfaction ----
# "Completely dissatisfied" is the reference category.
m_job_sat<- multinom(housing_tenure ~ job_satisfaction_f, data = dataReposs, na.action=na.omit)
# view raw model results
#summary(m_job_sat)
m_job_sat_coef <- summary(m_job_sat)$coefficients
m_job_sat_se <- summary(m_job_sat)$standard.errors
# To convert the coefficients and standard errors from log-odds units to odds ratios or probabilities, you can use the exp()
m_job_sat_odds_ratio <- exp(m_job_sat_coef) # Odds ratios
m_job_sat_percentage <- (exp(m_job_sat_coef)-1)*100 


# Write output Excel
# create a new workbook
wb <- createWorkbook()
# create a new sheet for the first object
addWorksheet(wb, "MNLR_Model")
# Write the data to the sheets
writeData(wb, sheet = "MNLR_Model", "Odds Ratio", startRow = 1, startCol = 1)
writeData(wb, sheet = "MNLR_Model", "Owned outright is the reference category", startRow = 2, startCol = 1)
writeData(wb, sheet = "MNLR_Model", "Completely dissatisfied is the reference category", startRow = 3, startCol = 1)
writeData(wb, sheet = "MNLR_Model", m_job_sat_odds_ratio, startRow = 5, startCol = 1,  rowNames = TRUE )
writeData(wb, sheet = "MNLR_Model", "Interpretation (odds) Percentage", startRow = 12, startCol = 1)
writeData(wb, sheet = "MNLR_Model", m_job_sat_percentage, startRow = 14, startCol = 1, rowNames = TRUE )
# save the workbook
saveWorkbook(wb, "MNLR Model_Housing Tenure & Job satisfaction.xlsx")

####  Framing bills ----
# "One bill" is the reference category
m_fram_bills<- multinom(housing_tenure ~ framing_bills_f, data = dataReposs, na.action=na.omit)
# view raw model results
#summary(m_fram_bills)
m_fram_bills_coef <- summary(m_fram_bills)$coefficients
m_fram_bills_se <- summary(m_fram_bills)$standard.errors
# To convert the coefficients and standard errors from log-odds units to odds ratios or probabilities, you can use the exp()
m_fram_bills_odds_ratio <- exp(m_fram_bills_coef) # Odds ratios: it is a measure of the strength of association
m_fram_bills_percentage<- (exp(m_fram_bills_coef)-1)*100 


# Write output Excel
# create a new workbook
wb <- createWorkbook()
# create a new sheet for the first object
addWorksheet(wb, "MNLR_Model")
# Write the data to the sheets
writeData(wb, sheet = "MNLR_Model", "Odds Ratio", startRow = 1, startCol = 1)
writeData(wb, sheet = "MNLR_Model", "Owned outright is the reference category", startRow = 2, startCol = 1)
writeData(wb, sheet = "MNLR_Model", "One bill is the reference category", startRow = 3, startCol = 1)
writeData(wb, sheet = "MNLR_Model", m_fram_bills_odds_ratio, startRow = 5, startCol = 1,  rowNames = TRUE )
writeData(wb, sheet = "MNLR_Model", "Interpretation (odds) Percentage", startRow = 12, startCol = 1)
writeData(wb, sheet = "MNLR_Model", m_fram_bills_percentage, startRow = 14, startCol = 1, rowNames = TRUE )
# save the workbook
saveWorkbook(wb, "MNLR Model_Housing Tenure & Framing Bills.xlsx")

# ####  Financial Situation Now 
# # "Living comfortably" is the reference category.
m_finnow <- multinom(housing_tenure ~ finnow_f, data = dataReposs, na.action=na.omit)
# # view raw model results
summary(m_finnow)
m_finnow_coef <- summary(m_finnow)$coefficients
m_finnow_se <- summary(m_finnow)$standard.errors
# # To convert the coefficients and standard errors from log-odds units to odds ratios or probabilities, you can use the exp()
m_finnow_odds_ratio <- exp(m_finnow_coef) # Odds ratios
m_finnow_odds_ratio_percentage <- (exp(m_finnow_coef)-1)*100 # Interpretations
# 
# 
# # Write output Excel
# # create a new workbook
wb <- createWorkbook()
# # create a new sheet for the first object
addWorksheet(wb, "MNLR_Model")
# # Write the data to the sheets
writeData(wb, sheet = "MNLR_Model", "Odds Ratio", startRow = 1, startCol = 1)
writeData(wb, sheet = "MNLR_Model", "Owned outright is the reference category", startRow = 2, startCol = 1)
writeData(wb, sheet = "MNLR_Model", "Living comfortably is the reference category", startRow = 3, startCol = 1)
writeData(wb, sheet = "MNLR_Model", m_finnow_odds_ratio, startRow = 5, startCol = 1,  rowNames = TRUE )
writeData(wb, sheet = "MNLR_Model", "Interpretation (odds) Percentage", startRow = 12, startCol = 1)
writeData(wb, sheet = "MNLR_Model", m_finnow_odds_ratio_percentage, startRow = 14, startCol = 1, rowNames = TRUE )
# # save the workbook
saveWorkbook(wb, "MNLR Model_Housing Tenure & Financial Situation Now.xlsx")




# ####  Age & Marital status 
# # "Single" is the reference category
m_dem1 <- multinom(housing_tenure ~ age + marstat_rec_f, data = dataReposs, na.action=na.omit)
# # view raw model results
summary(m_dem1)
m_dem1_coef <- summary(m_dem1)$coefficients
m_dem1_se <- summary(m_dem1)$standard.errors
# # To convert the coefficients and standard errors from log-odds units to odds ratios or probabilities, you can use the exp()
m_dem1_odds_ratio <- exp(m_dem1_coef) # Odds ratios: it is a measure of the strength of association
m_dem1_percentage <- (exp(m_dem1_coef)-1)*100 
# 
# # Write output Excel
# # create a new workbook
wb <- createWorkbook()
# # create a new sheet for the first object
addWorksheet(wb, "MNLR_Model")
# # Write the data to the sheets
writeData(wb, sheet = "MNLR_Model", "Odds Ratio", startRow = 1, startCol = 1)
writeData(wb, sheet = "MNLR_Model", "Owned outright is the reference category", startRow = 2, startCol = 1)
writeData(wb, sheet = "MNLR_Model", "Single is the reference category", startRow = 3, startCol = 1)
writeData(wb, sheet = "MNLR_Model", m_dem1_odds_ratio, startRow = 5, startCol = 1,  rowNames = TRUE )
writeData(wb, sheet = "MNLR_Model", "Interpretation (odds) Percentage", startRow = 12, startCol = 1)
writeData(wb, sheet = "MNLR_Model", m_dem1_percentage, startRow = 14, startCol = 1, rowNames = TRUE )
# # save the workbook
saveWorkbook(wb, "MNLR Model_Housing Tenure & Age & Marital status.xlsx")

# ####   Current labour force status
# # "Self employed" is the reference category
m_labour <- multinom(housing_tenure ~ jbstat_rec, data = dataReposs, na.action=na.omit)
# # view raw model results
summary(m_labour)
m_labour_coef <- summary(m_labour)$coefficients
m_labour_se <- summary(m_labour)$standard.errors
# # To convert the coefficients and standard errors from log-odds units to odds ratios or probabilities, you can use the exp()
m_labour_odds_ratio <- exp(m_labour_coef) # Odds ratios: it is a measure of the strength of association
m_labour_percentage <- (exp(m_labour_coef)-1)*100 
# 
# # Write output Excel
# # create a new workbook
wb <- createWorkbook()
# # create a new sheet for the first object
addWorksheet(wb, "MNLR_Model")
# # Write the data to the sheets
writeData(wb, sheet = "MNLR_Model", "Odds Ratio", startRow = 1, startCol = 1)
writeData(wb, sheet = "MNLR_Model", "Owned outright is the reference category", startRow = 2, startCol = 1)
writeData(wb, sheet = "MNLR_Model", "Self employed is the reference category", startRow = 3, startCol = 1)
writeData(wb, sheet = "MNLR_Model", m_labour_odds_ratio, startRow = 5, startCol = 1,  rowNames = TRUE )
writeData(wb, sheet = "MNLR_Model", "Interpretation (odds) Percentage", startRow = 12, startCol = 1)
writeData(wb, sheet = "MNLR_Model", m_labour_percentage , startRow = 14, startCol = 1, rowNames = TRUE )
# # save the workbook
saveWorkbook(wb, "MNLR Model_Housing Tenure & Current labour force status.xlsx")







