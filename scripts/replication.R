# ########################################################################################
# Purpose: Create 3 replication of table and figures
# Author: Sachin Chhikara
# Date: 11 February 2024
# Replication Package based off the paper: The Unequal Distribution of Opportunity: 
# A National Audit Study of Bureaucratic Discrimination in Primary School Access
#
# Code was inspired by: 
# Asmus Leth Olsen, Jonas H?gh Kyhse-Andersen and Donald Moynihan replication package                                   
# Summary: Replication code for producing all tables, figures and facts in the main text.    
#
#
# #########################################################################################

rm(list = ls())

#load packages
library(tidyverse)
library(ltm)
library(gridExtra)
library(estimatr)
library(texreg)
library(scales)
library(cowplot)
library(ggplot2)

####################
# Note to the reader
####################

# This script is arranged in the order as the figures, tables and facts are presented in the manuscript.
# A separate codebook provides definition of all variables and their respective values.

##set path for data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#load data for main analysis
load("df1.Rda")

##########################################################
# Facts in section 'Abstract' & 'Introduction'
##########################################################

#N in the main data file
nrow(df1)


##############################################################
#Table 3: Means for the Dependent Variables across Treatments
##############################################################

#content of table 3
#0=Muslim Name, public school
#1=Muslim Name, private school
#2=Danish Name, public school
#3=Danish Name, private school

for (i in 1:length(df1$treatments)) {
  
  if (df1$t_mohammad[i] == 1 & df1$school_type[i] == 1){
    df1$treatments[i] <- 0

  }
  else if (df1$t_mohammad[i] == 1 & df1$school_type[i] == 0){
    df1$treatments[i] <- 1

  }
  else if (df1$t_mohammad[i] == 0 & df1$school_type[i] == 1){
    df1$treatments[i] <- 2
  }
  else {
    df1$treatments[i] <- 3
  }
  
}

#Means for all outcomes for each of the four conditions
table7 <-  df1 %>% 
  dplyr::group_by(treatments)%>% 
  dplyr::summarise_at(vars(resp_true,reject1,unclear1,accept1, 
                           simple_q,complex_q,contact_q,meeting_q,
                           no_greet,greet_no_name,informal_greet,formal_greet), 
                      list(mean = ~ round(mean(.),2))
  )


count_of_private <- sum(df1$school_type==0)
count_of_public <- sum(df1$school_type==1)

response <- df1 %>% 
  dplyr::group_by(t_mohammad) %>%
  dplyr::summarise_at(vars(resp_true), 
                      list(mean = ~ round(mean(.), 2),
                           sd = ~ round(sd(.), 2)))

#using CLT, since our n is large( n > 30), we approximate the distribution to Normal
#95% condifence interval

calculate_CI <- function(mean, sd, df) {
  alpha <- 1 - 0.95
  t_val <- qt(p = 1 - alpha/2, df = df)
  se <- sd / sqrt(df)
  lower <- round(mean - t_val * se, 2)
  upper <- round(mean + t_val * se, 2)
  return(c(lower, upper))
}

# Calculate confidence intervals for Muslim group
df_muslim <- sum(df1$t_mohammad == 1) - 1
mean_muslim <- response$mean[2]
sd_muslim <- response$sd[2]
muslim_CI <- calculate_CI(mean_muslim, sd_muslim, df_muslim)

# Calculate confidence intervals for Danish group
df_danish <- sum(df1$t_mohammad == 0) - 1
mean_danish <- response$mean[1]
sd_danish <- response$sd[1]
danish_CI <- calculate_CI(mean_danish, sd_danish, df_danish)

# Create a data frame for the mean and confidence intervals
data <- data.frame(
  group = c("Muslim", "Danish"),
  mean = c(mean_muslim, mean_danish),
  lower = c(muslim_CI[1], danish_CI[1]),
  upper = c(muslim_CI[2], danish_CI[2])
)

# Create the bar plot with error bars
ggplot(data, aes(x = group, y = mean, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.9)) +
  labs(title = "Figure 2 Response Rate by Ethicity",
       x = "Group",
       y = "Mean") +
  theme_minimal()



##############
#END OF SCRIPT
##############

