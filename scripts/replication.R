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
load("../inputs/data/df1.Rda")

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


##########################################
#Figure 3: Decision Type Rate by Treatment
##########################################

####Estimates used in Panel A
m1fig3a <- lm_robust(reject1~I(t_mohammad==0), data=df1, se_type="HC2")
m2fig3a <- lm_robust(reject1~t_mohammad, data=df1, se_type="HC2")

figure3a <- tibble(
  Treatment = c("Muslim","Danish"),
  avg = c(coef(m1fig3a)[1],coef(m2fig3a)[1]), 
  lower = c(confint(m1fig3a)[c(1)],confint(m2fig3a)[c(1)]),
  upper = c(confint(m1fig3a)[c(3)],confint(m2fig3a)[c(3)])
)

####Estimates used in Panel B
m1fig3b <- lm_robust(unclear1~I(t_mohammad==0), data=df1, se_type="HC2")
m2fig3b <- lm_robust(unclear1~t_mohammad, data=df1, se_type="HC2")

figure3b <- tibble(
  Treatment = c("Muslim","Danish"),
  avg = c(coef(m1fig3b)[1],coef(m2fig3b)[1]), 
  lower = c(confint(m1fig3b)[c(1)],confint(m2fig3b)[c(1)]),
  upper = c(confint(m1fig3b)[c(3)],confint(m2fig3b)[c(3)])
)

####Estimates used in Panel C
m1fig3c <- lm_robust(accept1~I(t_mohammad==0), data=df1, se_type="HC2")
m2fig3c <- lm_robust(accept1~t_mohammad, data=df1, se_type="HC2")

m1fig3c <- lm_robust(accept1~I(t_mohammad==0), data=df1, se_type="HC2")
m2fig3c <- lm_robust(accept1~t_mohammad, data=df1, se_type="HC2")

figure3c <- tibble(
  Treatment = c("Muslim","Danish"),
  avg = c(coef(m1fig3c)[1],coef(m2fig3c)[1]), 
  lower = c(confint(m1fig3c)[c(1)],confint(m2fig3c)[c(1)]),
  upper = c(confint(m1fig3c)[c(3)],confint(m2fig3c)[c(3)])
)

####Figure in Panel A
fig3a <-  ggplot(figure3a, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  geom_text(aes(y=upper, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+
  labs(y="Decision Type Rate",x="Clear Rejection",title="A")+
  scale_fill_manual(values=c("grey","white")) +  
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5), limits=c(0,.5))+
  theme_minimal()+
  theme(text = element_text(size=12),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

####Figure in Panel B
fig3b <- ggplot(figure3b, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  geom_text(aes(y=upper, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+  
  labs(y="",x="Unclear Response", title="B")+
  scale_fill_manual(values=c("grey","white")) +  
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5), limits=c(0,.5))+
  theme_minimal()+
  theme(text = element_text(size=12),legend.position="none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

####Figure in Panel C
fig3c <- ggplot(figure3c, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  geom_text(aes(y=upper, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+  
  labs(y="",x="Clear Acceptance",title="C")+
  scale_fill_manual(values=c("grey","white")) + 
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5), limits=c(0,.5))+
  theme_minimal()+
  theme(text = element_text(size=12),legend.position="none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

#get common legend
legend <- get_legend(fig3a)
fig3a <- fig3a + theme(text = element_text(size=12),legend.position="none",axis.text.x=element_blank()) 

#Saving figure 3 as PNG
fig3 <- grid.arrange(fig3a, fig3b, fig3c, legend, nrow = 1, widths=c(2, 2, 2, 1), top=textGrob("Decision type rate based on each treatment", gp=gpar(fontsize=15,font=8)))
ggsave(file = "replicated_figure.png",fig3,path = "outputs/models",width=8,height=4,scale=1)



##############
#END OF SCRIPT
##############

