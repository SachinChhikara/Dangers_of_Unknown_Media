#### Preamble ####
# Purpose: Models the bar graph for different proportion depending on senders' name and school's traits
# Author: Kyungrok Park
# Date: 11 February 2024 
# Contact: kyungrok.park@mail.utoronto.ca
# License: MIT
# Pre-requisites: follow the workspace setup to install and load the following packages
# Any other information needed? df1.rda file was examined and the graphs were made based on the information in df1.rda, 
# it is recommended to read the codebook and df1.rda file to acknowledge variables used in the below script

#### Workspace setup ####
install.packages("gridExtra")
install.packages("texreg")
install.packages("scales")
install.packages("cowplot")

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(texreg)
library(scales)
library(cowplot)
library(data.table)

load("/cloud/project/inputs/data/df1.rda")

### Graph for proportion of different types of response based on senders'ethnicity and big city ###

# calculation for proportion 
prop1 <- sum(df1$t_mohammad == '0' & df1$reject1 == '1' & df1$bigcity == '1')/sum(df1$t_mohammad == '0' & df1$bigcity == '1')
prop2 <-sum(df1$t_mohammad == '1' & df1$reject1 == '1' & df1$bigcity == '1')/sum(df1$t_mohammad == '1' & df1$bigcity == '1')
prop3 <-sum(df1$t_mohammad == '0' & df1$unclear1 == '1' & df1$bigcity == '1')/sum(df1$t_mohammad == '0' & df1$bigcity == '1')
prop4 <-sum(df1$t_mohammad == '1' & df1$unclear1 == '1' & df1$bigcity == '1')/sum(df1$t_mohammad == '1' & df1$bigcity == '1')
prop5 <- sum(df1$t_mohammad == '0' & df1$accept1 == '1' & df1$bigcity == '1')/sum(df1$t_mohammad == '0' & df1$bigcity == '1')
prop6 <- sum(df1$t_mohammad == '1' & df1$accept1 == '1' & df1$bigcity == '1')/sum(df1$t_mohammad == '1' & df1$bigcity == '1')

# make a 2 by 2 table for proportion 
first_figure <- tibble(Treatment = c("Muslim","Danish"),avg = c(prop1, prop2))

# make a bar graph for proportion of clear rejection based on senders' name
first_graph <- ggplot(first_figure, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat = "identity", position="dodge", color= "black", width=.5)+
  geom_text(aes(y=avg, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+
  labs(y="Decision Type Rate",x="Clear Rejection",title="A")+
  scale_fill_manual(values=c("grey","white")) +  
  theme_minimal()+
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6), limits=c(0,.6))+
  theme(text = element_text(size=12),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) 


# For unclear resposne 
second_figure <- tibble(Treatment = c("Muslim","Danish"),avg = c(prop3, prop4))

second_graph <- ggplot(second_figure, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
  geom_text(aes(y=avg, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+
  labs(y="",x="Unclear Rejection",title="B")+
  scale_fill_manual(values=c("grey","white")) +  
  theme_minimal()+
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6), limits=c(0,.6))+
  theme(text = element_text(size=12),legend.position="none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

# for clear acceptance 
third_figure <- tibble(Treatment = c("Muslim", "Danish"), avg = c(prop5, prop6))

third_graph <- ggplot(third_figure, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
  geom_text(aes(y=avg, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+
  labs(y="",x="Clear Acceptance",title="C")+
  scale_fill_manual(values=c("grey","white")) +  
  theme_minimal()+
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6), limits=c(0,.6))+
  theme(text = element_text(size=12),legend.position="none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank())
legend1 <- get_legend(first_graph)
first_graph <- first_graph + theme(text = element_text(size=12),legend.position="none",axis.text.x=element_blank())
first_combined_figure <- grid.arrange(first_graph, second_graph, third_graph, legend1, nrow = 1, widths=c(2, 2, 2, 1))

saveRDS(
  first_combined_figure,
  file = "outputs/models/bigcity_prop.rds"
)

prop7 <- sum(df1$t_mohammad == '0' & df1$reject1 == '1' & df1$bigcity == '0')/sum(df1$t_mohammad == '0' & df1$bigcity == '0')
prop8 <-sum(df1$t_mohammad == '1' & df1$reject1 == '1' & df1$bigcity == '0')/sum(df1$t_mohammad == '1' & df1$bigcity == '0')
prop9 <-sum(df1$t_mohammad == '0' & df1$unclear1 == '1' & df1$bigcity == '0')/sum(df1$t_mohammad == '0' & df1$bigcity == '0')
prop10 <-sum(df1$t_mohammad == '1' & df1$unclear1 == '1' & df1$bigcity == '0')/sum(df1$t_mohammad == '1' & df1$bigcity == '0')
prop11 <- sum(df1$t_mohammad == '0' & df1$accept1 == '1' & df1$bigcity == '0')/sum(df1$t_mohammad == '0' & df1$bigcity == '0')
prop12 <- sum(df1$t_mohammad == '1' & df1$accept1 == '1' & df1$bigcity == '0')/sum(df1$t_mohammad == '1' & df1$bigcity == '0')

fourth_figure <- tibble(Treatment = c("Muslim","Danish"),avg = c(prop7, prop8))

fourth_graph <- ggplot(fourth_figure, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
  geom_text(aes(y=avg, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+
  labs(y="Decision Type Rate",x="Clear Rejection",title="A")+
  scale_fill_manual(values=c("grey","white")) + 
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6), limits=c(0,.6))+
  theme_minimal()+
  theme(text = element_text(size=12),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

fifth_figure <- tibble(Treatment = c("Muslim","Danish"),avg = c(prop9, prop10))

fifth_graph <- ggplot(second_figure, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
  geom_text(aes(y=avg, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+
  labs(y="",x="Unclear Rejection",title="B")+
  scale_fill_manual(values=c("grey","white")) +  
  theme_minimal()+
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6), limits=c(0,.6))+
  theme(text = element_text(size=12),legend.position="none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

sixth_figure <- tibble(Treatment = c("Muslim", "Danish"), avg = c(prop11, prop12))

sixth_graph <- ggplot(third_figure, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
  geom_text(aes(y=avg, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+
  labs(y="",x="Clear Acceptance",title="C")+
  scale_fill_manual(values=c("grey","white")) +  
  theme_minimal()+
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6), limits=c(0,.6))+
  theme(text = element_text(size=12),legend.position="none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank())
legend1 <- get_legend(fourth_graph)
fourth_graph <- fourth_graph + theme(text = element_text(size=12),legend.position="none",axis.text.x=element_blank())
second_combined_figure <- grid.arrange(fourth_graph, fifth_graph, sixth_graph, legend1, nrow = 1, widths=c(2, 2, 2, 1))

saveRDS(
  second_combined_figure,
  file = "outputs/models/non_bigcity_prop.rds"
)

### Graph for proportion of different types of responses based on senders' ethnicity and school type, private or public
# calculation for proportion 
prop1 <- sum(df1$t_mohammad == '0' & df1$reject1 == '1' & df1$school_type == '1')/sum(df1$t_mohammad == '0' & df1$school_type == '1')
prop2 <-sum(df1$t_mohammad == '1' & df1$reject1 == '1' & df1$school_type == '1')/sum(df1$t_mohammad == '1' & df1$school_type == '1')
prop3 <-sum(df1$t_mohammad == '0' & df1$unclear1 == '1' & df1$school_type == '1')/sum(df1$t_mohammad == '0' & df1$school_type == '1')
prop4 <-sum(df1$t_mohammad == '1' & df1$unclear1 == '1' & df1$school_type == '1')/sum(df1$t_mohammad == '1' & df1$school_type == '1')
prop5 <- sum(df1$t_mohammad == '0' & df1$accept1 == '1' & df1$school_type == '1')/sum(df1$t_mohammad == '0' & df1$school_type == '1')
prop6 <- sum(df1$t_mohammad == '1' & df1$accept1 == '1' & df1$school_type == '1')/sum(df1$t_mohammad == '1' & df1$school_type == '1')

# make a 2 by 2 table for proportion 
first_figure <- tibble(Treatment = c("Muslim","Danish"),avg = c(prop1, prop2))

# make a bar graph for proportion of clear rejection based on senders' name and big city 
first_graph <- ggplot(first_figure, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat = "identity", position="dodge", color= "black", width=.5)+
  geom_text(aes(y=avg, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+
  labs(y="Decision Type Rate",x="Clear Rejection",title="A")+
  scale_fill_manual(values=c("grey","white")) +  
  theme_minimal()+
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6,.7), limits=c(0,.7))+
  theme(text = element_text(size=12),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) 


# For unclear response 
second_figure <- tibble(Treatment = c("Muslim","Danish"),avg = c(prop3, prop4))

second_graph <- ggplot(second_figure, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
  geom_text(aes(y=avg, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+
  labs(y="",x="Unclear Rejection",title="B")+
  scale_fill_manual(values=c("grey","white")) +  
  theme_minimal()+
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6,.7), limits=c(0,.7))+
  theme(text = element_text(size=12),legend.position="none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

# for clear acceptance 
third_figure <- tibble(Treatment = c("Muslim", "Danish"), avg = c(prop5, prop6))

third_graph <- ggplot(third_figure, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
  geom_text(aes(y=avg, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+
  labs(y="",x="Clear Acceptance",title="C")+
  scale_fill_manual(values=c("grey","white")) +  
  theme_minimal()+
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6,.7), limits=c(0,.7))+
  theme(text = element_text(size=12),legend.position="none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank())
legend1 <- get_legend(first_graph)
first_graph <- first_graph + theme(text = element_text(size=12),legend.position="none",axis.text.x=element_blank())
first_combined_figure <- grid.arrange(first_graph, second_graph, third_graph, legend1, nrow = 1, widths=c(2, 2, 2, 1))

saveRDS(
  first_combined_figure,
  file = "outputs/models/public_school_prop.rds"
)

prop7 <- sum(df1$t_mohammad == '0' & df1$reject1 == '1' & df1$school_type == '0')/sum(df1$t_mohammad == '0' & df1$school_type == '0')
prop8 <-sum(df1$t_mohammad == '1' & df1$reject1 == '1' & df1$school_type == '0')/sum(df1$t_mohammad == '1' & df1$school_type == '0')
prop9 <-sum(df1$t_mohammad == '0' & df1$unclear1 == '1' & df1$school_type == '0')/sum(df1$t_mohammad == '0' & df1$school_type == '0')
prop10 <-sum(df1$t_mohammad == '1' & df1$unclear1 == '1' & df1$school_type == '0')/sum(df1$t_mohammad == '1' & df1$school_type == '0')
prop11 <- sum(df1$t_mohammad == '0' & df1$accept1 == '1' & df1$school_type == '0')/sum(df1$t_mohammad == '0' & df1$school_type == '0')
prop12 <- sum(df1$t_mohammad == '1' & df1$accept1 == '1' & df1$school_type == '0')/sum(df1$t_mohammad == '1' & df1$school_type == '0')

fourth_figure <- tibble(Treatment = c("Muslim","Danish"),avg = c(prop7, prop8))

fourth_graph <- ggplot(fourth_figure, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
  geom_text(aes(y=avg, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+
  labs(y="Decision Type Rate",x="Clear Rejection",title="A")+
  scale_fill_manual(values=c("grey","white")) + 
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6,.7), limits=c(0,.7))+
  theme_minimal()+
  theme(text = element_text(size=12),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

fifth_figure <- tibble(Treatment = c("Muslim","Danish"),avg = c(prop9, prop10))

fifth_graph <- ggplot(second_figure, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
  geom_text(aes(y=avg, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+
  labs(y="",x="Unclear Rejection",title="B")+
  scale_fill_manual(values=c("grey","white")) +  
  theme_minimal()+
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6,.7), limits=c(0,.7))+
  theme(text = element_text(size=12),legend.position="none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

sixth_figure <- tibble(Treatment = c("Muslim", "Danish"), avg = c(prop11, prop12))

sixth_graph <- ggplot(third_figure, aes(x=Treatment,y=avg,fill=Treatment)) +
  geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
  geom_text(aes(y=avg, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+
  labs(y="",x="Clear Acceptance",title="C")+
  scale_fill_manual(values=c("grey","white")) +  
  theme_minimal()+
  scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6,.7), limits=c(0,.7))+
  theme(text = element_text(size=12),legend.position="none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank())
legend1 <- get_legend(fourth_graph)
fourth_graph <- fourth_graph + theme(text = element_text(size=12),legend.position="none",axis.text.x=element_blank())
second_combined_figure <- grid.arrange(fourth_graph, fifth_graph, sixth_graph, legend1, nrow = 1, widths=c(2, 2, 2, 1))

saveRDS(
  second_combined_figure,
  file = "outputs/models/private_school_prop.rds"
)


### Table for proportion of different types of responses based on senders' ethnicity and school's trait (above median)
# calculation for proportion 
prop1 <- sum(df1$t_mohammad == '0' & df1$reject1 == '1' & df1$above_median_student == '1', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_student == '1', na.rm=T)
prop2 <-sum(df1$t_mohammad == '0' & df1$unclear1 == '1' & df1$above_median_student == '1', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_student == '1', na.rm=T)
prop3 <- sum(df1$t_mohammad == '0' & df1$accept1 == '1' & df1$above_median_student == '1', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_student == '1', na.rm=T)
prop4 <- sum(df1$t_mohammad == '0' & df1$reject1 == '1' & df1$above_median_student == '0', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_student == '0', na.rm=T)
prop5 <-sum(df1$t_mohammad == '0' & df1$unclear1 == '1' & df1$above_median_student == '0', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_student == '0', na.rm=T)
prop6 <- sum(df1$t_mohammad == '0' & df1$accept1 == '1' & df1$above_median_student == '0', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_student == '0', na.rm=T)



prop7 <- sum(df1$t_mohammad == '0' & df1$reject1 == '1' & df1$above_median_grades == '1', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_grades == '1', na.rm=T)
prop8 <-sum(df1$t_mohammad == '0' & df1$unclear1 == '1' & df1$above_median_grades == '1', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_grades == '1', na.rm=T)
prop9 <- sum(df1$t_mohammad == '0' & df1$accept1 == '1' & df1$above_median_grades == '1', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_grades == '1', na.rm=T)
prop10 <- sum(df1$t_mohammad == '0' & df1$reject1 == '1' & df1$above_median_grades == '0', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_grades == '0', na.rm=T)
prop11 <-sum(df1$t_mohammad == '0' & df1$unclear1 == '1' & df1$above_median_grades == '0', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_grades == '0', na.rm=T)
prop12<- sum(df1$t_mohammad == '0' & df1$accept1 == '1' & df1$above_median_grades == '0', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_grades == '0', na.rm=T)


prop13 <- sum(df1$t_mohammad == '0' & df1$reject1 == '1' & df1$above_median_non_danes == '1', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_non_danes == '1', na.rm=T)
prop14 <-sum(df1$t_mohammad == '0' & df1$unclear1 == '1' & df1$above_median_non_danes == '1', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_non_danes == '1', na.rm=T)
prop15 <- sum(df1$t_mohammad == '0' & df1$accept1 == '1' & df1$above_median_non_danes == '1', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_non_danes == '1', na.rm=T)
prop16 <- sum(df1$t_mohammad == '0' & df1$reject1 == '1' & df1$above_median_non_danes == '0', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_non_danes == '0', na.rm=T)
prop17 <-sum(df1$t_mohammad == '0' & df1$unclear1 == '1' & df1$above_median_non_danes == '0', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_non_danes == '0', na.rm=T)
prop18 <- sum(df1$t_mohammad == '0' & df1$accept1 == '1' & df1$above_median_non_danes == '0', na.rm=T)/sum(df1$t_mohammad == '0' & df1$above_median_non_danes == '0', na.rm=T)

danish_median_combined_table <- data.frame("Clear" = c(prop1, prop4, prop7, prop10, prop13, prop16), "Unclear" = c(prop2, prop5, prop8, prop11, prop14, prop17),
                                    "Accept" = c(prop3, prop6, prop9, prop12, prop15, prop18))

rownames(danish_median_combined_table) <- c("Above Median Number of Students", "Below Median Number of Students", "Above Median Grade", "Below Median Grade", "Above Median Number of Dansih Students", "Below Median Number of Danish Students") 

write_csv(danish_median_combined_table, 
          "outputs/models/danish_median_combined.csv")

prop1 <- sum(df1$t_mohammad == '1' & df1$reject1 == '1' & df1$above_median_student == '1', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_student == '1', na.rm=T)
prop2 <-sum(df1$t_mohammad == '1' & df1$unclear1 == '1' & df1$above_median_student == '1', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_student == '1', na.rm=T)
prop3 <- sum(df1$t_mohammad == '1' & df1$accept1 == '1' & df1$above_median_student == '1', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_student == '1', na.rm=T)
prop4 <- sum(df1$t_mohammad == '1' & df1$reject1 == '1' & df1$above_median_student == '0', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_student == '0', na.rm=T)
prop5 <-sum(df1$t_mohammad == '1' & df1$unclear1 == '1' & df1$above_median_student == '0', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_student == '0', na.rm=T)
prop6 <- sum(df1$t_mohammad == '1' & df1$accept1 == '1' & df1$above_median_student == '0', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_student == '0', na.rm=T)




prop7 <- sum(df1$t_mohammad == '1' & df1$reject1 == '1' & df1$above_median_grades == '1', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_grades == '1', na.rm=T)
prop8 <-sum(df1$t_mohammad == '1' & df1$unclear1 == '1' & df1$above_median_grades == '1', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_grades == '1', na.rm=T)
prop9 <- sum(df1$t_mohammad == '1' & df1$accept1 == '1' & df1$above_median_grades == '1', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_grades == '1', na.rm=T)
prop10 <- sum(df1$t_mohammad == '1' & df1$reject1 == '1' & df1$above_median_grades == '0', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_grades == '0', na.rm=T)
prop11 <-sum(df1$t_mohammad == '1' & df1$unclear1 == '1' & df1$above_median_grades == '0', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_grades == '0', na.rm=T)
prop12<- sum(df1$t_mohammad == '1' & df1$accept1 == '1' & df1$above_median_grades == '0', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_grades == '0', na.rm=T)


prop13 <- sum(df1$t_mohammad == '1' & df1$reject1 == '1' & df1$above_median_non_danes == '1', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_non_danes == '1', na.rm=T)
prop14 <-sum(df1$t_mohammad == '1' & df1$unclear1 == '1' & df1$above_median_non_danes == '1', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_non_danes == '1', na.rm=T)
prop15 <- sum(df1$t_mohammad == '1' & df1$accept1 == '1' & df1$above_median_non_danes == '1', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_non_danes == '1', na.rm=T)
prop16 <- sum(df1$t_mohammad == '1' & df1$reject1 == '1' & df1$above_median_non_danes == '0', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_non_danes == '0', na.rm=T)
prop17 <-sum(df1$t_mohammad == '1' & df1$unclear1 == '1' & df1$above_median_non_danes == '0', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_non_danes == '0', na.rm=T)
prop18 <- sum(df1$t_mohammad == '1' & df1$accept1 == '1' & df1$above_median_non_danes == '0', na.rm=T)/sum(df1$t_mohammad == '1' & df1$above_median_non_danes == '0', na.rm=T)

muslim_median_combined_table <- data.frame("Clear" = c(prop1, prop4, prop7, prop10, prop13, prop16), "Unclear" = c(prop2, prop5, prop8, prop11, prop14, prop17),
                                           "Accept" = c(prop3, prop6, prop9, prop12, prop15, prop18))
rownames(muslim_median_combined_table) <- c("Above Median Number of Students", "Below Median Number of Students", "Above Median Grade", "Below Median Grade", "Above Median Number of Dansih Students", "Below Median Number of Danish Students")

write_csv(muslim_median_combined_table, 
          "outputs/models/muslim_median_combined.csv")

