# ########################################################################################
#
# Code: October 7, 2020
# 
# Title: The Unequal Distribution of Opportunity: 
# A National Audit Study of Bureaucratic Discrimination in Primary School Access
#
# Authors: Asmus Leth Olsen, Jonas H?gh Kyhse-Andersen and Donald Moynihan                                                                          
#                                   
# Summary: Replication code for producing all tables, figures and facts in the main text.    
#
# Please report errors to: Asmus Leth Olsen, at: ajlo@ifs.ku.dk
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
library(grid)

####################
# Note to the reader
####################

# This script is arranged in the order as the figures, tables and facts are presented in the manuscript.
# A separate codebook provides definition of all variables and their respective values.

##set path for data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#load data for main analysis
load("df1.rda")

##########################################################
# Facts in section 'Abstract' & 'Introduction'
##########################################################

#N in the main data file
nrow(df1)

#Main finding: difference of ~10 percentage point in acceptance for the main treatment
main<-lm_robust(accept1~t_mohammad, data=df1, se_type="HC2")
summary(main)

#acceptance rate for Danish treatment in percent
round(coef(main)[1]*100,1)

#acceptance rate for Muslim treatment in percent
round(coef(main)[1]*100,1)+round(coef(main)[2]*100,1)

##########################################################################################
# Figure 1: Total Number of Students and Percentage of Non-native Danish Students in Each 
##########################################################################################

#load data for descriptives on schools
load("school_df.Rda")

####Panel A: histogram of the total number of students across the schools in the sample
hist_a <- school_df %>% 
          
          drop_na(.,num_student) %>% 

                    ggplot() +
                    geom_histogram(aes(x=num_student),bins=40) +
                    scale_x_continuous(breaks = pretty_breaks()) + 
                    labs(y="Counts",x="Total Number of Students in Each School",title="A") +
                    theme_minimal() +
                    theme(text = element_text(size=12))

####Panel B: histogram of non danish students across the schools in the sample
hist_b <- school_df %>% 
  
          drop_na(.,pct_non_danes) %>% 
  
                    ggplot() +
                    geom_histogram(aes(x=pct_non_danes),bins=40) +
                    scale_x_continuous(breaks = pretty_breaks()) + 
                    labs(y="",x="Pct. Non-native Danish Students",title="B") +
                    theme_minimal() +
                    theme(text = element_text(size=12))

hists <- grid.arrange(hist_a, hist_b, nrow = 1)

#produce PNG file of Figure 1
ggsave(file = "figure1.png",hists,width=6,height=4,scale=1)

#produce PDF file of Figure 1
ggsave(file = "figure1.pdf",hists,width=6,height=4,scale=1)


###############################
# Facts in Section 'Treatments'
###############################

#Treament mails send by condition: 0=Peter Nielsen, 1=Mohammad Osman
table(df1$t_mohammad)

##################################################
#Table 1: Descriptive Statistics and Balance Test
##################################################

#calculate N and means in in table 3
table1 <-df1 %>% group_by(.dots=c("treatments"))   %>%
                 summarize(obs=length(treatments),
                           above_median_student=round(mean(above_median_student,na.rm=T)*100,1),
                           above_median_non_danes=round(mean(above_median_non_danes,na.rm=T)*100,1),
                           above_median_grades=round(mean(above_median_grades,na.rm=T)*100,1),
                           bigcity=round(mean(bigcity*100),1),
                           school_type=round(mean(school_type==1)*100,1)
                           )

#see N and means in table 1
#0=Muslim Name, Diligent Student
#1=Muslim Name, No Student Info
#2=Danish Name, Diligent Student
#3=Danish Name, No Student Info
table1

#and calculate f-statistics - two final columns of table 1
#for all four conditions
summary(lm(above_median_student~factor(treatments),data=df1))
summary(lm(above_median_non_danes~factor(treatments),data=df1))
summary(lm(above_median_grades~factor(treatments),data=df1))
summary(lm(bigcity~factor(treatments),data=df1))
summary(lm(school_type~factor(treatments),data=df1))

#for main two condition
summary(lm(above_median_student~t_mohammad,data=df1))
summary(lm(above_median_non_danes~t_mohammad,data=df1))
summary(lm(above_median_grades~t_mohammad,data=df1))
summary(lm(bigcity~t_mohammad,data=df1))
summary(lm(school_type~t_mohammad,data=df1))


###########################################
# Facts in section 'Ethical Considerations'
###########################################

#Response times at the 50th and 95th percentile
quantile(df1$days_to_resp+1, na.rm=T, c(.5, .95)) 

########################################
# Facts in section 'Dependent Variables'
########################################

#codeable email responses
table(df1$resp_true)

#response rate
round(mean(df1$resp_true)*100,1)

#mails with a response of forwarding
table(df1$forwarding)

#mean clear rejections ignoring non-responses
round(mean(df1$reject1[subset=df1$resp_true==1])*100,0)

#mean clear rejects with non-responses as reject
round(mean(df1$reject1)*100,0)

#mean clear accepts 
round(mean(df1$accept1)*100,0)

#mean unclear responses
round(mean(df1$unclear1)*100,0)

#do reliability test on the subset of data with actual responses (in order to not overestimate reliability)
relia_df <-df1 %>% 
  dplyr::filter(.,resp_true==1) %>% 
  dplyr::select(.,c(reject1,reject2,unclear1,unclear2,accept1,accept2)) 

#Cronback alpha test of each of the three outcomes; all are over .9
cronbach.alpha(relia_df[,c("reject1","reject2")]) 
cronbach.alpha(relia_df[,c("unclear1","unclear2")]) 
cronbach.alpha(relia_df[,c("accept1","accept2")]) 

#mean for simple questions
round(mean(df1$simple_q)*100,0)

#mean for complex questions
round(mean(df1$complex_q)*100,0)

#mean for contact by phone
round(mean(df1$contact_q)*100,0)

#mean for meet in person
round(mean(df1$meeting_q)*100,0)

#mean for no greeting (with non-responses coded as no greeting)
round(mean(df1$no_greet)*100,0)

#mean for greeting with no name
round(mean(df1$greet_no_name)*100,0)

#mean for informal greeting ('hi', etc.)
round(mean(df1$informal_greet)*100,0)

#mean for formal greeting ('Dear' etc.)
round(mean(df1$formal_greet)*100,0)

#load data for responder characteristics
load("responder_df.Rda")

#gender of responder (percentages among responses)
round(prop.table(table(responder_df$sender_male))*100,0)

#ethnicity of responder (percentages among responses)
round(prop.table(table(responder_df$sender_eth))*100,0)

#formal position of responder (percentages among responses)
round(prop.table(table(responder_df$sender_pos))*100,0)

##############################################################
#Table 3: Means for the Dependent Variables across Treatments
##############################################################

#Means for all outcomes for each of the four conditions
table3 <-  df1 %>% 
          dplyr::group_by(treatments)%>% 
          dplyr::summarise_at(vars(resp_true,reject1,unclear1,accept1, 
                           simple_q,complex_q,contact_q,meeting_q,
                           no_greet,greet_no_name,informal_greet,formal_greet), 
                           list(mean = ~ round(mean(.),2))
                            )
#content of table 3
#0=Muslim Name, Diligent Student
#1=Muslim Name, No Student Info
#2=Danish Name, Diligent Student
#3=Danish Name, No Student Info
#first four outcomes
table3[c(1, 2:5)]
#next four outcomes
table3[c(1, 6:9)]
#final four outcomes
table3[c(1, 10:13)]

#####################################
#Figure 2: Response Rate by Treatment
#####################################

####Estimates used in figure 1
m1fig2 <-lm_robust(resp_true~I(t_mohammad==0), data=df1, se_type="HC2")
m2fig2 <-lm_robust(resp_true~t_mohammad, data=df1, se_type="HC2")

figure2 <- tibble(
                   Treatment = c("Muslim","Danish"),
                   avg = c(coef(m1fig2)[1],coef(m2fig2)[1]), 
                   lower = c(confint(m1fig2)[c(1)],confint(m2fig2)[c(1)]),
                   upper = c(confint(m1fig2)[c(3)],confint(m2fig2)[c(3)])
                   )
  
fig2 <- ggplot(figure2, aes(x=Treatment,y=avg,fill=Treatment)) +
            geom_bar(stat="identity",position="dodge", color= "black", width=.25)+
            geom_errorbar(aes(ymin=lower, ymax=upper), width=.1,position=position_dodge(.9))+
            geom_text(aes(y=upper, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+
            labs(y="Response Rate",x="",title="")+
            scale_fill_manual(values=c("grey","white")) + 
            scale_y_continuous(limits = c(0, 1))+
            theme_minimal()+
            theme(text = element_text(size=14),legend.position="none")

#Saving figure 3 as PNG
ggsave(file = "figure2.png",fig2, width=4,height=4,scale=1)

#Saving figure 3 as PDF
ggsave(file = "figure2.pdf",fig2, width=4,height=4,scale=1)

##################################
# Facts in section 'Response Rate'
##################################

#main effect on response rate
summary(m2fig2)

#main results for clear accept - scaled to percentages
round(coef(m2fig2)*100,1)
round(confint(m2fig2)*100,1)

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

#Saving figure 3 as PDF
ggsave(file = "figure3.pdf",fig3,width=8,height=4,scale=1)

##########################################################
# Facts in section 'Decision to Accept or Reject Students'
##########################################################

#main results for clear accept
summary(m2fig3c)

#main results for clear accept - scaled to percentages
round(coef(m2fig3c)*100,1)
round(confint(m2fig3c)*100,1)

#acceptance rate for Danish treatment in percent
round(coef(m2fig3c)[1]*100,1)

#acceptance rate for Muslim treatment in percent
round(coef(m2fig3c)[1]*100,1)+round(coef(m2fig3c)[2]*100,1)

#lower acceptance rates for Muslim name in percentages
round(round(coef(m2fig3c)*100,1)/(round(coef(m2fig3c)[1]*100,1))*100,1)

#main results for clear reject
summary(m2fig3a)

#main results for clear reject - scaled to percentages
round(coef(m2fig3a)*100,1)
round(confint(m2fig3a)*100,1)

#main results for unclear responses
summary(m2fig3b)

#main results for unclear responses- scaled to percentages
round(coef(m2fig3b)*100,1)
round(confint(m2fig3b)*100,1)

#waiting list result in footnote
mean(df1$waitinglist)
wait<-lm_robust(waitinglist~I(t_mohammad), data=df1, se_type="HC2")
summary(wait)


###############################################################
# Figure 4: Compliance Costs by Treatment
###############################################################

####Estimates used in panel A
m1fig4a <- lm_robust(simple_q~I(t_mohammad==0), data=df1, se_type="HC2")
m2fig4a <- lm_robust(simple_q~t_mohammad, data=df1, se_type="HC2")
figure4a <- tibble(
                  Treatment = c("Muslim","Danish"),
                  avg = c(coef(m1fig4a)[1],coef(m2fig4a)[1]), 
                  lower = c(confint(m1fig4a)[c(1)],confint(m2fig4a)[c(1)]),
                  upper = c(confint(m1fig4a)[c(3)],confint(m2fig4a)[c(3)])
                  )

####Estimates used in panel B
m1fig4b <- lm_robust(complex_q~I(t_mohammad==0), data=df1, se_type="HC2")
m2fig4b <- lm_robust(complex_q~t_mohammad, data=df1, se_type="HC2")
figure4b <- tibble(
                  Treatment = c("Muslim","Danish"),
                  avg = c(coef(m1fig4b)[1],coef(m2fig4b)[1]), 
                  lower = c(confint(m1fig4b)[c(1)],confint(m2fig4b)[c(1)]),
                  upper = c(confint(m1fig4b)[c(3)],confint(m2fig4b)[c(3)])
                  )

####Estimates used in panel C
m1fig4c <- lm_robust(contact_q~I(t_mohammad==0), data=df1, se_type="HC2")
m2fig4c <- lm_robust(contact_q~t_mohammad, data=df1, se_type="HC2")
figure4c <- tibble(
                  Treatment = c("Muslim","Danish"),
                  avg = c(coef(m1fig4c)[1],coef(m2fig4c)[1]), 
                  lower = c(confint(m1fig4c)[c(1)],confint(m2fig4c)[c(1)]),
                  upper = c(confint(m1fig4c)[c(3)],confint(m2fig4c)[c(3)])
                  )

####Estimates used in panel D
m1fig4d <- lm_robust(meeting_q~I(t_mohammad==0), data=df1, se_type="HC2")
m2fig4d <- lm_robust(meeting_q~t_mohammad, data=df1, se_type="HC2")
figure4d <- tibble(
                  Treatment = c("Muslim","Danish"),
                  avg = c(coef(m1fig4d)[1],coef(m2fig4d)[1]), 
                  lower = c(confint(m1fig4d)[c(1)],confint(m2fig4d)[c(1)]),
                  upper = c(confint(m1fig4d)[c(3)],confint(m2fig4d)[c(3)])
                  )

####Figure in Panel A
fig4a <- ggplot(figure4a, aes(x=Treatment,y=avg,fill=Treatment)) +
            geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
            geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
            geom_text(aes(y=upper, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+  
            labs(y="Question Rate",x="Simple Question",title="A")+
            scale_fill_manual(values=c("grey","white")) +  
            scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5),label=c(0,.1,.2,.3,.4,.5),limits=c(0,.5))+ 
            theme_minimal()+
            theme(text = element_text(size=12),
                  axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

####Figure in Panel B
fig4b <- ggplot(figure4b, aes(x=Treatment,y=avg,fill=Treatment)) +
            geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
            geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
            geom_text(aes(y=upper, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+  
            labs(y="",x="Complex Question",title="B")+
            scale_fill_manual(values=c("grey","white")) +  
            scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5),label=c(0,.1,.2,.3,.4,.5),limits=c(0,.5))+ 
            theme_minimal()+
            theme(text = element_text(size=12),legend.position="none",
                  axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

####Figure in Panel C
fig4c <- ggplot(figure4c, aes(x=Treatment,y=avg,fill=Treatment)) +
            geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
            geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
            geom_text(aes(y=upper, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+  
            labs(y="Request Rate",x="Request a Phone Call",title="C")+
            scale_fill_manual(values=c("grey","white")) +  
            scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5),label=c(0,.1,.2,.3,.4,.5),limits=c(0,.5))+ 
            theme_minimal()+
            theme(text = element_text(size=12),legend.position="none",
                  axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

####Figure in Panel D
fig4d <- ggplot(figure4d, aes(x=Treatment,y=avg,fill=Treatment)) +
            geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
            geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
            geom_text(aes(y=upper, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+  
            labs(y="",x="Request to Meet",title="D")+
            scale_fill_manual(values=c("grey","white")) +  
            scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5),label=c(0,.1,.2,.3,.4,.5),limits=c(0,.5))+ 
            theme_minimal()+
            theme(text = element_text(size=12),legend.position="none",
                  axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

#get common legend
legend <- get_legend(fig4a)
fig4a <- fig4a + theme(text = element_text(size=12),legend.position="none",axis.text.x=element_blank()) 

#Save figure 4 a PNG
fig4 <- grid.arrange(fig4a, fig4b, fig4c, fig4d, legend, widths=c(2, 2, 2, 2, 1))
ggsave(file = "figure4.png",fig4,width=10,height=4,scale=1)
ggsave(file = "figure4.pdf",fig4,width=10,height=4,scale=1)

########################################################################
# Facts in section 'Compliance costs: Additional Questions and Requests'
########################################################################

#main results for simple question
summary(m2fig4a)

#main results for simple question - scaled to percentages
round(coef(m2fig4a)*100,1)
round(confint(m2fig4a)*100,1)

#results for alternative coding of simple question that includes non-responses as a simple questions:
m2fig4aa <- lm_robust(simple_q_alt~t_mohammad, data=df1, se_type="HC2")
summary(m2fig4aa)
round(coef(m2fig4aa)*100,1)
round(confint(m2fig4aa)*100,1)

#main results for complex question
summary(m2fig4b)

#main results for complex question - scaled to percentages
round(coef(m2fig4b)*100,1)
round(confint(m2fig4b)*100,1)

#main results for request to meet
summary(m2fig4d)

#main results for request to meet - scaled to percentages
round(coef(m2fig4d)*100,1)
round(confint(m2fig4d)*100,1)

#main results for request to phone call
summary(m2fig4c)

#main results for request to phone call - scaled to percentages
round(coef(m2fig4c)*100,1)
round(confint(m2fig4c)*100,1)


###############################################################
# Figure 5: Psychological Costs by Treatment
###############################################################

####Estimates used in panel A
m1fig5a <- lm_robust(no_greet~I(t_mohammad==0), data=df1, se_type="HC2")
m2fig5a <- lm_robust(no_greet~t_mohammad, data=df1, se_type="HC2")
figure5a <- tibble(
                   Treatment = c("Muslim","Danish"),
                   avg = c(coef(m1fig5a)[1],coef(m2fig5a)[1]), 
                   lower = c(confint(m1fig5a)[c(1)],confint(m2fig5a)[c(1)]),
                   upper = c(confint(m1fig5a)[c(3)],confint(m2fig5a)[c(3)])
                   )

####Estimates used in panel B
m1fig5b <- lm_robust(greet_no_name~I(t_mohammad==0), data=df1, se_type="HC2")
m2fig5b <- lm_robust(greet_no_name~t_mohammad, data=df1, se_type="HC2")
figure5b <- tibble(
                   Treatment = c("Muslim","Danish"),
                   avg = c(coef(m1fig5b)[1],coef(m2fig5b)[1]), 
                   lower = c(confint(m1fig5b)[c(1)],confint(m2fig5b)[c(1)]),
                   upper = c(confint(m1fig5b)[c(3)],confint(m2fig5b)[c(3)])
                   )

####Estimates used in panel C
m1fig5c <- lm_robust(informal_greet~I(t_mohammad==0), data=df1, se_type="HC2")
m2fig5c <- lm_robust(informal_greet~t_mohammad, data=df1, se_type="HC2")
figure5c <- tibble(
                   Treatment = c("Muslim","Danish"),
                   avg = c(coef(m1fig5c)[1],coef(m2fig5c)[1]), 
                   lower = c(confint(m1fig5c)[c(1)],confint(m2fig5c)[c(1)]),
                   upper = c(confint(m1fig5c)[c(3)],confint(m2fig5c)[c(3)])
                   )

####Estimates used in panel D
m1fig5d <- lm_robust(formal_greet~I(t_mohammad==0), data=df1, se_type="HC2")
m2fig5d <- lm_robust(formal_greet~t_mohammad, data=df1, se_type="HC2")
figure5d <- tibble(
                   Treatment = c("Muslim","Danish"),
                   avg = c(coef(m1fig5d)[1],coef(m2fig5d)[1]), 
                   lower = c(confint(m1fig5d)[c(1)],confint(m2fig5d)[c(1)]),
                   upper = c(confint(m1fig5d)[c(3)],confint(m2fig5d)[c(3)])
                   )

####Figure in Panel A
fig5a <-  ggplot(figure5a, aes(x=Treatment,y=avg,fill=Treatment)) +
            geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
            geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
            geom_text(aes(y=upper, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+  
            labs(y="Greeting Tone Rate",x="No \n Greeting",title="A")+
            scale_fill_manual(values=c("grey","white")) +  
            scale_y_continuous(breaks=c(0,.2,.4,.6),labels=c(0,.2,.4,.6),limits=c(0,.6))+ 
            theme_minimal()+
            theme(text = element_text(size=12),
                axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

####Figure in Panel B
fig5b <-  ggplot(figure5b, aes(x=Treatment,y=avg,fill=Treatment)) +
            geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
            geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
            geom_text(aes(y=upper, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+    
            labs(y="",x="Greeting \n with No Name",title="B")+
            scale_fill_manual(values=c("grey","white")) +  
            scale_y_continuous(breaks=c(0,.2,.4,.6),labels=c(0,.2,.4,.6),limits=c(0,.6))+ 
            theme_minimal()+
            theme(text = element_text(size=12), legend.position="none",
                axis.text.x=element_blank(), axis.ticks.x=element_blank())

####Figure in Panel C
fig5c <-  ggplot(figure5c, aes(x=Treatment,y=avg,fill=Treatment)) +
            geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
            geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
            geom_text(aes(y=upper, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+    
            labs(y="",x="Informal Greeting ('hi') \n with Name",title="C")+
            scale_fill_manual(values=c("grey","white")) +  
            scale_y_continuous(breaks=c(0,.2,.4,.6),labels=c(0,.2,.4,.6),limits=c(0,.6))+ 
            theme_minimal()+
            theme(text = element_text(size=12), legend.position="none",
              axis.text.x=element_blank(), axis.ticks.x=element_blank())

####Figure in Panel D
fig5d <-  ggplot(figure5d, aes(x=Treatment,y=avg,fill=Treatment)) +
            geom_bar(stat="identity",position="dodge", color= "black", width=.5)+
            geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
            geom_text(aes(y=upper, label=round(avg,3)), position=position_dodge(width=0.9), vjust=-.75)+  
            labs(y="",x="Formal Greeting ('dear') \n with Name",title="D")+
            scale_fill_manual(values=c("grey","white")) +  
            scale_y_continuous(breaks=c(0,.2,.4,.6),labels=c(0,.2,.4,.6),limits=c(0,.6))+ 
            theme_minimal()+        
            theme(text = element_text(size=12), legend.position="none",
              axis.text.x=element_blank(), axis.ticks.x=element_blank())

#get common legend
legend <- get_legend(fig5a)
fig5a <- fig5a + theme(text = element_text(size=12),legend.position="none",axis.text.x=element_blank()) 

#Save figure 5 a PNG
fig5 <- grid.arrange(fig5a, fig5b, fig5c, fig5d, legend, nrow = 1, widths=c(2, 2, 2, 2, 1))
ggsave(file = "figure5.png", fig5, width=10,height=4,scale=1)
ggsave(file = "figure5.pdf", fig5, width=10,height=4,scale=1)

###########################################################
# Facts in section 'Psychological Costs: Tone of Responses'
###########################################################

#main results for no greeting
summary(m2fig5a)

#main results for no greeting - scaled to percentages
round(coef(m2fig5a)*100,1)
round(confint(m2fig5a)*100,1)

#main results for greeting with no name
summary(m2fig5b)

#main results for greeting with no name - scaled to percentages
round(coef(m2fig5b)*100,1)
round(confint(m2fig5b)*100,1)

#main results for informal greeting
summary(m2fig5c)

#main results for informal greeting - scaled to percentages
round(coef(m2fig5c)*100,1)
round(confint(m2fig5c)*100,1)

#main results for formal greeting
summary(m2fig5d)

#main results for formal greeting - scaled to percentages
round(coef(m2fig5d)*100,1)
round(confint(m2fig5d)*100,1)

#####################################
# Table 4: Summary of Overall Results
#####################################

m1 <- lm_robust(resp_true~t_mohammad+t_good, data=df1, se_type="HC2")
m2 <- lm_robust(reject1~t_mohammad+t_good, data=df1, se_type="HC2")
m3 <- lm_robust(unclear1~t_mohammad+t_good, data=df1, se_type="HC2")
m4 <- lm_robust(accept1~t_mohammad+t_good, data=df1, se_type="HC2")
m5 <- lm_robust(simple_q~t_mohammad+t_good, data=df1, se_type="HC2")
m6 <- lm_robust(complex_q~t_mohammad+t_good, data=df1, se_type="HC2")
m7 <- lm_robust(contact_q~t_mohammad+t_good, data=df1, se_type="HC2")
m8 <- lm_robust(meeting_q~t_mohammad+t_good, data=df1, se_type="HC2")
m9 <- lm_robust(no_greet~t_mohammad+t_good, data=df1, se_type="HC2")
m10 <- lm_robust(greet_no_name~t_mohammad+t_good, data=df1, se_type="HC2")
m11 <- lm_robust(informal_greet~t_mohammad+t_good, data=df1, se_type="HC2")
m12 <- lm_robust(formal_greet~t_mohammad+t_good, data=df1, se_type="HC2")

htmlreg(
  list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12),
  file = "table4.doc",
  custom.coef.names = c(
    "Intercept",
    "Muslim",
    "Diligent"
    ),
  reorder.coef = c(2,3,1),
  custom.model.names = c("1","2","3","4","5","6","7","8","9","10","11","12"),
  caption = "Table 4: Summary of Overall Results ",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
  include.rmse = TRUE,
  custom.note = "Note: %stars. OLS coefficients with H2 robust standard errors.
  Models:
  M1: Response,
  M2: Clear reject,
  M3: Unclear response,
  M4: Clear accept,
  M5: Simple Question,
  M6: Complex Question,
  M7: Request a phone call,
  M8: Request to meet,
  M9: No greeting,
  M10: Greeting with no name,
  M11: Informal greeting,
  M12: Formal greeting",
  digits = 2
  )

##############
#END OF SCRIPT
##############

