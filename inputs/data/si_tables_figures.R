# ########################################################################################
#
# Code: October 7, 2020
# 
# Title: The Unequal Distribution of Opportunity: 
# A National Audit Study of Bureaucratic Discrimination in Primary School Access
#
# Authors: Asmus Leth Olsen, Jonas Høgh Kyhse-Andersen and Donald Moynihan                                                                           
#                                   
# Summary: Replication code for Supporting Information. 
#
# Please report errors to: Asmus Leth Olsen, at: ajlo@ifs.ku.dk
#
# #########################################################################################

###
rm(list = ls())

#load packages
library(estimatr)
library(MASS)
library(nnet)
library(texreg)
library(tidyverse)

####################
# Note to the reader
####################

# This script is arranged in the order as the figures and tables in the Supporting Information
# A separate codebook provides definition of all variables and their respective values.

##set path for data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#load data df1
load("df1.Rda")

##########################################################################################################
# SI A -- Administrative Burdens and Existing Audit Studies 
##########################################################################################################

#SI A does not rely on data from this file.

####################################################################################
# SI -- B: Share of Non-Western Immigrants and School Grade Average
####################################################################################

#load data for descriptives on schools
load("school_df.Rda")

m1 <- lm_robust(avg_grades~pct_non_danes, data=school_df, se_type="HC2")

htmlreg(
  m1,
  file = "tableB1.doc",
  custom.coef.names = c(
    "Intercept",
    "Pct Non-Danish"),
  reorder.coef = c(2,1),
  custom.model.names = c("1"),
  caption = "Table B1: Correlation Between Share of non-Western Students and Average Grades",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
  custom.note = "Note: %stars. OLS coefficients with H2 robust standard errors.",
  digits = 2
)

school_df_no_na<-drop_na(school_df)

figureB2 <- ggplot(school_df_no_na, aes(x=pct_non_danes, y=avg_grades)) + geom_point(alpha=.3) + geom_smooth(se=F, col='black')+
  ylab('School Grade Average') + xlab('Share of non-Western Danes, pct') + 
  theme_bw() + theme(axis.text = element_text(size=15), axis.title = element_text(size=15)) + scale_y_continuous(limits=c(2,10), breaks=c(2:19))

ggsave(file="figureB2.png",figureB2,width=10,height=6,scale=1)


##########################################################################################################
# SI -- C: Existing Evidence of Similarity in Call-back Rates for Minority Named Job Applicants in Denmark
##########################################################################################################

#SI C does not rely on data from this file.

####################################################################################
# SI -- D: MODELS FOR CONFIRMATION OF RECEIPT and FORWARDING
####################################################################################

m1 <- lm_robust(I(forwarding==1)~t_mohammad+t_good, data=df1, se_type="HC2")

htmlreg(
  m1,
  file = "tableD1.doc",
  custom.coef.names = c("Intercept", "Arab Muslim Father","Diligent Student"),
  reorder.coef = c(2,3,1),
  custom.model.names = c('Forwarding og Email'),
  caption = "Table D1: Treatment and Forwarding of the Email",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
  custom.note = "Note: %stars. OLS coefficients with H2 robust standard errors.",
  digits = 2
)

####################################################################################
# SI -- E: WHO RESPONDS AND SCHOOL SIZE?
####################################################################################

#load data for responder characteristics
load("responder_df.Rda")

m1 <- lm_robust(sender_pos==1~above_median_student, data=responder_df, se_type="HC2")
m2 <- lm_robust(sender_pos==2~above_median_student, data=responder_df, se_type="HC2")
m3 <- lm_robust(sender_pos==3~above_median_student, data=responder_df, se_type="HC2")

htmlreg(
  list(m1, m2, m3),
  file = "tableE1.doc",
  custom.coef.names = c(
    "Intercept",
    "School size (1=Above median)"),
  reorder.coef = c(2,1),
  custom.model.names = c("Principal","Other Manager","Secretary"),
  caption = "Table E1: School Size and Organizational Position of Public Officials Responding to the Request",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
  custom.note = "Note: %stars. OLS coefficients with H2 robust standard errors.",
  digits = 2
)

####################################################################################
# SI -- F: WHO GETS OFFERED A WAITING LIST?
####################################################################################

m1 <- lm_robust(waitinglist~t_mohammad+t_good, data=df1, se_type="HC2")
m2 <- lm_robust(waitinglist~t_mohammad+t_good, data=df1, subset=school_type==1, se_type="HC2")
m3 <- lm_robust(waitinglist~t_mohammad+t_good, data=df1, subset=school_type!=1, se_type="HC2")

htmlreg(
  list(m1, m2, m3),
  file = "tableF1.doc",
  custom.coef.names = c("Intercept", "Arab Muslim Father","Diligent Student"),
  reorder.coef = c(2,3,1),
  custom.model.names = c("Alls schools","Public Schools","Private Schools"),
  caption = "Table F1: Offerend a Waiting List?",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
  custom.note = "Note: %stars. OLS coefficients with H2 robust standard errors.",
  digits = 2
)

####################################################################################
# SI -- G: MAIN EFFECTS WITH INTERACTION BETWEEN FACTORS
####################################################################################

m1 <- lm_robust(resp_true~t_mohammad*t_good, data=df1, se_type="HC2")
m2 <- lm_robust(reject1~t_mohammad*t_good, data=df1, se_type="HC2")
m3 <- lm_robust(unclear1~t_mohammad*t_good, data=df1, se_type="HC2")
m4 <- lm_robust(accept1~t_mohammad*t_good, data=df1, se_type="HC2")
m5 <- lm_robust(simple_q~t_mohammad*t_good, data=df1, se_type="HC2")
m6 <- lm_robust(complex_q~t_mohammad*t_good, data=df1, se_type="HC2")
m7 <- lm_robust(contact_q~t_mohammad*t_good, data=df1, se_type="HC2")
m8 <- lm_robust(meeting_q~t_mohammad*t_good, data=df1, se_type="HC2")
m9 <- lm_robust(no_greet~t_mohammad*t_good, data=df1, se_type="HC2")
m10 <- lm_robust(greet_no_name~t_mohammad*t_good, data=df1, se_type="HC2")
m11 <- lm_robust(informal_greet~t_mohammad*t_good, data=df1, se_type="HC2")
m12 <- lm_robust(formal_greet~t_mohammad*t_good, data=df1, se_type="HC2")

htmlreg(
  list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12),
  file = "tableG1.doc",
  custom.coef.names = c(
    "Intercept",
    "Muslim",
    "Diligent",
    "Interaction"),
  reorder.coef = c(2,3,4,1),
  custom.model.names = c("1","2","3","4","5","6","7","8","9","10","11","12"),
  caption = "Table G1: Main Results with Interaction",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
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



####################################################################################
# SI -- H: LOGIT AND ORDERED PROBI MODELS OF MAIN EFFECTS
####################################################################################

# LOGIT
m1 <- glm(resp_true~t_mohammad+t_good, data=df1, family = 'binomial')
m2 <- glm(reject1~t_mohammad+t_good, data=df1, family = "binomial")
m3 <- glm(unclear1~t_mohammad+t_good, data=df1, family = "binomial")
m4 <- glm(accept1~t_mohammad+t_good, data=df1, family = "binomial")
m5 <- glm(simple_q~t_mohammad+t_good, data=df1, family = "binomial")
m6 <- glm(complex_q~t_mohammad+t_good, data=df1, family = "binomial")
m7 <- glm(contact_q~t_mohammad+t_good, data=df1, family = "binomial")
m8 <- glm(meeting_q~t_mohammad+t_good, data=df1, family = "binomial")
m9 <- glm(no_greet~t_mohammad+t_good, data=df1, family = "binomial")
m10 <- glm(greet_no_name~t_mohammad+t_good, data=df1, family = "binomial")
m11 <- glm(informal_greet~t_mohammad+t_good, data=df1, family = "binomial")
m12 <- glm(formal_greet~t_mohammad+t_good, data=df1, family = "binomial")

htmlreg(
  list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12),
  file = "tableH1.doc",
  custom.coef.names = c(
    "Intercept",
    "Muslim",
    "Diligent"),
  custom.model.names = c("1","2","3","4","5","6","7","8","9","10","11","12"),
  caption = "Table H1: Main effects (Logit Specifications)",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
  custom.note = "Note: %stars. Logit coefficients.
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


# ORDERED PROBIT
m1 <- polr(as.factor(spot1) ~ t_mohammad+t_good, data=df1, Hess = T)

htmlreg(
  m1,
  custom.coef.names = c(
    "Arab Muslim Father",
    "Diligent Student",
    "Intercept 0|1",
    "Intercept 1|2"),
  custom.model.names = c("1"),
  file = "tableH2.doc",
  caption = "Table H2: Main Results (Ordered probit)",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
  custom.note = "Note: %stars. Ordered probit coefficients. Other intercepts omitted in SI.
  Baseline is Clear reject.",
  digits = 2
)


####################################################################################
# SI -- I: REGIONAL FIXED EFFECTS
####################################################################################

m1 <- lm_robust(resp_true~t_mohammad+t_good, data=df1, fixed_effects = region, se_type="HC2")
m2 <- lm_robust(reject1~t_mohammad+t_good, data=df1, fixed_effects = region, se_type="HC2")
m3 <- lm_robust(unclear1~t_mohammad+t_good, data=df1, fixed_effects = region, se_type="HC2")
m4 <- lm_robust(accept1~t_mohammad+t_good, data=df1, fixed_effects = region, se_type="HC2")
m5 <- lm_robust(simple_q~t_mohammad+t_good, data=df1, fixed_effects = region, se_type="HC2")
m6 <- lm_robust(complex_q~t_mohammad+t_good, data=df1, fixed_effects = region, se_type="HC2")
m7 <- lm_robust(contact_q~t_mohammad+t_good, data=df1, fixed_effects = region, se_type="HC2")
m8 <- lm_robust(meeting_q~t_mohammad+t_good, data=df1, fixed_effects = region, se_type="HC2")
m9 <- lm_robust(no_greet~t_mohammad+t_good, data=df1, fixed_effects = region, se_type="HC2")
m10 <- lm_robust(greet_no_name~t_mohammad+t_good, data=df1, fixed_effects = region, se_type="HC2")
m11 <- lm_robust(informal_greet~t_mohammad+t_good, data=df1, fixed_effects = region, se_type="HC2")
m12 <- lm_robust(formal_greet~t_mohammad+t_good, data=df1, fixed_effects = region, se_type="HC2")

htmlreg(
  list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12),
  file = "tableI1.doc",
  custom.coef.names = c(
    "Muslim",
    "Diligent"),
  custom.model.names = c("1","2","3","4","5","6","7","8","9","10","11","12"),
  caption = "Table I1: Main Effects (Including Regional Fixed Effects)",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
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


####################################################################################
# SI -- J: HOW DOES THE MAIN EFFECT VARY FOR PUBLIC VS. PRIVATE SCHOOLS?
####################################################################################

###interaction:

m1 <- lm_robust(resp_true~t_mohammad*I(school_type==1)+t_good, data=df1, se_type="HC2")
m2 <- lm_robust(reject1~t_mohammad*I(school_type==1)+t_good, data=df1, se_type="HC2")
m3 <- lm_robust(unclear1~t_mohammad*I(school_type==1)+t_good, data=df1, se_type="HC2")
m4 <- lm_robust(accept1~t_mohammad*I(school_type==1)+t_good, data=df1, se_type="HC2")
m5 <- lm_robust(simple_q~t_mohammad*I(school_type==1)+t_good, data=df1, se_type="HC2")
m6 <- lm_robust(complex_q~t_mohammad*I(school_type==1)+t_good, data=df1, se_type="HC2")
m7 <- lm_robust(contact_q~t_mohammad*I(school_type==1)+t_good, data=df1, se_type="HC2")
m8 <- lm_robust(meeting_q~t_mohammad*I(school_type==1)+t_good, data=df1, se_type="HC2")
m9 <- lm_robust(no_greet~t_mohammad*I(school_type==1)+t_good, data=df1, se_type="HC2")
m10 <- lm_robust(greet_no_name~t_mohammad*I(school_type==1)+t_good, data=df1, se_type="HC2")
m11 <- lm_robust(informal_greet~t_mohammad*I(school_type==1)+t_good, data=df1, se_type="HC2")
m12 <- lm_robust(formal_greet~t_mohammad*I(school_type==1)+t_good, data=df1, se_type="HC2")

htmlreg(
  list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12),
  file = "tableJ1.doc",
  custom.coef.names = c(
    "Intercept",
    "Muslim",
    "Public school",
    'Diligent',
    "Interaction"),
  reorder.coef = c(2,3,4,5,1),
  custom.model.names = c("1","2","3","4","5","6","7","8","9","10","11","12"),
  caption = "Table J1: Effect Across School Type",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
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

###public schools only: 

m1 <- lm_robust(resp_true~t_mohammad+t_good, data=df1, subset = school_type==1,  se_type="HC2")
m2 <- lm_robust(reject1~t_mohammad+t_good, data=df1, subset = school_type==1, se_type="HC2")
m3 <- lm_robust(unclear1~t_mohammad+t_good, data=df1, subset = school_type==1, se_type="HC2")
m4 <- lm_robust(accept1~t_mohammad+t_good, data=df1, subset = school_type==1, se_type="HC2")
m5 <- lm_robust(simple_q~t_mohammad+t_good, data=df1, subset = school_type==1, se_type="HC2")
m6 <- lm_robust(complex_q~t_mohammad+t_good, data=df1, subset = school_type==1, se_type="HC2")
m7 <- lm_robust(contact_q~t_mohammad+t_good, data=df1, subset = school_type==1, se_type="HC2")
m8 <- lm_robust(meeting_q~t_mohammad+t_good, data=df1, subset = school_type==1, se_type="HC2")
m9 <- lm_robust(no_greet~t_mohammad+t_good, data=df1, subset = school_type==1, se_type="HC2")
m10 <- lm_robust(greet_no_name~t_mohammad+t_good, data=df1, subset = school_type==1, se_type="HC2")
m11 <- lm_robust(informal_greet~t_mohammad+t_good, data=df1, subset = school_type==1, se_type="HC2")
m12 <- lm_robust(formal_greet~t_mohammad+t_good, data=df1, subset = school_type==1, se_type="HC2")

htmlreg(
  list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12),
  file = "tableJ2.doc",
  custom.coef.names = c(
    "Intercept",
    "Muslim",
    "Diligent"
  ),
  reorder.coef = c(2,3,1),
  custom.model.names = c("1","2","3","4","5","6","7","8","9","10","11","12"),
  caption = "Table J2: Main Results (Public Schools Only)",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
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

###private schools only: 
m1 <- lm_robust(resp_true~t_mohammad+t_good, data=df1, subset = school_type==0,  se_type="HC2")
m2 <- lm_robust(reject1~t_mohammad+t_good, data=df1, subset = school_type==0, se_type="HC2")
m3 <- lm_robust(unclear1~t_mohammad+t_good, data=df1, subset = school_type==0, se_type="HC2")
m4 <- lm_robust(accept1~t_mohammad+t_good, data=df1, subset = school_type==0, se_type="HC2")
m5 <- lm_robust(simple_q~t_mohammad+t_good, data=df1, subset = school_type==0, se_type="HC2")
m6 <- lm_robust(complex_q~t_mohammad+t_good, data=df1, subset = school_type==0, se_type="HC2")
m7 <- lm_robust(contact_q~t_mohammad+t_good, data=df1, subset = school_type==0, se_type="HC2")
m8 <- lm_robust(meeting_q~t_mohammad+t_good, data=df1, subset = school_type==0, se_type="HC2")
m9 <- lm_robust(no_greet~t_mohammad+t_good, data=df1, subset = school_type==0, se_type="HC2")
m10 <- lm_robust(greet_no_name~t_mohammad+t_good, data=df1, subset = school_type==0, se_type="HC2")
m11 <- lm_robust(informal_greet~t_mohammad+t_good, data=df1, subset = school_type==0, se_type="HC2")
m12 <- lm_robust(formal_greet~t_mohammad+t_good, data=df1, subset = school_type==0, se_type="HC2")

htmlreg(
  list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12),
  file = "tableJ3.doc",
  custom.coef.names = c(
    "Intercept",
    "Muslim",
    "Diligent"
  ),
  reorder.coef = c(2,3,1),
  custom.model.names = c("1","2","3","4","5","6","7","8","9","10","11","12"),
  caption = "Table J3: Main Results (Private Schools Only)",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
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

####################################################################################
# SI - K: HOW DOES THE MAIN EFFECT VARY FOR URBAN VS. RURAL SCHOOLS
####################################################################################

m1 <- lm_robust(resp_true~t_mohammad*I(bigcity==1)+t_good, data=df1, se_type="HC2")
m2 <- lm_robust(reject1~t_mohammad*I(bigcity==1)+t_good, data=df1, se_type="HC2")
m3 <- lm_robust(unclear1~t_mohammad*I(bigcity==1)+t_good, data=df1, se_type="HC2")
m4 <- lm_robust(accept1~t_mohammad*I(bigcity==1)+t_good, data=df1, se_type="HC2")
m5 <- lm_robust(simple_q~t_mohammad*I(bigcity==1)+t_good, data=df1, se_type="HC2")
m6 <- lm_robust(complex_q~t_mohammad*I(bigcity==1)+t_good, data=df1, se_type="HC2")
m7 <- lm_robust(contact_q~t_mohammad*I(bigcity==1)+t_good, data=df1, se_type="HC2")
m8 <- lm_robust(meeting_q~t_mohammad*I(bigcity==1)+t_good, data=df1, se_type="HC2")
m9 <- lm_robust(no_greet~t_mohammad*I(bigcity==1)+t_good, data=df1, se_type="HC2")
m10 <- lm_robust(greet_no_name~t_mohammad*I(bigcity==1)+t_good, data=df1, se_type="HC2")
m11 <- lm_robust(informal_greet~t_mohammad*I(bigcity==1)+t_good, data=df1, se_type="HC2")
m12 <- lm_robust(formal_greet~t_mohammad*I(bigcity==1)+t_good, data=df1, se_type="HC2")

htmlreg(
  list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12),
  file = "tableK1.doc",
  custom.coef.names = c(
    "Intercept",
    "Muslim",
    "Urban area",
    "Diligent",
    "Interaction"),
  reorder.coef = c(2,3,4,5,1),
  custom.model.names = c("1","2","3","4","5","6","7","8","9","10","11","12"),
  caption = "Table K1: Effect Across Urban and Rural Schools",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
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


####################################################################################
# SI -- L: DICOTOMIZE NON-WESTERN AT MEDIAN VALUE
####################################################################################

m1 <- lm_robust(resp_true~t_mohammad*above_median_non_danes+t_good, data=df1, se_type="HC2")
m2 <- lm_robust(reject1~t_mohammad*above_median_non_danes+t_good, data=df1, se_type="HC2")
m3 <- lm_robust(unclear1~t_mohammad*above_median_non_danes+t_good, data=df1, se_type="HC2")
m4 <- lm_robust(accept1~t_mohammad*above_median_non_danes+t_good, data=df1, se_type="HC2")
m5 <- lm_robust(simple_q~t_mohammad*above_median_non_danes+t_good, data=df1, se_type="HC2")
m6 <- lm_robust(complex_q~t_mohammad*above_median_non_danes+t_good, data=df1, se_type="HC2")
m7 <- lm_robust(contact_q~t_mohammad*above_median_non_danes+t_good, data=df1, se_type="HC2")
m8 <- lm_robust(meeting_q~t_mohammad*above_median_non_danes+t_good, data=df1, se_type="HC2")
m9 <- lm_robust(no_greet~t_mohammad*above_median_non_danes+t_good, data=df1, se_type="HC2")
m10 <- lm_robust(greet_no_name~t_mohammad*above_median_non_danes+t_good, data=df1, se_type="HC2")
m11 <- lm_robust(informal_greet~t_mohammad*above_median_non_danes+t_good, data=df1, se_type="HC2")
m12 <- lm_robust(formal_greet~t_mohammad*above_median_non_danes+t_good, data=df1, se_type="HC2")

htmlreg(
  list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12),
  file = "tableL1.doc",
  custom.coef.names = c(
    "Intercept",
    "Muslim",
    "Non-Western",
    "Diligent",
    "Interaction"),
  reorder.coef = c(2,3,4,5,1),
  custom.model.names = c("1","2","3","4","5","6","7","8","9","10","11","12"),
  caption = "Table X: Effect of Muslim Treatment Across Schools' Share of non-Western Students (binary)",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
  custom.note = "Note: %stars. OLS coefficients with H2 robust standard errors. 
  Share of non-Western pupils (binary) is a variable taking 1 if above the median value of share of non-Western pupils, and 0 otherwise.
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

####################################################################################
# SI -- M: HOW DOES THE MAIN EFFECT VARY WITH SCHOOL AVERAGE GRADE?
####################################################################################

m1 <- lm_robust(resp_true~t_mohammad*above_median_grades+t_good, data=df1, se_type="HC2")
m2 <- lm_robust(reject1~t_mohammad*above_median_grades+t_good, data=df1, se_type="HC2")
m3 <- lm_robust(unclear1~t_mohammad*above_median_grades+t_good, data=df1, se_type="HC2")
m4 <- lm_robust(accept1~t_mohammad*above_median_grades+t_good, data=df1, se_type="HC2")
m5 <- lm_robust(simple_q~t_mohammad*above_median_grades+t_good, data=df1, se_type="HC2")
m6 <- lm_robust(complex_q~t_mohammad*above_median_grades+t_good, data=df1, se_type="HC2")
m7 <- lm_robust(contact_q~t_mohammad*above_median_grades+t_good, data=df1, se_type="HC2")
m8 <- lm_robust(meeting_q~t_mohammad*above_median_grades+t_good, data=df1, se_type="HC2")
m9 <- lm_robust(no_greet~t_mohammad*above_median_grades+t_good, data=df1, se_type="HC2")
m10 <- lm_robust(greet_no_name~t_mohammad*above_median_grades+t_good, data=df1, se_type="HC2")
m11 <- lm_robust(informal_greet~t_mohammad*above_median_grades+t_good, data=df1, se_type="HC2")
m12 <- lm_robust(formal_greet~t_mohammad*above_median_grades+t_good, data=df1, se_type="HC2")

htmlreg(
  list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12),
  file = "tableM1.doc",
  custom.coef.names = c(
    "Intercept",
    "Muslim",
    "Grade Avg.",
    "Diligent",
    "Interaction"),
  reorder.coef = c(2,3,4,5,1),
  custom.model.names = c("1","2","3","4","5","6","7","8","9","10","11","12"),
  caption = "Table M1: Effect of Muslim Treatment Across Schools' Grade Average",
  caption.above = T,
  stars = c(0.05),
  single.row = F,
  include.ci = FALSE,
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


########################################################################################
############################################THE END
########################################################################################
