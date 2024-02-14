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
install.packages("webshot2")
install.packages("flextable")
library(kableExtra)
library(webshot2)
library(flextable)

### Table 1: 10 Examples of response emails ###
Table1 <- tibble("Examples of 10 Responses from the Schools" = c("Hello <first name>. I would like to know a little more about your son. Where does he live and what school does he go to? Once I have received this information, I will bring it to a manager. Good day. Sincerely <sender>",
                                                                 "Hi <first name>. Thanks for your mail. Before I can answer you if we have room, I need to know your address so we can check if you live in our school district... Sincerely <sender>",
                                                                 "Dear <first name> <surname> . Thank you for your mail with inquiry about a possible school transfer. Where do you live? Where is your son at school now? What is the reason you want a school transfer? You are welcome to contact me at <phone number> or <phone number>, so we can talk more about any visit to <school name>... Sincerely <sender>",
                                                                 "Hello <first name>. Thank you for your email concerning a school transfer for your son, and I need to have an in-depth chat with you to get a closer look at the problem. Can you call me at either mobile: <phone number> or my extension: <phone number>. Sincerely <sender>",
                                                                 "Hi <firstname> Unfortunately, there is no room on <schoolname> in 3rd grade unless you live in the school district. <sender>",
                                                                 "Dear <firstname> Unfortunately, we are fully booked in the year group. So, we are not in a position to take in your son. Kind regards <sender>",
                                                                 "Dear <firstname> <lastname> Unfortunately, we do not have any more places in 3rd grade. You can find information about the school on <website>. Kind regards <sender>",
                                                                 "Good morning <firstname> Your son is welcome at <schoolname> and it would be fine to hold a meeting, so you can see the school, meet the teachers and we can match our expectations. Would you please contact me – so we can set a date and time? Kind regards <sender>",
                                                                 "Hi <firstname> We do have room for your son. We think you should stop by and see <schoolname> and have a chat with our principal before you decide. When would it suit you to visit us? Kind regards <sender>",
                                                                 "Dear <firstname> <lastname> You are welcome to contact the school to get a tour. We have 26 students in 3rd grade. So, there is also room for your child. On our website <website> you can read a lot more about the school. <sender>"))

write.csv(Table1, "outputs/models/table1.csv")

