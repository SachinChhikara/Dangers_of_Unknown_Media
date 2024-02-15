#### Preamble ####
# Purpose: Tests the data given in the df1.rda file
# Author: Fares Alkorani
# Date: 15 February 2024
# Contact: 156137245+faralk@users.noreply.github.com
# License: MIT
# Pre-requisites: -


#### Workspace setup ####
library(tidyverse)
library(dplyr)

#### Test data ####

set.seed(123)

load('inputs/data/df1.rda')

# Confirms if respond date is always after the send date

ifelse(!is.na(df1$send_date) & !is.na(df1$resp_date), (df1$send_date <= df1$resp_date) == TRUE, TRUE)
class(df1$send_date) == "Date"

# Confirms if the variables are binary

df1$t_mohammad |> unique() == c(0, 1)
df1$t_good |> unique() == c(0, 1)
df1$reject1 |> unique() == c(0, 1)
df1$unclear1 |> unique() == c(1, 0)
df1$accept1 |> unique() == c(0, 1)
df1$simple_q |> unique() == c(0, 1)
df1$simple_q_alt |> unique() == c(0, 1)
df1$complex_q |> unique() == c(0, 1)
df1$contact_q |> unique() == c(1, 0)
df1$meeting_q |> unique() == c(1, 0)
df1$no_greet |> unique() == c(0, 1)
df1$greet_no_name |> unique() == c(0, 1)
df1$informal_greet |> unique() == c(1, 0)
df1$formal_greet |> unique() == c(0, 1)


# Checks for if every row, there are two different columns

all(df1$reject1 != df1$unclear1 | df1$accept1)
all(df1$unclear1 != df1$accept1 | df1$reject1)
all(df1$accept1 != df1$unclear1 | df1$reject1)




