
# Outbreak Investigation - Descriptive Stats for Line List Feb 27 25

setwd("~/Spring 2025 Classes/CPH 612")

library(readr)
Line_list_S_typhi_outbreak_cases_2025 <- read_excel("Line list_S.typhi_outbreak_cases_2025.xlsx")
View(Line_list_S_typhi_outbreak_cases_2025)
data1 <- Line_list_S_typhi_outbreak_cases_2025

library(dplyr)
library(tidyverse)

data2 <- data1 %>%
  mutate(age_cat = case_when(
           Age <= 17 ~ '<=17',
           Age >= 18 ~ '18+'
           ))

table(data2$age_cat)

table(data2$Sex)

table(data2$Race_eth)

table(data2$Hospitalized)

table(data2$Died)
table(data2$State)
