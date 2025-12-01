setwd("C:\\Users\\student\\OneDrive - Bryant University\\Documents\\GitHub\\Math421")
library(gganimate)
library(ggplot2)
library(tidyverse)
library(haven)
library(dplyr)
df <- read_sas("hdd0318cy.sas7bdat")
df <- df %>% 
  filter(yod==18)
df <- df[, c("yod", "payfix","pay_ub92","age",  
             "sex","raceethn","provider","moa", 
             "yoa","mod","admtype", "asource" , 
             "preopday" ,"los", "service" , "icu","ccu",    
             "dispub92", "payer"  ,"drg","trandb", 
             "randbg","randbs","orr", "anes","seq",   
             "lab","dtest", "ther","blood","phar", 
             "other","patcon","bwght","total","tot" ,  
             "ecodub92","b_wt","pt_state","diag_adm","ancilar" ,
             "campus","er_fee","er_chrg","er_mode","obs_chrg",
             "obs_hour","psycchrg","nicu_day",'race')]
missingdata <- df %>%
  summarise_all(~ sum(is.na(.)))
df <- df %>%
  select_if(~ !any(is.na(.)))
m_average_age <- mean(df$age[df$sex == 1], na.rm = TRUE)
f_average_age <- mean(df$age[df$sex == 2], na.rm = TRUE)
df <- df %>%
  mutate(season = case_when(
    moa %in% c(3, 4, 5) ~ "Spring",
    moa %in% c(6, 7, 8) ~ "Summer",
    moa %in% c(9, 10, 11) ~ "Fall",
    moa %in% c(12, 1, 2) ~ "Winter"
  )) 
save(df, file = "df_loaded.RData")
