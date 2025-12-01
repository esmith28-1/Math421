df3 = read_sas('hdd0318cy.sas7bdat')
df3 <- df3 %>%
  filter(yod == 18)
df3 <- df3 %>%
  select("yod", "payfix","pay_ub92","age",  
         "sex","raceethn","provider","moa", 
         "yoa","mod","admtype", "asource" , 
         "preopday" ,"los", "service" , "icu","ccu",    
         "dispub92", "payer"  ,"drg","trandb", 
         "randbg","randbs","orr", "anes","seq",   
         "lab","dtest", "ther","blood","phar", 
         "other","patcon","bwght","total","tot" ,  
         "ecodub92","b_wt","pt_state","diag_adm","ancilar" ,
         "campus","er_fee","er_chrg","er_mode","obs_chrg",
         "obs_hour","psycchrg","nicu_day")
missing_values <- df3 %>%
  summarise_all(~ sum(is.na(.)))
df3$total <- as.numeric(as.character(df3$total))
df3 <- df3 %>%
  filter(!is.na(raceethn) & !is.na(admtype))
df3 <- df3 %>%
  mutate(target = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female",
    TRUE     ~ NA_character_
  ))
df3 <- df3 %>%
  filter(!is.na(target))
table(df3$target, useNA = "ifany")
df3 <- df3 %>%
  select(age, raceethn, provider, moa, mod, admtype, campus, total, los, target)
library(tidyverse)
library(caret)
library(rpart)
df3 <- df3 %>% 
  mutate(target = as.factor(target),
         raceethn = as.factor(raceethn),
         age = as.numeric(age), 
         provider = as.factor(provider), 
         campus = as.factor(campus),
         admtype = as.factor(admtype), 
         moa = as.factor(moa), 
         mod = as.factor(mod),
         total = as.numeric(total))
splitIndex <- createDataPartition(df3$target, p = .10, 
                                  list = FALSE)
df3_train <- df3[ splitIndex,]
df3_test <- df3[-splitIndex,]
save(df3, df3_train, df3_test, file = "df3_loaded.RData")